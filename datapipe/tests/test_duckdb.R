#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# test_duckdb.R -- differential tests: DuckDB against R.
#
# The R engine in pipeline.R/ops.R is the specification. Every operation, and
# the pipeline as a whole, must produce identical results through DuckDB. Any
# difference is a bug in the SQL translation, never an acceptable variation.
#
#   Rscript tests/test_duckdb.R
# ---------------------------------------------------------------------------

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(dirname(normalizePath(sub("^--file=", "", fa[1])))) else getwd()
})
options(datapipe.app_dir = app_dir)
suppressPackageStartupMessages({ library(jsonlite); library(data.table) })
for (f in c("utils.R", "i18n.R", "ops.R", "io.R", "pipeline.R", "sql.R", "engine_duckdb.R")) {
  source(file.path(app_dir, "R", f))
}
dp_set_language("en", file.path(app_dir, "locale"))

if (!dp_duckdb_available()) {
  cat("duckdb is not installed -- nothing to compare against.\n")
  cat("Install it with:  install.packages(\"duckdb\")\n")
  quit(status = 0)
}
suppressPackageStartupMessages({ library(DBI); library(duckdb) })
cat("duckdb version: ", dp_duckdb_version(), "\n\n", sep = "")

PASS <- 0L; FAIL <- 0L; SKIP <- 0L; FAILURES <- character(0)
ok <- function(label, cond, extra = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat("  ok   ", label, "\n", sep = "") }
  else { FAIL <<- FAIL + 1L; FAILURES <<- c(FAILURES, label)
         cat("  FAIL ", label, if (nzchar(extra)) paste0("  -- ", extra) else "", "\n", sep = "") }
}
skip <- function(label, why) { SKIP <<- SKIP + 1L; cat("  skip ", label, "  (", why, ")\n", sep = "") }
section <- function(x) cat("\n", x, "\n", sep = "")

same_vec <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  if (length(a) != length(b)) return(FALSE)
  all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
}
show_diff <- function(inp, a, b) {
  i <- which(!((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))))
  i <- head(i, 3)
  paste(sprintf("[%s] R=%s duck=%s", ifelse(is.na(inp[i]), "<NA>", inp[i]),
                ifelse(is.na(a[i]), "<NA>", a[i]), ifelse(is.na(b[i]), "<NA>", b[i])),
        collapse = "; ")
}

con <- dbConnect(duckdb())
on.exit(try(dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

# Evaluate a step chain inside DuckDB.
ddb_steps <- function(values, steps) {
  expr <- sql_steps_expr('"x"', steps)
  if (is.null(expr)) return(NULL)
  df <- data.frame(x = as.character(values), rn = seq_along(values), stringsAsFactors = FALSE)
  duckdb_register(con, "tvals", df, overwrite = TRUE)
  as.character(dbGetQuery(con, paste0("SELECT ", expr, " AS v FROM tvals ORDER BY rn"))$v)
}

step <- function(op, ...) list(op = op, params = list(...))

# R uppercases through glibc's per-character tables; DuckDB uses ICU's full
# Unicode mapping. They agree on ASCII and on ordinary accented Latin, and
# disagree on the handful of characters Unicode gives a special casing rule --
# the sharp s being the one anybody meets in practice. Those inputs are held
# out of the general comparison and pinned separately below, so the exception
# is a known, tested quantity rather than a surprise.
SPECIAL_CASING <- c("stra\u00dfe")
holdout <- function(op) if (op == "case") !(INPUTS %in% SPECIAL_CASING) else rep(TRUE, length(INPUTS))

# Inputs chosen to poke at the places string handling usually breaks.
INPUTS <- c(
  "AC-000123 ", " ac/000456", "AC.000789", "000123", "0", "000", "",
  "  ", NA, "hello world", "hELLO wORLD", "Ada Lovelace", "o'brien",
  "INV-2026-77", "a-b-c", "a", "3,400.00", "1250.5", "  99.99", "-42abc",
  "n/a", "05/03/2026", "2026-03-05", "45000", "hello", "Spärck",
  "MiXeD-case_123", "x9y8", "12345678901234", "a.b.c", "AB", "ab",
  "file.csv", "  padded  ", "ABCDEF", "abcdef123456",
  # Non-ASCII: R's POSIX classes follow the locale, RE2's do not.
  "M\u00fcnchen", "\u00c9cole 42", "stra\u00dfe", "caf\u00e9-bar",
  "\u00a0leading nbsp", "tab\there", "\u4e2d\u6587123"
)

# ===========================================================================
section("Every operation, with its default parameters")

for (nm in names(TF_OPS)) {
  steps <- list(list(op = nm, params = tf_op_defaults(nm)))
  keep <- holdout(nm); inp <- INPUTS[keep]
  r_out <- tryCatch(tf_apply_steps(inp, steps), error = function(e) paste("ERR:", conditionMessage(e)))
  d_out <- tryCatch(ddb_steps(inp, steps), error = function(e) paste("ERR:", conditionMessage(e)))
  if (is.null(d_out)) {
    skip(paste0(nm, " (defaults)"), "no SQL translation, falls back to R")
  } else {
    ok(paste0(nm, " (defaults)"), same_vec(r_out, d_out), show_diff(inp, r_out, d_out))
  }
}

# ===========================================================================
section("Operations with non-default parameters")

CASES <- list(
  list("trim left",            step("trim", side = "left")),
  list("trim right",           step("trim", side = "right")),
  list("case upper",           step("case", to = "upper")),
  list("case lower",           step("case", to = "lower")),
  list("case title",           step("case", to = "title")),
  list("case sentence",        step("case", to = "sentence")),
  list("digits keep decimal",  step("digits_only", keep_decimal = TRUE)),
  list("digits keep sign",     step("digits_only", keep_sign = TRUE)),
  list("letters keep spaces",  step("letters_only", keep_spaces = TRUE)),
  list("alnum keep spaces",    step("alnum_only", keep_spaces = TRUE)),
  list("remove chars",         step("remove_chars", chars = "-/.")),
  list("remove chars regexy",  step("remove_chars", chars = "].[^$")),
  list("keep chars",           step("keep_chars", chars = "0123456789")),
  list("keep chars regexy",    step("keep_chars", chars = "0-9]^")),
  list("prefix",               step("prefix", text = "AC-")),
  list("prefix no skip",       step("prefix", text = "AC-", skip_blank = FALSE)),
  list("suffix",               step("suffix", text = "-UK")),
  list("remove prefix ci",     step("remove_prefix", text = "AC-", ignore_case = TRUE)),
  list("remove prefix cs",     step("remove_prefix", text = "AC-", ignore_case = FALSE)),
  list("remove suffix",        step("remove_suffix", text = ".csv")),
  list("replace literal",      step("replace", find = ".", replace = "-")),
  list("replace first only",   step("replace", find = ".", replace = "-", first_only = TRUE)),
  list("replace regex",        step("replace", find = "[0-9]+", replace = "#", regex = TRUE)),
  list("replace regex group",  step("replace", find = "([a-z])-", replace = "\\1_", regex = TRUE)),
  list("replace ignore case",  step("replace", find = "ac", replace = "XX", ignore_case = TRUE)),
  list("left pad 8",           step("left_pad", width = 8, char = "0")),
  list("left pad 2",           step("left_pad", width = 2, char = "0")),
  list("right pad",            step("right_pad", width = 12, char = ".")),
  list("strip leading zeros",  step("strip_leading_zeros")),
  list("left 3",               step("left", n = 3)),
  list("right 2",              step("right", n = 2)),
  list("right 50",             step("right", n = 50)),
  list("substring",            step("substring", start = 2, len = 3)),
  list("substring to end",     step("substring", start = 4, len = 0)),
  list("truncate",             step("truncate", width = 4)),
  list("regex extract",        step("regex_extract", pattern = "([0-9]{4})", group = 1)),
  list("regex extract keep",   step("regex_extract", pattern = "([0-9]{4})", group = 1,
                                    on_no_match = "keep original")),
  list("regex extract na",     step("regex_extract", pattern = "([0-9]{4})", group = 1,
                                    on_no_match = "empty (NA)")),
  list("regex extract whole",  step("regex_extract", pattern = "[0-9]+", group = 0)),
  list("split take first",     step("split_take", sep = "-", index = 1)),
  list("split take last",      step("split_take", sep = "-", index = -1)),
  list("split take oob",       step("split_take", sep = "-", index = 9)),
  list("format number 2dp",    step("format_number", decimals = 2)),
  list("format number 0dp",    step("format_number", decimals = 0)),
  list("format number keep",   step("format_number", decimals = 2, on_fail = "keep original")),
  list("format number na",     step("format_number", decimals = 2, on_fail = "empty (NA)")),
  list("format number comma",  step("format_number", decimals = 2, big_mark = ",")),
  list("round number",         step("round_number", decimals = 2)),
  list("date dmy",             step("format_date", input_format = "%d/%m/%Y")),
  list("date auto",            step("format_date")),
  list("date out fmt",         step("format_date", output_format = "%d/%m/%Y")),
  list("date keep original",   step("format_date", on_fail = "keep original")),
  list("fill na",              step("fill_na", value = "-")),
  list("blank to na",          step("blank_to_na")),
  list("set constant",         step("set_constant", value = "Z")),
  list("strip accents",        step("strip_accents")),
  list("chain: key repair",    step("trim"), step("case", to = "upper"),
                               step("digits_only"), step("left_pad", width = 8, char = "0")),
  list("chain: extract + pad", step("regex_extract", pattern = "([0-9]+)", group = 1),
                               step("left_pad", width = 6, char = "0"),
                               step("prefix", text = "N")),
  list("chain: date then trim", step("format_date"), step("left", n = 7))
)

for (cs in CASES) {
  label <- cs[[1]]
  steps <- cs[-1]
  keep <- holdout(scalar(steps[[1]]$op, ""))
  inp <- INPUTS[keep]
  r_out <- tryCatch(tf_apply_steps(inp, steps), error = function(e) paste("ERR:", conditionMessage(e)))
  d_out <- tryCatch(ddb_steps(inp, steps), error = function(e) paste("ERR:", conditionMessage(e)))
  if (is.null(d_out)) skip(label, "no SQL translation, falls back to R")
  else ok(label, same_vec(r_out, d_out), show_diff(inp, r_out, d_out))
}

section("The one documented divergence, pinned")
{
  up <- step("case", to = "upper")
  r_up <- tf_apply_steps(SPECIAL_CASING, list(up))
  d_up <- ddb_steps(SPECIAL_CASING, list(up))
  ok("sharp s: R leaves it, DuckDB raises it to U+1E9E",
     identical(r_up, "STRA\u00dfE") && identical(d_up, "STRA\u1e9eE"),
     paste0("R=", r_up, " duck=", d_up))
  ok("the divergence is confined to case conversion",
     same_vec(tf_apply_steps(SPECIAL_CASING, list(step("alnum_only"))),
              ddb_steps(SPECIAL_CASING, list(step("alnum_only")))))
  ok("lower case agrees on the same input",
     same_vec(tf_apply_steps(SPECIAL_CASING, list(step("case", to = "lower"))),
              ddb_steps(SPECIAL_CASING, list(step("case", to = "lower")))))
}

# ===========================================================================
section("Joins, filters, sorting and de-duplication")

tmp <- file.path(tempdir(), "dp_ddb")
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

# Build a pair of CSVs exercising duplicates, blanks and case differences.
left <- data.frame(
  id   = c("1", "2", "3", "", "4", "2", NA, "5"),
  code = c("a", "B", "c", "d", "", "b", "x", "E"),
  val  = c("10", "20", "30", "40", "50", "60", "70", "80"),
  stringsAsFactors = FALSE)
right <- data.frame(
  rid  = c("1", "2", "2", "4", "", "6"),
  name = c("one", "two", "TWO-dup", "four", "blank", "six"),
  val  = c("L1", "L2", "L3", "L4", "L5", "L6"),
  stringsAsFactors = FALSE)
write.csv(left, file.path(tmp, "left.csv"), row.names = FALSE, na = "")
write.csv(right, file.path(tmp, "right.csv"), row.names = FALSE, na = "")

mk <- function(...) {
  s <- dp_new_pipeline("cmp")
  s$sources <- list(
    list(id = "L", label = "L", role = "data", path_mode = "file",
         path = "left.csv", format = "csv", options = list(header = TRUE, delimiter = ",")),
    list(id = "R", label = "R", role = "mapping", path_mode = "file",
         path = "right.csv", format = "csv", options = list(header = TRUE, delimiter = ",")))
  s$joins$base_source_id <- "L"
  s$export$path <- "out.csv"
  modifyList(s, list(...))
}

join_case <- function(label, jstep, final = NULL) {
  s <- mk()
  s$joins$steps <- list(modifyList(list(right_source_id = "R", type = "left",
                                        keys = list(list(left = "id", right = "rid")),
                                        select = list(), prefix = "", suffix = "",
                                        conflict = "suffix", multi_match = "all",
                                        ignore_case = FALSE, match_blanks = FALSE), jstep))
  if (!is.null(final)) s$final <- modifyList(s$final, final)
  r <- tryCatch(dp_execute(s, tmp, stop_after = "final"), error = function(e) e)
  d <- tryCatch(dp_execute_duckdb(s, tmp, stop_after = "final"), error = function(e) e)
  if (inherits(r, "error") || inherits(d, "error")) {
    both_err <- inherits(r, "error") && inherits(d, "error")
    ok(label, both_err, paste0("R:", if (inherits(r, "error")) conditionMessage(r) else "ok",
                               " duck:", if (inherits(d, "error")) conditionMessage(d) else "ok"))
    return(invisible())
  }
  same_names <- identical(names(r$data), names(d$data))
  same_rows <- nrow(r$data) == nrow(d$data)
  same_data <- same_names && same_rows &&
    all(vapply(names(r$data), function(cn) same_vec(r$data[[cn]], d$data[[cn]]), logical(1)))
  ok(label, same_data,
     if (!same_names) paste0("names R=", paste(names(r$data), collapse = ","),
                             " duck=", paste(names(d$data), collapse = ","))
     else if (!same_rows) paste0("rows R=", nrow(r$data), " duck=", nrow(d$data))
     else {
       bad <- names(r$data)[!vapply(names(r$data), function(cn)
         same_vec(r$data[[cn]], d$data[[cn]]), logical(1))]
       paste0("cols differ: ", paste(bad, collapse = ","), " | R=",
              paste(head(r$data[[bad[1]]], 8), collapse = "/"), " duck=",
              paste(head(d$data[[bad[1]]], 8), collapse = "/"))
     })
}

join_case("left join, duplicate lookup keys",       list(type = "left"))
join_case("inner join",                             list(type = "inner"))
join_case("full join",                              list(type = "full"))
join_case("right join",                             list(type = "right"))
join_case("semi join",                              list(type = "semi"))
join_case("anti join",                              list(type = "anti"))
join_case("keep first of duplicates",               list(multi_match = "first"))
join_case("duplicates raise an error",              list(multi_match = "error"))
join_case("case-insensitive key",                   list(keys = list(list(left = "code", right = "name")),
                                                         ignore_case = TRUE))
join_case("blank keys match when asked",            list(match_blanks = TRUE, multi_match = "first"))
join_case("selected fields only",                   list(select = list("name")))
join_case("prefix on incoming names",               list(select = list("name"), prefix = "r_"))
join_case("conflict: keep both",                    list(conflict = "suffix", multi_match = "first"))
join_case("conflict: right wins",                   list(conflict = "right_wins", multi_match = "first"))
join_case("conflict: coalesce",                     list(conflict = "coalesce", multi_match = "first"))
join_case("conflict: skip",                         list(conflict = "skip", multi_match = "first"))

join_case("filter equals", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "code", op = "equals", value = "b", ignore_case = TRUE)))))
join_case("filter not blank", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "id", op = "not_blank", value = "")))))
join_case("filter numeric gt", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "val", op = "gt", value = "25")))))
join_case("filter in list", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "code", op = "in_list", value = "a, c", ignore_case = TRUE)))))
join_case("filter any-of", list(multi_match = "first"),
          list(filter = list(mode = "any", conditions = list(
            list(field = "code", op = "equals", value = "a", ignore_case = TRUE),
            list(field = "code", op = "equals", value = "c", ignore_case = TRUE)))))
join_case("filter contains", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "name", op = "contains", value = "o", ignore_case = TRUE)))))
join_case("filter regex", list(multi_match = "first"),
          list(filter = list(mode = "all", conditions = list(
            list(field = "val", op = "regex", value = "^[0-9]0$")))))

join_case("sort text ascending", list(multi_match = "first"),
          list(sort = list(list(field = "code", dir = "asc"))))
join_case("sort text descending", list(multi_match = "first"),
          list(sort = list(list(field = "code", dir = "desc"))))
join_case("sort numeric", list(multi_match = "first"),
          list(sort = list(list(field = "val", dir = "asc", numeric = TRUE))))
join_case("sort two columns", list(multi_match = "first"),
          list(sort = list(list(field = "code", dir = "asc"), list(field = "val", dir = "desc"))))

join_case("dedupe keep first", list(multi_match = "first"),
          list(dedupe = list(enabled = TRUE, keys = list("code"), keep = "first")))
join_case("dedupe keep last", list(multi_match = "first"),
          list(dedupe = list(enabled = TRUE, keys = list("code"), keep = "last")))

join_case("field selection and renaming", list(multi_match = "first"),
          list(fields = list(
            list(source = "id", output = "Identifier", include = TRUE, order = 1, steps = list()),
            list(source = "name", output = "Name", include = TRUE, order = 2, steps = list()),
            list(source = "val", output = "Hidden", include = FALSE, order = 3, steps = list()))))
join_case("field with formatting steps", list(multi_match = "first"),
          list(fields = list(
            list(source = "id", output = "Padded", include = TRUE, order = 1,
                 steps = list(step("left_pad", width = 5, char = "0"))),
            list(source = "val", output = "Money", include = TRUE, order = 2,
                 steps = list(step("format_number", decimals = 2))))))

# multi-field join
{
  s <- mk()
  s$joins$steps <- list(list(right_source_id = "R", type = "left",
    keys = list(list(left = "id", right = "rid"), list(left = "val", right = "val")),
    select = list("name"), prefix = "", suffix = "", conflict = "suffix",
    multi_match = "all", ignore_case = FALSE, match_blanks = FALSE))
  r <- dp_execute(s, tmp, stop_after = "final")
  d <- dp_execute_duckdb(s, tmp, stop_after = "final")
  ok("two-field join", nrow(r$data) == nrow(d$data) &&
       same_vec(r$data$name, d$data$name))
}

# ===========================================================================
section("Reading: the two engines see the same source data")

{
  s <- mk()
  s$joins$steps <- list()
  r <- dp_execute(s, tmp, stop_after = "joins")
  d <- dp_execute_duckdb(s, tmp, stop_after = "joins")
  ok("csv read identically", identical(names(r$data), names(d$data)) &&
       all(vapply(names(r$data), function(cn) same_vec(r$data[[cn]], d$data[[cn]]), logical(1))),
     paste0("R=", paste(names(r$data), collapse = ","), " duck=", paste(names(d$data), collapse = ",")))
}

# quoted fields, embedded separators and UTF-8
{
  qp <- file.path(tmp, "quoted.csv")
  conn <- file(qp, "wb")
  writeBin(charToRaw(paste0('id,txt\n1,"a,b"\n2,"say ""hi"""\n3,Sp\xc3\xa4rck\n',
                            '4,"line one"\n5,\n')), conn)
  close(conn)
  s <- mk()
  s$sources <- list(list(id = "L", label = "L", role = "data", path_mode = "file",
                         path = "quoted.csv", format = "csv",
                         options = list(header = TRUE, delimiter = ",")))
  s$joins$base_source_id <- "L"; s$joins$steps <- list()
  r <- dp_execute(s, tmp, stop_after = "joins")
  d <- dp_execute_duckdb(s, tmp, stop_after = "joins")
  ok("quoting, commas and UTF-8 read identically", same_vec(r$data$txt, d$data$txt),
     paste0("R=", paste(r$data$txt, collapse = "|"), " duck=", paste(d$data$txt, collapse = "|")))
}

# ===========================================================================
section("The example pipeline, end to end")

ex <- file.path(app_dir, "pipelines", "example_monthly_sales.json")
if (!file.exists(ex)) {
  skip("example pipeline", "not found")
} else {
  spec <- dp_load_pipeline(ex)
  out_r <- file.path(tmp, "res_r.csv"); out_d <- file.path(tmp, "res_d.csv")

  sr <- spec; sr$export$path <- out_r
  sd <- spec; sd$export$path <- out_d
  r <- dp_execute(sr, app_dir)
  d <- dp_execute_duckdb(sd, app_dir)

  ok("same column names", identical(names(r$data), names(d$data)))
  ok("same row count", nrow(r$data) == nrow(d$data),
     paste0("R=", nrow(r$data), " duck=", nrow(d$data)))
  bad <- names(r$data)[!vapply(names(r$data), function(cn)
    same_vec(r$data[[cn]], d$data[[cn]]), logical(1))]
  ok("same values in every column", length(bad) == 0,
     if (length(bad)) paste0("differing: ", paste(bad, collapse = ", ")) else "")
  ok("exported files are byte-identical",
     identical(readBin(out_r, "raw", file.size(out_r)),
               readBin(out_d, "raw", file.size(out_d))))
  ok("same warnings", identical(sort(r$warnings), sort(d$warnings)),
     paste0("R=", length(r$warnings), " duck=", length(d$warnings)))
  # The two runs necessarily write to different files, so compare the log with
  # the export path normalised out.
  norm <- function(x) sub("res_[rd]\\.csv", "<out>", unlist(x))
  ok("same run-log lines", identical(norm(r$log), norm(d$log)),
     { i <- which(norm(r$log) != norm(d$log))[1]
       if (is.na(i)) "" else paste0("line ", i, ": R='", norm(r$log)[i],
                                    "' duck='", norm(d$log)[i], "'") })
}

# ===========================================================================
section("Engine selection")

{
  spec <- dp_load_pipeline(ex)
  res <- dp_run(spec, app_dir, stop_after = "final")
  ok("auto picks duckdb when it can", identical(res$engine, "duckdb"))

  # A pipeline using an operation with no SQL equivalent must fall back.
  s2 <- spec
  s2$final$fields[[3]]$steps <- list(step("case", to = "title"))
  res2 <- dp_run(s2, app_dir, stop_after = "final")
  ok("auto falls back to R for untranslatable operations", identical(res2$engine, "r"))
  ok("the fallback says why", any(grepl("rather than DuckDB because", unlist(res2$log))))

  ok("forcing the R engine works", identical(dp_run(spec, app_dir, stop_after = "final",
                                                    engine = "r")$engine %||% "r", "r"))
  err <- tryCatch({ dp_run(s2, app_dir, stop_after = "final", engine = "duckdb"); NULL },
                  error = function(e) conditionMessage(e))
  ok("forcing duckdb on an untranslatable pipeline is a clear error",
     !is.null(err) && grepl("cannot run this pipeline", err))

  ok("untranslatable ops are listed by name",
     "Change case" %in% sql_untranslatable_ops(s2))
}

# ===========================================================================
cat("\n", strrep("-", 60), "\n", sep = "")
cat("Passed: ", PASS, "   Failed: ", FAIL, "   Skipped (falls back to R): ", SKIP, "\n", sep = "")
if (FAIL > 0) {
  cat("\nFailed checks:\n"); for (f in FAILURES) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("DuckDB matches R on every translatable operation.\n")
quit(status = 0)
