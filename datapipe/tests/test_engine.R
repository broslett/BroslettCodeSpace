#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# test_engine.R -- self-contained checks for the transformation, join and
# pipeline engines. No test framework needed:  Rscript tests/test_engine.R
# ---------------------------------------------------------------------------

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(dirname(normalizePath(sub("^--file=", "", fa[1])))) else getwd()
})
options(datapipe.app_dir = app_dir)
suppressPackageStartupMessages({ library(jsonlite); library(data.table) })
for (f in c("utils.R", "i18n.R", "ops.R", "io.R", "pipeline.R")) {
  source(file.path(app_dir, "R", f))
}
dp_set_language("en", file.path(app_dir, "locale"))

PASS <- 0L; FAIL <- 0L; FAILURES <- character(0)

ok <- function(label, cond) {
  if (isTRUE(cond)) {
    PASS <<- PASS + 1L
    cat("  ok   ", label, "\n", sep = "")
  } else {
    FAIL <<- FAIL + 1L
    FAILURES <<- c(FAILURES, label)
    cat("  FAIL ", label, "\n", sep = "")
  }
}

eq <- function(label, actual, expected) {
  same <- identical(as.character(actual), as.character(expected))
  if (!same) {
    cat("  FAIL ", label, "\n         expected: ", paste(expected, collapse = " | "),
        "\n         actual:   ", paste(actual, collapse = " | "), "\n", sep = "")
    FAIL <<- FAIL + 1L; FAILURES <<- c(FAILURES, label)
  } else {
    PASS <<- PASS + 1L; cat("  ok   ", label, "\n", sep = "")
  }
}

throws <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  ok(label, !is.null(err))
}

section <- function(x) cat("\n", x, "\n", sep = "")

# Cheap content fingerprint of a data frame, for "did the re-run match" checks.
digest_chr <- function(df) {
  paste(paste(names(df), collapse = "|"),
        paste(vapply(df, function(c) paste(ifelse(is.na(c), "<NA>", as.character(c)),
                                           collapse = ","), character(1)), collapse = "||"),
        sep = "##")
}

step <- function(op, ...) list(op = op, params = list(...))

# ===========================================================================
section("Transformation operations")

eq("trim both", tf_apply_steps(c("  a  ", "b"), list(step("trim"))), c("a", "b"))
eq("trim left only", tf_apply_steps(" a ", list(step("trim", side = "left"))), "a ")
eq("squish", tf_apply_steps("  a   b  ", list(step("squish"))), "a b")
eq("upper", tf_apply_steps("aB", list(step("case", to = "upper"))), "AB")
eq("title case", tf_apply_steps("ada LOVELACE", list(step("case", to = "title"))), "Ada Lovelace")
eq("sentence case", tf_apply_steps("hELLO world", list(step("case", to = "sentence"))), "Hello world")

eq("digits only", tf_apply_steps(c("AC-000123 ", "x9y8"), list(step("digits_only"))), c("000123", "98"))
eq("digits only keeps decimal", tf_apply_steps("$1,250.50", list(step("digits_only", keep_decimal = TRUE))), "1250.50")
eq("digits only keeps sign", tf_apply_steps("-42abc", list(step("digits_only", keep_sign = TRUE))), "-42")
eq("letters only", tf_apply_steps("a1b2 c", list(step("letters_only"))), "abc")
eq("alnum keeps spaces", tf_apply_steps("a-1 b!", list(step("alnum_only", keep_spaces = TRUE))), "a1 b")
eq("remove chars", tf_apply_steps("AC-00/12.3", list(step("remove_chars", chars = "-/."))), "AC00123")
eq("keep chars", tf_apply_steps("AC-000123", list(step("keep_chars", chars = "0123456789"))), "000123")

eq("prefix", tf_apply_steps("123", list(step("prefix", text = "AC-"))), "AC-123")
eq("prefix skips blank", tf_apply_steps("", list(step("prefix", text = "AC-", skip_blank = TRUE))), "")
eq("suffix", tf_apply_steps("123", list(step("suffix", text = "-UK"))), "123-UK")
eq("remove prefix ignores case",
   tf_apply_steps("ac-123", list(step("remove_prefix", text = "AC-", ignore_case = TRUE))), "123")
eq("remove suffix", tf_apply_steps("file.csv", list(step("remove_suffix", text = ".csv"))), "file")

eq("replace literal", tf_apply_steps("a.b.c", list(step("replace", find = ".", replace = "-"))), "a-b-c")
eq("replace regex", tf_apply_steps("a1b22c", list(step("replace", find = "[0-9]+", replace = "#", regex = TRUE))), "a#b#c")
eq("replace first only",
   tf_apply_steps("a.b.c", list(step("replace", find = ".", replace = "-", first_only = TRUE))), "a-b.c")

eq("left pad", tf_apply_steps("123", list(step("left_pad", width = 8, char = "0"))), "00000123")
eq("left pad leaves longer alone", tf_apply_steps("123456789", list(step("left_pad", width = 8))), "123456789")
eq("right pad", tf_apply_steps("ab", list(step("right_pad", width = 4, char = "."))), "ab..")
eq("strip leading zeros", tf_apply_steps(c("000123", "000", "0"), list(step("strip_leading_zeros"))), c("123", "0", "0"))
eq("left n", tf_apply_steps("abcdef", list(step("left", n = 3))), "abc")
eq("right n", tf_apply_steps("abcdef", list(step("right", n = 2))), "ef")
eq("right n longer than value", tf_apply_steps("ab", list(step("right", n = 5))), "ab")
eq("substring", tf_apply_steps("abcdef", list(step("substring", start = 2, len = 3))), "bcd")
eq("substring to end", tf_apply_steps("abcdef", list(step("substring", start = 4, len = 0))), "def")
eq("truncate", tf_apply_steps("abcdef", list(step("truncate", width = 3))), "abc")

eq("regex extract group",
   tf_apply_steps("INV-2026-77", list(step("regex_extract", pattern = "([0-9]{4})", group = 1))), "2026")
eq("regex extract no match blank",
   tf_apply_steps("none", list(step("regex_extract", pattern = "([0-9]+)", group = 1))), "")
eq("regex extract no match keeps original",
   tf_apply_steps("none", list(step("regex_extract", pattern = "([0-9]+)", group = 1,
                                    on_no_match = "keep original"))), "none")
eq("split take first", tf_apply_steps("a-b-c", list(step("split_take", sep = "-", index = 1))), "a")
eq("split take last", tf_apply_steps("a-b-c", list(step("split_take", sep = "-", index = -1))), "c")
eq("split take out of range", tf_apply_steps("a-b", list(step("split_take", sep = "-", index = 9))), "")

eq("format number", tf_apply_steps(c("3,400", "12.5"), list(step("format_number", decimals = 2))), c("3400.00", "12.50"))
eq("format number non-numeric blank", tf_apply_steps("n/a", list(step("format_number"))), "")
eq("format number keeps original on fail",
   tf_apply_steps("n/a", list(step("format_number", on_fail = "keep original"))), "n/a")
eq("round number", tf_apply_steps("3.14159", list(step("round_number", decimals = 2))), "3.14")

eq("date dd/mm/yyyy", tf_apply_steps("05/03/2026", list(step("format_date", input_format = "%d/%m/%Y"))), "2026-03-05")
eq("date autodetect", tf_apply_steps("2026-03-05", list(step("format_date"))), "2026-03-05")
eq("date output format",
   tf_apply_steps("2026-03-05", list(step("format_date", output_format = "%d %b %Y"))), format(as.Date("2026-03-05"), "%d %b %Y"))
eq("date excel serial", tf_apply_steps("45000", list(step("format_date"))), "2023-03-15")
eq("date not a date", tf_apply_steps("hello", list(step("format_date"))), "")

eq("fill na", tf_apply_steps(c(NA, "", "x"), list(step("fill_na", value = "-"))), c("-", "-", "x"))
eq("set constant", tf_apply_steps(c("a", "b"), list(step("set_constant", value = "Z"))), c("Z", "Z"))

ok("blank to na", is.na(tf_apply_steps("   ", list(step("blank_to_na"))))[1])
ok("NA survives transformations", is.na(tf_apply_steps(NA_character_, list(step("trim"), step("case", to = "upper"))))[1])
ok("NA is not stringified as 'NA'",
   !identical(tf_apply_steps(NA_character_, list(step("digits_only")))[1], "NA"))

eq("chained: the realistic key fix",
   tf_apply_steps(c("AC-000123 ", "ac/456", "  789"),
                  list(step("trim"), step("case", to = "upper"),
                       step("digits_only"), step("left_pad", width = 8, char = "0"))),
   c("00000123", "00000456", "00000789"))

throws("unknown op is an error", tf_apply_steps("x", list(step("no_such_op"))))
ok("every op has label/desc/params/fn",
   all(vapply(TF_OPS, function(o) all(c("label", "desc", "params", "fn", "group") %in% names(o)), logical(1))))
ok("every op runs with its own defaults",
   all(vapply(names(TF_OPS), function(nm) {
     r <- tryCatch(tf_apply_step(c("Test-123 ", NA, ""), list(op = nm, params = tf_op_defaults(nm))),
                   error = function(e) NULL)
     !is.null(r) && length(r) == 3L
   }, logical(1))))
ok("step labels render", all(nzchar(vapply(names(TF_OPS), function(nm)
  tf_step_label(list(op = nm, params = tf_op_defaults(nm))), character(1)))))

# ===========================================================================
section("Key building")

df <- data.frame(a = c("1", "1", NA, ""), b = c("x", "y", "z", ""), stringsAsFactors = FALSE)
k <- dp_build_key(df, c("a", "b"))
ok("composite keys differ", k[1] != k[2])
ok("NA and blank are treated the same way in keys",
   dp_build_key(data.frame(a = NA_character_, stringsAsFactors = FALSE), "a", match_blanks = TRUE) ==
     dp_build_key(data.frame(a = "", stringsAsFactors = FALSE), "a", match_blanks = TRUE))
k2 <- dp_build_key(data.frame(a = c("", ""), stringsAsFactors = FALSE), "a")
ok("two blank keys do not collide", k2[1] != k2[2])
k3 <- dp_build_key(data.frame(a = c("", ""), stringsAsFactors = FALSE), "a", match_blanks = TRUE)
ok("blank keys collide when asked to", k3[1] == k3[2])
ok("case-insensitive key folds case",
   dp_build_key(data.frame(a = "AB", stringsAsFactors = FALSE), "a", ignore_case = TRUE) ==
     dp_build_key(data.frame(a = "ab", stringsAsFactors = FALSE), "a", ignore_case = TRUE))
throws("missing key field errors", dp_build_key(df, "nope"))

# ===========================================================================
section("Joins")

L <- data.frame(id = c("1", "2", "3"), v = c("a", "b", "c"), stringsAsFactors = FALSE)
R <- data.frame(rid = c("1", "2", "4"), name = c("one", "two", "four"),
                extra = c("p", "q", "r"), stringsAsFactors = FALSE)
J <- function(...) list(right_source_id = "r", keys = list(list(left = "id", right = "rid")), ...)

res <- dp_apply_join(L, R, J(type = "left"))
eq("left join keeps all left rows", nrow(res$data), 3)
eq("left join brings columns", sort(setdiff(names(res$data), names(L))), c("extra", "name"))
eq("left join values", res$data$name, c("one", "two", NA))
eq("left join preserves left order", res$data$v, c("a", "b", "c"))
eq("left join unmatched count", res$stats$unmatched, 1)

eq("inner join drops unmatched", nrow(dp_apply_join(L, R, J(type = "inner"))$data), 2)
eq("full join keeps both sides", nrow(dp_apply_join(L, R, J(type = "full"))$data), 4)
eq("semi join filters", nrow(dp_apply_join(L, R, J(type = "semi"))$data), 2)
eq("semi join adds no columns", ncol(dp_apply_join(L, R, J(type = "semi"))$data), 2)
eq("anti join keeps non-matches", dp_apply_join(L, R, J(type = "anti"))$data$id, "3")

res <- dp_apply_join(L, R, J(type = "left", select = list("name")))
ok("select limits incoming columns", !"extra" %in% names(res$data) && "name" %in% names(res$data))

res <- dp_apply_join(L, R, J(type = "left", select = list("name"), prefix = "cust_"))
ok("prefix applied", "cust_name" %in% names(res$data))
res <- dp_apply_join(L, R, J(type = "left", select = list("name"), suffix = "_lkp"))
ok("suffix applied", "name_lkp" %in% names(res$data))

# duplicate lookup keys
Rd <- data.frame(rid = c("1", "1", "2"), name = c("one", "ONE", "two"), stringsAsFactors = FALSE)
eq("duplicates multiply rows by default", nrow(dp_apply_join(L, Rd, J(type = "left"))$data), 4)
res <- dp_apply_join(L, Rd, J(type = "left", multi_match = "first"))
eq("keep first collapses duplicates", nrow(res$data), 3)
eq("keep first takes the first value", res$data$name, c("one", "two", NA))
throws("duplicate lookup can be an error", dp_apply_join(L, Rd, J(type = "left", multi_match = "error")))

# name conflicts
Rc <- data.frame(rid = c("1", "2"), v = c("VV", "WW"), stringsAsFactors = FALSE)
res <- dp_apply_join(L, Rc, J(type = "left"))
ok("conflict default suffixes", "v_2" %in% names(res$data))
eq("original column untouched by suffix", res$data$v, c("a", "b", "c"))
res <- dp_apply_join(L, Rc, J(type = "left", conflict = "right_wins"))
eq("right_wins overwrites", res$data$v, c("VV", "WW", NA))
Lb <- data.frame(id = c("1", "2", "3"), v = c("", "b", "c"), stringsAsFactors = FALSE)
res <- dp_apply_join(Lb, Rc, J(type = "left", conflict = "coalesce"))
eq("coalesce fills blanks only", res$data$v, c("VV", "b", "c"))
res <- dp_apply_join(L, Rc, J(type = "left", conflict = "skip"))
eq("skip ignores incoming column", res$data$v, c("a", "b", "c"))

# case-insensitive join
Li <- data.frame(id = "AB", stringsAsFactors = FALSE)
Ri <- data.frame(rid = "ab", name = "hit", stringsAsFactors = FALSE)
eq("case-sensitive join misses", dp_apply_join(Li, Ri, J(type = "left"))$data$name, NA)
eq("ignore_case join matches",
   dp_apply_join(Li, Ri, J(type = "left", ignore_case = TRUE))$data$name, "hit")

# blank keys must not join to each other
Lb2 <- data.frame(id = c("", "1"), stringsAsFactors = FALSE)
Rb2 <- data.frame(rid = c("", "1"), name = c("blankmatch", "one"), stringsAsFactors = FALSE)
eq("blank keys do not match", dp_apply_join(Lb2, Rb2, J(type = "left"))$data$name, c(NA, "one"))

# multi-field join
Lm <- data.frame(a = c("1", "1"), b = c("x", "y"), stringsAsFactors = FALSE)
Rm <- data.frame(c1 = "1", c2 = "y", tag = "found", stringsAsFactors = FALSE)
res <- dp_apply_join(Lm, Rm, list(keys = list(list(left = "a", right = "c1"),
                                              list(left = "b", right = "c2")), type = "left"))
eq("two-field join matches only the right row", res$data$tag, c(NA, "found"))

# ===========================================================================
section("Final shaping")

fin <- data.frame(name = c("b", "a", "c", "a"), n = c("2", "10", "3", "10"),
                  blank = c("", "x", "", "y"), stringsAsFactors = FALSE)

eq("filter equals", nrow(dp_apply_filter(fin, list(mode = "all",
     conditions = list(list(field = "name", op = "equals", value = "a"))))), 2)
eq("filter not_blank", nrow(dp_apply_filter(fin, list(mode = "all",
     conditions = list(list(field = "blank", op = "not_blank"))))), 2)
eq("filter numeric gt", nrow(dp_apply_filter(fin, list(mode = "all",
     conditions = list(list(field = "n", op = "gt", value = "5"))))), 2)
eq("filter in_list", nrow(dp_apply_filter(fin, list(mode = "all",
     conditions = list(list(field = "name", op = "in_list", value = "a, c"))))), 3)
eq("filter mode any", nrow(dp_apply_filter(fin, list(mode = "any",
     conditions = list(list(field = "name", op = "equals", value = "b"),
                       list(field = "name", op = "equals", value = "c"))))), 2)
eq("filter mode all is an AND", nrow(dp_apply_filter(fin, list(mode = "all",
     conditions = list(list(field = "name", op = "equals", value = "b"),
                       list(field = "name", op = "equals", value = "c"))))), 0)
throws("filter on missing field errors",
       dp_apply_filter(fin, list(mode = "all", conditions = list(list(field = "zz", op = "equals", value = "1")))))

eq("sort ascending text", dp_apply_sort(fin, list(list(field = "name", dir = "asc")))$name,
   c("a", "a", "b", "c"))
eq("sort descending text", dp_apply_sort(fin, list(list(field = "name", dir = "desc")))$name,
   c("c", "b", "a", "a"))
eq("sort numeric respects magnitude",
   dp_apply_sort(fin, list(list(field = "n", dir = "asc", numeric = TRUE)))$n, c("2", "3", "10", "10"))
eq("sort as text is lexical",
   dp_apply_sort(fin, list(list(field = "n", dir = "asc")))$n, c("10", "10", "2", "3"))

eq("dedupe by key", nrow(dp_apply_dedupe(fin, list(enabled = TRUE, keys = list("name")))), 3)
eq("dedupe disabled is a no-op", nrow(dp_apply_dedupe(fin, list(enabled = FALSE, keys = list("name")))), 4)
dd_first <- dp_apply_dedupe(fin, list(enabled = TRUE, keys = list("name"), keep = "first"))
dd_last  <- dp_apply_dedupe(fin, list(enabled = TRUE, keys = list("name"), keep = "last"))
eq("dedupe keep first takes the earlier row", dd_first$blank[dd_first$name == "a"], "x")
eq("dedupe keep last takes the later row", dd_last$blank[dd_last$name == "a"], "y")

comb <- data.frame(f = c("Ada", "Grace"), l = c("Lovelace", ""), stringsAsFactors = FALSE)
eq("combine fields", dp_field_value(comb, list(combine = list("f", "l"), combine_sep = " ")),
   c("Ada Lovelace", "Grace"))
throws("missing output source errors", dp_field_value(comb, list(source = "nope")))

# ===========================================================================
section("Round-trip: readers and writers")

tmp <- file.path(tempdir(), "dp_io")
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

zdf <- data.frame(key = c("000123", "000456"), big = c("12345678901234", "2"),
                  txt = c("a,b", 'say "hi"'), stringsAsFactors = FALSE)
p <- file.path(tmp, "z.csv")
dp_write_file(zdf, p, "csv")
back <- dp_read_file(p, "csv")
eq("csv round-trip preserves leading zeros", back$key, zdf$key)
eq("csv round-trip preserves long numbers", back$big, zdf$big)
eq("csv round-trip preserves embedded commas and quotes", back$txt, zdf$txt)

p2 <- file.path(tmp, "z.xlsx")
dp_write_file(zdf, p2, "excel")
back2 <- dp_read_file(p2, "excel")
eq("excel round-trip preserves leading zeros", back2$key, zdf$key)
eq("excel round-trip preserves long numbers", back2$big, zdf$big)

p3 <- file.path(tmp, "z.tsv")
dp_write_file(zdf, p3, "tsv")
eq("tsv round-trip", dp_read_file(p3, "tsv")$key, zdf$key)

writeLines(c("a|b", "1|2"), file.path(tmp, "pipe.txt"))
eq("custom delimiter", dp_read_file(file.path(tmp, "pipe.txt"), "delimited",
                                    list(delimiter = "|"))$b, "2")

writeLines(c("a,b", "1,NA", "2,N/A"), file.path(tmp, "na.csv"))
nadf <- dp_read_file(file.path(tmp, "na.csv"), "csv")
ok("NA strings normalised on read", all(is.na(nadf$b)))

writeLines(c("x,x,y", "1,2,3"), file.path(tmp, "dup.csv"))
eq("duplicate headers made unique", names(dp_read_file(file.path(tmp, "dup.csv"), "csv")),
   c("x", "x_1", "y"))

# Accented text must survive byte-for-byte. R escapes non-ASCII as <c3><a4> in
# a "C" locale, which silently corrupts names and addresses.
utf8_path <- file.path(tmp, "utf8.csv")
con <- file(utf8_path, "wb")
writeBin(charToRaw("name,city\nSp\xc3\xa4rck,M\xc3\xbcnchen\nPlain,Town\n"), con)
close(con)
u <- dp_read_file(utf8_path, "csv")
eq("accented text read intact",
   paste(sprintf("%02x", as.integer(charToRaw(u$name[1]))), collapse = " "),
   "53 70 c3 a4 72 63 6b")
ok("accented text is marked UTF-8", Encoding(u$name[1]) == "UTF-8")
ok("accented text is not escaped", !grepl("<c3>", u$name[1], fixed = TRUE))
utf8_out <- file.path(tmp, "utf8_out.csv")
dp_write_file(u, utf8_out, "csv")
ok("accented text survives a csv round-trip",
   identical(readBin(utf8_path, "raw", 200), readBin(utf8_out, "raw", 200)))
utf8_xl <- file.path(tmp, "utf8.xlsx")
dp_write_file(u, utf8_xl, "excel")
ok("accented text survives an excel round-trip",
   identical(dp_read_file(utf8_xl, "excel")$name[1], u$name[1]))
eq("transformations preserve accented text",
   tf_apply_steps(u$name[1], list(step("trim"), step("case", to = "upper"))), "SPÄRCK")
eq("remove accents folds to ascii",
   tf_apply_steps(u$name[1], list(step("strip_accents"))), "Sparck")

eq("format detection", c(dp_detect_format("a.xlsx"), dp_detect_format("a.csv"),
                         dp_detect_format("a.tsv"), dp_detect_format("A.XLSX")),
   c("excel", "csv", "tsv", "excel"))

# ===========================================================================
section("Pipeline save / load / validate")

spec <- dp_new_pipeline("Test")
eq("empty pipeline is invalid", length(dp_validate(spec)) > 0, TRUE)

spec$sources <- list(dp_new_source("A", "examples/data/transactions.csv"))
spec$joins$base_source_id <- spec$sources[[1]]$id
ok("single-source pipeline validates", length(dp_validate(spec)) == 0)

spec$final$fields <- list(list(source = "X", output = "Same", include = TRUE),
                          list(source = "Y", output = "Same", include = TRUE))
ok("duplicate output names rejected", any(grepl("Duplicate", dp_validate(spec))))
spec$final$fields <- list()

pf <- file.path(tmp, "p.json")
dp_save_pipeline(spec, pf)
loaded <- dp_load_pipeline(pf)
eq("save/load keeps the name", loaded$name, spec$name)
eq("save/load keeps sources", length(loaded$sources), 1)
eq("save/load keeps the base table", scalar(loaded$joins$base_source_id), scalar(spec$joins$base_source_id))

writeLines('{"name":"Sparse"}', file.path(tmp, "sparse.json"))
sparse <- dp_load_pipeline(file.path(tmp, "sparse.json"))
ok("sparse pipeline file is filled in", !is.null(sparse$final$fields) && !is.null(sparse$export$path))
writeLines('{"name":"Future","spec_version":99}', file.path(tmp, "future.json"))
throws("future spec version is rejected", dp_load_pipeline(file.path(tmp, "future.json")))
throws("missing pipeline file errors", dp_load_pipeline(file.path(tmp, "nope.json")))

ok("relativise makes paths portable",
   relativise_path(file.path(tmp, "z.csv"), tmp) == "z.csv")
ok("resolve_path rebuilds an absolute path",
   normalizePath(resolve_path("z.csv", tmp)) == normalizePath(file.path(tmp, "z.csv")))
ok("absolute paths are left alone", resolve_path("/etc/hosts", tmp) == "/etc/hosts")

# ===========================================================================
section("End-to-end: the example pipeline")

ex <- file.path(app_dir, "pipelines", "example_monthly_sales.json")
if (file.exists(ex)) {
  spec <- dp_load_pipeline(ex)
  probs <- dp_validate(spec)
  ok("example pipeline validates", length(probs) == 0)
  if (length(probs)) for (p in probs) cat("        - ", p, "\n", sep = "")

  outdir <- file.path(tmp, "e2e")
  spec$export$path <- file.path(outdir, "result.csv")
  res <- tryCatch(dp_execute(spec, root = app_dir), error = function(e) {
    cat("        error: ", conditionMessage(e), "\n", sep = ""); NULL
  })
  ok("example pipeline runs", !is.null(res))
  if (!is.null(res)) {
    d <- res$data
    ok("export file created", file.exists(res$export_path))
    ok("output has rows", nrow(d) > 0)
    eq("output columns as configured",
       names(d), c("Reference", "Account", "Customer", "Region", "Manager",
                   "Product", "Category", "Amount", "Date"))
    ok("keys were normalised and matched",
       sum(!is.na(d$Customer) & d$Customer != "") >= 8)
    ok("account keys are zero-padded to 8", all(nchar(d$Account) == 8))
    ok("dates are ISO formatted", all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d$Date)))
    ok("amounts have two decimals", all(grepl("^[0-9]+\\.[0-9]{2}$", d$Amount)))
    ok("void rows filtered out", nrow(d) < 12)
    ok("mapping duplicate handled (no row explosion)", nrow(d) <= 12)
    ok("run log is populated", length(res$log) >= 4)

    # Re-running against a different root must not touch the first output.
    res2 <- dp_execute(spec, root = app_dir)
    eq("re-run is deterministic", nrow(res2$data), nrow(d))
    eq("re-run produces identical data", digest_chr(res2$data), digest_chr(d))
  }
} else {
  cat("  skip  example pipeline not found at ", ex, "\n", sep = "")
}

# ===========================================================================
section("Re-running out of a different set of folders")

# The portability promise: the same pipeline file, pointed at another month's
# folder via --root, picks up that month's files with no edits.
ex <- file.path(app_dir, "pipelines", "example_monthly_sales.json")
if (file.exists(ex)) {
  spec <- dp_load_pipeline(ex)
  month2 <- file.path(tmp, "month2")
  dir.create(file.path(month2, "examples", "data"), recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(file.path(app_dir, "examples", "data"), full.names = TRUE)) {
    file.copy(f, file.path(month2, "examples", "data", basename(f)), overwrite = TRUE)
  }
  # Give the second folder one extra transaction so the two runs differ.
  tf <- file.path(month2, "examples", "data", "transactions.csv")
  lines <- readLines(tf)
  writeLines(c(lines, '"TX0013","AC-000456","WID-01","500","30/03/2026","posted"'), tf)

  spec$export$path <- "output/m2.csv"
  r2 <- tryCatch(dp_execute(spec, root = month2), error = function(e) {
    cat("        error: ", conditionMessage(e), "\n", sep = ""); NULL })
  ok("same pipeline runs against another folder", !is.null(r2))
  if (!is.null(r2)) {
    ok("output landed under the new root", startsWith(normalizePath(r2$export_path), normalizePath(month2)))
    eq("the extra row came through", nrow(r2$data), 12)
    ok("the original folder was left untouched",
       nrow(dp_read_file(file.path(app_dir, "examples", "data", "transactions.csv"), "csv")) == 12)
  }

  # Folder + pattern mode: same idea, but the filename may change each month.
  spec2 <- spec
  spec2$sources[[1]]$path_mode <- "pattern"
  spec2$sources[[1]]$dir <- "examples/data"
  spec2$sources[[1]]$pattern <- "transactions*.csv"
  spec2$sources[[1]]$pick <- "latest"
  spec2$export$path <- "output/m2_pattern.csv"
  r3 <- tryCatch(dp_execute(spec2, root = month2), error = function(e) {
    cat("        error: ", conditionMessage(e), "\n", sep = ""); NULL })
  ok("folder + pattern source resolves", !is.null(r3))
  if (!is.null(r3)) eq("pattern run matches the exact-file run", nrow(r3$data), 12)

  # 'latest' really means most recently modified.
  newer <- file.path(month2, "examples", "data", "transactions_april.csv")
  file.copy(tf, newer, overwrite = TRUE)
  cat('"TX0014","AC-000123","WID-01","1","01/04/2026","posted"\n', file = newer, append = TRUE)
  Sys.setFileTime(newer, Sys.time() + 60)
  spec2$export$path <- "output/m2_latest.csv"
  r4 <- tryCatch(dp_execute(spec2, root = month2), error = function(e) NULL)
  ok("'newest match' picks up the newer file", !is.null(r4) && nrow(r4$data) == 13)

  spec2$sources[[1]]$pick <- "all"
  spec2$export$path <- "output/m2_all.csv"
  r5 <- tryCatch(dp_execute(spec2, root = month2), error = function(e) NULL)
  ok("'stack them all' concatenates the matches", !is.null(r5) && nrow(r5$data) == 25)

  spec3 <- spec2
  spec3$sources[[1]]$pattern <- "nothing_matches_*.csv"
  throws("a pattern that matches nothing is a clear error", dp_execute(spec3, root = month2))
}

# ===========================================================================
cat("\n", strrep("-", 60), "\n", sep = "")
cat("Passed: ", PASS, "   Failed: ", FAIL, "\n", sep = "")
if (FAIL > 0) {
  cat("\nFailed checks:\n")
  for (f in FAILURES) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("All checks passed.\n")
quit(status = 0)
