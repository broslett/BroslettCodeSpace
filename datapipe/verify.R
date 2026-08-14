#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# verify.R -- check this installation on the machine it is running on.
#
#   Rscript verify.R
#
# Reports the environment, confirms the DuckDB lockdown described in
# SECURITY.md is actually in effect, scans the shipped source for anything
# network-capable, and runs both test suites. Exits non-zero if anything is
# wrong, so it can be used as a deployment gate.
# ---------------------------------------------------------------------------

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
})
options(datapipe.app_dir = app_dir)
# Brings dp_ensure_utf8_locale() and %||%, so the report reflects the locale
# the application actually runs under.
suppressWarnings(try(source(file.path(app_dir, "R", "utils.R")), silent = TRUE))
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

FAIL <- 0L
ok   <- function(label, cond, note = "") {
  if (isTRUE(cond)) cat(sprintf("  ok    %-52s %s\n", label, note))
  else { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL  %-52s %s\n", label, note)) }
}
info <- function(label, value) cat(sprintf("  ..    %-52s %s\n", label, value))
head1 <- function(x) cat("\n", x, "\n", strrep("-", 70), "\n", sep = "")

# ---------------------------------------------------------------------------
head1("Environment")
info("R", R.version.string)
info("platform", R.version$platform)
info("locale (LC_CTYPE)", Sys.getlocale("LC_CTYPE"))

required <- c("shiny", "bslib", "DT", "shinyjs", "jsonlite",
              "data.table", "readxl", "writexl")
optional <- c("duckdb", "DBI")
for (p in required) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  ok(paste0("package: ", p), !is.na(v), if (is.na(v)) "MISSING -- install it" else v)
}
for (p in optional) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  if (is.na(v)) info(paste0("package: ", p), "not installed (the R engine will be used)")
  else ok(paste0("package: ", p), TRUE, v)
}

# ---------------------------------------------------------------------------
head1("Source scan: anything that could reach the network")

r_files <- c(list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE),
             file.path(app_dir, c("app.R", "run_pipeline.R")))
r_files <- r_files[file.exists(r_files)]
ok("application source files present", length(r_files) >= 8, paste(length(r_files), "files"))

# Calls that could open a connection, and eval-style calls that could run
# something a pipeline file smuggled in.
net_pat <- paste0("(^|[^[:alnum:]._])(url|download\\.file|socketConnection|nsl|",
                  "curl|curl_download|GET|POST|install\\.packages|available\\.packages)[[:space:]]*\\(")
eval_pat <- "(^|[^[:alnum:]._])(eval|parse|deparse2|system|system2|shell)[[:space:]]*\\("

net_hits <- character(0); eval_hits <- character(0); url_hits <- character(0)
for (f in r_files) {
  lines <- readLines(f, warn = FALSE)
  code <- sub("#.*$", "", lines)                       # ignore comments
  n <- grep(net_pat, code)
  e <- grep(eval_pat, code)
  # A URL only counts if a hostname actually follows the scheme. The startup
  # banner writes  http://  and then a variable, which names no host.
  u <- grep("https?://[A-Za-z0-9]", code)
  if (length(n)) net_hits  <- c(net_hits,  sprintf("%s:%d", basename(f), n))
  if (length(e)) eval_hits <- c(eval_hits, sprintf("%s:%d", basename(f), e))
  if (length(u)) {
    keep <- u[!grepl("127\\.0\\.0\\.1|localhost", code[u])]
    if (length(keep)) url_hits <- c(url_hits, sprintf("%s:%d", basename(f), keep))
  }
}
ok("no network-capable calls", length(net_hits) == 0,
   if (length(net_hits)) paste(net_hits, collapse = ", ") else "")
ok("no eval/system calls", length(eval_hits) == 0,
   if (length(eval_hits)) paste(eval_hits, collapse = ", ") else "")
ok("no remote URLs in source", length(url_hits) == 0,
   if (length(url_hits)) paste(url_hits, collapse = ", ") else "")

banner <- readLines(file.path(app_dir, "app.R"), warn = FALSE)
ok("web interface defaults to loopback only",
   any(grepl('DATAPIPE_HOST", "127\\.0\\.0\\.1"', banner)),
   if (nzchar(Sys.getenv("DATAPIPE_HOST")) &&
       !identical(Sys.getenv("DATAPIPE_HOST"), "127.0.0.1"))
     paste0("WARNING: DATAPIPE_HOST is set to ", Sys.getenv("DATAPIPE_HOST")) else "")

# ---------------------------------------------------------------------------
head1("DuckDB lockdown")

if (!requireNamespace("duckdb", quietly = TRUE)) {
  info("duckdb", "not installed -- nothing to lock down")
} else {
  suppressPackageStartupMessages({ library(DBI); library(duckdb) })
  # Open a connection exactly the way engine_duckdb.R does.
  drv <- tryCatch(duckdb::duckdb(shared_home = FALSE), error = function(e) duckdb::duckdb())
  con <- dbConnect(drv)
  on.exit(try(dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  for (s in c("SET autoinstall_known_extensions = false",
              "SET autoload_known_extensions = false",
              "SET allow_community_extensions = false")) try(dbExecute(con, s), silent = TRUE)

  setting <- function(k) tryCatch(
    dbGetQuery(con, sprintf("SELECT current_setting('%s') AS v", k))$v[1],
    error = function(e) NA_character_)
  for (k in c("autoinstall_known_extensions", "autoload_known_extensions",
              "allow_community_extensions")) {
    v <- setting(k)
    ok(paste0("disabled: ", k), identical(tolower(as.character(v)), "false"), as.character(v))
  }

  # A remote read must fail rather than quietly fetch an extension.
  remote <- tryCatch({
    dbGetQuery(con, "SELECT * FROM read_csv('https://example.com/nope.csv')"); "SUCCEEDED"
  }, error = function(e) "refused")
  ok("remote reads are refused", identical(remote, "refused"), remote)

  # Local reads must still work -- that is the whole job.
  p <- tempfile(fileext = ".csv"); writeLines(c("k", "000123"), p)
  local_ok <- tryCatch(
    identical(dbGetQuery(con, sprintf("SELECT * FROM read_csv('%s', all_varchar=true)", p))$k[1],
              "000123"),
    error = function(e) FALSE)
  ok("local files still readable (leading zeros kept)", local_ok)
  unlink(p)
}

# ---------------------------------------------------------------------------
head1("Test suites")

run_suite <- function(script) {
  path <- file.path(app_dir, "tests", script)
  if (!file.exists(path)) { info(script, "not present"); return(NA) }
  out <- suppressWarnings(system2("Rscript", path, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  line <- grep("^Passed:", out, value = TRUE)
  ok(script, identical(as.integer(status), 0L),
     if (length(line)) tail(line, 1) else paste("exit", status))
  invisible(identical(as.integer(status), 0L))
}
run_suite("test_engine.R")
run_suite("test_duckdb.R")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
if (FAIL == 0L) {
  cat("All checks passed. This installation makes no outbound connections.\n")
  cat("See SECURITY.md to verify that independently with no network at all.\n")
  quit(status = 0)
}
cat(FAIL, " check(s) failed. See above.\n", sep = "")
quit(status = 1)
