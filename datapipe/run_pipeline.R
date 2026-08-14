#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_pipeline.R -- run a saved pipeline without opening the app.
#
#   Rscript run_pipeline.R pipelines/monthly.json
#   Rscript run_pipeline.R pipelines/monthly.json --root /data/2026-08
#   Rscript run_pipeline.R pipelines/monthly.json --out custom/name.xlsx
#   Rscript run_pipeline.R pipelines/monthly.json --describe
#   Rscript run_pipeline.R --list
#
# This is the "re-run it out of the same folders" path: schedule it, or call
# it from a batch file, and it produces the same output the app would.
# ---------------------------------------------------------------------------

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
})
options(datapipe.app_dir = app_dir)

suppressPackageStartupMessages({
  library(jsonlite); library(data.table)
})
for (f in c("utils.R", "i18n.R", "ops.R", "io.R", "pipeline.R", "sql.R", "engine_duckdb.R")) {
  source(file.path(app_dir, "R", f), local = FALSE)
}
dp_set_language(Sys.getenv("DATAPIPE_LANG", "en"), file.path(app_dir, "locale"))

args <- commandArgs(trailingOnly = TRUE)

get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) && length(args) > i[1]) return(args[i[1] + 1])
  kv <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(kv)) return(sub(paste0("^--", name, "="), "", kv[1]))
  default
}
has_flag <- function(name) any(args == paste0("--", name))

usage <- function() {
  cat("Usage: Rscript run_pipeline.R <pipeline.json> [options]\n\n",
      "Options:\n",
      "  --root <dir>    Folder that relative paths resolve against (default: the\n",
      "                  pipeline file's parent's parent, i.e. the project folder)\n",
      "  --out <file>    Override the export path\n",
      "  --format <fmt>  Override the export format (csv, tsv, excel, delimited)\n",
      "  --describe      Print the pipeline outline and exit without running\n",
      "  --validate      Check the pipeline and exit without running\n",
      "  --quiet         Only print errors\n",
      "  --engine <e>    auto (default), duckdb, or r\n",
      "  --list          List pipelines in the pipelines/ folder and exit\n",
      sep = "")
}

if (has_flag("help") || has_flag("h")) { usage(); quit(status = 0) }

pipelines_dir <- file.path(app_dir, "pipelines")

if (has_flag("list")) {
  tb <- dp_list_pipelines(pipelines_dir)
  if (!nrow(tb)) {
    cat("No saved pipelines in ", pipelines_dir, "\n", sep = "")
  } else {
    cat("Saved pipelines in ", pipelines_dir, ":\n\n", sep = "")
    for (i in seq_len(nrow(tb))) {
      cat(sprintf("  %-28s %s\n", tb$file[i], tb$name[i]))
      if (nzchar(tb$description[i])) cat(sprintf("  %-28s   %s\n", "", tb$description[i]))
    }
  }
  quit(status = 0)
}

positional <- args[!grepl("^--", args)]
# Drop values that belong to a preceding flag.
flag_idx <- which(grepl("^--", args) & !grepl("=", args))
consumed <- intersect(flag_idx + 1, seq_along(args))
consumed <- consumed[!grepl("^--", args[consumed])]
value_flags <- c("root", "out", "format", "engine")
consumed <- consumed[vapply(consumed, function(i) sub("^--", "", args[i - 1]) %in% value_flags, logical(1))]
positional <- args[setdiff(which(!grepl("^--", args)), consumed)]

if (!length(positional)) {
  cat("Error: no pipeline file given.\n\n"); usage(); quit(status = 2)
}

pipeline_path <- positional[1]
if (!file.exists(pipeline_path)) {
  alt <- file.path(pipelines_dir, pipeline_path)
  if (file.exists(alt)) {
    pipeline_path <- alt
  } else if (file.exists(paste0(alt, ".json"))) {
    pipeline_path <- paste0(alt, ".json")
  } else {
    cat("Error: pipeline not found: ", pipeline_path, "\n", sep = ""); quit(status = 2)
  }
}

quiet <- has_flag("quiet")
say <- function(...) if (!quiet) cat(..., sep = "")

spec <- tryCatch(dp_load_pipeline(pipeline_path), error = function(e) {
  cat("Error: ", conditionMessage(e), "\n", sep = ""); quit(status = 2)
})

# Relative paths resolve against the project folder (the parent of pipelines/)
# unless the caller points somewhere else -- that override is what lets the
# same pipeline run against last month's and this month's folders.
root <- get_flag("root", normalizePath(dirname(dirname(pipeline_path)), mustWork = FALSE))

if (has_flag("describe")) {
  cat(paste(dp_describe(spec), collapse = "\n"), "\n", sep = "")
  quit(status = 0)
}

problems <- dp_validate(spec)
if (length(problems)) {
  cat("Pipeline is not ready to run:\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  quit(status = 1)
}
if (has_flag("validate")) { cat("Pipeline is valid.\n"); quit(status = 0) }

out_override <- get_flag("out", NULL)
if (!is.null(out_override)) {
  spec$export$path <- out_override
  spec$export$format <- dp_detect_format(out_override)
  spec$export$timestamp_filename <- FALSE
}
fmt_override <- get_flag("format", NULL)
if (!is.null(fmt_override)) spec$export$format <- fmt_override

engine <- get_flag("engine", NULL)
if (!is.null(engine) && !engine %in% c("auto", "duckdb", "r")) {
  cat("Error: --engine must be auto, duckdb or r.\n"); quit(status = 2)
}

say("Running '", scalar(spec$name, basename(pipeline_path)), "'\n")
say("  root:   ", root, "\n")
say("  engine: ", engine %||% scalar(spec$engine, "auto"),
    if (dp_duckdb_available()) paste0(" (duckdb ", dp_duckdb_version(), " available)")
    else " (duckdb not installed, using R)", "\n\n")

res <- tryCatch(
  dp_run(spec, root = root, engine = engine),
  error = function(e) {
    cat("\nFAILED: ", conditionMessage(e), "\n", sep = "")
    quit(status = 1)
  }
)

for (l in res$log) say("  ", l, "\n")
if (length(res$warnings)) {
  say("\nWarnings:\n")
  for (w in res$warnings) say("  ! ", w, "\n")
}
say("\nDone in ", sprintf("%.2f", res$elapsed), "s using the ",
    res$engine %||% "r", " engine -> ", res$export_path, "\n")
quit(status = 0)
