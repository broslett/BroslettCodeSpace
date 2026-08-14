#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# install_packages.R -- install what the application needs.
#
#   Rscript setup/install_packages.R                 # from CRAN
#   Rscript setup/install_packages.R /path/to/pkgs   # from a local folder
#
# The second form needs no internet: point it at a folder of package files
# downloaded elsewhere. See OFFLINE-INSTALL.md.
#
# This is the only part of the process that ever touches the network, and only
# in the first form. The application itself never does.
# ---------------------------------------------------------------------------

REQUIRED <- c("shiny", "bslib", "DT", "shinyjs", "jsonlite",
              "data.table", "readxl", "writexl")
OPTIONAL <- c("DBI", "duckdb")   # the faster engine; the app works without it

args <- commandArgs(trailingOnly = TRUE)
local_dir <- if (length(args)) args[1] else NULL

have <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))
missing_required <- REQUIRED[!vapply(REQUIRED, have, logical(1))]
missing_optional <- OPTIONAL[!vapply(OPTIONAL, have, logical(1))]

cat("R:        ", R.version.string, "\n", sep = "")
cat("library:  ", .libPaths()[1], "\n\n", sep = "")

if (!length(missing_required) && !length(missing_optional)) {
  cat("Everything is already installed. Run:  Rscript verify.R\n")
  quit(status = 0)
}

if (!is.null(local_dir)) {
  # ---- offline: install from files in a folder ----------------------------
  if (!dir.exists(local_dir)) {
    cat("No such folder: ", local_dir, "\n", sep = ""); quit(status = 2)
  }
  files <- list.files(local_dir, pattern = "\\.(tar\\.gz|tgz|zip)$", full.names = TRUE)
  if (!length(files)) {
    cat("No package files (.tar.gz / .zip) found in ", local_dir, "\n", sep = "")
    quit(status = 2)
  }
  cat("Installing ", length(files), " package file(s) from ", local_dir, "\n", sep = "")
  cat("(no network is used for this)\n\n")
  # type = "source" for .tar.gz, "binary"/"win.binary" handled by R from the
  # file extension; repos = NULL is what makes this a local install.
  utils::install.packages(files, repos = NULL, dependencies = FALSE)
} else {
  # ---- online: install from CRAN ------------------------------------------
  repo <- getOption("repos")[["CRAN"]]
  if (is.null(repo) || is.na(repo) || repo == "@CRAN@") repo <- "https://cloud.r-project.org"
  cat("Installing from ", repo, "\n", sep = "")
  cat("This is the only step that uses the internet.\n")
  cat("For a machine with no connection, see setup/OFFLINE-INSTALL.md\n\n")
  if (length(missing_required)) {
    cat("required: ", paste(missing_required, collapse = ", "), "\n", sep = "")
    utils::install.packages(missing_required, repos = repo)
  }
  if (length(missing_optional)) {
    cat("\noptional (DuckDB engine): ", paste(missing_optional, collapse = ", "), "\n", sep = "")
    cat("duckdb is a large C++ build and can take a while to compile from source.\n")
    ok <- tryCatch({ utils::install.packages(missing_optional, repos = repo); TRUE },
                   error = function(e) { cat("  could not install: ", conditionMessage(e), "\n", sep = ""); FALSE })
    if (!ok) cat("  Not fatal -- the application runs on its R engine without duckdb.\n")
  }
}

cat("\n--- result ---\n")
bad <- character(0)
for (p in c(REQUIRED, OPTIONAL)) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  need <- p %in% REQUIRED
  cat(sprintf("  %-12s %s\n", p,
              if (!is.na(v)) v else if (need) "MISSING (required)" else "not installed (optional)"))
  if (is.na(v) && need) bad <- c(bad, p)
}
if (length(bad)) {
  cat("\nStill missing: ", paste(bad, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
cat("\nReady. Next:  Rscript verify.R\n")
quit(status = 0)
