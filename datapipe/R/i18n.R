# ---------------------------------------------------------------------------
# i18n.R -- every user-visible string lives in locale/<lang>.json so the app
# can be translated (or reworded) without touching R code.
# ---------------------------------------------------------------------------

.dp_i18n <- new.env(parent = emptyenv())
.dp_i18n$strings <- list()
.dp_i18n$lang <- "en"
.dp_i18n$dir <- NULL

dp_locale_dir <- function() {
  .dp_i18n$dir %||% file.path(dirname(dp_app_dir()), "locale")
}

# Directory that holds this app (works from app.R, the CLI runner and tests).
dp_app_dir <- function() {
  d <- getOption("datapipe.app_dir", NULL)
  if (!is.null(d)) return(d)
  getwd()
}

dp_available_locales <- function(dir = NULL) {
  dir <- dir %||% dp_locale_dir()
  if (!dir.exists(dir)) return(c(en = "en"))
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(c(en = "en"))
  codes <- sub("\\.json$", "", basename(files))
  labels <- vapply(files, function(f) {
    j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(e) list())
    scalar(j[["_language_name"]], NA_character_) %||% NA_character_
  }, character(1))
  labels[is.na(labels)] <- codes[is.na(labels)]
  stats::setNames(codes, labels)
}

dp_set_language <- function(lang = "en", dir = NULL) {
  dir <- dir %||% dp_locale_dir()
  path <- file.path(dir, paste0(lang, ".json"))
  if (!file.exists(path)) {
    if (lang != "en") return(dp_set_language("en", dir))
    .dp_i18n$strings <- list()
    .dp_i18n$lang <- "en"
    return(invisible(FALSE))
  }
  .dp_i18n$strings <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  .dp_i18n$lang <- lang
  .dp_i18n$dir <- dir
  invisible(TRUE)
}

dp_language <- function() .dp_i18n$lang

# tr("some.key", name = "x") -> looked up string with {name} substituted.
# Falls back to the key itself so a missing translation is never fatal.
tr <- function(key, ...) {
  val <- .dp_i18n$strings[[key]]
  if (is.null(val) || !is.character(val) || length(val) != 1) val <- key
  args <- list(...)
  for (nm in names(args)) {
    val <- gsub(paste0("{", nm, "}"), as.character(args[[nm]]), val, fixed = TRUE)
  }
  val
}
