# ---------------------------------------------------------------------------
# utils.R -- small helpers shared by every part of the application.
# ---------------------------------------------------------------------------

# Containers and cron jobs frequently start in the "C" locale, where R escapes
# any non-ASCII text as <c3><a4> rather than carrying it through. Names and
# addresses are full of such characters, so switch to a UTF-8 locale if one is
# available before anything reads a file.
dp_ensure_utf8_locale <- function() {
  if (grepl("utf-?8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) return(invisible(TRUE))
  for (cand in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_GB.UTF-8")) {
    got <- suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", cand), error = function(e) ""))
    if (is.character(got) && nzchar(got)) return(invisible(TRUE))
  }
  invisible(FALSE)
}
dp_ensure_utf8_locale()

if (!exists("%||%")) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (is.character(a) && length(a) == 1 && !is.na(a) && a == "") return(b)
    a
  }
}

# A value coming back from JSON may be a length-1 list; unwrap it.
scalar <- function(x, default = NULL) {
  if (is.null(x)) return(default)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(default)
  x[[1]]
}

as_chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  as.character(unlist(x, use.names = FALSE))
}

as_bool <- function(x, default = FALSE) {
  x <- scalar(x, default)
  if (is.null(x) || is.na(x)) return(default)
  if (is.character(x)) return(tolower(x) %in% c("true", "t", "yes", "y", "1"))
  isTRUE(as.logical(x))
}

as_num <- function(x, default = NA_real_) {
  x <- scalar(x, default)
  if (is.null(x)) return(default)
  suppressWarnings(v <- as.numeric(x))
  if (is.na(v)) default else v
}

as_int <- function(x, default = NA_integer_) {
  v <- as_num(x, default)
  if (is.na(v)) return(as.integer(default))
  as.integer(round(v))
}

# Stop with a message that the UI can show verbatim.
dp_stop <- function(...) stop(paste0(...), call. = FALSE)

dp_warn_collector <- function() {
  msgs <- character(0)
  list(
    add = function(...) msgs <<- c(msgs, paste0(...)),
    get = function() msgs
  )
}

# Make a vector of names unique and syntactically usable, without mangling
# names that are already fine (we keep spaces -- these are data columns, not
# R symbols).
make_unique_names <- function(nms) {
  nms <- as.character(nms)
  nms[is.na(nms) | nms == ""] <- "V"
  out <- character(length(nms))
  seen <- list()
  for (i in seq_along(nms)) {
    base <- nms[i]
    n <- seen[[base]] %||% 0
    if (n == 0) {
      out[i] <- base
    } else {
      cand <- paste0(base, "_", n)
      while (cand %in% out) {
        n <- n + 1
        cand <- paste0(base, "_", n)
      }
      out[i] <- cand
    }
    seen[[base]] <- n + 1
  }
  out
}

file_ext <- function(path) tolower(sub(".*\\.", "", basename(path)))

# Resolve a possibly-relative path against the pipeline project root. This is
# what lets a saved pipeline be re-run "out of these same folders" on another
# machine.
resolve_path <- function(path, root = NULL) {
  path <- scalar(path, "")
  if (is.null(path) || is.na(path) || path == "") return(NA_character_)
  if (is_absolute_path(path) || is.null(root) || root == "") return(path)
  file.path(root, path)
}

is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)
}

# Store paths relative to root when they live underneath it, so pipelines stay
# portable; fall back to the absolute path when they do not.
relativise_path <- function(path, root = NULL) {
  if (is.null(path) || is.na(path) || path == "") return(path)
  if (is.null(root) || root == "") return(path)
  ap <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  ar <- tryCatch(normalizePath(root, winslash = "/", mustWork = FALSE), error = function(e) root)
  ar_slash <- if (grepl("/$", ar)) ar else paste0(ar, "/")
  if (startsWith(ap, ar_slash)) substring(ap, nchar(ar_slash) + 1) else path
}

timestamp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

new_id <- function(prefix = "id") {
  paste0(prefix, "_", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15),
         "_", sample.int(9999, 1))
}

# Truthy check for a data frame having rows/cols.
has_data <- function(df) !is.null(df) && is.data.frame(df) && ncol(df) > 0
