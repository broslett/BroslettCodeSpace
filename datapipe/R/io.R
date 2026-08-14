# ---------------------------------------------------------------------------
# io.R -- reading source files and writing the finished table.
#
# Everything is read as character on purpose. Type coercion is something the
# user asks for explicitly in the final step; doing it automatically at read
# time is how join keys quietly lose their leading zeros.
# ---------------------------------------------------------------------------

DP_FORMATS <- c(csv = "csv", tsv = "tsv", delimited = "delimited",
                excel = "excel", fixed = "fixed")

dp_detect_format <- function(path) {
  switch(file_ext(path),
         xlsx = "excel", xlsm = "excel", xls = "excel",
         tsv = "tsv", tab = "tsv",
         txt = "delimited", psv = "delimited",
         "csv")
}

dp_excel_sheets <- function(path) {
  if (!file.exists(path)) return(character(0))
  tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
}

# --- resolving a source to actual file paths --------------------------------
# A source is either a single file, or a folder plus a wildcard pattern. The
# pattern form is what makes a saved pipeline re-runnable next month against
# the same folder holding a newer file.
dp_resolve_source_files <- function(src, root = NULL) {
  mode <- scalar(src$path_mode, "file")
  if (identical(mode, "pattern")) {
    dir <- resolve_path(scalar(src$dir, ""), root)
    if (is.na(dir) || !dir.exists(dir)) {
      dp_stop("Folder not found for source '", scalar(src$label, src$id), "': ", dir)
    }
    pat <- scalar(src$pattern, "*")
    files <- list.files(dir, pattern = utils::glob2rx(pat), full.names = TRUE)
    files <- files[!dir.exists(files)]
    if (!length(files)) {
      dp_stop("No file in '", dir, "' matches '", pat, "' for source '",
              scalar(src$label, src$id), "'.")
    }
    pick <- scalar(src$pick, "latest")
    if (pick == "latest") {
      files <- files[order(file.mtime(files), decreasing = TRUE)][1]
    } else if (pick == "first") {
      files <- sort(files)[1]
    } # "all" keeps every match and they get stacked
    return(files)
  }
  p <- resolve_path(scalar(src$path, ""), root)
  if (is.na(p) || !file.exists(p)) {
    dp_stop("File not found for source '", scalar(src$label, src$id), "': ", p %||% "(none)")
  }
  p
}

# --- readers ----------------------------------------------------------------

dp_read_file <- function(path, format = NULL, options = list()) {
  format <- format %||% dp_detect_format(path)
  o <- options %||% list()
  df <- switch(
    format,
    excel = dp_read_excel(path, o),
    csv = dp_read_delim(path, o, default_sep = ","),
    tsv = dp_read_delim(path, o, default_sep = "\t"),
    delimited = dp_read_delim(path, o, default_sep = scalar(o$delimiter, ",")),
    fixed = dp_read_fixed(path, o),
    dp_stop("Unsupported format '", format, "'.")
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # Force character across the board and normalise the many faces of "missing".
  na_strings <- as_chr_vec(o$na_strings %||% c("NA", "N/A", "NULL", "-", ""))
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (inherits(v, "Date")) {
      v <- format(v, "%Y-%m-%d")
    } else if (inherits(v, "POSIXct")) {
      v <- format(v, "%Y-%m-%d %H:%M:%S")
    } else if (is.numeric(v)) {
      v <- ifelse(is.na(v), NA_character_, format(v, trim = TRUE, scientific = FALSE, digits = 15))
    } else {
      v <- as.character(v)
    }
    if (as_bool(o$trim_ws, TRUE)) v <- ifelse(is.na(v), NA_character_, trimws(v))
    v[!is.na(v) & v %in% na_strings] <- NA_character_
    # Tag valid UTF-8 explicitly. Without the mark, R treats the bytes as
    # "unknown" and escapes accented characters when formatting them.
    ok <- !is.na(v) & validUTF8(v)
    if (any(ok)) Encoding(v[ok]) <- "UTF-8"
    df[[j]] <- v
  }
  names(df) <- make_unique_names(names(df))
  df
}

dp_read_excel <- function(path, o) {
  sheet <- scalar(o$sheet, 1)
  if (is.character(sheet) && !is.na(suppressWarnings(as.numeric(sheet))) &&
      !(sheet %in% dp_excel_sheets(path))) {
    sheet <- as.numeric(sheet)
  }
  skip <- as_int(o$skip, 0)
  header <- as_bool(o$header, TRUE)
  df <- readxl::read_excel(path, sheet = sheet, skip = skip,
                           col_names = header, col_types = "text",
                           .name_repair = "minimal")
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!header) names(df) <- paste0("Column", seq_len(ncol(df)))
  df
}

dp_read_delim <- function(path, o, default_sep = ",") {
  sep <- scalar(o$delimiter, default_sep)
  if (identical(sep, "\\t")) sep <- "\t"
  header <- as_bool(o$header, TRUE)
  quote <- scalar(o$quote, "\"")

  df <- data.table::fread(
    file = path,
    sep = if (is.null(sep) || sep == "") "auto" else sep,
    header = header,
    skip = as_int(o$skip, 0),
    quote = quote,
    colClasses = "character",
    na.strings = character(0),   # we normalise NA ourselves, uniformly
    encoding = scalar(o$encoding, "UTF-8"),
    strip.white = FALSE,
    fill = TRUE,
    showProgress = FALSE,
    data.table = FALSE,
    check.names = FALSE
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  # fread is fast but (as of data.table 1.14) leaves doubled quotes escaped
  # inside quoted fields -- 'say ""hi""' instead of 'say "hi"'. Base R's parser
  # gets this right, so when the marker shows up we re-read with it. The check
  # costs nothing on the overwhelming majority of files, which contain no
  # embedded quotes at all, and deferring to the stricter parser is safe even
  # when the doubled quotes turned out to be genuine.
  if (nzchar(quote) && any(vapply(df, function(v)
        any(grepl(paste0(quote, quote), v, fixed = TRUE), na.rm = TRUE), logical(1)))) {
    df2 <- tryCatch(
      utils::read.table(path, sep = if (is.null(sep) || sep == "") "," else sep,
                        header = header, quote = quote, colClasses = "character",
                        skip = as_int(o$skip, 0), fill = TRUE, comment.char = "",
                        stringsAsFactors = FALSE, check.names = FALSE,
                        encoding = scalar(o$encoding, "UTF-8"), na.strings = character(0)),
      error = function(e) NULL)
    if (!is.null(df2) && ncol(df2) == ncol(df)) {
      names(df2) <- names(df)
      df <- df2
    }
  }

  if (!header) names(df) <- paste0("Column", seq_len(ncol(df)))
  df
}

dp_read_fixed <- function(path, o) {
  widths <- as.integer(as_chr_vec(o$widths))
  if (!length(widths) || any(is.na(widths))) dp_stop("Fixed-width source needs a list of column widths.")
  nms <- as_chr_vec(o$col_names)
  df <- utils::read.fwf(path, widths = widths, header = FALSE,
                        colClasses = "character", skip = as_int(o$skip, 0),
                        stringsAsFactors = FALSE)
  names(df) <- if (length(nms) == ncol(df)) nms else paste0("Column", seq_len(ncol(df)))
  df
}

# Read a source (one or many files) into a single frame.
dp_read_source <- function(src, root = NULL, warn = NULL) {
  files <- dp_resolve_source_files(src, root)
  fmt <- scalar(src$format, NULL) %||% dp_detect_format(files[1])
  opts <- src$options %||% list()
  frames <- lapply(files, function(f) dp_read_file(f, fmt, opts))
  if (length(frames) == 1) {
    df <- frames[[1]]
  } else {
    all_cols <- unique(unlist(lapply(frames, names)))
    frames <- lapply(frames, function(d) {
      for (cc in setdiff(all_cols, names(d))) d[[cc]] <- NA_character_
      d[, all_cols, drop = FALSE]
    })
    df <- do.call(rbind, frames)
    if (!is.null(warn)) warn$add("Source '", scalar(src$label, src$id), "': stacked ",
                                 length(files), " files into ", nrow(df), " rows.")
  }
  attr(df, "dp_files") <- files
  df
}

# --- writers ----------------------------------------------------------------

dp_write_file <- function(df, path, format = NULL, options = list()) {
  format <- format %||% dp_detect_format(path)
  o <- options %||% list()
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  out <- df
  na_out <- scalar(o$na_string, "")
  if (format == "excel") {
    if (as_bool(o$na_as_blank, TRUE)) {
      for (j in seq_along(out)) if (is.character(out[[j]])) out[[j]][is.na(out[[j]])] <- na_out
    }
    writexl::write_xlsx(out, path = path, col_names = as_bool(o$header, TRUE))
  } else {
    sep <- switch(format,
                  tsv = "\t",
                  delimited = { s <- scalar(o$delimiter, ","); if (identical(s, "\\t")) "\t" else s },
                  ",")
    data.table::fwrite(
      out, file = path, sep = sep,
      col.names = as_bool(o$header, TRUE),
      quote = if (identical(scalar(o$quote_mode, "auto"), "always")) TRUE else "auto",
      na = na_out,
      bom = as_bool(o$bom, FALSE),
      eol = if (identical(scalar(o$eol, "lf"), "crlf")) "\r\n" else "\n"
    )
  }
  invisible(path)
}
