# ---------------------------------------------------------------------------
# ops.R -- the field transformation library.
#
# Every operation is registered here with a label, a parameter spec and an
# implementation. The Shiny UI builds its controls straight from the parameter
# specs, so adding an operation here makes it appear in the app automatically.
#
# All operations take and return a *character* vector. Keeping keys as text
# end-to-end is deliberate: it is the only way to preserve leading zeros and
# avoid 1.2e+10 style damage on long numeric identifiers.
# ---------------------------------------------------------------------------

# --- parameter spec helpers -------------------------------------------------
p_text   <- function(label, default = "", placeholder = "") list(type = "text", label = label, default = default, placeholder = placeholder)
p_num    <- function(label, default = 0, min = NA, max = NA) list(type = "number", label = label, default = default, min = min, max = max)
p_choice <- function(label, choices, default = choices[1]) list(type = "choice", label = label, choices = choices, default = default)
p_bool   <- function(label, default = FALSE) list(type = "logical", label = label, default = default)

# NA-safe wrapper: operations never need to think about NA, and NA never
# silently becomes the string "NA".
keep_na <- function(x, f) {
  x <- as.character(x)
  idx <- !is.na(x)
  if (!any(idx)) return(x)
  x[idx] <- f(x[idx])
  x
}

.pad <- function(x, width, char, side) {
  char <- substr(paste0(char, " "), 1, 1)
  n <- pmax(0, width - nchar(x))
  fill <- vapply(n, function(k) paste(rep(char, k), collapse = ""), character(1))
  if (side == "left") paste0(fill, x) else paste0(x, fill)
}

.title_case <- function(x) {
  gsub("(^|[^[:alnum:]'])([[:alpha:]])", "\\1\\U\\2", tolower(x), perl = TRUE)
}

DATE_FORMATS <- c(
  "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y", "%Y/%m/%d",
  "%d.%m.%Y", "%Y%m%d", "%d %b %Y", "%d %B %Y", "%b %d, %Y", "%B %d, %Y",
  "%d/%m/%y", "%m/%d/%y", "%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M"
)

.parse_dates <- function(x, fmt_in) {
  fmts <- if (is.null(fmt_in) || fmt_in == "" || fmt_in == "auto") DATE_FORMATS else fmt_in
  out <- as.Date(rep(NA, length(x)), origin = "1970-01-01")
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x) & trimws(x) != ""
    if (!any(todo)) break
    parsed <- suppressWarnings(as.Date(x[todo], format = f))
    out[todo] <- parsed
  }
  # Excel serial numbers (a very common source of date pain).
  todo <- is.na(out) & !is.na(x) & grepl("^[0-9]{5}$", trimws(x))
  if (any(todo)) {
    out[todo] <- as.Date(as.numeric(trimws(x[todo])), origin = "1899-12-30")
  }
  out
}

# --- the registry -----------------------------------------------------------
# group is used only to organise the operation picker in the UI.
TF_OPS <- list(

  # -- whitespace & case ----------------------------------------------------
  trim = list(
    group = "Clean", label = "Trim whitespace",
    desc = "Remove whitespace from the start and/or end.",
    params = list(side = p_choice("Side", c("both", "left", "right"))),
    fn = function(x, p) keep_na(x, function(v) {
      switch(scalar(p$side, "both"),
             left  = sub("^[[:space:]]+", "", v),
             right = sub("[[:space:]]+$", "", v),
             trimws(v))
    })
  ),

  squish = list(
    group = "Clean", label = "Collapse inner spaces",
    desc = "Turn runs of whitespace into a single space and trim the ends.",
    params = list(),
    fn = function(x, p) keep_na(x, function(v) trimws(gsub("[[:space:]]+", " ", v)))
  ),

  case = list(
    group = "Clean", label = "Change case",
    desc = "Upper, lower, title or sentence case.",
    params = list(to = p_choice("Convert to", c("upper", "lower", "title", "sentence"))),
    fn = function(x, p) keep_na(x, function(v) {
      switch(scalar(p$to, "upper"),
             upper = toupper(v),
             lower = tolower(v),
             title = .title_case(v),
             sentence = sub("^([[:alpha:]])", "\\U\\1", tolower(v), perl = TRUE),
             v)
    })
  ),

  strip_accents = list(
    group = "Clean", label = "Remove accents",
    desc = "Fold accented characters to plain ASCII (Ä -> A).",
    params = list(),
    fn = function(x, p) keep_na(x, function(v) {
      out <- iconv(v, from = "UTF-8", to = "ASCII//TRANSLIT")
      ifelse(is.na(out), v, out)
    })
  ),

  # -- character filtering --------------------------------------------------
  digits_only = list(
    group = "Filter", label = "Numbers only",
    desc = "Keep digits, discard everything else. 'AB-123 x' becomes '123'.",
    params = list(keep_decimal = p_bool("Keep decimal point", FALSE),
                  keep_sign = p_bool("Keep leading minus sign", FALSE)),
    fn = function(x, p) keep_na(x, function(v) {
      neg <- if (as_bool(p$keep_sign)) grepl("^[[:space:]]*-", v) else rep(FALSE, length(v))
      pat <- if (as_bool(p$keep_decimal)) "[^0-9.]" else "[^0-9]"
      out <- gsub(pat, "", v)
      ifelse(neg & out != "", paste0("-", out), out)
    })
  ),

  letters_only = list(
    group = "Filter", label = "Letters only",
    desc = "Keep alphabetic characters, discard everything else.",
    params = list(keep_spaces = p_bool("Keep spaces", FALSE)),
    fn = function(x, p) keep_na(x, function(v)
      gsub(if (as_bool(p$keep_spaces)) "[^[:alpha:] ]" else "[^[:alpha:]]", "", v))
  ),

  alnum_only = list(
    group = "Filter", label = "Letters and numbers only",
    desc = "Strip punctuation and symbols. Ideal for making join keys comparable.",
    params = list(keep_spaces = p_bool("Keep spaces", FALSE)),
    fn = function(x, p) keep_na(x, function(v)
      gsub(if (as_bool(p$keep_spaces)) "[^[:alnum:] ]" else "[^[:alnum:]]", "", v))
  ),

  remove_chars = list(
    group = "Filter", label = "Remove specific characters",
    desc = "Delete every character listed. Example: '- /.' strips dashes, slashes and dots.",
    params = list(chars = p_text("Characters to remove", "", "e.g. -/. ")),
    fn = function(x, p) {
      chars <- scalar(p$chars, "")
      if (chars == "") return(as.character(x))
      keep_na(x, function(v) {
        for (ch in strsplit(chars, "")[[1]]) v <- gsub(ch, "", v, fixed = TRUE)
        v
      })
    }
  ),

  keep_chars = list(
    group = "Filter", label = "Keep only these characters",
    desc = "Whitelist: anything not listed is removed.",
    params = list(chars = p_text("Characters to keep", "", "e.g. 0123456789X")),
    fn = function(x, p) {
      chars <- scalar(p$chars, "")
      if (chars == "") return(as.character(x))
      keep_na(x, function(v) {
        allowed <- strsplit(chars, "")[[1]]
        vapply(v, function(s) {
          cs <- strsplit(s, "")[[1]]
          paste(cs[cs %in% allowed], collapse = "")
        }, character(1), USE.NAMES = FALSE)
      })
    }
  ),

  # -- adding / removing text ----------------------------------------------
  prefix = list(
    group = "Compose", label = "Add prefix",
    desc = "Put text in front of the value.",
    params = list(text = p_text("Prefix", "", "e.g. CUST-"),
                  skip_blank = p_bool("Skip blank values", TRUE)),
    fn = function(x, p) {
      pre <- scalar(p$text, "")
      keep_na(x, function(v) ifelse(as_bool(p$skip_blank, TRUE) & v == "", v, paste0(pre, v)))
    }
  ),

  suffix = list(
    group = "Compose", label = "Add suffix",
    desc = "Put text after the value.",
    params = list(text = p_text("Suffix", "", "e.g. -UK"),
                  skip_blank = p_bool("Skip blank values", TRUE)),
    fn = function(x, p) {
      suf <- scalar(p$text, "")
      keep_na(x, function(v) ifelse(as_bool(p$skip_blank, TRUE) & v == "", v, paste0(v, suf)))
    }
  ),

  remove_prefix = list(
    group = "Compose", label = "Remove prefix",
    desc = "Strip the given text from the start, if present.",
    params = list(text = p_text("Prefix to remove", ""),
                  ignore_case = p_bool("Ignore case", TRUE)),
    fn = function(x, p) {
      pre <- scalar(p$text, "")
      if (pre == "") return(as.character(x))
      keep_na(x, function(v) {
        hit <- if (as_bool(p$ignore_case, TRUE)) startsWith(tolower(v), tolower(pre)) else startsWith(v, pre)
        ifelse(hit, substring(v, nchar(pre) + 1), v)
      })
    }
  ),

  remove_suffix = list(
    group = "Compose", label = "Remove suffix",
    desc = "Strip the given text from the end, if present.",
    params = list(text = p_text("Suffix to remove", ""),
                  ignore_case = p_bool("Ignore case", TRUE)),
    fn = function(x, p) {
      suf <- scalar(p$text, "")
      if (suf == "") return(as.character(x))
      keep_na(x, function(v) {
        hit <- if (as_bool(p$ignore_case, TRUE)) endsWith(tolower(v), tolower(suf)) else endsWith(v, suf)
        ifelse(hit, substring(v, 1, nchar(v) - nchar(suf)), v)
      })
    }
  ),

  replace = list(
    group = "Compose", label = "Find and replace",
    desc = "Literal or regular-expression replacement.",
    params = list(find = p_text("Find", ""),
                  replace = p_text("Replace with", ""),
                  regex = p_bool("Treat 'Find' as a regular expression", FALSE),
                  ignore_case = p_bool("Ignore case", FALSE),
                  first_only = p_bool("Replace first match only", FALSE)),
    fn = function(x, p) {
      find <- scalar(p$find, "")
      if (find == "") return(as.character(x))
      rep <- scalar(p$replace, "")
      use_regex <- as_bool(p$regex)
      f <- if (as_bool(p$first_only)) sub else gsub
      keep_na(x, function(v) f(find, rep, v,
                               fixed = !use_regex && !as_bool(p$ignore_case),
                               ignore.case = if (use_regex || as_bool(p$ignore_case)) as_bool(p$ignore_case) else FALSE,
                               perl = FALSE))
    }
  ),

  # -- padding & slicing ----------------------------------------------------
  left_pad = list(
    group = "Length", label = "Left pad",
    desc = "Pad on the left up to a fixed width. The classic fix for keys that lost their leading zeros in Excel.",
    params = list(width = p_num("Width", 10, min = 0),
                  char = p_text("Pad character", "0")),
    fn = function(x, p) keep_na(x, function(v)
      .pad(v, as_int(p$width, 0), scalar(p$char, "0"), "left"))
  ),

  right_pad = list(
    group = "Length", label = "Right pad",
    desc = "Pad on the right up to a fixed width.",
    params = list(width = p_num("Width", 10, min = 0),
                  char = p_text("Pad character", " ")),
    fn = function(x, p) keep_na(x, function(v)
      .pad(v, as_int(p$width, 0), scalar(p$char, " "), "right"))
  ),

  strip_leading_zeros = list(
    group = "Length", label = "Remove leading zeros",
    desc = "'000123' becomes '123'. A value of all zeros becomes '0'.",
    params = list(),
    fn = function(x, p) keep_na(x, function(v) {
      out <- sub("^0+", "", v)
      ifelse(out == "" & v != "", "0", out)
    })
  ),

  left = list(
    group = "Length", label = "Left (first N characters)",
    desc = "Keep the first N characters.",
    params = list(n = p_num("Number of characters", 5, min = 0)),
    fn = function(x, p) keep_na(x, function(v) substr(v, 1, as_int(p$n, 0)))
  ),

  right = list(
    group = "Length", label = "Right (last N characters)",
    desc = "Keep the last N characters.",
    params = list(n = p_num("Number of characters", 5, min = 0)),
    fn = function(x, p) keep_na(x, function(v) {
      n <- as_int(p$n, 0)
      substr(v, pmax(1, nchar(v) - n + 1), nchar(v))
    })
  ),

  substring = list(
    group = "Length", label = "Substring",
    desc = "Take characters from a start position for a given length.",
    params = list(start = p_num("Start position (1 = first)", 1, min = 1),
                  len = p_num("Length (0 = to the end)", 0, min = 0)),
    fn = function(x, p) keep_na(x, function(v) {
      st <- max(1, as_int(p$start, 1)); ln <- as_int(p$len, 0)
      if (ln <= 0) substring(v, st) else substr(v, st, st + ln - 1)
    })
  ),

  truncate = list(
    group = "Length", label = "Truncate to width",
    desc = "Cut anything longer than the given width.",
    params = list(width = p_num("Maximum width", 50, min = 1)),
    fn = function(x, p) keep_na(x, function(v) substr(v, 1, as_int(p$width, 50)))
  ),

  # -- extraction -----------------------------------------------------------
  regex_extract = list(
    group = "Extract", label = "Extract by pattern",
    desc = "Pull out the first match of a regular expression. Use a capture group to keep just part of it.",
    params = list(pattern = p_text("Pattern", "", "e.g. ([0-9]{4})"),
                  group = p_num("Capture group (0 = whole match)", 1, min = 0),
                  on_no_match = p_choice("If nothing matches", c("blank", "keep original", "empty (NA)"))),
    fn = function(x, p) {
      pat <- scalar(p$pattern, "")
      if (pat == "") return(as.character(x))
      grp <- as_int(p$group, 1)
      miss <- scalar(p$on_no_match, "blank")
      keep_na(x, function(v) {
        m <- regexec(pat, v, perl = TRUE)
        parts <- regmatches(v, m)
        vapply(seq_along(parts), function(i) {
          pr <- parts[[i]]
          if (!length(pr)) {
            return(switch(miss, "keep original" = v[i], "empty (NA)" = NA_character_, ""))
          }
          idx <- min(grp + 1, length(pr))
          pr[idx]
        }, character(1))
      })
    }
  ),

  split_take = list(
    group = "Extract", label = "Split and take part",
    desc = "Split on a separator and keep one piece. Part -1 means the last piece.",
    params = list(sep = p_text("Separator", "-"),
                  index = p_num("Part number (1 = first, -1 = last)", 1)),
    fn = function(x, p) {
      sep <- scalar(p$sep, "-")
      if (sep == "") return(as.character(x))
      i <- as_int(p$index, 1)
      keep_na(x, function(v) vapply(strsplit(v, sep, fixed = TRUE), function(pr) {
        if (!length(pr)) return("")
        k <- if (i < 0) length(pr) + i + 1 else i
        if (k < 1 || k > length(pr)) "" else pr[k]
      }, character(1)))
    }
  ),

  # -- numbers & dates ------------------------------------------------------
  format_number = list(
    group = "Type", label = "Format as number",
    desc = "Parse the value as a number and re-render it consistently.",
    params = list(decimals = p_num("Decimal places", 2, min = 0),
                  big_mark = p_text("Thousands separator", ""),
                  dec_mark = p_text("Decimal separator", "."),
                  on_fail = p_choice("If not a number", c("blank", "keep original", "empty (NA)"))),
    fn = function(x, p) {
      dec <- as_int(p$decimals, 2)
      miss <- scalar(p$on_fail, "blank")
      keep_na(x, function(v) {
        cleaned <- gsub("[[:space:]]|,", "", v)
        num <- suppressWarnings(as.numeric(cleaned))
        out <- formatC(num, format = "f", digits = dec,
                       big.mark = scalar(p$big_mark, ""),
                       decimal.mark = scalar(p$dec_mark, "."))
        bad <- is.na(num)
        out[bad] <- switch(miss, "keep original" = v[bad], "empty (NA)" = NA_character_, "")
        out
      })
    }
  ),

  round_number = list(
    group = "Type", label = "Round number",
    desc = "Round to a number of decimal places, keeping the value numeric-looking.",
    params = list(decimals = p_num("Decimal places", 0, min = 0)),
    fn = function(x, p) keep_na(x, function(v) {
      num <- suppressWarnings(as.numeric(gsub("[[:space:]]|,", "", v)))
      out <- ifelse(is.na(num), v, format(round(num, as_int(p$decimals, 0)),
                                          trim = TRUE, scientific = FALSE))
      out
    })
  ),

  format_date = list(
    group = "Type", label = "Parse and format date",
    desc = "Read a date in almost any layout (including Excel serial numbers) and write it back in one consistent format.",
    params = list(input_format = p_text("Input format (blank = detect)", ""),
                  output_format = p_text("Output format", "%Y-%m-%d"),
                  on_fail = p_choice("If not a date", c("blank", "keep original", "empty (NA)"))),
    fn = function(x, p) {
      outfmt <- scalar(p$output_format, "%Y-%m-%d")
      miss <- scalar(p$on_fail, "blank")
      keep_na(x, function(v) {
        d <- .parse_dates(v, scalar(p$input_format, ""))
        out <- format(d, outfmt)
        bad <- is.na(d)
        out[bad] <- switch(miss, "keep original" = v[bad], "empty (NA)" = NA_character_, "")
        out
      })
    }
  ),

  # -- blanks ---------------------------------------------------------------
  blank_to_na = list(
    group = "Blanks", label = "Blank to empty",
    desc = "Treat empty or whitespace-only values as genuinely missing.",
    params = list(),
    fn = function(x, p) {
      x <- as.character(x)
      x[!is.na(x) & trimws(x) == ""] <- NA_character_
      x
    }
  ),

  fill_na = list(
    group = "Blanks", label = "Fill empty values",
    desc = "Replace missing or blank values with fixed text.",
    params = list(value = p_text("Fill with", "")),
    fn = function(x, p) {
      x <- as.character(x)
      val <- scalar(p$value, "")
      x[is.na(x) | trimws(x) == ""] <- val
      x
    }
  ),

  # -- literal --------------------------------------------------------------
  set_constant = list(
    group = "Compose", label = "Set to fixed value",
    desc = "Overwrite every row with the same text.",
    params = list(value = p_text("Value", "")),
    fn = function(x, p) rep(scalar(p$value, ""), length(x))
  )
)

# --- application ------------------------------------------------------------

tf_op_labels <- function() {
  groups <- vapply(TF_OPS, function(o) o$group, character(1))
  labels <- vapply(TF_OPS, function(o) o$label, character(1))
  keys <- names(TF_OPS)
  out <- list()
  for (g in unique(groups)) {
    idx <- which(groups == g)
    out[[g]] <- stats::setNames(keys[idx], labels[idx])
  }
  out
}

tf_op_defaults <- function(op) {
  spec <- TF_OPS[[op]]
  if (is.null(spec)) return(list())
  lapply(spec$params, function(p) p$default)
}

# Human-readable one-line summary of a step, for the pipeline overview.
tf_step_label <- function(step) {
  op <- scalar(step$op, "")
  spec <- TF_OPS[[op]]
  if (is.null(spec)) return(paste0("<unknown: ", op, ">"))
  ps <- step$params %||% list()
  bits <- character(0)
  for (nm in names(spec$params)) {
    v <- ps[[nm]]
    if (is.null(v)) next
    v <- scalar(v, "")
    if (identical(v, spec$params[[nm]]$default)) next
    if (is.logical(v)) v <- if (isTRUE(v)) "yes" else "no"
    if (identical(as.character(v), "")) next
    bits <- c(bits, paste0(nm, "=", v))
  }
  if (length(bits)) paste0(spec$label, " (", paste(bits, collapse = ", "), ")") else spec$label
}

# Apply one step. Unknown ops are a hard error -- silently ignoring them would
# produce a wrong result that looks right.
tf_apply_step <- function(x, step) {
  op <- scalar(step$op, "")
  spec <- TF_OPS[[op]]
  if (is.null(spec)) dp_stop("Unknown transformation '", op, "'.")
  params <- step$params %||% list()
  # Fill in defaults for anything the caller omitted.
  for (nm in names(spec$params)) {
    if (is.null(params[[nm]])) params[[nm]] <- spec$params[[nm]]$default
  }
  res <- spec$fn(as.character(x), params)
  as.character(res)
}

tf_apply_steps <- function(x, steps) {
  x <- as.character(x)
  if (is.null(steps) || !length(steps)) return(x)
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    x <- tryCatch(tf_apply_step(x, step),
                  error = function(e) dp_stop("Step ", i, " (", scalar(step$op, "?"), "): ", conditionMessage(e)))
  }
  x
}
