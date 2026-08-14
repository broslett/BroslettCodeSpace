# ---------------------------------------------------------------------------
# sql.R -- translates the transformation library in ops.R into DuckDB SQL.
#
# Each entry here must produce *exactly* what its counterpart in ops.R
# produces. tests/test_duckdb.R enforces that by running both over the same
# inputs and comparing, so the R implementation acts as the specification and
# this file as an optimisation of it.
#
# An operation that cannot be reproduced faithfully returns NULL rather than
# an approximation. The engine then runs the whole pipeline in R instead, and
# says so in the log. Silently returning something close would be worse than
# being slow.
# ---------------------------------------------------------------------------

# --- literal and identifier quoting -----------------------------------------

sq <- function(x) paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
qi <- function(x) paste0('"', gsub('"', '""', as.character(x), fixed = TRUE), '"')

# Escape a literal string for use inside an RE2 pattern. Done character by
# character: expressing "the set of regex metacharacters" as a bracket
# expression is itself ambiguous, and R's own regex engine rejects it.
RE_META <- c("\\", ".", "^", "$", "|", "(", ")", "[", "]", "{", "}",
             "*", "+", "?", "/", "-")
re_esc <- function(x) {
  if (!nzchar(x)) return(x)
  ch <- strsplit(x, "")[[1]]
  paste(ifelse(ch %in% RE_META, paste0("\\", ch), ch), collapse = "")
}

# Escape a literal string for use inside an RE2 character class, where a bare
# ] would close the class and a bare - would open a range.
RE_CLASS_META <- c("\\", "]", "^", "-", "[")
re_class_esc <- function(x) {
  if (!nzchar(x)) return(x)
  ch <- strsplit(x, "")[[1]]
  paste(ifelse(ch %in% RE_CLASS_META, paste0("\\", ch), ch), collapse = "")
}

# R's trimws() strips [ \t\r\n] -- not the full POSIX space class. Match it.
TRIMWS_CLASS <- "[ \\t\\r\\n]"
sql_trimws <- function(x, side = "both") {
  out <- x
  if (side %in% c("both", "left")) {
    out <- paste0("regexp_replace(", out, ", '^", TRIMWS_CLASS, "+', '')")
  }
  if (side %in% c("both", "right")) {
    out <- paste0("regexp_replace(", out, ", '", TRIMWS_CLASS, "+$', '')")
  }
  out
}

# Operations in ops.R leave NA untouched, and most SQL functions propagate
# NULL the same way. A CASE or a coalesce does not, so those translations wrap
# themselves in this guard rather than turning a missing value into a blank.
null_guard <- function(x, expr) {
  paste0("CASE WHEN ", x, " IS NULL THEN NULL ELSE ", expr, " END")
}

# --- the operation translations ---------------------------------------------
# Each function takes the resolved parameter list and the SQL expression for
# the incoming value, and returns a new SQL expression (or NULL).

SQL_OPS <- list(

  trim = function(p, x) sql_trimws(x, scalar(p$side, "both")),

  squish = function(p, x)
    sql_trimws(paste0("regexp_replace(", x, ", '[[:space:]]+', ' ', 'g')")),

  case = function(p, x) {
    switch(scalar(p$to, "upper"),
      upper = paste0("upper(", x, ")"),
      lower = paste0("lower(", x, ")"),
      # R uppercases the first character only when it is a letter.
      sentence = paste0(
        "CASE WHEN regexp_matches(lower(", x, "), '^[[:alpha:]]') ",
        "THEN upper(substr(lower(", x, "), 1, 1)) || substr(lower(", x, "), 2) ",
        "ELSE lower(", x, ") END"),
      # Title case follows a rule about apostrophes that RE2 cannot express
      # (no case-conversion in replacements). Left to the R engine.
      title = NULL,
      NULL)
  },

  # DuckDB's strip_accents() only removes combining diacritics. R's
  # iconv(..., "ASCII//TRANSLIT") transliterates: it turns ss out of a sharp s
  # and question marks out of anything with no Latin equivalent. Those are
  # different operations, not two spellings of one, so this stays in R.
  strip_accents = function(p, x) NULL,

  digits_only = function(p, x) {
    pat <- if (as_bool(p$keep_decimal)) "[^0-9.]" else "[^0-9]"
    core <- paste0("regexp_replace(", x, ", '", pat, "', '', 'g')")
    if (!as_bool(p$keep_sign)) return(core)
    paste0("CASE WHEN regexp_matches(", x, ", '^[[:space:]]*-') AND ", core, " <> '' ",
           "THEN '-' || ", core, " ELSE ", core, " END")
  },

  # RE2 reads [[:alpha:]] as ASCII only, while R's follows the locale and
  # accepts accented letters. \\p{L} and \\p{N} are the Unicode equivalents.
  letters_only = function(p, x) {
    pat <- if (as_bool(p$keep_spaces)) "[^\\p{L} ]" else "[^\\p{L}]"
    paste0("regexp_replace(", x, ", '", pat, "', '', 'g')")
  },

  alnum_only = function(p, x) {
    pat <- if (as_bool(p$keep_spaces)) "[^\\p{L}\\p{N} ]" else "[^\\p{L}\\p{N}]"
    paste0("regexp_replace(", x, ", '", pat, "', '', 'g')")
  },

  remove_chars = function(p, x) {
    chars <- scalar(p$chars, "")
    if (identical(chars, "")) return(x)
    out <- x
    for (ch in strsplit(chars, "")[[1]]) {
      out <- paste0("replace(", out, ", ", sq(ch), ", '')")
    }
    out
  },

  keep_chars = function(p, x) {
    chars <- scalar(p$chars, "")
    if (identical(chars, "")) return(x)
    cls <- paste(vapply(strsplit(chars, "")[[1]], re_class_esc, character(1)), collapse = "")
    paste0("regexp_replace(", x, ", '[^", cls, "]', '', 'g')")
  },

  prefix = function(p, x) {
    pre <- scalar(p$text, "")
    if (as_bool(p$skip_blank, TRUE)) {
      paste0("CASE WHEN ", x, " = '' THEN ", x, " ELSE ", sq(pre), " || ", x, " END")
    } else paste0(sq(pre), " || ", x)
  },

  suffix = function(p, x) {
    suf <- scalar(p$text, "")
    if (as_bool(p$skip_blank, TRUE)) {
      paste0("CASE WHEN ", x, " = '' THEN ", x, " ELSE ", x, " || ", sq(suf), " END")
    } else paste0(x, " || ", sq(suf))
  },

  remove_prefix = function(p, x) {
    pre <- scalar(p$text, "")
    if (identical(pre, "")) return(x)
    lhs <- if (as_bool(p$ignore_case, TRUE)) paste0("lower(", x, ")") else x
    rhs <- if (as_bool(p$ignore_case, TRUE)) sq(tolower(pre)) else sq(pre)
    paste0("CASE WHEN starts_with(", lhs, ", ", rhs, ") ",
           "THEN substr(", x, ", ", nchar(pre) + 1, ") ELSE ", x, " END")
  },

  remove_suffix = function(p, x) {
    suf <- scalar(p$text, "")
    if (identical(suf, "")) return(x)
    lhs <- if (as_bool(p$ignore_case, TRUE)) paste0("lower(", x, ")") else x
    rhs <- if (as_bool(p$ignore_case, TRUE)) sq(tolower(suf)) else sq(suf)
    paste0("CASE WHEN ends_with(", lhs, ", ", rhs, ") ",
           "THEN substr(", x, ", 1, length(", x, ") - ", nchar(suf), ") ELSE ", x, " END")
  },

  replace = function(p, x) {
    find <- scalar(p$find, "")
    if (identical(find, "")) return(x)
    rep <- scalar(p$replace, "")
    use_regex <- as_bool(p$regex)
    icase <- as_bool(p$ignore_case)
    first <- as_bool(p$first_only)
    # Plain literal replacement of every match is a direct call.
    if (!use_regex && !icase && !first) {
      return(paste0("replace(", x, ", ", sq(find), ", ", sq(rep), ")"))
    }
    pat <- if (use_regex) find else re_esc(find)
    opts <- paste0(if (!first) "g" else "", if (icase) "i" else "")
    # A literal replacement string must not be re-read for backreferences.
    repl <- if (use_regex) rep else gsub("\\", "\\\\", rep, fixed = TRUE)
    paste0("regexp_replace(", x, ", ", sq(pat), ", ", sq(repl), ", ", sq(opts), ")")
  },

  # lpad/rpad truncate when the value is already longer than the target width;
  # ops.R leaves such values alone, so guard the call.
  left_pad = function(p, x) {
    w <- as_int(p$width, 0); ch <- substr(paste0(scalar(p$char, "0"), " "), 1, 1)
    paste0("CASE WHEN length(", x, ") >= ", w, " THEN ", x,
           " ELSE lpad(", x, ", ", w, ", ", sq(ch), ") END")
  },

  right_pad = function(p, x) {
    w <- as_int(p$width, 0); ch <- substr(paste0(scalar(p$char, " "), " "), 1, 1)
    paste0("CASE WHEN length(", x, ") >= ", w, " THEN ", x,
           " ELSE rpad(", x, ", ", w, ", ", sq(ch), ") END")
  },

  strip_leading_zeros = function(p, x) {
    stripped <- paste0("regexp_replace(", x, ", '^0+', '')")
    paste0("CASE WHEN ", stripped, " = '' AND ", x, " <> '' THEN '0' ELSE ", stripped, " END")
  },

  left = function(p, x) paste0("substr(", x, ", 1, ", as_int(p$n, 0), ")"),

  right = function(p, x) {
    n <- as_int(p$n, 0)
    paste0("CASE WHEN length(", x, ") <= ", n, " THEN ", x,
           " ELSE substr(", x, ", length(", x, ") - ", n, " + 1) END")
  },

  substring = function(p, x) {
    st <- max(1, as_int(p$start, 1)); ln <- as_int(p$len, 0)
    if (ln <= 0) paste0("substr(", x, ", ", st, ")")
    else paste0("substr(", x, ", ", st, ", ", ln, ")")
  },

  truncate = function(p, x) paste0("substr(", x, ", 1, ", as_int(p$width, 50), ")"),

  regex_extract = function(p, x) {
    pat <- scalar(p$pattern, "")
    if (identical(pat, "")) return(x)
    grp <- as_int(p$group, 1)
    miss <- scalar(p$on_no_match, "blank")
    fallback <- switch(miss, "keep original" = x, "empty (NA)" = "NULL", "''")
    null_guard(x, paste0("CASE WHEN regexp_matches(", x, ", ", sq(pat), ") ",
                         "THEN regexp_extract(", x, ", ", sq(pat), ", ", grp, ") ",
                         "ELSE ", fallback, " END"))
  },

  split_take = function(p, x) {
    sep <- scalar(p$sep, "-")
    if (identical(sep, "")) return(x)
    i <- as_int(p$index, 1)
    null_guard(x, paste0("coalesce(list_extract(str_split(", x, ", ", sq(sep),
                         "), ", i, "), '')"))
  },

  format_number = function(p, x) {
    dec <- as_int(p$decimals, 2)
    big <- scalar(p$big_mark, "")
    decm <- scalar(p$dec_mark, ".")
    miss <- scalar(p$on_fail, "blank")
    num <- paste0("try_cast(regexp_replace(", x, ", '[[:space:],]', '', 'g') AS DOUBLE)")
    body <- if (identical(big, "")) {
      paste0("printf('%.", dec, "f', ", num, ")")
    } else {
      # DuckDB groups with ',' and separates decimals with '.'; swap both into
      # place using a placeholder so the two substitutions cannot collide.
      grouped <- paste0("format('{:,.", dec, "f}', ", num, ")")
      s1 <- paste0("replace(", grouped, ", ',', chr(1))")
      s2 <- paste0("replace(", s1, ", '.', chr(2))")
      s3 <- paste0("replace(", s2, ", chr(1), ", sq(big), ")")
      paste0("replace(", s3, ", chr(2), ", sq(decm), ")")
    }
    if (!identical(big, "") || identical(decm, ".")) {
      out <- body
    } else {
      out <- paste0("replace(", body, ", '.', ", sq(decm), ")")
    }
    fallback <- switch(miss, "keep original" = x, "empty (NA)" = "NULL", "''")
    null_guard(x, paste0("CASE WHEN ", num, " IS NULL THEN ", fallback,
                         " ELSE ", out, " END"))
  },

  # R rounds half-to-even and then formats with format(), which drops trailing
  # zeros and switches to 15 significant digits. Not worth imitating in SQL.
  round_number = function(p, x) NULL,

  format_date = function(p, x) {
    infmt <- scalar(p$input_format, "")
    outfmt <- scalar(p$output_format, "%Y-%m-%d")
    miss <- scalar(p$on_fail, "blank")
    fmts <- if (identical(infmt, "") || identical(infmt, "auto")) DATE_FORMATS else infmt
    fmt_list <- paste0("[", paste(vapply(fmts, sq, character(1)), collapse = ", "), "]")
    parsed <- paste0("try_strptime(", x, ", ", fmt_list, ")")
    # ops.R also accepts bare five-digit Excel serial numbers.
    serial <- paste0("CASE WHEN regexp_matches(", sql_trimws(x), ", '^[0-9]{5}$') ",
                     "THEN (DATE '1899-12-30' + to_days(try_cast(", sql_trimws(x),
                     " AS INTEGER)))::TIMESTAMP ELSE NULL END")
    d <- paste0("coalesce(", parsed, ", ", serial, ")")
    fallback <- switch(miss, "keep original" = x, "empty (NA)" = "NULL", "''")
    null_guard(x, paste0("CASE WHEN ", d, " IS NULL THEN ", fallback,
                         " ELSE strftime(", d, ", ", sq(outfmt), ") END"))
  },

  blank_to_na = function(p, x)
    paste0("CASE WHEN ", x, " IS NOT NULL AND ", sql_trimws(x), " = '' THEN NULL ELSE ", x, " END"),

  fill_na = function(p, x)
    paste0("CASE WHEN ", x, " IS NULL OR ", sql_trimws(x), " = '' THEN ",
           sq(scalar(p$value, "")), " ELSE ", x, " END"),

  set_constant = function(p, x) sq(scalar(p$value, ""))
)

# --- step chains ------------------------------------------------------------

# Build the SQL expression for a chain of steps, or NULL if any step in the
# chain has no faithful translation.
sql_steps_expr <- function(base_expr, steps) {
  if (is.null(steps) || !length(steps)) return(base_expr)
  out <- base_expr
  for (st in steps) {
    op <- scalar(st$op, "")
    fn <- SQL_OPS[[op]]
    if (is.null(fn)) return(NULL)
    spec <- TF_OPS[[op]]
    if (is.null(spec)) return(NULL)
    params <- st$params %||% list()
    for (nm in names(spec$params)) {
      if (is.null(params[[nm]])) params[[nm]] <- spec$params[[nm]]$default
    }
    # Most operations leave NA as NA; SQL NULL already propagates through the
    # functions used above, except where a CASE would swallow it.
    out <- tryCatch(fn(params, out), error = function(e) NULL)
    if (is.null(out)) return(NULL)
    out <- paste0("(", out, ")")
  }
  out
}

# Which operations in a pipeline have no SQL translation. Used to explain the
# fallback in the run log.
sql_untranslatable_ops <- function(spec) {
  found <- character(0)
  check <- function(steps) {
    for (st in steps %||% list()) {
      op <- scalar(st$op, "")
      if (is.null(SQL_OPS[[op]]) ||
          is.null(tryCatch(sql_steps_expr("x", list(st)), error = function(e) NULL))) {
        found <<- c(found, TF_OPS[[op]]$label %||% op)
      }
    }
  }
  for (t in spec$transforms %||% list()) check(t$steps)
  for (f in spec$final$fields %||% list()) check(f$steps)
  unique(found)
}
