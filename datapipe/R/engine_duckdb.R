# ---------------------------------------------------------------------------
# engine_duckdb.R -- runs a pipeline inside DuckDB.
#
# The pipeline becomes a chain of temporary views: one per source, one per
# linking-field transformation, one per join, then the final shaping. DuckDB
# does the reading, matching, filtering and sorting; R only receives the
# finished table.
#
# This engine is an optimisation, never a second opinion. Anything it cannot
# reproduce exactly -- an operation with no faithful SQL translation, or a
# missing duckdb package -- hands the whole run back to dp_execute() in
# pipeline.R and says so in the log.
# ---------------------------------------------------------------------------

dp_duckdb_available <- function() {
  isTRUE(requireNamespace("duckdb", quietly = TRUE)) &&
    isTRUE(requireNamespace("DBI", quietly = TRUE))
}

dp_duckdb_version <- function() {
  if (!dp_duckdb_available()) return(NA_character_)
  as.character(utils::packageVersion("duckdb"))
}

# Sentinel used to glue multi-field keys together, matching .KEYSEP in
# pipeline.R so both engines build the same composite key.
SQL_KEYSEP <- "chr(31)"

# --- helpers ----------------------------------------------------------------

ddb_cols <- function(con, relation) {
  names(DBI::dbGetQuery(con, paste0("SELECT * FROM ", relation, " LIMIT 0")))
}

ddb_view <- function(con, name, sql) {
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TEMP VIEW ", qi(name), " AS ", sql))
  name
}

# Stage outputs are materialised rather than left as views. The run log reports
# row counts and match counts at every stage, and against a view each of those
# counts re-executes the entire chain behind it -- which made the whole run
# several times slower than doing the work in R. As a table, each stage is
# computed once and counting it is free.
ddb_table <- function(con, name, sql) {
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TEMP TABLE ", qi(name), " AS ", sql))
  name
}

ddb_count <- function(con, relation) {
  DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ", relation))$n[1]
}

# The ordering column carried through every stage so the result keeps the base
# table's row order, exactly as the R engine does.
RN <- "__dp_rn"

# A key expression that is NULL when every part is blank. SQL never joins NULL
# to NULL, which is precisely the "blank keys never match each other" rule.
ddb_key_expr <- function(cols, ignore_case = FALSE, match_blanks = FALSE, alias = NULL) {
  pref <- if (is.null(alias)) "" else paste0(qi(alias), ".")
  parts <- vapply(cols, function(c) {
    e <- paste0("coalesce(", pref, qi(c), ", '')")
    if (ignore_case) e <- paste0("lower(", e, ")")
    e
  }, character(1))
  joined <- if (length(parts) == 1) parts[1]
            else paste0("concat_ws(", SQL_KEYSEP, ", ", paste(parts, collapse = ", "), ")")
  if (match_blanks) return(joined)
  blank <- paste(vapply(parts, function(p) paste0(p, " = ''"), character(1)), collapse = " AND ")
  paste0("CASE WHEN ", blank, " THEN NULL ELSE ", joined, " END")
}

# --- sources ----------------------------------------------------------------

ddb_source_view <- function(con, src, root, idx, warn = NULL, preview_rows = NULL) {
  files <- dp_resolve_source_files(src, root)
  fmt <- scalar(src$format, NULL) %||% dp_detect_format(files[1])
  o <- src$options %||% list()
  name <- paste0("src_", idx)

  if (fmt %in% c("csv", "tsv", "delimited")) {
    sep <- scalar(o$delimiter, if (fmt == "tsv") "\t" else ",")
    if (identical(sep, "\\t")) sep <- "\t"
    header <- as_bool(o$header, TRUE)
    paths <- paste0("[", paste(vapply(files, sq, character(1)), collapse = ", "), "]")
    scan <- paste0(
      "read_csv(", paths,
      ", all_varchar = true",
      ", header = ", if (header) "true" else "false",
      ", delim = ", sq(sep),
      ", quote = ", sq(scalar(o$quote, "\"")),
      ", skip = ", as_int(o$skip, 0),
      ", sample_size = -1",
      ", union_by_name = true",
      ", null_padding = true)")
    raw <- ddb_view(con, paste0(name, "_raw"), paste0("SELECT * FROM ", scan))
    cols <- ddb_cols(con, qi(raw))
    out_names <- if (header) make_unique_names(cols) else paste0("Column", seq_along(cols))

    # Reproduce the reader in io.R: trim, then map the "missing" spellings to
    # NULL, in that order.
    na_strings <- as_chr_vec(o$na_strings %||% c("NA", "N/A", "NULL", "-", ""))
    exprs <- vapply(seq_along(cols), function(i) {
      e <- qi(cols[i])
      if (as_bool(o$trim_ws, TRUE)) e <- sql_trimws(e)
      if (length(na_strings)) {
        lst <- paste(vapply(na_strings, sq, character(1)), collapse = ", ")
        e <- paste0("CASE WHEN ", e, " IN (", lst, ") THEN NULL ELSE ", e, " END")
      }
      paste0(e, " AS ", qi(out_names[i]))
    }, character(1))
    sql <- paste0("SELECT ", paste(exprs, collapse = ", "),
                  ", row_number() OVER () AS ", qi(RN), " FROM ", qi(raw))
    if (!is.null(preview_rows) && identical(scalar(src$role, "data"), "data")) {
      sql <- paste0(sql, " LIMIT ", as_int(preview_rows, 500))
    }
    ddb_table(con, name, sql)
    return(list(view = name, cols = out_names, files = files))
  }

  # Excel and fixed-width have no DuckDB reader that works without downloading
  # an extension, so they are read by the tested R reader and handed over.
  df <- dp_read_source(src, root, warn)
  if (!is.null(preview_rows) && nrow(df) > preview_rows &&
      identical(scalar(src$role, "data"), "data")) {
    df <- df[seq_len(preview_rows), , drop = FALSE]
  }
  reg <- paste0(name, "_r")
  duckdb::duckdb_register(con, reg, df, overwrite = TRUE)
  cols <- names(df)
  sql <- paste0("SELECT ", paste(vapply(cols, function(c) paste0(qi(c), " AS ", qi(c)), character(1)),
                                 collapse = ", "),
                ", row_number() OVER () AS ", qi(RN), " FROM ", qi(reg))
  ddb_table(con, name, sql)
  list(view = name, cols = cols, files = attr(df, "dp_files") %||% files)
}

# --- the run ----------------------------------------------------------------

dp_execute_duckdb <- function(spec, root = NULL, stop_after = "export", progress = NULL,
                              preview_rows = NULL) {
  warn <- dp_warn_collector()
  log <- list()
  say <- function(...) log[[length(log) + 1]] <<- paste0(...)
  tick <- function(frac, msg) if (is.function(progress)) progress(frac, msg)
  t0 <- Sys.time()

  # shared_home keeps any downloaded extensions in ~/.duckdb instead of a
  # per-session temp dir (and silences the notice about it). Older duckdb
  # builds have no such argument, hence the fallback.
  drv <- tryCatch(duckdb::duckdb(shared_home = TRUE),
                  error = function(e) duckdb::duckdb())
  con <- DBI::dbConnect(drv)
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET preserve_insertion_order = true")
  # Allow spilling so a table larger than memory still runs.
  try(DBI::dbExecute(con, paste0("SET temp_directory = ", sq(tempdir()))), silent = TRUE)

  # 1. sources -------------------------------------------------------------
  tick(0.05, tr("run.reading"))
  views <- list()
  for (i in seq_along(spec$sources)) {
    s <- spec$sources[[i]]
    v <- ddb_source_view(con, s, root, i, warn, preview_rows)
    views[[scalar(s$id)]] <- v
    n <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ", qi(v$view)))$n[1]
    say("Read '", scalar(s$label, s$id), "': ", n, " rows x ", length(v$cols),
        " columns  <- ", paste(basename(v$files), collapse = ", "))
    tick(0.05 + 0.25 * i / max(1, length(spec$sources)), tr("run.reading"))
  }

  # 2. linking-field transformations ---------------------------------------
  tick(0.35, tr("run.transforming"))
  tf_n <- 0L
  for (t in spec$transforms %||% list()) {
    sid <- scalar(t$source_id, "")
    if (!sid %in% names(views)) { warn$add("Transformation skipped: source no longer exists."); next }
    v <- views[[sid]]
    infield <- scalar(t$input_field, "")
    if (!infield %in% v$cols) {
      dp_stop("Transformation on '", dp_source_label(spec, sid), "': field '", infield, "' not found.")
    }
    outfield <- scalar(t$output_field, ""); if (outfield == "") outfield <- infield
    expr <- sql_steps_expr(qi(infield), t$steps %||% list())
    if (is.null(expr)) dp_stop("internal: untranslatable step reached the SQL engine")

    tf_n <- tf_n + 1L
    new_cols <- v$cols
    exprs <- vapply(v$cols, function(c) paste0(qi(c), " AS ", qi(c)), character(1))
    if (outfield %in% v$cols) {
      exprs[match(outfield, v$cols)] <- paste0(expr, " AS ", qi(outfield))
    } else {
      exprs <- c(exprs, paste0(expr, " AS ", qi(outfield)))
      new_cols <- c(new_cols, outfield)
    }
    nm <- paste0("tf_", tf_n)

    counts <- DBI::dbGetQuery(con, paste0(
      "SELECT count(*) AS total, count(*) FILTER (WHERE ",
      "(", qi(infield), " IS NULL) <> ((", expr, ") IS NULL) OR (",
      qi(infield), " IS NOT NULL AND (", expr, ") IS NOT NULL AND ",
      qi(infield), " <> (", expr, "))) AS changed FROM ", qi(v$view)))
    changed <- counts$changed[1]; total <- counts$total[1]
    ddb_table(con, nm, paste0("SELECT ", paste(exprs, collapse = ", "), ", ", qi(RN),
                              " FROM ", qi(v$view)))

    views[[sid]] <- list(view = nm, cols = new_cols, files = v$files)
    say("Transformed '", dp_source_label(spec, sid), "'.", infield,
        if (outfield != infield) paste0(" -> ", outfield) else " (in place)",
        ": ", changed, " of ", total, " value(s) changed")
  }

  # 3. joins ----------------------------------------------------------------
  tick(0.5, tr("run.joining"))
  base_id <- scalar(spec$joins$base_source_id, "")
  if (!base_id %in% names(views)) dp_stop("The base table has not been chosen.")
  cur <- views[[base_id]]
  cur_view <- cur$view; cur_cols <- cur$cols
  say("Base table '", dp_source_label(spec, base_id), "': ", ddb_count(con, qi(cur_view)), " rows")

  steps <- spec$joins$steps %||% list()
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    rid <- scalar(st$right_source_id, "")
    if (!rid %in% names(views)) { warn$add("Join skipped: source no longer exists."); next }
    rl <- dp_source_label(spec, rid)
    rv <- views[[rid]]

    keys <- st$keys %||% list()
    lk <- vapply(keys, function(k) scalar(k$left, ""), character(1))
    rk <- vapply(keys, function(k) scalar(k$right, ""), character(1))
    miss_l <- setdiff(lk, cur_cols); miss_r <- setdiff(rk, rv$cols)
    if (length(miss_l)) dp_stop("Field(s) not found: ", paste(miss_l, collapse = ", "), ".")
    if (length(miss_r)) dp_stop("Field(s) not found: ", paste(miss_r, collapse = ", "), ".")

    icase <- as_bool(st$ignore_case); mblank <- as_bool(st$match_blanks)
    type <- scalar(st$type, "left")
    lkey <- ddb_key_expr(lk, icase, mblank, alias = "l")
    rkey_bare <- ddb_key_expr(rk, icase, mblank)

    # Duplicate keys on the right: same three choices as the R engine.
    right_rel <- qi(rv$view)
    multi <- scalar(st$multi_match, "all")
    ndup <- DBI::dbGetQuery(con, paste0(
      "SELECT count(*) - count(DISTINCT k) AS n FROM (SELECT ", rkey_bare,
      " AS k FROM ", qi(rv$view), " WHERE ", rkey_bare, " IS NOT NULL) t"))$n[1]
    if (ndup > 0) {
      if (multi == "error") {
        dp_stop("Join onto '", rl, "': the lookup table has ", ndup,
                " duplicate key(s). Set 'if the lookup has duplicates' to keep the first, ",
                "or de-duplicate the lookup file.")
      } else if (multi == "first") {
        dnm <- paste0("j", i, "_first")
        ddb_table(con, dnm, paste0(
          "SELECT * FROM ", qi(rv$view),
          " QUALIFY row_number() OVER (PARTITION BY coalesce(", rkey_bare,
          ", concat(chr(1), ", qi(RN), ")) ORDER BY ", qi(RN), ") = 1"))
        right_rel <- qi(dnm)
        warn$add("Join onto '", rl, "': ignored ", ndup,
                 " duplicate lookup row(s), kept the first of each.")
      }
    }
    rkey <- ddb_key_expr(rk, icase, mblank, alias = "r")

    n_left <- ddb_count(con, qi(cur_view))
    n_matched <- DBI::dbGetQuery(con, paste0(
      "SELECT count(*) AS n FROM ", qi(cur_view), " l WHERE EXISTS (SELECT 1 FROM ",
      right_rel, " r WHERE ", lkey, " = ", rkey, ")"))$n[1]
    n_unmatched <- n_left - n_matched

    nm <- paste0("j_", i)
    if (type %in% c("semi", "anti")) {
      neg <- if (type == "anti") "NOT " else ""
      ddb_table(con, nm, paste0(
        "SELECT ", paste(vapply(cur_cols, function(c) paste0("l.", qi(c), " AS ", qi(c)), character(1)),
                         collapse = ", "),
        ", l.", qi(RN), " AS ", qi(RN),
        " FROM ", qi(cur_view), " l WHERE ", neg, "EXISTS (SELECT 1 FROM ", right_rel,
        " r WHERE ", lkey, " = ", rkey, ")"))
      new_cols <- cur_cols
      added <- 0L
    } else {
      sel <- as_chr_vec(st$select)
      if (!length(sel)) sel <- setdiff(rv$cols, rk)
      missing_sel <- setdiff(as_chr_vec(st$select), rv$cols)
      if (length(missing_sel)) {
        warn$add("Join onto '", rl, "': field(s) ", paste(missing_sel, collapse = ", "),
                 " no longer exist and were skipped.")
      }
      sel <- intersect(sel, rv$cols)

      pre <- scalar(st$prefix, ""); suf <- scalar(st$suffix, "")
      conflict <- scalar(st$conflict, "suffix")
      left_exprs <- vapply(cur_cols, function(c) paste0("l.", qi(c), " AS ", qi(c)), character(1))
      names(left_exprs) <- cur_cols
      right_exprs <- character(0); new_cols <- cur_cols; added <- 0L

      for (s1 in sel) {
        target <- paste0(pre, s1, suf)
        if (target %in% new_cols) {
          if (conflict == "skip") next
          if (conflict == "right_wins") {
            left_exprs[[target]] <- paste0("r.", qi(s1), " AS ", qi(target))
          } else if (conflict == "coalesce") {
            left_exprs[[target]] <- paste0(
              "CASE WHEN l.", qi(target), " IS NULL OR l.", qi(target), " = '' ",
              "THEN r.", qi(s1), " ELSE l.", qi(target), " END AS ", qi(target))
          } else {
            nm2 <- target; k <- 2
            while (nm2 %in% new_cols) { nm2 <- paste0(target, "_", k); k <- k + 1 }
            right_exprs <- c(right_exprs, paste0("r.", qi(s1), " AS ", qi(nm2)))
            new_cols <- c(new_cols, nm2); added <- added + 1L
          }
        } else {
          right_exprs <- c(right_exprs, paste0("r.", qi(s1), " AS ", qi(target)))
          new_cols <- c(new_cols, target); added <- added + 1L
        }
      }

      jtype <- switch(type, left = "LEFT", inner = "INNER", full = "FULL", right = "RIGHT", "LEFT")
      # Re-derive the ordering column so the base table's order survives the
      # join; rows arriving only from the right side go last.
      ddb_table(con, nm, paste0(
        "SELECT ", paste(c(unname(left_exprs), right_exprs), collapse = ", "),
        ", row_number() OVER (ORDER BY l.", qi(RN), " NULLS LAST, r.", qi(RN),
        " NULLS LAST) AS ", qi(RN),
        " FROM ", qi(cur_view), " l ", jtype, " JOIN ", right_rel, " r ON ",
        lkey, " = ", rkey))
    }

    n_out <- ddb_count(con, qi(nm))
    if (n_out > n_left) {
      warn$add("Join onto '", rl, "': row count grew from ", n_left, " to ", n_out,
               " because some keys matched more than one lookup row.")
    }
    if (n_unmatched > 0 && type %in% c("left", "full")) {
      warn$add("Join onto '", rl, "': ", n_unmatched, " of ", n_left,
               " row(s) found no match and were left blank.")
    }
    say(toupper(type), " join onto '", rl, "': ", n_matched, " matched / ", n_unmatched,
        " unmatched, ", added, " column(s) added, ", n_out, " rows")

    cur_view <- nm; cur_cols <- new_cols
    tick(0.5 + 0.25 * i / length(steps), tr("run.joining"))
  }

  joined_view <- cur_view; joined_cols <- cur_cols
  if (identical(stop_after, "joins")) {
    d <- DBI::dbGetQuery(con, paste0("SELECT ", paste(vapply(joined_cols, qi, character(1)),
                                                      collapse = ", "),
                                     " FROM ", qi(joined_view), " ORDER BY ", qi(RN)))
    return(dp_result(d, log, warn, t0))
  }

  # 4. final shaping --------------------------------------------------------
  tick(0.8, tr("run.finalising"))

  # The filter runs on the combined table, before columns are dropped.
  where <- ddb_filter_sql(spec$final$filter %||% list(), joined_cols)
  filt_view <- joined_view
  if (!is.null(where)) {
    n_before <- ddb_count(con, qi(joined_view))
    filt_view <- ddb_table(con, "filtered", paste0("SELECT * FROM ", qi(joined_view),
                                                   " WHERE ", where))
    n_after <- ddb_count(con, qi(filt_view))
    if (n_after != n_before) say("Filter kept ", n_after, " of ", n_before, " rows")
  }

  fields <- spec$final$fields %||% list()
  if (length(fields)) {
    ordv <- vapply(fields, function(f) as_num(f$order, NA_real_), numeric(1))
    if (!all(is.na(ordv))) {
      ordv[is.na(ordv)] <- max(ordv, na.rm = TRUE) + seq_len(sum(is.na(ordv)))
      fields <- fields[order(ordv)]
    }
    exprs <- character(0); out_names <- character(0)
    for (f in fields) {
      if (!as_bool(f$include, TRUE)) next
      base <- ddb_field_expr(f, joined_cols)
      e <- sql_steps_expr(base, f$steps %||% list())
      if (is.null(e)) dp_stop("internal: untranslatable step reached the SQL engine")
      nm <- scalar(f$output, ""); if (nm == "") nm <- scalar(f$source, "column")
      exprs <- c(exprs, e); out_names <- c(out_names, nm)
    }
    if (!length(exprs)) dp_stop("No output fields are switched on.")
    out_names <- make_unique_names(out_names)
    sel <- paste(paste0(exprs, " AS ", vapply(out_names, qi, character(1))), collapse = ", ")
    final_view <- ddb_table(con, "shaped", paste0("SELECT ", sel, ", ", qi(RN),
                                                  " FROM ", qi(filt_view)))
    say("Selected ", length(out_names), " output column(s) from ", length(joined_cols), " available")
  } else {
    out_names <- joined_cols
    final_view <- filt_view
    say("No field selection set -- keeping all ", length(out_names), " joined columns")
  }

  # de-duplicate, matching the R engine's NA-and-blank-are-the-same rule
  dd <- spec$final$dedupe %||% list()
  if (as_bool(dd$enabled, FALSE)) {
    keys <- as_chr_vec(dd$keys); if (!length(keys)) keys <- out_names
    missing <- setdiff(keys, out_names)
    if (length(missing)) dp_stop("De-duplicate refers to missing field(s): ",
                                 paste(missing, collapse = ", "), ".")
    n_before <- ddb_count(con, qi(final_view))
    part <- paste(vapply(keys, function(k) paste0("coalesce(", qi(k), ", '')"), character(1)),
                  collapse = ", ")
    dir <- if (identical(scalar(dd$keep, "first"), "last")) "DESC" else "ASC"
    final_view <- ddb_table(con, "deduped", paste0(
      "SELECT * FROM ", qi(final_view), " QUALIFY row_number() OVER (PARTITION BY ", part,
      " ORDER BY ", qi(RN), " ", dir, ") = 1"))
    n_after <- ddb_count(con, qi(final_view))
    if (n_after != n_before) {
      warn$add("Removed ", n_before - n_after, " duplicate row(s).")
      say("De-duplicate kept ", n_after, " of ", n_before, " rows")
    }
  }

  # order: the configured sort, or the base table's original order
  sorts <- spec$final$sort %||% list()
  if (length(sorts)) {
    terms <- vapply(sorts, function(s) {
      f <- scalar(s$field, "")
      if (!f %in% out_names) dp_stop("Sort refers to missing field '", f, "'.")
      e <- if (as_bool(s$numeric, FALSE))
        paste0("try_cast(regexp_replace(", qi(f), ", '[[:space:],]', '', 'g') AS DOUBLE)")
      else qi(f)
      paste0(e, if (identical(scalar(s$dir, "asc"), "desc")) " DESC" else " ASC", " NULLS LAST")
    }, character(1))
    order_by <- paste(terms, collapse = ", ")
  } else {
    order_by <- qi(RN)
  }

  out <- DBI::dbGetQuery(con, paste0(
    "SELECT ", paste(vapply(out_names, qi, character(1)), collapse = ", "),
    " FROM ", qi(final_view), " ORDER BY ", order_by))
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(out) <- NULL

  if (identical(stop_after, "final")) return(dp_result(out, log, warn, t0))

  # 5. export ---------------------------------------------------------------
  tick(0.92, tr("run.exporting"))
  path <- dp_export_path(spec, root)
  dp_write_file(out, path, scalar(spec$export$format, NULL), spec$export$options %||% list())
  say("Wrote ", nrow(out), " rows x ", ncol(out), " columns to ", path)
  tick(1, tr("run.done"))

  res <- dp_result(out, log, warn, t0)
  res$export_path <- path
  res
}

# --- filter translation -----------------------------------------------------

ddb_filter_sql <- function(filt, cols) {
  conds <- filt$conditions %||% list()
  if (!length(conds)) return(NULL)
  mode <- scalar(filt$mode, "all")
  parts <- character(0)
  for (cnd in conds) {
    f <- scalar(cnd$field, "")
    if (!f %in% cols) dp_stop("Filter refers to missing field '", f, "'.")
    op <- scalar(cnd$op, "equals")
    val <- scalar(cnd$value, "")
    ic <- as_bool(cnd$ignore_case, TRUE)
    raw <- qi(f)
    # The R engine compares against "" for NULLs, so mirror that here.
    cmp <- paste0("coalesce(", raw, ", '')")
    if (ic) cmp <- paste0("lower(", cmp, ")")
    cval <- if (ic) tolower(val) else val
    num <- paste0("try_cast(regexp_replace(coalesce(", raw,
                  ", ''), '[[:space:],]', '', 'g') AS DOUBLE)")
    nval <- suppressWarnings(as.numeric(val))
    lst <- trimws(strsplit(cval, ",")[[1]])
    lst_sql <- if (length(lst)) paste(vapply(lst, sq, character(1)), collapse = ", ") else "''"
    e <- switch(
      op,
      equals       = paste0(cmp, " = ", sq(cval)),
      not_equals   = paste0(cmp, " <> ", sq(cval)),
      contains     = paste0("contains(", cmp, ", ", sq(cval), ")"),
      not_contains = paste0("NOT contains(", cmp, ", ", sq(cval), ")"),
      starts_with  = paste0("starts_with(", cmp, ", ", sq(cval), ")"),
      ends_with    = paste0("ends_with(", cmp, ", ", sq(cval), ")"),
      is_blank     = paste0("(", raw, " IS NULL OR ", sql_trimws(raw), " = '')"),
      not_blank    = paste0("(", raw, " IS NOT NULL AND ", sql_trimws(raw), " <> '')"),
      in_list      = paste0(cmp, " IN (", lst_sql, ")"),
      not_in_list  = paste0(cmp, " NOT IN (", lst_sql, ")"),
      regex        = paste0("regexp_matches(coalesce(", raw, ", ''), ", sq(val),
                            if (ic) ", 'i'" else "", ")"),
      gt  = if (is.na(nval)) "false" else paste0("(", num, " IS NOT NULL AND ", num, " > ", nval, ")"),
      gte = if (is.na(nval)) "false" else paste0("(", num, " IS NOT NULL AND ", num, " >= ", nval, ")"),
      lt  = if (is.na(nval)) "false" else paste0("(", num, " IS NOT NULL AND ", num, " < ", nval, ")"),
      lte = if (is.na(nval)) "false" else paste0("(", num, " IS NOT NULL AND ", num, " <= ", nval, ")"),
      dp_stop("Unknown filter condition '", op, "'.")
    )
    parts <- c(parts, paste0("coalesce(", e, ", false)"))
  }
  paste0("(", paste(parts, collapse = if (mode == "all") " AND " else " OR "), ")")
}

# --- output field expression ------------------------------------------------

ddb_field_expr <- function(field, cols) {
  combine <- as_chr_vec(field$combine)
  if (length(combine) > 1) {
    missing <- setdiff(combine, cols)
    if (length(missing)) dp_stop("Field '", missing[1], "' not found.")
    sepc <- scalar(field$combine_sep, " ")
    parts <- paste(vapply(combine, function(f) paste0("coalesce(", qi(f), ", '')"), character(1)),
                   collapse = paste0(", ", sq(sepc), ", "))
    e <- paste0("concat(", parts, ")")
    if (as_bool(field$combine_skip_blank, TRUE) && nzchar(sepc)) {
      esc <- re_esc(sepc)
      e <- paste0("regexp_replace(", e, ", '(", esc, ")+', ", sq(sepc), ", 'g')")
      e <- paste0("regexp_replace(", e, ", '^", esc, "|", esc, "$', '', 'g')")
    }
    return(e)
  }
  src <- scalar(field$source, "")
  if (!identical(src, "") && !is.na(src)) {
    if (!src %in% cols) dp_stop("Field '", src, "' not found in the joined table.")
    return(qi(src))
  }
  "NULL"
}

# --- engine selection -------------------------------------------------------

# Single entry point used by the app and the command-line runner. Picks the
# DuckDB engine when it is available and the pipeline is fully translatable,
# and otherwise runs in R -- always saying which, and why.
dp_run <- function(spec, root = NULL, stop_after = "export", progress = NULL,
                   preview_rows = NULL, engine = NULL) {
  engine <- engine %||% scalar(spec$engine, "auto") %||% "auto"

  if (identical(engine, "r")) return(dp_execute(spec, root, stop_after, progress, preview_rows))

  reason <- NULL
  if (!dp_duckdb_available()) {
    reason <- "the duckdb package is not installed"
  } else {
    bad <- sql_untranslatable_ops(spec)
    if (length(bad)) {
      reason <- paste0("these operations have no exact SQL equivalent: ",
                       paste(bad, collapse = ", "))
    }
  }

  if (is.null(reason)) {
    res <- dp_execute_duckdb(spec, root, stop_after, progress, preview_rows)
    res$engine <- "duckdb"
    return(res)
  }

  if (identical(engine, "duckdb")) {
    dp_stop("The DuckDB engine cannot run this pipeline: ", reason,
            ". Use the R engine instead.")
  }
  res <- dp_execute(spec, root, stop_after, progress, preview_rows)
  res$engine <- "r"
  res$log <- c(list(paste0("Ran in R rather than DuckDB because ", reason, ".")), res$log)
  res
}
