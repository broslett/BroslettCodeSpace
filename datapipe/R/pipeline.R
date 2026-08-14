# ---------------------------------------------------------------------------
# pipeline.R -- the pipeline specification, its validation, and the engine
# that executes it.
#
# A pipeline is a plain list that serialises cleanly to JSON. That is the
# whole portability story: the JSON file plus the folders it points at are
# everything needed to reproduce a run, in the app or from the command line.
# ---------------------------------------------------------------------------

DP_SPEC_VERSION <- 1L

# --- construction -----------------------------------------------------------

dp_new_pipeline <- function(name = "Untitled pipeline") {
  list(
    spec_version = DP_SPEC_VERSION,
    name = name,
    description = "",
    created = timestamp_now(),
    modified = timestamp_now(),
    engine = "auto",
    sources = list(),
    transforms = list(),
    joins = list(base_source_id = NULL, steps = list()),
    final = list(fields = list(), filter = list(mode = "all", conditions = list()),
                 dedupe = list(enabled = FALSE, keys = list(), keep = "first"),
                 sort = list()),
    export = list(path = "output/result.csv", format = "csv",
                  options = list(header = TRUE, na_string = "", eol = "lf"),
                  timestamp_filename = FALSE)
  )
}

dp_new_source <- function(label, path, role = "data", format = NULL, options = list()) {
  list(id = new_id("src"), label = label, role = role,
       path_mode = "file", path = path, dir = "", pattern = "", pick = "latest",
       format = format %||% dp_detect_format(path), options = options)
}

dp_source_by_id <- function(spec, id) {
  for (s in spec$sources) if (identical(scalar(s$id), scalar(id))) return(s)
  NULL
}

dp_source_label <- function(spec, id) {
  s <- dp_source_by_id(spec, id)
  if (is.null(s)) return(paste0("<missing source ", id, ">"))
  scalar(s$label, s$id)
}

# --- validation -------------------------------------------------------------
# Returns a character vector of problems; empty means the pipeline can run.

dp_validate <- function(spec) {
  p <- character(0)
  if (!length(spec$sources)) p <- c(p, "Add at least one input file.")

  ids <- vapply(spec$sources, function(s) scalar(s$id, ""), character(1))
  if (anyDuplicated(ids)) p <- c(p, "Two sources share the same internal id.")

  for (s in spec$sources) {
    lbl <- scalar(s$label, s$id)
    if (identical(scalar(s$path_mode, "file"), "pattern")) {
      if (identical(scalar(s$dir, ""), "")) p <- c(p, paste0("Source '", lbl, "' has no folder set."))
      if (identical(scalar(s$pattern, ""), "")) p <- c(p, paste0("Source '", lbl, "' has no filename pattern set."))
    } else if (identical(scalar(s$path, ""), "")) {
      p <- c(p, paste0("Source '", lbl, "' has no file selected."))
    }
  }

  base <- scalar(spec$joins$base_source_id, "")
  if (length(spec$sources) > 0 && base == "") {
    p <- c(p, "Choose which table the others are joined onto.")
  } else if (base != "" && is.null(dp_source_by_id(spec, base))) {
    p <- c(p, "The base table refers to a source that no longer exists.")
  }

  for (st in spec$joins$steps %||% list()) {
    rl <- dp_source_label(spec, st$right_source_id)
    if (is.null(dp_source_by_id(spec, st$right_source_id))) {
      p <- c(p, paste0("A join refers to a source that no longer exists."))
      next
    }
    keys <- st$keys %||% list()
    if (!length(keys)) {
      p <- c(p, paste0("Join onto '", rl, "' has no matching fields."))
    } else {
      for (k in keys) {
        if (identical(scalar(k$left, ""), "") || identical(scalar(k$right, ""), "")) {
          p <- c(p, paste0("Join onto '", rl, "' has an incomplete field pair."))
        }
      }
    }
  }

  for (t in spec$transforms %||% list()) {
    if (identical(scalar(t$input_field, ""), "")) {
      p <- c(p, "A field transformation has no input field selected.")
    }
    for (stp in t$steps %||% list()) {
      if (is.null(TF_OPS[[scalar(stp$op, "")]])) {
        p <- c(p, paste0("Unknown transformation '", scalar(stp$op, "?"), "'."))
      }
    }
  }

  inc <- Filter(function(f) as_bool(f$include, TRUE), spec$final$fields %||% list())
  if (length(spec$final$fields %||% list()) && !length(inc)) {
    p <- c(p, "Every output field is switched off -- the export would be empty.")
  }
  outs <- vapply(inc, function(f) scalar(f$output, ""), character(1))
  if (length(outs) && anyDuplicated(outs)) {
    dup <- unique(outs[duplicated(outs)])
    p <- c(p, paste0("Duplicate output column name(s): ", paste(dup, collapse = ", "), "."))
  }
  if (any(outs == "")) p <- c(p, "An output field has a blank name.")

  if (identical(scalar(spec$export$path, ""), "")) p <- c(p, "Set an export file path.")

  unique(p)
}

# --- key building -----------------------------------------------------------

.KEYSEP <- "\u001f"

# Build a single composite key column from one or more fields. Rows with a
# blank key get a unique sentinel so they can never match another blank row --
# joining all the unknowns to each other is never what anyone means.
dp_build_key <- function(df, fields, ignore_case = FALSE, match_blanks = FALSE, tag = "") {
  fields <- as_chr_vec(fields)
  missing <- setdiff(fields, names(df))
  if (length(missing)) dp_stop("Field(s) not found: ", paste(missing, collapse = ", "), ".")
  parts <- lapply(fields, function(f) {
    v <- as.character(df[[f]])
    v[is.na(v)] <- ""
    if (ignore_case) v <- tolower(v)
    v
  })
  key <- do.call(paste, c(parts, list(sep = .KEYSEP)))
  blank <- vapply(seq_along(key), function(i) all(vapply(parts, function(p) p[i] == "", logical(1))), logical(1))
  if (!match_blanks && any(blank)) {
    # The tag keeps the two sides of a join from generating identical
    # sentinels -- without it a blank key on the left would match a blank
    # key on the right, the exact thing this is meant to prevent.
    key[blank] <- paste0("\u0001blank", tag, ":", seq_len(sum(blank)))
  }
  key
}

# --- the join engine --------------------------------------------------------

dp_apply_join <- function(left, right, step, warn = NULL, right_label = "table") {
  keys <- step$keys %||% list()
  lk <- vapply(keys, function(k) scalar(k$left, ""), character(1))
  rk <- vapply(keys, function(k) scalar(k$right, ""), character(1))
  ignore_case <- as_bool(step$ignore_case, FALSE)
  match_blanks <- as_bool(step$match_blanks, FALSE)
  type <- scalar(step$type, "left")

  lkey <- dp_build_key(left, lk, ignore_case, match_blanks, tag = "L")
  rkey <- dp_build_key(right, rk, ignore_case, match_blanks, tag = "R")

  # Which right-hand columns come across.
  sel <- as_chr_vec(step$select)
  if (!length(sel)) sel <- setdiff(names(right), rk)
  sel <- intersect(sel, names(right))
  missing_sel <- setdiff(as_chr_vec(step$select), names(right))
  if (length(missing_sel) && !is.null(warn)) {
    warn$add("Join onto '", right_label, "': field(s) ",
             paste(missing_sel, collapse = ", "), " no longer exist and were skipped.")
  }

  # Duplicate keys on the right multiply rows -- make that an explicit choice.
  dup_keys <- duplicated(rkey)
  multi <- scalar(step$multi_match, "all")
  if (any(dup_keys)) {
    if (multi == "error") {
      dp_stop("Join onto '", right_label, "': the lookup table has ", sum(dup_keys),
              " duplicate key(s). Set 'if the lookup has duplicates' to keep the first, ",
              "or de-duplicate the lookup file.")
    } else if (multi == "first") {
      keep <- !dup_keys
      right <- right[keep, , drop = FALSE]
      rkey <- rkey[keep]
      if (!is.null(warn)) warn$add("Join onto '", right_label, "': ignored ", sum(dup_keys),
                                   " duplicate lookup row(s), kept the first of each.")
    }
  }

  matched_left <- lkey %in% rkey
  n_unmatched <- sum(!matched_left)

  # Semi/anti joins are filters, not merges.
  if (type == "semi") {
    out <- left[matched_left, , drop = FALSE]
    return(list(data = out, stats = list(right_rows = nrow(right),
                                         matched = sum(matched_left),
                                         unmatched = n_unmatched,
                                         rows_out = nrow(out), added = 0L)))
  }
  if (type == "anti") {
    out <- left[!matched_left, , drop = FALSE]
    return(list(data = out, stats = list(right_rows = nrow(right),
                                         matched = sum(matched_left),
                                         unmatched = n_unmatched,
                                         rows_out = nrow(out), added = 0L)))
  }

  # Name the incoming columns, resolving clashes as the user asked.
  pre <- scalar(step$prefix, ""); suf <- scalar(step$suffix, "")
  target <- paste0(pre, sel, suf)
  conflict <- scalar(step$conflict, "suffix")
  existing <- names(left)
  tmp <- paste0("r", seq_along(sel))

  rsub <- right[, sel, drop = FALSE]
  names(rsub) <- tmp
  rsub[[".__key"]] <- rkey

  ldt <- data.table::as.data.table(left)
  ldt[[".__key"]] <- lkey
  ldt[[".__row"]] <- seq_len(nrow(ldt))
  rdt <- data.table::as.data.table(rsub)

  all_x <- type %in% c("left", "full")
  all_y <- type %in% c("right", "full")
  merged <- merge(ldt, rdt, by = ".__key", all.x = all_x, all.y = all_y,
                  allow.cartesian = TRUE, sort = FALSE)
  merged <- as.data.frame(merged, stringsAsFactors = FALSE, check.names = FALSE)
  # Restore the left-hand row order; new rows from a full/right join go last.
  ord <- order(is.na(merged[[".__row"]]), merged[[".__row"]], method = "radix")
  merged <- merged[ord, , drop = FALSE]
  merged[[".__key"]] <- NULL
  merged[[".__row"]] <- NULL
  rownames(merged) <- NULL

  added <- 0L
  for (i in seq_along(sel)) {
    src_col <- tmp[i]
    if (!src_col %in% names(merged)) next
    nm <- target[i]
    vals <- as.character(merged[[src_col]])
    merged[[src_col]] <- NULL
    if (nm %in% names(merged)) {
      if (conflict == "skip") {
        next
      } else if (conflict == "right_wins") {
        merged[[nm]] <- vals
      } else if (conflict == "coalesce") {
        cur <- as.character(merged[[nm]])
        fill <- is.na(cur) | cur == ""
        cur[fill] <- vals[fill]
        merged[[nm]] <- cur
      } else { # suffix
        nm2 <- nm; k <- 2
        while (nm2 %in% names(merged)) { nm2 <- paste0(nm, "_", k); k <- k + 1 }
        merged[[nm2]] <- vals
        added <- added + 1L
      }
    } else {
      merged[[nm]] <- vals
      added <- added + 1L
    }
  }
  # Drop any helper column that survived (e.g. nothing selected).
  merged <- merged[, !grepl("^r[0-9]+$", names(merged)), drop = FALSE]

  if (nrow(merged) > nrow(left) && !is.null(warn)) {
    warn$add("Join onto '", right_label, "': row count grew from ", nrow(left), " to ",
             nrow(merged), " because some keys matched more than one lookup row.")
  }
  if (n_unmatched > 0 && type %in% c("left", "full") && !is.null(warn)) {
    warn$add("Join onto '", right_label, "': ", n_unmatched, " of ", length(lkey),
             " row(s) found no match and were left blank.")
  }

  list(data = merged, stats = list(right_rows = nrow(right),
                                   matched = sum(matched_left),
                                   unmatched = n_unmatched,
                                   rows_out = nrow(merged), added = added))
}

# --- final shaping ----------------------------------------------------------

dp_field_value <- function(df, field) {
  src <- as_chr_vec(field$source)
  combine <- as_chr_vec(field$combine)
  if (length(combine) > 1) {
    sepc <- scalar(field$combine_sep, " ")
    parts <- lapply(combine, function(f) {
      if (!f %in% names(df)) dp_stop("Field '", f, "' not found.")
      v <- as.character(df[[f]]); v[is.na(v)] <- ""; v
    })
    v <- do.call(paste, c(parts, list(sep = sepc)))
    if (as_bool(field$combine_skip_blank, TRUE)) {
      # Collapse the separator runs left behind by blank parts.
      if (nzchar(sepc)) {
        v <- gsub(paste0("(", gsub("([][{}()*+?.\\^$|])", "\\\\\\1", sepc), ")+"), sepc, v)
        v <- gsub(paste0("^", gsub("([][{}()*+?.\\^$|])", "\\\\\\1", sepc), "|",
                         gsub("([][{}()*+?.\\^$|])", "\\\\\\1", sepc), "$"), "", v)
      }
    }
    return(v)
  }
  if (length(src) == 1 && !is.na(src) && src != "") {
    if (!src %in% names(df)) dp_stop("Field '", src, "' not found in the joined table.")
    return(as.character(df[[src]]))
  }
  rep(NA_character_, nrow(df))
}

dp_apply_filter <- function(df, filt) {
  conds <- filt$conditions %||% list()
  if (!length(conds)) return(df)
  mode <- scalar(filt$mode, "all")
  keep <- rep(mode == "all", nrow(df))
  for (cnd in conds) {
    f <- scalar(cnd$field, "")
    if (!f %in% names(df)) dp_stop("Filter refers to missing field '", f, "'.")
    v <- as.character(df[[f]])
    val <- scalar(cnd$value, "")
    op <- scalar(cnd$op, "equals")
    ic <- as_bool(cnd$ignore_case, TRUE)
    cmp <- if (ic) tolower(ifelse(is.na(v), "", v)) else ifelse(is.na(v), "", v)
    cval <- if (ic) tolower(val) else val
    num <- suppressWarnings(as.numeric(gsub("[[:space:],]", "", v)))
    nval <- suppressWarnings(as.numeric(val))
    hit <- switch(
      op,
      equals        = cmp == cval,
      not_equals    = cmp != cval,
      contains      = grepl(cval, cmp, fixed = TRUE),
      not_contains  = !grepl(cval, cmp, fixed = TRUE),
      starts_with   = startsWith(cmp, cval),
      ends_with     = endsWith(cmp, cval),
      is_blank      = is.na(v) | trimws(v) == "",
      not_blank     = !is.na(v) & trimws(v) != "",
      in_list       = cmp %in% trimws(strsplit(cval, ",")[[1]]),
      not_in_list   = !(cmp %in% trimws(strsplit(cval, ",")[[1]])),
      regex         = grepl(val, ifelse(is.na(v), "", v), perl = TRUE, ignore.case = ic),
      gt            = !is.na(num) & num >  nval,
      gte           = !is.na(num) & num >= nval,
      lt            = !is.na(num) & num <  nval,
      lte           = !is.na(num) & num <= nval,
      dp_stop("Unknown filter condition '", op, "'.")
    )
    hit[is.na(hit)] <- FALSE
    keep <- if (mode == "all") keep & hit else keep | hit
  }
  df[keep, , drop = FALSE]
}

dp_apply_sort <- function(df, sorts) {
  sorts <- sorts %||% list()
  if (!length(sorts) || !nrow(df)) return(df)
  cols <- list()
  for (s in sorts) {
    f <- scalar(s$field, "")
    if (!f %in% names(df)) dp_stop("Sort refers to missing field '", f, "'.")
    v <- df[[f]]
    if (as_bool(s$numeric, FALSE)) v <- suppressWarnings(as.numeric(gsub("[[:space:],]", "", v)))
    if (identical(scalar(s$dir, "asc"), "desc")) {
      cols[[length(cols) + 1]] <- if (is.numeric(v)) -xtfrm(v) else -xtfrm(v)
    } else {
      cols[[length(cols) + 1]] <- xtfrm(v)
    }
  }
  df[do.call(order, c(cols, list(method = "radix"))), , drop = FALSE]
}

dp_apply_dedupe <- function(df, dd, warn = NULL) {
  if (!as_bool(dd$enabled, FALSE) || !nrow(df)) return(df)
  keys <- as_chr_vec(dd$keys)
  if (!length(keys)) keys <- names(df)
  missing <- setdiff(keys, names(df))
  if (length(missing)) dp_stop("De-duplicate refers to missing field(s): ", paste(missing, collapse = ", "), ".")
  key <- do.call(paste, c(lapply(keys, function(k) {
    v <- as.character(df[[k]]); v[is.na(v)] <- ""; v
  }), list(sep = .KEYSEP)))
  dup <- if (identical(scalar(dd$keep, "first"), "last")) duplicated(key, fromLast = TRUE) else duplicated(key)
  if (any(dup) && !is.null(warn)) warn$add("Removed ", sum(dup), " duplicate row(s).")
  df[!dup, , drop = FALSE]
}

# --- execution --------------------------------------------------------------
# stop_after: "sources" | "transforms" | "joins" | "final" | "export"
# Used by the app to preview intermediate stages without writing a file.

dp_execute <- function(spec, root = NULL, stop_after = "export", progress = NULL,
                       preview_rows = NULL) {
  warn <- dp_warn_collector()
  log <- list()
  say <- function(...) log[[length(log) + 1]] <<- paste0(...)
  tick <- function(frac, msg) if (is.function(progress)) progress(frac, msg)

  t0 <- Sys.time()

  # 1. Read every source ----------------------------------------------------
  tick(0.05, tr("run.reading"))
  tables <- list()
  for (i in seq_along(spec$sources)) {
    s <- spec$sources[[i]]
    lbl <- scalar(s$label, s$id)
    df <- dp_read_source(s, root, warn)
    if (!is.null(preview_rows) && nrow(df) > preview_rows && identical(scalar(s$role, "data"), "data")) {
      df <- df[seq_len(preview_rows), , drop = FALSE]
    }
    tables[[scalar(s$id)]] <- df
    files <- attr(df, "dp_files")
    say("Read '", lbl, "': ", nrow(df), " rows x ", ncol(df), " columns  <- ",
        paste(basename(files), collapse = ", "))
    tick(0.05 + 0.25 * i / max(1, length(spec$sources)), tr("run.reading"))
  }
  if (identical(stop_after, "sources")) {
    return(dp_result(tables, log, warn, t0, tables = tables))
  }

  # 2. Linking-field transformations ---------------------------------------
  tick(0.35, tr("run.transforming"))
  for (t in spec$transforms %||% list()) {
    sid <- scalar(t$source_id, "")
    if (!sid %in% names(tables)) {
      warn$add("Transformation skipped: source no longer exists.")
      next
    }
    df <- tables[[sid]]
    infield <- scalar(t$input_field, "")
    if (!infield %in% names(df)) {
      dp_stop("Transformation on '", dp_source_label(spec, sid), "': field '", infield, "' not found.")
    }
    outfield <- scalar(t$output_field, "")
    if (outfield == "") outfield <- infield
    before <- as.character(df[[infield]])
    after <- tf_apply_steps(before, t$steps %||% list())
    df[[outfield]] <- after
    tables[[sid]] <- df
    changed <- sum(is.na(before) != is.na(after) |
                     (!is.na(before) & !is.na(after) & before != after))
    say("Transformed '", dp_source_label(spec, sid), "'.", infield,
        if (outfield != infield) paste0(" -> ", outfield) else " (in place)",
        ": ", changed, " of ", length(before), " value(s) changed")
  }
  if (identical(stop_after, "transforms")) {
    return(dp_result(tables, log, warn, t0, tables = tables))
  }

  # 3. Amalgamate -----------------------------------------------------------
  tick(0.5, tr("run.joining"))
  base_id <- scalar(spec$joins$base_source_id, "")
  if (!base_id %in% names(tables)) dp_stop("The base table has not been chosen.")
  out <- tables[[base_id]]
  say("Base table '", dp_source_label(spec, base_id), "': ", nrow(out), " rows")

  steps <- spec$joins$steps %||% list()
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    rid <- scalar(st$right_source_id, "")
    if (!rid %in% names(tables)) {
      warn$add("Join skipped: source no longer exists.")
      next
    }
    rl <- dp_source_label(spec, rid)
    res <- dp_apply_join(out, tables[[rid]], st, warn, rl)
    out <- res$data
    s <- res$stats
    say(toupper(scalar(st$type, "left")), " join onto '", rl, "': ",
        s$matched, " matched / ", s$unmatched, " unmatched, ",
        s$added, " column(s) added, ", s$rows_out, " rows")
    tick(0.5 + 0.25 * i / length(steps), tr("run.joining"))
  }
  joined <- out
  if (identical(stop_after, "joins")) {
    return(dp_result(joined, log, warn, t0, tables = tables))
  }

  # 4. Finalise the table ---------------------------------------------------
  tick(0.8, tr("run.finalising"))

  # The row filter runs against the combined table, before columns are
  # dropped, so it can test fields that are not exported (a status flag, say).
  n_before <- nrow(joined)
  joined <- dp_apply_filter(joined, spec$final$filter %||% list())
  if (nrow(joined) != n_before) say("Filter kept ", nrow(joined), " of ", n_before, " rows")

  fields <- spec$final$fields %||% list()
  if (length(fields)) {
    ordv <- vapply(fields, function(f) as_num(f$order, NA_real_), numeric(1))
    if (!all(is.na(ordv))) {
      ordv[is.na(ordv)] <- max(ordv, na.rm = TRUE) + seq_len(sum(is.na(ordv)))
      fields <- fields[order(ordv)]
    }
    final <- list()
    nms <- character(0)
    for (f in fields) {
      if (!as_bool(f$include, TRUE)) next
      v <- dp_field_value(joined, f)
      v <- tf_apply_steps(v, f$steps %||% list())
      nm <- scalar(f$output, "")
      if (nm == "") nm <- scalar(f$source, "column")
      final[[length(final) + 1]] <- v
      nms <- c(nms, nm)
    }
    if (!length(final)) dp_stop("No output fields are switched on.")
    out <- as.data.frame(final, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- make_unique_names(nms)
    say("Selected ", ncol(out), " output column(s) from ", ncol(joined), " available")
  } else {
    out <- joined
    say("No field selection set -- keeping all ", ncol(out), " joined columns")
  }

  n_before <- nrow(out)
  out <- dp_apply_dedupe(out, spec$final$dedupe %||% list(), warn)
  if (nrow(out) != n_before) say("De-duplicate kept ", nrow(out), " of ", n_before, " rows")

  out <- dp_apply_sort(out, spec$final$sort %||% list())
  rownames(out) <- NULL

  if (identical(stop_after, "final")) {
    return(dp_result(out, log, warn, t0, tables = tables, joined = joined))
  }

  # 5. Export ---------------------------------------------------------------
  tick(0.92, tr("run.exporting"))
  path <- dp_export_path(spec, root)
  dp_write_file(out, path, scalar(spec$export$format, NULL), spec$export$options %||% list())
  say("Wrote ", nrow(out), " rows x ", ncol(out), " columns to ", path)
  tick(1, tr("run.done"))

  res <- dp_result(out, log, warn, t0, tables = tables, joined = joined)
  res$export_path <- path
  res
}

dp_export_path <- function(spec, root = NULL) {
  p <- scalar(spec$export$path, "output/result.csv")
  if (as_bool(spec$export$timestamp_filename, FALSE)) {
    ext <- file_ext(p)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    p <- paste0(sub(paste0("\\.", ext, "$"), "", p), "_", stamp, ".", ext)
  }
  resolve_path(p, root)
}

dp_result <- function(data, log, warn, t0, tables = NULL, joined = NULL) {
  list(data = data, log = log, warnings = warn$get(), tables = tables,
       joined = joined, elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       export_path = NULL)
}

# --- persistence ------------------------------------------------------------

dp_save_pipeline <- function(spec, path) {
  spec$modified <- timestamp_now()
  spec$spec_version <- DP_SPEC_VERSION
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(spec, auto_unbox = TRUE, pretty = TRUE, null = "null",
                           digits = NA, force = TRUE)
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

dp_load_pipeline <- function(path) {
  if (!file.exists(path)) dp_stop("Pipeline file not found: ", path)
  spec <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) dp_stop("Could not read pipeline file: ", conditionMessage(e))
  )
  dp_migrate_pipeline(spec)
}

# Fill in anything an older or hand-edited file is missing, so a partial JSON
# never turns into a confusing error deep inside the engine.
dp_migrate_pipeline <- function(spec) {
  base <- dp_new_pipeline()
  if (!is.list(spec)) dp_stop("Pipeline file is not a valid pipeline.")
  v <- as_int(spec$spec_version, 1L)
  if (v > DP_SPEC_VERSION) {
    dp_stop("This pipeline was saved by a newer version of the app (format ", v, ").")
  }
  for (nm in names(base)) if (is.null(spec[[nm]])) spec[[nm]] <- base[[nm]]
  if (is.null(spec$joins$steps)) spec$joins$steps <- list()
  for (nm in names(base$final)) if (is.null(spec$final[[nm]])) spec$final[[nm]] <- base$final[[nm]]
  for (nm in names(base$export)) if (is.null(spec$export[[nm]])) spec$export[[nm]] <- base$export[[nm]]
  spec$sources <- lapply(spec$sources %||% list(), function(s) {
    if (is.null(s$id)) s$id <- new_id("src")
    if (is.null(s$role)) s$role <- "data"
    if (is.null(s$path_mode)) s$path_mode <- "file"
    if (is.null(s$options)) s$options <- list()
    s
  })
  spec$spec_version <- DP_SPEC_VERSION
  spec
}

dp_list_pipelines <- function(dir) {
  if (!dir.exists(dir)) return(data.frame())
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(data.frame())
  rows <- lapply(files, function(f) {
    s <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    data.frame(
      file = basename(f),
      name = scalar(s$name, tools::file_path_sans_ext(basename(f))) %||% basename(f),
      description = scalar(s$description, "") %||% "",
      sources = length(s$sources %||% list()),
      modified = scalar(s$modified, "") %||% "",
      path = f,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Human-readable outline of a pipeline, used on the "start a pipeline" screen
# and by the CLI runner's --describe flag.
dp_describe <- function(spec) {
  out <- c(paste0("Pipeline: ", scalar(spec$name, "(unnamed)")))
  if (nzchar(scalar(spec$description, ""))) out <- c(out, paste0("  ", spec$description))
  out <- c(out, "", "Inputs:")
  for (s in spec$sources) {
    loc <- if (identical(scalar(s$path_mode, "file"), "pattern")) {
      paste0(scalar(s$dir, ""), "/", scalar(s$pattern, ""), "  [", scalar(s$pick, "latest"), "]")
    } else scalar(s$path, "")
    out <- c(out, paste0("  - ", scalar(s$label, s$id), " (", scalar(s$role, "data"), ", ",
                         scalar(s$format, "?"), "): ", loc))
  }
  tr_list <- spec$transforms %||% list()
  if (length(tr_list)) {
    out <- c(out, "", "Linking-field transformations:")
    for (t in tr_list) {
      steps <- paste(vapply(t$steps %||% list(), tf_step_label, character(1)), collapse = " -> ")
      out <- c(out, paste0("  - ", dp_source_label(spec, t$source_id), ".",
                           scalar(t$input_field, "?"), " -> ", scalar(t$output_field, "?"),
                           ": ", if (nzchar(steps)) steps else "(no steps)"))
    }
  }
  out <- c(out, "", paste0("Base table: ", dp_source_label(spec, spec$joins$base_source_id)))
  for (st in spec$joins$steps %||% list()) {
    keys <- paste(vapply(st$keys %||% list(), function(k)
      paste0(scalar(k$left, "?"), " = ", scalar(k$right, "?")), character(1)), collapse = " AND ")
    out <- c(out, paste0("  ", toupper(scalar(st$type, "left")), " join ",
                         dp_source_label(spec, st$right_source_id), " ON ", keys))
  }
  inc <- Filter(function(f) as_bool(f$include, TRUE), spec$final$fields %||% list())
  out <- c(out, "", paste0("Output columns (", length(inc), "):"))
  for (f in inc) {
    steps <- paste(vapply(f$steps %||% list(), tf_step_label, character(1)), collapse = " -> ")
    out <- c(out, paste0("  - ", scalar(f$output, "?"),
                         if (!identical(scalar(f$output, ""), scalar(f$source, "")))
                           paste0("  <- ", scalar(f$source, "(combined)")) else "",
                         if (nzchar(steps)) paste0("  [", steps, "]") else ""))
  }
  out <- c(out, "", paste0("Export: ", scalar(spec$export$path, "?"),
                           " (", scalar(spec$export$format, "?"), ")"))
  out
}
