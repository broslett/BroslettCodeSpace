#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# app.R -- the Data Pipeline Builder user interface.
#
#   Rscript app.R              (then open the printed http://127.0.0.1 address)
#
# The UI never transforms data itself: every preview and every run goes through
# the same dp_execute() the command-line runner uses, so what you see while
# building is exactly what a scheduled re-run produces.
# ---------------------------------------------------------------------------

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]))) else getwd()
})
options(datapipe.app_dir = app_dir, shiny.maxRequestSize = 512 * 1024^2)

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT)
  library(jsonlite); library(data.table); library(shinyjs)
})
for (f in c("utils.R", "i18n.R", "ops.R", "io.R", "pipeline.R")) {
  source(file.path(app_dir, "R", f), local = FALSE)
}
dp_set_language(Sys.getenv("DATAPIPE_LANG", "en"), file.path(app_dir, "locale"))

PIPELINE_DIR <- file.path(app_dir, "pipelines")
PREVIEW_ROWS <- 500L
dir.create(PIPELINE_DIR, showWarnings = FALSE, recursive = TRUE)

# ===========================================================================
# UI
# ===========================================================================

card_help <- function(...) div(class = "text-muted small mb-3", ...)

ui <- page_navbar(
  id = "nav",
  title = tr("app.title"),
  theme = bs_theme(version = 5, preset = "flatly"),
  header = tagList(
    useShinyjs(),
    tags$style(HTML("
      .dp-item { border:1px solid #dee2e6; border-radius:.5rem; padding:.9rem;
                 margin-bottom:.9rem; background:#fff; }
      .dp-item h6 { font-weight:600; margin-bottom:.6rem; }
      .dp-step { border-left:3px solid #18bc9c; background:#f8f9fa;
                 padding:.6rem .8rem; margin-bottom:.5rem; border-radius:.25rem; }
      .dp-problems { border-left:4px solid #e74c3c; background:#fdf3f2;
                     padding:.6rem .9rem; border-radius:.25rem; }
      .dp-ready { border-left:4px solid #18bc9c; background:#f2fbf8;
                  padding:.6rem .9rem; border-radius:.25rem; }
      .dp-log { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.8rem;
                white-space:pre-wrap; background:#f8f9fa; padding:.75rem;
                border-radius:.25rem; max-height:22rem; overflow:auto; }
      .form-group, .shiny-input-container { margin-bottom:.6rem; }
      .dp-narrow .form-control, .dp-narrow .form-select { font-size:.875rem; }
      table.dataTable tbody td { font-size:.82rem; white-space:nowrap; }
    "))
  ),
  nav_panel(tr("nav.start"),   value = "start",   uiOutput("ui_start")),
  nav_panel(tr("nav.sources"), value = "sources", uiOutput("ui_sources")),
  nav_panel(tr("nav.keys"),    value = "keys",    uiOutput("ui_keys")),
  nav_panel(tr("nav.join"),    value = "join",    uiOutput("ui_join")),
  nav_panel(tr("nav.final"),   value = "final",   uiOutput("ui_final")),
  nav_panel(tr("nav.export"),  value = "export",  uiOutput("ui_export")),
  nav_spacer(),
  nav_item(uiOutput("status_pill"))
)

# ===========================================================================
# Server
# ===========================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    spec = dp_new_pipeline("New pipeline"),
    root = app_dir,
    version = 0L,      # bumped on structural change -> re-render dynamic UI
    tables = NULL,     # cached preview reads, keyed by source id
    tables_key = NULL,
    result = NULL,
    browse_target = NULL,
    browse_dir = app_dir,
    browse_mode = "file"
  )

  bump <- function() rv$version <- rv$version + 1L

  # Dynamic inputs need dynamically created observers. Keep a registry so the
  # previous generation is destroyed before a new one is built, otherwise every
  # re-render would stack another handler on the same button.
  obs <- list()
  clear_obs <- function() { for (o in obs) try(o$destroy(), silent = TRUE); obs <<- list() }
  add_obs <- function(o) { obs[[length(obs) + 1]] <<- o; invisible(NULL) }

  gv <- function(id, fallback = NULL) {
    v <- input[[id]]
    if (is.null(v)) fallback else v
  }

  # -----------------------------------------------------------------------
  # Reading the live inputs back into the spec.
  #
  # Rather than syncing on every keystroke, the whole form is collected on
  # demand -- before any structural change, preview, run or save. Inputs that
  # have never been rendered fall back to the stored value, so collecting from
  # a tab the user has not opened is harmless.
  # -----------------------------------------------------------------------
  collect <- function() {
    s <- rv$spec

    s$name <- gv("pipe_name", s$name)
    s$description <- gv("pipe_desc", s$description)

    # --- sources
    if (length(s$sources)) {
      s$sources <- lapply(seq_along(s$sources), function(i) {
        src <- s$sources[[i]]
        src$label <- gv(paste0("src_label_", i), src$label)
        src$role <- gv(paste0("src_role_", i), src$role)
        src$format <- gv(paste0("src_format_", i), src$format)
        src$path_mode <- gv(paste0("src_mode_", i), src$path_mode)
        src$path <- gv(paste0("src_path_", i), src$path)
        src$dir <- gv(paste0("src_dir_", i), src$dir)
        src$pattern <- gv(paste0("src_pattern_", i), src$pattern)
        src$pick <- gv(paste0("src_pick_", i), src$pick)
        o <- src$options %||% list()
        o$header <- as_bool(gv(paste0("src_header_", i), o$header %||% TRUE), TRUE)
        o$skip <- as_int(gv(paste0("src_skip_", i), o$skip %||% 0), 0)
        o$sheet <- gv(paste0("src_sheet_", i), o$sheet %||% 1)
        o$delimiter <- gv(paste0("src_delim_", i), o$delimiter %||% ",")
        src$options <- o
        src
      })
    }

    # --- linking-field transforms
    if (length(s$transforms)) {
      s$transforms <- lapply(seq_along(s$transforms), function(i) {
        t <- s$transforms[[i]]
        t$source_id <- gv(paste0("tf_src_", i), t$source_id)
        t$input_field <- gv(paste0("tf_in_", i), t$input_field)
        t$output_field <- gv(paste0("tf_out_", i), t$output_field)
        if (length(t$steps)) {
          t$steps <- lapply(seq_along(t$steps), function(k) {
            st <- t$steps[[k]]
            op <- gv(paste0("tf_", i, "_op_", k), st$op)
            pars <- st$params %||% list()
            if (!identical(op, scalar(st$op, ""))) pars <- tf_op_defaults(op)
            spec_p <- TF_OPS[[op]]$params %||% list()
            for (nm in names(spec_p)) {
              val <- gv(paste0("tf_", i, "_p_", k, "_", nm), pars[[nm]])
              pars[[nm]] <- val
            }
            list(op = op, params = pars)
          })
        }
        t
      })
    }

    # --- joins
    s$joins$base_source_id <- gv("join_base", s$joins$base_source_id)
    if (length(s$joins$steps)) {
      s$joins$steps <- lapply(seq_along(s$joins$steps), function(i) {
        j <- s$joins$steps[[i]]
        j$right_source_id <- gv(paste0("j_right_", i), j$right_source_id)
        j$type <- gv(paste0("j_type_", i), j$type)
        j$select <- as.list(gv(paste0("j_select_", i), j$select) %||% list())
        j$prefix <- gv(paste0("j_prefix_", i), j$prefix)
        j$suffix <- gv(paste0("j_suffix_", i), j$suffix)
        j$conflict <- gv(paste0("j_conflict_", i), j$conflict)
        j$multi_match <- gv(paste0("j_multi_", i), j$multi_match)
        j$ignore_case <- as_bool(gv(paste0("j_icase_", i), j$ignore_case))
        j$match_blanks <- as_bool(gv(paste0("j_blanks_", i), j$match_blanks))
        if (length(j$keys)) {
          j$keys <- lapply(seq_along(j$keys), function(k) {
            list(left  = gv(paste0("j_", i, "_kl_", k), scalar(j$keys[[k]]$left, "")),
                 right = gv(paste0("j_", i, "_kr_", k), scalar(j$keys[[k]]$right, "")))
          })
        }
        j
      })
    }

    # --- final fields
    if (length(s$final$fields)) {
      s$final$fields <- lapply(seq_along(s$final$fields), function(i) {
        f <- s$final$fields[[i]]
        f$include <- as_bool(gv(paste0("fin_inc_", i), f$include), TRUE)
        f$output <- gv(paste0("fin_out_", i), f$output)
        f$order <- i
        if (length(f$steps)) {
          f$steps <- lapply(seq_along(f$steps), function(k) {
            st <- f$steps[[k]]
            op <- gv(paste0("fin_", i, "_op_", k), st$op)
            pars <- st$params %||% list()
            if (!identical(op, scalar(st$op, ""))) pars <- tf_op_defaults(op)
            spec_p <- TF_OPS[[op]]$params %||% list()
            for (nm in names(spec_p)) {
              pars[[nm]] <- gv(paste0("fin_", i, "_p_", k, "_", nm), pars[[nm]])
            }
            list(op = op, params = pars)
          })
        }
        f
      })
    }

    # --- filter / dedupe / sort
    s$final$filter$mode <- gv("filt_mode", s$final$filter$mode %||% "all")
    if (length(s$final$filter$conditions)) {
      s$final$filter$conditions <- lapply(seq_along(s$final$filter$conditions), function(i) {
        c0 <- s$final$filter$conditions[[i]]
        list(field = gv(paste0("filt_f_", i), scalar(c0$field, "")),
             op = gv(paste0("filt_op_", i), scalar(c0$op, "equals")),
             value = gv(paste0("filt_v_", i), scalar(c0$value, "")),
             ignore_case = as_bool(gv(paste0("filt_ic_", i), c0$ignore_case), TRUE))
      })
    }
    s$final$dedupe$enabled <- as_bool(gv("dd_on", s$final$dedupe$enabled))
    s$final$dedupe$keys <- as.list(gv("dd_keys", s$final$dedupe$keys) %||% list())
    s$final$dedupe$keep <- gv("dd_keep", s$final$dedupe$keep %||% "first")
    if (length(s$final$sort)) {
      s$final$sort <- lapply(seq_along(s$final$sort), function(i) {
        so <- s$final$sort[[i]]
        list(field = gv(paste0("sort_f_", i), scalar(so$field, "")),
             dir = gv(paste0("sort_d_", i), scalar(so$dir, "asc")),
             numeric = as_bool(gv(paste0("sort_n_", i), so$numeric)))
      })
    }

    # --- export
    s$export$path <- gv("exp_path", s$export$path)
    s$export$format <- gv("exp_format", s$export$format)
    s$export$timestamp_filename <- as_bool(gv("exp_stamp", s$export$timestamp_filename))
    o <- s$export$options %||% list()
    o$header <- as_bool(gv("exp_header", o$header %||% TRUE), TRUE)
    o$na_string <- gv("exp_na", o$na_string %||% "")
    o$delimiter <- gv("exp_delim", o$delimiter %||% ",")
    o$eol <- gv("exp_eol", o$eol %||% "lf")
    s$export$options <- o

    rv$spec <- s
    s
  }

  # -----------------------------------------------------------------------
  # Reading source data for previews and field lists.
  # -----------------------------------------------------------------------
  sources_signature <- function(s) {
    paste(vapply(s$sources, function(x) paste(scalar(x$id, ""), scalar(x$path_mode, ""),
      scalar(x$path, ""), scalar(x$dir, ""), scalar(x$pattern, ""), scalar(x$pick, ""),
      scalar(x$format, ""), scalar(x$options$sheet, ""), scalar(x$options$delimiter, ""),
      scalar(x$options$header, ""), scalar(x$options$skip, ""), sep = "|"), character(1)),
      collapse = "//")
  }

  # Reads (and caches) the raw sources. Returns NULL and shows a message if a
  # file cannot be read, rather than letting the error escape into the UI.
  get_tables <- function(force = FALSE, notify = TRUE) {
    s <- rv$spec
    if (!length(s$sources)) return(list())
    sig <- sources_signature(s)
    if (!force && identical(sig, rv$tables_key) && !is.null(rv$tables)) return(rv$tables)
    out <- list()
    for (src in s$sources) {
      df <- tryCatch(dp_read_source(src, rv$root), error = function(e) {
        if (notify) showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      })
      if (is.null(df)) return(NULL)
      if (nrow(df) > PREVIEW_ROWS) df <- df[seq_len(PREVIEW_ROWS), , drop = FALSE]
      out[[scalar(src$id)]] <- df
    }
    rv$tables <- out
    rv$tables_key <- sig
    out
  }

  # Columns of a source *after* its linking-field transformations, which is
  # what the join step actually gets to match on.
  transformed_cols <- function(sid) {
    tb <- get_tables(notify = FALSE)
    if (is.null(tb) || is.null(tb[[sid]])) return(character(0))
    cols <- names(tb[[sid]])
    for (t in rv$spec$transforms) {
      if (identical(scalar(t$source_id, ""), sid)) {
        of <- scalar(t$output_field, "")
        if (nzchar(of)) cols <- unique(c(cols, of))
      }
    }
    cols
  }

  # Columns of the running table just before join step `upto` (1-based).
  cols_before_join <- function(upto) {
    s <- rv$spec
    base <- scalar(s$joins$base_source_id, "")
    if (!nzchar(base)) return(character(0))
    cols <- transformed_cols(base)
    n <- min(length(s$joins$steps), max(0, upto - 1))
    if (n > 0) {
      probe <- s
      probe$joins$steps <- s$joins$steps[seq_len(n)]
      probe$final$fields <- list()
      probe$final$filter <- list(mode = "all", conditions = list())
      probe$final$sort <- list()
      probe$final$dedupe <- list(enabled = FALSE)
      j <- tryCatch(dp_execute(probe, rv$root, stop_after = "joins",
                               preview_rows = 50L)$data,
                    error = function(e) NULL)
      if (!is.null(j)) cols <- names(j)
    }
    cols
  }

  # The full combined table, used by the final step.
  joined_preview <- reactive({
    rv$version
    s <- rv$spec
    if (!nzchar(scalar(s$joins$base_source_id, ""))) return(NULL)
    probe <- s
    probe$final$fields <- list()
    probe$final$filter <- list(mode = "all", conditions = list())
    probe$final$sort <- list()
    probe$final$dedupe <- list(enabled = FALSE)
    tryCatch(dp_execute(probe, rv$root, stop_after = "joins",
                        preview_rows = PREVIEW_ROWS)$data,
             error = function(e) NULL)
  })

  # Names of the columns the export will actually have. Sorting and
  # de-duplicating happen after renaming, so they work on these, not on the
  # combined table's column names.
  output_cols <- function() {
    f <- rv$spec$final$fields %||% list()
    if (!length(f)) return(character(0))
    inc <- Filter(function(x) as_bool(x$include, TRUE), f)
    nm <- vapply(inc, function(x) scalar(x$output, ""), character(1))
    nm[nzchar(nm)]
  }

  # A select whose stored value is not among its choices would silently fall
  # back to the first one -- and the next collect() would write that wrong
  # value into the pipeline. Keeping the stored value in the list, flagged,
  # means a pipeline is never quietly rewritten just by opening a tab.
  with_current <- function(choices, current) {
    cur <- as_chr_vec(current)
    cur <- cur[nzchar(cur) & !is.na(cur)]
    miss <- setdiff(cur, as.character(choices))
    if (length(miss)) {
      choices <- c(as.list(choices),
                   stats::setNames(as.list(miss), paste0(miss, "  (not in the table)")))
    }
    choices
  }

  source_choices <- function() {
    s <- rv$spec
    if (!length(s$sources)) return(character(0))
    stats::setNames(vapply(s$sources, function(x) scalar(x$id, ""), character(1)),
                    vapply(s$sources, function(x) scalar(x$label, x$id), character(1)))
  }

  # =======================================================================
  # Step 1 -- Start
  # =======================================================================
  output$ui_start <- renderUI({
    rv$version
    saved <- dp_list_pipelines(PIPELINE_DIR)
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tr("start.existing")),
        card_body(
          card_help(tr("start.existing.help")),
          if (!nrow(saved)) div(class = "text-muted", tr("start.none")) else tagList(
            selectInput("load_which", NULL,
                        choices = stats::setNames(saved$path, paste0(saved$name, "  (", saved$file, ")")),
                        width = "100%"),
            div(class = "d-flex gap-2",
                actionButton("do_load", tr("start.load.button"), class = "btn-primary"),
                actionButton("do_load_run", tr("start.run.button"), class = "btn-success")),
            uiOutput("load_desc")
          ),
          hr(),
          fileInput("upload_pipeline", tr("start.upload"), accept = ".json", width = "100%")
        )
      ),
      card(
        card_header(tr("start.new")),
        card_body(
          card_help(tr("start.new.help")),
          textInput("pipe_name", tr("start.name"), value = rv$spec$name, width = "100%"),
          textAreaInput("pipe_desc", tr("start.description"), value = rv$spec$description,
                        width = "100%", height = "80px"),
          div(class = "input-group mb-2",
              tags$span(class = "input-group-text", tr("start.root")),
              tags$input(type = "text", class = "form-control", id = "root_display",
                         value = rv$root, readonly = NA)),
          card_help(tr("start.root.help")),
          div(class = "d-flex gap-2",
              actionButton("pick_root", "Change folder", class = "btn-outline-secondary btn-sm"),
              actionButton("do_new", tr("start.new.button"), class = "btn-primary")),
          hr(),
          h6("Current pipeline"),
          verbatimTextOutput("spec_outline")
        )
      )
    )
  })

  output$load_desc <- renderUI({
    req(input$load_which)
    s <- tryCatch(dp_load_pipeline(input$load_which), error = function(e) NULL)
    if (is.null(s)) return(div(class = "text-danger small", "Could not read this file."))
    div(class = "small text-muted mt-2",
        tags$strong(scalar(s$name, "")), tags$br(), scalar(s$description, ""), tags$br(),
        sprintf("%d input file(s), %d link(s), %d output column(s)",
                length(s$sources %||% list()), length(s$joins$steps %||% list()),
                length(Filter(function(f) as_bool(f$include, TRUE), s$final$fields %||% list()))))
  })

  output$spec_outline <- renderText(paste(dp_describe(rv$spec), collapse = "\n"))

  load_spec <- function(path) {
    s <- tryCatch(dp_load_pipeline(path), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8); NULL })
    if (is.null(s)) return(FALSE)
    rv$spec <- s
    # Relative paths in a saved pipeline resolve against the project folder,
    # which is the parent of pipelines/.
    rv$root <- normalizePath(dirname(dirname(path)), mustWork = FALSE)
    rv$tables <- NULL; rv$tables_key <- NULL; rv$result <- NULL
    bump()
    showNotification(paste0("Loaded '", scalar(s$name, ""), "'"), type = "message")
    TRUE
  }

  observeEvent(input$do_load, { if (load_spec(input$load_which)) updateNavbarPage(session, "nav", "sources") })
  observeEvent(input$do_load_run, { if (load_spec(input$load_which)) { updateNavbarPage(session, "nav", "export"); run_pipeline() } })
  observeEvent(input$upload_pipeline, {
    req(input$upload_pipeline)
    dest <- file.path(PIPELINE_DIR, input$upload_pipeline$name)
    file.copy(input$upload_pipeline$datapath, dest, overwrite = TRUE)
    load_spec(dest)
  })
  observeEvent(input$do_new, {
    rv$spec <- dp_new_pipeline(gv("pipe_name", "New pipeline"))
    rv$spec$description <- gv("pipe_desc", "")
    rv$tables <- NULL; rv$tables_key <- NULL; rv$result <- NULL
    bump()
    updateNavbarPage(session, "nav", "sources")
  })

  # =======================================================================
  # A small file/folder browser, so the app can point at real folders rather
  # than only at uploads. This is what makes "re-run out of these same
  # folders" work.
  # =======================================================================
  open_browser <- function(target, mode = "file", start = NULL) {
    rv$browse_target <- target
    rv$browse_mode <- mode
    rv$browse_dir <- start %||% rv$root
    showModal(modalDialog(
      title = if (mode == "dir") "Choose a folder" else "Choose a file",
      size = "l", easyClose = TRUE,
      textInput("browse_path", "Folder", value = rv$browse_dir, width = "100%"),
      div(class = "d-flex gap-2 mb-2",
          actionButton("browse_go", "Go", class = "btn-sm btn-outline-secondary"),
          actionButton("browse_up", "Up one level", class = "btn-sm btn-outline-secondary"),
          actionButton("browse_home", "Project folder", class = "btn-sm btn-outline-secondary")),
      uiOutput("browse_list"),
      footer = tagList(
        modalButton(tr("common.close")),
        if (mode == "dir") actionButton("browse_use_dir", "Use this folder", class = "btn-primary")
      )
    ))
  }

  # The listing uses one fixed select box rather than a link per entry: fixed
  # input ids mean no per-render observers to leak, and no chance of two
  # handlers firing for the same click.
  output$browse_list <- renderUI({
    d <- rv$browse_dir
    if (!dir.exists(d)) return(div(class = "text-danger", "Folder not found."))
    entries <- list.files(d, full.names = TRUE, all.files = FALSE)
    isdir <- dir.exists(entries)
    dirs <- sort(basename(entries[isdir]))
    files <- entries[!isdir]
    if (rv$browse_mode == "file") {
      files <- files[tolower(tools::file_ext(files)) %in%
                       c("csv", "tsv", "txt", "xlsx", "xls", "xlsm", "psv", "tab", "dat")]
    }
    files <- sort(basename(files))
    choices <- c(stats::setNames(paste0("d:", dirs), paste0("[folder]  ", dirs)),
                 stats::setNames(paste0("f:", files), files))
    tagList(
      div(class = "small text-muted mb-1", d),
      if (!length(choices)) div(class = "text-muted small mb-2", "Nothing here.")
      else selectInput("browse_pick", NULL, choices = choices, selectize = FALSE,
                       size = 12, width = "100%"),
      div(class = "d-flex gap-2",
        actionButton("browse_open", "Open folder", class = "btn-sm btn-outline-secondary"),
        if (rv$browse_mode == "file")
          actionButton("browse_use_file", "Use this file", class = "btn-sm btn-primary"))
    )
  })

  observeEvent(input$browse_open, {
    p <- input$browse_pick
    req(p)
    if (!startsWith(p, "d:")) {
      showNotification("That is a file, not a folder.", type = "warning"); return()
    }
    rv$browse_dir <- file.path(rv$browse_dir, substring(p, 3))
    updateTextInput(session, "browse_path", value = rv$browse_dir)
  })

  observeEvent(input$browse_use_file, {
    p <- input$browse_pick
    req(p)
    if (!startsWith(p, "f:")) {
      showNotification("That is a folder. Use 'Open folder', or pick a file.", type = "warning"); return()
    }
    apply_browse_choice(file.path(rv$browse_dir, substring(p, 3)))
  })

  apply_browse_choice <- function(path) {
    tgt <- rv$browse_target
    if (is.null(tgt)) return(invisible(NULL))
    collect()
    if (identical(tgt, "root")) {
      rv$root <- path
    } else if (grepl("^srcfile_", tgt)) {
      i <- as.integer(sub("^srcfile_", "", tgt))
      rv$spec$sources[[i]]$path <- relativise_path(path, rv$root)
      rv$spec$sources[[i]]$format <- dp_detect_format(path)
      rv$spec$sources[[i]]$path_mode <- "file"
    } else if (grepl("^srcdir_", tgt)) {
      i <- as.integer(sub("^srcdir_", "", tgt))
      rv$spec$sources[[i]]$dir <- relativise_path(path, rv$root)
      rv$spec$sources[[i]]$path_mode <- "pattern"
    } else if (identical(tgt, "exportdir")) {
      rv$spec$export$path <- file.path(relativise_path(path, rv$root),
                                       basename(scalar(rv$spec$export$path, "result.csv")))
    }
    rv$tables <- NULL; rv$tables_key <- NULL
    removeModal(); bump()
  }

  observeEvent(input$browse_go, {
    if (dir.exists(input$browse_path)) rv$browse_dir <- input$browse_path
    else showNotification("No such folder.", type = "warning")
  })
  observeEvent(input$browse_up, {
    rv$browse_dir <- dirname(rv$browse_dir)
    updateTextInput(session, "browse_path", value = rv$browse_dir)
  })
  observeEvent(input$browse_home, {
    rv$browse_dir <- rv$root
    updateTextInput(session, "browse_path", value = rv$browse_dir)
  })
  observeEvent(input$browse_use_dir, apply_browse_choice(rv$browse_dir))
  observeEvent(input$pick_root, open_browser("root", "dir", rv$root))

  # =======================================================================
  # Step 2 -- Import files
  # =======================================================================
  output$ui_sources <- renderUI({
    rv$version
    s <- rv$spec
    tagList(
      card(card_body(
        h5(tr("src.heading")), card_help(tr("src.help")),
        div(class = "d-flex gap-2 flex-wrap",
            actionButton("src_add_path", tr("src.add.path"), class = "btn-primary btn-sm"),
            div(style = "max-width:22rem;",
                fileInput("src_upload", NULL, multiple = TRUE, width = "100%",
                          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".xlsm"),
                          buttonLabel = tr("src.add.upload"))))
      )),
      if (!length(s$sources)) card(card_body(div(class = "text-muted", tr("src.none"))))
      else lapply(seq_along(s$sources), function(i) source_card(s$sources[[i]], i))
    )
  })

  source_card <- function(src, i) {
    is_excel <- identical(scalar(src$format, ""), "excel")
    is_delim <- scalar(src$format, "") %in% c("csv", "tsv", "delimited")
    mode <- scalar(src$path_mode, "file")
    resolved <- tryCatch(dp_resolve_source_files(src, rv$root), error = function(e) NULL)
    div(class = "dp-item dp-narrow",
      div(class = "d-flex justify-content-between align-items-start",
        h6(paste0(i, ". ", scalar(src$label, src$id))),
        actionButton(paste0("src_del_", i), tr("src.remove"), class = "btn-outline-danger btn-sm")),
      layout_columns(col_widths = c(4, 3, 5),
        textInput(paste0("src_label_", i), tr("src.label"), scalar(src$label, ""), width = "100%"),
        selectInput(paste0("src_role_", i), tr("src.role"),
                    choices = stats::setNames(c("data", "mapping"), c(tr("src.role.data"), tr("src.role.mapping"))),
                    selected = scalar(src$role, "data"), width = "100%"),
        selectInput(paste0("src_format_", i), tr("src.format"),
                    choices = DP_FORMATS, selected = scalar(src$format, "csv"), width = "100%")
      ),
      radioButtons(paste0("src_mode_", i), tr("src.mode"), inline = TRUE,
                   choices = stats::setNames(c("file", "pattern"), c(tr("src.mode.file"), tr("src.mode.pattern"))),
                   selected = mode),
      if (mode == "file") div(class = "input-group input-group-sm mb-2",
        tags$span(class = "input-group-text", tr("src.file")),
        tags$input(type = "text", class = "form-control", id = paste0("src_path_", i),
                   value = scalar(src$path, "")),
        actionButton(paste0("src_browse_", i), "Browse", class = "btn-outline-secondary")
      ) else tagList(
        div(class = "input-group input-group-sm mb-2",
          tags$span(class = "input-group-text", tr("src.dir")),
          tags$input(type = "text", class = "form-control", id = paste0("src_dir_", i),
                     value = scalar(src$dir, "")),
          actionButton(paste0("src_browsedir_", i), "Browse", class = "btn-outline-secondary")),
        layout_columns(col_widths = c(6, 6),
          textInput(paste0("src_pattern_", i), tr("src.pattern"), scalar(src$pattern, "*.csv"),
                    width = "100%", placeholder = "sales_*.csv"),
          selectInput(paste0("src_pick_", i), tr("src.pick"),
                      choices = stats::setNames(c("latest", "first", "all"),
                                                c(tr("src.pick.latest"), tr("src.pick.first"), tr("src.pick.all"))),
                      selected = scalar(src$pick, "latest"), width = "100%")),
        card_help(tr("src.pattern.help"))
      ),
      layout_columns(col_widths = c(3, 3, 3, 3),
        if (is_excel) selectInput(paste0("src_sheet_", i), tr("src.sheet"),
                                  choices = { sh <- if (!is.null(resolved)) dp_excel_sheets(resolved[1]) else character(0)
                                              if (length(sh)) sh else "1" },
                                  selected = scalar(src$options$sheet, 1), width = "100%"),
        if (is_delim) textInput(paste0("src_delim_", i), tr("src.delimiter"),
                                scalar(src$options$delimiter, ","), width = "100%"),
        numericInput(paste0("src_skip_", i), tr("src.skip"),
                     as_int(src$options$skip, 0), min = 0, width = "100%"),
        div(class = "mt-4", checkboxInput(paste0("src_header_", i), tr("src.header"),
                                          as_bool(src$options$header, TRUE)))
      ),
      div(class = "small mb-2",
        if (is.null(resolved)) span(class = "text-danger", "File not found -- check the path.")
        else span(class = "text-success", paste0("Resolved: ", paste(basename(resolved), collapse = ", ")))),
      actionButton(paste0("src_prev_", i), tr("src.preview"), class = "btn-outline-secondary btn-sm"),
      uiOutput(paste0("src_prevout_", i))
    )
  }

  observeEvent(input$src_add_path, {
    collect()
    n <- length(rv$spec$sources) + 1
    rv$spec$sources[[n]] <- dp_new_source(paste0("Table ", n), "", "data", "csv",
                                          list(header = TRUE, delimiter = ",", skip = 0))
    bump()
  })

  observeEvent(input$src_upload, {
    req(input$src_upload)
    collect()
    updir <- file.path(rv$root, "uploads")
    dir.create(updir, showWarnings = FALSE, recursive = TRUE)
    for (k in seq_len(nrow(input$src_upload))) {
      nm <- input$src_upload$name[k]
      dest <- file.path(updir, nm)
      # Copy out of Shiny's temp area so the saved pipeline can find it again.
      file.copy(input$src_upload$datapath[k], dest, overwrite = TRUE)
      n <- length(rv$spec$sources) + 1
      rv$spec$sources[[n]] <- dp_new_source(tools::file_path_sans_ext(nm),
                                            relativise_path(dest, rv$root), "data",
                                            dp_detect_format(nm),
                                            list(header = TRUE, delimiter = ",", skip = 0))
    }
    rv$tables <- NULL; rv$tables_key <- NULL
    bump()
  })

  # =======================================================================
  # Step 3 -- Linking fields
  # =======================================================================
  output$ui_keys <- renderUI({
    rv$version
    s <- rv$spec
    if (!length(s$sources)) return(card(card_body(div(class = "text-muted", tr("src.none")))))
    tagList(
      card(card_body(
        h5(tr("keys.heading")), card_help(tr("keys.help")),
        actionButton("tf_add", tr("keys.add"), class = "btn-primary btn-sm")
      )),
      if (!length(s$transforms)) card(card_body(div(class = "text-muted", tr("keys.none"))))
      else lapply(seq_along(s$transforms), function(i) transform_card(s$transforms[[i]], i))
    )
  })

  transform_card <- function(t, i) {
    sid <- scalar(t$source_id, "")
    tb <- get_tables(notify = FALSE)
    cols <- if (!is.null(tb) && !is.null(tb[[sid]])) names(tb[[sid]]) else character(0)
    div(class = "dp-item dp-narrow",
      div(class = "d-flex justify-content-between align-items-start",
        h6(paste0(tr("common.step"), " ", i)),
        actionButton(paste0("tf_del_", i), tr("common.remove"), class = "btn-outline-danger btn-sm")),
      layout_columns(col_widths = c(4, 4, 4),
        selectInput(paste0("tf_src_", i), tr("keys.source"), choices = source_choices(),
                    selected = sid, width = "100%"),
        selectInput(paste0("tf_in_", i), tr("keys.input"),
                    choices = with_current(cols, scalar(t$input_field, "")),
                    selected = scalar(t$input_field, ""), width = "100%"),
        textInput(paste0("tf_out_", i), tr("keys.output"), scalar(t$output_field, ""), width = "100%")),
      card_help(tr("keys.output.help")),
      tags$label(class = "form-label fw-semibold", tr("keys.steps")),
      if (!length(t$steps)) div(class = "text-muted small mb-2", "No steps yet.")
      else lapply(seq_along(t$steps), function(k) step_ui(t$steps[[k]], paste0("tf_", i), k, i)),
      div(class = "d-flex gap-2",
        actionButton(paste0("tf_addstep_", i), tr("keys.addstep"), class = "btn-outline-primary btn-sm"),
        actionButton(paste0("tf_prev_", i), tr("src.preview"), class = "btn-outline-secondary btn-sm")),
      uiOutput(paste0("tf_prevout_", i))
    )
  }

  # One transformation step: an operation picker plus its parameter controls.
  step_ui <- function(st, prefix, k, owner) {
    op <- scalar(st$op, "trim")
    spec_p <- TF_OPS[[op]]$params %||% list()
    pars <- st$params %||% list()
    div(class = "dp-step",
      div(class = "d-flex gap-2 align-items-end",
        div(style = "min-width:16rem;",
          selectInput(paste0(prefix, "_op_", k), paste0(k, "."), choices = tf_op_labels(),
                      selected = op, width = "100%")),
        div(class = "flex-grow-1",
          if (length(spec_p)) div(class = "row g-2",
            lapply(names(spec_p), function(nm) {
              ps <- spec_p[[nm]]
              val <- pars[[nm]] %||% ps$default
              id <- paste0(prefix, "_p_", k, "_", nm)
              div(class = "col-auto", switch(ps$type,
                text = textInput(id, ps$label, value = scalar(val, ""), width = "12rem",
                                 placeholder = ps$placeholder %||% ""),
                number = numericInput(id, ps$label, value = as_num(val, 0), width = "8rem"),
                choice = selectInput(id, ps$label, choices = ps$choices,
                                     selected = scalar(val, ps$default), width = "12rem"),
                logical = div(class = "mt-4", checkboxInput(id, ps$label, as_bool(val, FALSE))),
                NULL))
            })
          ) else div(class = "text-muted small", TF_OPS[[op]]$desc)),
        actionButton(paste0(prefix, "_delstep_", k), "x", class = "btn-outline-danger btn-sm mb-3")
      ),
      if (length(spec_p)) div(class = "text-muted small", TF_OPS[[op]]$desc)
    )
  }

  observeEvent(input$tf_add, {
    collect()
    s <- rv$spec
    sid <- scalar(s$sources[[1]]$id, "")
    tb <- get_tables(notify = FALSE)
    first_col <- if (!is.null(tb) && !is.null(tb[[sid]]) && ncol(tb[[sid]])) names(tb[[sid]])[1] else ""
    n <- length(s$transforms) + 1
    rv$spec$transforms[[n]] <- list(id = new_id("tf"), source_id = sid,
                                    input_field = first_col, output_field = paste0(first_col, "Key"),
                                    steps = list(list(op = "trim", params = tf_op_defaults("trim"))))
    bump()
  })

  # =======================================================================
  # Step 4 -- Combine tables
  # =======================================================================
  output$ui_join <- renderUI({
    rv$version
    s <- rv$spec
    if (!length(s$sources)) return(card(card_body(div(class = "text-muted", tr("src.none")))))
    tagList(
      card(card_body(
        h5(tr("join.heading")), card_help(tr("join.help")),
        layout_columns(col_widths = c(6, 6),
          selectInput("join_base", tr("join.base"), choices = source_choices(),
                      selected = scalar(s$joins$base_source_id, ""), width = "100%"),
          div(class = "mt-4", actionButton("join_add", tr("join.add"), class = "btn-primary btn-sm"))),
        card_help(tr("join.base.help"))
      )),
      if (!length(s$joins$steps)) card(card_body(div(class = "text-muted", tr("join.none"))))
      else lapply(seq_along(s$joins$steps), function(i) join_card(s$joins$steps[[i]], i))
    )
  })

  join_card <- function(j, i) {
    rid <- scalar(j$right_source_id, "")
    lcols <- cols_before_join(i)
    rcols <- transformed_cols(rid)
    keys <- j$keys %||% list(list(left = "", right = ""))
    div(class = "dp-item dp-narrow",
      div(class = "d-flex justify-content-between align-items-start",
        h6(paste0(tr("common.step"), " ", i)),
        actionButton(paste0("j_del_", i), tr("common.remove"), class = "btn-outline-danger btn-sm")),
      layout_columns(col_widths = c(5, 7),
        selectInput(paste0("j_right_", i), tr("join.right"),
                    choices = source_choices(), selected = rid, width = "100%"),
        selectInput(paste0("j_type_", i), tr("join.type"),
                    choices = stats::setNames(
                      c("left", "inner", "full", "right", "semi", "anti"),
                      c(tr("join.type.left"), tr("join.type.inner"), tr("join.type.full"),
                        tr("join.type.right"), tr("join.type.semi"), tr("join.type.anti"))),
                    selected = scalar(j$type, "left"), width = "100%")),
      tags$label(class = "form-label fw-semibold", tr("join.keys")),
      lapply(seq_along(keys), function(k) {
        div(class = "row g-2 align-items-end mb-1",
          div(class = "col", selectInput(paste0("j_", i, "_kl_", k), if (k == 1) tr("join.left.field") else NULL,
                                         choices = with_current(lcols, scalar(keys[[k]]$left, "")),
                                         selected = scalar(keys[[k]]$left, ""), width = "100%")),
          div(class = "col-auto pb-2", "="),
          div(class = "col", selectInput(paste0("j_", i, "_kr_", k), if (k == 1) tr("join.right.field") else NULL,
                                         choices = with_current(rcols, scalar(keys[[k]]$right, "")),
                                         selected = scalar(keys[[k]]$right, ""), width = "100%")),
          div(class = "col-auto",
              actionButton(paste0("j_", i, "_delkey_", k), "x", class = "btn-outline-danger btn-sm mb-2")))
      }),
      actionButton(paste0("j_addkey_", i), tr("join.addkey"), class = "btn-outline-primary btn-sm mb-3"),
      selectInput(paste0("j_select_", i), tr("join.select"),
                  choices = with_current(rcols, j$select),
                  selected = as_chr_vec(j$select), multiple = TRUE, width = "100%"),
      card_help(tr("join.select.help")),
      layout_columns(col_widths = c(3, 3, 6),
        textInput(paste0("j_prefix_", i), tr("join.prefix"), scalar(j$prefix, ""), width = "100%"),
        textInput(paste0("j_suffix_", i), tr("join.suffix"), scalar(j$suffix, ""), width = "100%"),
        selectInput(paste0("j_conflict_", i), tr("join.conflict"),
                    choices = stats::setNames(c("suffix", "right_wins", "coalesce", "skip"),
                      c(tr("join.conflict.suffix"), tr("join.conflict.right_wins"),
                        tr("join.conflict.coalesce"), tr("join.conflict.skip"))),
                    selected = scalar(j$conflict, "suffix"), width = "100%")),
      layout_columns(col_widths = c(6, 3, 3),
        selectInput(paste0("j_multi_", i), tr("join.multi"),
                    choices = stats::setNames(c("all", "first", "error"),
                      c(tr("join.multi.all"), tr("join.multi.first"), tr("join.multi.error"))),
                    selected = scalar(j$multi_match, "all"), width = "100%"),
        checkboxInput(paste0("j_icase_", i), tr("join.ignorecase"), as_bool(j$ignore_case)),
        checkboxInput(paste0("j_blanks_", i), tr("join.matchblanks"), as_bool(j$match_blanks))),
      actionButton(paste0("j_check_", i), tr("join.check"), class = "btn-outline-secondary btn-sm"),
      uiOutput(paste0("j_checkout_", i))
    )
  }

  observeEvent(input$join_add, {
    collect()
    s <- rv$spec
    ids <- vapply(s$sources, function(x) scalar(x$id, ""), character(1))
    used <- c(scalar(s$joins$base_source_id, ""),
              vapply(s$joins$steps, function(x) scalar(x$right_source_id, ""), character(1)))
    cand <- setdiff(ids, used)
    rid <- if (length(cand)) cand[1] else ids[length(ids)]
    n <- length(s$joins$steps) + 1
    rv$spec$joins$steps[[n]] <- list(id = new_id("j"), right_source_id = rid, type = "left",
                                     keys = list(list(left = "", right = "")), select = list(),
                                     prefix = "", suffix = "", conflict = "suffix",
                                     multi_match = "all", ignore_case = FALSE, match_blanks = FALSE)
    bump()
  })

  # =======================================================================
  # Step 5 -- Final table
  # =======================================================================
  output$ui_final <- renderUI({
    rv$version
    s <- rv$spec
    j <- joined_preview()
    if (is.null(j)) return(card(card_body(div(class = "text-muted", tr("final.none")))))
    cols <- names(j)
    ocols <- output_cols()
    tagList(
      card(card_body(
        h5(tr("final.heading")), card_help(tr("final.help")),
        div(class = "d-flex gap-2 flex-wrap",
          actionButton("fin_refresh", tr("final.refresh"), class = "btn-primary btn-sm"),
          actionButton("fin_all", tr("final.selectall"), class = "btn-outline-secondary btn-sm"),
          actionButton("fin_none", tr("final.selectnone"), class = "btn-outline-secondary btn-sm"))
      )),
      card(card_header("Columns"), card_body(class = "dp-narrow",
        if (!length(s$final$fields)) div(class = "text-muted", "Press 'Refresh from the combined table' to list the available columns.")
        else lapply(seq_along(s$final$fields), function(i) field_row(s$final$fields[[i]], i))
      )),
      card(card_header(tr("final.filter")), card_body(class = "dp-narrow",
        selectInput("filt_mode", tr("final.filter.mode"),
                    choices = stats::setNames(c("all", "any"), c(tr("final.filter.all"), tr("final.filter.any"))),
                    selected = scalar(s$final$filter$mode, "all"), width = "20rem"),
        card_help("Conditions are tested against the combined table, so you can filter on columns you do not export."),
        lapply(seq_along(s$final$filter$conditions %||% list()), function(i) {
          c0 <- s$final$filter$conditions[[i]]
          div(class = "row g-2 align-items-end mb-1",
            div(class = "col-3", selectInput(paste0("filt_f_", i), NULL,
                                             choices = with_current(cols, scalar(c0$field, "")),
                                             selected = scalar(c0$field, ""), width = "100%")),
            div(class = "col-3", selectInput(paste0("filt_op_", i), NULL,
              choices = c("equals", "not_equals", "contains", "not_contains", "starts_with",
                          "ends_with", "is_blank", "not_blank", "in_list", "not_in_list",
                          "regex", "gt", "gte", "lt", "lte"),
              selected = scalar(c0$op, "equals"), width = "100%")),
            div(class = "col-3", textInput(paste0("filt_v_", i), NULL, scalar(c0$value, ""), width = "100%")),
            div(class = "col-2", checkboxInput(paste0("filt_ic_", i), "Ignore case", as_bool(c0$ignore_case, TRUE))),
            div(class = "col-1", actionButton(paste0("filt_del_", i), "x", class = "btn-outline-danger btn-sm mb-2")))
        }),
        actionButton("filt_add", tr("final.filter.add"), class = "btn-outline-primary btn-sm")
      )),
      layout_columns(col_widths = c(6, 6),
        card(card_header(tr("final.dedupe")), card_body(class = "dp-narrow",
          checkboxInput("dd_on", tr("final.dedupe"), as_bool(s$final$dedupe$enabled)),
          selectInput("dd_keys", tr("final.dedupe.keys"),
                      choices = with_current(ocols, s$final$dedupe$keys),
                      selected = as_chr_vec(s$final$dedupe$keys), multiple = TRUE, width = "100%"),
          card_help("These are the exported column names, since duplicates are removed after renaming."),
          selectInput("dd_keep", tr("final.dedupe.keep"),
                      choices = stats::setNames(c("first", "last"), c(tr("final.dedupe.first"), tr("final.dedupe.last"))),
                      selected = scalar(s$final$dedupe$keep, "first"), width = "100%")
        )),
        card(card_header(tr("final.sort")), card_body(class = "dp-narrow",
          card_help("Sorting uses the exported column names."),
          lapply(seq_along(s$final$sort %||% list()), function(i) {
            so <- s$final$sort[[i]]
            div(class = "row g-2 align-items-end mb-1",
              div(class = "col-5", selectInput(paste0("sort_f_", i), NULL,
                                               choices = with_current(ocols, scalar(so$field, "")),
                                               selected = scalar(so$field, ""), width = "100%")),
              div(class = "col-3", selectInput(paste0("sort_d_", i), NULL,
                                               choices = c(asc = "asc", desc = "desc"),
                                               selected = scalar(so$dir, "asc"), width = "100%")),
              div(class = "col-3", checkboxInput(paste0("sort_n_", i), "Numeric", as_bool(so$numeric))),
              div(class = "col-1", actionButton(paste0("sort_del_", i), "x", class = "btn-outline-danger btn-sm mb-2")))
          }),
          actionButton("sort_add", tr("final.sort.add"), class = "btn-outline-primary btn-sm")
        ))
      ),
      card(card_header(tr("run.preview")), card_body(
        actionButton("fin_preview", "Preview the final table", class = "btn-outline-secondary btn-sm mb-2"),
        DTOutput("final_preview")))
    )
  }
  )

  field_row <- function(f, i) {
    op_steps <- f$steps %||% list()
    div(class = "dp-item",
      div(class = "row g-2 align-items-center",
        div(class = "col-auto", checkboxInput(paste0("fin_inc_", i), NULL, as_bool(f$include, TRUE))),
        div(class = "col-3", div(class = "small text-muted", tr("final.source")),
            div(class = "fw-semibold text-truncate", scalar(f$source, "(combined)"))),
        div(class = "col-3", textInput(paste0("fin_out_", i), NULL, scalar(f$output, ""), width = "100%")),
        div(class = "col-auto",
          actionButton(paste0("fin_up_", i), "^", class = "btn-outline-secondary btn-sm"),
          actionButton(paste0("fin_dn_", i), "v", class = "btn-outline-secondary btn-sm"),
          actionButton(paste0("fin_addstep_", i), "+ format", class = "btn-outline-primary btn-sm"),
          actionButton(paste0("fin_del_", i), "x", class = "btn-outline-danger btn-sm"))),
      if (length(op_steps)) div(class = "mt-2",
        lapply(seq_along(op_steps), function(k) step_ui(op_steps[[k]], paste0("fin_", i), k, i)))
    )
  }

  observeEvent(input$fin_refresh, {
    collect()
    j <- joined_preview()
    if (is.null(j)) { showNotification("Combine the tables first.", type = "warning"); return() }
    existing <- rv$spec$final$fields
    by_src <- list()
    for (f in existing) by_src[[scalar(f$source, "")]] <- f
    rv$spec$final$fields <- lapply(seq_along(names(j)), function(i) {
      nm <- names(j)[i]
      old <- by_src[[nm]]
      if (!is.null(old)) { old$order <- i; old }
      else list(source = nm, output = nm, include = TRUE, order = i, steps = list())
    })
    bump()
  })
  observeEvent(input$fin_all, {
    collect()
    rv$spec$final$fields <- lapply(rv$spec$final$fields, function(f) { f$include <- TRUE; f })
    bump()
  })
  observeEvent(input$fin_none, {
    collect()
    rv$spec$final$fields <- lapply(rv$spec$final$fields, function(f) { f$include <- FALSE; f })
    bump()
  })
  observeEvent(input$filt_add, {
    collect()
    n <- length(rv$spec$final$filter$conditions) + 1
    rv$spec$final$filter$conditions[[n]] <- list(field = "", op = "equals", value = "", ignore_case = TRUE)
    bump()
  })
  observeEvent(input$sort_add, {
    collect()
    n <- length(rv$spec$final$sort) + 1
    rv$spec$final$sort[[n]] <- list(field = "", dir = "asc", numeric = FALSE)
    bump()
  })
  observeEvent(input$fin_preview, {
    collect()
    withProgress(message = "Building preview", value = 0.5, {
      res <- tryCatch(dp_execute(rv$spec, rv$root, stop_after = "final", preview_rows = PREVIEW_ROWS),
                      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
      output$final_preview <- renderDT({
        req(res)
        datatable(res$data, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
                  rownames = FALSE, class = "compact stripe")
      })
    })
  })

  # =======================================================================
  # Step 6 -- Export & save
  # =======================================================================
  output$ui_export <- renderUI({
    rv$version
    s <- rv$spec
    probs <- dp_validate(s)
    tagList(
      card(card_body(
        h5(tr("export.heading")),
        if (length(probs)) div(class = "dp-problems mb-3",
          tags$strong(tr("common.problems")),
          tags$ul(lapply(probs, tags$li)))
        else div(class = "dp-ready mb-3", tr("common.ready")),
        layout_columns(col_widths = c(7, 5),
          div(class = "input-group input-group-sm mb-2",
            tags$span(class = "input-group-text", tr("export.file")),
            tags$input(type = "text", class = "form-control", id = "exp_path",
                       value = scalar(s$export$path, "")),
            actionButton("exp_browse", "Folder", class = "btn-outline-secondary")),
          selectInput("exp_format", tr("export.format"), choices = DP_FORMATS,
                      selected = scalar(s$export$format, "csv"), width = "100%")),
        layout_columns(col_widths = c(3, 3, 3, 3),
          textInput("exp_na", tr("export.na"), scalar(s$export$options$na_string, ""), width = "100%"),
          textInput("exp_delim", tr("src.delimiter"), scalar(s$export$options$delimiter, ","), width = "100%"),
          selectInput("exp_eol", tr("export.eol"), choices = c(lf = "lf", crlf = "crlf"),
                      selected = scalar(s$export$options$eol, "lf"), width = "100%"),
          div(checkboxInput("exp_header", tr("export.header"), as_bool(s$export$options$header, TRUE)),
              checkboxInput("exp_stamp", tr("export.timestamp"), as_bool(s$export$timestamp_filename)))),
        div(class = "d-flex gap-2 flex-wrap mt-2",
          actionButton("do_run", tr("export.run"), class = "btn-success"),
          downloadButton("do_download", tr("export.download"), class = "btn-outline-primary"))
      )),
      card(card_header(tr("export.save")), card_body(
        card_help(tr("export.save.help")),
        layout_columns(col_widths = c(8, 4),
          textInput("save_file", "File name", value = paste0(
            gsub("[^A-Za-z0-9_-]+", "_", tolower(scalar(s$name, "pipeline"))), ".json"), width = "100%"),
          div(class = "mt-4", actionButton("do_save", tr("export.save"), class = "btn-primary"))),
        uiOutput("cli_hint")
      )),
      card(card_header(tr("run.log")), card_body(
        uiOutput("run_summary"),
        verbatimTextOutput("run_log")
      )),
      card(card_header(tr("run.preview")), card_body(DTOutput("result_preview")))
    )
  })

  output$cli_hint <- renderUI({
    rv$version
    f <- gv("save_file", "pipeline.json")
    div(class = "mt-2",
      tags$label(class = "form-label small fw-semibold", tr("export.cli")),
      tags$pre(class = "dp-log", paste0(
        "cd ", app_dir, "\n",
        "Rscript run_pipeline.R pipelines/", f, "\n\n",
        "# same pipeline against a different month's folders:\n",
        "Rscript run_pipeline.R pipelines/", f, " --root /path/to/2026-09")))
  })

  run_pipeline <- function() {
    s <- collect()
    probs <- dp_validate(s)
    if (length(probs)) {
      showNotification(paste0(tr("common.problems"), ": ", probs[1]), type = "error", duration = 8)
      return(invisible(NULL))
    }
    withProgress(message = tr("run.reading"), value = 0, {
      res <- tryCatch(
        dp_execute(s, rv$root, progress = function(frac, msg) setProgress(value = frac, message = msg)),
        error = function(e) { showNotification(paste0(tr("run.failed"), ": ", conditionMessage(e)),
                                               type = "error", duration = NULL); NULL })
      rv$result <- res
    })
    if (!is.null(rv$result)) {
      showNotification(tr("run.success",
        rows = nrow(rv$result$data), cols = ncol(rv$result$data),
        path = rv$result$export_path, secs = sprintf("%.2f", rv$result$elapsed)),
        type = "message", duration = 10)
    }
    invisible(NULL)
  }

  observeEvent(input$do_run, run_pipeline())
  observeEvent(input$exp_browse, open_browser("exportdir", "dir", rv$root))

  output$run_summary <- renderUI({
    res <- rv$result
    if (is.null(res)) return(div(class = "text-muted", "Not run yet."))
    tagList(
      div(class = "dp-ready mb-2",
        tr("run.success", rows = nrow(res$data), cols = ncol(res$data),
           path = res$export_path %||% "(preview only)", secs = sprintf("%.2f", res$elapsed))),
      if (length(res$warnings)) div(class = "dp-problems mb-2",
        tags$strong(tr("run.warnings")), tags$ul(lapply(res$warnings, tags$li)))
    )
  })

  output$run_log <- renderText({
    res <- rv$result
    if (is.null(res)) return("")
    paste(unlist(res$log), collapse = "\n")
  })

  output$result_preview <- renderDT({
    res <- rv$result
    req(res)
    datatable(head(res$data, 500), options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
              rownames = FALSE, class = "compact stripe")
  })

  output$do_download <- downloadHandler(
    filename = function() basename(scalar(rv$spec$export$path, "result.csv")),
    content = function(file) {
      s <- collect()
      res <- rv$result
      if (is.null(res)) {
        res <- dp_execute(s, rv$root, stop_after = "final")
        rv$result <- res
      }
      dp_write_file(res$data, file, scalar(s$export$format, "csv"), s$export$options)
    }
  )

  observeEvent(input$do_save, {
    s <- collect()
    fname <- gv("save_file", "pipeline.json")
    if (!grepl("\\.json$", fname)) fname <- paste0(fname, ".json")
    path <- file.path(PIPELINE_DIR, basename(fname))
    ok <- tryCatch({ dp_save_pipeline(s, path); TRUE },
                   error = function(e) { showNotification(conditionMessage(e), type = "error"); FALSE })
    if (ok) showNotification(tr("export.saved", path = path), type = "message", duration = 8)
    bump()
  })

  # =======================================================================
  # Status pill and the dynamic observers
  # =======================================================================
  output$status_pill <- renderUI({
    rv$version
    probs <- dp_validate(rv$spec)
    if (length(probs)) span(class = "navbar-text small text-warning",
                            paste0(length(probs), " thing(s) to fix"))
    else span(class = "navbar-text small text-success", tr("common.ready"))
  })

  # Rebuild the per-item observers whenever the structure changes.
  observeEvent(rv$version, {
    clear_obs()
    s <- rv$spec

    # --- sources
    for (i in seq_along(s$sources)) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("src_del_", ii)]], {
        collect(); rv$spec$sources[[ii]] <- NULL
        rv$tables <- NULL; rv$tables_key <- NULL; bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("src_browse_", ii)]], {
        collect(); open_browser(paste0("srcfile_", ii), "file", rv$root)
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("src_browsedir_", ii)]], {
        collect(); open_browser(paste0("srcdir_", ii), "dir", rv$root)
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("src_format_", ii)]], {
        collect(); bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("src_mode_", ii)]], {
        collect(); bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("src_prev_", ii)]], {
        collect()
        tb <- get_tables(force = TRUE)
        df <- if (!is.null(tb)) tb[[scalar(rv$spec$sources[[ii]]$id)]] else NULL
        output[[paste0("src_prevout_", ii)]] <- renderUI({
          if (is.null(df)) return(div(class = "text-danger small mt-2", "Could not read this file."))
          tagList(div(class = "small text-muted mt-2",
                      paste0(nrow(df), " ", tr("common.rows"), ", ", ncol(df), " ", tr("common.columns"),
                             ". ", tr("common.preview.note", n = min(20, nrow(df))))),
                  renderTable(head(df, 20), striped = TRUE, spacing = "xs", width = "100%"))
        })
      }, ignoreInit = TRUE))
    })

    # --- transforms
    for (i in seq_along(s$transforms)) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("tf_del_", ii)]], {
        collect(); rv$spec$transforms[[ii]] <- NULL; bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("tf_addstep_", ii)]], {
        collect()
        n <- length(rv$spec$transforms[[ii]]$steps) + 1
        rv$spec$transforms[[ii]]$steps[[n]] <- list(op = "trim", params = tf_op_defaults("trim"))
        bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("tf_src_", ii)]], { collect(); bump() }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("tf_prev_", ii)]], {
        collect()
        t <- rv$spec$transforms[[ii]]
        tb <- get_tables()
        df <- if (!is.null(tb)) tb[[scalar(t$source_id, "")]] else NULL
        output[[paste0("tf_prevout_", ii)]] <- renderUI({
          if (is.null(df) || !(scalar(t$input_field, "") %in% names(df)))
            return(div(class = "text-danger small mt-2", "Pick a table and field first."))
          before <- head(as.character(df[[scalar(t$input_field, "")]]), 15)
          after <- tryCatch(tf_apply_steps(before, t$steps),
                            error = function(e) rep(paste("error:", conditionMessage(e)), length(before)))
          cmp <- data.frame(before, after, stringsAsFactors = FALSE, check.names = FALSE)
          names(cmp) <- c(tr("keys.preview.before"), tr("keys.preview.after"))
          renderTable(cmp, striped = TRUE, spacing = "xs", width = "100%", na = "")
        })
      }, ignoreInit = TRUE))
      for (k in seq_along(s$transforms[[i]]$steps)) local({
        kk <- k
        add_obs(observeEvent(input[[paste0("tf_", ii, "_delstep_", kk)]], {
          collect(); rv$spec$transforms[[ii]]$steps[[kk]] <- NULL; bump()
        }, ignoreInit = TRUE))
        add_obs(observeEvent(input[[paste0("tf_", ii, "_op_", kk)]], {
          collect(); bump()
        }, ignoreInit = TRUE))
      })
    })

    # --- joins
    for (i in seq_along(s$joins$steps)) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("j_del_", ii)]], {
        collect(); rv$spec$joins$steps[[ii]] <- NULL; bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("j_addkey_", ii)]], {
        collect()
        n <- length(rv$spec$joins$steps[[ii]]$keys) + 1
        rv$spec$joins$steps[[ii]]$keys[[n]] <- list(left = "", right = "")
        bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("j_right_", ii)]], { collect(); bump() }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("j_check_", ii)]], {
        collect()
        output[[paste0("j_checkout_", ii)]] <- renderUI({
          probe <- rv$spec
          probe$joins$steps <- rv$spec$joins$steps[seq_len(ii)]
          probe$final$fields <- list()
          probe$final$filter <- list(mode = "all", conditions = list())
          probe$final$sort <- list(); probe$final$dedupe <- list(enabled = FALSE)
          r <- tryCatch(dp_execute(probe, rv$root, stop_after = "joins", preview_rows = PREVIEW_ROWS),
                        error = function(e) e)
          if (inherits(r, "error")) return(div(class = "text-danger small mt-2", conditionMessage(r)))
          msgs <- grep(paste0("join onto"), unlist(r$log), ignore.case = TRUE, value = TRUE)
          tagList(
            div(class = "small mt-2", tags$strong("Match check:"),
                tags$ul(lapply(c(tail(msgs, 1), r$warnings), tags$li))),
            renderTable(head(r$data, 8), striped = TRUE, spacing = "xs", width = "100%", na = ""))
        })
      }, ignoreInit = TRUE))
      for (k in seq_along(s$joins$steps[[i]]$keys)) local({
        kk <- k
        add_obs(observeEvent(input[[paste0("j_", ii, "_delkey_", kk)]], {
          collect()
          if (length(rv$spec$joins$steps[[ii]]$keys) > 1) rv$spec$joins$steps[[ii]]$keys[[kk]] <- NULL
          bump()
        }, ignoreInit = TRUE))
      })
    })

    # --- final fields
    for (i in seq_along(s$final$fields)) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("fin_del_", ii)]], {
        collect(); rv$spec$final$fields[[ii]] <- NULL; bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("fin_up_", ii)]], {
        collect()
        if (ii > 1) {
          f <- rv$spec$final$fields
          f[c(ii - 1, ii)] <- f[c(ii, ii - 1)]
          rv$spec$final$fields <- f
        }
        bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("fin_dn_", ii)]], {
        collect()
        f <- rv$spec$final$fields
        if (ii < length(f)) {
          f[c(ii, ii + 1)] <- f[c(ii + 1, ii)]
          rv$spec$final$fields <- f
        }
        bump()
      }, ignoreInit = TRUE))
      add_obs(observeEvent(input[[paste0("fin_addstep_", ii)]], {
        collect()
        n <- length(rv$spec$final$fields[[ii]]$steps) + 1
        rv$spec$final$fields[[ii]]$steps[[n]] <- list(op = "trim", params = tf_op_defaults("trim"))
        bump()
      }, ignoreInit = TRUE))
      for (k in seq_along(s$final$fields[[i]]$steps)) local({
        kk <- k
        add_obs(observeEvent(input[[paste0("fin_", ii, "_delstep_", kk)]], {
          collect(); rv$spec$final$fields[[ii]]$steps[[kk]] <- NULL; bump()
        }, ignoreInit = TRUE))
        add_obs(observeEvent(input[[paste0("fin_", ii, "_op_", kk)]], {
          collect(); bump()
        }, ignoreInit = TRUE))
      })
    })

    # --- filter / sort rows
    for (i in seq_along(s$final$filter$conditions %||% list())) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("filt_del_", ii)]], {
        collect(); rv$spec$final$filter$conditions[[ii]] <- NULL; bump()
      }, ignoreInit = TRUE))
    })
    for (i in seq_along(s$final$sort %||% list())) local({
      ii <- i
      add_obs(observeEvent(input[[paste0("sort_del_", ii)]], {
        collect(); rv$spec$final$sort[[ii]] <- NULL; bump()
      }, ignoreInit = TRUE))
    })
  }, ignoreInit = FALSE)
}

# ===========================================================================
if (interactive()) {
  shinyApp(ui, server)
} else {
  port <- as.integer(Sys.getenv("DATAPIPE_PORT", "8080"))
  host <- Sys.getenv("DATAPIPE_HOST", "127.0.0.1")
  cat("Data Pipeline Builder -> http://", host, ":", port, "\n", sep = "")
  shiny::runApp(shinyApp(ui, server), port = port, host = host, launch.browser = FALSE)
}
