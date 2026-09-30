# =============================================================================
# NLP Scoring Dashboard — Shiny App
# Upload your pipeline's output CSV/XLSX and (optionally) a gold-standard CSV.
# Computes Precision (PPV), Recall (Sensitivity), F1, false positives/negatives,
# and hallucination rate (evidence_span verified verbatim against NOTE_TEXT).
# No gold standard needed for the Hallucination tab — it runs on the output
# file alone. The Accuracy tab needs a gold standard to compute P/R/F1.
# =============================================================================

library(shiny)
library(bslib)
library(readr)
library(readxl)
library(dplyr)
library(DT)

options(shiny.maxRequestSize = 500 * 1024^2)

# =============================================================================
# SHARED HELPERS  (same logic as nlp_scoring_harness.R / hallucination_checker.R)
# =============================================================================

read_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(ext,
    csv  = readr::read_csv(path, show_col_types = FALSE),
    xlsx = readxl::read_excel(path),
    xls  = readxl::read_excel(path),
    stop(paste("Unsupported file type:", ext))
  )
}

norm <- function(x) tolower(trimws(as.character(x)))

to_logical_positive <- function(x, positive_vals) {
  v <- norm(x)
  out <- v %in% norm(positive_vals)
  out[v %in% c("", "na", "null", "n/a") | is.na(x)] <- NA
  out
}

wilson_ci <- function(k, n, conf = 0.95) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- k / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(max(0, center - half), min(1, center + half))
}

fmt_ci <- function(p, lo, hi) {
  if (is.na(p)) return("NA")
  sprintf("%.3f  [%.3f\u2013%.3f]", p, lo, hi)
}

binary_metrics <- function(gold, pred) {
  keep <- !is.na(gold) & !is.na(pred)
  g <- gold[keep]; p <- pred[keep]
  TP <- sum(g & p); FP <- sum(!g & p); FN <- sum(g & !p); TN <- sum(!g & !p)
  N <- TP + FP + FN + TN
  metric <- function(k, n) { ci <- wilson_ci(k, n); list(p = if (n > 0) k / n else NA, lo = ci[1], hi = ci[2]) }
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA
  recall    <- if ((TP + FN) > 0) TP / (TP + FN) else NA
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA
  list(
    N = N, TP = TP, FP = FP, FN = FN, TN = TN,
    sensitivity = metric(TP, TP + FN),
    specificity = metric(TN, TN + FP),
    ppv         = metric(TP, TP + FP),
    npv         = metric(TN, TN + FN),
    accuracy    = metric(TP + TN, N),
    f1          = f1
  )
}

.normalize_for_match <- function(x) {
  x <- tolower(x)
  x <- gsub("[\u2018\u2019]", "'", x)
  x <- gsub("[\u201c\u201d]", '"', x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.is_verbatim <- function(evidence, note_text) {
  if (is.na(evidence) || is.na(note_text)) return(NA)
  if (!nzchar(trimws(evidence))) return(NA)
  ev <- .normalize_for_match(evidence)
  nt <- .normalize_for_match(note_text)
  if (nzchar(ev) && grepl(ev, nt, fixed = TRUE)) return(TRUE)
  ev_trim <- gsub("[.,;:]+$", "", ev)
  if (nzchar(ev_trim) && grepl(ev_trim, nt, fixed = TRUE)) return(TRUE)
  FALSE
}

find_evidence_cols <- function(df) grep("evidence_span$", names(df), value = TRUE)
find_review_cols    <- function(df) grep("review_status$", names(df), value = TRUE)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5, bg = "#0d1117", fg = "#e6edf3",
    primary = "#58a6ff", secondary = "#30363d",
    success = "#3fb950", danger = "#f85149", warning = "#d29922", info = "#58a6ff",
    base_font = font_google("IBM Plex Mono"),
    heading_font = font_google("IBM Plex Sans"),
    code_font = font_google("IBM Plex Mono")
  ),
  tags$head(tags$style(HTML("
    body { background: #0d1117; }
    .app-header {
      background: linear-gradient(135deg, #161b22 0%, #0d1117 100%);
      border-bottom: 1px solid #30363d; padding: 20px 32px 16px; margin-bottom: 18px;
    }
    .app-header h2 { font-family:'IBM Plex Sans',sans-serif; font-weight:700; font-size:1.35rem;
      letter-spacing:0.04em; color:#e6edf3; margin:0; }
    .app-header .subtitle { font-size:0.75rem; color:#8b949e; margin-top:4px;
      letter-spacing:0.08em; text-transform:uppercase; }
    .section-card { background:#161b22; border:1px solid #30363d; border-radius:8px;
      padding:16px 18px; margin-bottom:18px; }
    .section-title { font-family:'IBM Plex Sans',sans-serif; font-size:0.72rem; font-weight:600;
      letter-spacing:0.12em; text-transform:uppercase; color:#8b949e; margin-bottom:14px;
      border-bottom:1px solid #21262d; padding-bottom:8px; }
    .form-label { font-size:0.72rem; font-weight:600; letter-spacing:0.06em; text-transform:uppercase;
      color:#8b949e; margin-bottom:6px; }
    textarea.form-control, input.form-control, .form-select {
      background:#0d1117 !important; border:1px solid #30363d !important; color:#e6edf3 !important;
      border-radius:6px !important; font-family:'IBM Plex Mono',monospace !important; font-size:0.8rem !important; }
    .file-upload-area { border:2px dashed #30363d; border-radius:8px; padding:16px; text-align:center;
      background:#0d1117; transition:border-color 0.2s; }
    .file-upload-area:hover { border-color:#58a6ff; }
    .metric-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(150px,1fr)); gap:12px; margin:14px 0; }
    .metric-box { background:#0d1117; border:1px solid #21262d; border-radius:8px; padding:14px 16px; }
    .metric-box .metric-label { font-size:0.66rem; color:#8b949e; text-transform:uppercase;
      letter-spacing:0.08em; margin-bottom:6px; }
    .metric-box .metric-value { font-size:1.35rem; font-weight:700; font-family:'IBM Plex Mono',monospace; }
    .metric-box .metric-ci { font-size:0.68rem; color:#6e7681; margin-top:2px; }
    .mv-good { color:#3fb950; } .mv-warn { color:#d29922; } .mv-bad { color:#f85149; } .mv-neutral { color:#58a6ff; }
    .pill-tag { display:inline-block; padding:2px 10px; border-radius:20px; font-size:0.68rem;
      font-weight:600; letter-spacing:0.04em; }
    .pill-success { background:rgba(63,185,80,0.15); color:#3fb950; border:1px solid rgba(63,185,80,0.3); }
    .pill-danger  { background:rgba(248,81,73,0.15); color:#f85149; border:1px solid rgba(248,81,73,0.3); }
    .pill-pending { background:rgba(139,148,158,0.15); color:#8b949e; border:1px solid rgba(139,148,158,0.3); }
    .pill-info    { background:rgba(88,166,255,0.15); color:#58a6ff; border:1px solid rgba(88,166,255,0.3); }
    .confusion-table { border-collapse:collapse; font-family:'IBM Plex Mono',monospace; font-size:0.8rem; margin-top:10px; }
    .confusion-table td, .confusion-table th { border:1px solid #30363d; padding:10px 16px; text-align:center; }
    .confusion-table th { background:#161b22; color:#8b949e; font-size:0.68rem; text-transform:uppercase; }
    .confusion-table .cell-tp { background:rgba(63,185,80,0.15); color:#3fb950; font-weight:700; }
    .confusion-table .cell-tn { background:rgba(63,185,80,0.08); color:#3fb950; }
    .confusion-table .cell-fp { background:rgba(248,81,73,0.15); color:#f85149; font-weight:700; }
    .confusion-table .cell-fn { background:rgba(248,81,73,0.15); color:#f85149; font-weight:700; }
    .divider-line { border:none; border-top:1px solid #21262d; margin:14px 0; }
    .empty-state { text-align:center; padding:50px 20px; color:#8b949e; }
    .empty-state .empty-icon { font-size:2.4rem; margin-bottom:10px; }
    .nav-tabs .nav-link { color:#8b949e; font-family:'IBM Plex Sans',sans-serif; font-size:0.78rem;
      font-weight:600; letter-spacing:0.05em; text-transform:uppercase; }
    .nav-tabs .nav-link.active { color:#58a6ff; background:transparent; border-bottom:2px solid #58a6ff; }
    table.dataTable { background:#0d1117 !important; color:#e6edf3 !important; font-size:0.76rem !important;
      font-family:'IBM Plex Mono',monospace !important; }
    table.dataTable thead th { background:#161b22 !important; color:#8b949e !important; font-size:0.66rem !important;
      text-transform:uppercase !important; letter-spacing:0.06em !important; }
  "))),

  div(class = "app-header",
      h2("\U0001f4ca NLP Scoring Dashboard"),
      div(class = "subtitle", "Precision \u00b7 Recall \u00b7 F1 \u00b7 False Positives \u00b7 Hallucination Detection")),

  fluidRow(
    column(4,
      div(class = "section-card",
          div(class = "section-title", "1. Upload Pipeline Output"),
          div(class = "file-upload-area",
              fileInput("nlp_file", label = NULL, buttonLabel = "\U0001f4c2 Choose File",
                        placeholder = "CSV or XLSX \u2014 your NLP pipeline output",
                        accept = c(".csv", ".xlsx", ".xls"))),
          uiOutput("nlp_file_status")
      ),
      div(class = "section-card",
          div(class = "section-title", "2. Upload Gold Standard (optional)"),
          div(class = "file-upload-area",
              fileInput("gold_file", label = NULL, buttonLabel = "\U0001f4c2 Choose File",
                        placeholder = "CSV \u2014 human-adjudicated labels",
                        accept = c(".csv", ".xlsx", ".xls"))),
          uiOutput("gold_file_status"),
          div(style = "font-size:0.7rem;color:#8b949e;margin-top:8px;",
              "Needed only for the Accuracy tab (Precision/Recall/F1). The Hallucination tab works from the output file alone.")
      ),
      div(class = "section-card",
          div(class = "section-title", "3. Column Mapping"),
          uiOutput("join_key_ui"),
          uiOutput("present_col_ui"),
          textInput("positive_value", "Positive value (case-insensitive)", value = "true"),
          uiOutput("review_col_ui"),
          uiOutput("note_text_col_ui"),
          hr(class = "divider-line"),
          conditionalPanel(
            condition = "output.gold_uploaded == true",
            uiOutput("gold_label_col_ui"),
            textInput("gold_positive_value", "Gold positive value", value = "yes")
          ),
          actionButton("btn_compute", "\u25b6  Compute Scores",
                       class = "btn btn-primary w-100",
                       style = "font-family:'IBM Plex Mono';font-weight:700;margin-top:10px;")
      )
    ),
    column(8,
      navset_card_tab(
        nav_panel("Hallucination Check",
          uiOutput("halluc_area")
        ),
        nav_panel("Accuracy (Precision / Recall / F1)",
          uiOutput("accuracy_area")
        ),
        nav_panel("False Positives / False Negatives",
          uiOutput("fp_fn_area")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(nlp_df = NULL, gold_df = NULL, joined = NULL, metrics = NULL, halluc = NULL)

  # ---- File loading ----
  nlp_data <- reactive({
    req(input$nlp_file)
    tryCatch(read_any(input$nlp_file$datapath, input$nlp_file$name),
             error = function(e) { showNotification(paste("File error:", e$message), type = "error"); NULL })
  })

  gold_data <- reactive({
    req(input$gold_file)
    tryCatch(read_any(input$gold_file$datapath, input$gold_file$name),
             error = function(e) { showNotification(paste("Gold file error:", e$message), type = "error"); NULL })
  })

  output$nlp_file_status <- renderUI({
    req(input$nlp_file)
    df <- nlp_data(); req(df)
    span(sprintf("\U0001f4c4 %s \u00b7 %d rows \u00b7 %d cols", input$nlp_file$name, nrow(df), ncol(df)),
         class = "pill-tag pill-success", style = "font-size:0.72rem;margin-top:8px;display:inline-block;")
  })

  output$gold_file_status <- renderUI({
    req(input$gold_file)
    df <- gold_data(); req(df)
    span(sprintf("\U0001f4c4 %s \u00b7 %d rows", input$gold_file$name, nrow(df)),
         class = "pill-tag pill-success", style = "font-size:0.72rem;margin-top:8px;display:inline-block;")
  })

  output$gold_uploaded <- reactive({ !is.null(input$gold_file) })
  outputOptions(output, "gold_uploaded", suspendWhenHidden = FALSE)

  # ---- Column mapping UI (populated once a file is loaded) ----
  output$join_key_ui <- renderUI({
    df <- nlp_data(); req(df)
    default <- if ("NOTE_ID" %in% names(df)) "NOTE_ID" else names(df)[1]
    selectInput("join_key", "Join key (matches output <-> gold)", choices = names(df), selected = default)
  })

  output$present_col_ui <- renderUI({
    df <- nlp_data(); req(df)
    likely <- grep("present$|_result$", names(df), value = TRUE)
    default <- if (length(likely) > 0) likely[1] else names(df)[1]
    selectInput("present_col", "Present / result column to score", choices = names(df), selected = default)
  })

  output$review_col_ui <- renderUI({
    df <- nlp_data(); req(df)
    rcols <- find_review_cols(df)
    choices <- c("(none \u2014 raw only)" = "__none__", rcols)
    selectInput("review_col", "Review-status column (for gated view)", choices = choices,
                selected = if (length(rcols) > 0) rcols[1] else "__none__")
  })

  output$note_text_col_ui <- renderUI({
    df <- nlp_data(); req(df)
    default <- if ("NOTE_TEXT" %in% names(df)) "NOTE_TEXT" else names(df)[1]
    selectInput("note_text_col", "Note text column (for hallucination check)", choices = names(df), selected = default)
  })

  output$gold_label_col_ui <- renderUI({
    df <- gold_data(); req(df)
    default <- if ("gold_label" %in% names(df)) "gold_label" else names(df)[1]
    selectInput("gold_label_col", "Gold label column", choices = names(df), selected = default)
  })

  # ---- Compute on button press ----
  observeEvent(input$btn_compute, {
    df <- nlp_data()
    if (is.null(df)) { showNotification("Upload a pipeline output file first.", type = "error"); return() }

    # --- Hallucination check (always runs; needs no gold) ---
    ev_cols <- find_evidence_cols(df)
    note_col <- input$note_text_col
    halluc_summary <- list(); halluc_flagged <- list()
    if (length(ev_cols) > 0 && note_col %in% names(df)) {
      for (ec in ev_cols) {
        verbatim <- mapply(.is_verbatim, df[[ec]], df[[note_col]])
        n_checked <- sum(!is.na(verbatim))
        n_bad <- sum(!is.na(verbatim) & !verbatim)
        n_good <- sum(!is.na(verbatim) & verbatim)
        halluc_summary[[ec]] <- data.frame(
          evidence_column = ec, n_with_evidence = n_checked,
          n_verified = n_good, n_hallucinated = n_bad,
          hallucination_rate = if (n_checked > 0) round(n_bad / n_checked, 4) else NA_real_
        )
        if (n_bad > 0) {
          bad <- df[!is.na(verbatim) & !verbatim, , drop = FALSE]
          bad$hallucinated_column <- ec
          bad$hallucinated_text <- bad[[ec]]
          keep <- intersect(c(input$join_key, "NOTE_ID", "PAT_ID", "hallucinated_column",
                               "hallucinated_text", note_col), names(bad))
          halluc_flagged[[ec]] <- bad[, keep, drop = FALSE]
        }
      }
    }
    rv$halluc <- list(
      summary = if (length(halluc_summary) > 0) do.call(rbind, halluc_summary) else NULL,
      flagged = if (length(halluc_flagged) > 0) do.call(rbind, halluc_flagged) else NULL,
      ev_cols_found = ev_cols, note_col_used = note_col
    )

    # --- Accuracy (only if gold uploaded) ---
    rv$metrics <- NULL
    if (!is.null(input$gold_file)) {
      gdf <- gold_data()
      if (is.null(gdf)) { showNotification("Gold file failed to load.", type = "error"); return() }
      if (!(input$join_key %in% names(gdf))) {
        showNotification(sprintf("Join key '%s' not found in gold file.", input$join_key), type = "error")
        return()
      }
      df2  <- df;  df2[[input$join_key]]  <- as.character(df2[[input$join_key]])
      gdf2 <- gdf; gdf2[[input$join_key]] <- as.character(gdf2[[input$join_key]])

      gold_pos <- to_logical_positive(gdf2[[input$gold_label_col]], c(input$gold_positive_value))
      gdf2$.gold_pos <- gold_pos

      pred_raw <- to_logical_positive(df2[[input$present_col]], c(input$positive_value))
      df2$.pred_raw <- pred_raw

      use_review <- !is.null(input$review_col) && input$review_col != "__none__" && input$review_col %in% names(df2)
      if (use_review) {
        df2$.pred_gated <- norm(df2[[input$review_col]]) == "include"
      } else {
        df2$.pred_gated <- df2$.pred_raw
      }

      keep_cols <- unique(c(input$join_key, input$present_col, ".pred_raw", ".pred_gated",
                            if (use_review) input$review_col else NULL,
                            intersect(c("PAT_ID", "NOTE_ID"), names(df2))))
      j <- merge(gdf2[, c(input$join_key, ".gold_pos"), drop = FALSE],
                 df2[, keep_cols, drop = FALSE], by = input$join_key, all.x = TRUE)

      j$.pred_raw[is.na(j$.pred_raw)]     <- FALSE
      j$.pred_gated[is.na(j$.pred_gated)] <- FALSE

      m_raw   <- binary_metrics(j$.gold_pos, j$.pred_raw)
      m_gated <- binary_metrics(j$.gold_pos, j$.pred_gated)

      j$.disagree_raw <- j$.gold_pos != j$.pred_raw
      j$.error_type <- ifelse(j$.gold_pos & !j$.pred_raw, "false_negative",
                        ifelse(!j$.gold_pos & j$.pred_raw, "false_positive", "correct"))

      rv$metrics <- list(raw = m_raw, gated = m_gated, joined = j, n_matched = nrow(j),
                        n_gold = nrow(gdf2), n_nlp = nrow(df2), used_review = use_review)
    }

    showNotification("Scores computed.", type = "message", duration = 3)
  })

  # =========================================================================
  # HALLUCINATION TAB
  # =========================================================================
  output$halluc_area <- renderUI({
    if (is.null(rv$halluc)) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "\U0001f9ea"),
                 div("Upload a pipeline output file, map the columns, and click Compute Scores.")))
    }
    h <- rv$halluc
    if (length(h$ev_cols_found) == 0) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "\u26a0\ufe0f"),
                 div("No *_evidence_span column found in this file \u2014 nothing to check.")))
    }
    boxes <- lapply(seq_len(nrow(h$summary)), function(i) {
      r <- h$summary[i, ]
      rate <- r$hallucination_rate
      cls <- if (is.na(rate)) "mv-neutral" else if (rate == 0) "mv-good" else if (rate < 0.05) "mv-warn" else "mv-bad"
      div(class = "metric-box",
          div(class = "metric-label", r$evidence_column),
          div(class = paste("metric-value", cls), sprintf("%.1f%%", 100 * (rate %||% 0))),
          div(class = "metric-ci", sprintf("%d hallucinated / %d checked", r$n_hallucinated, r$n_with_evidence)))
    })
    tagList(
      div(class = "metric-grid", boxes),
      if (!is.null(h$flagged)) tagList(
        hr(class = "divider-line"),
        div(class = "section-title", sprintf("Flagged rows (%d)", nrow(h$flagged))),
        downloadButton("dl_halluc", "\u2b07 Download flagged rows CSV",
                       class = "btn btn-outline-danger btn-sm", style = "margin-bottom:10px;"),
        DT::dataTableOutput("halluc_table")
      ) else div(style = "margin-top:14px;color:#3fb950;font-size:0.85rem;",
                 "\u2713 No hallucinated evidence spans detected in this file.")
    )
  })

  output$halluc_table <- DT::renderDataTable({
    req(rv$halluc$flagged)
    DT::datatable(rv$halluc$flagged, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_halluc <- downloadHandler(
    filename = function() paste0("hallucinations_", tools::file_path_sans_ext(input$nlp_file$name), ".csv"),
    content = function(file) readr::write_csv(rv$halluc$flagged, file)
  )

  # =========================================================================
  # ACCURACY TAB
  # =========================================================================
  metric_box <- function(label, m, cls_fn = NULL) {
    val <- if (is.na(m$p)) "NA" else sprintf("%.3f", m$p)
    cls <- if (is.null(cls_fn)) "mv-neutral" else cls_fn(m$p)
    div(class = "metric-box",
        div(class = "metric-label", label),
        div(class = paste("metric-value", cls), val),
        div(class = "metric-ci", if (is.na(m$p)) "" else sprintf("95%% CI [%.3f\u2013%.3f]", m$lo, m$hi)))
  }

  render_metrics_block <- function(m, title) {
    grade <- function(p) if (is.na(p)) "mv-neutral" else if (p >= 0.85) "mv-good" else if (p >= 0.7) "mv-warn" else "mv-bad"
    tagList(
      div(class = "section-title", title),
      div(class = "metric-grid",
          metric_box("Recall (Sensitivity)", m$sensitivity, grade),
          metric_box("Precision (PPV)", m$ppv, grade),
          div(class = "metric-box",
              div(class = "metric-label", "F1 Score"),
              div(class = paste("metric-value", grade(m$f1)), if (is.na(m$f1)) "NA" else sprintf("%.3f", m$f1))),
          metric_box("Specificity", m$specificity, grade),
          metric_box("NPV", m$npv, grade),
          metric_box("Accuracy", m$accuracy, grade)
      ),
      tags$table(class = "confusion-table",
        tags$tr(tags$th(""), tags$th("Predicted +"), tags$th("Predicted \u2212")),
        tags$tr(tags$th("Gold +"), tags$td(class = "cell-tp", m$TP), tags$td(class = "cell-fn", m$FN)),
        tags$tr(tags$th("Gold \u2212"), tags$td(class = "cell-fp", m$FP), tags$td(class = "cell-tn", m$TN))
      ),
      div(style = "font-size:0.72rem;color:#8b949e;margin-top:8px;", sprintf("N scored: %d", m$N))
    )
  }

  output$accuracy_area <- renderUI({
    if (is.null(input$gold_file)) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "\U0001f3af"),
                 div("Upload a gold-standard CSV (Step 2) to compute Precision / Recall / F1."),
                 div(style = "margin-top:8px;font-size:0.75rem;",
                     "Gold CSV needs one row per note with a label column (e.g. NOTE_ID, gold_label).")))
    }
    if (is.null(rv$metrics)) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "\u25b6"),
                 div("Map your columns and click Compute Scores.")))
    }
    mt <- rv$metrics
    tagList(
      div(class = "section-card",
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              sprintf("Matched %d of %d gold rows against %d output rows on '%s'.%s",
                      mt$n_matched, mt$n_gold, mt$n_nlp, input$join_key,
                      if (!mt$used_review) "  (No review_status column selected \u2014 raw view only.)" else "")),
          render_metrics_block(mt$raw, "VIEW A \u2014 Raw model output")
      ),
      if (mt$used_review)
        div(class = "section-card",
            render_metrics_block(mt$gated, "VIEW B \u2014 After review-status gating (review_status == 'include')"))
      else NULL
    )
  })

  # =========================================================================
  # FALSE POSITIVE / FALSE NEGATIVE TAB
  # =========================================================================
  output$fp_fn_area <- renderUI({
    if (is.null(rv$metrics)) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "\U0001f50d"),
                 div("Compute scores with a gold standard uploaded to see disagreements here.")))
    }
    j <- rv$metrics$joined
    fp <- sum(j$.error_type == "false_positive")
    fn <- sum(j$.error_type == "false_negative")
    tagList(
      div(class = "metric-grid",
          div(class = "metric-box",
              div(class = "metric-label", "False Positives"),
              div(class = "metric-value mv-bad", fp),
              div(class = "metric-ci", "model said present; gold says absent")),
          div(class = "metric-box",
              div(class = "metric-label", "False Negatives"),
              div(class = "metric-value mv-bad", fn),
              div(class = "metric-ci", "model said absent; gold says present"))
      ),
      hr(class = "divider-line"),
      downloadButton("dl_disagree", "\u2b07 Download all disagreements CSV",
                     class = "btn btn-outline-danger btn-sm", style = "margin-bottom:10px;"),
      DT::dataTableOutput("disagree_table")
    )
  })

  output$disagree_table <- DT::renderDataTable({
    req(rv$metrics)
    j <- rv$metrics$joined
    disagree <- j[j$.error_type %in% c("false_positive", "false_negative"), , drop = FALSE]
    show_cols <- setdiff(names(disagree), c(".disagree_raw"))
    DT::datatable(disagree[, show_cols, drop = FALSE], options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_disagree <- downloadHandler(
    filename = function() paste0("disagreements_", tools::file_path_sans_ext(input$nlp_file$name), ".csv"),
    content = function(file) {
      j <- rv$metrics$joined
      readr::write_csv(j[j$.error_type %in% c("false_positive", "false_negative"), , drop = FALSE], file)
    }
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

shinyApp(ui = ui, server = server)
