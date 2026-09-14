# =============================================================================
# NLP Notes Pipeline — Shiny App  (matched to working engineer approach)
# =============================================================================

library(shiny)
library(bslib)
library(DBI)
library(odbc)
library(httr2)
library(jsonlite)
library(readr)
library(readxl)
library(dplyr)
library(DT)

# =============================================================================
# CONFIGURATION DEFAULTS
# =============================================================================
DEFAULT_DB_CONFIG <- list(
  server   = "your-sql-server.example.org",
  port     = 1433,
  database = "Clarity"
)

DEFAULT_AI_CONFIG <- list(
  project  = "your-gcp-project-id",
  location = "us-central1",
  model    = "gemini-2.5-flash"
)

DEFAULT_SQL <- "
--VERSION 6
IF OBJECT_ID('tempdb..#cases')      IS NOT NULL DROP TABLE #cases;
IF OBJECT_ID('tempdb..#csns')       IS NOT NULL DROP TABLE #csns;
IF OBJECT_ID('tempdb..#notes')      IS NOT NULL DROP TABLE #notes;
IF OBJECT_ID('tempdb..#note_text')  IS NOT NULL DROP TABLE #note_text;
IF OBJECT_ID('tempdb..#proc_agg')   IS NOT NULL DROP TABLE #proc_agg;
IF OBJECT_ID('tempdb..#preop_dx')   IS NOT NULL DROP TABLE #preop_dx;

SELECT DISTINCT orc.OR_CASE_ID, orc.PAT_ID, orc.SURGERY_DATE, orc.CASE_NAME
INTO #cases
FROM OR_CASE orc
JOIN OR_CASE_ALL_PROC ocap ON ocap.OR_CASE_ID = orc.OR_CASE_ID
WHERE orc.LOC_ID = 1010128;
CREATE INDEX IX_#cases_caseid ON #cases(OR_CASE_ID);
CREATE INDEX IX_#cases_pat    ON #cases(PAT_ID);

SELECT DISTINCT c.OR_CASE_ID, c.PAT_ID, c.SURGERY_DATE, c.CASE_NAME, v.CSN AS PAT_ENC_CSN_ID
INTO #csns
FROM #cases c
LEFT JOIN PAT_OR_ADM_LINK pol ON pol.CASE_ID = c.OR_CASE_ID AND pol.PAT_ID = c.PAT_ID
CROSS APPLY (VALUES (pol.PAT_ENC_CSN_ID),(pol.OR_LINK_CSN),(pol.OR_SRC_VISIT_CSN)) v(CSN)
WHERE v.CSN IS NOT NULL;

SELECT DISTINCT s.OR_CASE_ID, s.PAT_ID, s.SURGERY_DATE, s.CASE_NAME,
       s.PAT_ENC_CSN_ID, hi.NOTE_ID, hi.IP_NOTE_TYPE_C
INTO #notes
FROM #csns s
JOIN HNO_INFO hi ON hi.PAT_ID = s.PAT_ID AND hi.PAT_ENC_CSN_ID = s.PAT_ENC_CSN_ID
WHERE (hi.DELETED_CAT_C IS NULL OR hi.DELETED_CAT_C NOT IN (2,4,6))
  AND TRY_CONVERT(bigint, hi.ip_note_type_c) IN (1000004);

SELECT n.OR_CASE_ID, n.PAT_ID, n.SURGERY_DATE, n.CASE_NAME,
       n.PAT_ENC_CSN_ID, n.NOTE_ID, hnt.LINE, n.IP_NOTE_TYPE_C,
       hnt.CONTACT_DATE, hnt.NOTE_TEXT
INTO #note_text
FROM #notes n
JOIN HNO_NOTE_TEXT hnt ON hnt.NOTE_ID = n.NOTE_ID
WHERE CONVERT(date, hnt.CONTACT_DATE) = CONVERT(date, n.SURGERY_DATE);

SELECT ocap.OR_CASE_ID,
  STRING_AGG(CAST(ocap.PROC_DISPLAY_NAME AS varchar(max)), CHAR(10)) AS PROC_DISPLAY_NAME,
  STRING_AGG(CAST(opci.REAL_CPT_CODE     AS varchar(max)), CHAR(10)) AS REAL_CPT_CODE
INTO #proc_agg
FROM OR_CASE_ALL_PROC ocap
LEFT JOIN OR_PROC_CPT_ID opci ON opci.OR_PROC_ID = ocap.OR_PROC_ID
WHERE ocap.OR_CASE_ID IN (SELECT OR_CASE_ID FROM #cases)
GROUP BY ocap.OR_CASE_ID;

SELECT ocp.OR_CASE_ID,
  STRING_AGG(CAST(ocp.PRE_OP_DX AS varchar(max)), CHAR(10)) AS PRE_OP_DX
INTO #preop_dx
FROM OR_CASE_PREOPDX ocp
WHERE ocp.OR_CASE_ID IN (SELECT OR_CASE_ID FROM #cases)
GROUP BY ocp.OR_CASE_ID;

SELECT DISTINCT nt.OR_CASE_ID, nt.PAT_ID,
  id.IDENTITY_ID, nt.SURGERY_DATE, nt.CONTACT_DATE, nt.CASE_NAME,
  nt.PAT_ENC_CSN_ID, pa.PROC_DISPLAY_NAME, pa.REAL_CPT_CODE,
  pdx.PRE_OP_DX, nt.NOTE_ID, nt.LINE,
  TRY_CONVERT(bigint, nt.IP_NOTE_TYPE_C) AS IP_NOTE_TYPE_C,
  zcnt.NAME AS IP_NOTE_TYPE_NAME, nt.NOTE_TEXT
FROM #note_text nt
LEFT JOIN #proc_agg  pa   ON pa.OR_CASE_ID  = nt.OR_CASE_ID
LEFT JOIN #preop_dx  pdx  ON pdx.OR_CASE_ID = nt.OR_CASE_ID
LEFT JOIN IDENTITY_ID id  ON id.PAT_ID = nt.PAT_ID AND id.IDENTITY_TYPE_ID = '2'
LEFT JOIN ZC_NOTE_TYPE_IP zcnt ON TRY_CONVERT(bigint, zcnt.TYPE_IP_C) = TRY_CONVERT(bigint, nt.IP_NOTE_TYPE_C)
ORDER BY nt.SURGERY_DATE DESC, nt.OR_CASE_ID, nt.NOTE_ID, nt.LINE;
"

DEFAULT_PROMPT_TARGET   <- "diagnosis for NEC, necrotizing enterocolitis"
DEFAULT_PROMPT_TEMPLATE <- paste0(
  "Determine whether the following peri-operative note text contains evidence of: {TARGET}\n",
  "Return ONLY valid JSON matching the schema.\n\n",
  "Rules:\n",
  "- target_present: true if clearly documented (not just boilerplate).\n",
  "- negated: true if explicitly denied (e.g., 'no X').\n",
  "- target_terms: short supporting keywords/phrases.\n",
  "- where_found: brief context if clear.\n",
  "- confidence: number 0-1.\n\n",
  "Text: \"%s\"\n"
)

# =============================================================================
# BACKEND FUNCTIONS
# =============================================================================

find_gcloud <- function() {
  paths <- c(
    Sys.which("gcloud"),
    path.expand("~/google-cloud-sdk/bin/gcloud"),
    "/opt/homebrew/bin/gcloud",
    "/usr/local/bin/gcloud",
    "/usr/bin/gcloud"
  )
  for (p in paths) {
    if (p != "" && file.exists(p)) return(p)
  }
  NULL
}

# Matched to the working script: just get ADC token, no extra project checks
check_gcp_auth <- function() {
  gcloud_cmd <- find_gcloud()
  if (is.null(gcloud_cmd)) {
    return(list(ok = FALSE, token = NULL, msg = "gcloud CLI not found"))
  }
  
  token_result <- suppressWarnings(
    tryCatch(
      system(paste(gcloud_cmd, "auth application-default print-access-token 2>&1"), intern = TRUE),
      error = function(e) NULL
    )
  )
  
  if (is.null(token_result) || length(token_result) == 0 ||
      any(grepl("ERROR|error|not found|could not|invalid|expired", token_result, ignore.case = TRUE))) {
    return(list(ok = FALSE, token = NULL, msg = "Run: gcloud auth application-default login"))
  }
  
  list(ok = TRUE, token = trimws(token_result[1]), msg = NULL)
}

connect_clarity_trusted <- function(cfg) {
  DBI::dbConnect(
    odbc::odbc(),
    Driver                 = "ODBC Driver 17 for SQL Server",
    Server                 = cfg$server,
    Database               = cfg$database,
    Port                   = as.integer(cfg$port),
    Trusted_Connection     = "Yes",
    TrustServerCertificate = "Yes"
  )
}

connect_clarity_userpass <- function(cfg) {
  DBI::dbConnect(
    odbc::odbc(),
    Driver                 = "ODBC Driver 17 for SQL Server",
    Server                 = cfg$server,
    Database               = cfg$database,
    Port                   = as.integer(cfg$port),
    UID                    = cfg$username,
    PWD                    = cfg$password,
    TrustServerCertificate = "Yes"
  )
}

connect_clarity <- function(cfg) {
  if (cfg$auth_type == "windows") connect_clarity_trusted(cfg) else connect_clarity_userpass(cfg)
}

# Matched to the working script:
# - direct generateContent call
# - bearer token only
# - no x-goog-user-project
# - no Service Usage or project prechecks
call_gemini <- function(prompt, token, ai_cfg) {
  endpoint <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
    ai_cfg$location,
    ai_cfg$project,
    ai_cfg$location,
    ai_cfg$model
  )
  
  body <- list(
    contents = list(
      list(role = "user", parts = list(list(text = prompt)))
    ),
    generationConfig = list(
      temperature = 0.0,
      maxOutputTokens = 2048,
      responseMimeType = "application/json",
      responseSchema = list(
        type = "OBJECT",
        properties = list(
          target_present = list(type = "BOOLEAN"),
          target_terms   = list(type = "STRING"),
          where_found    = list(type = "STRING"),
          negated        = list(type = "BOOLEAN"),
          confidence     = list(type = "NUMBER")
        )
      )
    ),
    safetySettings = list(
      list(category = "HARM_CATEGORY_HARASSMENT",        threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HATE_SPEECH",       threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
    )
  )
  
  response <- tryCatch({
    httr2::request(endpoint) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(120) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(response)) {
    return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL))
  }
  
  if (httr2::resp_status(response) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_, reason = "HTTP_ERROR", detail = err_body))
  }
  
  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
  
  if (!is.null(result$candidates) && length(result$candidates) > 0) {
    candidate <- result$candidates[[1]]
    parts <- candidate$content$parts
    finish_reason <- candidate$finishReason
    
    if (!is.null(parts) && length(parts) > 0 && !is.null(parts[[1]]$text)) {
      return(list(text = parts[[1]]$text, reason = finish_reason, detail = NULL))
    }
  }
  
  list(text = NA_character_, reason = "UNKNOWN", detail = NULL)
}

clean_json_fence <- function(x) {
  if (is.na(x) || is.null(x)) return("")
  x <- trimws(x)
  x <- gsub("^```json\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^```\\s*",     "", x, ignore.case = TRUE)
  x <- gsub("\\s*```$",     "", x, ignore.case = TRUE)
  trimws(x)
}

load_uploaded_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  tryCatch({
    switch(
      ext,
      csv  = readr::read_csv(path, show_col_types = FALSE),
      xlsx = readxl::read_excel(path),
      xls  = readxl::read_excel(path),
      txt  = readr::read_delim(path, delim = "\t", show_col_types = FALSE),
      stop(paste("Unsupported file type:", ext))
    )
  }, error = function(e) stop(e$message))
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    bg           = "#0d1117",
    fg           = "#e6edf3",
    primary      = "#58a6ff",
    secondary    = "#30363d",
    success      = "#3fb950",
    danger       = "#f85149",
    warning      = "#d29922",
    info         = "#58a6ff",
    base_font    = font_google("IBM Plex Mono"),
    heading_font = font_google("IBM Plex Sans"),
    code_font    = font_google("IBM Plex Mono")
  ),
  tags$head(
    tags$style(HTML("
      body { background: #0d1117; }
      .app-header {
        background: linear-gradient(135deg, #161b22 0%, #0d1117 100%);
        border-bottom: 1px solid #30363d;
        padding: 20px 32px 16px;
        margin-bottom: 28px;
      }
      .app-header h2 {
        font-family: 'IBM Plex Sans', sans-serif;
        font-weight: 700; font-size: 1.4rem;
        letter-spacing: 0.04em; color: #e6edf3; margin: 0;
      }
      .app-header .subtitle {
        font-size: 0.75rem; color: #8b949e; margin-top: 4px;
        letter-spacing: 0.08em; text-transform: uppercase;
      }
      .section-card {
        background: #161b22; border: 1px solid #30363d;
        border-radius: 8px; padding: 20px 22px; margin-bottom: 18px;
      }
      .section-title {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 0.72rem; font-weight: 600;
        letter-spacing: 0.12em; text-transform: uppercase;
        color: #8b949e; margin-bottom: 14px;
        border-bottom: 1px solid #21262d; padding-bottom: 8px;
      }
      .status-row {
        display: flex; align-items: center; gap: 10px;
        padding: 10px 14px; border-radius: 6px;
        background: #0d1117; border: 1px solid #21262d;
        margin-bottom: 10px; font-size: 0.85rem;
      }
      .status-icon { font-size: 1.1rem; }
      .status-ok { color: #3fb950; }
      .status-fail { color: #f85149; }
      .status-pending { color: #8b949e; }
      textarea.form-control, input.form-control, .form-select {
        background: #0d1117 !important; border: 1px solid #30363d !important;
        color: #e6edf3 !important; border-radius: 6px !important;
        font-family: 'IBM Plex Mono', monospace !important; font-size: 0.8rem !important;
      }
      textarea.form-control:focus, input.form-control:focus {
        border-color: #58a6ff !important;
        box-shadow: 0 0 0 3px rgba(88,166,255,0.12) !important;
      }
      .form-label {
        font-size: 0.72rem; font-weight: 600;
        letter-spacing: 0.08em; text-transform: uppercase;
        color: #8b949e; margin-bottom: 6px;
      }
      .auth-radio .form-check {
        display: inline-block; margin-right: 16px;
      }
      .auth-radio .form-check-input {
        background-color: #0d1117 !important;
        border-color: #30363d !important;
      }
      .auth-radio .form-check-input:checked {
        background-color: #1f6feb !important;
        border-color: #1f6feb !important;
      }
      .auth-radio .form-check-label { font-size: 0.8rem; color: #c9d1d9; }
      .credentials-box {
        background: #0d1117; border: 1px solid #21262d;
        border-radius: 6px; padding: 12px 14px; margin-top: 10px;
      }
      .log-box {
        background: #010409; border: 1px solid #21262d;
        border-radius: 6px; padding: 14px 16px;
        font-family: 'IBM Plex Mono', monospace; font-size: 0.75rem;
        color: #7ee787; max-height: 260px; overflow-y: auto;
        white-space: pre-wrap; line-height: 1.6;
      }
      .progress-wrapper { margin: 12px 0; }
      .progress { background: #21262d !important; border-radius: 4px !important; height: 6px !important; }
      .progress-bar { background: #1f6feb !important; transition: width 0.3s ease !important; }
      table.dataTable {
        background: #0d1117 !important; color: #e6edf3 !important;
        font-size: 0.78rem !important; font-family: 'IBM Plex Mono', monospace !important; border: none !important;
      }
      table.dataTable thead th {
        background: #161b22 !important; color: #8b949e !important;
        font-size: 0.68rem !important; text-transform: uppercase !important;
        letter-spacing: 0.08em !important; border-bottom: 1px solid #30363d !important;
      }
      table.dataTable tbody tr { border-bottom: 1px solid #21262d !important; }
      table.dataTable tbody tr:hover td { background: #161b22 !important; }
      .shiny-file-input-progress { display: none; }
      .file-upload-area {
        border: 2px dashed #30363d; border-radius: 8px; padding: 18px;
        text-align: center; background: #0d1117; transition: border-color 0.2s;
      }
      .file-upload-area:hover { border-color: #58a6ff; }
      .pill-tag {
        display: inline-block; padding: 2px 10px; border-radius: 20px;
        font-size: 0.68rem; font-weight: 600; letter-spacing: 0.04em;
      }
      .pill-success { background: rgba(63,185,80,0.15); color: #3fb950; border: 1px solid rgba(63,185,80,0.3); }
      .pill-danger  { background: rgba(248,81,73,0.15); color: #f85149; border: 1px solid rgba(248,81,73,0.3); }
      .pill-pending { background: rgba(139,148,158,0.15); color: #8b949e; border: 1px solid rgba(139,148,158,0.3); }
      .eta-badge { font-size: 0.72rem; color: #8b949e; margin-left: 10px; }
      .note-preview-box {
        background: #010409; border: 1px solid #21262d; border-radius: 6px;
        padding: 12px 14px; font-family: 'IBM Plex Mono', monospace;
        font-size: 0.75rem; color: #c9d1d9; max-height: 180px;
        overflow-y: auto; white-space: pre-wrap; line-height: 1.6;
        margin-top: 10px;
      }
      .shiny-notification {
        background: #161b22 !important; border: 1px solid #30363d !important; color: #e6edf3 !important;
      }
      .filter-bar { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
      .filter-bar .form-select { width: auto !important; min-width: 160px; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update-progress', function(x) {
        var el = document.getElementById('prog_bar');
        if (!el) return;
        el.style.width = x.pct + '%';
        el.setAttribute('aria-valuenow', x.pct);
      });
    "))
  ),
  
  div(class = "app-header",
      h2("🔬 Clinical NLP Notes Pipeline"),
      div(class = "subtitle", "Clarity EHR  ·  Vertex AI / Gemini  ·  Structured Extraction")
  ),
  
  fluidRow(
    column(
      4,
      
      div(class = "section-card",
          div(class = "section-title", "System Status"),
          
          div(class = "status-row",
              span(class = "status-icon", uiOutput("gcp_icon")),
              div(
                div(class = "form-label", "GCP Authentication"),
                div(uiOutput("gcp_status_text"))
              )
          ),
          
          div(class = "status-row",
              span(class = "status-icon", uiOutput("db_icon")),
              div(
                div(class = "form-label", "Database Connection"),
                div(uiOutput("db_status_text"))
              )
          ),
          
          fluidRow(
            column(
              6,
              actionButton(
                "btn_check_gcp", "GCP Auth",
                class = "btn btn-sm btn-outline-info w-100",
                style = "font-family:'IBM Plex Mono';font-size:0.72rem;"
              )
            ),
            column(
              6,
              actionButton(
                "btn_test_db", "Test DB",
                class = "btn btn-sm btn-outline-secondary w-100",
                style = "font-family:'IBM Plex Mono';font-size:0.72rem;"
              )
            )
          )
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Database Config"),
          textInput("db_server", "Server", value = DEFAULT_DB_CONFIG$server),
          fluidRow(
            column(6, textInput("db_port", "Port", value = DEFAULT_DB_CONFIG$port)),
            column(6, textInput("db_name", "Database", value = DEFAULT_DB_CONFIG$database))
          ),
          div(class = "form-label", style = "margin-top: 10px;", "Authentication"),
          div(class = "auth-radio",
              radioButtons(
                "db_auth_type", label = NULL,
                choices  = c("Windows Authentication" = "windows",
                             "Username / Password"    = "userpass"),
                selected = "windows", inline = TRUE
              )
          ),
          conditionalPanel(
            condition = "input.db_auth_type == 'userpass'",
            div(class = "credentials-box",
                textInput("db_username", "Username", value = "", placeholder = "domain\\username or login"),
                passwordInput("db_password", "Password", value = "")
            )
          )
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Vertex AI Config"),
          textInput("ai_project", "GCP Project", value = DEFAULT_AI_CONFIG$project),
          fluidRow(
            column(6, textInput("ai_location", "Location", value = DEFAULT_AI_CONFIG$location)),
            column(6, textInput("ai_model", "Model", value = DEFAULT_AI_CONFIG$model))
          )
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Resume / Load from File"),
          div(class = "file-upload-area",
              fileInput(
                "upload_file", label = NULL,
                buttonLabel = "📂  Choose File",
                placeholder = "CSV · XLSX · TXT — drag & drop",
                accept = c(".csv", ".xlsx", ".xls", ".txt",
                           "text/csv",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                           "application/vnd.ms-excel",
                           "text/plain")
              )
          ),
          uiOutput("file_upload_status"),
          conditionalPanel(
            condition = "output.show_sheet_selector",
            div(style = "margin-top: 8px;", uiOutput("sheet_selector_ui"))
          )
      )
    ),
    
    column(
      8,
      
      div(class = "section-card",
          div(class = "section-title", "SQL Query  —  Clarity Database"),
          textAreaInput("sql_query", label = NULL,
                        value = DEFAULT_SQL, rows = 12, width = "100%")
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Extraction Prompt  —  Gemini"),
          div(class = "form-label", "Extraction Target"),
          textInput("prompt_target", label = NULL, value = DEFAULT_PROMPT_TARGET, width = "100%"),
          div(class = "form-label", "Prompt Template  ({TARGET} and %s are placeholders)"),
          textAreaInput("prompt_template", label = NULL,
                        value = DEFAULT_PROMPT_TEMPLATE, rows = 7, width = "100%")
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Run Pipeline"),
          fluidRow(
            column(
              4,
              numericInput("autosave_every", "Autosave every N rows",
                           value = 25, min = 5, max = 500, step = 5),
              numericInput("sleep_secs", "Delay between API calls (s)",
                           value = 4, min = 0, max = 30),
              numericInput("max_rows", "Max rows to process (0 = all)",
                           value = 0, min = 0, max = 10000, step = 10)
            ),
            column(
              4,
              checkboxInput("use_csv_mode", "Use uploaded file (skip DB query)", value = FALSE),
              div(style = "margin-top: 18px;",
                  actionButton(
                    "btn_run", "▶  Run Pipeline",
                    class = "btn btn-primary w-100",
                    style = "font-family:'IBM Plex Mono';font-weight:700;letter-spacing:0.05em;"
                  )
              )
            ),
            column(
              4,
              div(style = "margin-top: 34px;",
                  actionButton(
                    "btn_stop", "⏹  Stop",
                    class = "btn btn-danger w-100",
                    style = "font-family:'IBM Plex Mono';font-weight:600;"
                  ),
                  uiOutput("download_ui")
              )
            )
          ),
          
          div(class = "progress-wrapper",
              div(style = "display:flex; align-items:center; margin-bottom:6px;",
                  uiOutput("progress_label"),
                  span(class = "eta-badge", uiOutput("eta_label"))
              ),
              div(class = "progress",
                  div(id = "prog_bar", class = "progress-bar", role = "progressbar",
                      style = "width: 0%", `aria-valuenow` = "0",
                      `aria-valuemin` = "0", `aria-valuemax` = "100")
              )
          )
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Activity Log"),
          div(class = "log-box", verbatimTextOutput("log_output", placeholder = TRUE))
      ),
      
      div(class = "section-card",
          div(class = "section-title", "Results Preview  —  Extracted Fields"),
          div(class = "filter-bar",
              tags$label("Filter by target_present:", style = "font-size:0.72rem;color:#8b949e;"),
              selectInput(
                "filter_target", label = NULL,
                choices = c("All" = "all", "TRUE" = "true", "FALSE" = "false", "NA" = "na"),
                selected = "all", width = "160px"
              ),
              uiOutput("results_count_badge")
          ),
          DT::dataTableOutput("results_table"),
          uiOutput("note_preview_header"),
          uiOutput("note_preview_box")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    gcp_ok       = NULL,
    gcp_token    = NULL,
    gcp_msg      = "",
    db_ok        = NULL,
    db_msg       = "",
    log_lines    = character(0),
    notes_data   = NULL,
    processing   = FALSE,
    stop_flag    = FALSE,
    progress_i   = 0,
    progress_n   = 0,
    start_time   = NULL,
    selected_row = NULL
  )
  
  log_add <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "]  ", paste(...))
    rv$log_lines <- c(rv$log_lines, msg)
  }
  
  build_db_cfg <- function() {
    list(
      server    = input$db_server,
      port      = input$db_port,
      database  = input$db_name,
      auth_type = input$db_auth_type,
      username  = input$db_username,
      password  = input$db_password
    )
  }
  
  update_progress_bar <- function(i, n) {
    pct <- if (n > 0) round(100 * i / n) else 0
    session$sendCustomMessage("update-progress", list(pct = pct))
  }
  
  observe({
    update_progress_bar(rv$progress_i, rv$progress_n)
  })
  
  # ── GCP Auth ───────────────────────────────────────────────────────────────
  observeEvent(input$btn_check_gcp, {
    log_add("Checking GCP authentication...")
    gcp <- check_gcp_auth()
    rv$gcp_ok    <- gcp$ok
    rv$gcp_token <- gcp$token
    rv$gcp_msg   <- if (gcp$ok) "Authenticated via ADC" else gcp$msg
    log_add(if (gcp$ok) "✓ GCP auth OK" else paste("✗ GCP auth failed:", gcp$msg))
  })
  
  # ── DB Test ────────────────────────────────────────────────────────────────
  observeEvent(input$btn_test_db, {
    log_add("Testing database connection...")
    cfg <- build_db_cfg()
    conn <- tryCatch(connect_clarity(cfg), error = function(e) e)
    if (inherits(conn, "error")) {
      rv$db_ok  <- FALSE
      rv$db_msg <- conditionMessage(conn)
      log_add("✗ DB connection failed:", rv$db_msg)
    } else {
      rv$db_ok  <- TRUE
      rv$db_msg <- paste("Connected to", input$db_name, "on", input$db_server)
      log_add("✓ DB connection OK")
      DBI::dbDisconnect(conn)
    }
  })
  
  # ── Status outputs ─────────────────────────────────────────────────────────
  output$gcp_icon <- renderUI({
    if (is.null(rv$gcp_ok))  span("○", class = "status-pending")
    else if (rv$gcp_ok)      span("✓", class = "status-ok")
    else                     span("✗", class = "status-fail")
  })
  
  output$gcp_status_text <- renderUI({
    if (is.null(rv$gcp_ok))  span("Not checked", class = "pill-tag pill-pending")
    else if (rv$gcp_ok)      span("Authenticated", class = "pill-tag pill-success")
    else                     span(rv$gcp_msg, class = "pill-tag pill-danger")
  })
  
  output$db_icon <- renderUI({
    if (is.null(rv$db_ok))  span("○", class = "status-pending")
    else if (rv$db_ok)      span("✓", class = "status-ok")
    else                    span("✗", class = "status-fail")
  })
  
  output$db_status_text <- renderUI({
    if (is.null(rv$db_ok))  span("Not checked", class = "pill-tag pill-pending")
    else if (rv$db_ok)      span("Connected", class = "pill-tag pill-success")
    else                    span("Failed", class = "pill-tag pill-danger")
  })
  
  # ── File upload ────────────────────────────────────────────────────────────
  output$file_upload_status <- renderUI({
    req(input$upload_file)
    ext <- tolower(tools::file_ext(input$upload_file$name))
    icon <- switch(ext, csv = "📄", xlsx = "📊", xls = "📊", txt = "📝", "📁")
    span(
      paste(icon, input$upload_file$name),
      class = "pill-tag pill-success",
      style = "font-size:0.75rem; margin-top:6px; display:inline-block;"
    )
  })
  
  xlsx_sheets <- reactive({
    req(input$upload_file)
    ext <- tolower(tools::file_ext(input$upload_file$name))
    if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(input$upload_file$datapath) else NULL
  })
  
  output$show_sheet_selector <- reactive({
    !is.null(xlsx_sheets())
  })
  outputOptions(output, "show_sheet_selector", suspendWhenHidden = FALSE)
  
  output$sheet_selector_ui <- renderUI({
    sheets <- xlsx_sheets()
    req(sheets)
    selectInput("xlsx_sheet", "Sheet", choices = sheets, selected = sheets[1])
  })
  
  uploaded_data <- reactive({
    req(input$upload_file)
    ext  <- tolower(tools::file_ext(input$upload_file$name))
    path <- input$upload_file$datapath
    name <- input$upload_file$name
    
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        sheet <- if (!is.null(input$xlsx_sheet)) input$xlsx_sheet else 1
        readxl::read_excel(path, sheet = sheet)
      } else {
        load_uploaded_file(path, name)
      }
    }, error = function(e) {
      log_add("✗ File load error:", e$message)
      NULL
    })
  })
  
  # ── Log output ─────────────────────────────────────────────────────────────
  output$log_output <- renderText({
    paste(tail(rv$log_lines, 100), collapse = "\n")
  })
  
  # ── Progress / ETA ─────────────────────────────────────────────────────────
  output$progress_label <- renderUI({
    if (rv$progress_n == 0) return(div(style = "font-size:0.72rem;color:#8b949e;", "Idle"))
    pct <- round(100 * rv$progress_i / rv$progress_n)
    div(style = "font-size:0.72rem;color:#8b949e;",
        sprintf("Row %d / %d  (%d%%)", rv$progress_i, rv$progress_n, pct))
  })
  
  output$eta_label <- renderUI({
    if (is.null(rv$start_time) || rv$progress_i == 0 || rv$progress_n == 0) return(NULL)
    elapsed <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    rate <- rv$progress_i / max(elapsed, 0.001)
    remaining <- (rv$progress_n - rv$progress_i) / max(rate, 0.001)
    mins <- floor(remaining / 60)
    secs <- round(remaining %% 60)
    span(sprintf("ETA ~%dm %ds", mins, secs))
  })
  
  # ── Stop ───────────────────────────────────────────────────────────────────
  observeEvent(input$btn_stop, {
    rv$stop_flag <- TRUE
    log_add("⏹ Stop requested — will halt after current row.")
  })
  
  # ── Download ───────────────────────────────────────────────────────────────
  output$download_ui <- renderUI({
    req(!is.null(rv$notes_data))
    downloadButton(
      "btn_download", "⬇  Download CSV",
      class = "btn btn-outline-success w-100 mt-2",
      style = "font-family:'IBM Plex Mono';font-size:0.78rem;"
    )
  })
  
  output$btn_download <- downloadHandler(
    filename = function() sprintf("notes_llm_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) readr::write_csv(rv$notes_data, file)
  )
  
  # ── Results table ──────────────────────────────────────────────────────────
  filtered_data <- reactive({
    req(rv$notes_data)
    df <- rv$notes_data
    out_cols <- intersect(
      c("OR_CASE_ID", "NOTE_ID", "target_present", "target_terms", "where_found", "negated", "confidence"),
      names(df)
    )
    df <- df[, out_cols, drop = FALSE]
    filt <- input$filter_target
    if (filt == "true")  df <- df[!is.na(df$target_present) & tolower(df$target_present) == "true", , drop = FALSE]
    if (filt == "false") df <- df[!is.na(df$target_present) & tolower(df$target_present) == "false", , drop = FALSE]
    if (filt == "na")    df <- df[is.na(df$target_present), , drop = FALSE]
    df
  })
  
  output$results_count_badge <- renderUI({
    req(rv$notes_data)
    n_shown <- nrow(filtered_data())
    n_total <- nrow(rv$notes_data)
    span(sprintf("%d / %d rows", n_shown, n_total), class = "pill-tag pill-pending")
  })
  
  output$results_table <- DT::renderDataTable({
    df <- filtered_data()
    DT::datatable(
      head(df, 100),
      selection = "single",
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = "tp",
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).css({'background':'#0d1117','color':'#e6edf3'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = "display compact"
    )
  })
  
  observeEvent(input$results_table_rows_selected, {
    row_idx <- input$results_table_rows_selected
    if (!is.null(row_idx) && !is.null(rv$notes_data)) {
      df <- filtered_data()
      if ("NOTE_ID" %in% names(df) && "NOTE_ID" %in% names(rv$notes_data)) {
        note_id <- df$NOTE_ID[row_idx]
        matched <- rv$notes_data[rv$notes_data$NOTE_ID == note_id, , drop = FALSE]
        rv$selected_row <- if (nrow(matched) > 0) matched[1, , drop = FALSE] else NULL
      }
    }
  })
  
  output$note_preview_header <- renderUI({
    req(rv$selected_row)
    div(class = "form-label", style = "margin-top:12px;",
        sprintf("Note Text Preview — NOTE_ID %s", rv$selected_row$NOTE_ID))
  })
  
  output$note_preview_box <- renderUI({
    req(rv$selected_row)
    txt <- if ("NOTE_TEXT" %in% names(rv$selected_row)) rv$selected_row$NOTE_TEXT else "(no text)"
    div(class = "note-preview-box", txt)
  })
  
  # ── Main pipeline ──────────────────────────────────────────────────────────
  observeEvent(input$btn_run, {
    if (rv$processing) {
      showNotification("Pipeline already running.", type = "warning")
      return()
    }
    
    rv$stop_flag   <- FALSE
    rv$processing  <- TRUE
    rv$notes_data  <- NULL
    rv$selected_row <- NULL
    rv$start_time  <- Sys.time()
    rv$progress_i  <- 0
    rv$progress_n  <- 0
    
    log_add("═══ Pipeline started ═══")
    
    ai_cfg <- list(
      project  = input$ai_project,
      location = input$ai_location,
      model    = input$ai_model
    )
    db_cfg <- build_db_cfg()
    target <- input$prompt_target
    prompt_tmpl <- gsub("\\{TARGET\\}", target, input$prompt_template)
    
    on.exit({
      rv$processing <- FALSE
    }, add = TRUE)
    
    withProgress(message = "Running pipeline...", value = 0, {
      
      # 1. GCP auth only (matched working script)
      log_add("Checking GCP authentication...")
      gcp <- check_gcp_auth()
      rv$gcp_ok    <- gcp$ok
      rv$gcp_token <- gcp$token
      rv$gcp_msg   <- if (gcp$ok) "Authenticated via ADC" else gcp$msg
      
      if (!gcp$ok) {
        log_add("✗ GCP auth failed. Aborting.")
        showNotification(paste("GCP auth failed:", gcp$msg), type = "error", duration = NULL)
        return()
      }
      log_add("✓ GCP auth OK")
      
      # 2. Load data
      notes_data <- NULL
      use_file <- input$use_csv_mode && !is.null(input$upload_file)
      
      if (use_file) {
        log_add("Loading data from uploaded file...")
        notes_data <- tryCatch(uploaded_data(), error = function(e) {
          log_add("✗ File load error:", e$message)
          NULL
        })
        if (!is.null(notes_data)) log_add(sprintf("✓ Loaded %d rows from file", nrow(notes_data)))
      } else {
        log_add("Connecting to Clarity database...")
        conn <- tryCatch(connect_clarity(db_cfg), error = function(e) e)
        if (inherits(conn, "error")) {
          rv$db_ok  <- FALSE
          rv$db_msg <- conditionMessage(conn)
          log_add("✗ DB connection failed:", rv$db_msg)
          showNotification("DB connection failed.", type = "error", duration = NULL)
          return()
        }
        
        rv$db_ok  <- TRUE
        rv$db_msg <- "Connected"
        log_add("✓ DB connected — running SQL query...")
        
        notes_data <- tryCatch(DBI::dbGetQuery(conn, input$sql_query), error = function(e) {
          log_add("✗ SQL error:", e$message)
          NULL
        })
        
        DBI::dbDisconnect(conn)
        
        if (!is.null(notes_data)) log_add(sprintf("✓ Query returned %d rows", nrow(notes_data)))
      }
      
      if (is.null(notes_data)) {
        log_add("✗ No data to process. Aborting.")
        return()
      }
      
      if (!("NOTE_TEXT" %in% names(notes_data))) {
        log_add("✗ NOTE_TEXT column missing.")
        showNotification("NOTE_TEXT column missing.", type = "error", duration = NULL)
        return()
      }
      
      for (col in c("target_present", "target_terms", "where_found", "negated", "confidence")) {
        if (!(col %in% names(notes_data))) notes_data[[col]] <- NA_character_
      }
      
      max_r <- as.integer(input$max_rows)
      n_rows <- if (max_r > 0) min(nrow(notes_data), max_r) else nrow(notes_data)
      if (max_r > 0 && max_r < nrow(notes_data)) {
        log_add(sprintf("ℹ Max rows cap: processing %d of %d total rows", n_rows, nrow(notes_data)))
      }
      
      rv$progress_n <- n_rows
      rv$progress_i <- 0
      rv$start_time <- Sys.time()
      rv$notes_data <- notes_data
      
      log_add(sprintf("Starting extraction on %d rows...", n_rows))
      
      # 3. Row-by-row extraction
      for (i in seq_len(n_rows)) {
        
        if (rv$stop_flag) {
          log_add("⏹ Stopped by user at row", i)
          break
        }
        
        # matched working script: refresh token every 200 rows
        if (i > 1 && i %% 200 == 0) {
          log_add("[INFO] Refreshing Google Security Token to prevent expiration...")
          gcp <- check_gcp_auth()
          if (!gcp$ok) {
            log_add("✗ Token refresh failed.")
            break
          }
          rv$gcp_token <- gcp$token
          log_add("✓ Token refreshed")
        }
        
        current_text <- notes_data$NOTE_TEXT[i]
        if (is.na(current_text) || nchar(trimws(current_text)) < 5) {
          rv$progress_i <- i
          incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d", i, n_rows))
          next
        }
        
        log_add(sprintf("Processing row %d...", i))
        full_prompt <- sprintf(prompt_tmpl, current_text)
        
        success <- FALSE
        attempts <- 0
        
        # matched working script: up to 2 attempts
        while (!success && attempts < 2) {
          attempts <- attempts + 1
          if (attempts > 1) log_add("  -> Retrying row...")
          
          api_result <- call_gemini(full_prompt, rv$gcp_token, ai_cfg)
          json_response <- api_result$text
          finish_reason <- api_result$reason
          err_detail <- api_result$detail
          
          if (!is.na(json_response)) {
            clean_json <- clean_json_fence(json_response)
            
            tryCatch({
              parsed <- jsonlite::fromJSON(clean_json)
              
              notes_data$target_present[i] <- as.character(parsed$target_present)
              notes_data$target_terms[i]   <- as.character(parsed$target_terms)
              notes_data$where_found[i]    <- as.character(parsed$where_found)
              notes_data$negated[i]        <- as.character(parsed$negated)
              notes_data$confidence[i]     <- as.character(parsed$confidence)
              
              success <- TRUE
            }, error = function(e) {
              log_add(sprintf("  -> JSON ERROR (Attempt %d): %s", attempts, e$message))
              if (!is.null(finish_reason) && finish_reason != "STOP") {
                log_add(sprintf("     [API WARNING] Google cut response. Reason: %s", finish_reason))
              }
            })
          } else if (identical(finish_reason, "NETWORK_CRASH")) {
            log_add("  -> Network disconnected (Is laptop asleep?). Waiting 15 seconds to retry...")
            Sys.sleep(15)
          } else if (identical(finish_reason, "HTTP_ERROR")) {
            short_err <- substr(gsub("\\s+", " ", ifelse(is.null(err_detail), "", err_detail)), 1, 350)
            log_add(sprintf("  -> [API ERROR] HTTP_ERROR: %s", short_err))
          } else {
            log_add(sprintf("  -> No response. Reason: %s", finish_reason))
          }
          
          if (input$sleep_secs > 0) Sys.sleep(input$sleep_secs)
        }
        
        rv$progress_i <- i
        incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d", i, n_rows))
        
        # matched working script: autosave every 25 rows
        if (i %% input$autosave_every == 0) {
          readr::write_csv(notes_data, "notes_llm_AUTOSAVE.csv")
          log_add(sprintf("  -> Progress auto-saved to notes_llm_AUTOSAVE.csv at row %d", i))
          rv$notes_data <- notes_data
        }
      }
      
      rv$notes_data <- notes_data
      out_file <- sprintf("notes_llm_processed_%s.csv", format(Sys.Date(), "%Y%m%d"))
      readr::write_csv(notes_data, out_file)
      
      log_add(sprintf("✓ Done — saved to %s", out_file))
      log_add("═══ Pipeline complete ═══")
    })
  })
}

# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui = ui, server = server)