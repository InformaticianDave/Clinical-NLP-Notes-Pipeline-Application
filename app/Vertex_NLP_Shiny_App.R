# =============================================================================
# NLP Notes Pipeline — Shiny App
# Supports: Vertex AI (Gemini) or LM Studio (local OpenAI-compatible API)
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

DEFAULT_VERTEX_CONFIG <- list(
  project  = "your-gcp-project-id",
  location = "us-central1",
  model    = "gemini-2.5-flash"
)

DEFAULT_LM_CONFIG <- list(
  base_url = "http://localhost:1234/v1",
  model    = "biomistral"
)

DEFAULT_PROMPT_TARGET   <- "diagnosis for NEC, necrotizing enterocolitis"
DEFAULT_PROMPT_TEMPLATE <- paste0(
  "Determine whether the following peri-operative note text contains evidence of: {TARGET}\n",
  "Return ONLY valid JSON matching the schema.\n\n",
  "Rules:\n",
  "- target_present: true if clearly documented (not just boilerplate).\n",
  "- negated: true if explicitly denied (e.g., 'no X').\n",
  "- target_terms: short supporting keywords/phrases, or null if none.\n",
  "- where_found: brief context if clear, or null.\n",
  "- confidence: number 0-1.\n\n",
  "Respond with JSON only. No preamble, no markdown fences.\n\n",
  "Text: \"%s\"\n"
)

# =============================================================================
# DIAGNOSIS PRESETS
# Each entry: label, target, prompt_template, schema (Vertex responseSchema),
#             cols (result columns), col_labels (display names for table)
# =============================================================================
DIAGNOSIS_PRESETS <- list(

  NEC = list(
    label  = "NEC — Necrotizing Enterocolitis",
    target = "necrotizing enterocolitis (NEC)",
    prompt_template = paste0(
      "You are a clinical NLP assistant. Analyze this peri-operative or clinical note ",
      "for evidence of necrotizing enterocolitis (NEC).\n",
      "Return ONLY valid JSON — no markdown, no preamble.\n\n",
      "Fields:\n",
      "- target_present: true if NEC is a confirmed or working diagnosis; false otherwise.\n",
      "- nec_type: classify as \"medical\" if NEC was managed non-operatively (antibiotics, bowel rest, ",
      "NPO, TPN), \"surgical\" if the patient underwent an operation for NEC (laparotomy, bowel resection, ",
      "peritoneal drain placed for NEC), or null if target_present is false or type cannot be determined.\n",
      "- negated: true if NEC is explicitly ruled out or denied in the note.\n",
      "- confidence: 0.0–1.0 reflecting how certain the classification is.\n\n",
      "Important: base nec_type solely on what is documented in this note. ",
      "Do not infer from procedure type alone — a drain or laparotomy must be linked to NEC.\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present = list(type = "BOOLEAN"),
        nec_type       = list(type = "STRING"),
        negated        = list(type = "BOOLEAN"),
        confidence     = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "nec_type", "negated", "confidence"),
    col_labels = c("NEC Present", "Type (Medical/Surgical)", "Negated", "Confidence")
  ),

  IVH = list(
    label  = "IVH — Intraventricular Hemorrhage",
    target = "intraventricular hemorrhage (IVH)",
    prompt_template = paste0(
      "You are a clinical NLP assistant. Analyze this radiology or clinical note ",
      "for evidence of intraventricular hemorrhage (IVH).\n",
      "Return ONLY valid JSON — no markdown, no preamble.\n\n",
      "Fields:\n",
      "- target_present: true if IVH is documented.\n",
      "- grade: Papile grade if documented (\"I\",\"II\",\"III\",\"IV\"), else null.\n",
      "- laterality: \"left\", \"right\", \"bilateral\", or null.\n",
      "- hydrocephalus: true if post-hemorrhagic hydrocephalus is mentioned.\n",
      "- negated: true if IVH is explicitly ruled out.\n",
      "- confidence: 0.0–1.0.\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present = list(type = "BOOLEAN"),
        grade          = list(type = "STRING"),
        laterality     = list(type = "STRING"),
        hydrocephalus  = list(type = "BOOLEAN"),
        negated        = list(type = "BOOLEAN"),
        confidence     = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "grade", "laterality", "hydrocephalus", "negated", "confidence"),
    col_labels = c("Present", "Grade", "Laterality", "Hydrocephalus", "Negated", "Confidence")
  ),

  PVL = list(
    label  = "PVL — Periventricular Leukomalacia",
    target = "periventricular leukomalacia (PVL)",
    prompt_template = paste0(
      "You are a clinical NLP assistant. Analyze this radiology or clinical note ",
      "for evidence of periventricular leukomalacia (PVL).\n",
      "Return ONLY valid JSON — no markdown, no preamble.\n\n",
      "Fields:\n",
      "- target_present: true if PVL is documented.\n",
      "- cystic: true if cystic PVL is specifically documented.\n",
      "- echogenicity_noted: true if periventricular echogenicity or echodensity is noted ",
      "(may represent early or non-cystic PVL).\n",
      "- negated: true if PVL is explicitly ruled out.\n",
      "- confidence: 0.0–1.0.\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present     = list(type = "BOOLEAN"),
        cystic             = list(type = "BOOLEAN"),
        echogenicity_noted = list(type = "BOOLEAN"),
        negated            = list(type = "BOOLEAN"),
        confidence         = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "cystic", "echogenicity_noted", "negated", "confidence"),
    col_labels = c("Present", "Cystic", "Echogenicity Noted", "Negated", "Confidence")
  ),

  ROP = list(
    label  = "ROP — Retinopathy of Prematurity",
    target = "retinopathy of prematurity (ROP)",
    prompt_template = paste0(
      "You are a clinical NLP assistant. Analyze this ophthalmology or clinical note ",
      "for evidence of retinopathy of prematurity (ROP).\n",
      "Return ONLY valid JSON — no markdown, no preamble.\n\n",
      "Fields:\n",
      "- target_present: true if ROP is documented.\n",
      "- stage: ROP stage if documented (\"1\",\"2\",\"3\",\"4\",\"5\"), else null.\n",
      "- zone: ROP zone if documented (\"I\",\"II\",\"III\"), else null.\n",
      "- plus_disease: true if plus disease is documented.\n",
      "- treatment: \"laser\", \"bevacizumab\", \"both\", \"conservative\", or null.\n",
      "- negated: true if ROP is explicitly ruled out.\n",
      "- confidence: 0.0–1.0.\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present = list(type = "BOOLEAN"),
        stage          = list(type = "STRING"),
        zone           = list(type = "STRING"),
        plus_disease   = list(type = "BOOLEAN"),
        treatment      = list(type = "STRING"),
        negated        = list(type = "BOOLEAN"),
        confidence     = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "stage", "zone", "plus_disease", "treatment", "negated", "confidence"),
    col_labels = c("Present", "Stage", "Zone", "Plus Disease", "Treatment", "Negated", "Confidence")
  ),

  custom = list(
    label  = "Custom",
    target = DEFAULT_PROMPT_TARGET,
    prompt_template = DEFAULT_PROMPT_TEMPLATE,
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present = list(type = "BOOLEAN"),
        target_terms   = list(type = "STRING"),
        where_found    = list(type = "STRING"),
        negated        = list(type = "BOOLEAN"),
        confidence     = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "target_terms", "where_found", "negated", "confidence"),
    col_labels = c("Present", "Terms Found", "Where Found", "Negated", "Confidence")
  )
)

# =============================================================================
# BACKEND: DATABASE
# =============================================================================

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

# =============================================================================
# BACKEND: VERTEX AI (GCP / GEMINI)
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

check_gcp_auth <- function() {
  gcloud_cmd <- find_gcloud()
  if (is.null(gcloud_cmd))
    return(list(ok = FALSE, token = NULL, msg = "gcloud CLI not found"))

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

call_gemini <- function(prompt, token, ai_cfg, schema) {
  endpoint <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
    ai_cfg$location, ai_cfg$project, ai_cfg$location, ai_cfg$model
  )

  body <- list(
    contents = list(
      list(role = "user", parts = list(list(text = prompt)))
    ),
    generationConfig = list(
      temperature      = 0.0,
      maxOutputTokens  = 2048,
      responseMimeType = "application/json",
      responseSchema   = schema
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
  }, error = function(e) NULL)

  if (is.null(response))
    return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL))

  if (httr2::resp_status(response) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_, reason = "HTTP_ERROR", detail = err_body))
  }

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)

  if (!is.null(result$candidates) && length(result$candidates) > 0) {
    candidate    <- result$candidates[[1]]
    parts        <- candidate$content$parts
    finish_reason <- candidate$finishReason
    if (!is.null(parts) && length(parts) > 0 && !is.null(parts[[1]]$text))
      return(list(text = parts[[1]]$text, reason = finish_reason, detail = NULL))
  }

  list(text = NA_character_, reason = "UNKNOWN", detail = NULL)
}

# =============================================================================
# BACKEND: LM STUDIO (LOCAL OpenAI-compatible)
# =============================================================================

lm_base_url_clean <- function(base_url) sub("/+$", "", trimws(base_url))

check_lmstudio <- function(base_url) {
  endpoint <- paste0(lm_base_url_clean(base_url), "/models")
  response <- tryCatch({
    httr2::request(endpoint) |>
      httr2::req_timeout(8) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(response))
    return(list(ok = FALSE, models = NULL, msg = "Cannot reach LM Studio — is it running?"))

  status <- httr2::resp_status(response)
  if (status >= 400)
    return(list(ok = FALSE, models = NULL, msg = sprintf("HTTP %d from LM Studio", status)))

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
  models <- character(0)
  if (!is.null(result$data))
    models <- vapply(result$data, function(x) as.character(x$id), character(1))

  list(ok = TRUE, models = models, msg = NULL)
}

call_lmstudio <- function(prompt, base_url, model) {
  endpoint <- paste0(lm_base_url_clean(base_url), "/chat/completions")

  body <- list(
    model    = model,
    messages = list(
      list(role = "system",
           content = paste0(
             "You are a clinical NLP assistant. ",
             "Always respond with valid JSON only. ",
             "No markdown fences, no preamble, no explanation — raw JSON only."
           )),
      list(role = "user", content = prompt)
    ),
    temperature = 0.0,
    max_tokens  = 1024
  )

  response <- tryCatch({
    httr2::request(endpoint) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(180) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(response))
    return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL))

  if (httr2::resp_status(response) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_, reason = "HTTP_ERROR", detail = err_body))
  }

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)

  if (!is.null(result$choices) && length(result$choices) > 0) {
    content <- result$choices[[1]]$message$content
    if (!is.null(content) && nchar(trimws(content)) > 0)
      return(list(text = content, reason = "stop", detail = NULL))
  }

  list(text = NA_character_, reason = "UNKNOWN", detail = NULL)
}

# =============================================================================
# BACKEND: SHARED
# =============================================================================

call_llm <- function(prompt, provider, gcp_token, vertex_cfg, lm_cfg, schema) {
  if (provider == "vertex") {
    call_gemini(prompt, gcp_token, vertex_cfg, schema)
  } else {
    call_lmstudio(prompt, lm_cfg$base_url, lm_cfg$model)
  }
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
        padding: 20px 32px 16px; margin-bottom: 28px;
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
      .status-ok      { color: #3fb950; }
      .status-fail    { color: #f85149; }
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
      .auth-radio .form-check { display: inline-block; margin-right: 16px; }
      .auth-radio .form-check-input {
        background-color: #0d1117 !important; border-color: #30363d !important;
      }
      .auth-radio .form-check-input:checked {
        background-color: #1f6feb !important; border-color: #1f6feb !important;
      }
      .auth-radio .form-check-label { font-size: 0.8rem; color: #c9d1d9; }
      .credentials-box {
        background: #0d1117; border: 1px solid #21262d;
        border-radius: 6px; padding: 12px 14px; margin-top: 10px;
      }
      .provider-toggle .form-check { display: inline-block; margin-right: 20px; }
      .provider-toggle .form-check-input {
        background-color: #0d1117 !important; border-color: #30363d !important;
      }
      .provider-toggle .form-check-input:checked {
        background-color: #388bfd !important; border-color: #388bfd !important;
      }
      .provider-toggle .form-check-label { font-size: 0.85rem; color: #c9d1d9; font-weight: 600; }
      .model-list-box {
        background: #0d1117; border: 1px solid #21262d;
        border-radius: 6px; padding: 10px 12px; margin-top: 8px;
        max-height: 140px; overflow-y: auto;
      }
      .model-item {
        padding: 4px 8px; border-radius: 4px; cursor: pointer;
        font-family: 'IBM Plex Mono', monospace; font-size: 0.75rem;
        color: #c9d1d9; transition: background 0.15s;
      }
      .model-item:hover { background: #21262d; color: #58a6ff; }
      .log-box {
        background: #010409; border: 1px solid #21262d; border-radius: 6px;
        padding: 14px 16px; font-family: 'IBM Plex Mono', monospace;
        font-size: 0.75rem; color: #7ee787; max-height: 260px;
        overflow-y: auto; white-space: pre-wrap; line-height: 1.6;
      }
      .progress-wrapper { margin: 12px 0; }
      .progress { background: #21262d !important; border-radius: 4px !important; height: 6px !important; }
      .progress-bar { background: #1f6feb !important; transition: width 0.3s ease !important; }
      table.dataTable {
        background: #0d1117 !important; color: #e6edf3 !important;
        font-size: 0.78rem !important; font-family: 'IBM Plex Mono', monospace !important;
        border: none !important;
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
      .pill-success { background: rgba(63,185,80,0.15);   color: #3fb950; border: 1px solid rgba(63,185,80,0.3); }
      .pill-danger  { background: rgba(248,81,73,0.15);   color: #f85149; border: 1px solid rgba(248,81,73,0.3); }
      .pill-pending { background: rgba(139,148,158,0.15); color: #8b949e; border: 1px solid rgba(139,148,158,0.3); }
      .pill-info    { background: rgba(88,166,255,0.15);  color: #58a6ff; border: 1px solid rgba(88,166,255,0.3); }
      .eta-badge { font-size: 0.72rem; color: #8b949e; margin-left: 10px; }
      .note-preview-box {
        background: #010409; border: 1px solid #21262d; border-radius: 6px;
        padding: 12px 14px; font-family: 'IBM Plex Mono', monospace;
        font-size: 0.75rem; color: #c9d1d9; max-height: 180px;
        overflow-y: auto; white-space: pre-wrap; line-height: 1.6; margin-top: 10px;
      }
      .filter-bar { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
      .filter-bar .form-select { width: auto !important; min-width: 160px; }
      .divider-line {
        border: none; border-top: 1px solid #21262d; margin: 12px 0;
      }
      .diag-badge {
        display: inline-flex; align-items: center; gap: 6px;
        padding: 4px 12px; border-radius: 20px; font-size: 0.72rem; font-weight: 600;
        letter-spacing: 0.04em; margin-bottom: 12px;
      }
      .diag-NEC     { background: rgba(248,81,73,0.12);   color: #f85149; border: 1px solid rgba(248,81,73,0.3); }
      .diag-IVH     { background: rgba(88,166,255,0.12);  color: #58a6ff; border: 1px solid rgba(88,166,255,0.3); }
      .diag-PVL     { background: rgba(210,153,34,0.15);  color: #d29922; border: 1px solid rgba(210,153,34,0.3); }
      .diag-ROP     { background: rgba(63,185,80,0.12);   color: #3fb950; border: 1px solid rgba(63,185,80,0.3); }
      .diag-custom  { background: rgba(139,148,158,0.12); color: #8b949e; border: 1px solid rgba(139,148,158,0.3); }
      .schema-fields {
        display: flex; flex-wrap: wrap; gap: 5px; margin-top: 6px;
      }
      .schema-field {
        padding: 2px 8px; border-radius: 4px; font-size: 0.65rem; font-weight: 600;
        font-family: 'IBM Plex Mono', monospace; letter-spacing: 0.04em;
        background: #21262d; color: #8b949e; border: 1px solid #30363d;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update-progress', function(x) {
        var el = document.getElementById('prog_bar');
        if (!el) return;
        el.style.width = x.pct + '%';
        el.setAttribute('aria-valuenow', x.pct);
      });
      Shiny.addCustomMessageHandler('set-model-input', function(x) {
        var el = document.getElementById(x.id);
        if (el) { el.value = x.value; el.dispatchEvent(new Event('input')); }
      });
    "))
  ),

  div(class = "app-header",
      h2("🔬 Clinical NLP Notes Pipeline"),
      div(class = "subtitle", uiOutput("header_subtitle"))
  ),

  fluidRow(
    # ── LEFT COLUMN ──────────────────────────────────────────────────────────
    column(
      4,

      # System Status
      div(class = "section-card",
          div(class = "section-title", "System Status"),

          div(class = "status-row",
              span(class = "status-icon", uiOutput("llm_status_icon")),
              div(
                div(class = "form-label", uiOutput("llm_status_label")),
                div(uiOutput("llm_status_text"))
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
            column(6,
                   actionButton("btn_check_llm", "Check LLM",
                                class = "btn btn-sm btn-outline-info w-100",
                                style = "font-family:'IBM Plex Mono';font-size:0.72rem;")
            ),
            column(6,
                   actionButton("btn_test_db", "Test DB",
                                class = "btn btn-sm btn-outline-secondary w-100",
                                style = "font-family:'IBM Plex Mono';font-size:0.72rem;")
            )
          )
      ),

      # LLM Provider Config
      div(class = "section-card",
          div(class = "section-title", "LLM Provider"),

          div(class = "provider-toggle",
              radioButtons(
                "llm_provider", label = NULL,
                choices  = c("Vertex AI (GCP)" = "vertex", "LM Studio (Local)" = "lmstudio"),
                selected = "lmstudio", inline = TRUE
              )
          ),

          hr(class = "divider-line"),

          # ── VERTEX AI PANEL ──
          conditionalPanel(
            condition = "input.llm_provider == 'vertex'",
            div(
              textInput("ai_project",  "GCP Project",  value = DEFAULT_VERTEX_CONFIG$project),
              fluidRow(
                column(6, textInput("ai_location", "Location", value = DEFAULT_VERTEX_CONFIG$location)),
                column(6, textInput("ai_model_vertex", "Model", value = DEFAULT_VERTEX_CONFIG$model))
              )
            )
          ),

          # ── LM STUDIO PANEL ──
          conditionalPanel(
            condition = "input.llm_provider == 'lmstudio'",
            div(
              textInput("lm_base_url", "API Base URL", value = DEFAULT_LM_CONFIG$base_url,
                        placeholder = "http://localhost:1234/v1"),
              textInput("lm_model_input", "Model Name", value = DEFAULT_LM_CONFIG$model,
                        placeholder = "e.g. biomistral, qwen2.5-7b-instruct"),
              fluidRow(
                column(6,
                       actionButton("btn_fetch_models", "📋 Fetch Models",
                                    class = "btn btn-sm btn-outline-secondary w-100",
                                    style = "font-family:'IBM Plex Mono';font-size:0.7rem;")
                ),
                column(6,
                       div(style = "font-size:0.68rem;color:#8b949e;padding-top:4px;",
                           "Click a model below to use it ↓")
                )
              ),
              uiOutput("lm_model_list_ui")
            )
          )
      ),

      # Database Config
      div(class = "section-card",
          div(class = "section-title", "Database Config"),
          textInput("db_server", "Server", value = DEFAULT_DB_CONFIG$server),
          fluidRow(
            column(6, textInput("db_port", "Port",     value = DEFAULT_DB_CONFIG$port)),
            column(6, textInput("db_name", "Database", value = DEFAULT_DB_CONFIG$database))
          ),
          div(class = "form-label", style = "margin-top:10px;", "Authentication"),
          div(class = "auth-radio",
              radioButtons("db_auth_type", label = NULL,
                           choices  = c("Windows Authentication" = "windows",
                                        "Username / Password"    = "userpass"),
                           selected = "windows", inline = TRUE)
          ),
          conditionalPanel(
            condition = "input.db_auth_type == 'userpass'",
            div(class = "credentials-box",
                textInput("db_username", "Username", value = "",
                          placeholder = "domain\\username or login"),
                passwordInput("db_password", "Password", value = "")
            )
          )
      ),

      # File Upload
      div(class = "section-card",
          div(class = "section-title", "Resume / Load from File"),
          div(class = "file-upload-area",
              fileInput("upload_file", label = NULL,
                        buttonLabel = "📂  Choose File",
                        placeholder = "CSV · XLSX · TXT — drag & drop",
                        accept = c(".csv", ".xlsx", ".xls", ".txt",
                                   "text/csv",
                                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                   "application/vnd.ms-excel", "text/plain"))
          ),
          uiOutput("file_upload_status"),
          conditionalPanel(
            condition = "output.show_sheet_selector",
            div(style = "margin-top:8px;", uiOutput("sheet_selector_ui"))
          )
      )
    ),

    # ── RIGHT COLUMN ─────────────────────────────────────────────────────────
    column(
      8,

      # SQL Query
      div(class = "section-card",
          div(class = "section-title", "SQL Query  —  Clarity Database"),
          textAreaInput("sql_query", label = NULL, value = "", rows = 12, width = "100%",
                        placeholder = "Paste your Clarity SQL query here...")
      ),

      # Extraction Prompt
      div(class = "section-card",
          div(class = "section-title", "Extraction Prompt"),

          # Diagnosis selector row
          fluidRow(
            column(6,
                   div(class = "form-label", "Diagnosis Preset"),
                   selectInput(
                     "diagnosis_preset", label = NULL,
                     choices = c(
                       "NEC — Necrotizing Enterocolitis" = "NEC",
                       "IVH — Intraventricular Hemorrhage" = "IVH",
                       "PVL — Periventricular Leukomalacia" = "PVL",
                       "ROP — Retinopathy of Prematurity" = "ROP",
                       "Custom" = "custom"
                     ),
                     selected = "NEC", width = "100%"
                   )
            ),
            column(6,
                   div(class = "form-label", "Schema Fields"),
                   uiOutput("schema_fields_ui")
            )
          ),

          hr(class = "divider-line"),

          div(class = "form-label", "Extraction Target"),
          textInput("prompt_target", label = NULL,
                    value = DIAGNOSIS_PRESETS$NEC$target, width = "100%"),
          div(class = "form-label", "Prompt Template  (%s = note text placeholder)"),
          textAreaInput("prompt_template", label = NULL,
                        value = DIAGNOSIS_PRESETS$NEC$prompt_template, rows = 7, width = "100%")
      ),

      # Run Pipeline
      div(class = "section-card",
          div(class = "section-title", "Run Pipeline"),

          # Output path row
          fluidRow(
            column(7,
                   div(class = "form-label", "Output Directory"),
                   div(style = "display:flex;gap:6px;align-items:center;",
                       textInput("output_dir", label = NULL,
                                 value = path.expand("~"),
                                 placeholder = "e.g. C:/Users/you/Documents or /home/you",
                                 width = "100%"),
                       uiOutput("output_dir_status")
                   )
            ),
            column(5,
                   div(class = "form-label", "Filename  (no extension)"),
                   textInput("output_filename", label = NULL,
                             value = "notes_llm_processed",
                             placeholder = "my_extraction_run",
                             width = "100%")
            )
          ),
          uiOutput("output_path_preview"),

          hr(class = "divider-line"),

          fluidRow(
            column(4,
                   numericInput("autosave_every", "Autosave every N rows",
                                value = 25, min = 5, max = 500, step = 5),
                   numericInput("sleep_secs", "Delay between API calls (s)",
                                value = 2, min = 0, max = 30),
                   numericInput("max_rows", "Max rows (0 = all)",
                                value = 0, min = 0, max = 10000, step = 10)
            ),
            column(4,
                   checkboxInput("use_csv_mode", "Use uploaded file (skip DB query)", value = FALSE),
                   div(style = "margin-top:18px;",
                       actionButton("btn_run", "▶  Run Pipeline",
                                    class = "btn btn-primary w-100",
                                    style = "font-family:'IBM Plex Mono';font-weight:700;letter-spacing:0.05em;")
                   )
            ),
            column(4,
                   div(style = "margin-top:34px;",
                       actionButton("btn_stop", "⏹  Stop",
                                    class = "btn btn-danger w-100",
                                    style = "font-family:'IBM Plex Mono';font-weight:600;"),
                       uiOutput("download_ui")
                   )
            )
          ),

          div(class = "progress-wrapper",
              div(style = "display:flex;align-items:center;margin-bottom:6px;",
                  uiOutput("progress_label"),
                  span(class = "eta-badge", uiOutput("eta_label"))
              ),
              div(class = "progress",
                  div(id = "prog_bar", class = "progress-bar", role = "progressbar",
                      style = "width:0%", `aria-valuenow` = "0",
                      `aria-valuemin` = "0", `aria-valuemax` = "100")
              )
          )
      ),

      # Log
      div(class = "section-card",
          div(class = "section-title", "Activity Log"),
          div(class = "log-box", verbatimTextOutput("log_output", placeholder = TRUE))
      ),

      # Results
      div(class = "section-card",
          div(class = "section-title", "Results Preview  —  Extracted Fields"),
          div(class = "filter-bar",
              tags$label("Filter by target_present:", style = "font-size:0.72rem;color:#8b949e;"),
              selectInput("filter_target", label = NULL,
                          choices  = c("All" = "all", "TRUE" = "true", "FALSE" = "false", "NA" = "na"),
                          selected = "all", width = "160px"),
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
    gcp_ok           = NULL,
    gcp_token        = NULL,
    gcp_msg          = "",
    lm_ok            = NULL,
    lm_msg           = "",
    lm_models        = character(0),
    db_ok            = NULL,
    db_msg           = "",
    log_lines        = character(0),
    notes_data       = NULL,
    active_diagnosis = "NEC",
    processing       = FALSE,
    stop_flag        = FALSE,
    progress_i       = 0,
    progress_n       = 0,
    start_time       = NULL,
    selected_row     = NULL
  )

  log_add <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "]  ", paste(...))
    rv$log_lines <- c(rv$log_lines, msg)
  }

  build_db_cfg <- function() {
    list(server = input$db_server, port = input$db_port, database = input$db_name,
         auth_type = input$db_auth_type, username = input$db_username, password = input$db_password)
  }

  build_vertex_cfg <- function() {
    list(project = input$ai_project, location = input$ai_location, model = input$ai_model_vertex)
  }

  build_lm_cfg <- function() {
    list(base_url = input$lm_base_url, model = input$lm_model_input)
  }

  update_progress_bar <- function(i, n) {
    pct <- if (n > 0) round(100 * i / n) else 0
    session$sendCustomMessage("update-progress", list(pct = pct))
  }

  observe({ update_progress_bar(rv$progress_i, rv$progress_n) })

  # ── Diagnosis preset switching ────────────────────────────────────────────────
  observeEvent(input$diagnosis_preset, {
    key    <- input$diagnosis_preset
    preset <- DIAGNOSIS_PRESETS[[key]]
    if (is.null(preset)) return()
    updateTextInput(session,     "prompt_target",   value = preset$target)
    updateTextAreaInput(session, "prompt_template", value = preset$prompt_template)
    # Suggest a matching filename when switching (only if user hasn't customised it)
    cur_fname <- trimws(input$output_filename)
    if (cur_fname == "" || cur_fname %in% vapply(DIAGNOSIS_PRESETS, function(p)
      tolower(gsub("[^A-Za-z0-9]", "_", p$label)), character(1)) ||
      grepl("^notes_llm", cur_fname)) {
      updateTextInput(session, "output_filename",
                      value = paste0(tolower(key), "_nlp_extraction"))
    }
    log_add(sprintf("Preset switched to: %s", preset$label))
  }, ignoreInit = TRUE)

  # Schema field pills shown next to preset selector
  output$schema_fields_ui <- renderUI({
    key    <- if (!is.null(input$diagnosis_preset)) input$diagnosis_preset else "NEC"
    preset <- DIAGNOSIS_PRESETS[[key]]
    if (is.null(preset)) return(NULL)
    field_pills <- lapply(preset$col_labels, function(lbl) {
      span(class = "schema-field", lbl)
    })
    div(class = "schema-fields", tagList(field_pills))
  })

  # ── Header subtitle ─────────────────────────────────────────────────────────
  output$header_subtitle <- renderUI({
    provider_label <- if (!is.null(input$llm_provider) && input$llm_provider == "vertex")
      "Clarity EHR  ·  Vertex AI / Gemini" else "Clarity EHR  ·  LM Studio (Local)"
    span(provider_label)
  })

  # ── LLM status labels (switch by provider) ──────────────────────────────────
  output$llm_status_label <- renderUI({
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex")
      "GCP Authentication" else "LM Studio Connection"
  })

  output$llm_status_icon <- renderUI({
    ok <- if (!is.null(input$llm_provider) && input$llm_provider == "vertex") rv$gcp_ok else rv$lm_ok
    if (is.null(ok))  span("○", class = "status-pending")
    else if (ok)      span("✓", class = "status-ok")
    else              span("✗", class = "status-fail")
  })

  output$llm_status_text <- renderUI({
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex") {
      if (is.null(rv$gcp_ok))  span("Not checked",     class = "pill-tag pill-pending")
      else if (rv$gcp_ok)      span("Authenticated",   class = "pill-tag pill-success")
      else                     span(rv$gcp_msg,         class = "pill-tag pill-danger")
    } else {
      if (is.null(rv$lm_ok))  span("Not checked",      class = "pill-tag pill-pending")
      else if (rv$lm_ok) {
        model_count <- length(rv$lm_models)
        label <- if (model_count > 0) sprintf("Connected · %d model(s)", model_count) else "Connected"
        span(label, class = "pill-tag pill-success")
      } else                  span(rv$lm_msg,           class = "pill-tag pill-danger")
    }
  })

  # ── Check LLM button (routes by provider) ───────────────────────────────────
  observeEvent(input$btn_check_llm, {
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex") {
      log_add("Checking GCP authentication...")
      gcp <- check_gcp_auth()
      rv$gcp_ok    <- gcp$ok
      rv$gcp_token <- gcp$token
      rv$gcp_msg   <- if (gcp$ok) "Authenticated via ADC" else gcp$msg
      log_add(if (gcp$ok) "✓ GCP auth OK" else paste("✗ GCP auth failed:", gcp$msg))
    } else {
      log_add("Testing LM Studio connection at", input$lm_base_url, "...")
      result <- check_lmstudio(input$lm_base_url)
      rv$lm_ok     <- result$ok
      rv$lm_msg    <- if (result$ok) "Connected" else result$msg
      rv$lm_models <- if (!is.null(result$models)) result$models else character(0)
      if (result$ok) {
        log_add(sprintf("✓ LM Studio OK — %d model(s) found", length(rv$lm_models)))
        if (length(rv$lm_models) > 0) log_add("  Models:", paste(rv$lm_models, collapse = ", "))
      } else {
        log_add("✗ LM Studio connection failed:", result$msg)
      }
    }
  })

  # ── Fetch Models button (LM Studio) ─────────────────────────────────────────
  observeEvent(input$btn_fetch_models, {
    log_add("Fetching models from LM Studio at", input$lm_base_url, "...")
    result <- check_lmstudio(input$lm_base_url)
    rv$lm_ok     <- result$ok
    rv$lm_msg    <- if (result$ok) "Connected" else result$msg
    rv$lm_models <- if (!is.null(result$models)) result$models else character(0)
    if (result$ok && length(rv$lm_models) > 0) {
      log_add(sprintf("✓ %d model(s) available:", length(rv$lm_models)))
      for (m in rv$lm_models) log_add("   •", m)
    } else if (result$ok) {
      log_add("✓ Connected but no models returned (server may need a model loaded)")
    } else {
      log_add("✗ Failed to reach LM Studio:", result$msg)
    }
  })

  # ── Model list UI (clickable, sets model input) ─────────────────────────────
  output$lm_model_list_ui <- renderUI({
    models <- rv$lm_models
    if (length(models) == 0) return(NULL)

    items <- lapply(models, function(m) {
      div(
        class = "model-item",
        onclick = sprintf(
          "Shiny.setInputValue('lm_model_click', '%s', {priority: 'event'});", m
        ),
        m
      )
    })

    tagList(
      div(class = "form-label", style = "margin-top:10px;", "Available Models (click to select)"),
      div(class = "model-list-box", tagList(items))
    )
  })

  observeEvent(input$lm_model_click, {
    updateTextInput(session, "lm_model_input", value = input$lm_model_click)
    log_add("Model set to:", input$lm_model_click)
  })

  # ── DB Test ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_test_db, {
    log_add("Testing database connection...")
    cfg  <- build_db_cfg()
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

  output$db_icon <- renderUI({
    if (is.null(rv$db_ok))  span("○", class = "status-pending")
    else if (rv$db_ok)      span("✓", class = "status-ok")
    else                    span("✗", class = "status-fail")
  })

  output$db_status_text <- renderUI({
    if (is.null(rv$db_ok))  span("Not checked", class = "pill-tag pill-pending")
    else if (rv$db_ok)      span("Connected",   class = "pill-tag pill-success")
    else                    span("Failed",       class = "pill-tag pill-danger")
  })

  # ── File upload ──────────────────────────────────────────────────────────────
  output$file_upload_status <- renderUI({
    req(input$upload_file)
    ext  <- tolower(tools::file_ext(input$upload_file$name))
    icon <- switch(ext, csv = "📄", xlsx = "📊", xls = "📊", txt = "📝", "📁")
    span(paste(icon, input$upload_file$name),
         class = "pill-tag pill-success",
         style = "font-size:0.75rem;margin-top:6px;display:inline-block;")
  })

  xlsx_sheets <- reactive({
    req(input$upload_file)
    ext <- tolower(tools::file_ext(input$upload_file$name))
    if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(input$upload_file$datapath) else NULL
  })

  output$show_sheet_selector <- reactive({ !is.null(xlsx_sheets()) })
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
    }, error = function(e) { log_add("✗ File load error:", e$message); NULL })
  })

  # ── Log output ───────────────────────────────────────────────────────────────
  output$log_output <- renderText({
    paste(tail(rv$log_lines, 100), collapse = "\n")
  })

  # ── Progress / ETA ───────────────────────────────────────────────────────────
  output$progress_label <- renderUI({
    if (rv$progress_n == 0) return(div(style = "font-size:0.72rem;color:#8b949e;", "Idle"))
    pct <- round(100 * rv$progress_i / rv$progress_n)
    div(style = "font-size:0.72rem;color:#8b949e;",
        sprintf("Row %d / %d  (%d%%)", rv$progress_i, rv$progress_n, pct))
  })

  output$eta_label <- renderUI({
    if (is.null(rv$start_time) || rv$progress_i == 0 || rv$progress_n == 0) return(NULL)
    elapsed   <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    rate      <- rv$progress_i / max(elapsed, 0.001)
    remaining <- (rv$progress_n - rv$progress_i) / max(rate, 0.001)
    mins <- floor(remaining / 60)
    secs <- round(remaining %% 60)
    span(sprintf("ETA ~%dm %ds", mins, secs))
  })

  # ── Stop ─────────────────────────────────────────────────────────────────────
  observeEvent(input$btn_stop, {
    rv$stop_flag <- TRUE
    log_add("⏹ Stop requested — will halt after current row.")
  })

  # ── Output path helpers ───────────────────────────────────────────────────────
  resolved_output_path <- reactive({
    fname <- trimws(input$output_filename)
    dname <- trimws(input$output_dir)
    if (fname == "") fname <- "notes_llm_processed"
    fname <- gsub("\\.csv$", "", fname, ignore.case = TRUE)   # strip accidental .csv
    fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)             # sanitise
    list(
      dir      = dname,
      base     = fname,
      final    = file.path(dname, paste0(fname, ".csv")),
      autosave = file.path(dname, paste0(fname, "_AUTOSAVE.csv"))
    )
  })

  output$output_dir_status <- renderUI({
    d <- trimws(input$output_dir)
    if (d == "") return(NULL)
    if (!dir.exists(d))
      span("✗", title = "Directory does not exist", class = "status-fail",
           style = "font-size:1rem;flex-shrink:0;")
    else if (file.access(d, 2) != 0)
      span("⚠", title = "Directory not writable", class = "status-fail",
           style = "font-size:1rem;flex-shrink:0;")
    else
      span("✓", title = "Directory OK", class = "status-ok",
           style = "font-size:1rem;flex-shrink:0;")
  })

  output$output_path_preview <- renderUI({
    p <- resolved_output_path()
    div(style = "font-family:'IBM Plex Mono',monospace;font-size:0.7rem;color:#8b949e;margin-bottom:4px;",
        sprintf("→ %s", p$final))
  })

  # ── Download ─────────────────────────────────────────────────────────────────
  output$download_ui <- renderUI({
    req(!is.null(rv$notes_data))
    downloadButton("btn_download", "⬇  Download CSV",
                   class = "btn btn-outline-success w-100 mt-2",
                   style = "font-family:'IBM Plex Mono';font-size:0.78rem;")
  })

  output$btn_download <- downloadHandler(
    filename = function() {
      fname <- trimws(input$output_filename)
      if (fname == "") fname <- "notes_llm_processed"
      fname <- gsub("\\.csv$", "", fname, ignore.case = TRUE)
      fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)
      paste0(fname, ".csv")
    },
    content = function(file) readr::write_csv(rv$notes_data, file)
  )

  # ── Results table ─────────────────────────────────────────────────────────────
  filtered_data <- reactive({
    req(rv$notes_data)
    df     <- rv$notes_data
    key    <- rv$active_diagnosis
    preset <- DIAGNOSIS_PRESETS[[key]]

    # ID columns always shown first
    id_cols   <- intersect(c("OR_CASE_ID", "NOTE_ID"), names(df))
    diag_cols <- intersect(preset$cols, names(df))
    out_cols  <- c(id_cols, diag_cols)
    df        <- df[, out_cols, drop = FALSE]

    # Rename diag columns to human-readable labels
    present_diag <- intersect(preset$cols, names(df))
    label_map    <- setNames(preset$col_labels, preset$cols)
    names(df)[names(df) %in% present_diag] <- label_map[present_diag]

    # Filter
    present_col <- "Present"
    filt <- input$filter_target
    if (present_col %in% names(df)) {
      if (filt == "true")  df <- df[!is.na(df[[present_col]]) & tolower(df[[present_col]]) == "true",  , drop = FALSE]
      if (filt == "false") df <- df[!is.na(df[[present_col]]) & tolower(df[[present_col]]) == "false", , drop = FALSE]
      if (filt == "na")    df <- df[is.na(df[[present_col]]), , drop = FALSE]
    }
    df
  })

  output$results_count_badge <- renderUI({
    req(rv$notes_data)
    key   <- rv$active_diagnosis
    badge_class <- paste0("pill-tag diag-", key)
    tagList(
      span(sprintf("%d / %d rows", nrow(filtered_data()), nrow(rv$notes_data)),
           class = "pill-tag pill-pending"),
      span(DIAGNOSIS_PRESETS[[key]]$label, class = badge_class,
           style = "margin-left:6px;")
    )
  })

  output$results_table <- DT::renderDataTable({
    df <- filtered_data()
    DT::datatable(
      head(df, 200),
      selection = "single",
      options   = list(
        pageLength  = 10, scrollX = TRUE, dom = "tp",
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).css({'background':'#0d1117','color':'#e6edf3'});",
          "}"
        )
      ),
      rownames = FALSE, class = "display compact"
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

  # ── Main pipeline ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_run, {
    if (rv$processing) { showNotification("Pipeline already running.", type = "warning"); return() }

    rv$stop_flag    <- FALSE
    rv$processing   <- TRUE
    rv$notes_data   <- NULL
    rv$selected_row <- NULL
    rv$start_time   <- Sys.time()
    rv$progress_i   <- 0
    rv$progress_n   <- 0

    provider    <- input$llm_provider
    vertex_cfg  <- build_vertex_cfg()
    lm_cfg      <- build_lm_cfg()
    db_cfg      <- build_db_cfg()

    # Lock in the active preset for this run
    preset_key  <- input$diagnosis_preset
    preset      <- DIAGNOSIS_PRESETS[[preset_key]]
    active_schema <- preset$schema
    active_cols   <- preset$cols

    target      <- input$prompt_target
    prompt_tmpl <- input$prompt_template   # already filled by preset; user may override

    log_add("═══ Pipeline started ═══")
    log_add(sprintf("Diagnosis: %s", preset$label))
    log_add(sprintf("Provider:  %s", if (provider == "vertex") "Vertex AI (Gemini)" else paste("LM Studio —", lm_cfg$model)))

    on.exit({ rv$processing <- FALSE }, add = TRUE)

    withProgress(message = "Running pipeline...", value = 0, {

      # 1. Auth / connection check
      if (provider == "vertex") {
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
      } else {
        log_add("Verifying LM Studio connection...")
        lm_check <- check_lmstudio(lm_cfg$base_url)
        rv$lm_ok  <- lm_check$ok
        rv$lm_msg <- if (lm_check$ok) "Connected" else lm_check$msg
        if (!lm_check$ok) {
          log_add("✗ LM Studio not reachable. Aborting.")
          showNotification(paste("LM Studio error:", lm_check$msg), type = "error", duration = NULL)
          return()
        }
        log_add(sprintf("✓ LM Studio OK (model: %s)", lm_cfg$model))
      }

      # 2. Load data
      notes_data <- NULL
      use_file   <- input$use_csv_mode && !is.null(input$upload_file)

      if (use_file) {
        log_add("Loading data from uploaded file...")
        notes_data <- tryCatch(uploaded_data(), error = function(e) {
          log_add("✗ File load error:", e$message); NULL
        })
        if (!is.null(notes_data)) log_add(sprintf("✓ Loaded %d rows from file", nrow(notes_data)))
      } else {
        if (trimws(input$sql_query) == "") {
          log_add("✗ SQL query is empty. Aborting.")
          showNotification("SQL query is empty.", type = "error", duration = NULL)
          return()
        }
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
          log_add("✗ SQL error:", e$message); NULL
        })
        DBI::dbDisconnect(conn)
        if (!is.null(notes_data)) log_add(sprintf("✓ Query returned %d rows", nrow(notes_data)))
      }

      if (is.null(notes_data)) { log_add("✗ No data. Aborting."); return() }

      if (!("NOTE_TEXT" %in% names(notes_data))) {
        log_add("✗ NOTE_TEXT column missing.")
        showNotification("NOTE_TEXT column missing.", type = "error", duration = NULL)
        return()
      }

      # Initialize all diagnosis-specific output columns
      for (col in active_cols) {
        if (!(col %in% names(notes_data))) notes_data[[col]] <- NA_character_
      }

      # Store active diagnosis so results table knows which columns to show
      rv$active_diagnosis <- preset_key

      max_r  <- as.integer(input$max_rows)
      n_rows <- if (max_r > 0) min(nrow(notes_data), max_r) else nrow(notes_data)
      if (max_r > 0 && max_r < nrow(notes_data))
        log_add(sprintf("ℹ Max rows cap: processing %d of %d", n_rows, nrow(notes_data)))

      rv$progress_n <- n_rows
      rv$progress_i <- 0
      rv$start_time <- Sys.time()
      rv$notes_data <- notes_data

      log_add(sprintf("Starting extraction on %d rows...", n_rows))

      # 3. Row-by-row extraction
      for (i in seq_len(n_rows)) {

        if (rv$stop_flag) { log_add("⏹ Stopped by user at row", i); break }

        # Refresh GCP token every 200 rows (Vertex only)
        if (provider == "vertex" && i > 1 && i %% 200 == 0) {
          log_add("[INFO] Refreshing GCP token...")
          gcp <- check_gcp_auth()
          if (!gcp$ok) { log_add("✗ Token refresh failed."); break }
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

        success  <- FALSE
        attempts <- 0

        while (!success && attempts < 2) {
          attempts <- attempts + 1
          if (attempts > 1) log_add("  -> Retrying row...")

          api_result    <- call_llm(full_prompt, provider, rv$gcp_token, vertex_cfg, lm_cfg, active_schema)
          json_response <- api_result$text
          finish_reason <- api_result$reason
          err_detail    <- api_result$detail

          if (!is.na(json_response)) {
            clean_json <- clean_json_fence(json_response)

            # Parse in tryCatch — assignments happen OUTSIDE so scoping is never an issue
            parse_result <- tryCatch({
              list(ok = TRUE, parsed = jsonlite::fromJSON(clean_json))
            }, error = function(e) {
              list(ok = FALSE, msg = e$message)
            })

            if (parse_result$ok) {
              parsed <- parse_result$parsed
              for (col in active_cols) {
                val <- parsed[[col]]
                notes_data[[col]][i] <- if (is.null(val)) NA_character_ else as.character(val)
              }
              success <- TRUE
            } else {
              log_add(sprintf("  -> JSON parse error (attempt %d): %s", attempts, parse_result$msg))
              if (!is.null(finish_reason) && finish_reason != "STOP" && finish_reason != "stop")
                log_add(sprintf("     [API] finish_reason: %s", finish_reason))
            }
          } else if (identical(finish_reason, "NETWORK_CRASH")) {
            log_add("  -> Network error. Waiting 15s before retry...")
            Sys.sleep(15)
          } else if (identical(finish_reason, "HTTP_ERROR")) {
            short_err <- substr(gsub("\\s+", " ", ifelse(is.null(err_detail), "", err_detail)), 1, 350)
            log_add(sprintf("  -> HTTP error: %s", short_err))
          } else {
            log_add(sprintf("  -> No response. reason: %s", finish_reason))
          }

          if (input$sleep_secs > 0) Sys.sleep(input$sleep_secs)
        }

        rv$progress_i <- i
        incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d", i, n_rows))

        if (i %% input$autosave_every == 0) {
          autosave_path <- resolved_output_path()$autosave
          tryCatch(
            readr::write_csv(notes_data, autosave_path),
            error = function(e) log_add("  -> Autosave failed:", e$message)
          )
          log_add(sprintf("  -> Autosaved to %s at row %d", autosave_path, i))
          rv$notes_data <- notes_data
        }
      }

      rv$notes_data <- notes_data
      out_path <- resolved_output_path()
      tryCatch({
        if (!dir.exists(out_path$dir)) dir.create(out_path$dir, recursive = TRUE)
        readr::write_csv(notes_data, out_path$final)
        log_add(sprintf("✓ Done — saved to %s", out_path$final))
      }, error = function(e) {
        fallback <- paste0(out_path$base, ".csv")
        readr::write_csv(notes_data, fallback)
        log_add(sprintf("⚠ Could not write to target dir — saved to working dir: %s", fallback))
        log_add("  Error:", e$message)
      })
      log_add("═══ Pipeline complete ═══")
    })
  })
}

# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui = ui, server = server)