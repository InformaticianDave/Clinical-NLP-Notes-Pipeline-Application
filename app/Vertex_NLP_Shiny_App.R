# =============================================================================
# NLP Notes Pipeline — Shiny App
# Supports: Vertex AI (Gemini) or LM Studio (local OpenAI-compatible API)
#
# RELIABILITY GUARANTEE (v2):
#   Every processed row ALWAYS receives a non-NA `review_status` and, on any
#   failure, a human-readable `extract_error`. A row can never again come back
#   blank with no explanation. See ".review_status_core", "call_gemini"
#   (schema-free retry), and the terminal-status block in the pipeline loop.
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

# Shiny's default upload cap is 5 MB, which a note-text CSV/XLSX blows past almost
# immediately (full clinical notes are wide). Raise it to 500 MB. Adjust if your
# resume files are ever larger than that.
options(shiny.maxRequestSize = 500 * 1024^2)

# Small null-coalescing helper used by the Vertex fallback logic.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.character(a) && all(is.na(a))) return(b)
  a
}

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
  location = "global",
  model    = "gemini-2.5-flash"
)

# -----------------------------------------------------------------------------
# VERTEX MODEL CATALOG — drives the Model dropdown AND the Usage & Cost tab.
# Prices are USD per 1M tokens, standard (non-batch) tier, prompts <= 200K tokens
# (every NICU note is far below that). Checked Oct 2026 — re-verify periodically.
# -----------------------------------------------------------------------------
GEMINI_MODELS <- data.frame(
  id = c("gemini-4-argon", "gemini-3.8-flash", "gemini-3.7-flash", "gemini-3.6-flash",
         "gemini-3.5-flash", "gemini-3.5-flash-lite", "gemini-3.1-flash-lite",
         "gemini-3.1-pro-preview", "gemini-2.5-pro", "gemini-2.5-flash", "gemini-2.5-flash-lite"),
  price_in  = c(2.00, 0.75, 0.75, 0.75, 1.50, 0.30, 0.25, 2.00, 1.25, 0.30, 0.10),
  price_out = c(10.00, 3.75, 3.75, 3.75, 9.00, 2.50, 1.50, 12.00, 10.00, 2.50, 0.40),
  note = c(
    "Rises to $4.00 / $20.00 on Dec 1, 2026.",
    "Promotional rate through Dec 31, 2026; doubles to $1.50 / $7.50 on Jan 1, 2027.",
    "Deprecated Oct 2026 \u2014 requests route to gemini-3.8-flash.",
    "Promotional rate through Dec 31, 2026; doubles to $1.50 / $7.50 on Jan 1, 2027.",
    "Deprecated Oct 2026 \u2014 requests route to gemini-3.6-flash.",
    "",
    "",
    "Preview model. Prompts > 200K tokens: $4.00 / $18.00.",
    "Prompts > 200K tokens: $2.50 / $15.00.",
    "",
    ""
  ),
  stringsAsFactors = FALSE
)

model_pricing <- function(model_id) {
  hit <- GEMINI_MODELS[GEMINI_MODELS$id == model_id, , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  as.list(hit[1, ])
}

gemini_model_choices <- function() {
  stats::setNames(
    GEMINI_MODELS$id,
    sprintf("%s  \u00b7  $%.2f in / $%.2f out", GEMINI_MODELS$id, GEMINI_MODELS$price_in, GEMINI_MODELS$price_out)
  )
}

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
# =============================================================================
DIAGNOSIS_PRESETS <- list(

  NEC = list(
    label  = "NEC \u2014 Necrotizing Enterocolitis",
    target = "necrotizing enterocolitis (NEC)",
    present_field = "nec_present",
    present_true  = c("true"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing neonatal medical notes.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Determine whether this note AFFIRMATIVELY DOCUMENTS active Necrotizing Enterocolitis (NEC) in THIS patient, using the registry definition below.\n\n",
      "NEC DIAGNOSTIC CRITERIA:\n",
      "Answer Yes only if the infant had NEC diagnosed at surgery, at postmortem examination, or with BOTH:\n",
      "  Clinical signs (>=1): bilious gastric aspirate or emesis, abdominal distension or discoloration, occult or gross blood in stool (no fissure)\n",
      "  AND imaging findings (>=1): pneumatosis intestinalis, hepato-biliary (portal venous) gas, pneumoperitoneum\n\n",
      "EVIDENCE RULE (critical):\n",
      "- You MUST be able to quote ONE verbatim sentence from THIS note that affirmatively documents NEC in this patient.\n",
      "- Copy that sentence EXACTLY as it appears (character-for-character) into evidence_span. Do NOT paraphrase.\n",
      "- If you cannot find such a verbatim sentence, set nec_present=false and evidence_span=null.\n\n",
      "NEGATION & UNCERTAINTY (force to false):\n",
      "- Explicit denial (\"no pneumatosis\", \"NEC ruled out\", \"no evidence of NEC\") => nec_present=false, negated=true.\n",
      "- Hedged / equivocal language (\"possible\", \"concerning for\", \"cannot be excluded\", \"questionable\", \"equivocal\", \"r/o NEC\", \"vs\") => nec_present=false, confidence <= 0.4.\n",
      "- A mention in problem list, past history, HPI, differential, or carried-forward text is NOT an active diagnosis => nec_present=false unless THIS note affirms it.\n\n",
      "FIP EXCLUSION: If the infant was found at surgery/post-mortem to have focal intestinal perforation (FIP/SIP) rather than NEC => nec_present=false, fip_instead=true.\n\n",
      "FIELDS TO RETURN:\n",
      "- nec_present: true ONLY if affirmatively documented per the criteria AND a verbatim evidence_span exists.\n",
      "- nec_basis: \"surgical\", \"postmortem\", \"clinical_and_imaging\", or null if nec_present is false.\n",
      "- clinical_signs: documented clinical signs found (e.g. \"bilious aspirates, abdominal distension\"). Empty string if none.\n",
      "- imaging_findings: documented imaging findings found (e.g. \"pneumatosis intestinalis\"). Empty string if none.\n",
      "- fip_instead: true if focal intestinal perforation rather than NEC. false otherwise.\n",
      "- evidence_span: the EXACT verbatim sentence supporting nec_present=true, or null.\n",
      "- where_found: section the evidence came from (e.g. \"operative findings\", \"impression\", \"assessment/plan\", \"radiology impression\", \"problem list\", \"HPI/history\"), or null.\n",
      "- negated: true if NEC is explicitly ruled out or denied.\n",
      "- confidence: 0.0-1.0. Definitive surgical/imaging confirmation ~1.0; hedged <=0.4; 0.5 if truly ambiguous.\n\n",
      "EXAMPLES:\n",
      "Note: \"Abdominal XR shows extensive pneumatosis intestinalis; clinical picture consistent with NEC.\"\n",
      "JSON: {\"nec_present\":true,\"nec_basis\":\"clinical_and_imaging\",\"clinical_signs\":\"\",\"imaging_findings\":\"pneumatosis intestinalis\",\"fip_instead\":false,\"evidence_span\":\"Abdominal XR shows extensive pneumatosis intestinalis; clinical picture consistent with NEC.\",\"where_found\":\"radiology impression\",\"negated\":false,\"confidence\":0.95}\n",
      "Note: \"No pneumatosis or free air. NEC ruled out.\"\n",
      "JSON: {\"nec_present\":false,\"nec_basis\":null,\"clinical_signs\":\"\",\"imaging_findings\":\"\",\"fip_instead\":false,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.95}\n",
      "Note: \"Possible early pneumatosis, cannot be excluded; will repeat film.\"\n",
      "JSON: {\"nec_present\":false,\"nec_basis\":null,\"clinical_signs\":\"\",\"imaging_findings\":\"\",\"fip_instead\":false,\"evidence_span\":null,\"where_found\":null,\"negated\":false,\"confidence\":0.3}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        nec_present      = list(type = "BOOLEAN"),
        nec_basis        = list(type = "STRING"),
        clinical_signs   = list(type = "STRING"),
        imaging_findings = list(type = "STRING"),
        fip_instead      = list(type = "BOOLEAN"),
        evidence_span    = list(type = "STRING"),
        where_found      = list(type = "STRING"),
        negated          = list(type = "BOOLEAN"),
        confidence       = list(type = "NUMBER")
      )
    ),
    cols       = c("nec_present", "nec_basis", "clinical_signs", "imaging_findings", "fip_instead", "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("NEC Present", "Basis", "Clinical Signs", "Imaging Findings", "FIP Instead", "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  IVH = list(
    label  = "IVH \u2014 Periventricular-Intraventricular Hemorrhage",
    target = "periventricular-intraventricular hemorrhage (PIH/IVH)",
    present_field = "ivh_present",
    present_true  = c("true"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing neonatal radiology and clinical notes.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Determine the worst grade of Periventricular-Intraventricular Hemorrhage (PIH/IVH) AFFIRMATIVELY DOCUMENTED in THIS note.\n",
      "Scan ALL sections of the note: FINDINGS, IMPRESSION, ASSESSMENT/PLAN, and any free text. Do not stop at one section.\n\n",
      "GRADING CRITERIA (worst grade from any cranial ultrasound, CT, or MRI):\n",
      "  Grade 0: No subependymal or intraventricular hemorrhage\n",
      "  Grade 1: Germinal matrix hemorrhage (GMH) ONLY \u2014 no blood in the ventricle.\n",
      "    SYNONYMS for Grade 1 (all map to worst_grade=1, ivh_present=true):\n",
      "      germinal matrix hemorrhage (GMH), subependymal hemorrhage (SEH),\n",
      "      caudothalamic groove hemorrhage, subependymal bleed,\n",
      "      echogenic focus in the caudothalamic groove (CTG),\n",
      "      subependymal hematoma, periventricular germinal matrix bleed.\n",
      "    NOTE: \"without intraventricular extension\" confirms Grade 1, NOT Grade 0.\n",
      "    NOTE: \"no IVH\" after a GMH/SEH description means no blood INSIDE the ventricle,\n",
      "          which still confirms Grade 1 \u2014 do NOT set negated=true in that case.\n",
      "  Grade 2: Intraventricular blood, no ventricular dilation\n",
      "  Grade 3: Intraventricular blood WITH ventricular dilation\n",
      "  Grade 4: Intraparenchymal hemorrhage\n",
      "If multiple studies are referenced, record the most severe grade documented.\n\n",
      "FOLLOW-UP / SURVEILLANCE STUDIES (very important \u2014 these are common):\n",
      "Many cranial ultrasounds are serial monitoring scans on infants with KNOWN prior IVH.\n",
      "In these reports the radiologist commonly writes 'no NEW hemorrhage' or 'no ACUTE\n",
      "hemorrhage' while ALSO documenting that the prior bleed is still present.\n",
      "- 'No new hemorrhage', 'no new intracranial hemorrhage', 'no acute hemorrhage',\n",
      "  'no sonographic evidence of acute intracranial hemorrhage' mean NO ADDITIONAL bleed\n",
      "  today. They are NOT a denial of the existing hemorrhage.\n",
      "- If the note ALSO documents any of: 'resolving hemorrhage', 'decreased intraventricular\n",
      "  hemorrhage', 'stable intraventricular blood products', 'layering blood products',\n",
      "  'echogenic material within the ventricle', 'evolving IVH', 'residual hemorrhage',\n",
      "  'blood products', or a prior-bleed site \u2014 then ivh_present=true, negated=false,\n",
      "  and you MUST quote that sentence as evidence_span.\n",
      "- Grade the follow-up study on what is visible NOW: intraventricular blood WITH\n",
      "  enlarged/dilated ventricles or ventriculomegaly => worst_grade=3.\n",
      "- Set negated=true ONLY when the note documents NO hemorrhage of any kind, past or present.\n\n",
      "EVIDENCE RULE (critical):\n",
      "- Quote the single most informative verbatim sentence. For Grade 1 this may be a Findings\n",
      "  sentence (e.g. 'Echogenic focus in the right CTG') OR an Impression sentence\n",
      "  (e.g. 'Germinal matrix hemorrhage, right'). Either is sufficient \u2014 prefer whichever\n",
      "  is most explicit about the hemorrhage location/nature.\n",
      "- Copy it EXACTLY into evidence_span.\n",
      "- If no sentence in ANY section documents a hemorrhage, set ivh_present=false, worst_grade=null, evidence_span=null.\n\n",
      "NEGATION & UNCERTAINTY:\n",
      "- True negation (Grade 0): 'no intracranial hemorrhage', 'no IVH, no GMH', 'normal head ultrasound with no hemorrhagic foci' \u2014 with NO other hemorrhage mentioned anywhere.\n",
      "- 'No intraventricular extension' or 'without IVH' following a GMH/SEH description is NOT negation \u2014 it specifies Grade 1.\n",
      "- 'No new' / 'no acute' hemorrhage on a follow-up study is NOT negation \u2014 see FOLLOW-UP section above.\n",
      "- Hedged / equivocal ('possible', 'cannot exclude', 'questionable', 'concerning for') => ivh_present=false, confidence <= 0.4.\n",
      "- Problem-list / history / carried-forward mentions are NOT an active finding unless THIS note affirms imaging.\n\n",
      "FIELDS TO RETURN:\n",
      "- ivh_present: true if any grade >= 1 is affirmatively documented (with evidence_span), false if grade 0 or none.\n",
      "- worst_grade: integer 0-4. null if no grading information is present.\n",
      "- laterality: \"left\", \"right\", \"bilateral\", or null.\n",
      "- hydrocephalus: true if post-hemorrhagic hydrocephalus, ventriculomegaly, or ventricular dilation is documented.\n",
      "- imaging_modality: \"ultrasound\", \"MRI\", \"CT\", or null.\n",
      "- evidence_span: the EXACT verbatim sentence supporting the grade, or null.\n",
      "- where_found: section the evidence came from (\"findings\", \"impression\", \"assessment/plan\", \"problem list\"), or null.\n",
      "- is_followup: true if this is a surveillance/comparison study on a known prior hemorrhage (mentions a COMPARISON date, 'unchanged', 'stable', 'previously seen', 'resolving').\n",
      "- negated: true ONLY if hemorrhage of any kind (IVH or GMH), past or present, is explicitly denied throughout the note.\n",
      "- confidence: 0.0-1.0. Explicit grade or GMH synonym ~0.95; hedged <=0.4; 0.5 if ambiguous.\n\n",
      "EXAMPLES:\n",
      "Note: \"FINDINGS: Echogenic focus in the right caudothalamic groove without intraventricular extension. IMPRESSION: Germinal matrix hemorrhage, right.\"\n",
      "JSON: {\"ivh_present\":true,\"worst_grade\":1,\"laterality\":\"right\",\"hydrocephalus\":false,\"imaging_modality\":\"ultrasound\",\"evidence_span\":\"Germinal matrix hemorrhage, right.\",\"where_found\":\"impression\",\"is_followup\":false,\"negated\":false,\"confidence\":0.95}\n",
      "Note: \"FINDINGS: Small SEH in the left CTG, no IVH. IMPRESSION: Small left subependymal hemorrhage without intraventricular extension.\"\n",
      "JSON: {\"ivh_present\":true,\"worst_grade\":1,\"laterality\":\"left\",\"hydrocephalus\":false,\"imaging_modality\":\"ultrasound\",\"evidence_span\":\"Small left subependymal hemorrhage without intraventricular extension.\",\"where_found\":\"impression\",\"is_followup\":false,\"negated\":false,\"confidence\":0.95}\n",
      "Note: \"Head US: bilateral grade 3 IVH with ventricular dilation.\"\n",
      "JSON: {\"ivh_present\":true,\"worst_grade\":3,\"laterality\":\"bilateral\",\"hydrocephalus\":true,\"imaging_modality\":\"ultrasound\",\"evidence_span\":\"Head US: bilateral grade 3 IVH with ventricular dilation.\",\"where_found\":\"impression\",\"is_followup\":false,\"negated\":false,\"confidence\":0.97}\n",
      "Note: \"COMPARISON: 05/06/2024. FINDINGS: There is no sonographic evidence of acute intracranial hemorrhage. Previously seen echogenic material within the left occipital horn appears decreased in conspicuity, consistent with resolving hemorrhage. Lateral ventricles again noted to be enlarged. IMPRESSION: 1. Unchanged ventriculomegaly 2. Decreased intraventricular hemorrhage in the left lateral ventricle 3. No sonographic evidence of acute intracranial hemorrhage\"\n",
      "JSON: {\"ivh_present\":true,\"worst_grade\":3,\"laterality\":\"left\",\"hydrocephalus\":true,\"imaging_modality\":\"ultrasound\",\"evidence_span\":\"Decreased intraventricular hemorrhage in the left lateral ventricle\",\"where_found\":\"impression\",\"is_followup\":true,\"negated\":false,\"confidence\":0.93}\n",
      "Note: \"COMPARISON: 04/01/2024. FINDINGS: Enlarged lateral ventricles, unchanged. Layering intraventricular blood products in the bilateral occipital horns are unchanged. No new intracranial hemorrhage. IMPRESSION: 1. Stable ventriculomegaly. 2. Stable intraventricular blood products. No new hemorrhage.\"\n",
      "JSON: {\"ivh_present\":true,\"worst_grade\":3,\"laterality\":\"bilateral\",\"hydrocephalus\":true,\"imaging_modality\":\"ultrasound\",\"evidence_span\":\"Layering intraventricular blood products in the bilateral occipital horns are unchanged.\",\"where_found\":\"findings\",\"is_followup\":true,\"negated\":false,\"confidence\":0.94}\n",
      "Note: \"Cranial ultrasound within normal limits. No hemorrhagic foci. No GMH or IVH.\"\n",
      "JSON: {\"ivh_present\":false,\"worst_grade\":0,\"laterality\":null,\"hydrocephalus\":false,\"imaging_modality\":\"ultrasound\",\"evidence_span\":null,\"where_found\":null,\"is_followup\":false,\"negated\":true,\"confidence\":0.97}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        ivh_present      = list(type = "BOOLEAN"),
        worst_grade      = list(type = "NUMBER"),
        laterality       = list(type = "STRING"),
        hydrocephalus    = list(type = "BOOLEAN"),
        imaging_modality = list(type = "STRING"),
        evidence_span    = list(type = "STRING"),
        where_found      = list(type = "STRING"),
        is_followup      = list(type = "BOOLEAN"),
        negated          = list(type = "BOOLEAN"),
        confidence       = list(type = "NUMBER")
      )
    ),
    cols       = c("ivh_present", "worst_grade", "laterality", "hydrocephalus", "imaging_modality", "evidence_span", "where_found", "is_followup", "negated", "confidence"),
    col_labels = c("IVH Present", "Worst Grade", "Laterality", "Hydrocephalus", "Imaging Modality", "Evidence Span", "Where Found", "Follow-up Study", "Negated", "Confidence")
  ),

  PVL = list(
    label  = "PVL \u2014 Cystic Periventricular Leukomalacia",
    target = "cystic periventricular leukomalacia (cPVL)",
    present_field = "cpvl_result",
    present_true  = c("yes"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing neonatal radiology and clinical notes.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Determine whether this note AFFIRMATIVELY DOCUMENTS cystic periventricular leukomalacia (cPVL).\n\n",
      "DIAGNOSTIC CRITERIA:\n",
      "  Yes: MULTIPLE SMALL PERIVENTRICULAR CYSTS OR Multiple PERIVENTRICULAR CYSTS identified on cranial ultrasound, CT, or MRI.\n",
      "  No: No evidence of cystic PVL on cranial imaging, and at least one imaging study was done.\n",
      "  N/A: No cranial imaging study (ultrasound, CT, or MRI) was ever done.\n\n",
      "EXCLUSIONS (do NOT code as cPVL):\n",
      "  - Periventricular echogenicity WITHOUT cysts.\n",
      "  - Porencephalic cyst in the area of previously identified intraparenchymal hemorrhage.\n",
      "  - Periventricular abnormality on CT/MRI unless multiple small periventricular cysts are identified.\n\n",
      "EVIDENCE RULE (critical):\n",
      "- For cpvl_result=\"yes\" you MUST quote ONE verbatim sentence documenting the cysts. Copy it EXACTLY into evidence_span.\n",
      "- If no such verbatim sentence exists, cpvl_result is \"no\" or \"na\" and evidence_span=null.\n\n",
      "NEGATION & UNCERTAINTY:\n",
      "- Explicit denial (\"no PVL\", \"no cystic changes\") => cpvl_result=\"no\", negated=true.\n",
      "- Hedged / equivocal (\"possible\", \"cannot exclude\", \"early/evolving\", \"questionable\") => cpvl_result=\"no\", confidence <= 0.4.\n",
      "- Echogenicity without cysts => cpvl_result=\"no\", echogenicity_only=true.\n\n",
      "FIELDS TO RETURN:\n",
      "- cpvl_result: \"yes\", \"no\", or \"na\".\n",
      "- cysts_documented: true if multiple small periventricular cysts are explicitly described. false otherwise.\n",
      "- echogenicity_only: true if periventricular echogenicity is noted WITHOUT cysts (this is NOT cPVL).\n",
      "- porencephalic_cyst: true if a porencephalic cyst from prior hemorrhage is noted (this is NOT cPVL).\n",
      "- imaging_modality: \"ultrasound\", \"MRI\", \"CT\", or null.\n",
      "- evidence_span: the EXACT verbatim sentence supporting cpvl_result=\"yes\", or null.\n",
      "- where_found: section the evidence came from (e.g. \"radiology impression\", \"findings\"), or null.\n",
      "- negated: true if PVL is explicitly ruled out.\n",
      "- confidence: 0.0-1.0. Explicit cysts ~1.0; hedged <=0.4; 0.5 if ambiguous.\n\n",
      "EXAMPLES:\n",
      "Note: \"MRI: multiple small cysts in the periventricular white matter, consistent with cystic PVL.\"\n",
      "JSON: {\"cpvl_result\":\"yes\",\"cysts_documented\":true,\"echogenicity_only\":false,\"porencephalic_cyst\":false,\"imaging_modality\":\"MRI\",\"evidence_span\":\"MRI: multiple small cysts in the periventricular white matter, consistent with cystic PVL.\",\"where_found\":\"radiology impression\",\"negated\":false,\"confidence\":0.95}\n",
      "Note: \"Increased periventricular echogenicity, no cysts identified.\"\n",
      "JSON: {\"cpvl_result\":\"no\",\"cysts_documented\":false,\"echogenicity_only\":true,\"porencephalic_cyst\":false,\"imaging_modality\":\"ultrasound\",\"evidence_span\":null,\"where_found\":null,\"negated\":false,\"confidence\":0.9}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        cpvl_result        = list(type = "STRING"),
        cysts_documented   = list(type = "BOOLEAN"),
        echogenicity_only  = list(type = "BOOLEAN"),
        porencephalic_cyst = list(type = "BOOLEAN"),
        imaging_modality   = list(type = "STRING"),
        evidence_span      = list(type = "STRING"),
        where_found        = list(type = "STRING"),
        negated            = list(type = "BOOLEAN"),
        confidence         = list(type = "NUMBER")
      )
    ),
    cols       = c("cpvl_result", "cysts_documented", "echogenicity_only", "porencephalic_cyst", "imaging_modality", "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("cPVL Result", "Cysts Documented", "Echogenicity Only", "Porencephalic Cyst", "Imaging Modality", "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  # ---------------------------------------------------------------------------
  # COMBINED: IVH + PVL from one cranial-imaging report (single Vertex pass).
  # ---------------------------------------------------------------------------
  IVH_PVL = list(
    label  = "IVH + PVL \u2014 Cranial Imaging (combined)",
    target = "IVH grade and cystic PVL from a single cranial imaging report",
    present_field = "ivh_present",
    present_true  = c("true"),
    review_specs = list(
      list(out = "ivh_review_status",  present_field = "ivh_present", present_true = c("true"),
           negated_field = "ivh_negated",  conf_field = "ivh_confidence"),
      list(out = "cpvl_review_status", present_field = "cpvl_result", present_true = c("yes"),
           negated_field = "cpvl_negated", conf_field = "cpvl_confidence")
    ),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing a SINGLE neonatal cranial imaging report\n",
      "(head ultrasound, brain MRI, or CT). Return ONLY valid JSON. No markdown, no preamble.\n\n",
      "TASK: From THIS ONE report, assess TWO conditions INDEPENDENTLY:\n",
      "  (A) Periventricular-Intraventricular Hemorrhage (IVH)\n",
      "  (B) Cystic Periventricular Leukomalacia (cPVL)\n",
      "A report can be positive for one and negative/absent for the other, or normal for both.\n",
      "Do NOT let a finding for one condition imply the other. Judge each on its own evidence.\n\n",
      "=== (A) IVH GRADING (worst grade on this study) ===\n",
      "  Grade 0: no subependymal or intraventricular hemorrhage\n",
      "  Grade 1: Germinal matrix hemorrhage (GMH) ONLY \u2014 no blood in the ventricle.\n",
      "    SYNONYMS for Grade 1 (all map to ivh_worst_grade=1, ivh_present=true):\n",
      "      germinal matrix hemorrhage (GMH), subependymal hemorrhage (SEH),\n",
      "      caudothalamic groove hemorrhage, subependymal bleed,\n",
      "      echogenic focus in the caudothalamic groove (CTG), subependymal hematoma.\n",
      "    NOTE: 'without intraventricular extension' confirms Grade 1, NOT Grade 0.\n",
      "    NOTE: 'no IVH' after a GMH/SEH description means no intraventricular blood,\n",
      "          still Grade 1 \u2014 do NOT set ivh_negated=true in that case.\n",
      "  Grade 2: intraventricular blood, no ventricular dilation\n",
      "  Grade 3: intraventricular blood WITH ventricular dilation\n",
      "  Grade 4: intraparenchymal hemorrhage\n\n",
      "FOLLOW-UP / SURVEILLANCE STUDIES (common \u2014 handle carefully):\n",
      "- 'No new hemorrhage' / 'no acute hemorrhage' / 'no sonographic evidence of acute\n",
      "  intracranial hemorrhage' mean no ADDITIONAL bleed today, NOT absence of hemorrhage.\n",
      "- If the report also documents 'resolving hemorrhage', 'decreased intraventricular\n",
      "  hemorrhage', 'stable intraventricular blood products', 'layering blood products', or\n",
      "  'echogenic material within the ventricle' => ivh_present=true, ivh_negated=false, and\n",
      "  quote that sentence into ivh_evidence_span.\n",
      "- Intraventricular blood WITH ventriculomegaly/enlarged ventricles => ivh_worst_grade=3.\n",
      "- Set ivh_negated=true ONLY when no hemorrhage of any kind, past or present, is documented.\n\n",
      "=== (B) cPVL ===\n",
      "  yes: MULTIPLE SMALL PERIVENTRICULAR CYSTS identified.\n",
      "  no : imaging done, but no cystic PVL.\n",
      "  na : no cranial imaging described.\n",
      "  Do NOT code echogenicity-without-cysts or a porencephalic cyst (from prior bleed) as cPVL.\n\n",
      "EVIDENCE RULE (applies to BOTH, independently):\n",
      "- For ivh_present=true you MUST copy ONE exact verbatim sentence supporting the grade into ivh_evidence_span.\n",
      "- For cpvl_result=\"yes\" you MUST copy ONE exact verbatim sentence supporting the cysts into cpvl_evidence_span.\n",
      "- If you cannot quote a supporting sentence, that condition is negative and its evidence_span is null.\n\n",
      "NEGATION & UNCERTAINTY (per condition):\n",
      "- IVH ivh_negated=true ONLY when hemorrhage of any kind is denied throughout: 'no IVH, no GMH', 'no hemorrhagic foci', 'normal without hemorrhage'.\n",
      "- 'No IVH' or 'no intraventricular extension' appearing AFTER a GMH/SEH description is NOT negation \u2014 it specifies Grade 1. Set ivh_negated=false.\n",
      "- 'No new' / 'no acute' hemorrhage on a follow-up study is NOT negation \u2014 see FOLLOW-UP section.\n",
      "- PVL cpvl_negated=true: 'no PVL', 'no cystic changes', 'no periventricular cysts'.\n",
      "- Hedged language ('possible', 'cannot exclude', 'questionable', 'early/evolving') => that condition negative, its *_confidence <= 0.4.\n",
      "- A normal/unremarkable study sets study_normal=true and drives BOTH conditions negative unless a specific finding is quoted.\n\n",
      "FIELDS TO RETURN:\n",
      "  imaging_modality: \"ultrasound\", \"MRI\", \"CT\", or null.\n",
      "  study_normal: true if the study is explicitly normal/unremarkable.\n",
      "  is_followup: true if this is a surveillance/comparison study on known prior findings.\n",
      "  -- IVH --\n",
      "  ivh_present: true if IVH grade >= 1 is affirmatively documented (with ivh_evidence_span), else false.\n",
      "  ivh_worst_grade: integer 0-4, or null if no grading info.\n",
      "  ivh_laterality: \"left\", \"right\", \"bilateral\", or null.\n",
      "  ivh_hydrocephalus: true if post-hemorrhagic hydrocephalus / ventriculomegaly / ventricular dilation.\n",
      "  ivh_evidence_span: exact verbatim sentence supporting IVH, or null.\n",
      "  ivh_where_found: section it came from (e.g. \"findings\", \"impression\"), or null.\n",
      "  ivh_negated: true if IVH explicitly ruled out.\n",
      "  ivh_confidence: 0.0-1.0.\n",
      "  -- cPVL --\n",
      "  cpvl_result: \"yes\", \"no\", or \"na\".\n",
      "  pvl_cysts_documented: true if multiple small periventricular cysts explicitly described.\n",
      "  pvl_echogenicity_only: true if periventricular echogenicity WITHOUT cysts.\n",
      "  pvl_porencephalic_cyst: true if a porencephalic cyst from prior hemorrhage is noted.\n",
      "  cpvl_evidence_span: exact verbatim sentence supporting cPVL, or null.\n",
      "  cpvl_where_found: section it came from, or null.\n",
      "  cpvl_negated: true if PVL explicitly ruled out.\n",
      "  cpvl_confidence: 0.0-1.0.\n",
      "  -- shared --\n",
      "  other_findings: brief note of other notable findings (e.g. ventriculomegaly without hemorrhage), or null.\n\n",
      "EXAMPLES:\n",
      "Report: \"Head US: left grade 2 IVH. No periventricular cysts.\"\n",
      "JSON: {\"imaging_modality\":\"ultrasound\",\"study_normal\":false,\"is_followup\":false,\"ivh_present\":true,\"ivh_worst_grade\":2,\"ivh_laterality\":\"left\",\"ivh_hydrocephalus\":false,\"ivh_evidence_span\":\"Head US: left grade 2 IVH.\",\"ivh_where_found\":\"impression\",\"ivh_negated\":false,\"ivh_confidence\":0.95,\"cpvl_result\":\"no\",\"pvl_cysts_documented\":false,\"pvl_echogenicity_only\":false,\"pvl_porencephalic_cyst\":false,\"cpvl_evidence_span\":null,\"cpvl_where_found\":null,\"cpvl_negated\":true,\"cpvl_confidence\":0.9,\"other_findings\":null}\n",
      "Report: \"COMPARISON: 04/01/2024. Layering intraventricular blood products in the bilateral occipital horns are unchanged. Enlarged lateral ventricles unchanged. No new intracranial hemorrhage. IMPRESSION: Stable ventriculomegaly. Stable intraventricular blood products.\"\n",
      "JSON: {\"imaging_modality\":\"ultrasound\",\"study_normal\":false,\"is_followup\":true,\"ivh_present\":true,\"ivh_worst_grade\":3,\"ivh_laterality\":\"bilateral\",\"ivh_hydrocephalus\":true,\"ivh_evidence_span\":\"Layering intraventricular blood products in the bilateral occipital horns are unchanged.\",\"ivh_where_found\":\"findings\",\"ivh_negated\":false,\"ivh_confidence\":0.94,\"cpvl_result\":\"no\",\"pvl_cysts_documented\":false,\"pvl_echogenicity_only\":false,\"pvl_porencephalic_cyst\":false,\"cpvl_evidence_span\":null,\"cpvl_where_found\":null,\"cpvl_negated\":false,\"cpvl_confidence\":0.85,\"other_findings\":\"stable ventriculomegaly\"}\n",
      "Report: \"Normal cranial ultrasound. No hemorrhage, no cystic change.\"\n",
      "JSON: {\"imaging_modality\":\"ultrasound\",\"study_normal\":true,\"is_followup\":false,\"ivh_present\":false,\"ivh_worst_grade\":0,\"ivh_laterality\":null,\"ivh_hydrocephalus\":false,\"ivh_evidence_span\":null,\"ivh_where_found\":null,\"ivh_negated\":true,\"ivh_confidence\":0.95,\"cpvl_result\":\"no\",\"pvl_cysts_documented\":false,\"pvl_echogenicity_only\":false,\"pvl_porencephalic_cyst\":false,\"cpvl_evidence_span\":null,\"cpvl_where_found\":null,\"cpvl_negated\":true,\"cpvl_confidence\":0.95,\"other_findings\":null}\n",
      "Report: \"MRI: multiple small periventricular cysts consistent with cystic PVL. No acute hemorrhage.\"\n",
      "JSON: {\"imaging_modality\":\"MRI\",\"study_normal\":false,\"is_followup\":false,\"ivh_present\":false,\"ivh_worst_grade\":0,\"ivh_laterality\":null,\"ivh_hydrocephalus\":false,\"ivh_evidence_span\":null,\"ivh_where_found\":null,\"ivh_negated\":true,\"ivh_confidence\":0.92,\"cpvl_result\":\"yes\",\"pvl_cysts_documented\":true,\"pvl_echogenicity_only\":false,\"pvl_porencephalic_cyst\":false,\"cpvl_evidence_span\":\"MRI: multiple small periventricular cysts consistent with cystic PVL.\",\"cpvl_where_found\":\"impression\",\"cpvl_negated\":false,\"cpvl_confidence\":0.95,\"other_findings\":null}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        imaging_modality       = list(type = "STRING"),
        study_normal           = list(type = "BOOLEAN"),
        is_followup            = list(type = "BOOLEAN"),
        ivh_present            = list(type = "BOOLEAN"),
        ivh_worst_grade        = list(type = "NUMBER"),
        ivh_laterality         = list(type = "STRING"),
        ivh_hydrocephalus      = list(type = "BOOLEAN"),
        ivh_evidence_span      = list(type = "STRING"),
        ivh_where_found        = list(type = "STRING"),
        ivh_negated            = list(type = "BOOLEAN"),
        ivh_confidence         = list(type = "NUMBER"),
        cpvl_result            = list(type = "STRING"),
        pvl_cysts_documented   = list(type = "BOOLEAN"),
        pvl_echogenicity_only  = list(type = "BOOLEAN"),
        pvl_porencephalic_cyst = list(type = "BOOLEAN"),
        cpvl_evidence_span     = list(type = "STRING"),
        cpvl_where_found       = list(type = "STRING"),
        cpvl_negated           = list(type = "BOOLEAN"),
        cpvl_confidence        = list(type = "NUMBER"),
        other_findings         = list(type = "STRING")
      )
    ),
    cols = c("ivh_present", "ivh_worst_grade", "ivh_laterality", "ivh_hydrocephalus",
             "ivh_evidence_span", "ivh_where_found", "ivh_negated", "ivh_confidence",
             "cpvl_result", "pvl_cysts_documented", "pvl_echogenicity_only", "pvl_porencephalic_cyst",
             "cpvl_evidence_span", "cpvl_where_found", "cpvl_negated", "cpvl_confidence",
             "imaging_modality", "study_normal", "is_followup", "other_findings"),
    col_labels = c("IVH Present", "IVH Grade", "IVH Laterality", "Hydrocephalus",
                   "IVH Evidence", "IVH Where", "IVH Negated", "IVH Conf",
                   "cPVL Result", "PVL Cysts", "Echogenicity Only", "Porencephalic Cyst",
                   "cPVL Evidence", "cPVL Where", "cPVL Negated", "cPVL Conf",
                   "Imaging Modality", "Study Normal", "Follow-up Study", "Other Findings")
  ),

  ROP = list(
    label  = "ROP \u2014 Retinopathy of Prematurity",
    target = "retinopathy of prematurity (ROP)",
    present_field = "rop_present",
    present_true  = c("true"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing neonatal ophthalmology and clinical notes.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Determine the worst stage of Retinopathy of Prematurity (ROP) AFFIRMATIVELY DOCUMENTED in THIS note.\n\n",
      "ROP STAGING (worst stage in the eye with the most advanced disease):\n",
      "  Stage 0: No evidence of ROP (immature/mature retina, no ROP)\n",
      "  Stage 1: Demarcation line (+/- abnormal vascularization)\n",
      "  Stage 2: Intraretinal ridge\n",
      "  Stage 3: Ridge with extraretinal fibrovascular proliferation\n",
      "  Stage 4: Partial retinal detachment\n",
      "  Stage 5: Total retinal detachment\n",
      "If multiple exams are referenced, record the most severe stage from any exam.\n\n",
      "EVIDENCE RULE (critical):\n",
      "- You MUST quote ONE verbatim sentence documenting the stage/finding. Copy it EXACTLY into evidence_span.\n",
      "- If no such verbatim sentence exists, set rop_present=false, worst_stage=null, evidence_span=null.\n\n",
      "NEGATION & UNCERTAINTY:\n",
      "- Explicit absence (\"no ROP\", \"immature retina without ROP\", \"mature retina\") => rop_present=false, worst_stage=0, negated=true.\n",
      "- Hedged / equivocal (\"possible\", \"cannot exclude\", \"questionable\", \"early\") => rop_present=false, confidence <= 0.4.\n",
      "- Screening-scheduling or history mentions are NOT an active diagnosis.\n\n",
      "FIELDS TO RETURN:\n",
      "- rop_present: true if any stage >= 1 is affirmatively documented (with evidence_span), false otherwise.\n",
      "- worst_stage: integer 0-5. null if no staging information is present.\n",
      "- zone: \"I\", \"II\", \"III\", or null.\n",
      "- plus_disease: true if plus or pre-plus disease is documented.\n",
      "- treatment: \"laser\", \"bevacizumab\", \"both\", \"surgery\", \"observation\", or null.\n",
      "- laterality: \"left\" (OS), \"right\" (OD), \"bilateral\" (OU), or null.\n",
      "- evidence_span: the EXACT verbatim sentence supporting the stage, or null.\n",
      "- where_found: section the evidence came from (e.g. \"eye exam impression\", \"assessment/plan\"), or null.\n",
      "- negated: true if ROP is explicitly ruled out.\n",
      "- confidence: 0.0-1.0. Explicit stage ~1.0; hedged <=0.4; 0.5 if ambiguous.\n\n",
      "EXAMPLES:\n",
      "Note: \"OU: Stage 2, zone II, no plus disease.\"\n",
      "JSON: {\"rop_present\":true,\"worst_stage\":2,\"zone\":\"II\",\"plus_disease\":false,\"treatment\":null,\"laterality\":\"bilateral\",\"evidence_span\":\"OU: Stage 2, zone II, no plus disease.\",\"where_found\":\"eye exam impression\",\"negated\":false,\"confidence\":0.96}\n",
      "Note: \"Immature retina zone II, no ROP. Follow up in 2 weeks.\"\n",
      "JSON: {\"rop_present\":false,\"worst_stage\":0,\"zone\":\"II\",\"plus_disease\":false,\"treatment\":null,\"laterality\":null,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.95}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        rop_present   = list(type = "BOOLEAN"),
        worst_stage   = list(type = "NUMBER"),
        zone          = list(type = "STRING"),
        plus_disease  = list(type = "BOOLEAN"),
        treatment     = list(type = "STRING"),
        laterality    = list(type = "STRING"),
        evidence_span = list(type = "STRING"),
        where_found   = list(type = "STRING"),
        negated       = list(type = "BOOLEAN"),
        confidence    = list(type = "NUMBER")
      )
    ),
    cols       = c("rop_present", "worst_stage", "zone", "plus_disease", "treatment", "laterality", "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("ROP Present", "Worst Stage", "Zone", "Plus Disease", "Treatment", "Laterality", "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  # ---------------------------------------------------------------------------
  # PDA — Patent Ductus Arteriosus (Echo + CXR combined, single pipeline pass)
  # Input file must have a column named 'pda_tier' with values "1" or "2".
  # ---------------------------------------------------------------------------
  PDA = list(
    label        = "PDA \u2014 Echo + CXR (Combined)",
    target       = "patent ductus arteriosus (PDA): echo and chest X-ray combined",
    tier_column  = "pda_tier",
    present_field = "pda_present",
    present_true  = c("true"),
    review_specs = list(
      list(out = "pda_echo_status", present_field = "pda_present",
           present_true = c("true"), negated_field = "negated", conf_field = "confidence"),
      list(out = "pda_cxr_status",  present_field = "cxr_supports_pda",
           present_true = c("true"), negated_field = "negated", conf_field = "confidence")
    ),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing a neonatal clinical report.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "REPORT TYPE: {TIER}\n\n",
      "Based on the report type above, extract the fields relevant to that type.\n",
      "Fields that do not apply to this report type MUST be returned as null.\n\n",
      "=================================================================\n",
      "IF TIER 1 (Echocardiogram / TTE) \u2014 extract PDA echo fields:\n",
      "=================================================================\n",
      "VON TIER 1 DEFINITION: PDA confirmed if left-to-right OR bidirectional ductal\n",
      "shunting is documented on Doppler echo. Right-to-left shunting alone may indicate\n",
      "pulmonary hypertension rather than a simple PDA.\n\n",
      "TIER 1 EVIDENCE RULE:\n",
      "- Quote ONE verbatim sentence documenting ductal shunting into evidence_span.\n",
      "- If no such sentence exists, pda_present=false and evidence_span=null.\n\n",
      "TIER 1 NEGATION:\n",
      "- 'No PDA', 'ductus closed', 'no ductal shunting' => pda_present=false, negated=true.\n",
      "- 'Previously known PDA, now closed' => pda_present=false, spontaneous_closure=true, negated=true.\n",
      "- 'Small PDA, not hemodynamically significant' is STILL pda_present=true (set hemodynamic_significance='small').\n",
      "- Hedged ('possible', 'cannot exclude', 'equivocal') => pda_present=false, confidence <= 0.4.\n\n",
      "HEMODYNAMIC SIGNIFICANCE levels: silent / small / moderate / large / hemodynamically_significant\n",
      "  hemodynamically_significant (hsPDA): LA:Ao > 1.4, ventricular dilation, or clinical compromise.\n\n",
      "=================================================================\n",
      "IF TIER 2 (Chest X-Ray / CXR) \u2014 extract CXR support fields only:\n",
      "=================================================================\n",
      "Extract ONLY: cardiomegaly and increased pulmonary vasculature (VON Tier 2 criteria).\n",
      "These findings CANNOT confirm PDA alone. All echo fields must be null for CXR rows.\n\n",
      "TIER 2 EVIDENCE RULE:\n",
      "- Quote the most informative sentence covering cardiomegaly/pulmonary vasculature.\n",
      "- If neither finding is present, evidence_span=null.\n\n",
      "TIER 2 NEGATION:\n",
      "- 'Heart size normal', 'no cardiomegaly' => cardiomegaly=false.\n",
      "- 'Normal pulmonary vascularity', 'clear lung fields' => increased_pulmonary_vasculature=false.\n",
      "- 'Borderline', 'mild', 'possible' => code true but confidence <= 0.6.\n\n",
      "=================================================================\n",
      "FIELDS TO RETURN (null for fields not applicable to this report type):\n",
      "=================================================================\n",
      "  report_type: 'echo' if this is a TTE/echocardiogram, 'cxr' if chest X-ray.\n",
      "  -- TIER 1 (ECHO) FIELDS \u2014 null for CXR rows --\n",
      "  pda_present: true if PDA confirmed per VON Tier 1 (with evidence_span). false/null for CXR.\n",
      "  shunt_direction: 'left_to_right', 'right_to_left', 'bidirectional', 'not_detected', or null.\n",
      "  hemodynamic_significance: 'silent','small','moderate','large','hemodynamically_significant', or null.\n",
      "  ductal_diameter_mm: numeric diameter in mm if stated, else null.\n",
      "  la_ao_ratio: LA:Ao ratio as a number if stated, else null.\n",
      "  treatment_recommended: 'indomethacin','acetaminophen','ibuprofen','surgical_ligation',\n",
      "      'catheter_closure','observation', or null.\n",
      "  spontaneous_closure: true if this echo documents the ductus has now closed. null for CXR.\n",
      "  -- TIER 2 (CXR) FIELDS \u2014 null for echo rows --\n",
      "  cardiomegaly: true if cardiomegaly or enlarged cardiac silhouette documented. null for echo.\n",
      "  increased_pulmonary_vasculature: true if increased/prominent pulmonary vasculature. null for echo.\n",
      "  pulmonary_edema: true if pulmonary edema or interstitial edema documented. null for echo.\n",
      "  normal_cardiac_silhouette: true if heart size explicitly normal. null for echo.\n",
      "  cxr_supports_pda: true if EITHER cardiomegaly OR increased_pulmonary_vasculature is true. null for echo.\n",
      "  -- SHARED FIELDS --\n",
      "  evidence_span: EXACT verbatim sentence supporting the positive finding, or null.\n",
      "  where_found: section the evidence came from ('impression','findings','doppler findings'), or null.\n",
      "  negated: true if the key finding (ductal shunting for echo; cardiomegaly for CXR) is explicitly denied.\n",
      "  confidence: 0.0-1.0. Explicit finding ~0.95; hedged/borderline <=0.6; equivocal <=0.4.\n\n",
      "EXAMPLES:\n",
      "TIER 1 \u2014 hsPDA echo:\n",
      "Report: \"Doppler shows left-to-right ductal shunting. LA:Ao 1.8, hemodynamically significant PDA. Indomethacin recommended.\"\n",
      "JSON: {\"report_type\":\"echo\",\"pda_present\":true,\"shunt_direction\":\"left_to_right\",\"hemodynamic_significance\":\"hemodynamically_significant\",\"ductal_diameter_mm\":null,\"la_ao_ratio\":1.8,\"treatment_recommended\":\"indomethacin\",\"spontaneous_closure\":false,\"cardiomegaly\":null,\"increased_pulmonary_vasculature\":null,\"pulmonary_edema\":null,\"normal_cardiac_silhouette\":null,\"cxr_supports_pda\":null,\"evidence_span\":\"Doppler shows left-to-right ductal shunting.\",\"where_found\":\"doppler findings\",\"negated\":false,\"confidence\":0.97}\n",
      "TIER 1 \u2014 small PDA, no treatment:\n",
      "Report: \"Small PDA with left-to-right flow. Not hemodynamically significant.\"\n",
      "JSON: {\"report_type\":\"echo\",\"pda_present\":true,\"shunt_direction\":\"left_to_right\",\"hemodynamic_significance\":\"small\",\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":\"observation\",\"spontaneous_closure\":false,\"cardiomegaly\":null,\"increased_pulmonary_vasculature\":null,\"pulmonary_edema\":null,\"normal_cardiac_silhouette\":null,\"cxr_supports_pda\":null,\"evidence_span\":\"Small PDA with left-to-right flow.\",\"where_found\":\"impression\",\"negated\":false,\"confidence\":0.95}\n",
      "TIER 1 \u2014 no PDA:\n",
      "Report: \"No patent ductus arteriosus. Ductus appears closed.\"\n",
      "JSON: {\"report_type\":\"echo\",\"pda_present\":false,\"shunt_direction\":\"not_detected\",\"hemodynamic_significance\":null,\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":null,\"spontaneous_closure\":false,\"cardiomegaly\":null,\"increased_pulmonary_vasculature\":null,\"pulmonary_edema\":null,\"normal_cardiac_silhouette\":null,\"cxr_supports_pda\":null,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.97}\n",
      "TIER 2 \u2014 CXR positive:\n",
      "Report: \"IMPRESSION: Cardiomegaly with increased pulmonary vascular markings.\"\n",
      "JSON: {\"report_type\":\"cxr\",\"pda_present\":null,\"shunt_direction\":null,\"hemodynamic_significance\":null,\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":null,\"spontaneous_closure\":null,\"cardiomegaly\":true,\"increased_pulmonary_vasculature\":true,\"pulmonary_edema\":false,\"normal_cardiac_silhouette\":false,\"cxr_supports_pda\":true,\"evidence_span\":\"Cardiomegaly with increased pulmonary vascular markings.\",\"where_found\":\"impression\",\"negated\":false,\"confidence\":0.96}\n",
      "TIER 2 \u2014 CXR negative:\n",
      "Report: \"Heart size normal. Lung fields clear. No vascular congestion.\"\n",
      "JSON: {\"report_type\":\"cxr\",\"pda_present\":null,\"shunt_direction\":null,\"hemodynamic_significance\":null,\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":null,\"spontaneous_closure\":null,\"cardiomegaly\":false,\"increased_pulmonary_vasculature\":false,\"pulmonary_edema\":false,\"normal_cardiac_silhouette\":true,\"cxr_supports_pda\":false,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.96}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        report_type                     = list(type = "STRING"),
        pda_present                     = list(type = "BOOLEAN"),
        shunt_direction                 = list(type = "STRING"),
        hemodynamic_significance        = list(type = "STRING"),
        ductal_diameter_mm              = list(type = "NUMBER"),
        la_ao_ratio                     = list(type = "NUMBER"),
        treatment_recommended           = list(type = "STRING"),
        spontaneous_closure             = list(type = "BOOLEAN"),
        cardiomegaly                    = list(type = "BOOLEAN"),
        increased_pulmonary_vasculature = list(type = "BOOLEAN"),
        pulmonary_edema                 = list(type = "BOOLEAN"),
        normal_cardiac_silhouette       = list(type = "BOOLEAN"),
        cxr_supports_pda                = list(type = "BOOLEAN"),
        evidence_span                   = list(type = "STRING"),
        where_found                     = list(type = "STRING"),
        negated                         = list(type = "BOOLEAN"),
        confidence                      = list(type = "NUMBER")
      )
    ),
    cols = c("report_type",
             "pda_present", "shunt_direction", "hemodynamic_significance",
             "ductal_diameter_mm", "la_ao_ratio", "treatment_recommended", "spontaneous_closure",
             "cardiomegaly", "increased_pulmonary_vasculature", "pulmonary_edema",
             "normal_cardiac_silhouette", "cxr_supports_pda",
             "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("Report Type",
                   "PDA Present", "Shunt Direction", "Hemodynamic Significance",
                   "Diameter (mm)", "LA:Ao Ratio", "Treatment Recommended", "Spontaneous Closure",
                   "Cardiomegaly", "Increased Pulm Vasculature", "Pulmonary Edema",
                   "Normal Cardiac Silhouette", "CXR Supports PDA",
                   "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  # (PDA_Echo and PDA_CXR kept as legacy single-type presets.)
  PDA_Echo = list(
    label  = "PDA \u2014 Echo (TTE)",
    target = "patent ductus arteriosus (PDA) on echocardiogram",
    present_field = "pda_present",
    present_true  = c("true"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing a neonatal echocardiogram (TTE) report.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Determine whether this echo report AFFIRMATIVELY DOCUMENTS a Patent Ductus Arteriosus (PDA)\n",
      "using the Vermont Oxford Network (VON) Tier 1 definition.\n\n",
      "VON TIER 1 DEFINITION (echo-based, sufficient on its own):\n",
      "  PDA confirmed if: left-to-right ductal shunting OR bidirectional ductal shunting on Doppler echo.\n",
      "  A right-to-left shunt may indicate pulmonary hypertension rather than a simple PDA.\n\n",
      "EVIDENCE RULE (critical):\n",
      "- You MUST quote ONE verbatim sentence documenting the ductal finding. Copy it EXACTLY into evidence_span.\n",
      "- If no such sentence exists, set pda_present=false and evidence_span=null.\n\n",
      "NEGATION & UNCERTAINTY:\n",
      "- 'No PDA', 'ductus closed', 'no ductal shunting', 'PDA not identified' => pda_present=false, negated=true.\n",
      "- 'Previously known PDA, now closed on this study' => pda_present=false, spontaneous_closure=true, negated=true.\n",
      "- Hedged ('possible', 'cannot exclude', 'equivocal', 'questionable') => pda_present=false, confidence <= 0.4.\n",
      "- 'Small PDA, not hemodynamically significant' IS still pda_present=true \u2014 code the size, set hemodynamic_significance='small'.\n",
      "- A prior note mentioning PDA is NOT evidence \u2014 this study must document it independently.\n\n",
      "HEMODYNAMIC SIGNIFICANCE:\n",
      "  silent: no clinical signs, incidental finding.\n",
      "  small: left-to-right flow, no significant shunt physiology.\n",
      "  moderate: some evidence of volume loading but not severe.\n",
      "  large: significant left-to-right shunting.\n",
      "  hemodynamically_significant (hsPDA): LA:Ao > 1.4, ventricular dilation, or clinical compromise attributed to PDA.\n\n",
      "FIELDS TO RETURN:\n",
      "- pda_present: true if PDA documented per VON Tier 1 (with evidence_span). false otherwise.\n",
      "- shunt_direction: 'left_to_right', 'right_to_left', 'bidirectional', 'not_detected', or null.\n",
      "- hemodynamic_significance: 'silent', 'small', 'moderate', 'large', 'hemodynamically_significant', or null.\n",
      "- ductal_diameter_mm: numeric ductal diameter in mm if stated, or null.\n",
      "- la_ao_ratio: LA:Ao ratio as a number if stated, or null.\n",
      "- treatment_recommended: 'indomethacin', 'acetaminophen', 'ibuprofen', 'surgical_ligation',\n",
      "    'catheter_closure', 'observation', or null. Use the first mentioned if multiple.\n",
      "- spontaneous_closure: true if this echo explicitly documents the ductus has closed.\n",
      "- evidence_span: EXACT verbatim sentence supporting pda_present=true, or null.\n",
      "- where_found: section evidence came from ('impression', 'findings', 'doppler findings', 'assessment'), or null.\n",
      "- negated: true if PDA is explicitly absent or closed on this study.\n",
      "- confidence: 0.0-1.0. Explicit ductal shunting ~0.97; hedged <=0.4; 0.5 if ambiguous.\n\n",
      "EXAMPLES:\n",
      "Report: \"Doppler interrogation demonstrates left-to-right ductal shunting. Hemodynamically significant PDA with LA:Ao ratio 1.7. Indomethacin recommended.\"\n",
      "JSON: {\"pda_present\":true,\"shunt_direction\":\"left_to_right\",\"hemodynamic_significance\":\"hemodynamically_significant\",\"ductal_diameter_mm\":null,\"la_ao_ratio\":1.7,\"treatment_recommended\":\"indomethacin\",\"spontaneous_closure\":false,\"evidence_span\":\"Doppler interrogation demonstrates left-to-right ductal shunting.\",\"where_found\":\"doppler findings\",\"negated\":false,\"confidence\":0.97}\n",
      "Report: \"Small PDA with left-to-right flow identified. Not hemodynamically significant. No treatment required at this time.\"\n",
      "JSON: {\"pda_present\":true,\"shunt_direction\":\"left_to_right\",\"hemodynamic_significance\":\"small\",\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":\"observation\",\"spontaneous_closure\":false,\"evidence_span\":\"Small PDA with left-to-right flow identified.\",\"where_found\":\"impression\",\"negated\":false,\"confidence\":0.95}\n",
      "Report: \"No patent ductus arteriosus identified. Ductus appears closed.\"\n",
      "JSON: {\"pda_present\":false,\"shunt_direction\":\"not_detected\",\"hemodynamic_significance\":null,\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":null,\"spontaneous_closure\":false,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.97}\n",
      "Report: \"Previously known PDA; on this study the ductus is now closed, no residual shunting.\"\n",
      "JSON: {\"pda_present\":false,\"shunt_direction\":\"not_detected\",\"hemodynamic_significance\":null,\"ductal_diameter_mm\":null,\"la_ao_ratio\":null,\"treatment_recommended\":null,\"spontaneous_closure\":true,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.96}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        pda_present              = list(type = "BOOLEAN"),
        shunt_direction          = list(type = "STRING"),
        hemodynamic_significance = list(type = "STRING"),
        ductal_diameter_mm       = list(type = "NUMBER"),
        la_ao_ratio              = list(type = "NUMBER"),
        treatment_recommended    = list(type = "STRING"),
        spontaneous_closure      = list(type = "BOOLEAN"),
        evidence_span            = list(type = "STRING"),
        where_found              = list(type = "STRING"),
        negated                  = list(type = "BOOLEAN"),
        confidence               = list(type = "NUMBER")
      )
    ),
    cols       = c("pda_present", "shunt_direction", "hemodynamic_significance", "ductal_diameter_mm",
                   "la_ao_ratio", "treatment_recommended", "spontaneous_closure",
                   "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("PDA Present", "Shunt Direction", "Hemodynamic Significance", "Diameter (mm)",
                   "LA:Ao Ratio", "Treatment Recommended", "Spontaneous Closure",
                   "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  PDA_CXR = list(
    label  = "PDA \u2014 CXR Support (Tier 2)",
    target = "cardiomegaly or increased pulmonary vasculature on chest radiograph",
    present_field = "cxr_supports_pda",
    present_true  = c("true"),
    prompt_template = paste0(
      "You are a clinical NLP assistant reviewing a neonatal chest radiograph (CXR) report.\n",
      "Return ONLY valid JSON. No markdown, no preamble, no explanation.\n\n",
      "TASK: Extract TWO specific findings from this chest X-ray report that are used as\n",
      "Vermont Oxford Network (VON) Tier 2 criteria for PDA: cardiomegaly and increased\n",
      "pulmonary vasculature. These findings ALONE cannot diagnose PDA but contribute to\n",
      "clinical PDA criteria when combined with a murmur and other signs.\n\n",
      "Also note pulmonary edema if present, as it may indicate hemodynamic compromise.\n\n",
      "EVIDENCE RULE:\n",
      "- For each positive finding, quote the verbatim sentence that supports it into evidence_span.\n",
      "- If BOTH cardiomegaly AND increased pulmonary vasculature are present, quote the most\n",
      "  informative sentence covering both (or the impression sentence if it summarizes both).\n",
      "- If neither is present, evidence_span=null.\n\n",
      "NEGATION:\n",
      "- 'Normal cardiac silhouette', 'heart size normal', 'no cardiomegaly' => cardiomegaly=false, negated=true.\n",
      "- 'Normal pulmonary vascularity', 'clear lung fields' => increased_pulmonary_vasculature=false.\n",
      "- Hedged ('possible', 'mild', 'borderline') => code as true but confidence <= 0.6.\n\n",
      "FIELDS TO RETURN:\n",
      "- cardiomegaly: true if cardiomegaly or enlarged cardiac silhouette is documented.\n",
      "- increased_pulmonary_vasculature: true if increased, prominent, or engorged pulmonary vasculature is documented.\n",
      "- pulmonary_edema: true if pulmonary edema, interstitial edema, or pulmonary congestion is documented.\n",
      "- normal_cardiac_silhouette: true if heart size is explicitly normal.\n",
      "- cxr_supports_pda: true if EITHER cardiomegaly OR increased_pulmonary_vasculature is true.\n",
      "- evidence_span: EXACT verbatim sentence supporting positive findings, or null if none.\n",
      "- where_found: section evidence came from ('impression', 'findings'), or null.\n",
      "- negated: true if cardiomegaly is explicitly denied AND pulmonary vasculature is explicitly normal.\n",
      "- confidence: 0.0-1.0. Explicit cardiomegaly or vascular prominence ~0.95; borderline/mild <=0.6.\n\n",
      "EXAMPLES:\n",
      "Report: \"IMPRESSION: Cardiomegaly with increased pulmonary vascular markings, consistent with left-to-right shunt physiology.\"\n",
      "JSON: {\"cardiomegaly\":true,\"increased_pulmonary_vasculature\":true,\"pulmonary_edema\":false,\"normal_cardiac_silhouette\":false,\"cxr_supports_pda\":true,\"evidence_span\":\"Cardiomegaly with increased pulmonary vascular markings, consistent with left-to-right shunt physiology.\",\"where_found\":\"impression\",\"negated\":false,\"confidence\":0.96}\n",
      "Report: \"Heart size normal. Lung fields clear. No pulmonary vascular congestion.\"\n",
      "JSON: {\"cardiomegaly\":false,\"increased_pulmonary_vasculature\":false,\"pulmonary_edema\":false,\"normal_cardiac_silhouette\":true,\"cxr_supports_pda\":false,\"evidence_span\":null,\"where_found\":null,\"negated\":true,\"confidence\":0.96}\n",
      "Report: \"Mildly prominent pulmonary vasculature. Heart size borderline enlarged.\"\n",
      "JSON: {\"cardiomegaly\":true,\"increased_pulmonary_vasculature\":true,\"pulmonary_edema\":false,\"normal_cardiac_silhouette\":false,\"cxr_supports_pda\":true,\"evidence_span\":\"Mildly prominent pulmonary vasculature. Heart size borderline enlarged.\",\"where_found\":\"findings\",\"negated\":false,\"confidence\":0.58}\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        cardiomegaly                    = list(type = "BOOLEAN"),
        increased_pulmonary_vasculature = list(type = "BOOLEAN"),
        pulmonary_edema                 = list(type = "BOOLEAN"),
        normal_cardiac_silhouette       = list(type = "BOOLEAN"),
        cxr_supports_pda                = list(type = "BOOLEAN"),
        evidence_span                   = list(type = "STRING"),
        where_found                     = list(type = "STRING"),
        negated                         = list(type = "BOOLEAN"),
        confidence                      = list(type = "NUMBER")
      )
    ),
    cols       = c("cxr_supports_pda", "cardiomegaly", "increased_pulmonary_vasculature",
                   "pulmonary_edema", "normal_cardiac_silhouette",
                   "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("CXR Supports PDA", "Cardiomegaly", "Increased Pulm Vasculature",
                   "Pulmonary Edema", "Normal Cardiac Silhouette",
                   "Evidence Span", "Where Found", "Negated", "Confidence")
  ),

  custom = list(
    label  = "Custom",
    target = DEFAULT_PROMPT_TARGET,
    present_field = "target_present",
    present_true  = c("true"),
    prompt_template = paste0(
      "Determine whether the following note text AFFIRMATIVELY DOCUMENTS: {TARGET}\n",
      "Return ONLY valid JSON matching the schema. No preamble, no markdown fences.\n\n",
      "Rules:\n",
      "- target_present: true ONLY if clearly documented as present in THIS patient (not boilerplate, not history, not denied).\n",
      "- You MUST quote ONE verbatim sentence supporting target_present=true into evidence_span (exact copy). If none exists, target_present=false and evidence_span=null.\n",
      "- negated: true if explicitly denied (e.g. 'no X', 'X ruled out'). If negated=true, target_present must be false.\n",
      "- Hedged / equivocal ('possible', 'cannot exclude', 'concerning for') => target_present=false, confidence <= 0.4.\n",
      "- target_terms: short supporting keywords/phrases, or null.\n",
      "- where_found: section the evidence came from, or null.\n",
      "- confidence: number 0-1.\n\n",
      "Text:\n\"%s\"\n"
    ),
    schema = list(
      type = "OBJECT",
      properties = list(
        target_present = list(type = "BOOLEAN"),
        target_terms   = list(type = "STRING"),
        evidence_span  = list(type = "STRING"),
        where_found    = list(type = "STRING"),
        negated        = list(type = "BOOLEAN"),
        confidence     = list(type = "NUMBER")
      )
    ),
    cols       = c("target_present", "target_terms", "evidence_span", "where_found", "negated", "confidence"),
    col_labels = c("Present", "Terms Found", "Evidence Span", "Where Found", "Negated", "Confidence")
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
#
# HARDENED (v2) — this is the fix for silently-NA rows:
#   1. maxOutputTokens raised 2048 -> 8192. Long surveillance reports were
#      hitting the old cap mid-JSON, producing unparseable truncated output.
#   2. Schema-free retry. Vertex intermittently suppresses a schema-constrained
#      response on notes with mixed affirm/deny language (returns an empty
#      candidate). The fallback re-asks WITHOUT responseSchema; repair_json +
#      fromJSON handle the free-form result.
#   3. Every failure path now returns a SPECIFIC reason string instead of the
#      catch-all "UNKNOWN", so the log and the extract_error column tell you
#      exactly what happened.
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
  # The "global" endpoint is NOT region-prefixed (aiplatform.googleapis.com);
  # every other location IS region-prefixed ({location}-aiplatform.googleapis.com).
  # Using the regional hostname with location = "global" in the path (or vice
  # versa) returns a 404 NOT_FOUND on publishers/google/models/..., which looks
  # exactly like "wrong model name" but is actually "wrong endpoint shape".
  endpoint <- if (identical(ai_cfg$location, "global")) {
    sprintf(
      "https://aiplatform.googleapis.com/v1/projects/%s/locations/global/publishers/google/models/%s:generateContent",
      ai_cfg$project, ai_cfg$model
    )
  } else {
    sprintf(
      "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
      ai_cfg$location, ai_cfg$project, ai_cfg$location, ai_cfg$model
    )
  }

  make_body <- function(use_schema) {
    b <- list(
      contents = list(
        list(role = "user", parts = list(list(text = prompt)))
      ),
      generationConfig = list(
        temperature      = 0.0,
        maxOutputTokens  = 8192,
        responseMimeType = "application/json"
      ),
      safetySettings = list(
        list(category = "HARM_CATEGORY_HARASSMENT",        threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_HATE_SPEECH",       threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
        list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
      )
    )
    if (use_schema) b$generationConfig$responseSchema <- schema
    b
  }

  do_call <- function(body) {
    tryCatch({
      httr2::request(endpoint) |>
        httr2::req_auth_bearer_token(token) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(180) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    }, error = function(e) NULL)
  }

  # Pull token-usage counts out of a Vertex response, when present.
  extract_usage <- function(result) {
    um <- result$usageMetadata
    if (is.null(um)) return(NULL)
    list(
      input_tokens  = um$promptTokenCount %||% NA_real_,
      output_tokens = sum(c(um$candidatesTokenCount, um$thoughtsTokenCount), na.rm = TRUE),
      thought_tokens = um$thoughtsTokenCount %||% 0,
      total_tokens  = um$totalTokenCount %||% NA_real_
    )
  }

  extract <- function(response) {
    if (is.null(response))
      return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL, usage = NULL))

    status <- httr2::resp_status(response)
    if (status >= 400) {
      err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
      return(list(text = NA_character_,
                  reason = sprintf("HTTP_ERROR_%d", status),
                  detail = err_body, usage = NULL))
    }

    result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
    if (is.null(result))
      return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON", detail = NULL, usage = NULL))

    usage <- extract_usage(result)

    # Prompt-level block (safety filter on the input itself)
    if (!is.null(result$promptFeedback) && !is.null(result$promptFeedback$blockReason))
      return(list(text = NA_character_,
                  reason = paste0("PROMPT_BLOCKED_", result$promptFeedback$blockReason),
                  detail = NULL, usage = usage))

    if (is.null(result$candidates) || length(result$candidates) == 0)
      return(list(text = NA_character_, reason = "NO_CANDIDATES", detail = NULL, usage = usage))

    candidate     <- result$candidates[[1]]
    finish_reason <- candidate$finishReason %||% "NO_FINISH_REASON"
    parts         <- candidate$content$parts

    if (!is.null(parts) && length(parts) > 0 && !is.null(parts[[1]]$text)) {
      txt <- parts[[1]]$text
      if (nzchar(trimws(txt)))
        return(list(text = txt, reason = finish_reason, detail = NULL, usage = usage))
    }

    # Candidate returned but carried no usable text — report WHY.
    list(text = NA_character_,
         reason = paste0("EMPTY_CANDIDATE_", finish_reason),
         detail = NULL, usage = usage)
  }

  # Attempt 1: schema-constrained (preferred; gives typed output)
  r1 <- extract(do_call(make_body(TRUE)))
  if (!is.na(r1$text)) return(r1)

  # Attempt 2: schema-free fallback. Vertex sometimes suppresses the constrained
  # response on complex notes; free-form output still parses after repair_json.
  r2 <- extract(do_call(make_body(FALSE)))
  if (!is.na(r2$text)) {
    r2$reason <- paste0(r2$reason, "+SCHEMA_FALLBACK")
    return(r2)
  }

  # Both failed — surface the most informative reason. Keep whichever attempt
  # actually reported usage (a failed parse can still have billed tokens).
  list(text   = NA_character_,
       reason = paste0(r1$reason, " | fallback: ", r2$reason),
       detail = r1$detail %||% r2$detail,
       usage  = r1$usage %||% r2$usage)
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
    return(list(ok = FALSE, models = NULL, msg = "Cannot reach LM Studio \u2014 is it running?"))

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

  # BioMistral rejects system role — fold instructions into user message
  combined_prompt <- paste0(
    "You are a clinical NLP assistant. ",
    "Always respond with valid JSON only. ",
    "No markdown fences, no preamble, no explanation \u2014 raw JSON only.\n\n",
    prompt
  )

  body <- list(
    model    = model,
    messages = list(
      list(role = "user", content = combined_prompt)
    ),
    temperature = 0.1,
    max_tokens  = 4096
  )

  response <- tryCatch({
    httr2::request(endpoint) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(300) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(response))
    return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL))

  if (httr2::resp_status(response) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_,
                reason = sprintf("HTTP_ERROR_%d", httr2::resp_status(response)),
                detail = err_body))
  }

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
  if (is.null(result))
    return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON", detail = NULL, usage = NULL))

  usage <- NULL
  if (!is.null(result$usage)) {
    usage <- list(
      input_tokens  = result$usage$prompt_tokens %||% NA_real_,
      output_tokens = result$usage$completion_tokens %||% NA_real_,
      total_tokens  = result$usage$total_tokens %||% NA_real_
    )
  }

  if (!is.null(result$choices) && length(result$choices) > 0) {
    content <- result$choices[[1]]$message$content
    if (!is.null(content) && nchar(trimws(content)) > 0)
      return(list(text = content, reason = "stop", detail = NULL, usage = usage))
    return(list(text = NA_character_, reason = "EMPTY_CHOICE_CONTENT", detail = NULL, usage = usage))
  }

  list(text = NA_character_, reason = "NO_CHOICES", detail = NULL, usage = usage)
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

# =============================================================================
# CHAT TAB — free-form, multi-turn, no response schema.
#
# Separate from call_gemini()/call_lmstudio() above on purpose: those force
# responseMimeType = "application/json" (and, for Vertex, a responseSchema),
# which is exactly wrong for a conversation where the model should answer in
# plain prose. These two functions take the full turn history (list of
# list(role = "user"/"assistant", text = ...)) and send it as one multi-turn
# request each time, since neither API keeps server-side conversation state.
# =============================================================================

call_gemini_chat <- function(history, token, ai_cfg) {
  endpoint <- if (identical(ai_cfg$location, "global")) {
    sprintf(
      "https://aiplatform.googleapis.com/v1/projects/%s/locations/global/publishers/google/models/%s:generateContent",
      ai_cfg$project, ai_cfg$model
    )
  } else {
    sprintf(
      "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
      ai_cfg$location, ai_cfg$project, ai_cfg$location, ai_cfg$model
    )
  }

  # Vertex's chat role for the model's own turns is "model", not "assistant".
  contents <- lapply(history, function(turn) {
    list(role = if (identical(turn$role, "user")) "user" else "model",
         parts = list(list(text = turn$text)))
  })

  body <- list(
    contents = contents,
    generationConfig = list(temperature = 0.7, maxOutputTokens = 4096),
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
    return(list(text = NA_character_, reason = "NETWORK_CRASH"))

  status <- httr2::resp_status(response)
  if (status >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_, reason = sprintf("HTTP_ERROR_%d: %s", status, err_body)))
  }

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
  if (is.null(result))
    return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON"))

  if (!is.null(result$promptFeedback) && !is.null(result$promptFeedback$blockReason))
    return(list(text = NA_character_, reason = paste0("PROMPT_BLOCKED_", result$promptFeedback$blockReason)))

  if (is.null(result$candidates) || length(result$candidates) == 0)
    return(list(text = NA_character_, reason = "NO_CANDIDATES"))

  candidate <- result$candidates[[1]]
  parts     <- candidate$content$parts
  if (!is.null(parts) && length(parts) > 0 && !is.null(parts[[1]]$text)) {
    txt <- parts[[1]]$text
    if (nzchar(trimws(txt))) return(list(text = txt, reason = "ok"))
  }
  list(text = NA_character_, reason = paste0("EMPTY_CANDIDATE_", candidate$finishReason %||% "?"))
}

call_lmstudio_chat <- function(history, base_url, model) {
  endpoint <- paste0(lm_base_url_clean(base_url), "/chat/completions")

  messages <- lapply(history, function(turn) {
    list(role = if (identical(turn$role, "user")) "user" else "assistant", content = turn$text)
  })

  body <- list(model = model, messages = messages, temperature = 0.7, max_tokens = 2048)

  response <- tryCatch({
    httr2::request(endpoint) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(180) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(response))
    return(list(text = NA_character_, reason = "NETWORK_CRASH"))

  if (httr2::resp_status(response) >= 400) {
    err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    return(list(text = NA_character_,
                reason = sprintf("HTTP_ERROR_%d: %s", httr2::resp_status(response), err_body)))
  }

  result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
  if (is.null(result))
    return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON"))

  if (!is.null(result$choices) && length(result$choices) > 0) {
    content <- result$choices[[1]]$message$content
    if (!is.null(content) && nchar(trimws(content)) > 0)
      return(list(text = content, reason = "ok"))
  }
  list(text = NA_character_, reason = "NO_CHOICES")
}

# =============================================================================
# HARDWARE MONITORING — best-effort, for the Usage & Cost tab's live GPU/CPU
# view. Everything here is read from the machine running the Shiny app itself,
# via nvidia-smi and OS-specific system calls. It only means something when
# the active backend is LM Studio (local): a Vertex AI call runs entirely in
# Google's datacenter and this machine's GPU sits idle for it. Every read is
# wrapped so a missing tool or a locked-down machine degrades to NA rather
# than crashing the tab.
# =============================================================================

get_gpu_stats <- function() {
  nv <- Sys.which("nvidia-smi")
  if (!nzchar(nv)) return(NULL)
  out <- tryCatch(
    system2(nv, c("--query-gpu=utilization.gpu,memory.used,memory.total,temperature.gpu,power.draw,power.limit,name",
                  "--format=csv,noheader,nounits"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) == 0 || !nzchar(out[1])) return(NULL)
  parts <- trimws(strsplit(out[1], ",")[[1]])
  if (length(parts) < 7) return(NULL)
  list(
    gpu_util_pct  = suppressWarnings(as.numeric(parts[1])),
    vram_used_mb  = suppressWarnings(as.numeric(parts[2])),
    vram_total_mb = suppressWarnings(as.numeric(parts[3])),
    temp_c        = suppressWarnings(as.numeric(parts[4])),
    power_w       = suppressWarnings(as.numeric(parts[5])),
    power_limit_w = suppressWarnings(as.numeric(parts[6])),
    name          = parts[7]
  )
}

get_sys_stats <- function() {
  sysname <- Sys.info()[["sysname"]]
  cores   <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  cpu_pct <- NA_real_; ram_used_gb <- NA_real_; ram_total_gb <- NA_real_

  if (identical(sysname, "Linux")) {
    meminfo <- tryCatch(readLines("/proc/meminfo"), error = function(e) character(0))
    get_kb <- function(key) {
      ln <- grep(paste0("^", key, ":"), meminfo, value = TRUE)
      if (length(ln) == 0) return(NA_real_)
      as.numeric(regmatches(ln, regexpr("[0-9]+", ln)))
    }
    total_kb <- get_kb("MemTotal"); avail_kb <- get_kb("MemAvailable")
    if (!is.na(total_kb) && !is.na(avail_kb)) {
      ram_total_gb <- total_kb / 1024^2
      ram_used_gb  <- (total_kb - avail_kb) / 1024^2
    }
    load1 <- tryCatch(as.numeric(strsplit(readLines("/proc/loadavg")[1], " ")[[1]][1]),
                      error = function(e) NA_real_)
    if (!is.na(load1) && !is.na(cores) && cores > 0) cpu_pct <- min(100, 100 * load1 / cores)
  } else if (identical(sysname, "Darwin")) {
    cpu_pct <- tryCatch({
      top_out  <- system("top -l 1 -n 0", intern = TRUE)
      idle_ln  <- grep("CPU usage", top_out, value = TRUE)
      idle_pct <- as.numeric(sub(".*?([0-9.]+)% idle.*", "\\1", idle_ln))
      if (length(idle_pct) == 1 && !is.na(idle_pct)) 100 - idle_pct else NA_real_
    }, error = function(e) NA_real_)
    ram_total_gb <- tryCatch(
      suppressWarnings(as.numeric(system("sysctl -n hw.memsize", intern = TRUE))) / 1024^3,
      error = function(e) NA_real_)
  } else if (identical(sysname, "Windows")) {
    cpu_pct <- tryCatch({
      v <- system("wmic cpu get loadpercentage /value", intern = TRUE)
      v <- grep("LoadPercentage=", v, value = TRUE)
      suppressWarnings(as.numeric(sub("LoadPercentage=", "", v)))
    }, error = function(e) NA_real_)
    mem_out <- tryCatch(
      system("wmic OS get FreePhysicalMemory,TotalVisibleMemorySize /value", intern = TRUE),
      error = function(e) character(0))
    free_kb  <- suppressWarnings(as.numeric(sub("FreePhysicalMemory=", "",
                  grep("FreePhysicalMemory=", mem_out, value = TRUE))))
    total_kb <- suppressWarnings(as.numeric(sub("TotalVisibleMemorySize=", "",
                  grep("TotalVisibleMemorySize=", mem_out, value = TRUE))))
    if (length(free_kb) == 1 && length(total_kb) == 1 && !is.na(free_kb) && !is.na(total_kb)) {
      ram_total_gb <- total_kb / 1024^2
      ram_used_gb  <- (total_kb - free_kb) / 1024^2
    }
  }

  list(sysname = sysname, cores = cores, cpu_pct = cpu_pct,
       ram_used_gb = ram_used_gb, ram_total_gb = ram_total_gb)
}

clean_json_fence <- function(x) {
  if (is.null(x)) return("")
  if (length(x) == 0) return("")
  if (is.na(x)) return("")
  x <- trimws(x)
  x <- gsub("^```json\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^```\\s*",     "", x, ignore.case = TRUE)
  x <- gsub("\\s*```$",     "", x, ignore.case = TRUE)
  # Some models prepend prose before the object — keep from the first brace.
  first_brace <- regexpr("\\{", x)
  if (first_brace > 1) x <- substr(x, first_brace, nchar(x))
  trimws(x)
}

repair_json <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.na(x) || nchar(trimws(x)) == 0) return(x)
  x <- trimws(x)
  # Drop a dangling trailing comma before closing, a common truncation artifact.
  x <- gsub(",\\s*$", "", x)
  open_braces    <- nchar(gsub("[^{]", "", x))
  close_braces   <- nchar(gsub("[^}]", "", x))
  open_brackets  <- nchar(gsub("[^[]", "", x))
  close_brackets <- nchar(gsub("[^]]", "", x))
  missing_brackets <- open_brackets - close_brackets
  missing_braces   <- open_braces  - close_braces
  if (missing_brackets > 0) x <- paste0(x, paste(rep("]", missing_brackets), collapse = ""))
  if (missing_braces   > 0) x <- paste0(x, paste(rep("}", missing_braces),   collapse = ""))
  x
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
# MODEL A/B COMPARISON — SCORING HELPERS
#
# These power the "Model A/B" tab, which scores two runs of the SAME notes by
# two different models against each other.
#
# A NOTE ON WHAT CAN AND CANNOT BE MEASURED HERE:
#   Precision, recall and F1 are defined against a GROUND TRUTH. Two model
#   outputs alone do not contain one. Comparing model B to model A measures
#   AGREEMENT, not accuracy — if both models are wrong in the same way, they
#   agree perfectly and every metric looks excellent. The tab therefore
#   defaults to agreement statistics (Cohen's kappa, McNemar) and only reports
#   precision/recall when the user explicitly nominates a reference, with the
#   reference named in the output so the number is never mistaken for
#   validated accuracy.
#
#   The quality audit below is the exception: hallucinated quotes, parse
#   failures and schema violations are objectively checkable from the file
#   itself with NO gold standard, because they are internal-consistency
#   failures rather than clinical judgments.
# =============================================================================

ab_norm <- function(x) tolower(trimws(as.character(x)))

# Treat empty strings and the usual null spellings as "no value", not as a level.
ab_blank <- function(x) {
  v <- tolower(trimws(as.character(x)))
  is.na(x) | is.na(v) | !nzchar(v) | v %in% c("na", "null", "n/a", "none", "nan")
}

# Map a column to TRUE/FALSE/NA given the strings that count as positive.
ab_to_logical <- function(x, positive_vals) {
  v <- ab_norm(x)
  out <- v %in% ab_norm(positive_vals)
  out[ab_blank(x)] <- NA
  out
}

# Wilson score interval — correct at small n and near 0/1, where the normal
# approximation produces impossible bounds.
ab_wilson_ci <- function(k, n, conf = 0.95) {
  if (is.na(k) || is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  phat <- k / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(max(0, center - half), min(1, center + half))
}

ab_fmt_rate <- function(k, n) {
  if (is.na(k) || is.na(n) || n == 0) return("\u2014")
  ci <- ab_wilson_ci(k, n)
  sprintf("%.3f  [%.3f\u2013%.3f]", k / n, ci[1], ci[2])
}

ab_fmt_num <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
  formatC(x, format = "f", digits = digits)
}

# Full binary confusion matrix. `gold` is whatever the user nominated as the
# reference; it is not necessarily a clinical gold standard.
ab_binary_metrics <- function(gold, pred) {
  keep <- !is.na(gold) & !is.na(pred)
  g <- gold[keep]; p <- pred[keep]
  TP <- sum(g & p); FP <- sum(!g & p); FN <- sum(g & !p); TN <- sum(!g & !p)
  N <- TP + FP + FN + TN
  prec <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  rec  <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
  f1 <- NA_real_
  if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) {
    f1 <- 2 * prec * rec / (prec + rec)
  }
  list(
    N = N, TP = TP, FP = FP, FN = FN, TN = TN,
    precision   = prec,
    recall      = rec,
    f1          = f1,
    specificity = if ((TN + FP) > 0) TN / (TN + FP) else NA_real_,
    npv         = if ((TN + FN) > 0) TN / (TN + FN) else NA_real_,
    accuracy    = if (N > 0) (TP + TN) / N else NA_real_
  )
}

# Cohen's kappa, general categorical form (works for binary too).
# Chance-corrected: with a rare finding, two models can agree on 95% of rows
# purely because both call almost everything negative. Kappa strips that out.
ab_cohen_kappa <- function(a, b) {
  keep <- !is.na(a) & !is.na(b)
  a <- as.character(a)[keep]; b <- as.character(b)[keep]
  n <- length(a)
  if (n == 0) return(NA_real_)
  lv <- union(unique(a), unique(b))
  if (length(lv) < 2) return(NA_real_)
  po <- sum(a == b) / n
  pe <- sum(vapply(lv, function(l) (sum(a == l) / n) * (sum(b == l) / n), numeric(1)))
  if (!is.finite(pe) || pe >= 1) return(NA_real_)
  (po - pe) / (1 - pe)
}

# Exact McNemar test on the discordant pairs. This is the right paired test for
# "do these two models differ on the same notes" — the concordant rows carry no
# information about a difference. Exact binomial is used at every n, so there is
# no chi-square approximation to defend at small discordant counts.
ab_mcnemar <- function(a, b) {
  keep <- !is.na(a) & !is.na(b)
  a <- a[keep]; b <- b[keep]
  a_only <- sum(a & !b)
  b_only <- sum(!a & b)
  nd <- a_only + b_only
  p <- NA_real_
  if (nd == 0) {
    p <- 1
  } else {
    p <- tryCatch(stats::binom.test(a_only, nd, 0.5)$p.value,
                  error = function(e) NA_real_)
  }
  list(a_only = a_only, b_only = b_only, n_discordant = nd, p_value = p)
}

# --- Hallucinated-quote detection -----------------------------------------
# Normalize away differences that are not fabrication: case, smart quotes and
# runs of whitespace. Without this, a curly apostrophe reads as a hallucination.
ab_normalize_match <- function(x) {
  x <- tolower(x)
  x <- gsub("[\u2018\u2019]", "'", x)
  x <- gsub("[\u201C\u201D]", "\"", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# TRUE = quote really is in the note, FALSE = fabricated/paraphrased,
# NA = no quote was claimed (a negative row), which is not a failure.
ab_is_verbatim <- function(evidence, note_text) {
  if (is.na(evidence) || is.na(note_text)) return(NA)
  if (!nzchar(trimws(as.character(evidence)))) return(NA)
  ev <- ab_normalize_match(as.character(evidence))
  nt <- ab_normalize_match(as.character(note_text))
  if (!nzchar(ev)) return(NA)
  if (grepl(ev, nt, fixed = TRUE)) return(TRUE)
  ev2 <- gsub("[.,;:]+$", "", ev)
  if (nzchar(ev2) && grepl(ev2, nt, fixed = TRUE)) return(TRUE)
  FALSE
}

ab_verbatim_vec <- function(evidence, note_text) {
  n <- length(evidence)
  if (n == 0) return(logical(0))
  out <- rep(NA, n)
  for (i in seq_len(n)) out[i] <- ab_is_verbatim(evidence[i], note_text[i])
  as.logical(out)
}

ab_evidence_cols <- function(df) grep("evidence_span$", names(df), value = TRUE)

# Guess which preset produced a file, by column overlap, so the schema audit
# knows which fields are supposed to be numeric vs boolean.
ab_detect_preset <- function(cols) {
  best <- NULL; best_n <- 0
  for (k in names(DIAGNOSIS_PRESETS)) {
    p <- DIAGNOSIS_PRESETS[[k]]
    if (is.null(p$cols)) next
    n <- length(intersect(p$cols, cols))
    if (n > best_n) { best_n <- n; best <- k }
  }
  if (best_n >= 2) best else NULL
}

# Values that a BOOLEAN field is allowed to hold once stringified.
AB_BOOL_OK <- c("true", "false", "t", "f", "yes", "no", "1", "0")

# Count cells that violate the preset's declared JSON schema type. A NUMBER
# field holding "grade 3" or a BOOLEAN holding "probable" means the model
# ignored the schema, which is a failure mode worth counting separately from
# getting the clinical answer wrong.
ab_schema_violations <- function(df, preset_key) {
  if (is.null(preset_key)) return(data.frame())
  props <- DIAGNOSIS_PRESETS[[preset_key]]$schema$properties
  if (is.null(props)) return(data.frame())
  rows <- list()
  for (fld in names(props)) {
    if (!(fld %in% names(df))) next
    ty <- props[[fld]]$type
    vals <- df[[fld]]
    nb <- !ab_blank(vals)
    n_checked <- sum(nb)
    if (n_checked == 0) next
    bad <- 0
    if (identical(ty, "NUMBER") || identical(ty, "INTEGER")) {
      num <- suppressWarnings(as.numeric(as.character(vals[nb])))
      bad <- sum(is.na(num))
    } else if (identical(ty, "BOOLEAN")) {
      bad <- sum(!(ab_norm(vals[nb]) %in% AB_BOOL_OK))
    } else {
      next
    }
    if (bad > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        field = fld, declared_type = ty, n_checked = n_checked,
        n_violations = bad, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# Per-model integrity audit. Everything here is computable from one file alone.
ab_quality_audit <- function(df, label, primary_field, pos_vals,
                             note_col = "NOTE_TEXT", preset_key = NULL) {
  rows <- list()
  add <- function(metric, count, denom, detail = "") {
    rows[[length(rows) + 1]] <<- data.frame(
      model = label, metric = metric, count = count, denom = denom,
      rate = if (!is.na(denom) && denom > 0) count / denom else NA_real_,
      detail = detail, stringsAsFactors = FALSE)
  }
  n <- nrow(df)
  add("Rows in file", n, n, "")

  if (!is.null(primary_field) && primary_field %in% names(df)) {
    pv <- ab_to_logical(df[[primary_field]], pos_vals)
    add("Positive calls", sum(pv, na.rm = TRUE), sum(!is.na(pv)),
        paste0(primary_field, " in {", paste(pos_vals, collapse = ", "), "}"))
    add("No value on primary field", sum(is.na(pv)), n,
        paste0(primary_field, " blank or null"))
  }

  if ("review_status" %in% names(df)) {
    rs <- ab_norm(df$review_status)
    add("API / parse failures", sum(rs %in% c("failed_api", "failed_parse")), n,
        "review_status = failed_api or failed_parse")
    add("No call returned", sum(rs == "no_call"), n, "review_status = no_call")
    add("Flagged for inclusion", sum(rs == "include"), n, "review_status = include")
    add("Needs human review (low confidence)",
        sum(rs %in% c("review_low_conf", "review_no_conf")), n,
        "review_status = review_low_conf or review_no_conf")
  }

  ev_cols <- ab_evidence_cols(df)
  if (length(ev_cols) > 0 && note_col %in% names(df) && n > 0) {
    for (ec in ev_cols) {
      v <- ab_verbatim_vec(as.character(df[[ec]]), as.character(df[[note_col]]))
      n_claimed <- sum(!is.na(v))
      add(paste0("Hallucinated quote \u2014 ", ec), sum(!is.na(v) & !v), n_claimed,
          "quote not found verbatim in NOTE_TEXT")
    }
  } else if (length(ev_cols) > 0) {
    add("Hallucinated quote", NA_integer_, NA_integer_,
        "cannot check \u2014 NOTE_TEXT column not in this file")
  }

  # Two mirror-image breaches of the evidence-span contract. Both are schema
  # compliance, not clinical accuracy, and both are checkable with no reference
  # standard.
  if (!is.null(primary_field) && primary_field %in% names(df) && length(ev_cols) > 0) {
    pv <- ab_to_logical(df[[primary_field]], pos_vals)
    ev_any <- rep(FALSE, nrow(df))
    for (ec in ev_cols) ev_any <- ev_any | !ab_blank(df[[ec]])

    # (a) A positive call with no supporting quote breaks the audit trail the
    #     whole evidence-span design exists to provide.
    n_pos <- sum(pv, na.rm = TRUE)
    add("Positive call with no quote", sum(!is.na(pv) & pv & !ev_any), n_pos,
        "positive but every evidence_span is empty")

    # (b) The inverse: a quote attached to a NEGATIVE call, where the prompt
    #     requires evidence_span to be null. This matters because such a span
    #     can never be verbatim (there is no affirming sentence to quote), so it
    #     is counted as a hallucinated quote and inflates that rate -- making a
    #     model look like it fabricates diagnostic evidence when what it is
    #     really doing is ignoring the "null on negatives" instruction. Counting
    #     it separately keeps the two failure modes apart.
    neg <- !is.na(pv) & !pv
    n_neg <- sum(neg)
    add("Quote supplied on a negative call", sum(neg & ev_any), n_neg,
        "negative call but an evidence_span was returned; schema requires null")
  }

  for (cc in grep("confidence$", names(df), value = TRUE)) {
    num <- suppressWarnings(as.numeric(as.character(df[[cc]])))
    nb <- !is.na(num)
    if (sum(nb) == 0) next
    add(paste0("Confidence outside 0\u20131 \u2014 ", cc),
        sum(nb & (num < 0 | num > 1)), sum(nb), "value out of range")
  }

  sv <- ab_schema_violations(df, preset_key)
  if (nrow(sv) > 0) {
    for (i in seq_len(nrow(sv))) {
      add(paste0("Schema type violation \u2014 ", sv$field[i]),
          sv$n_violations[i], sv$n_checked[i],
          paste0("declared ", sv$declared_type[i]))
    }
  }

  do.call(rbind, rows)
}

# Per-field agreement across every shared column the user asked to compare.
# A field is treated as continuous only when it parses as numeric AND carries a
# genuinely fractional value somewhere. That distinction matters: exact-match
# agreement on a 0-1 confidence score is near-meaningless (two models almost
# never return the identical decimal), whereas exact match on an integer grade,
# stage or gestational age is exactly the right question. Using "is it numeric"
# alone would wrongly silence grade and stage.
ab_is_continuous <- function(x, y) {
  nx <- suppressWarnings(as.numeric(as.character(x)))
  ny <- suppressWarnings(as.numeric(as.character(y)))
  ok <- !is.na(nx) | !is.na(ny)
  if (!any(ok)) return(FALSE)
  vals <- c(nx[!is.na(nx)], ny[!is.na(ny)])
  if (length(vals) == 0) return(FALSE)
  # numeric for most non-blank cells, and at least one non-integer value
  n_nonblank <- sum(!ab_blank(x) | !ab_blank(y))
  frac_numeric <- length(vals) / max(1, 2 * n_nonblank)
  any(abs(vals - round(vals)) > 1e-9) && frac_numeric > 0.5
}

ab_field_agreement <- function(joined, fields, model_cols = NULL) {
  rows <- list()
  for (f in fields) {
    ca <- paste0(f, "__A"); cb <- paste0(f, "__B")
    if (!(ca %in% names(joined)) || !(cb %in% names(joined))) next
    va <- joined[[ca]]; vb <- joined[[cb]]
    ba <- ab_blank(va); bb <- ab_blank(vb)
    keep <- !ba & !bb
    n_cmp <- sum(keep)

    na_ <- suppressWarnings(as.numeric(as.character(va)))
    nb_ <- suppressWarnings(as.numeric(as.character(vb)))
    both_num <- !is.na(na_) & !is.na(nb_)
    cont <- ab_is_continuous(va, vb)

    mad <- NA_real_; mxd <- NA_real_; rho <- NA_real_
    if (sum(both_num) > 0) {
      d <- abs(na_[both_num] - nb_[both_num])
      mad <- mean(d); mxd <- max(d)
      if (sum(both_num) >= 3) {
        sa <- stats::sd(na_[both_num]); sb <- stats::sd(nb_[both_num])
        # Correlation is undefined when either side is constant; report NA
        # rather than letting cor() warn and return NA silently.
        if (is.finite(sa) && is.finite(sb) && sa > 0 && sb > 0)
          rho <- suppressWarnings(stats::cor(na_[both_num], nb_[both_num]))
      }
    }

    if (cont) {
      # Exact match and kappa are not meaningful here, so they are left empty
      # instead of being reported as a near-zero agreement that looks alarming.
      n_agree <- NA_integer_; pct <- NA_real_; kap <- NA_real_
    } else {
      n_agree <- if (n_cmp > 0) sum(ab_norm(va[keep]) == ab_norm(vb[keep])) else 0L
      pct <- if (n_cmp > 0) n_agree / n_cmp else NA_real_
      ka <- ifelse(ba, NA_character_, ab_norm(va))
      kb <- ifelse(bb, NA_character_, ab_norm(vb))
      kap <- ab_cohen_kappa(ka, kb)
    }

    src <- if (is.null(model_cols)) "" else
      if (f %in% model_cols) "model output" else "passthrough"

    rows[[length(rows) + 1]] <- data.frame(
      field = f,
      source = src,
      type = if (cont) "continuous" else "categorical",
      n_compared = n_cmp,
      n_agree = n_agree,
      pct_agree = pct,
      kappa = kap,
      mean_abs_diff = mad,
      max_abs_diff = mxd,
      correlation = rho,
      a_missing = sum(ba),
      b_missing = sum(bb),
      stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  if (all(!nzchar(out$source))) out$source <- NULL
  # Worst agreement first, but continuous fields (pct_agree = NA) sort last
  # instead of floating to the top on a meaningless near-zero exact-match rate.
  out[order(out$pct_agree, na.last = TRUE), , drop = FALSE]
}

# Drop columns that are entirely empty. The per-field table carries columns that
# only apply to one kind of field (correlation and max difference for continuous
# ones), so on an all-categorical run three columns would otherwise print as a
# wall of em dashes and crowd the page. The first column is always kept so a
# table can never come back with nothing in it.
ab_drop_empty_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) <= 1) return(df)
  keep <- vapply(seq_along(df), function(i) {
    if (i == 1) return(TRUE)
    any(!ab_blank(df[[i]]))
  }, logical(1))
  df[, keep, drop = FALSE]
}

# Landis & Koch bands — gives a kappa a plain-language reading for
# collaborators who do not work with agreement statistics daily.
ab_kappa_label <- function(k) {
  if (is.null(k) || length(k) == 0 || is.na(k)) return("not estimable")
  if (k < 0)    return("worse than chance")
  if (k < 0.20) return("slight")
  if (k < 0.40) return("fair")
  if (k < 0.60) return("moderate")
  if (k < 0.80) return("substantial")
  "almost perfect"
}

# ---- DOCX report ---------------------------------------------------------
# Rendered with officer. Kept as a plain function (not a Shiny reactive) so it
# can be unit-tested or called from a scheduled script later.

# Coerce a results table into something Word can render: whole numbers stay
# whole, decimals get 3 digits, long strings are truncated, NA becomes an
# em dash rather than the string "NA".
ab_doc_prep_table <- function(df, max_chars = 150, drop_cols = NULL, max_rows = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  d <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!is.null(drop_cols)) d <- d[, setdiff(names(d), drop_cols), drop = FALSE]
  if (!is.null(max_rows) && nrow(d) > max_rows) d <- d[seq_len(max_rows), , drop = FALSE]
  if (ncol(d) == 0) return(NULL)
  for (cn in names(d)) {
    v <- d[[cn]]
    if (is.numeric(v)) {
      vv <- rep(NA_character_, length(v))
      ok <- !is.na(v)
      if (all(v[ok] == round(v[ok]))) {
        vv[ok] <- formatC(v[ok], format = "d", big.mark = ",")
      } else {
        vv[ok] <- formatC(v[ok], format = "f", digits = 3)
      }
      v <- vv
    } else {
      v <- as.character(v)
    }
    long <- !is.na(v) & nchar(v) > max_chars
    v[long] <- paste0(substr(v[long], 1, max_chars), "\u2026")
    v[is.na(v)] <- "\u2014"
    d[[cn]] <- v
  }
  d
}

# ---------------------------------------------------------------------------
# REPORT CONTENT
# One source of truth for the comparison report. Returns an ordered list of
# blocks; each output format is only a renderer over this list, so the HTML and
# Word versions cannot drift apart -- which matters, because this report is an
# audit record and two formats of it must say the same thing.
#
# Block types: title | h1 | h2 | p | bullets | table
# ---------------------------------------------------------------------------
ab_report_blocks <- function(r, meta) {
  B <- list()
  add <- function(...) B[[length(B) + 1]] <<- list(...)

  # Formatters that never print "NA" into a clinical report.
  pc <- function(x, dg = 1) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("not estimable")
    sprintf(paste0("%.", dg, "f%%"), 100 * x)
  }
  nm <- function(x, dg = 3) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("not estimable")
    formatC(x, format = "f", digits = dg)
  }
  cm <- function(x) formatC(x, format = "d", big.mark = ",")

  p_val <- r$mc$p_value
  p_txt <- if (is.null(p_val) || is.na(p_val)) "not estimable" else
    if (p_val < 0.001) "< 0.001" else formatC(p_val, format = "f", digits = 3)
  mode_name <- switch(r$mode,
    agree = "Agreement only (no ground truth supplied)",
    ref_a = paste0("Relative \u2014 ", r$la, " nominated as reference"),
    ref_b = paste0("Relative \u2014 ", r$lb, " nominated as reference"),
    gold  = "Scored against a gold-standard column",
    r$mode)

  add(type = "title",
      title = meta$title,
      subtitle = sprintf("%s versus %s", r$la, r$lb),
      stamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      author = meta$prepared_by)

  # ---- 1. Methods --------------------------------------------------------
  add(type = "h1", text = "1. Methods")
  add(type = "p", text = paste0(
    "Two independent large-language-model extraction runs over the same set of clinical notes were ",
    "compared at the note level. Both runs used the same prompt and the same structured output schema; ",
    "they differ only in the model that produced the output. The comparison evaluates (a) how often the ",
    "two models reach the same clinical call, and (b) whether each model's output is internally ",
    "consistent and evidence-grounded."))
  add(type = "h2", text = "1.1 Inputs and row matching")
  add(type = "table", value = data.frame(
    Item = c("Model A", "Model A file", "Model A rows in file", "Model A rows after de-duplication",
             "Model B", "Model B file", "Model B rows in file", "Model B rows after de-duplication",
             "Row matching key", "Rows matched",
             "Rows only in A", "Rows only in B",
             "Duplicate keys dropped (A)", "Duplicate keys dropped (B)",
             "Share of rows discarded",
             "Primary field (case definition)", "Values counted as positive",
             "Detected diagnosis preset", "Scoring mode", "Gold-standard column"),
    Value = c(r$la, meta$file_a, cm(r$n_a_raw %||% r$n_a), cm(r$n_a),
              r$lb, meta$file_b, cm(r$n_b_raw %||% r$n_b), cm(r$n_b),
              meta$key_desc, cm(r$n_matched),
              cm(r$unmatched_a), cm(r$unmatched_b),
              cm(r$dup_a), cm(r$dup_b),
              pc(r$dup_frac %||% 0),
              r$pf, paste(r$pos, collapse = ", "),
              if (is.null(r$preset)) "not detected" else r$preset,
              mode_name,
              if (is.null(meta$gold_col)) "not used" else meta$gold_col),
    stringsAsFactors = FALSE))
  add(type = "p", text = paste0(
    "A row contributes to the agreement statistics only when both models returned a usable value for the ",
    "primary field. Rows where exactly one model returned no call are reported separately in Section 3.5 ",
    "rather than being counted as agreement or disagreement, because a missing call is a different failure ",
    "from a wrong call. Duplicate keys were reduced to their first occurrence and the number dropped is ",
    "reported above."))

  # A key at the wrong granularity silently discards most of the data, so the
  # caveat belongs in Methods where it is read, not only in Limitations.
  if (isTRUE((r$dup_frac %||% 0) >= 0.10)) {
    add(type = "p", text = paste0(
      "CAVEAT ON ROW MATCHING. ", pc(r$dup_frac), " of rows were discarded as duplicate keys, so every ",
      "figure in this report describes the surviving subset rather than the full extraction. ",
      if (isTRUE(r$key_is_coarse))
        paste0("The matching key (", meta$key_desc, ") identifies a patient or an encounter rather than a ",
               "single row. One NICU encounter contains many notes and many imaging studies, so all but ",
               "the first were dropped and the comparison ran on one arbitrary row per encounter. ")
      else
        paste0("Every row after the first for each key value was dropped. "),
      if (length(r$finer_keys %||% character(0)) > 0)
        paste0("A finer identifier is present in both files (", paste(r$finer_keys, collapse = ", "),
               "); re-running keyed on it would retain the discarded rows. ")
      else
        paste0("No finer identifier was found in both files. "),
      "These results should not be interpreted until the matching key is resolved."))
  }

  # ---- 2. Statistical Analysis ------------------------------------------
  add(type = "h1", text = "2. Statistical Analysis")
  add(type = "h2", text = "2.1 Agreement")
  add(type = "p", text = paste0(
    "Raw agreement is the proportion of jointly scored rows on which the two models returned the same ",
    "call. Raw agreement alone is misleading when a finding is uncommon: if a condition is present in 5% ",
    "of notes, two models that both call almost everything negative will agree on roughly 95% of rows ",
    "without either one being informative. Cohen's kappa is therefore reported alongside it. Kappa is the ",
    "share of agreement remaining after the agreement expected from the two models' marginal rates is ",
    "removed, so 0 means no better than chance and 1 means perfect. It is interpreted using the ",
    "conventional Landis and Koch bands: below 0.20 slight, 0.20 to 0.40 fair, 0.40 to 0.60 moderate, ",
    "0.60 to 0.80 substantial, above 0.80 almost perfect."))
  add(type = "h2", text = "2.2 Testing for a systematic difference")
  add(type = "p", text = paste0(
    "Because both models scored the same notes, the two sets of calls are paired, and an unpaired test ",
    "would be invalid. McNemar's test is used instead. It discards the rows on which the models agree, ",
    "since those carry no information about a difference between them, and asks whether the remaining ",
    "discordant rows split evenly between the two directions. An exact binomial test is used at every ",
    "sample size rather than the chi-square approximation, so the p-value needs no large-sample ",
    "justification at the small discordant counts typical of uncommon findings. A significant result ",
    "means one model calls cases positive more readily than the other; it does not identify which model ",
    "is correct."))
  add(type = "h2", text = "2.3 Classification metrics")
  add(type = "p", text = paste0(
    "Where a reference was nominated, performance is summarised by positive predictive value (PPV, also ",
    "called precision) and sensitivity (also called recall), in preference to a single global figure such ",
    "as AUC. PPV answers the question a reviewer actually faces: of the notes this model flagged, what ",
    "share were genuinely positive, and therefore how much of the review queue is wasted effort. ",
    "Sensitivity answers the complementary question: of the true positives, what share would be missed ",
    "if the model's output were used unreviewed. F1 is their harmonic mean. Each proportion carries a 95% ",
    "Wilson score interval, which, unlike the normal approximation, remains inside the 0 to 1 range when ",
    "counts are small or a proportion sits near 0 or 1."))
  add(type = "h2", text = "2.4 Integrity checks (no ground truth required)")
  add(type = "p", text = paste0(
    "Three checks are computable from a single output file and need no reference standard. First, a quote ",
    "is counted as hallucinated when the evidence span the model returned is not present verbatim in the ",
    "source note, after normalising case, curly quotation marks and runs of whitespace so that cosmetic ",
    "differences are not flagged. A model can reach the correct clinical conclusion while fabricating or ",
    "paraphrasing the supporting quote, and that failure specifically defeats the audit trail the ",
    "evidence-span design exists to provide. Second, a positive call accompanied by no quote at all is ",
    "the same defect in a different form. Third, schema violations count cells whose value does not match ",
    "the field type declared in the extraction schema, which indicates the model ignored the structured ",
    "output contract."))

  # ---- 3. Results -------------------------------------------------------
  add(type = "h1", text = "3. Results")
  add(type = "h2", text = "3.1 Summary")
  add(type = "table", value = data.frame(
    Metric = c("Rows matched", "Rows jointly scored", "Rows in agreement",
               "Raw agreement", "Cohen's kappa", "Kappa interpretation",
               "Discordant rows", paste0("Positive in ", r$la, " only"),
               paste0("Positive in ", r$lb, " only"), "McNemar exact p",
               paste0("Hallucinated quote rate \u2014 ", r$la),
               paste0("Hallucinated quote rate \u2014 ", r$lb)),
    Value = c(cm(r$n_matched), cm(r$n_both), cm(r$n_agree),
              pc(r$pct_agree), nm(r$kappa), ab_kappa_label(r$kappa),
              cm(r$mc$n_discordant), cm(r$mc$a_only), cm(r$mc$b_only), p_txt,
              pc(r$hall_a, 2), pc(r$hall_b, 2)),
    stringsAsFactors = FALSE))

  if (!is.null(r$met_tbl)) {
    add(type = "h2", text = "3.2 Classification metrics")
    add(type = "p", text = if (identical(r$mode, "gold"))
      "Scored against the nominated gold-standard column. These are accuracy estimates."
      else paste0("Scored against a nominated model rather than a reference standard. ",
                  "These quantify how the two models differ, not how accurate either one is."))
    add(type = "table", value = ab_doc_prep_table(r$met_tbl))
  }

  add(type = "h2", text = "3.3 Per-field agreement")
  add(type = "p", text = paste0(
    "Compared fields, ordered worst agreement first. Fields are split by type, because the right question ",
    "differs between them. For a categorical field (presence, grade, laterality, modality) the question is ",
    "whether the two models returned the same value, so exact agreement and kappa are reported. For a ",
    "continuous field such as a confidence score, exact agreement is close to meaningless, since two models ",
    "almost never return the identical decimal; agreement and kappa are therefore left blank and the mean ",
    "and maximum absolute difference and the correlation are reported instead. An integer field such as a ",
    "grade or a gestational age counts as categorical, because matching exactly is genuinely what is being ",
    "asked of it.",
    if (!is.null(r$model_cols))
      paste0(" The source column marks whether a field was produced by the model or carried through from ",
             "the source query; passthrough fields are identical in both files by construction and agree ",
             "trivially.")
    else ""))
  add(type = "table", value = ab_doc_prep_table(ab_drop_empty_cols(r$fld_tbl)),
      empty = "No shared fields were compared.")

  add(type = "h2", text = "3.4 Integrity audit")
  aud_doc <- r$aud
  if (!is.null(aud_doc) && nrow(aud_doc) > 0) {
    aud_doc$rate_pct <- ifelse(is.na(aud_doc$rate), NA_real_, 100 * aud_doc$rate)
    aud_doc <- aud_doc[, c("model", "metric", "count", "denom", "rate_pct", "detail"), drop = FALSE]
    names(aud_doc) <- c("Model", "Check", "N", "Denominator", "Rate (%)", "Definition")
  }
  add(type = "table", value = ab_doc_prep_table(aud_doc, max_chars = 90),
      empty = "Nothing to audit.")

  add(type = "h2", text = "3.5 Disagreements")
  n_dis <- if (is.null(r$dis)) 0 else nrow(r$dis)
  add(type = "p", text = sprintf(
    "%s row(s) disagree on the primary field or had a call from only one model. %s",
    cm(n_dis),
    if (n_dis > meta$max_dis)
      sprintf("The first %s are listed below; the complete list is in the disagreements CSV export.", cm(meta$max_dis))
    else "All are listed below."))
  add(type = "table",
      value = ab_doc_prep_table(r$dis, max_chars = 140,
                                drop_cols = "NOTE_TEXT", max_rows = meta$max_dis),
      empty = "No disagreements on the primary field.")
  if (n_dis > 0)
    add(type = "p", text = paste0(
      "Note text is omitted from this table for length and is retained in the CSV export. This list is ",
      "the adjudication worklist: these are the notes a clinical reviewer must read to establish which ",
      "model is correct."))

  # ---- 4. Discussion ----------------------------------------------------
  add(type = "h1", text = "4. Discussion")
  # Kappa can be genuinely undefined (only one observed level), and in that case
  # "was not estimable, conventionally read as not estimable" is nonsense. The
  # undefined case gets its own sentence, because an undefined kappa sitting
  # beside 100% agreement is a substantive finding rather than a missing number.
  if (is.null(r$kappa) || length(r$kappa) == 0 || is.na(r$kappa)) {
    add(type = "p", text = sprintf(paste0(
      "The two models returned the same call on %s of %s jointly scored notes (%s). Cohen's kappa could ",
      "not be computed, because the primary field took only one value across the scored rows and a ",
      "chance-corrected statistic needs variation to work on. That is itself the main finding rather ",
      "than a gap in the analysis: agreement on a constant carries no information about which model ",
      "detects the condition better, since a genuinely accurate model and a model that returns the same ",
      "answer every time are indistinguishable on such data. A cohort containing confirmed positive ",
      "cases is required before these two models can be ranked."),
      cm(r$n_agree), cm(r$n_both), pc(r$pct_agree)))
  } else {
    add(type = "p", text = sprintf(paste0(
      "The two models returned the same call on %s of %s jointly scored notes (%s). Chance-corrected ",
      "agreement was %s, conventionally read as %s."),
      cm(r$n_agree), cm(r$n_both), pc(r$pct_agree), nm(r$kappa), ab_kappa_label(r$kappa)))
  }

  if (isTRUE(r$mc$n_discordant == 0)) {
    # "Split evenly within sampling error" is a strange thing to say about zero
    # rows, and p = 1 here is arithmetic rather than evidence of anything.
    add(type = "p", text = paste0(
      "There were no discordant rows, so McNemar's test has nothing to work on and returns p = 1 by ",
      "construction. This is a complete absence of evidence of a difference, which is not the same as ",
      "evidence that the two models behave identically: a test with no discordant pairs has no power at all."))
  } else if (!is.null(p_val) && !is.na(p_val) && p_val < 0.05) {
    lead <- if (r$mc$a_only > r$mc$b_only) r$la else r$lb
    add(type = "p", text = sprintf(paste0(
      "The discordant rows did not split evenly (exact McNemar p = %s): %s called %s case(s) positive ",
      "that %s did not, against %s in the opposite direction. %s therefore applies a systematically ",
      "lower threshold for calling a case positive. Whether that reflects better sensitivity or more ",
      "false positives cannot be settled from these two files, because neither is a reference standard; ",
      "it requires chart review of the disagreement list."),
      p_txt, r$la, cm(r$mc$a_only), r$lb, cm(r$mc$b_only), lead))
  } else {
    add(type = "p", text = sprintf(paste0(
      "The discordant rows split evenly within sampling error (exact McNemar p = %s), so there is no ",
      "evidence that either model systematically calls more cases positive than the other. With %s ",
      "discordant row(s) the test may simply lack power to detect a modest difference, so this should ",
      "not be read as evidence that the models are equivalent."),
      p_txt, cm(r$mc$n_discordant)))
  }

  add(type = "p", text = paste0(
    "On the integrity checks, the hallucinated-quote rate was ", pc(r$hall_a, 2), " for ", r$la,
    " and ", pc(r$hall_b, 2), " for ", r$lb, ". ",
    "This is the check that most directly bears on whether an extracted value can be audited, since a ",
    "quote that is not in the note cannot be verified by a reviewer, and it is measurable without any ",
    "reference standard."))

  add(type = "h2", text = "4.1 Limitations")
  lim <- c()
  # Leads the list when present: it invalidates everything above it.
  if (isTRUE((r$dup_frac %||% 0) >= 0.10)) {
    lim <- c(lim, paste0(
      pc(r$dup_frac), " of rows were discarded as duplicate keys when matching on ", meta$key_desc,
      ", so this report describes a subset of the extraction rather than all of it. ",
      if (isTRUE(r$key_is_coarse))
        "The key identifies a patient or an encounter rather than a row, so the retained row for each key is arbitrary. "
      else "",
      "Resolve the matching key before drawing any conclusion from these figures."))
  }
  # Zero positives makes every detection metric vacuous, which is easy to miss
  # behind a reassuring 100% agreement.
  if (isTRUE(r$mc$a_only == 0) && isTRUE(r$mc$b_only == 0) &&
      (is.null(r$kappa) || length(r$kappa) == 0 || is.na(r$kappa))) {
    lim <- c(lim, paste0(
      "Neither model returned a positive call on any scored row. Sensitivity and positive predictive ",
      "value are undefined without positives, and the high raw agreement reflects two models concurring ",
      "on a negative rather than either one demonstrating detection ability. This comparison cannot rank ",
      "the models until it is run on a cohort known to contain positive cases."))
  }
  if (r$mode %in% c("agree", "ref_a", "ref_b")) {
    lim <- c(lim, paste0(
      "No reference standard was supplied, so nothing in this report establishes accuracy. Agreement ",
      "between two models is not evidence that either is correct: models that share a blind spot agree ",
      "perfectly. Any accuracy claim requires scoring against expert chart review."))
  }
  if (r$mode %in% c("ref_a", "ref_b")) {
    lim <- c(lim, paste0(
      "The PPV and sensitivity figures in Section 3.2 are computed against a nominated model, not a ",
      "validated standard, and should be described as relative agreement rather than accuracy."))
  }
  if (identical(r$mode, "gold")) {
    lim <- c(lim, paste0(
      "Accuracy estimates cover only the rows where the gold-standard column is populated, which may not ",
      "be a random sample of the cohort. If the reviewed rows were selected because they were difficult ",
      "or because a model flagged them, the resulting PPV and sensitivity will not transfer to the full ",
      "cohort."))
  }
  lim <- c(lim,
    paste0("Results reflect a single run of each model. Generation is not fully deterministic, so part of ",
           "the observed disagreement is run-to-run variation rather than a stable difference between ",
           "models. Repeat runs would be needed to separate the two."),
    paste0("The unit of analysis is the note, not the infant. A patient with several cranial studies ",
           "contributes several rows, so these figures do not describe accuracy of a patient-level ",
           "diagnosis, which depends additionally on how per-note results are rolled up (for example a ",
           "worst-grade-ever rule)."),
    paste0("Kappa is sensitive to how common the finding is. For a rare condition it can be low even when ",
           "raw agreement is high, and it is not comparable across cohorts with different prevalence."),
    paste0("Rows present in only one file are excluded from the agreement statistics and are counted in ",
           "Section 1.1. A large imbalance there indicates the two runs did not cover the same notes, ",
           "which would need resolving before these figures are interpreted."))
  add(type = "bullets", items = lim)

  add(type = "h2", text = "4.2 Suggested next step")
  add(type = "p", text = paste0(
    "Chart-review the rows in Section 3.5 and record the adjudicated call in a gold-standard column, then ",
    "re-run this comparison in gold-standard mode. Reviewing only the discordant rows is substantially ",
    "cheaper than reviewing the full cohort and is sufficient to rank the two models, though a sample of ",
    "concordant rows is also needed to detect errors the two models share."))

  # ---- Appendix ---------------------------------------------------------
  add(type = "h1", text = "Appendix. Reproducibility")
  add(type = "table", value = data.frame(
    Item = c("Report generated", "Model A label", "Model A source file",
             "Model B label", "Model B source file", "Row matching",
             "Primary field", "Positive value definition", "Scoring mode",
             "Hallucination rule", "Agreement test", "Interval method"),
    Value = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), r$la, meta$file_a,
              r$lb, meta$file_b, meta$key_desc, r$pf,
              paste(r$pos, collapse = " | "), mode_name,
              "evidence span absent from NOTE_TEXT after case/quote/whitespace normalisation",
              "exact McNemar (binomial, paired)", "Wilson score, 95%"),
    stringsAsFactors = FALSE))

  B
}

# ---------------------------------------------------------------------------
# RENDERER: self-contained HTML  (the recommended format)
# No package dependencies, no external assets, no network fetch. Opens on any
# machine and prints to PDF from the browser, which avoids needing a LaTeX
# toolchain just to produce a fixed-layout copy.
# ---------------------------------------------------------------------------

# Escape order matters: & must be replaced first or later entities get mangled.
ab_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

ab_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("")
  hd <- paste0("<th>", ab_html_escape(names(df)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(names(df), function(cn) {
      paste0("<td>", ab_html_escape(df[[cn]][i]), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class='tw'><table><thead><tr>", hd, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table></div>")
}

AB_HTML_CSS <- "
:root{--navy:#13294b;--navy2:#1f4068;--teal:#008c95;--ink:#1c1c1c;
      --muted:#5a6672;--line:#d6dce3;--bg:#ffffff;--zebra:#f6f8fa;}
*{box-sizing:border-box;}
body{margin:0;padding:48px 56px 72px;background:var(--bg);color:var(--ink);
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
  font-size:11.5pt;line-height:1.65;max-width:1100px;}
h1{color:var(--navy);font-size:17pt;font-weight:650;letter-spacing:-0.01em;
  margin:34px 0 12px;padding-bottom:7px;border-bottom:2px solid var(--teal);}
h2{color:var(--navy2);font-size:12.5pt;font-weight:600;margin:24px 0 8px;}
p{margin:0 0 12px;text-align:justify;}
ul{margin:0 0 14px;padding-left:22px;}
li{margin-bottom:9px;}
table{border-collapse:collapse;width:100%;margin:10px 0 20px;font-size:9.8pt;}
thead th{background:var(--navy);color:#fff;text-align:left;font-weight:600;
  padding:7px 10px;border:1px solid var(--navy);}
tbody td{padding:6px 10px;border:1px solid var(--line);vertical-align:top;
  overflow-wrap:break-word;}
tbody tr:nth-child(even){background:var(--zebra);}
tbody tr td:first-child{font-weight:500;}
.cover{border-bottom:3px solid var(--navy);padding-bottom:18px;margin-bottom:8px;}
.cover h1{border:none;margin:0 0 6px;padding:0;font-size:22pt;}
.sub{color:var(--teal);font-size:13pt;font-weight:600;margin-bottom:10px;
  font-variant-numeric:tabular-nums;}
.meta{color:var(--muted);font-size:9.5pt;line-height:1.5;}
.none{color:var(--muted);font-style:italic;margin:0 0 16px;}
.hint{background:#f0f6f7;border-left:3px solid var(--teal);padding:10px 14px;
  margin:0 0 22px;font-size:9.5pt;color:var(--muted);}
/* A wide table scrolls inside its own box on a narrow screen rather than
   forcing the whole page sideways. Reset for print so nothing is clipped. */
.tw{overflow-x:auto;-webkit-overflow-scrolling:touch;margin:10px 0 20px;}
.tw table{margin:0;}
@media (max-width:640px){
  body{padding:24px 16px 48px;font-size:11pt;}
  .cover h1{font-size:18pt;}
  table{font-size:9pt;}
  p{text-align:left;}
}
@media print{
  body{padding:0;font-size:10pt;max-width:none;
    -webkit-print-color-adjust:exact;print-color-adjust:exact;}
  .hint{display:none;}
  .tw{overflow:visible;}
  h1{page-break-after:avoid;}
  h2{page-break-after:avoid;}
  table{page-break-inside:auto;}
  tr{page-break-inside:avoid;}
  thead{display:table-header-group;}
}
"

ab_build_html <- function(r, meta, file) {
  blocks <- ab_report_blocks(r, meta)
  out <- character(0)
  push <- function(s) out[[length(out) + 1]] <<- s
  ttl <- meta$title

  for (b in blocks) {
    if (identical(b$type, "title")) {
      ttl <- b$title
      push(paste0(
        "<div class='cover'><h1>", ab_html_escape(b$title), "</h1>",
        "<div class='sub'>", ab_html_escape(b$subtitle), "</div>",
        "<div class='meta'>Generated ", ab_html_escape(b$stamp),
        if (!is.null(b$author) && nzchar(b$author))
          paste0("<br>Prepared by: ", ab_html_escape(b$author)) else "",
        "</div></div>"))
      push(paste0("<div class='hint'>To save a fixed-layout copy, print this page ",
                  "and choose &quot;Save as PDF&quot;. This block is hidden when printed.</div>"))
    } else if (identical(b$type, "h1")) {
      push(paste0("<h1>", ab_html_escape(b$text), "</h1>"))
    } else if (identical(b$type, "h2")) {
      push(paste0("<h2>", ab_html_escape(b$text), "</h2>"))
    } else if (identical(b$type, "p")) {
      push(paste0("<p>", ab_html_escape(b$text), "</p>"))
    } else if (identical(b$type, "bullets")) {
      push(paste0("<ul>", paste0("<li>", ab_html_escape(b$items), "</li>",
                                 collapse = ""), "</ul>"))
    } else if (identical(b$type, "table")) {
      if (is.null(b$value) || nrow(b$value) == 0) {
        push(paste0("<p class='none'>",
                    ab_html_escape(if (is.null(b$empty)) "None." else b$empty), "</p>"))
      } else {
        push(ab_html_table(b$value))
      }
    }
  }

  html <- paste0(
    "<!doctype html>\n<html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", ab_html_escape(ttl), "</title><style>", AB_HTML_CSS, "</style>",
    "</head><body>\n", paste(out, collapse = "\n"), "\n</body></html>\n")

  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(html), con, useBytes = TRUE)
  invisible(file)
}

# ---------------------------------------------------------------------------
# RENDERER: Word (.docx) -- for co-authors who track-change, and for pasting
# tables into a manuscript. Requires the officer package.
# ---------------------------------------------------------------------------

# Apply the first style the template actually accepts, falling back to the
# template default. Style names differ between Word templates, so nothing here
# depends on a particular one being present.
ab_try_styles <- function(fn, d, value, styles) {
  for (s in styles) {
    out <- tryCatch(fn(d, value, style = s), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  tryCatch(fn(d, value), error = function(e) d)
}

ab_build_docx <- function(r, meta, file) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("The 'officer' package is required for Word export. Install it with: install.packages(\"officer\")")

  blocks <- ab_report_blocks(r, meta)
  doc <- officer::read_docx()
  h1 <- c("heading 1", "Heading 1", "Title")
  h2 <- c("heading 2", "Heading 2", "Subtitle")
  tb <- c("table_template", "Table Grid", "Plain Table 1", "Normal Table")

  P <- function(d, txt, styles = NULL) {
    if (is.null(styles)) return(officer::body_add_par(d, txt))
    ab_try_styles(officer::body_add_par, d, txt, styles)
  }

  for (b in blocks) {
    if (identical(b$type, "title")) {
      doc <- P(doc, b$title, h1)
      doc <- P(doc, b$subtitle)
      doc <- P(doc, paste0("Generated ", b$stamp))
      if (!is.null(b$author) && nzchar(b$author))
        doc <- P(doc, paste0("Prepared by: ", b$author))
      doc <- P(doc, "")
    } else if (identical(b$type, "h1")) {
      doc <- P(doc, b$text, h1)
    } else if (identical(b$type, "h2")) {
      doc <- P(doc, b$text, h2)
    } else if (identical(b$type, "p")) {
      doc <- P(doc, b$text)
    } else if (identical(b$type, "bullets")) {
      for (it in b$items) doc <- P(doc, paste0("\u2022  ", it))
    } else if (identical(b$type, "table")) {
      if (is.null(b$value) || nrow(b$value) == 0) {
        doc <- P(doc, if (is.null(b$empty)) "None." else b$empty)
      } else {
        doc <- ab_try_styles(officer::body_add_table, doc, b$value, tb)
        doc <- P(doc, "")
      }
    }
  }

  print(doc, target = file)
  invisible(file)
}

ab_read_file <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet) || !nzchar(sheet)) return(readxl::read_excel(path))
    return(readxl::read_excel(path, sheet = sheet))
  }
  if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
  if (ext == "txt") return(readr::read_delim(path, delim = "\t", show_col_types = FALSE))
  stop(paste("Unsupported file type:", ext))
}

# Columns that make sense as a row key, MOST GRANULAR FIRST -- the first match
# is auto-selected, so ordering decides what a user gets by default.
#
# This ordering is load-bearing. An encounter- or patient-level ID looks like a
# perfectly good key until you notice that one NICU encounter has many cranial
# studies: every study after the first is then discarded as a duplicate, and the
# comparison silently runs on one arbitrary study per infant. Row-level
# identifiers (the note, the imaging order, the accession) must therefore be
# preferred over PAT_ENC_CSN_ID / PAT_ID / MRN, which appear last and are
# flagged as coarse below.
AB_KEY_CANDIDATES <- c(
  # note level
  "NOTE_ID", "NOTE_CSN_ID", "NOTE_CSN",
  # imaging order / study level
  "ORDER_PROC_ID", "ORDER_ID", "PROC_ORD_ID", "ORDER_PROC_CSN_ID",
  "ACCESSION_NUM", "ACCESSION_NUMBER", "ACCESSION",
  # procedure / case level
  "OR_CASE_ID", "LOG_ID",
  # line within a result
  "LINE_NUM", "LINE",
  # encounter level -- one per hospital stay, NOT per study
  "PAT_ENC_CSN_ID", "PAT_ENC_CSN", "HSP_ACCOUNT_ID",
  # patient level -- coarsest of all
  "PAT_ID", "MRN",
  "row_id")

# Keys that identify a patient or an encounter rather than a row. Selecting one
# is legitimate when the files really are one row per patient, so it is allowed,
# but it triggers the duplicate-key warning when it collapses rows.
AB_COARSE_KEYS <- c("PAT_ENC_CSN_ID", "PAT_ENC_CSN", "HSP_ACCOUNT_ID", "PAT_ID", "MRN")

# Columns that are inputs or bookkeeping, not model output, so they are not
# offered as things to compare.
AB_NON_COMPARE <- c("NOTE_TEXT", "NOTE_DATE", "CONTACT_DATE", "extract_error",
                    "raw_response", "prompt_used")

# =============================================================================
# REVIEW STATUS  (honors negation; gates inclusion on confidence threshold)
#   include            -> present, not negated, confidence >= threshold
#   review_low_conf    -> present, not negated, confidence < threshold
#   review_no_conf     -> present, not negated, no numeric confidence returned
#   exclude_negated    -> negated == true  (regardless of present)
#   exclude_absent     -> present field is the negative value
#   no_call            -> model returned JSON but the present field was null
#   -- terminal failure statuses written by the pipeline (never NA) --
#   failed_api         -> both API attempts returned no text
#   failed_parse       -> API returned text but JSON never parsed
#   skipped_empty_note -> NOTE_TEXT was blank / < 5 chars
# =============================================================================
.review_status_core <- function(present_val, negated_val, conf_val, present_true, conf_threshold = 0.7) {
  norm <- function(x) tolower(trimws(as.character(x)))
  is_flag_true <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    v <- norm(x[1]); !is.na(v) && v %in% c("true", "1", "yes")
  }
  if (is.null(present_val) || length(present_val) == 0 ||
      is.na(present_val[1]) || norm(present_val[1]) %in% c("", "na", "null"))
    return("no_call")
  if (is_flag_true(negated_val)) return("exclude_negated")
  if (!(norm(present_val[1]) %in% norm(present_true))) return("exclude_absent")
  conf <- suppressWarnings(as.numeric(conf_val[1]))
  if (is.na(conf)) return("review_no_conf")
  if (conf >= conf_threshold) return("include")
  "review_low_conf"
}

.rollup_review_status <- function(statuses) {
  if (any(statuses == "include")) return("include")
  if (any(statuses %in% c("review_low_conf", "review_no_conf"))) return("review_low_conf")
  if (any(statuses %in% c("exclude_negated", "exclude_absent"))) return("exclude_absent")
  "no_call"
}

compute_review_status <- function(parsed, preset, conf_threshold = 0.7) {
  pf <- preset$present_field
  if (is.null(pf)) pf <- preset$cols[1]
  .review_status_core(parsed[[pf]], parsed[["negated"]], parsed[["confidence"]],
                      preset$present_true, conf_threshold)
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
        padding: 20px 32px 16px; margin-bottom: 18px;
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
        border-radius: 8px; padding: 14px 18px; margin-bottom: 18px;
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
        letter-spacing: 0.06em; text-transform: uppercase;
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
      .pill-warn    { background: rgba(210,153,34,0.15);  color: #d29922; border: 1px solid rgba(210,153,34,0.3); }
      .eta-badge { font-size: 0.72rem; color: #8b949e; margin-left: 10px; }
      .note-preview-box {
        background: #010409; border: 1px solid #21262d; border-radius: 6px;
        padding: 12px 14px; font-family: 'IBM Plex Mono', monospace;
        font-size: 0.75rem; color: #c9d1d9; max-height: 180px;
        overflow-y: auto; white-space: pre-wrap; line-height: 1.6; margin-top: 10px;
      }
      .filter-bar { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; flex-wrap: wrap; }
      .filter-bar .form-select { width: auto !important; min-width: 150px; }
      .divider-line { border: none; border-top: 1px solid #21262d; margin: 12px 0; }
      .diag-badge {
        display: inline-flex; align-items: center; gap: 6px;
        padding: 4px 12px; border-radius: 20px; font-size: 0.72rem; font-weight: 600;
        letter-spacing: 0.04em; margin-bottom: 12px;
      }
      .diag-NEC     { background: rgba(248,81,73,0.12);   color: #f85149; border: 1px solid rgba(248,81,73,0.3); }
      .diag-IVH     { background: rgba(88,166,255,0.12);  color: #58a6ff; border: 1px solid rgba(88,166,255,0.3); }
      .diag-PVL     { background: rgba(210,153,34,0.15);  color: #d29922; border: 1px solid rgba(210,153,34,0.3); }
      .diag-ROP     { background: rgba(63,185,80,0.12);   color: #3fb950; border: 1px solid rgba(63,185,80,0.3); }
      .diag-IVH_PVL { background: rgba(20,184,166,0.12);  color: #2dd4bf; border: 1px solid rgba(20,184,166,0.3); }
      .diag-PDA      { background: rgba(163,113,247,0.12); color: #a371f7; border: 1px solid rgba(163,113,247,0.3); }
      .diag-PDA_Echo { background: rgba(163,113,247,0.08); color: #a371f7; border: 1px solid rgba(163,113,247,0.2); }
      .diag-PDA_CXR  { background: rgba(163,113,247,0.06); color: #a371f7; border: 1px solid rgba(163,113,247,0.15); }
      .diag-custom  { background: rgba(139,148,158,0.12); color: #8b949e; border: 1px solid rgba(139,148,158,0.3); }
      .schema-fields { display: flex; flex-wrap: wrap; gap: 5px; margin-top: 6px; }
      .schema-field {
        padding: 2px 8px; border-radius: 4px; font-size: 0.65rem; font-weight: 600;
        font-family: 'IBM Plex Mono', monospace; letter-spacing: 0.04em;
        background: #21262d; color: #8b949e; border: 1px solid #30363d;
      }

      /* ---- Persistent status bar ---- */
      .status-bar {
        display: flex; align-items: center; gap: 18px; flex-wrap: wrap;
        background: #161b22; border: 1px solid #30363d; border-radius: 8px;
        padding: 10px 16px; margin: 0 0 18px;
      }
      .status-item { display: flex; align-items: center; gap: 8px; font-size: 0.75rem; color: #8b949e; }
      .status-item-label {
        font-size: 0.66rem; font-weight: 600; letter-spacing: 0.1em;
        text-transform: uppercase; color: #6e7681;
      }
      .status-dot { font-size: 0.85rem; }
      .status-progress { flex: 1 1 240px; min-width: 220px; }

      /* ---- Segmented diagnosis selector ---- */
      .diag-seg-group { display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 12px; }
      .diag-seg {
        display: inline-flex; align-items: center; gap: 6px;
        padding: 6px 12px; border-radius: 20px; font-size: 0.7rem; font-weight: 600;
        letter-spacing: 0.04em; cursor: pointer; border: 1px solid #30363d;
        background: #0d1117; color: #8b949e; font-family: 'IBM Plex Mono', monospace;
        transition: transform 0.15s ease, box-shadow 0.15s ease, background 0.15s ease;
        user-select: none;
      }
      .diag-seg:hover { transform: translateY(-1px); }
      .diag-seg.diag-NEC     { background: rgba(248,81,73,0.12);   color: #f85149; border: 1px solid rgba(248,81,73,0.3); }
      .diag-seg.diag-IVH     { background: rgba(88,166,255,0.12);  color: #58a6ff; border: 1px solid rgba(88,166,255,0.3); }
      .diag-seg.diag-PVL     { background: rgba(210,153,34,0.15);  color: #d29922; border: 1px solid rgba(210,153,34,0.3); }
      .diag-seg.diag-ROP     { background: rgba(63,185,80,0.12);   color: #3fb950; border: 1px solid rgba(63,185,80,0.3); }
      .diag-seg.diag-IVH_PVL { background: rgba(20,184,166,0.12);  color: #2dd4bf; border: 1px solid rgba(20,184,166,0.3); }
      .diag-seg.diag-PDA      { background: rgba(163,113,247,0.12); color: #a371f7; border: 1px solid rgba(163,113,247,0.3); }
      .diag-seg.diag-PDA_Echo { background: rgba(163,113,247,0.08); color: #a371f7; border: 1px solid rgba(163,113,247,0.2); }
      .diag-seg.diag-PDA_CXR  { background: rgba(163,113,247,0.06); color: #a371f7; border: 1px solid rgba(163,113,247,0.15); }
      .diag-seg.diag-custom  { background: rgba(139,148,158,0.12); color: #8b949e; border: 1px solid rgba(139,148,158,0.3); }
      .diag-seg.active {
        box-shadow: 0 0 0 2px rgba(88,166,255,0.35), inset 0 0 0 1px currentColor;
        transform: translateY(-1px);
      }

      /* ---- Run pipeline grid ---- */
      .run-checkboxes { padding-top: 6px; }
      .run-checkboxes .form-check { margin-bottom: 10px; }
      .run-checkboxes .form-check-input {
        background-color: #0d1117 !important; border-color: #30363d !important;
      }
      .run-checkboxes .form-check-input:checked {
        background-color: #1f6feb !important; border-color: #1f6feb !important;
      }
      .run-checkboxes .form-check-label { font-size: 0.8rem; color: #c9d1d9; }
      .run-actions { display: flex; flex-direction: column; gap: 8px; padding-top: 6px; }

      /* ---- Note preview / evidence / copy ---- */
      .note-copy-row { display: flex; gap: 8px; margin: 10px 0 6px; }
      .copy-btn { font-size: 0.68rem; }
      .evidence-highlight {
        background: #0d2818; border: 1px solid #1f6f3f; border-radius: 6px;
        padding: 10px 12px; margin: 8px 0; color: #7ee787;
        font-family: 'IBM Plex Mono', monospace; font-size: 0.75rem; white-space: pre-wrap;
      }
      .evidence-label {
        color: #8b949e; text-transform: uppercase; font-size: 0.62rem;
        letter-spacing: 0.08em; margin-bottom: 4px;
      }
      .error-highlight {
        background: #2d0f0f; border: 1px solid #6e2020; border-radius: 6px;
        padding: 10px 12px; margin: 8px 0; color: #ff9492;
        font-family: 'IBM Plex Mono', monospace; font-size: 0.75rem; white-space: pre-wrap;
      }
      .note-preview-placeholder {
        padding: 18px; text-align: center; color: #8b949e; font-size: 0.8rem;
        border: 1px dashed #30363d; border-radius: 6px; margin-top: 10px;
      }

      /* ---- Failure banner ---- */
      .failure-banner {
        background: rgba(248,81,73,0.10); border: 1px solid rgba(248,81,73,0.35);
        border-radius: 6px; padding: 10px 14px; margin-bottom: 12px;
        color: #ff9492; font-size: 0.76rem;
      }

      /* ---- Empty state ---- */
      .empty-state { text-align: center; padding: 60px 20px; color: #8b949e; }
      .empty-state .empty-icon { font-size: 2.6rem; margin-bottom: 12px; }
      .empty-state .empty-title { font-size: 1.05rem; font-weight: 600; color: #c9d1d9; margin-bottom: 6px; }
      .empty-state .empty-sub { font-size: 0.8rem; }

      /* ---- Load / resume hint ---- */
      .load-hint {
        margin-top: 14px; padding: 10px 12px; border-radius: 6px;
        background: #0d1117; border: 1px solid #21262d;
        font-size: 0.72rem; color: #8b949e; line-height: 1.5;
      }

      /* ---- Dark theme: nav tabs / card / accordion ---- */
      .card { background: #161b22; border: 1px solid #30363d; border-radius: 10px; }
      .card-header { background: #161b22; border-bottom: 1px solid #30363d; padding: 0; }
      .nav-tabs { border-bottom: 1px solid #30363d; }
      .nav-tabs .nav-link {
        color: #8b949e; border: none; border-radius: 0; padding: 12px 20px;
        font-family: 'IBM Plex Sans', sans-serif; font-size: 0.78rem; font-weight: 600;
        letter-spacing: 0.05em; text-transform: uppercase;
      }
      .nav-tabs .nav-link:hover { color: #e6edf3; background: rgba(88,166,255,0.06); }
      .nav-tabs .nav-link.active { color: #58a6ff; background: transparent; border-bottom: 2px solid #58a6ff; }
      .card-body { background: #0d1117; padding-top: 20px; }
      .accordion {
        --bs-accordion-bg: #161b22; --bs-accordion-border-color: #30363d;
        --bs-accordion-active-bg: #161b22; --bs-accordion-active-color: #e6edf3;
        --bs-accordion-btn-color: #8b949e; --bs-accordion-btn-bg: #161b22;
        --bs-accordion-btn-focus-box-shadow: 0 0 0 0.25rem rgba(88,166,255,0.15);
      }
      .accordion-button {
        background: #161b22; color: #c9d1d9; font-family: 'IBM Plex Sans', sans-serif;
        font-size: 0.72rem; font-weight: 600; letter-spacing: 0.08em; text-transform: uppercase;
      }
      .accordion-button:not(.collapsed) { background: #161b22; color: #58a6ff; }
      .accordion-button::after { filter: invert(0.7); }
      .accordion-body { background: #0d1117; }

      /* ---- Action button hover transitions ---- */
      .btn { transition: transform 0.12s ease, box-shadow 0.12s ease, background-color 0.12s ease; }
      .btn:hover { transform: translateY(-1px); }
      .btn-primary:hover { box-shadow: 0 4px 14px rgba(88,166,255,0.25); }
      .btn-danger:hover  { box-shadow: 0 4px 14px rgba(248,81,73,0.25); }
      .btn-outline-success:hover { box-shadow: 0 4px 14px rgba(63,185,80,0.25); }
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
      Shiny.addCustomMessageHandler('collapse-log', function(x) {
        var acc = document.getElementById('log_accordion');
        if (!acc) return;
        var coll = acc.querySelector('.accordion-collapse');
        if (coll) coll.classList.remove('show');
        var btn = acc.querySelector('.accordion-button');
        if (btn) { btn.classList.add('collapsed'); btn.setAttribute('aria-expanded', 'false'); }
      });
      function copyText(txt) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(txt);
        } else {
          var ta = document.createElement('textarea');
          ta.value = txt; document.body.appendChild(ta);
          ta.select(); try { document.execCommand('copy'); } catch(e) {}
          document.body.removeChild(ta);
        }
      }
      function copyFromAttr(el) { copyText(el.getAttribute('data-copy')); }
      document.addEventListener('click', function(e) {
        var seg = e.target.closest('.diag-seg');
        if (!seg) return;
        document.querySelectorAll('.diag-seg').forEach(function(s) { s.classList.remove('active'); });
        seg.classList.add('active');
      });
    "))
  ),

  div(class = "app-header",
      h2("\U0001f52c Clinical NLP Notes Pipeline"),
      div(class = "subtitle", uiOutput("header_subtitle"))
  ),

  div(class = "status-bar",
      div(class = "status-item",
          span(class = "status-item-label", "Diagnosis"),
          uiOutput("status_diag_badge")),
      div(class = "status-item",
          span(class = "status-item-label", "Provider"),
          uiOutput("status_provider")),
      div(class = "status-item",
          span(class = "status-item-label", "Database"),
          uiOutput("status_db_dot")),
      div(class = "status-item",
          span(class = "status-item-label", "Failures"),
          uiOutput("status_failures")),
      div(class = "status-progress",
          div(style = "display:flex;align-items:center;margin-bottom:4px;",
              uiOutput("progress_label"),
              span(class = "eta-badge", uiOutput("eta_label"))),
          div(class = "progress",
              div(id = "prog_bar", class = "progress-bar", role = "progressbar",
                  style = "width:0%", `aria-valuenow` = "0",
                  `aria-valuemin` = "0", `aria-valuemax` = "100")))
  ),

  navset_card_tab(
    nav_panel(
      "Pipeline",
      div(class = "section-card",
          div(class = "section-title", "SQL Query \u2014 Clarity Database"),
          textAreaInput("sql_query", label = NULL, value = "", rows = 12, width = "100%",
                        placeholder = "Paste your Clarity SQL query here...")
      ),
      div(class = "section-card",
          div(class = "section-title", "Extraction Prompt"),
          div(class = "form-label", "Diagnosis Preset"),
          div(class = "diag-seg-group",
              div(class = "diag-seg diag-NEC active", onclick = "Shiny.setInputValue('diagnosis_preset','NEC',{priority:'event'});", "NEC"),
              div(class = "diag-seg diag-IVH", onclick = "Shiny.setInputValue('diagnosis_preset','IVH',{priority:'event'});", "IVH"),
              div(class = "diag-seg diag-PVL", onclick = "Shiny.setInputValue('diagnosis_preset','PVL',{priority:'event'});", "PVL"),
              div(class = "diag-seg diag-IVH_PVL", onclick = "Shiny.setInputValue('diagnosis_preset','IVH_PVL',{priority:'event'});", "IVH+PVL"),
              div(class = "diag-seg diag-ROP", onclick = "Shiny.setInputValue('diagnosis_preset','ROP',{priority:'event'});", "ROP"),
              div(class = "diag-seg diag-PDA", onclick = "Shiny.setInputValue('diagnosis_preset','PDA',{priority:'event'});", "PDA"),
              div(class = "diag-seg diag-PDA_Echo", onclick = "Shiny.setInputValue('diagnosis_preset','PDA_Echo',{priority:'event'});", "PDA Echo"),
              div(class = "diag-seg diag-PDA_CXR", onclick = "Shiny.setInputValue('diagnosis_preset','PDA_CXR',{priority:'event'});", "PDA CXR"),
              div(class = "diag-seg diag-custom", onclick = "Shiny.setInputValue('diagnosis_preset','custom',{priority:'event'});", "Custom")
          ),
          hr(class = "divider-line"),
          div(class = "form-label", "Schema Fields"),
          uiOutput("schema_fields_ui"),
          hr(class = "divider-line"),
          div(class = "form-label", "Extraction Target"),
          textInput("prompt_target", label = NULL,
                    value = DIAGNOSIS_PRESETS$NEC$target, width = "100%"),
          div(class = "form-label", "Prompt Template  (%s = note text placeholder)"),
          textAreaInput("prompt_template", label = NULL,
                        value = DIAGNOSIS_PRESETS$NEC$prompt_template, rows = 7, width = "100%")
      ),
      div(class = "section-card",
          div(class = "section-title", "Run Pipeline"),
          fluidRow(
            column(7,
                   div(class = "form-label", "Output Directory"),
                   div(style = "display:flex;gap:6px;align-items:center;",
                       textInput("output_dir", label = NULL, value = path.expand("~"),
                                 placeholder = "e.g. C:/Users/you/Documents", width = "100%"),
                       uiOutput("output_dir_status"))),
            column(5,
                   div(class = "form-label", "Filename (no extension)"),
                   textInput("output_filename", label = NULL, value = "notes_llm_processed",
                             placeholder = "my_extraction_run", width = "100%"))
          ),
          uiOutput("output_path_preview"),
          hr(class = "divider-line"),
          fluidRow(
            column(4,
                   numericInput("autosave_every", "Autosave every N rows", value = 25, min = 5, max = 500, step = 5),
                   numericInput("sleep_secs", "Delay between API calls (s)", value = 2, min = 0, max = 30),
                   numericInput("conf_threshold", "Include confidence threshold", value = 0.7, min = 0, max = 1, step = 0.05),
                   numericInput("max_rows", "Max rows (0 = all)", value = 0, min = 0, max = 10000, step = 10)),
            column(4,
                   div(class = "run-checkboxes",
                       checkboxInput("use_csv_mode", "Use uploaded file (skip DB query)", value = FALSE),
                       checkboxInput("resume_skip", "Resume: skip rows already processed", value = TRUE),
                       checkboxInput("retry_failed", "Retry previously failed rows", value = TRUE))),
            column(4,
                   div(class = "run-actions",
                       actionButton("btn_run", "\u25b6  Run Pipeline",
                                    class = "btn btn-primary w-100",
                                    style = "font-family:'IBM Plex Mono';font-weight:700;letter-spacing:0.05em;"),
                       actionButton("btn_stop", "\u23f9  Stop",
                                    class = "btn btn-danger w-100",
                                    style = "font-family:'IBM Plex Mono';font-weight:600;"),
                       uiOutput("download_ui")))
          )
      ),
      div(class = "section-card",
          bslib::accordion(
            id = "log_accordion",
            open = TRUE,
            bslib::accordion_panel(
              title = "Activity Log",
              div(style = "display:flex;justify-content:flex-end;margin-bottom:6px;",
                  actionButton("btn_clear_log", "\U0001f5d1  Clear log",
                               class = "btn btn-sm btn-outline-secondary",
                               style = "font-family:'IBM Plex Mono';font-size:0.68rem;")),
              div(class = "log-box", verbatimTextOutput("log_output", placeholder = TRUE))
            )
          )
      )
    ),
    nav_panel(
      "Results",
      div(class = "section-card",
          div(class = "section-title", "Results Preview \u2014 Extracted Fields"),
          uiOutput("results_area")
      )
    ),
    nav_panel(
      "Config",
      fluidRow(
        column(6,
               div(class = "section-card",
                   div(class = "section-title", "System Status"),
                   div(class = "status-row",
                       span(class = "status-icon", uiOutput("llm_status_icon")),
                       div(div(class = "form-label", uiOutput("llm_status_label")),
                           div(uiOutput("llm_status_text")))
                   ),
                   div(class = "status-row",
                       span(class = "status-icon", uiOutput("db_icon")),
                       div(div(class = "form-label", "Database Connection"),
                           div(uiOutput("db_status_text")))
                   ),
                   fluidRow(
                     column(6, actionButton("btn_check_llm", "Check LLM",
                                            class = "btn btn-sm btn-outline-info w-100",
                                            style = "font-family:'IBM Plex Mono';font-size:0.72rem;")),
                     column(6, actionButton("btn_test_db", "Test DB",
                                            class = "btn btn-sm btn-outline-secondary w-100",
                                            style = "font-family:'IBM Plex Mono';font-size:0.72rem;"))
                   )
               ),
               div(class = "section-card",
                   div(class = "section-title", "Database Config"),
                   textInput("db_server", "Server", value = DEFAULT_DB_CONFIG$server),
                   fluidRow(
                     column(6, textInput("db_port", "Port", value = DEFAULT_DB_CONFIG$port)),
                     column(6, textInput("db_name", "Database", value = DEFAULT_DB_CONFIG$database))
                   ),
                   div(class = "form-label", style = "margin-top:10px;", "Authentication"),
                   div(class = "auth-radio",
                       radioButtons("db_auth_type", label = NULL,
                                    choices = c("Windows Authentication" = "windows",
                                                "Username / Password" = "userpass"),
                                    selected = "windows", inline = TRUE)),
                   conditionalPanel(
                     condition = "input.db_auth_type == 'userpass'",
                     div(class = "credentials-box",
                         textInput("db_username", "Username", value = "", placeholder = "domain\\username or login"),
                         passwordInput("db_password", "Password", value = ""))
                   )
               )
        ),
        column(6,
               div(class = "section-card",
                   div(class = "section-title", "LLM Provider"),
                   div(class = "provider-toggle",
                       radioButtons("llm_provider", label = NULL,
                                    choices = c("Vertex AI (GCP)" = "vertex", "LM Studio (Local)" = "lmstudio"),
                                    selected = "lmstudio", inline = TRUE)),
                   hr(class = "divider-line"),
                   conditionalPanel(
                     condition = "input.llm_provider == 'vertex'",
                     div(textInput("ai_project", "GCP Project", value = DEFAULT_VERTEX_CONFIG$project),
                         fluidRow(
                           column(6, textInput("ai_location", "Location", value = DEFAULT_VERTEX_CONFIG$location),
                                  helpText("\"global\" works for every current Gemini model. ",
                                           "\"us-central1\" and other regions only serve a subset ",
                                           "and will 404 for models not deployed there.")),
                           column(6, selectizeInput("ai_model_vertex", "Model",
                                                 choices = gemini_model_choices(),
                                                 selected = DEFAULT_VERTEX_CONFIG$model,
                                                 options = list(create = TRUE)))
                         ))
                   ),
                   conditionalPanel(
                     condition = "input.llm_provider == 'lmstudio'",
                     div(textInput("lm_base_url", "API Base URL", value = DEFAULT_LM_CONFIG$base_url,
                                   placeholder = "http://localhost:1234/v1"),
                         textInput("lm_model_input", "Model Name", value = DEFAULT_LM_CONFIG$model,
                                   placeholder = "e.g. biomistral, qwen2.5-7b-instruct"),
                         fluidRow(
                           column(6, actionButton("btn_fetch_models", "\U0001f4cb Fetch Models",
                                                  class = "btn btn-sm btn-outline-secondary w-100",
                                                  style = "font-family:'IBM Plex Mono';font-size:0.7rem;")),
                           column(6, div(style = "font-size:0.68rem;color:#8b949e;padding-top:4px;",
                                         "Click a model below to use it \u2193"))
                         ),
                         uiOutput("lm_model_list_ui"))
                   )
               )
        )
      )
    ),
    nav_panel(
      "Chat",
      div(class = "section-card",
          div(class = "section-title", "Chat \u2014 talk to the model directly"),
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              "Uses whichever backend and model are set in Config, as a free-form conversation \u2014 no JSON schema, no pipeline. Good for testing a prompt idea or sanity-checking that a model/location combination actually works before wiring it into an extraction run. Nothing here is saved to disk, and the conversation resets if you reload the page."),
          uiOutput("chat_history_ui"),
          hr(class = "divider-line"),
          fluidRow(
            column(10, textAreaInput("chat_input", label = NULL, rows = 3, width = "100%",
                                     placeholder = "Type a message... (Ctrl+Enter also sends)")),
            column(2,
                   div(style = "display:flex;flex-direction:column;gap:6px;height:100%;",
                       actionButton("chat_send", "Send", class = "btn btn-primary w-100",
                                    style = "font-family:'IBM Plex Mono';font-weight:700;"),
                       actionButton("chat_clear", "Clear chat", class = "btn btn-outline-secondary w-100",
                                    style = "font-family:'IBM Plex Mono';font-size:0.72rem;")))
          ),
          uiOutput("chat_status"),
          tags$script(HTML("
            $(document).on('keydown', '#chat_input', function(e) {
              if (e.ctrlKey && e.key === 'Enter') { $('#chat_send').click(); }
            });
          "))
      )
    ),
    nav_panel(
      "Usage & Cost",
      div(class = "section-card",
          div(class = "section-title", "Tokens, Duration & Estimated Cost"),
          uiOutput("usage_summary_ui"),
          hr(class = "divider-line"),
          div(class = "form-label", "Pricing (per 1,000,000 tokens, USD) \u2014 auto-filled from the Model selected in Config"),
          fluidRow(
            column(6, numericInput("cost_per_1m_input", "Input token price ($/1M)",
                                   value = model_pricing(DEFAULT_VERTEX_CONFIG$model)$price_in %||% 0,
                                   min = 0, step = 0.01)),
            column(6, numericInput("cost_per_1m_output", "Output token price ($/1M)",
                                   value = model_pricing(DEFAULT_VERTEX_CONFIG$model)$price_out %||% 0,
                                   min = 0, step = 0.01))
          ),
          uiOutput("model_price_note"),
          div(style = "font-size:0.72rem;color:#8b949e;",
              "Rates are standard-tier list prices checked Oct 2026; reasoning (\u201cthinking\u201d) tokens are billed as output and are included in Output Tokens. You can overwrite either field. LM Studio runs are local and always shown as $0.00."),
          hr(class = "divider-line"),
          actionButton("btn_reset_usage", "Reset counters",
                       class = "btn btn-sm btn-outline-secondary",
                       style = "font-family:'IBM Plex Mono';font-size:0.7rem;")
      ),

      div(class = "section-card",
          div(class = "section-title", "Hardware & Live Requests"),
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              "GPU, CPU and RAM read straight off this machine every few seconds \u2014 only meaningful when the backend is LM Studio (local). Vertex calls run in Google's datacenter and show up only in the request table below, with no GPU line, since there is no local GPU work to measure for them."),
          uiOutput("hw_tiles_ui"),
          hr(class = "divider-line"),
          div(class = "form-label", "Recent requests \u2014 this session, most recent first"),
          DT::dataTableOutput("hw_requests_dt")
      )
    ),
    nav_panel(
      "Model A/B",
      div(class = "section-card",
          div(class = "section-title", "Load the two runs"),
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              "Two exports of the SAME notes, scored by two different models. CSV or XLSX."),
          fluidRow(
            column(6,
                   div(class = "form-label", "Model A \u2014 file"),
                   fileInput("ab_file_a", label = NULL,
                             accept = c(".csv", ".xlsx", ".xls", ".txt"),
                             buttonLabel = "Browse", placeholder = "No file"),
                   uiOutput("ab_sheet_a_ui"),
                   textInput("ab_label_a", "Label for A", value = "Gemini 2.5 Flash")
            ),
            column(6,
                   div(class = "form-label", "Model B \u2014 file"),
                   fileInput("ab_file_b", label = NULL,
                             accept = c(".csv", ".xlsx", ".xls", ".txt"),
                             buttonLabel = "Browse", placeholder = "No file"),
                   uiOutput("ab_sheet_b_ui"),
                   textInput("ab_label_b", "Label for B", value = "LM Studio (local)")
            )
          ),
          uiOutput("ab_load_status")
      ),

      div(class = "section-card",
          div(class = "section-title", "Comparison settings"),
          fluidRow(
            column(4,
                   div(class = "form-label", "Match rows on"),
                   selectInput("ab_join_key", label = NULL, choices = character(0),
                               multiple = TRUE, width = "100%")),
            column(4,
                   div(class = "form-label", "Primary field (the yes/no call)"),
                   selectInput("ab_primary_field", label = NULL,
                               choices = character(0), width = "100%")),
            column(4,
                   div(class = "form-label", "Values that count as POSITIVE"),
                   textInput("ab_pos_vals", label = NULL, value = "true,yes,1",
                             width = "100%"))
          ),
          hr(class = "divider-line"),
          div(class = "form-label", "Scoring mode"),
          radioButtons("ab_mode", label = NULL,
                       choices = c(
                         "Agreement only \u2014 no ground truth" = "agree",
                         "Treat Model A as the reference" = "ref_a",
                         "Treat Model B as the reference" = "ref_b",
                         "Score both against a gold-standard column" = "gold"),
                       selected = "agree"),
          div(style = "font-size:0.72rem;color:#d29922;margin:-4px 0 8px 0;",
              "\u26A0 Precision, recall and F1 need a ground truth. With two model outputs and nothing else, ",
              "you are measuring AGREEMENT \u2014 if both models share a blind spot they agree perfectly and every ",
              "score looks excellent. Use a reference mode only to describe how B differs from A, and use gold ",
              "mode (a chart-reviewed column) for anything you intend to report as accuracy."),
          uiOutput("ab_gold_ui"),
          hr(class = "divider-line"),
          div(class = "form-label", "Other fields to compare (grade, laterality, confidence, ...)"),
          selectizeInput("ab_compare_fields", label = NULL, choices = character(0),
                         multiple = TRUE, width = "100%",
                         options = list(plugins = list("remove_button"))),
          hr(class = "divider-line"),
          actionButton("ab_run", "Run comparison", class = "btn btn-primary",
                       style = "font-family:'IBM Plex Mono';font-size:0.75rem;")
      ),

      uiOutput("ab_results_ui"),

      div(class = "section-card",
          div(class = "section-title", "Full report"),
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              "Methods, Statistical Analysis, Results, Discussion and a reproducibility appendix. Both formats render from the same content, so they always say the same thing."),
          fluidRow(
            column(5, textInput("ab_rep_title", "Report title",
                                value = "NICU LLM Extraction: Model Comparison Report",
                                width = "100%")),
            column(4, textInput("ab_rep_author", "Prepared by (optional)",
                                value = "", width = "100%")),
            column(3, numericInput("ab_rep_maxdis", "Max disagreement rows in the report",
                                   value = 50, min = 0, step = 10, width = "100%"))
          ),
          uiOutput("ab_report_status"),
          div(style = "display:flex;flex-wrap:wrap;gap:10px;align-items:center;",
              downloadButton("ab_dl_html", "Download report (.html)",
                             class = "btn btn-primary",
                             style = "font-family:'IBM Plex Mono';font-size:0.75rem;"),
              downloadButton("ab_dl_docx", "Download report (.docx)",
                             class = "btn btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.75rem;")),
          div(style = "font-size:0.72rem;color:#8b949e;margin-top:10px;line-height:1.6;",
              tags$b("HTML"), " needs no extra R packages, opens on any machine without Word, and prints ",
              "to PDF straight from the browser, so there is no LaTeX toolchain to install. It is one file ",
              "with nothing linked externally, which is what an archived audit record wants.",
              tags$br(),
              tags$b("Word"), " is for co-authors who track-change and for pasting tables into a ",
              "manuscript. It needs the officer package."),
          uiOutput("ab_docx_note")
      )
    ),
    nav_panel(
      "Gold-Standard Adjudication",
      div(class = "section-card",
          div(class = "section-title", "Load rows to review"),
          div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:10px;",
              "Reviewing the disagreement/no-call rows from a comparison is the cheap, sufficient way to rank two models. A sample of concordant rows, reviewed separately, is what catches an error the models share \u2014 upload one as a file here if you want that too."),
          radioButtons("adj_source", NULL, choices = c(
                         "From the last Model A/B run (disagreements)" = "ab_run",
                         "Upload a file" = "upload"), selected = "ab_run"),
          uiOutput("adj_source_status"),
          conditionalPanel(
            condition = "input.adj_source == 'upload'",
            fileInput("adj_file", NULL, accept = c(".csv", ".xlsx", ".xls", ".txt"),
                      buttonLabel = "Browse", placeholder = "No file"),
            div(style = "font-size:0.72rem;color:#8b949e;margin-bottom:6px;",
                "Multi-sheet Excel files use the first sheet \u2014 save as CSV first if that is not the one you want."),
            uiOutput("adj_keycol_ui")
          )
      ),
      div(class = "section-card",
          div(class = "section-title", "Who's reviewing"),
          fluidRow(
            column(6, textInput("adj_reviewer", "Your name or initials", value = "", width = "100%")),
            column(6, div(style = "padding-top:28px;",
                         checkboxInput("adj_skip_labeled",
                                      "Skip rows I've already adjudicated when I hit Next",
                                      value = TRUE)))
          ),
          div(style = "font-size:0.72rem;color:#8b949e;",
              "Each reviewer's calls are kept separately by name, so a second clinician can adjudicate the same rows independently \u2014 see Progress below for where you disagree with each other.")
      ),
      div(class = "section-card",
          div(class = "section-title", "Review"),
          fluidRow(
            column(8, uiOutput("adj_row_ui")),
            column(4,
                   div(class = "form-label", "Jump to row"),
                   selectInput("adj_jump", NULL, choices = character(0), width = "100%"),
                   div(style = "display:flex;gap:8px;margin-top:4px;margin-bottom:10px;",
                       actionButton("adj_prev", "\u2190 Prev",
                                   style = "font-family:'IBM Plex Mono';font-size:0.72rem;"),
                       actionButton("adj_next", "Next \u2192",
                                   style = "font-family:'IBM Plex Mono';font-size:0.72rem;")),
                   hr(class = "divider-line"),
                   div(class = "form-label", "Adjudicated call"),
                   radioButtons("adj_call", NULL, choices = c(
                                  "Positive" = "positive", "Negative" = "negative",
                                  "Uncertain / cannot determine" = "uncertain")),
                   textAreaInput("adj_notes", "Rationale (optional)", rows = 3, width = "100%"),
                   actionButton("adj_save", "Save & next", class = "btn btn-primary",
                               style = "font-family:'IBM Plex Mono';font-size:0.75rem;width:100%;")
            )
          )
      ),
      div(class = "section-card",
          div(class = "section-title", "Progress & inter-rater agreement"),
          uiOutput("adj_progress_ui"),
          conditionalPanel(
            condition = "output.adj_has_conflicts",
            div(style = "margin-top:10px;",
                div(class = "form-label", "Rows where reviewers disagree"),
                DT::dataTableOutput("adj_conflicts_table")))
      ),
      div(class = "section-card",
          div(class = "section-title", "Export"),
          div(style = "display:flex;flex-wrap:wrap;gap:10px;align-items:center;",
              downloadButton("adj_dl_all", "Download all adjudications (long format)",
                             class = "btn btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.75rem;"),
              downloadButton("adj_dl_gold", "Download consensus gold-standard CSV",
                             class = "btn btn-primary",
                             style = "font-family:'IBM Plex Mono';font-size:0.75rem;")),
          div(style = "font-size:0.72rem;color:#8b949e;margin-top:10px;line-height:1.6;",
              "The consensus file has one row per note: ", tags$code("row_key"), " + ", tags$code("gold_call"),
              ", plus how many reviewers scored it and whether they agreed. Rows marked CONFLICT need a ",
              "third opinion before they can be used as ground truth.",
              tags$br(),
              "Upload that file as the gold file back in the Model A/B tab (Scoring mode \u2192 \u2018gold\u2019) ",
              "to score both models against it automatically \u2014 it is matched on the same row key, no manual merge needed.")
      )
    ),
    nav_panel(
      "Load / Resume",
      div(class = "section-card",
          div(class = "section-title", "Resume / Load from File"),
          div(class = "file-upload-area",
              fileInput("upload_file", label = NULL,
                        buttonLabel = "\U0001f4c2  Choose File",
                        placeholder = "CSV \u00b7 XLSX \u00b7 TXT \u2014 drag & drop",
                        accept = c(".csv", ".xlsx", ".xls", ".txt", "text/csv",
                                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                   "application/vnd.ms-excel", "text/plain"))),
          uiOutput("file_upload_status"),
          conditionalPanel(condition = "output.show_sheet_selector",
                           div(style = "margin-top:8px;", uiOutput("sheet_selector_ui"))),
          div(class = "load-hint",
              "Upload a previously saved CSV / XLSX / TXT of notes to resume an extraction, or run in \u2018use uploaded file\u2019 mode from the Pipeline tab.",
              tags$br(),
              "With \u2018Retry previously failed rows\u2019 ticked, any row whose review_status is failed_api / failed_parse is re-attempted on the next run.")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    gcp_ok = NULL, gcp_token = NULL, gcp_msg = "",
    lm_ok = NULL, lm_msg = "", lm_models = character(0),
    db_ok = NULL, db_msg = "",
    log_lines = character(0), notes_data = NULL,
    active_diagnosis = "NEC",
    processing = FALSE, stop_flag = FALSE,
    progress_i = 0, progress_n = 0, start_time = NULL,
    selected_row = NULL,
    n_failed = 0,
    # Token usage / cost / duration — reset at the start of each run,
    # accumulated across every API call (including retries) in that run.
    total_input_tokens = 0, total_output_tokens = 0, total_api_calls = 0,
    total_thought_tokens = 0,
    run_elapsed_secs = 0, run_finished = FALSE, run_model = NULL,
    # Per-call history for the Usage & Cost "Hardware & Live Requests" panel.
    # Capped at 200 rows (most recent kept) so a long run doesn't grow this
    # without bound in memory.
    request_log = data.frame(
      time = character(0), source = character(0), status = character(0),
      prompt_tokens = numeric(0), output_tokens = numeric(0),
      duration_secs = numeric(0), tok_per_sec = numeric(0),
      stringsAsFactors = FALSE
    )
  )

  log_add <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "]  ", paste(...))
    rv$log_lines <- c(rv$log_lines, msg)
  }

  # source: "Pipeline" or "Chat", for the Recent Requests table.
  record_request <- function(source, status, prompt_tokens, output_tokens, duration_secs) {
    out_tok <- num0(output_tokens)
    tok_s <- if (duration_secs > 0) round(out_tok / duration_secs, 1) else NA_real_
    row <- data.frame(
      time = format(Sys.time(), "%H:%M:%S"),
      source = source, status = status,
      prompt_tokens = num0(prompt_tokens), output_tokens = out_tok,
      duration_secs = round(duration_secs, 1), tok_per_sec = tok_s,
      stringsAsFactors = FALSE
    )
    updated <- rbind(rv$request_log, row)
    if (nrow(updated) > 200) updated <- updated[(nrow(updated) - 199):nrow(updated), ]
    rv$request_log <- updated
  }

  # NA-safe numeric coalesce — a token count missing from an API response
  # should add 0, not poison the running total with NA.
  num0 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) 0 else as.numeric(x)

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

  # ── Chat tab ──
  chat_rv <- reactiveValues(turns = list(), busy = FALSE)

  output$chat_history_ui <- renderUI({
    turns <- chat_rv$turns
    if (length(turns) == 0)
      return(div(style = "color:#8b949e;font-size:0.8rem;padding:10px 0;",
                 "No messages yet \u2014 say something below."))
    div(style = "max-height:420px;overflow-y:auto;padding:4px 2px;",
        tagList(lapply(turns, function(t) {
          is_user <- identical(t$role, "user")
          is_err  <- identical(t$role, "error")
          div(style = sprintf(
                "margin-bottom:10px;padding:8px 12px;border-radius:8px;max-width:85%%;%s background:%s;",
                if (is_user) "margin-left:auto;" else "margin-right:auto;",
                if (is_err) "#da363322" else if (is_user) "#1f6feb22" else "#30363d55"
              ),
              div(style = "font-size:0.65rem;color:#8b949e;margin-bottom:3px;font-family:'IBM Plex Mono';text-transform:uppercase;letter-spacing:0.05em;",
                  if (is_user) "You" else if (is_err) "Error" else "Model"),
              div(style = "white-space:pre-wrap;font-size:0.85rem;line-height:1.5;", t$text))
        })))
  })

  output$chat_status <- renderUI({
    if (isTRUE(chat_rv$busy))
      div(style = "font-size:0.75rem;color:#8b949e;margin-top:6px;", "Waiting for a response\u2026")
    else NULL
  })

  observeEvent(input$chat_clear, {
    chat_rv$turns <- list()
  })

  observeEvent(input$chat_send, {
    msg <- trimws(input$chat_input %||% "")
    if (msg == "" || isTRUE(chat_rv$busy)) return()

    chat_rv$turns <- c(chat_rv$turns, list(list(role = "user", text = msg)))
    updateTextAreaInput(session, "chat_input", value = "")
    chat_rv$busy <- TRUE
    on.exit(chat_rv$busy <- FALSE)

    provider <- input$llm_provider %||% "lmstudio"

    if (provider == "vertex" && !isTRUE(rv$gcp_ok)) {
      chat_rv$turns <- c(chat_rv$turns, list(list(
        role = "error",
        text = "Not authenticated to Vertex AI \u2014 go to Config and click \u201cCheck LLM\u201d first (this runs gcloud auth application-default login under the hood if needed)."
      )))
      return()
    }

    chat_t0 <- Sys.time()
    result <- withProgress(message = "Waiting for model\u2026", value = 0.5, {
      if (provider == "vertex") {
        call_gemini_chat(chat_rv$turns, rv$gcp_token, build_vertex_cfg())
      } else {
        call_lmstudio_chat(chat_rv$turns, input$lm_base_url, input$lm_model_input)
      }
    })
    chat_dur <- as.numeric(difftime(Sys.time(), chat_t0, units = "secs"))

    # Neither chat helper returns a token count (they're free-form, not run
    # through the schema path that parses usageMetadata) — approximate output
    # tokens from characters so the tok/s column in Usage & Cost still has
    # something comparable, rather than leaving every chat row blank.
    approx_tokens <- function(txt) if (is.na(txt)) 0 else round(nchar(txt) / 4)

    if (!is.na(result$text)) {
      chat_rv$turns <- c(chat_rv$turns, list(list(role = "assistant", text = result$text)))
      record_request("Chat", "Done", approx_tokens(msg), approx_tokens(result$text), chat_dur)
    } else {
      chat_rv$turns <- c(chat_rv$turns, list(list(
        role = "error",
        text = paste0("Request failed: ", result$reason)
      )))
      record_request("Chat", "Error", approx_tokens(msg), 0, chat_dur)
    }
  })

  # ── Diagnosis preset switching ──
  observeEvent(input$diagnosis_preset, {
    key <- input$diagnosis_preset
    preset <- DIAGNOSIS_PRESETS[[key]]
    if (is.null(preset)) return()
    updateTextInput(session, "prompt_target", value = preset$target)
    updateTextAreaInput(session, "prompt_template", value = preset$prompt_template)
    cur_fname <- trimws(input$output_filename)
    if (cur_fname == "" || grepl("^notes_llm|_nlp_extraction$", cur_fname))
      updateTextInput(session, "output_filename", value = paste0(tolower(key), "_nlp_extraction"))
    log_add(sprintf("Preset switched to: %s", preset$label))
  }, ignoreInit = TRUE)

  output$schema_fields_ui <- renderUI({
    key <- if (!is.null(input$diagnosis_preset)) input$diagnosis_preset else "NEC"
    preset <- DIAGNOSIS_PRESETS[[key]]
    if (is.null(preset)) return(NULL)
    field_pills <- lapply(preset$col_labels, function(lbl) span(class = "schema-field", lbl))
    div(class = "schema-fields", tagList(field_pills))
  })

  output$header_subtitle <- renderUI({
    provider_label <- if (!is.null(input$llm_provider) && input$llm_provider == "vertex")
      "Clarity EHR \u00b7 Vertex AI / Gemini" else "Clarity EHR \u00b7 LM Studio (Local)"
    span(provider_label)
  })

  output$llm_status_label <- renderUI({
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex")
      "GCP Authentication" else "LM Studio Connection"
  })

  output$llm_status_icon <- renderUI({
    ok <- if (!is.null(input$llm_provider) && input$llm_provider == "vertex") rv$gcp_ok else rv$lm_ok
    if (is.null(ok)) span("\u25cb", class = "status-pending")
    else if (ok)     span("\u2713", class = "status-ok")
    else             span("\u2717", class = "status-fail")
  })

  output$llm_status_text <- renderUI({
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex") {
      if (is.null(rv$gcp_ok)) span("Not checked", class = "pill-tag pill-pending")
      else if (rv$gcp_ok)     span("Authenticated", class = "pill-tag pill-success")
      else                    span(rv$gcp_msg, class = "pill-tag pill-danger")
    } else {
      if (is.null(rv$lm_ok)) span("Not checked", class = "pill-tag pill-pending")
      else if (rv$lm_ok) {
        n <- length(rv$lm_models)
        span(if (n > 0) sprintf("Connected \u00b7 %d model(s)", n) else "Connected",
             class = "pill-tag pill-success")
      } else span(rv$lm_msg, class = "pill-tag pill-danger")
    }
  })

  observeEvent(input$btn_check_llm, {
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex") {
      log_add("Checking GCP authentication...")
      gcp <- check_gcp_auth()
      rv$gcp_ok <- gcp$ok; rv$gcp_token <- gcp$token
      rv$gcp_msg <- if (gcp$ok) "Authenticated via ADC" else gcp$msg
      log_add(if (gcp$ok) "\u2713 GCP auth OK" else paste("\u2717 GCP auth failed:", gcp$msg))
    } else {
      log_add("Testing LM Studio connection at", input$lm_base_url, "...")
      result <- check_lmstudio(input$lm_base_url)
      rv$lm_ok <- result$ok
      rv$lm_msg <- if (result$ok) "Connected" else result$msg
      rv$lm_models <- if (!is.null(result$models)) result$models else character(0)
      if (result$ok) {
        log_add(sprintf("\u2713 LM Studio OK \u2014 %d model(s)", length(rv$lm_models)))
        if (length(rv$lm_models) > 0) log_add("  Models:", paste(rv$lm_models, collapse = ", "))
      } else log_add("\u2717 LM Studio failed:", result$msg)
    }
  })

  observeEvent(input$btn_fetch_models, {
    log_add("Fetching models from LM Studio...")
    result <- check_lmstudio(input$lm_base_url)
    rv$lm_ok <- result$ok
    rv$lm_msg <- if (result$ok) "Connected" else result$msg
    rv$lm_models <- if (!is.null(result$models)) result$models else character(0)
    if (result$ok && length(rv$lm_models) > 0) {
      log_add(sprintf("\u2713 %d model(s):", length(rv$lm_models)))
      for (m in rv$lm_models) log_add("   \u2022", m)
    } else if (result$ok) log_add("\u2713 Connected but no models loaded")
    else log_add("\u2717 Failed:", result$msg)
  })

  output$lm_model_list_ui <- renderUI({
    models <- rv$lm_models
    if (length(models) == 0) return(NULL)
    items <- lapply(models, function(m) {
      div(class = "model-item",
          onclick = sprintf("Shiny.setInputValue('lm_model_click', '%s', {priority: 'event'});", m), m)
    })
    tagList(
      div(class = "form-label", style = "margin-top:10px;", "Available Models (click to select)"),
      div(class = "model-list-box", tagList(items)))
  })

  observeEvent(input$lm_model_click, {
    updateTextInput(session, "lm_model_input", value = input$lm_model_click)
    log_add("Model set to:", input$lm_model_click)
  })

  observeEvent(input$btn_test_db, {
    log_add("Testing database connection...")
    cfg <- build_db_cfg()
    conn <- tryCatch(connect_clarity(cfg), error = function(e) e)
    if (inherits(conn, "error")) {
      rv$db_ok <- FALSE; rv$db_msg <- conditionMessage(conn)
      log_add("\u2717 DB connection failed:", rv$db_msg)
    } else {
      rv$db_ok <- TRUE; rv$db_msg <- paste("Connected to", input$db_name, "on", input$db_server)
      log_add("\u2713 DB connection OK"); DBI::dbDisconnect(conn)
    }
  })

  output$db_icon <- renderUI({
    if (is.null(rv$db_ok)) span("\u25cb", class = "status-pending")
    else if (rv$db_ok)     span("\u2713", class = "status-ok")
    else                   span("\u2717", class = "status-fail")
  })
  output$db_status_text <- renderUI({
    if (is.null(rv$db_ok)) span("Not checked", class = "pill-tag pill-pending")
    else if (rv$db_ok)     span("Connected", class = "pill-tag pill-success")
    else                   span("Failed", class = "pill-tag pill-danger")
  })

  # ── Persistent status bar ──
  output$status_diag_badge <- renderUI({
    key <- if (!is.null(input$diagnosis_preset)) input$diagnosis_preset else "NEC"
    preset <- DIAGNOSIS_PRESETS[[key]]
    if (is.null(preset)) preset <- DIAGNOSIS_PRESETS[["NEC"]]
    span(preset$label, class = paste0("pill-tag diag-", key))
  })

  output$status_provider <- renderUI({
    if (!is.null(input$llm_provider) && input$llm_provider == "vertex")
      span("Vertex AI", class = "pill-tag pill-info")
    else
      span("LM Studio (Local)", class = "pill-tag pill-pending")
  })

  output$status_db_dot <- renderUI({
    if (is.null(rv$db_ok)) span("\u25cb Not checked", class = "status-dot status-pending")
    else if (rv$db_ok)     span("\u25cf Connected", class = "status-dot status-ok")
    else                   span("\u25cf Failed", class = "status-dot status-fail")
  })

  # Failure counter — makes silent NA impossible to miss.
  output$status_failures <- renderUI({
    if (is.null(rv$notes_data)) return(span("\u2014", class = "pill-tag pill-pending"))
    if (!("review_status" %in% names(rv$notes_data)))
      return(span("\u2014", class = "pill-tag pill-pending"))
    rs <- rv$notes_data$review_status
    n_fail <- sum(!is.na(rs) & rs %in% c("failed_api", "failed_parse"), na.rm = TRUE)
    n_na   <- sum(is.na(rs), na.rm = TRUE)
    if (n_fail == 0 && n_na == 0)
      span("0", class = "pill-tag pill-success")
    else
      span(sprintf("%d failed%s", n_fail, if (n_na > 0) sprintf(" \u00b7 %d unprocessed", n_na) else ""),
           class = "pill-tag pill-danger")
  })

  output$file_upload_status <- renderUI({
    req(input$upload_file)
    ext <- tolower(tools::file_ext(input$upload_file$name))
    icon <- switch(ext, csv = "\U0001f4c4", xlsx = "\U0001f4ca", xls = "\U0001f4ca", txt = "\U0001f4dd", "\U0001f4c1")
    span(paste(icon, input$upload_file$name), class = "pill-tag pill-success",
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
    sheets <- xlsx_sheets(); req(sheets)
    selectInput("xlsx_sheet", "Sheet", choices = sheets, selected = sheets[1])
  })

  uploaded_data <- reactive({
    req(input$upload_file)
    ext <- tolower(tools::file_ext(input$upload_file$name))
    path <- input$upload_file$datapath; name <- input$upload_file$name
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        sheet <- if (!is.null(input$xlsx_sheet)) input$xlsx_sheet else 1
        readxl::read_excel(path, sheet = sheet)
      } else load_uploaded_file(path, name)
    }, error = function(e) { log_add("\u2717 File load error:", e$message); NULL })
  })

  output$log_output <- renderText({ paste(tail(rv$log_lines, 200), collapse = "\n") })

  observeEvent(input$btn_clear_log, {
    rv$log_lines <- character(0)
  })

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
    span(sprintf("ETA ~%dm %ds", floor(remaining / 60), round(remaining %% 60)))
  })

  observeEvent(input$btn_stop, {
    rv$stop_flag <- TRUE; log_add("\u23f9 Stop requested.")
  })

  resolved_output_path <- reactive({
    fname <- trimws(input$output_filename); dname <- trimws(input$output_dir)
    if (fname == "") fname <- "notes_llm_processed"
    fname <- gsub("\\.csv$", "", fname, ignore.case = TRUE)
    fname <- gsub("[^A-Za-z0-9_-]", "_", fname)
    list(dir = dname, base = fname,
         final = file.path(dname, paste0(fname, ".csv")),
         autosave = file.path(dname, paste0(fname, "_AUTOSAVE.csv")))
  })

  output$output_dir_status <- renderUI({
    d <- trimws(input$output_dir); if (d == "") return(NULL)
    if (!dir.exists(d)) span("\u2717", title = "Does not exist", class = "status-fail", style = "font-size:1rem;")
    else if (file.access(d, 2) != 0) span("\u26a0", title = "Not writable", class = "status-fail", style = "font-size:1rem;")
    else span("\u2713", title = "OK", class = "status-ok", style = "font-size:1rem;")
  })

  output$output_path_preview <- renderUI({
    p <- resolved_output_path()
    div(style = "font-family:'IBM Plex Mono',monospace;font-size:0.7rem;color:#8b949e;margin-bottom:4px;",
        sprintf("\u2192 %s", p$final))
  })

  output$download_ui <- renderUI({
    req(!is.null(rv$notes_data))
    downloadButton("btn_download", "\u2b07  Download CSV",
                   class = "btn btn-outline-success w-100 mt-2",
                   style = "font-family:'IBM Plex Mono';font-size:0.78rem;")
  })

  output$btn_download <- downloadHandler(
    filename = function() {
      fname <- trimws(input$output_filename)
      if (fname == "") fname <- "notes_llm_processed"
      fname <- gsub("\\.csv$", "", fname, ignore.case = TRUE)
      fname <- gsub("[^A-Za-z0-9_-]", "_", fname)
      paste0(fname, ".csv")
    },
    content = function(file) readr::write_csv(rv$notes_data, file)
  )

  # ── Results table ──
  present_col_name <- reactive({
    key <- rv$active_diagnosis
    preset <- DIAGNOSIS_PRESETS[[key]]
    preset$cols[1]
  })

  filtered_data <- reactive({
    req(rv$notes_data)
    df <- rv$notes_data; key <- rv$active_diagnosis; preset <- DIAGNOSIS_PRESETS[[key]]
    id_cols <- intersect(c("OR_CASE_ID", "NOTE_ID"), names(df))
    diag_cols <- intersect(preset$cols, names(df))
    extra_cols <- c(grep("review_status$", names(df), value = TRUE),
                    intersect("extract_error", names(df)))
    out_cols <- c(id_cols, diag_cols, extra_cols)
    df <- df[, out_cols, drop = FALSE]
    present_diag <- intersect(preset$cols, names(df))
    label_map <- setNames(preset$col_labels, preset$cols)
    names(df)[names(df) %in% present_diag] <- label_map[present_diag]
    pcol <- label_map[preset$cols[1]]
    filt <- input$filter_target
    if (!is.null(filt) && pcol %in% names(df)) {
      if (filt == "true")  df <- df[!is.na(df[[pcol]]) & tolower(df[[pcol]]) == "true",  , drop = FALSE]
      if (filt == "false") df <- df[!is.na(df[[pcol]]) & tolower(df[[pcol]]) == "false", , drop = FALSE]
      if (filt == "na")    df <- df[is.na(df[[pcol]]), , drop = FALSE]
    }
    rfilt <- input$filter_review
    if (!is.null(rfilt) && rfilt != "all" && "review_status" %in% names(df)) {
      if (rfilt == "include")
        df <- df[!is.na(df$review_status) & df$review_status == "include", , drop = FALSE]
      else if (rfilt == "review")
        df <- df[!is.na(df$review_status) & df$review_status %in% c("review_low_conf", "review_no_conf"), , drop = FALSE]
      else if (rfilt == "exclude")
        df <- df[!is.na(df$review_status) & df$review_status %in% c("exclude_negated", "exclude_absent"), , drop = FALSE]
      else if (rfilt == "no_call")
        df <- df[!is.na(df$review_status) & df$review_status == "no_call", , drop = FALSE]
      else if (rfilt == "failed")
        df <- df[is.na(df$review_status) |
                   df$review_status %in% c("failed_api", "failed_parse", "skipped_empty_note"), , drop = FALSE]
    }
    df
  })

  output$results_area <- renderUI({
    if (is.null(rv$notes_data)) {
      div(class = "empty-state",
          div(class = "empty-icon", "\U0001f52c"),
          div(class = "empty-title", "No results yet"),
          div(class = "empty-sub", "Run a pipeline to see results here"))
    } else {
      tagList(
        uiOutput("failure_banner"),
        div(class = "filter-bar",
            tags$label("Filter by present:", style = "font-size:0.72rem;color:#8b949e;"),
            selectInput("filter_target", label = NULL,
                        choices = c("All" = "all", "TRUE" = "true", "FALSE" = "false", "NA" = "na"),
                        selected = "all", width = "150px"),
            tags$label("Review status:", style = "font-size:0.72rem;color:#8b949e;"),
            selectInput("filter_review", label = NULL,
                        choices = c("All" = "all", "Include" = "include",
                                    "Needs review" = "review", "Excluded" = "exclude",
                                    "No call" = "no_call", "Failed / unprocessed" = "failed"),
                        selected = "all", width = "180px"),
            uiOutput("results_count_badge")),
        DT::dataTableOutput("results_table"),
        bslib::accordion(
          id = "note_accordion",
          open = TRUE,
          bslib::accordion_panel(
            title = "Note Preview",
            uiOutput("note_preview_header"),
            uiOutput("note_preview_box")
          )
        )
      )
    }
  })

  output$failure_banner <- renderUI({
    req(rv$notes_data)
    if (!("review_status" %in% names(rv$notes_data))) return(NULL)
    rs <- rv$notes_data$review_status
    n_fail <- sum(!is.na(rs) & rs %in% c("failed_api", "failed_parse"), na.rm = TRUE)
    n_na   <- sum(is.na(rs), na.rm = TRUE)
    if (n_fail == 0 && n_na == 0) return(NULL)
    div(class = "failure-banner",
        sprintf("\u26a0 %d row(s) failed extraction%s. Set 'Review status' to 'Failed / unprocessed' to inspect them \u2014 the extract_error column gives the reason. Re-run with 'Retry previously failed rows' ticked to re-attempt.",
                n_fail,
                if (n_na > 0) sprintf(" and %d row(s) were never processed", n_na) else ""))
  })

  output$results_count_badge <- renderUI({
    req(rv$notes_data); key <- rv$active_diagnosis
    badge_class <- paste0("pill-tag diag-", key)
    tagList(
      span(sprintf("%d / %d rows", nrow(filtered_data()), nrow(rv$notes_data)), class = "pill-tag pill-pending"),
      span(DIAGNOSIS_PRESETS[[key]]$label, class = badge_class, style = "margin-left:6px;"))
  })

  output$results_table <- DT::renderDataTable({
    df <- filtered_data()
    DT::datatable(head(df, 200), selection = "single",
                  options = list(pageLength = 10, scrollX = TRUE, dom = "tp",
                                 initComplete = DT::JS(
                                   "function(s,j){$(this.api().table().container()).css({'background':'#0d1117','color':'#e6edf3'});}")),
                  rownames = FALSE, class = "display compact")
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
        sprintf("Note Text Preview \u2014 NOTE_ID %s", rv$selected_row$NOTE_ID))
  })

  output$note_preview_box <- renderUI({
    if (is.null(rv$selected_row)) {
      return(div(class = "note-preview-placeholder",
                 "Select a row in the results table to preview the full note text."))
    }
    row <- rv$selected_row
    txt <- if ("NOTE_TEXT" %in% names(row)) row$NOTE_TEXT else "(no text)"
    nm  <- names(row)
    val_ok <- function(x) !is.null(x) && length(x) > 0 && !is.na(x) &&
      nzchar(as.character(x)) && !(tolower(as.character(x)) %in% c("na", "null"))
    rs_cols <- nm[grepl("review_status$", nm)]
    ev_cols <- nm[grepl("evidence_span$", nm)]
    rs_pills <- lapply(rs_cols, function(c) {
      if (!val_ok(row[[c]])) return(NULL)
      lbl <- sub("_?review_status$", "", c)
      if (lbl == "") lbl <- "review"
      cls <- if (as.character(row[[c]]) %in% c("failed_api", "failed_parse", "skipped_empty_note"))
        "pill-tag pill-danger" else "pill-tag pill-info"
      span(class = cls, style = "margin-right:6px;", sprintf("%s: %s", lbl, row[[c]]))
    })
    err_box <- NULL
    if ("extract_error" %in% nm && val_ok(row[["extract_error"]])) {
      err_box <- div(class = "error-highlight",
                     div(class = "evidence-label", "Extraction error (why this row has no result)"),
                     as.character(row[["extract_error"]]))
    }
    ev_boxes <- lapply(ev_cols, function(c) {
      if (!val_ok(row[[c]])) return(NULL)
      lbl <- sub("_?evidence_span$", "", c)
      if (lbl == "") lbl <- "evidence"
      div(class = "evidence-highlight",
          div(class = "evidence-label", sprintf("%s evidence span (verify verbatim in note)", toupper(lbl))),
          as.character(row[[c]]))
    })
    mrn_val  <- if ("MRN" %in% nm) as.character(row$MRN) else ""
    note_val <- if ("NOTE_ID" %in% nm) as.character(row$NOTE_ID) else ""
    copy_btn <- function(label, val) {
      if (!nzchar(val)) return(NULL)
      tags$button(class = "btn btn-sm btn-outline-secondary copy-btn",
                  `data-copy` = val, onclick = "copyFromAttr(this);", label)
    }
    tagList(
      div(class = "note-copy-row",
          copy_btn("Copy MRN", mrn_val),
          copy_btn("Copy NOTE_ID", note_val)),
      if (length(Filter(Negate(is.null), rs_pills)) > 0) div(style = "margin:6px 0;", rs_pills),
      err_box,
      ev_boxes,
      div(class = "note-preview-box", txt)
    )
  })

  # ── Main pipeline ──
  observeEvent(input$btn_run, {
    if (rv$processing) { showNotification("Pipeline already running.", type = "warning"); return() }
    rv$stop_flag <- FALSE; rv$processing <- TRUE; rv$notes_data <- NULL; rv$selected_row <- NULL
    rv$start_time <- Sys.time(); rv$progress_i <- 0; rv$progress_n <- 0; rv$n_failed <- 0
    rv$total_input_tokens <- 0; rv$total_output_tokens <- 0; rv$total_api_calls <- 0
    rv$total_thought_tokens <- 0
    rv$run_elapsed_secs <- 0; rv$run_finished <- FALSE

    provider <- input$llm_provider
    rv$run_model <- if (provider == "vertex") input$ai_model_vertex else input$lm_model_input
    vertex_cfg <- build_vertex_cfg(); lm_cfg <- build_lm_cfg(); db_cfg <- build_db_cfg()
    preset_key <- input$diagnosis_preset; preset <- DIAGNOSIS_PRESETS[[preset_key]]
    active_schema <- preset$schema; active_cols <- preset$cols
    prompt_tmpl <- input$prompt_template
    conf_threshold <- suppressWarnings(as.numeric(input$conf_threshold))
    if (is.na(conf_threshold)) conf_threshold <- 0.7

    log_add("\u2550\u2550\u2550 Pipeline started \u2550\u2550\u2550")
    log_add(sprintf("Diagnosis: %s", preset$label))
    log_add(sprintf("Provider:  %s", if (provider == "vertex") "Vertex AI (Gemini)" else paste("LM Studio \u2014", lm_cfg$model)))

    on.exit({ rv$processing <- FALSE }, add = TRUE)

    withProgress(message = "Running pipeline...", value = 0, {
      if (provider == "vertex") {
        log_add("Checking GCP authentication...")
        gcp <- check_gcp_auth()
        rv$gcp_ok <- gcp$ok; rv$gcp_token <- gcp$token
        rv$gcp_msg <- if (gcp$ok) "Authenticated via ADC" else gcp$msg
        if (!gcp$ok) { log_add("\u2717 GCP auth failed. Aborting."); showNotification(paste("GCP auth failed:", gcp$msg), type = "error", duration = NULL); return() }
        log_add("\u2713 GCP auth OK")
      } else {
        log_add("Verifying LM Studio connection...")
        lm_check <- check_lmstudio(lm_cfg$base_url)
        rv$lm_ok <- lm_check$ok; rv$lm_msg <- if (lm_check$ok) "Connected" else lm_check$msg
        if (!lm_check$ok) { log_add("\u2717 LM Studio not reachable. Aborting."); showNotification(paste("LM Studio error:", lm_check$msg), type = "error", duration = NULL); return() }
        log_add(sprintf("\u2713 LM Studio OK (model: %s)", lm_cfg$model))
      }

      notes_data <- NULL; want_file_mode <- isTRUE(input$use_csv_mode)
      if (want_file_mode && is.null(input$upload_file)) {
        log_add("\u2717 'Use uploaded file' is checked, but no file has finished uploading yet.")
        showNotification("No file uploaded \u2014 choose a file in 'Resume / Load from File' and wait for the green filename badge before running.",
                         type = "error", duration = NULL)
        return()
      }
      use_file <- want_file_mode && !is.null(input$upload_file)
      if (use_file) {
        log_add(sprintf("Loading from uploaded file: %s", input$upload_file$name))
        notes_data <- tryCatch(uploaded_data(), error = function(e) { log_add("\u2717 File error:", e$message); NULL })
        if (!is.null(notes_data)) log_add(sprintf("\u2713 Loaded %d rows", nrow(notes_data)))
      } else {
        if (trimws(input$sql_query) == "") { log_add("\u2717 SQL query empty."); showNotification("SQL query is empty.", type = "error", duration = NULL); return() }
        log_add("Connecting to Clarity...")
        conn <- tryCatch(connect_clarity(db_cfg), error = function(e) e)
        if (inherits(conn, "error")) { rv$db_ok <- FALSE; rv$db_msg <- conditionMessage(conn); log_add("\u2717 DB failed:", rv$db_msg); showNotification("DB connection failed.", type = "error", duration = NULL); return() }
        rv$db_ok <- TRUE; rv$db_msg <- "Connected"; log_add("\u2713 DB connected \u2014 running SQL...")
        notes_data <- tryCatch(DBI::dbGetQuery(conn, input$sql_query), error = function(e) { log_add("\u2717 SQL error:", e$message); NULL })
        DBI::dbDisconnect(conn)
        if (!is.null(notes_data)) log_add(sprintf("\u2713 Query returned %d rows", nrow(notes_data)))
      }

      if (is.null(notes_data)) { log_add("\u2717 No data."); return() }
      if (!("NOTE_TEXT" %in% names(notes_data))) { log_add("\u2717 NOTE_TEXT column missing."); showNotification("NOTE_TEXT column missing.", type = "error", duration = NULL); return() }

      for (col in active_cols) { if (!(col %in% names(notes_data))) notes_data[[col]] <- NA_character_ }
      if (!("review_status" %in% names(notes_data))) notes_data[["review_status"]] <- NA_character_
      if (!("extract_error"  %in% names(notes_data))) notes_data[["extract_error"]]  <- NA_character_
      if (!is.null(preset$review_specs))
        for (rspec in preset$review_specs)
          if (!(rspec$out %in% names(notes_data))) notes_data[[rspec$out]] <- NA_character_
      rv$active_diagnosis <- preset_key

      max_r <- as.integer(input$max_rows)
      n_rows <- if (max_r > 0) min(nrow(notes_data), max_r) else nrow(notes_data)
      if (max_r > 0 && max_r < nrow(notes_data)) log_add(sprintf("\u2139 Cap: %d of %d rows", n_rows, nrow(notes_data)))

      rv$progress_n <- n_rows; rv$progress_i <- 0; rv$start_time <- Sys.time(); rv$notes_data <- notes_data
      log_add(sprintf("Extracting %d rows...", n_rows))

      resume_skip  <- isTRUE(input$resume_skip)
      retry_failed <- isTRUE(input$retry_failed)
      present_marker <- preset$present_field; if (is.null(present_marker)) present_marker <- preset$cols[1]
      FAILED_STATUSES <- c("failed_api", "failed_parse")

      # A row counts as "already done" only if it has a NON-failure status.
      # Failure statuses are re-attempted when retry_failed is TRUE.
      is_done <- function(idx) {
        rsv <- if ("review_status" %in% names(notes_data)) notes_data[["review_status"]][idx] else NA
        pmv <- if (present_marker %in% names(notes_data)) notes_data[[present_marker]][idx] else NA
        has_rs <- !is.na(rsv) && nzchar(trimws(as.character(rsv)))
        has_pm <- !is.na(pmv) && nzchar(trimws(as.character(pmv)))
        if (has_rs && retry_failed && as.character(rsv) %in% FAILED_STATUSES) return(FALSE)
        has_rs || has_pm
      }

      n_skipped <- 0L; n_failed_run <- 0L
      if (resume_skip) {
        already <- sum(vapply(seq_len(n_rows), is_done, logical(1)))
        if (already > 0) log_add(sprintf("\u21bb Resume on: %d of %d rows already processed will be skipped.", already, n_rows))
        if (retry_failed) log_add("\u21bb Retry mode: rows marked failed_api / failed_parse WILL be re-attempted.")
      }

      for (i in seq_len(n_rows)) {
        if (rv$stop_flag) { log_add("\u23f9 Stopped at row", i); break }

        if (resume_skip && is_done(i)) {
          n_skipped <- n_skipped + 1L
          rv$progress_i <- i
          incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d (skipped)", i, n_rows))
          next
        }

        if (provider == "vertex" && i > 1 && i %% 200 == 0) {
          log_add("[INFO] Refreshing GCP token...")
          gcp <- check_gcp_auth()
          if (!gcp$ok) { log_add("\u2717 Token refresh failed."); break }
          rv$gcp_token <- gcp$token; log_add("\u2713 Token refreshed")
        }

        current_text <- notes_data$NOTE_TEXT[i]
        if (is.na(current_text) || nchar(trimws(current_text)) < 5) {
          # TERMINAL STATUS — never leave this row silently blank.
          notes_data[["review_status"]][i] <- "skipped_empty_note"
          notes_data[["extract_error"]][i] <- "NOTE_TEXT was empty or shorter than 5 characters."
          log_add(sprintf("  -> Row %d skipped: NOTE_TEXT empty/too short.", i))
          rv$progress_i <- i
          incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d", i, n_rows))
          next
        }

        log_add(sprintf("Processing row %d...", i))
        prompt_tmpl_row <- prompt_tmpl
        if (!is.null(preset$tier_column) && preset$tier_column %in% names(notes_data)) {
          tier_raw <- trimws(as.character(notes_data[[preset$tier_column]][i]))
          tier_label <- switch(tier_raw,
                               "1" = "Tier 1 \u2014 Echocardiogram / TTE (cardiac ultrasound)",
                               "2" = "Tier 2 \u2014 Chest X-Ray (CXR)",
                               paste0("Unknown tier: ", tier_raw)
          )
          prompt_tmpl_row <- gsub("\\{TIER\\}", tier_label, prompt_tmpl_row)
        }
        full_prompt <- sprintf(prompt_tmpl_row, current_text)
        success <- FALSE; attempts <- 0
        last_reason <- NA_character_; last_detail <- NA_character_; last_raw <- NA_character_

        while (!success && attempts < 3) {
          attempts <- attempts + 1
          if (attempts > 1) log_add(sprintf("  -> Retry attempt %d...", attempts))

          call_t0 <- Sys.time()
          api_result <- call_llm(full_prompt, provider, rv$gcp_token, vertex_cfg, lm_cfg, active_schema)
          call_dur <- as.numeric(difftime(Sys.time(), call_t0, units = "secs"))
          json_response <- api_result$text; finish_reason <- api_result$reason; err_detail <- api_result$detail
          if (!is.null(api_result$usage)) {
            rv$total_input_tokens  <- rv$total_input_tokens  + num0(api_result$usage$input_tokens)
            rv$total_output_tokens <- rv$total_output_tokens + num0(api_result$usage$output_tokens)
            rv$total_api_calls     <- rv$total_api_calls + 1
            rv$total_thought_tokens <- rv$total_thought_tokens + num0(api_result$usage$thought_tokens)
          }
          record_request("Pipeline", if (!is.na(json_response)) "Done" else "Error",
                         api_result$usage$input_tokens %||% 0, api_result$usage$output_tokens %||% 0, call_dur)
          last_reason <- as.character(finish_reason)
          if (!is.null(err_detail) && length(err_detail) > 0)
            last_detail <- substr(gsub("\\s+", " ", as.character(err_detail)), 1, 300)

          if (!is.na(json_response)) {
            last_raw <- substr(json_response, 1, 300)
            clean_json <- repair_json(clean_json_fence(json_response))
            parse_result <- tryCatch(list(ok = TRUE, parsed = jsonlite::fromJSON(clean_json)),
                                     error = function(e) list(ok = FALSE, msg = e$message))
            if (parse_result$ok) {
              parsed <- parse_result$parsed
              for (col in active_cols) {
                val <- parsed[[col]]
                if (is.null(val)) {
                  notes_data[[col]][i] <- NA_character_
                } else if (is.list(val) || length(val) > 1) {
                  notes_data[[col]][i] <- paste(unlist(val), collapse = "; ")
                } else {
                  notes_data[[col]][i] <- as.character(val)
                }
              }
              if (!is.null(preset$review_specs)) {
                st <- character(0)
                for (rspec in preset$review_specs) {
                  s <- .review_status_core(parsed[[rspec$present_field]], parsed[[rspec$negated_field]],
                                           parsed[[rspec$conf_field]], rspec$present_true, conf_threshold)
                  notes_data[[rspec$out]][i] <- s
                  st <- c(st, s)
                }
                notes_data[["review_status"]][i] <- .rollup_review_status(st)
              } else {
                notes_data[["review_status"]][i] <- compute_review_status(parsed, preset, conf_threshold)
              }
              # Clear any stale error from a previous failed run of this row.
              notes_data[["extract_error"]][i] <- NA_character_
              if (grepl("SCHEMA_FALLBACK", last_reason, fixed = TRUE))
                log_add("  -> Recovered via schema-free fallback.")
              success <- TRUE
            } else {
              last_reason <- paste0("JSON_PARSE_ERROR: ", parse_result$msg)
              log_add(sprintf("  -> JSON error (attempt %d): %s", attempts, parse_result$msg))
              log_add(sprintf("     Raw: %s", substr(json_response, 1, 200)))
            }
          } else if (grepl("NETWORK_CRASH", last_reason, fixed = TRUE)) {
            log_add("  -> Network error. Waiting 15s..."); Sys.sleep(15)
          } else if (grepl("HTTP_ERROR", last_reason, fixed = TRUE)) {
            log_add(sprintf("  -> %s: %s", last_reason, ifelse(is.na(last_detail), "", last_detail)))
            if (grepl("429", last_reason, fixed = TRUE)) { log_add("  -> Rate limited. Waiting 30s..."); Sys.sleep(30) }
          } else {
            log_add(sprintf("  -> No usable response. reason: %s", last_reason))
          }
          if (input$sleep_secs > 0) Sys.sleep(input$sleep_secs)
        }

        # ---------------------------------------------------------------------
        # TERMINAL STATUS GUARANTEE
        # If every attempt failed, this row STILL gets a status and a reason.
        # A row can never again come back with review_status = NA and no clue why.
        # ---------------------------------------------------------------------
        if (!success) {
          is_parse_fail <- grepl("JSON_PARSE_ERROR", last_reason %||% "", fixed = TRUE)
          notes_data[["review_status"]][i] <- if (is_parse_fail) "failed_parse" else "failed_api"
          notes_data[["extract_error"]][i] <- paste0(
            "attempts=", attempts,
            " | reason=", last_reason %||% "unknown",
            if (!is.na(last_detail)) paste0(" | detail=", last_detail) else "",
            if (!is.na(last_raw)) paste0(" | raw=", last_raw) else ""
          )
          n_failed_run <- n_failed_run + 1L
          log_add(sprintf("  \u2717 Row %d FAILED after %d attempts \u2014 status=%s, reason=%s",
                          i, attempts,
                          notes_data[["review_status"]][i],
                          substr(last_reason %||% "unknown", 1, 120)))
        }

        rv$progress_i <- i
        incProgress(1 / max(n_rows, 1), detail = sprintf("Row %d / %d", i, n_rows))

        if (i %% input$autosave_every == 0) {
          autosave_path <- resolved_output_path()$autosave
          tryCatch(readr::write_csv(notes_data, autosave_path),
                   error = function(e) log_add("  -> Autosave failed:", e$message))
          log_add(sprintf("  -> Autosaved at row %d", i))
          rv$notes_data <- notes_data
        }
      }

      rv$notes_data <- notes_data
      rv$n_failed <- n_failed_run
      out_path <- resolved_output_path()
      tryCatch({
        if (!dir.exists(out_path$dir)) dir.create(out_path$dir, recursive = TRUE)
        readr::write_csv(notes_data, out_path$final)
        log_add(sprintf("\u2713 Done \u2014 saved to %s", out_path$final))
      }, error = function(e) {
        fallback <- paste0(out_path$base, ".csv")
        readr::write_csv(notes_data, fallback)
        log_add(sprintf("\u26a0 Fallback save: %s", fallback))
      })

      if (resume_skip && n_skipped > 0)
        log_add(sprintf("\u21bb Skipped %d already-processed rows; processed %d new.", n_skipped, n_rows - n_skipped))

      # Final reconciliation — prove every processed row has a status.
      rs_final <- notes_data[["review_status"]][seq_len(n_rows)]
      n_na_final <- sum(is.na(rs_final))
      if (n_failed_run > 0)
        log_add(sprintf("\u26a0 %d row(s) failed extraction. Filter Results by 'Failed / unprocessed' and read extract_error.", n_failed_run))
      if (n_na_final > 0)
        log_add(sprintf("\u26a0 %d row(s) still have no status (stopped early?).", n_na_final))
      else
        log_add("\u2713 Status check: every processed row has a review_status.")

      rv$run_elapsed_secs <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
      rv$run_finished <- TRUE
      log_add(sprintf("\u2139 Tokens used: %s in / %s out across %d call(s) \u2014 elapsed %s",
                      format(round(rv$total_input_tokens), big.mark = ","),
                      format(round(rv$total_output_tokens), big.mark = ","),
                      rv$total_api_calls,
                      format(round(rv$run_elapsed_secs), big.mark = ",")))
      log_add("\u2550\u2550\u2550 Pipeline complete \u2550\u2550\u2550")
      session$sendCustomMessage("collapse-log", list())
    })
  })

  # Model dropdown -> auto-fill the Usage & Cost price fields
  observeEvent(input$ai_model_vertex, {
    p <- model_pricing(input$ai_model_vertex)
    if (!is.null(p)) {
      updateNumericInput(session, "cost_per_1m_input",  value = p$price_in)
      updateNumericInput(session, "cost_per_1m_output", value = p$price_out)
    }
  }, ignoreInit = FALSE)

  output$model_price_note <- renderUI({
    m <- input$ai_model_vertex
    p <- model_pricing(m)
    msg <- NULL
    if (is.null(p)) {
      msg <- sprintf("%s is not in the price list \u2014 enter its rates manually.", m)
    } else if (nzchar(p$note)) {
      msg <- sprintf("%s: %s", m, p$note)
    }
    if (is.null(msg)) return(NULL)
    div(style = "font-size:0.72rem;color:#d29922;margin:2px 0 6px 0;", "\u26a0 ", msg)
  })

  observeEvent(input$btn_reset_usage, {
    rv$total_thought_tokens <- 0
    rv$total_input_tokens <- 0; rv$total_output_tokens <- 0; rv$total_api_calls <- 0
    rv$run_elapsed_secs <- 0; rv$run_finished <- FALSE
    log_add("Usage counters reset.")
  })

  # ── Usage & Cost tab ──
  output$usage_summary_ui <- renderUI({
    in_tok  <- rv$total_input_tokens  %||% 0
    out_tok <- rv$total_output_tokens %||% 0
    tot_tok <- in_tok + out_tok
    calls   <- rv$total_api_calls %||% 0

    # Live-update elapsed time while a run is in progress; freeze once done.
    elapsed <- if (isTRUE(rv$processing) && !is.null(rv$start_time)) {
      as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    } else {
      rv$run_elapsed_secs %||% 0
    }

    fmt_duration <- function(s) {
      s <- max(0, round(s))
      h <- s %/% 3600L; m <- (s %% 3600L) %/% 60L; sec <- s %% 60L
      if (h > 0) sprintf("%dh %dm %ds", h, m, sec)
      else if (m > 0) sprintf("%dm %ds", m, sec)
      else sprintf("%ds", sec)
    }

    price_in  <- suppressWarnings(as.numeric(input$cost_per_1m_input));  if (is.na(price_in))  price_in  <- 0
    price_out <- suppressWarnings(as.numeric(input$cost_per_1m_output)); if (is.na(price_out)) price_out <- 0
    cost_in   <- (in_tok  / 1e6) * price_in
    cost_out  <- (out_tok / 1e6) * price_out
    cost_tot  <- cost_in + cost_out

    is_local <- !is.null(input$llm_provider) && input$llm_provider == "lmstudio"

    tile <- function(label, value, sub = NULL) {
      div(style = "background:#161b22;border:1px solid #30363d;border-radius:8px;padding:12px 16px;min-width:150px;flex:1 1 150px;",
          div(style = "font-size:0.68rem;color:#8b949e;text-transform:uppercase;letter-spacing:0.05em;", label),
          div(style = "font-family:'IBM Plex Mono';font-size:1.3rem;font-weight:700;color:#e6edf3;margin-top:4px;", value),
          if (!is.null(sub)) div(style = "font-size:0.7rem;color:#8b949e;margin-top:2px;", sub))
    }

    sel_model <- if (is_local) input$lm_model_input else input$ai_model_vertex
    run_model <- rv$run_model
    mismatch  <- !is.null(run_model) && !identical(run_model, sel_model) && calls > 0
    thought   <- rv$total_thought_tokens %||% 0

    div(
      if (mismatch)
        div(style = "font-size:0.75rem;color:#d29922;margin-bottom:8px;",
            sprintf("\u26a0 Last run used %s, but prices below are for %s. Switch the Model back to %s for an accurate cost.",
                    run_model, sel_model, run_model)),
      div(style = "display:flex;flex-wrap:wrap;gap:12px;",
          tile("Model", run_model %||% sel_model %||% "\u2014",
               if (is_local) "LM Studio" else "Vertex AI"),
          tile("API Calls", format(calls, big.mark = ",")),
          tile("Input Tokens", format(round(in_tok), big.mark = ",")),
          tile("Output Tokens", format(round(out_tok), big.mark = ","),
               if (thought > 0) sprintf("incl. %s reasoning", format(round(thought), big.mark = ",")) else NULL),
          tile("Total Tokens", format(round(tot_tok), big.mark = ",")),
          tile(if (isTRUE(rv$processing)) "Elapsed (running)" else "Run Time",
               fmt_duration(elapsed),
               if (calls > 0) sprintf("%.1fs / call avg", elapsed / calls) else NULL),
          tile(if (is_local) "Cost (local = free)" else "Estimated Cost",
               if (is_local) "$0.00" else sprintf("$%.4f", cost_tot),
               if (!is_local) sprintf("in $%.4f + out $%.4f", cost_in, cost_out) else "LM Studio runs locally")
      )
    )
  })
  # =========================================================================
  # MODEL A/B COMPARISON TAB
  # =========================================================================
  ab_rv <- reactiveValues(res = NULL, err = NULL)

  ab_tile <- function(label, value, sub = NULL, accent = "#e6edf3") {
    div(style = "background:#161b22;border:1px solid #30363d;border-radius:8px;padding:12px 16px;min-width:150px;flex:1 1 150px;",
        div(style = "font-size:0.68rem;color:#8b949e;text-transform:uppercase;letter-spacing:0.05em;", label),
        div(style = paste0("font-family:'IBM Plex Mono';font-size:1.3rem;font-weight:700;margin-top:4px;color:", accent, ";"), value),
        if (!is.null(sub)) div(style = "font-size:0.7rem;color:#8b949e;margin-top:2px;", sub))
  }

  # ── Hardware & Live Requests (Usage & Cost tab) ──
  output$hw_tiles_ui <- renderUI({
    invalidateLater(3000, session)
    gpu <- tryCatch(get_gpu_stats(), error = function(e) NULL)
    sys <- tryCatch(get_sys_stats(), error = function(e) NULL)

    last_tok_s <- {
      rl <- rv$request_log
      ok <- rl[rl$status == "Done" & !is.na(rl$tok_per_sec), , drop = FALSE]
      if (nrow(ok) > 0) ok$tok_per_sec[nrow(ok)] else NA_real_
    }

    tiles <- list(
      ab_tile("Speed", if (is.na(last_tok_s)) "\u2014" else sprintf("%.1f tok/s", last_tok_s), "last request")
    )

    if (!is.null(gpu)) {
      vram_pct <- if (!is.na(gpu$vram_total_mb) && gpu$vram_total_mb > 0)
        round(100 * gpu$vram_used_mb / gpu$vram_total_mb) else NA
      tiles <- c(tiles, list(
        ab_tile("GPU load", sprintf("%s%%", gpu$gpu_util_pct %||% "\u2014"), gpu$name),
        ab_tile("VRAM", sprintf("%.1f / %.1f GB", gpu$vram_used_mb / 1024, gpu$vram_total_mb / 1024),
               if (!is.na(vram_pct)) sprintf("%d%% used", vram_pct) else NULL,
               accent = if (!is.na(vram_pct) && vram_pct > 90) "#f85149" else "#e6edf3"),
        ab_tile("GPU temp", sprintf("%s\u00b0C", gpu$temp_c %||% "\u2014"), NULL,
               accent = if (!is.na(gpu$temp_c) && gpu$temp_c > 80) "#f85149" else "#e6edf3"),
        ab_tile("Power", sprintf("%s W", round(gpu$power_w %||% NA)),
               sprintf("of %s W limit", round(gpu$power_limit_w %||% NA)))
      ))
    } else {
      tiles <- c(tiles, list(
        ab_tile("GPU", "Not detected", "no nvidia-smi on this machine \u2014 normal if you're on Vertex AI only")
      ))
    }

    if (!is.null(sys)) {
      tiles <- c(tiles, list(
        ab_tile("CPU", if (is.na(sys$cpu_pct)) "\u2014" else sprintf("%.0f%%", sys$cpu_pct),
               sprintf("%s cores", sys$cores %||% "?")),
        ab_tile("System RAM",
               if (is.na(sys$ram_used_gb) || is.na(sys$ram_total_gb)) "\u2014"
               else sprintf("%.1f / %.1f GB", sys$ram_used_gb, sys$ram_total_gb))
      ))
    }

    div(style = "display:flex;flex-wrap:wrap;gap:10px;", tagList(tiles))
  })

  output$hw_requests_dt <- DT::renderDataTable({
    rl <- rv$request_log
    if (nrow(rl) == 0) {
      return(DT::datatable(
        data.frame(Message = "No requests yet this session."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    disp <- rl[rev(seq_len(nrow(rl))), , drop = FALSE]
    names(disp) <- c("Time", "Source", "Status", "Prompt tok", "Output tok", "Duration (s)", "Tok/s")
    DT::datatable(
      disp, rownames = FALSE, selection = "none",
      options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE,
                     order = list(), dom = "tip")
    )
  })

  ab_data_a <- reactive({
    fi <- input$ab_file_a; req(fi)
    ab_read_file(fi$datapath, fi$name, input$ab_sheet_a)
  })
  ab_data_b <- reactive({
    fi <- input$ab_file_b; req(fi)
    ab_read_file(fi$datapath, fi$name, input$ab_sheet_b)
  })

  output$ab_sheet_a_ui <- renderUI({
    req(input$ab_file_a)
    if (!(tolower(tools::file_ext(input$ab_file_a$name)) %in% c("xlsx", "xls"))) return(NULL)
    sh <- tryCatch(readxl::excel_sheets(input$ab_file_a$datapath), error = function(e) NULL)
    if (is.null(sh) || length(sh) == 0) return(NULL)
    selectInput("ab_sheet_a", "Sheet (A)", choices = sh, selected = sh[1], width = "100%")
  })
  output$ab_sheet_b_ui <- renderUI({
    req(input$ab_file_b)
    if (!(tolower(tools::file_ext(input$ab_file_b$name)) %in% c("xlsx", "xls"))) return(NULL)
    sh <- tryCatch(readxl::excel_sheets(input$ab_file_b$datapath), error = function(e) NULL)
    if (is.null(sh) || length(sh) == 0) return(NULL)
    selectInput("ab_sheet_b", "Sheet (B)", choices = sh, selected = sh[1], width = "100%")
  })

  output$ab_load_status <- renderUI({
    a <- tryCatch(ab_data_a(), error = function(e) e)
    b <- tryCatch(ab_data_b(), error = function(e) e)
    msgs <- list()
    if (inherits(a, "error")) msgs[[length(msgs) + 1]] <-
      div(class = "pill-tag pill-danger", paste("A:", conditionMessage(a)))
    if (inherits(b, "error")) msgs[[length(msgs) + 1]] <-
      div(class = "pill-tag pill-danger", paste("B:", conditionMessage(b)))
    if (length(msgs) > 0) return(div(style = "margin-top:8px;", msgs))
    if (is.null(input$ab_file_a) || is.null(input$ab_file_b))
      return(div(style = "font-size:0.72rem;color:#8b949e;margin-top:6px;",
                 "Waiting for both files."))
    shared <- intersect(names(a), names(b))
    div(style = "margin-top:8px;display:flex;flex-wrap:wrap;gap:6px;",
        span(class = "pill-tag pill-info", sprintf("A: %d rows \u00D7 %d cols", nrow(a), ncol(a))),
        span(class = "pill-tag pill-info", sprintf("B: %d rows \u00D7 %d cols", nrow(b), ncol(b))),
        span(class = if (length(shared) > 0) "pill-tag pill-success" else "pill-tag pill-danger",
             sprintf("%d shared column(s)", length(shared))),
        if (nrow(a) != nrow(b))
          span(class = "pill-tag pill-warn",
               sprintf("row counts differ by %d \u2014 unmatched rows are reported, not silently dropped",
                       abs(nrow(a) - nrow(b)))))
  })

  # Populate the key / field / compare selectors once both files are readable.
  observe({
    a <- tryCatch(ab_data_a(), error = function(e) NULL)
    b <- tryCatch(ab_data_b(), error = function(e) NULL)
    if (is.null(a) || is.null(b)) return()
    shared <- intersect(names(a), names(b))
    keys <- intersect(AB_KEY_CANDIDATES, shared)
    key_choices <- c(stats::setNames(keys, keys), "(match by row order)" = "__ROWORDER__")
    updateSelectInput(session, "ab_join_key", choices = key_choices,
                      selected = if (length(keys) > 0) keys[1] else "__ROWORDER__")

    cand <- setdiff(shared, c(AB_NON_COMPARE, AB_KEY_CANDIDATES))
    pk <- ab_detect_preset(shared)
    default_primary <- NULL
    if (!is.null(pk)) {
      pf <- DIAGNOSIS_PRESETS[[pk]]$present_field
      if (!is.null(pf) && pf %in% cand) default_primary <- pf
    }
    if (is.null(default_primary)) {
      g <- grep("_present$|_result$|^target_present$|supports", cand, value = TRUE)
      default_primary <- if (length(g) > 0) g[1] else if (length(cand) > 0) cand[1] else NULL
    }
    updateSelectInput(session, "ab_primary_field", choices = cand, selected = default_primary)

    # Default to the detected preset's OWN columns rather than every shared
    # column. A note pull carries the whole cohort frame alongside the model
    # output -- gestational age, birth weight, order IDs, department names --
    # and those are identical in both files by construction. Comparing them
    # yields a page of 100%-agreement rows that pad out the real findings.
    # Every column stays available in `choices` so nothing is hidden; only the
    # default selection narrows.
    drop_always <- c(default_primary, grep("evidence_span$", cand, value = TRUE))
    if (!is.null(pk) && !is.null(DIAGNOSIS_PRESETS[[pk]]$cols)) {
      extras <- setdiff(intersect(DIAGNOSIS_PRESETS[[pk]]$cols, cand), drop_always)
    } else {
      # No preset recognised, so there is no reliable way to tell model output
      # from passthrough. Select everything rather than guess with a name
      # pattern, which could silently hide a real extracted field.
      extras <- setdiff(cand, drop_always)
    }
    updateSelectizeInput(session, "ab_compare_fields", choices = cand, selected = extras)
  })

  output$ab_gold_ui <- renderUI({
    if (!identical(input$ab_mode, "gold")) return(NULL)
    a <- tryCatch(ab_data_a(), error = function(e) NULL)
    b <- tryCatch(ab_data_b(), error = function(e) NULL)
    if (is.null(a) || is.null(b))
      return(div(style = "font-size:0.72rem;color:#d29922;", "Load both files to pick a gold column."))
    ch <- c(stats::setNames(paste0(names(a), "__A"), paste0(names(a), "  (from A)")),
            stats::setNames(paste0(names(b), "__B"), paste0(names(b), "  (from B)")))
    guess <- grep("gold|truth|adjudicat|chart_review|reviewer|manual",
                  names(ch), ignore.case = TRUE, value = TRUE)
    div(
      div(class = "form-label", "Gold-standard column (expert chart review)"),
      selectInput("ab_gold_col", label = NULL, choices = ch,
                  selected = if (length(guess) > 0) ch[[guess[1]]] else NULL, width = "100%"),
      div(style = "font-size:0.72rem;color:#8b949e;",
          "The human label. Uses the same positive values as above. Rows where this is blank are left out of the scored denominator."),
      hr(class = "divider-line"),
      div(class = "form-label", "...or upload a gold CSV from the Adjudication tab"),
      fileInput("ab_gold_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
                buttonLabel = "Browse", placeholder = "No file"),
      div(style = "font-size:0.72rem;color:#8b949e;",
          "Exported by the Gold-Standard Adjudication tab: a ", tags$code("row_key"), " + ",
          tags$code("gold_call"), " column. Matched automatically on the same row key used for this ",
          "comparison, so no manual merge is needed \u2014 overrides the column picked above when a file is attached.")
    )
  })

  observeEvent(input$ab_run, {
    ab_rv$res <- NULL; ab_rv$err <- NULL
    a <- tryCatch(ab_data_a(), error = function(e) e)
    b <- tryCatch(ab_data_b(), error = function(e) e)
    if (inherits(a, "error") || inherits(b, "error")) {
      ab_rv$err <- "Could not read one of the files."; return()
    }
    a <- as.data.frame(a, stringsAsFactors = FALSE)
    b <- as.data.frame(b, stringsAsFactors = FALSE)

    pf <- input$ab_primary_field
    if (is.null(pf) || !nzchar(pf)) { ab_rv$err <- "Pick a primary field."; return() }
    if (!(pf %in% names(a)) || !(pf %in% names(b))) {
      ab_rv$err <- sprintf("Primary field '%s' is not in both files.", pf); return()
    }
    pos <- trimws(strsplit(input$ab_pos_vals %||% "", ",")[[1]])
    pos <- pos[nzchar(pos)]
    if (length(pos) == 0) pos <- c("true", "yes", "1")

    keys <- input$ab_join_key
    use_row <- length(keys) == 0 || "__ROWORDER__" %in% keys
    if (use_row) {
      if (nrow(a) != nrow(b)) {
        ab_rv$err <- sprintf(
          "Row-order matching needs equal row counts (A has %d, B has %d). Pick a key column instead.",
          nrow(a), nrow(b)); return()
      }
      a[["__ROWNUM__"]] <- seq_len(nrow(a))
      b[["__ROWNUM__"]] <- seq_len(nrow(b))
      keys <- "__ROWNUM__"
    }
    if (!all(keys %in% names(a)) || !all(keys %in% names(b))) {
      ab_rv$err <- "The chosen key column is missing from one of the files."; return()
    }

    # Raw counts captured BEFORE deduplication. Reporting only the post-dedup
    # count made a 51-row file look like a 19-row file in the Methods table.
    n_a_raw <- nrow(a); n_b_raw <- nrow(b)

    ka <- do.call(paste, c(lapply(keys, function(k) as.character(a[[k]])), sep = "\u001f"))
    kb <- do.call(paste, c(lapply(keys, function(k) as.character(b[[k]])), sep = "\u001f"))
    da <- duplicated(ka); db_ <- duplicated(kb)
    dup_a <- sum(da); dup_b <- sum(db_)

    # A key that collapses a large share of rows is almost always the wrong
    # granularity -- an encounter ID on note-level data, say, where every study
    # after the first is discarded and the comparison quietly runs on one
    # arbitrary note per encounter. Work out whether a finer key exists so the
    # warning can name it rather than just raise an alarm. This is evaluated on
    # the RAW frames, before the rows are dropped, or the counts would not be
    # comparable with dup_a.
    dup_frac <- max(if (n_a_raw > 0) dup_a / n_a_raw else 0,
                    if (n_b_raw > 0) dup_b / n_b_raw else 0)
    key_is_coarse <- any(keys %in% AB_COARSE_KEYS)
    finer_keys <- character(0)
    if (dup_a > 0 || dup_b > 0) {
      avail <- setdiff(intersect(AB_KEY_CANDIDATES, intersect(names(a), names(b))),
                       c(keys, "row_id"))
      for (cand_k in avail) {
        d_a <- sum(duplicated(as.character(a[[cand_k]])))
        d_b <- sum(duplicated(as.character(b[[cand_k]])))
        if (d_a < dup_a || d_b < dup_b) finer_keys <- c(finer_keys, cand_k)
      }
    }

    a <- a[!da, , drop = FALSE]; ka <- ka[!da]
    b <- b[!db_, , drop = FALSE]; kb <- kb[!db_]

    common <- intersect(ka, kb)
    if (length(common) == 0) {
      ab_rv$err <- "No rows matched on the chosen key. Check that the key column holds the same IDs in both files."
      return()
    }
    ia <- match(common, ka); ib <- match(common, kb)
    A <- a[ia, , drop = FALSE]; B <- b[ib, , drop = FALSE]

    shared <- intersect(names(a), names(b))
    joined <- data.frame(row_key = common, stringsAsFactors = FALSE)
    for (cn in shared) {
      joined[[paste0(cn, "__A")]] <- A[[cn]]
      joined[[paste0(cn, "__B")]] <- B[[cn]]
    }
    for (cn in setdiff(names(a), shared)) joined[[paste0(cn, "__A")]] <- A[[cn]]
    for (cn in setdiff(names(b), shared)) joined[[paste0(cn, "__B")]] <- B[[cn]]

    pa <- ab_to_logical(joined[[paste0(pf, "__A")]], pos)
    pb <- ab_to_logical(joined[[paste0(pf, "__B")]], pos)
    both <- !is.na(pa) & !is.na(pb)
    n_both <- sum(both)
    n_agree <- if (n_both > 0) sum(pa[both] == pb[both]) else 0
    kap <- ab_cohen_kappa(ifelse(is.na(pa), NA_character_, as.character(pa)),
                          ifelse(is.na(pb), NA_character_, as.character(pb)))
    mc <- ab_mcnemar(pa, pb)

    la <- if (nzchar(trimws(input$ab_label_a %||% ""))) trimws(input$ab_label_a) else "Model A"
    lb <- if (nzchar(trimws(input$ab_label_b %||% ""))) trimws(input$ab_label_b) else "Model B"
    # The audit table is grouped by label, so identical labels would merge the
    # two models into one set of rows.
    if (identical(la, lb)) { la <- paste0(la, " (A)"); lb <- paste0(lb, " (B)") }

    mk_row <- function(nm, ref_nm, m) data.frame(
      comparison = nm, reference = ref_nm,
      N_scored = m$N, TP = m$TP, FP = m$FP, FN = m$FN, TN = m$TN,
      precision_PPV = m$precision, recall_sensitivity = m$recall, F1 = m$f1,
      specificity = m$specificity, NPV = m$npv, accuracy = m$accuracy,
      precision_95CI = ab_fmt_rate(m$TP, m$TP + m$FP),
      recall_95CI    = ab_fmt_rate(m$TP, m$TP + m$FN),
      stringsAsFactors = FALSE)

    mode <- input$ab_mode %||% "agree"
    met_tbl <- NULL; gold_vec <- NULL; gc_label <- NULL; mode_note <- NULL
    if (identical(mode, "ref_a")) {
      met_tbl <- mk_row(lb, paste0(la, " (nominated reference)"), ab_binary_metrics(pa, pb))
      mode_note <- sprintf("%s is treated as the reference. These are agreement-relative figures, not validated accuracy \u2014 a shared error in both models cannot appear here.", la)
    } else if (identical(mode, "ref_b")) {
      met_tbl <- mk_row(la, paste0(lb, " (nominated reference)"), ab_binary_metrics(pb, pa))
      mode_note <- sprintf("%s is treated as the reference. These are agreement-relative figures, not validated accuracy \u2014 a shared error in both models cannot appear here.", lb)
    } else if (identical(mode, "gold")) {
      # A gold file exported by the Adjudication tab carries its own row_key,
      # built from the exact same join-key paste used here, so it matches
      # without the user doing a manual VLOOKUP/merge first.
      gf <- input$ab_gold_file
      if (!is.null(gf)) {
        gdf <- tryCatch(as.data.frame(ab_read_file(gf$datapath, gf$name), stringsAsFactors = FALSE),
                        error = function(e) NULL)
        if (is.null(gdf) || !("row_key" %in% names(gdf))) {
          ab_rv$err <- "The uploaded gold file needs a 'row_key' column, matching the Adjudication tab's export."
          return()
        }
        gcol <- if ("gold_call" %in% names(gdf)) "gold_call" else setdiff(names(gdf), "row_key")[1]
        m <- match(joined$row_key, as.character(gdf$row_key))
        # The Adjudication tab writes positive / negative / uncertain, which are
        # not in the user's "positive values" list (true,yes,1). Without this,
        # every adjudicated positive would silently score as a negative.
        # "uncertain" is a refusal to call, so it is left out, not scored as 0.
        gold_raw <- gdf[[gcol]][m]
        gold_vec <- ab_to_logical(gold_raw, c(pos, "positive"))
        gold_vec[ab_norm(gold_raw) %in% c("uncertain", "conflict")] <- NA
        gc_label <- paste0("uploaded gold file (", gf$name, ")")
      } else {
        gc <- input$ab_gold_col
        if (is.null(gc) || !(gc %in% names(joined))) {
          ab_rv$err <- "Pick a gold-standard column, or upload a gold file."; return()
        }
        gold_vec <- ab_to_logical(joined[[gc]], pos)
        gc_label <- gc
      }
      if (sum(!is.na(gold_vec)) == 0) {
        ab_rv$err <- sprintf("Gold column/file '%s' has no usable values on the matched rows.", gc_label); return()
      }
      met_tbl <- rbind(
        mk_row(la, paste0(gc_label, " (gold)"), ab_binary_metrics(gold_vec, pa)),
        mk_row(lb, paste0(gc_label, " (gold)"), ab_binary_metrics(gold_vec, pb)))
      mode_note <- sprintf("Both models scored against %s. These are true accuracy figures for the rows where the gold column is filled in (n = %d).",
                           gc_label, sum(!is.na(gold_vec)))
    } else {
      mode_note <- "Agreement only. No ground truth was supplied, so no precision or recall is reported \u2014 see the quality audit below for the checks that do not need one."
    }

    pk <- ab_detect_preset(shared)
    model_cols <- if (!is.null(pk)) DIAGNOSIS_PRESETS[[pk]]$cols else NULL

    flds <- unique(c(pf, input$ab_compare_fields))
    flds <- intersect(flds, shared)
    fld_tbl <- if (length(flds) > 0) ab_field_agreement(joined, flds, model_cols) else data.frame()

    aud <- rbind(
      ab_quality_audit(a, la, pf, pos, "NOTE_TEXT", pk),
      ab_quality_audit(b, lb, pf, pos, "NOTE_TEXT", pk))

    # ---- disagreement listing -------------------------------------------
    d_flip <- which(both & (pa != pb))
    d_one  <- which(xor(is.na(pa), is.na(pb)))
    idx <- c(d_flip, d_one)
    dtype <- c(rep("opposite call", length(d_flip)), rep("one model gave no call", length(d_one)))
    ev_a <- grep("evidence_span__A$", names(joined), value = TRUE)
    ev_b <- grep("evidence_span__B$", names(joined), value = TRUE)
    note_col <- if ("NOTE_TEXT__A" %in% names(joined)) "NOTE_TEXT__A" else
      if ("NOTE_TEXT__B" %in% names(joined)) "NOTE_TEXT__B" else NULL

    dis <- data.frame(row_key = character(0), stringsAsFactors = FALSE)
    if (length(idx) > 0) {
      dis <- data.frame(
        row_key = joined$row_key[idx],
        disagreement = dtype,
        stringsAsFactors = FALSE)
      dis[[paste0(pf, " [A] ", la)]] <- as.character(joined[[paste0(pf, "__A")]])[idx]
      dis[[paste0(pf, " [B] ", lb)]] <- as.character(joined[[paste0(pf, "__B")]])[idx]
      if (!is.null(gold_vec)) dis[["gold"]] <- as.character(gold_vec)[idx]
      for (ec in ev_a) dis[[paste0("A: ", sub("__A$", "", ec))]] <- as.character(joined[[ec]])[idx]
      for (ec in ev_b) dis[[paste0("B: ", sub("__B$", "", ec))]] <- as.character(joined[[ec]])[idx]
      if ("review_status__A" %in% names(joined)) dis[["A: review_status"]] <- as.character(joined[["review_status__A"]])[idx]
      if ("review_status__B" %in% names(joined)) dis[["B: review_status"]] <- as.character(joined[["review_status__B"]])[idx]
      if (!is.null(note_col)) dis[["NOTE_TEXT"]] <- as.character(joined[[note_col]])[idx]
    }

    hr_a <- aud[aud$model == la & grepl("^Hallucinated quote", aud$metric), , drop = FALSE]
    hr_b <- aud[aud$model == lb & grepl("^Hallucinated quote", aud$metric), , drop = FALSE]
    # NA (shown as an em dash), never 0, when the check could not be run --
    # a missing NOTE_TEXT column must not look like a clean bill of health.
    ab_hall_rate <- function(sub) {
      if (nrow(sub) == 0) return(NA_real_)
      den <- sum(sub$denom, na.rm = TRUE)
      if (!is.finite(den) || den == 0) return(NA_real_)
      sum(sub$count, na.rm = TRUE) / den
    }
    hall_a <- ab_hall_rate(hr_a)
    hall_b <- ab_hall_rate(hr_b)

    ab_rv$res <- list(
      la = la, lb = lb, pf = pf, pos = pos, mode = mode, mode_note = mode_note,
      n_a = nrow(a), n_b = nrow(b),
      n_a_raw = n_a_raw, n_b_raw = n_b_raw,
      n_matched = length(common),
      unmatched_a = nrow(a) - length(common), unmatched_b = nrow(b) - length(common),
      dup_a = dup_a, dup_b = dup_b, dup_frac = dup_frac,
      key_desc = paste(keys, collapse = " + "),
      key_is_coarse = key_is_coarse, finer_keys = finer_keys,
      n_both = n_both, n_agree = n_agree,
      pct_agree = if (n_both > 0) n_agree / n_both else NA_real_,
      kappa = kap, mc = mc,
      met_tbl = met_tbl, fld_tbl = fld_tbl, aud = aud, dis = dis,
      hall_a = hall_a, hall_b = hall_b, gold_col_label = gc_label,
      preset = pk, model_cols = model_cols, joined = joined)

    log_add(sprintf("A/B comparison: %d matched rows \u2014 %.1f%% agreement, kappa %s",
                    length(common), 100 * (if (n_both > 0) n_agree / n_both else NA_real_),
                    ab_fmt_num(kap)))
    if (dup_frac >= 0.10)
      log_add(sprintf("\u26a0 %.0f%% of rows dropped as duplicate keys on '%s' \u2014 check key granularity.",
                      100 * dup_frac, paste(keys, collapse = " + ")))
  })

  output$ab_results_ui <- renderUI({
    if (!is.null(ab_rv$err))
      return(div(class = "section-card",
                 div(style = "color:#f85149;font-size:0.8rem;", paste("\u2717", ab_rv$err))))
    r <- ab_rv$res
    if (is.null(r)) return(NULL)

    kap_txt <- ab_fmt_num(r$kappa)
    p_txt <- if (is.na(r$mc$p_value)) "\u2014" else
      if (r$mc$p_value < 0.001) "< 0.001" else sprintf("%.3f", r$mc$p_value)
    p_accent <- if (!is.na(r$mc$p_value) && r$mc$p_value < 0.05) "#d29922" else "#e6edf3"

    tagList(
      div(class = "section-card",
          div(class = "section-title", "Headline"),
          div(style = "display:flex;flex-wrap:wrap;gap:12px;",
              ab_tile("Matched rows", format(r$n_matched, big.mark = ","),
                      sprintf("A %s / B %s", format(r$n_a, big.mark = ","), format(r$n_b, big.mark = ","))),
              ab_tile("Agreement", if (is.na(r$pct_agree)) "\u2014" else sprintf("%.1f%%", 100 * r$pct_agree),
                      sprintf("%d of %d scored rows", r$n_agree, r$n_both)),
              ab_tile("Cohen's kappa", kap_txt, ab_kappa_label(r$kappa)),
              ab_tile("McNemar p", p_txt,
                      sprintf("%d discordant (%d A-only, %d B-only)",
                              r$mc$n_discordant, r$mc$a_only, r$mc$b_only), p_accent),
              ab_tile(paste0("Hallucination \u2014 ", r$la),
                      if (is.na(r$hall_a)) "\u2014" else sprintf("%.1f%%", 100 * r$hall_a),
                      "quotes not found in the note"),
              ab_tile(paste0("Hallucination \u2014 ", r$lb),
                      if (is.na(r$hall_b)) "\u2014" else sprintf("%.1f%%", 100 * r$hall_b),
                      "quotes not found in the note")
          ),
          div(style = "font-size:0.75rem;color:#8b949e;margin-top:10px;line-height:1.6;",
              sprintf("Primary field: %s. Positive = {%s}. %s",
                      r$pf, paste(r$pos, collapse = ", "),
                      if (!is.null(r$preset)) paste0("Detected preset: ", r$preset, ".") else "")),
          # A key that collapses a tenth or more of the data gets a loud banner,
          # not a grey footnote: every figure above is computed on what survived.
          if (isTRUE(r$dup_frac >= 0.10))
            div(style = paste0("background:rgba(248,81,73,0.10);border:1px solid rgba(248,81,73,0.45);",
                               "border-radius:6px;padding:10px 14px;margin-top:10px;",
                               "font-size:0.78rem;color:#f85149;line-height:1.6;"),
                tags$b(sprintf("\u26A0 %.0f%% of rows were discarded as duplicate keys.",
                               100 * r$dup_frac)),
                tags$br(),
                sprintf("A had %s rows, %s kept. B had %s rows, %s kept. Matching on %s.",
                        format(r$n_a_raw, big.mark = ","), format(r$n_a, big.mark = ","),
                        format(r$n_b_raw, big.mark = ","), format(r$n_b, big.mark = ","),
                        r$key_desc),
                tags$br(),
                if (isTRUE(r$key_is_coarse))
                  sprintf("%s identifies a patient or encounter, not a row. One encounter has many notes or studies, so all but the first were dropped and every figure above describes one arbitrary row per %s. ",
                          r$key_desc, r$key_desc)
                else
                  "Every row after the first for each key was dropped, so the figures above describe a subset. ",
                if (length(r$finer_keys) > 0)
                  tags$span("A finer key is available in both files: ",
                            tags$b(paste(r$finer_keys, collapse = ", ")),
                            ". Re-run with that selected.")
                else
                  "No finer key was found in both files; check whether the export carries a note or order identifier."),
          if (r$unmatched_a > 0 || r$unmatched_b > 0 || (r$dup_a > 0 && !isTRUE(r$dup_frac >= 0.10)))
            div(style = "font-size:0.75rem;color:#d29922;margin-top:6px;",
                sprintf("\u26A0 Unmatched: %d row(s) only in A, %d only in B. Duplicate keys dropped: %d in A, %d in B.",
                        r$unmatched_a, r$unmatched_b, r$dup_a, r$dup_b)),
          div(style = "font-size:0.75rem;color:#58a6ff;margin-top:8px;line-height:1.6;", r$mode_note),
          div(style = "font-size:0.72rem;color:#8b949e;margin-top:6px;line-height:1.6;",
              paste0("McNemar tests whether the two models differ on the same notes, using only the rows where ",
                     "they disagree \u2014 the rows they both call the same way carry no information about a difference. ",
                     "A small p means one model systematically calls more positives than the other; it does not say which one is right."))
      ),

      if (!is.null(r$met_tbl))
        div(class = "section-card",
            div(class = "section-title",
                if (identical(r$mode, "gold")) "Accuracy against gold standard" else "Relative metrics (not validated accuracy)"),
            DT::dataTableOutput("ab_met_table"),
            div(style = "font-size:0.72rem;color:#8b949e;margin-top:8px;line-height:1.6;",
                paste0("PPV = precision = of the rows this model called positive, the share that the reference also called positive. ",
                       "Sensitivity = recall = of the reference's positives, the share this model caught. ",
                       "Brackets are 95% Wilson intervals, which stay inside 0\u20131 at small counts where the usual normal interval does not."))),

      div(class = "section-card",
          div(class = "section-title", "Per-field agreement"),
          DT::dataTableOutput("ab_fld_table"),
          div(style = "font-size:0.72rem;color:#8b949e;margin-top:8px;line-height:1.6;",
              paste0("Sorted worst-agreement first. kappa corrects for agreement that happens by chance: with a rare ",
                     "finding, two models can match on 95% of rows simply because both call almost everything negative. ",
                     "mean_abs_diff is filled in only for numeric fields such as grade, stage or confidence."))),

      div(class = "section-card",
          div(class = "section-title", "Quality audit \u2014 no ground truth required"),
          DT::dataTableOutput("ab_aud_table"),
          div(style = "font-size:0.72rem;color:#8b949e;margin-top:8px;line-height:1.6;",
              paste0("Every row here is checkable from the file alone. A hallucinated quote means the model returned an ",
                     "evidence_span that is not actually in NOTE_TEXT \u2014 a model can be right about the diagnosis and still ",
                     "fabricate the quote, which defeats the audit trail. A positive call with no quote is the same problem ",
                     "in a different form. Schema violations mean the model ignored the declared field type."))),

      div(class = "section-card",
          div(class = "section-title",
              sprintf("Disagreements (%d)", nrow(r$dis))),
          if (nrow(r$dis) == 0)
            div(style = "font-size:0.8rem;color:#3fb950;", "\u2713 No disagreements on the primary field.")
          else tagList(
            DT::dataTableOutput("ab_dis_table"),
            div(style = "font-size:0.72rem;color:#8b949e;margin-top:8px;",
                "This is the adjudication worklist: these are the rows a human has to read to decide which model is right.")
          )),

      div(class = "section-card",
          div(class = "section-title", "Download"),
          div(style = "display:flex;flex-wrap:wrap;gap:10px;",
              downloadButton("ab_dl_dis", "Disagreements CSV", class = "btn btn-sm btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.7rem;"),
              downloadButton("ab_dl_met", "Metrics CSV", class = "btn btn-sm btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.7rem;"),
              downloadButton("ab_dl_fld", "Per-field agreement CSV", class = "btn btn-sm btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.7rem;"),
              downloadButton("ab_dl_aud", "Quality audit CSV", class = "btn btn-sm btn-outline-secondary",
                             style = "font-family:'IBM Plex Mono';font-size:0.7rem;")))
    )
  })

  ab_dt <- function(df, page = 10, trunc_cols = NULL) {
    d <- df
    if (!is.null(trunc_cols)) {
      for (cn in intersect(trunc_cols, names(d))) {
        v <- as.character(d[[cn]])
        long <- !is.na(v) & nchar(v) > 300
        v[long] <- paste0(substr(v[long], 1, 300), " \u2026")
        d[[cn]] <- v
      }
    }
    DT::datatable(d, selection = "none",
                  options = list(pageLength = page, scrollX = TRUE, dom = "tp",
                                 initComplete = DT::JS(
                                   "function(s,j){$(this.api().table().container()).css({'background':'#0d1117','color':'#e6edf3'});}")),
                  rownames = FALSE, class = "display compact")
  }

  output$ab_met_table <- DT::renderDataTable({
    r <- ab_rv$res; req(r); req(r$met_tbl)
    d <- r$met_tbl
    for (cn in c("precision_PPV", "recall_sensitivity", "F1", "specificity", "NPV", "accuracy")) {
      if (cn %in% names(d)) d[[cn]] <- round(d[[cn]], 3)
    }
    ab_dt(d, page = 5)
  })

  output$ab_fld_table <- DT::renderDataTable({
    r <- ab_rv$res; req(r)
    d <- r$fld_tbl
    if (is.null(d) || nrow(d) == 0) return(ab_dt(data.frame(note = "No shared fields selected.")))
    d$pct_agree <- round(100 * d$pct_agree, 1)
    d$kappa <- round(d$kappa, 3)
    d$mean_abs_diff <- round(d$mean_abs_diff, 3)
    ab_dt(d, page = 15)
  })

  output$ab_aud_table <- DT::renderDataTable({
    r <- ab_rv$res; req(r)
    d <- r$aud
    if (is.null(d) || nrow(d) == 0) return(ab_dt(data.frame(note = "Nothing to audit.")))
    d$rate_pct <- ifelse(is.na(d$rate), NA, round(100 * d$rate, 2))
    d <- d[, c("model", "metric", "count", "denom", "rate_pct", "detail"), drop = FALSE]
    ab_dt(d, page = 20)
  })

  output$ab_dis_table <- DT::renderDataTable({
    r <- ab_rv$res; req(r); req(nrow(r$dis) > 0)
    ab_dt(r$dis, page = 10, trunc_cols = c("NOTE_TEXT", grep("^A: |^B: ", names(r$dis), value = TRUE)))
  })

  output$ab_report_status <- renderUI({
    if (is.null(ab_rv$res))
      return(div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:8px;",
                 "Run a comparison first \u2014 the report is built from the results above."))
    div(style = "font-size:0.75rem;color:#3fb950;margin-bottom:8px;",
        sprintf("\u2713 Ready \u2014 %s matched rows, %s vs %s.",
                format(ab_rv$res$n_matched, big.mark = ","), ab_rv$res$la, ab_rv$res$lb))
  })

  # Surfaced only when the optional Word dependency is missing. The HTML report
  # is unaffected, so this is a note rather than a blocking error.
  output$ab_docx_note <- renderUI({
    if (requireNamespace("officer", quietly = TRUE)) return(NULL)
    div(style = "font-size:0.72rem;color:#d29922;margin-top:6px;",
        "\u26A0 The 'officer' package is not installed, so the .docx button will fail. Install it once with:  install.packages(\"officer\")  \u2014 the .html report works regardless.")
  })

  # Shared by both renderers, so the two formats cannot diverge.
  ab_report_meta <- function(r) {
    keys <- input$ab_join_key
    key_desc <- if (length(keys) == 0 || "__ROWORDER__" %in% keys) "row order (no key column)"
                else paste(keys, collapse = " + ")
    md <- suppressWarnings(as.integer(input$ab_rep_maxdis))
    if (is.na(md) || md < 0) md <- 50
    list(
      title = if (nzchar(trimws(input$ab_rep_title %||% ""))) trimws(input$ab_rep_title)
              else "NICU LLM Extraction: Model Comparison Report",
      prepared_by = trimws(input$ab_rep_author %||% ""),
      file_a = if (!is.null(input$ab_file_a)) input$ab_file_a$name else "(unknown)",
      file_b = if (!is.null(input$ab_file_b)) input$ab_file_b$name else "(unknown)",
      key_desc = key_desc,
      gold_col = if (identical(r$mode, "gold")) (r$gold_col_label %||% input$ab_gold_col) else NULL,
      max_dis = md)
  }

  ab_report_filename <- function(ext) {
    base <- trimws(input$ab_rep_title %||% "")
    # Inside a POSIX bracket expression a backslash is a LITERAL character, not
    # an escape. "[^A-Za-z0-9_\\- ]" therefore parsed as ... "_", then the range
    # "\" (0x5C) to " " (0x20) -- which runs backwards, so TRE rejected the whole
    # pattern and the download died before Shiny could name the file. The hyphen
    # goes last instead, where it is unambiguously literal, and the backslash is
    # dropped so it can never reach a Windows filename.
    base <- gsub("[^A-Za-z0-9_ -]", "", base)
    base <- gsub("[[:space:]]+", "_", trimws(base))
    # Checked after stripping, so a title made only of punctuation still yields
    # a usable name rather than a file called "_20261103_1225.html".
    if (!nzchar(base)) base <- "model_comparison_report"
    paste0(substr(base, 1, 60), "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", ext)
  }

  output$ab_dl_html <- downloadHandler(
    filename = function() ab_report_filename("html"),
    content = function(file) {
      r <- ab_rv$res
      if (is.null(r)) stop("Run a comparison before downloading the report.")
      ab_build_html(r, ab_report_meta(r), file)
      log_add(sprintf("\u2713 HTML report written (%s matched rows)",
                      format(r$n_matched, big.mark = ",")))
    })

  output$ab_dl_docx <- downloadHandler(
    filename = function() ab_report_filename("docx"),
    content = function(file) {
      r <- ab_rv$res
      if (is.null(r)) stop("Run a comparison before downloading the report.")
      ab_build_docx(r, ab_report_meta(r), file)
      log_add(sprintf("\u2713 Word report written (%s matched rows)",
                      format(r$n_matched, big.mark = ",")))
    })

  ab_stamp <- function() format(Sys.time(), "%Y%m%d_%H%M")
  output$ab_dl_dis <- downloadHandler(
    filename = function() paste0("ab_disagreements_", ab_stamp(), ".csv"),
    content = function(file) {
      r <- ab_rv$res
      readr::write_csv(if (is.null(r)) data.frame() else r$dis, file)
    })
  output$ab_dl_met <- downloadHandler(
    filename = function() paste0("ab_metrics_", ab_stamp(), ".csv"),
    content = function(file) {
      r <- ab_rv$res
      base <- data.frame(
        model_a = r$la, model_b = r$lb, primary_field = r$pf,
        positive_values = paste(r$pos, collapse = "|"),
        scoring_mode = r$mode,
        n_matched = r$n_matched, n_scored = r$n_both,
        n_agree = r$n_agree, pct_agree = r$pct_agree,
        cohens_kappa = r$kappa, kappa_strength = ab_kappa_label(r$kappa),
        mcnemar_discordant = r$mc$n_discordant,
        mcnemar_a_only = r$mc$a_only, mcnemar_b_only = r$mc$b_only,
        mcnemar_p_exact = r$mc$p_value,
        unmatched_a = r$unmatched_a, unmatched_b = r$unmatched_b,
        duplicate_keys_a = r$dup_a, duplicate_keys_b = r$dup_b,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE)
      if (!is.null(r$met_tbl)) {
        out <- cbind(base[rep(1, nrow(r$met_tbl)), , drop = FALSE], r$met_tbl)
      } else {
        out <- base
      }
      readr::write_csv(out, file)
    })
  output$ab_dl_fld <- downloadHandler(
    filename = function() paste0("ab_field_agreement_", ab_stamp(), ".csv"),
    content = function(file) {
      r <- ab_rv$res
      readr::write_csv(if (is.null(r)) data.frame() else r$fld_tbl, file)
    })
  output$ab_dl_aud <- downloadHandler(
    filename = function() paste0("ab_quality_audit_", ab_stamp(), ".csv"),
    content = function(file) {
      r <- ab_rv$res
      readr::write_csv(if (is.null(r)) data.frame() else r$aud, file)
    })

  # ===========================================================================
  # GOLD-STANDARD ADJUDICATION
  # Turns a Model A/B run's disagreement list (or an uploaded file) into a
  # row-by-row review workflow with independent per-reviewer records, so a
  # true accuracy figure -- not just agreement between two models -- becomes
  # possible via the Model A/B tab's gold-scoring mode. row_key here is the
  # exact same paste'd key string the A/B tab builds, so a gold file exported
  # from here matches straight back onto that tab's data with no extra join.
  # ===========================================================================

  adj_rv <- reactiveValues(
    rows = NULL, idx = 1,
    labels = data.frame(row_key = character(0), reviewer = character(0),
                        call = character(0), notes = character(0),
                        timestamp = character(0), stringsAsFactors = FALSE))

  adj_upload_df <- reactive({
    fi <- input$adj_file; req(fi)
    tryCatch(as.data.frame(ab_read_file(fi$datapath, fi$name), stringsAsFactors = FALSE),
             error = function(e) NULL)
  })

  output$adj_keycol_ui <- renderUI({
    df <- adj_upload_df()
    if (is.null(df))
      return(div(style = "color:#f85149;font-size:0.75rem;", "Could not read this file."))
    nms <- names(df)
    guess <- if ("row_key" %in% nms) "row_key" else nms[1]
    div(
      div(class = "form-label", "Which column uniquely identifies each row?"),
      selectInput("adj_key_col", NULL, choices = nms, selected = guess, width = "100%"),
      div(style = "font-size:0.72rem;color:#8b949e;",
          "A column named NOTE_TEXT, and columns containing \u2018[A]\u2019/\u2018[B]\u2019 or starting \u2018A: \u2019/\u2018B: \u2019, are shown automatically; everything else is listed as-is.")
    )
  })

  adj_source_rows <- reactive({
    if (identical(input$adj_source %||% "ab_run", "ab_run")) {
      r <- ab_rv$res
      if (is.null(r) || is.null(r$dis) || nrow(r$dis) == 0) return(NULL)
      r$dis
    } else {
      df <- adj_upload_df()
      kc <- input$adj_key_col
      if (is.null(df) || is.null(kc) || !(kc %in% names(df))) return(NULL)
      names(df)[names(df) == kc] <- "row_key"
      df$row_key <- as.character(df$row_key)
      df
    }
  })

  observeEvent(adj_source_rows(), {
    adj_rv$rows <- adj_source_rows()
    adj_rv$idx <- 1
  }, ignoreNULL = FALSE)

  observeEvent(adj_rv$rows, {
    rows <- adj_rv$rows
    choices <- if (is.null(rows) || nrow(rows) == 0) character(0)
               else stats::setNames(seq_len(nrow(rows)),
                                    sprintf("%d: %s", seq_len(nrow(rows)), rows$row_key))
    updateSelectInput(session, "adj_jump", choices = choices,
                      selected = if (length(choices) > 0) choices[[1]] else NULL)
  })

  output$adj_source_status <- renderUI({
    if (identical(input$adj_source %||% "ab_run", "ab_run")) {
      r <- ab_rv$res
      if (is.null(r))
        return(div(style = "font-size:0.75rem;color:#d29922;",
                   "No comparison has been run yet in this session. Run one in the Model A/B tab, then come back here."))
      n <- if (is.null(r$dis)) 0 else nrow(r$dis)
      if (n == 0)
        return(div(style = "font-size:0.75rem;color:#3fb950;",
                   "The last run had no disagreements or missing calls \u2014 nothing to adjudicate."))
      div(style = "font-size:0.75rem;color:#3fb950;",
          sprintf("\u2713 %d disagreement/no-call row(s) loaded from the last comparison (%s vs %s).",
                  n, r$la, r$lb))
    } else {
      rows <- adj_source_rows()
      if (is.null(rows))
        div(style = "font-size:0.75rem;color:#8b949e;", "Upload a file and pick the row-key column.")
      else
        div(style = "font-size:0.75rem;color:#3fb950;", sprintf("\u2713 %d row(s) loaded.", nrow(rows)))
    }
  })

  observeEvent(input$adj_jump, {
    i <- suppressWarnings(as.integer(input$adj_jump))
    if (!is.na(i)) adj_rv$idx <- i
  }, ignoreInit = TRUE)

  observeEvent(input$adj_prev, { adj_rv$idx <- max(1, adj_rv$idx - 1) })

  observeEvent(input$adj_next, {
    rows <- adj_rv$rows
    if (is.null(rows) || nrow(rows) == 0) return()
    adj_rv$idx <- min(nrow(rows), adj_rv$idx + 1)
  })

  # Preload the current reviewer's existing answer for the row in view, so
  # re-visiting a row (or switching reviewer name) never looks blank-by-default
  # when it was actually already adjudicated.
  observe({
    rows <- adj_rv$rows
    if (is.null(rows) || nrow(rows) == 0) return()
    i <- max(1, min(adj_rv$idx, nrow(rows)))
    rk <- rows$row_key[i]
    rev <- trimws(input$adj_reviewer %||% "")
    lb <- adj_rv$labels
    hit <- lb[lb$row_key == rk & lb$reviewer == rev, , drop = FALSE]
    if (nrow(hit) > 0) {
      updateRadioButtons(session, "adj_call", selected = hit$call[nrow(hit)])
      updateTextAreaInput(session, "adj_notes", value = hit$notes[nrow(hit)])
    } else {
      updateRadioButtons(session, "adj_call", selected = character(0))
      updateTextAreaInput(session, "adj_notes", value = "")
    }
  })

  adj_render_row <- function(row) {
    cols <- names(row)
    note_col <- intersect(c("NOTE_TEXT"), cols)
    a_cols <- cols[grepl("\\[A\\]|^A: ", cols)]
    b_cols <- cols[grepl("\\[B\\]|^B: ", cols)]
    other <- setdiff(cols, c("row_key", note_col, a_cols, b_cols))
    kv <- function(cols_) {
      if (length(cols_) == 0) return(div(style = "font-size:0.72rem;color:#8b949e;", "(none)"))
      tagList(lapply(cols_, function(cn) {
        v <- row[[cn]]
        div(style = "margin-bottom:5px;",
            tags$b(style = "color:#8b949e;font-size:0.68rem;",
                  paste0(sub("^A: |^B: ", "", cn), ": ")),
            tags$span(style = "font-size:0.78rem;",
                      if (is.null(v) || (length(v) == 1 && is.na(v))) "\u2014" else as.character(v)))
      }))
    }
    tagList(
      div(style = "font-size:0.72rem;color:#8b949e;margin-bottom:6px;",
          paste("Row key:", row$row_key)),
      if (length(note_col) > 0)
        div(style = "white-space:pre-wrap;max-height:260px;overflow-y:auto;background:#0d1117;border:1px solid #30363d;border-radius:6px;padding:10px;font-size:0.78rem;margin-bottom:10px;",
            as.character(row[[note_col]])),
      fluidRow(
        column(6, div(style = "font-weight:600;font-size:0.72rem;color:#58a6ff;margin-bottom:4px;", "MODEL A"), kv(a_cols)),
        column(6, div(style = "font-weight:600;font-size:0.72rem;color:#d29922;margin-bottom:4px;", "MODEL B"), kv(b_cols))
      ),
      if (length(other) > 0) tagList(hr(class = "divider-line"), kv(other))
    )
  }

  output$adj_row_ui <- renderUI({
    rows <- adj_rv$rows
    if (is.null(rows) || nrow(rows) == 0)
      return(div(style = "font-size:0.8rem;color:#8b949e;",
                "No rows loaded. Run a Model A/B comparison that has disagreements, or upload a file above."))
    i <- max(1, min(adj_rv$idx, nrow(rows)))
    tagList(
      div(style = "font-size:0.75rem;color:#8b949e;margin-bottom:8px;",
          sprintf("Row %d of %d", i, nrow(rows))),
      adj_render_row(rows[i, , drop = FALSE])
    )
  })

  observeEvent(input$adj_save, {
    rows <- adj_rv$rows
    if (is.null(rows) || nrow(rows) == 0) return()
    rev <- trimws(input$adj_reviewer %||% "")
    if (!nzchar(rev)) {
      showNotification("Enter your name or initials before saving.", type = "error"); return()
    }
    i <- max(1, min(adj_rv$idx, nrow(rows)))
    rk <- rows$row_key[i]
    rec <- data.frame(row_key = rk, reviewer = rev, call = input$adj_call %||% "uncertain",
                      notes = trimws(input$adj_notes %||% ""),
                      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      stringsAsFactors = FALSE)
    lb <- adj_rv$labels
    dup <- which(lb$row_key == rk & lb$reviewer == rev)
    if (length(dup) > 0) lb[dup[1], ] <- rec else lb <- rbind(lb, rec)
    adj_rv$labels <- lb
    log_add(sprintf("Adjudicated row %s as %s (%s).", rk, rec$call, rev))

    n <- nrow(rows); nxt <- i
    if (isTRUE(input$adj_skip_labeled)) {
      labeled <- lb$row_key[lb$reviewer == rev]
      repeat {
        nxt <- nxt + 1
        if (nxt > n || !(rows$row_key[nxt] %in% labeled)) break
      }
    } else {
      nxt <- nxt + 1
    }
    adj_rv$idx <- min(nxt, n)
  })

  output$adj_progress_ui <- renderUI({
    rows <- adj_rv$rows; lb <- adj_rv$labels
    if (is.null(rows) || nrow(rows) == 0)
      return(div(style = "font-size:0.75rem;color:#8b949e;", "No rows loaded yet."))
    lb <- lb[lb$row_key %in% rows$row_key, , drop = FALSE]
    reviewers <- unique(lb$reviewer)
    by_row <- if (nrow(lb) > 0) split(lb, lb$row_key) else list()
    multi <- by_row[sapply(by_row, function(d) length(unique(d$reviewer)) >= 2)]
    n_conflict <- sum(sapply(multi, function(d) length(unique(d$call)) > 1))
    tagList(
      div(style = "display:flex;flex-wrap:wrap;gap:12px;",
          ab_tile("Rows loaded", format(nrow(rows), big.mark = ",")),
          ab_tile("Reviewers", if (length(reviewers) == 0) "0" else paste(reviewers, collapse = ", ")),
          ab_tile("Adjudicated (any reviewer)", format(length(unique(lb$row_key)), big.mark = ",")),
          ab_tile("Rows with \u22652 reviewers", format(length(multi), big.mark = ",")),
          ab_tile("Conflicting calls", as.character(n_conflict),
                  "need a third opinion", accent = if (n_conflict > 0) "#f85149" else "#3fb950")
      )
    )
  })

  output$adj_has_conflicts <- reactive({
    rows <- adj_rv$rows; lb <- adj_rv$labels
    if (is.null(rows) || nrow(rows) == 0 || nrow(lb) == 0) return(FALSE)
    lb <- lb[lb$row_key %in% rows$row_key, , drop = FALSE]
    by_row <- split(lb, lb$row_key)
    multi <- by_row[sapply(by_row, function(d) length(unique(d$reviewer)) >= 2)]
    any(sapply(multi, function(d) length(unique(d$call)) > 1))
  })
  outputOptions(output, "adj_has_conflicts", suspendWhenHidden = FALSE)

  output$adj_conflicts_table <- DT::renderDataTable({
    rows <- adj_rv$rows; lb <- adj_rv$labels
    req(rows); req(nrow(lb) > 0)
    lb <- lb[lb$row_key %in% rows$row_key, , drop = FALSE]
    by_row <- split(lb, lb$row_key)
    multi <- by_row[sapply(by_row, function(d) length(unique(d$reviewer)) >= 2)]
    conf <- multi[sapply(multi, function(d) length(unique(d$call)) > 1)]
    if (length(conf) == 0) return(ab_dt(data.frame(note = "No conflicts.")))
    out <- do.call(rbind, lapply(names(conf), function(rk) {
      d <- conf[[rk]]
      data.frame(row_key = rk,
                reviewer = paste(d$reviewer, collapse = " / "),
                call = paste(d$call, collapse = " / "),
                stringsAsFactors = FALSE)
    }))
    ab_dt(out, page = 10)
  })

  output$adj_dl_all <- downloadHandler(
    filename = function() paste0("adjudications_all_", ab_stamp(), ".csv"),
    content = function(file) {
      lb <- adj_rv$labels
      readr::write_csv(if (is.null(lb)) data.frame() else lb, file)
    })

  output$adj_dl_gold <- downloadHandler(
    filename = function() paste0("gold_standard_consensus_", ab_stamp(), ".csv"),
    content = function(file) {
      lb <- adj_rv$labels
      if (is.null(lb) || nrow(lb) == 0) {
        readr::write_csv(data.frame(row_key = character(0), gold_call = character(0),
                                    n_reviewers = integer(0), agreement = character(0)), file)
        return()
      }
      by_row <- split(lb, lb$row_key)
      out <- do.call(rbind, lapply(names(by_row), function(rk) {
        d <- by_row[[rk]]
        calls <- unique(d$call)
        gold <- if (length(calls) == 1) calls[1] else NA_character_
        data.frame(row_key = rk, gold_call = gold,
                  n_reviewers = length(unique(d$reviewer)),
                  agreement = if (length(calls) == 1) "consensus" else "CONFLICT \u2014 needs a third opinion",
                  stringsAsFactors = FALSE)
      }))
      readr::write_csv(out, file)
    })
}

shinyApp(ui = ui, server = server)