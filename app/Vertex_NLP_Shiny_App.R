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
  endpoint <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
    ai_cfg$location, ai_cfg$project, ai_cfg$location, ai_cfg$model
  )
  
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
  
  extract <- function(response) {
    if (is.null(response))
      return(list(text = NA_character_, reason = "NETWORK_CRASH", detail = NULL))
    
    status <- httr2::resp_status(response)
    if (status >= 400) {
      err_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
      return(list(text = NA_character_,
                  reason = sprintf("HTTP_ERROR_%d", status),
                  detail = err_body))
    }
    
    result <- tryCatch(httr2::resp_body_json(response), error = function(e) NULL)
    if (is.null(result))
      return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON", detail = NULL))
    
    # Prompt-level block (safety filter on the input itself)
    if (!is.null(result$promptFeedback) && !is.null(result$promptFeedback$blockReason))
      return(list(text = NA_character_,
                  reason = paste0("PROMPT_BLOCKED_", result$promptFeedback$blockReason),
                  detail = NULL))
    
    if (is.null(result$candidates) || length(result$candidates) == 0)
      return(list(text = NA_character_, reason = "NO_CANDIDATES", detail = NULL))
    
    candidate     <- result$candidates[[1]]
    finish_reason <- candidate$finishReason %||% "NO_FINISH_REASON"
    parts         <- candidate$content$parts
    
    if (!is.null(parts) && length(parts) > 0 && !is.null(parts[[1]]$text)) {
      txt <- parts[[1]]$text
      if (nzchar(trimws(txt)))
        return(list(text = txt, reason = finish_reason, detail = NULL))
    }
    
    # Candidate returned but carried no usable text — report WHY.
    list(text = NA_character_,
         reason = paste0("EMPTY_CANDIDATE_", finish_reason),
         detail = NULL)
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
  
  # Both failed — surface the most informative reason.
  list(text   = NA_character_,
       reason = paste0(r1$reason, " | fallback: ", r2$reason),
       detail = r1$detail %||% r2$detail)
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
    return(list(text = NA_character_, reason = "RESPONSE_NOT_JSON", detail = NULL))
  
  if (!is.null(result$choices) && length(result$choices) > 0) {
    content <- result$choices[[1]]$message$content
    if (!is.null(content) && nchar(trimws(content)) > 0)
      return(list(text = content, reason = "stop", detail = NULL))
    return(list(text = NA_character_, reason = "EMPTY_CHOICE_CONTENT", detail = NULL))
  }
  
  list(text = NA_character_, reason = "NO_CHOICES", detail = NULL)
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
                           column(6, textInput("ai_location", "Location", value = DEFAULT_VERTEX_CONFIG$location)),
                           column(6, textInput("ai_model_vertex", "Model", value = DEFAULT_VERTEX_CONFIG$model))
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
    n_failed = 0
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
    fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)
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
      fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)
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
    
    provider <- input$llm_provider
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
          
          api_result <- call_llm(full_prompt, provider, rv$gcp_token, vertex_cfg, lm_cfg, active_schema)
          json_response <- api_result$text; finish_reason <- api_result$reason; err_detail <- api_result$detail
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
      
      log_add("\u2550\u2550\u2550 Pipeline complete \u2550\u2550\u2550")
      session$sendCustomMessage("collapse-log", list())
    })
  })
}

shinyApp(ui = ui, server = server)