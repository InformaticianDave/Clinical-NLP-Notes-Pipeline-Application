# Evidence-Span Prompt Revisions — NICU NLP Disease Presets

**Purpose.** This documents the revision of the four disease classification prompts (NEC, IVH, PVL, ROP) in `Vertex_NLP_Shiny_App.R` to add a verbatim evidence-span requirement, an explicit section field (`where_found`), enforced negation handling, and equivocation rules with few-shot anchors. The goal is to cut classification false positives (cross-contamination, negation, hedging, carried-forward diagnoses) and make human adjudication fast enough to scale. The same text serves as the prompt-engineering subsection of the methods paper.

**Scope note.** These changes fix *classification* errors only — cases where the right note reached the model and was misread. They do nothing for *retrieval* false negatives (a note that the SQL never pulled). Those are fixed upstream in the query layer. Keep the two error classes separate when reporting.

---

## What changed (shared pattern across all four prompts)

Every disease prompt gained the same five elements:

| Element | What it does |
|---|---|
| `evidence_span` | The model must copy ONE exact verbatim sentence from the note that justifies `present = true`. If it cannot, `present` must be `false` and `evidence_span = null`. |
| `where_found` | The section the evidence came from (e.g. *operative findings*, *radiology impression*, *assessment/plan*, *problem list*, *HPI/history*). Surfaces carried-forward / problem-list mentions. |
| Negation rule | Explicit denial ("no pneumatosis", "ruled out", "no IVH") forces `present = false` and `negated = true`. |
| Equivocation rule | Hedged language ("possible", "cannot be excluded", "concerning for", "questionable", "equivocal") forces `present = false` with `confidence ≤ 0.4`. |
| Few-shot block | 2–3 compact input→JSON examples per prompt, always including a negated and a hedged case. |

The evidence-span requirement is the load-bearing change. Because the model must quote a real sentence from *this* note, a note that doesn't contain disease-specific language cannot be labeled positive — which is exactly what collapses cross-contamination false positives (e.g. a PDA-ligation op note can't quote a NEC finding).

---

## Failure mode → fix mapping

| Failure mode | Example | Fix that addresses it |
|---|---|---|
| Cross-contamination FP | PAT_ID 1401222713: a non-NEC note labeled NEC-positive | `evidence_span` — no quotable NEC sentence ⇒ forced to `false` |
| Negation FP | "NEC ruled out", "no IVH" read as positive | Negation rule + `negated` gated in `review_status` |
| Equivocation FP | "possible early pneumatosis, cannot exclude" | Equivocation rule ⇒ `false`, `confidence ≤ 0.4` |
| Carried-forward FP | Diagnosis in problem list / past history, not active | `where_found` surfaces the section for the reviewer |
| Slow adjudication | Reviewer reads the whole note per case | `evidence_span` ⇒ reviewer reads one quoted sentence |

---

## NEC — Necrotizing Enterocolitis

**Before.** Returned `clinical_signs` and `imaging_findings` as free strings; no quote required; negation captured in a field but not enforced; no hedging rule; no section.

**After (key additions).**
- A verbatim `evidence_span` is mandatory for `nec_present = true`; absent ⇒ `false`.
- Negation ("no pneumatosis", "NEC ruled out") ⇒ `nec_present = false`, `negated = true`.
- Hedging ("possible", "cannot be excluded", "r/o NEC", "vs") ⇒ `false`, `confidence ≤ 0.4`.
- Problem-list / history / differential mentions are not an active diagnosis.
- FIP/SIP exclusion retained (`fip_instead`).
- New fields: `evidence_span`, `where_found`.

**Few-shot anchors.**
- *"Abdominal XR shows extensive pneumatosis intestinalis; clinical picture consistent with NEC."* → `nec_present:true`, evidence quoted, `confidence:0.95`
- *"No pneumatosis or free air. NEC ruled out."* → `nec_present:false`, `negated:true`
- *"Possible early pneumatosis, cannot be excluded; will repeat film."* → `nec_present:false`, `confidence:0.3`

---

## IVH — Periventricular-Intraventricular Hemorrhage

**Before.** Worst-grade extraction (0–4) with laterality/hydrocephalus/modality; no quote; negation unenforced; no hedging rule; no section.

**After (key additions).**
- Verbatim `evidence_span` mandatory for the documented grade; absent ⇒ `ivh_present = false`, `worst_grade = null`.
- Negation ("no intracranial hemorrhage", "normal head ultrasound") ⇒ `false`, `worst_grade = 0`, `negated = true`.
- Hedging ⇒ `false`, `confidence ≤ 0.4`.
- Problem-list / carried-forward mentions excluded unless this note affirms imaging.
- New fields: `evidence_span`, `where_found`.

**Few-shot anchors.**
- *"Head US: bilateral grade 3 IVH with ventricular dilation."* → `ivh_present:true`, `worst_grade:3`, `hydrocephalus:true`
- *"Cranial ultrasound within normal limits, no IVH."* → `ivh_present:false`, `worst_grade:0`, `negated:true`

---

## PVL — Cystic Periventricular Leukomalacia

**Before.** Tri-state result (`yes`/`no`/`na`) with cyst/echogenicity/porencephaly flags; no quote; negation unenforced; no hedging rule.

**After (key additions).**
- `evidence_span` mandatory for `cpvl_result = "yes"`; absent ⇒ `"no"` or `"na"`.
- Negation ("no PVL", "no cystic changes") ⇒ `"no"`, `negated = true`.
- Hedging ("possible", "early/evolving", "cannot exclude") ⇒ `"no"`, `confidence ≤ 0.4`.
- Echogenicity-without-cysts and porencephalic-cyst exclusions retained and tied to the no-quote rule.
- New fields: `evidence_span`, `where_found`.
- **Positive value is `"yes"`, not `"true"`** — the scoring harness and `review_status` are configured for this.

**Few-shot anchors.**
- *"MRI: multiple small cysts in the periventricular white matter, consistent with cystic PVL."* → `cpvl_result:"yes"`, `cysts_documented:true`
- *"Increased periventricular echogenicity, no cysts identified."* → `cpvl_result:"no"`, `echogenicity_only:true`

---

## ROP — Retinopathy of Prematurity

**Before.** Worst-stage extraction (0–5) with zone/plus/treatment/laterality; no quote; negation unenforced; no hedging rule.

**After (key additions).**
- Verbatim `evidence_span` mandatory for the documented stage; absent ⇒ `rop_present = false`, `worst_stage = null`.
- Explicit absence ("no ROP", "immature retina without ROP", "mature retina") ⇒ `false`, `worst_stage = 0`, `negated = true`.
- Hedging ⇒ `false`, `confidence ≤ 0.4`.
- Screening-scheduling / history mentions are not an active diagnosis.
- New fields: `evidence_span`, `where_found`.

**Few-shot anchors.**
- *"OU: Stage 2, zone II, no plus disease."* → `rop_present:true`, `worst_stage:2`, `zone:"II"`
- *"Immature retina zone II, no ROP. Follow up in 2 weeks."* → `rop_present:false`, `worst_stage:0`, `negated:true`

---

## Review-status gating (the rollup safeguard)

Populating a `negated` field is not enough if nothing acts on it. The revised app computes a `review_status` per row from the parsed JSON, so negation is structurally honored before a row ever enters a cohort:

| `review_status` | Condition |
|---|---|
| `include` | present + not negated + `confidence ≥ threshold` |
| `review_low_conf` | present + not negated + `confidence < threshold` |
| `review_no_conf` | present + not negated + no numeric confidence returned |
| `exclude_negated` | `negated == true` (overrides present) |
| `exclude_absent` | present field is the negative value |
| `no_call` | model returned nothing usable for the present field |

Threshold is user-set in the UI (default 0.7). The scoring harness reports two views — RAW present vs `review_status == "include"` — so the precision gained from gating is quantified, not assumed.

---

## Change log

- Added `evidence_span` (verbatim quote) + `where_found` (section) to NEC, IVH, PVL, ROP, and custom presets.
- Added explicit negation and equivocation rules to all four disease prompts.
- Added negated/hedged few-shot examples to each disease prompt.
- Added `present_field` / `present_true` to each preset (PVL positive = `"yes"`; others `"true"`).
- Added `compute_review_status()` and a `review_status` output column; added confidence-threshold input and a review-status filter to the UI; evidence span now shown above the note text for adjudication.
