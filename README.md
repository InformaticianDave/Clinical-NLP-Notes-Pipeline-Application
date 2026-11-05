# NICU LLM Extraction and Validation Pipeline

An R Shiny application for extracting neonatal morbidity diagnoses from Epic
clinical notes using large language models, and for validating that extraction
against a second model and against expert chart review.

The pipeline is built around one principle: **every extracted value must be
auditable**. The model is required to return the verbatim span of text that
supports its call, and the app checks that the span actually appears in the
source note. A model that reaches the right conclusion while fabricating its
evidence is treated as a failure, not a success.

---

## Configuration

Every connection detail in this repository is a placeholder. Nothing here points
at a real server, project or dataset:

| Constant | Committed placeholder |
|---|---|
| `DEFAULT_DB_CONFIG$server` | `your-sql-server.example.org` |
| `DEFAULT_VERTEX_CONFIG$project` | `your-gcp-project-id` |
| `DEFAULT_VERTEX_CONFIG$location` | `global` |
| `DEFAULT_LM_CONFIG$base_url` | `http://localhost:1234/v1` |

### A note on `location`

Leave it at `global` unless you have a reason not to. Vertex serves every
current Gemini model from the global endpoint, but a regional endpoint such as
`us-central1` serves only a subset — and asking a region for a model it does
not host returns `404 NOT_FOUND` naming the *model*, which reads like a typo or
a permissions problem rather than a location problem. The two endpoints also
have different URL shapes (global is not region-prefixed), which the app
handles for you.

Set your real values locally and keep them out of version control. The supported
way is a `.Renviron` file, which `.gitignore` excludes — copy `.Renviron.example`
and fill it in. Editing the constants directly also works, as long as that edit
is never committed.

### Keeping patient data out

No patient data is tracked here, and `.gitignore` is written to keep it that
way: every CSV, XLSX and report pattern this pipeline emits is blocked, because
all of them carry `NOTE_TEXT`, MRNs, CSNs and dates of birth.

Two habits matter more than the ignore rules:

- **Run `git status` before every commit** and look at what is actually staged.
  `.gitignore` cannot help with a file that has already been force-added.
- **Never use `git add -A` in a directory holding extraction output.** Add files
  by name.

An ignore rule prevents a mistake; it cannot undo one. Anything committed
survives in history until the history is rewritten, and if it has been pushed,
assume it has been copied.

The one report in `examples/` is synthetic. Its figures, row keys and quoted
phrases are fabricated for layout demonstration and it is labelled as such.

---

## What the app does

### 1. Extraction

Pulls notes from Epic Clarity over ODBC (or accepts a CSV/XLSX export), sends
each note to an LLM with a diagnosis-specific prompt and a JSON output schema,
and writes one structured row per note.

Two backends are supported:

- **Vertex AI (Google Gemini)** — for non-PHI work. Uses `responseSchema` to
  enforce typed JSON output, with an automatic schema-free retry for notes where
  the constrained response comes back empty.
- **LM Studio (local, OpenAI-compatible)** — for PHI-containing work, where
  nothing may leave the machine.

Built-in diagnosis presets: **NEC, IVH, PVL, IVH+PVL, ROP, PDA, PDA_CXR**.

Runs autosave every N rows and can resume, skipping rows already processed, so a
long run survives an interruption.

### 2. Model A/B comparison

Takes two extraction files over the same notes — typically one cloud model and
one local model — and reports:

- Raw agreement and Cohen's kappa, with Landis–Koch interpretation
- Exact McNemar test for a systematic difference in calling threshold
- Precision (PPV), recall (sensitivity), F1 and specificity with Wilson 95% CIs,
  when a reference is nominated
- Per-field agreement, split by field type — exact match and kappa for
  categorical fields, mean/max absolute difference and correlation for
  continuous ones
- An integrity audit needing no ground truth: hallucinated evidence spans,
  positive calls with no quote, quotes supplied on negative calls, schema type
  violations
- A disagreement worklist, which is the set of notes a human has to read

Exports a full report in HTML and Word, structured as Methods, Statistical
Analysis, Results, Discussion and a reproducibility appendix.

### 3. Gold-standard adjudication

Turns the disagreement worklist into a review workflow. A reviewer reads the
note text alongside both models' calls and evidence, records the true call, and
exports a consensus gold-standard file. Multiple reviewers can score the same
rows independently; the app reports inter-rater conflicts.

That gold file uploads back into the A/B tab, which converts the comparison from
*agreement between two models* into *measured accuracy against chart review*.
The distinction matters: two models that share a blind spot agree perfectly.

### 4. Chat

A free-form, multi-turn conversation with whichever backend and model are
active. It uses no output schema, so replies come back as prose. Useful for
trying a prompt before wiring it into a preset, or for confirming a model and
location combination answers at all before starting a run against it. Nothing
is written to disk and the thread resets on reload.

### 5. Usage, cost and hardware

Token counts, duration and estimated cost per run, driven by a model price
catalogue. Alongside it, a live panel reads GPU load, VRAM, temperature and
power from `nvidia-smi`, plus CPU and system RAM, and lists recent requests
with their duration and tok/s.

The hardware figures describe the machine running the app. They are therefore
meaningful on the LM Studio backend and not on Vertex, whose calls execute in
Google's datacenter while the local GPU sits idle. Where a reading is
unavailable — no `nvidia-smi`, WMI blocked — the tile shows `—` rather than
failing.

---

## Requirements

- R 4.3 or later
- CRAN packages: `shiny`, `bslib`, `DT`, `DBI`, `odbc`, `httr2`, `jsonlite`,
  `dplyr`, `readr`, `readxl`
- `officer` — optional, only for Word report export; HTML export works without it
- Google Cloud SDK (`gcloud`) on PATH, for Vertex AI authentication
- An ODBC driver for SQL Server, for direct database pulls

```r
install.packages(c("shiny", "bslib", "DT", "DBI", "odbc", "httr2",
                   "jsonlite", "dplyr", "readr", "readxl", "officer"))
```

## Running it

```bash
gcloud auth application-default login   # Vertex AI backend only
```

```r
shiny::runApp("app/Vertex_NLP_Shiny_App.R")
```

For the local backend, start LM Studio and load a model before launching the
app; the Config tab tests the connection and lists available models.

---

## Repository layout

```
app/     Vertex_NLP_Shiny_App.R     the application (single file)
R/       nlp_scoring_harness.R      standalone scoring, scriptable
         nlp_scoring_app.R          earlier standalone scoring UI
         hallucination_checker.R    verbatim evidence-span checking
docs/    model-ab-comparison.md     how to run a comparison, inputs required
         gold-standard-adjudication.md   the chart-review workflow
         statistical-methods.md     why kappa, McNemar and Wilson, and their limits
         evidence-span-prompting.md prompt revisions for evidence spans
examples/SAMPLE_model_comparison_report.html   synthetic report, no patient data
```

The scripts in `R/` predate the in-app comparison tab and are kept because they
run headless, which the Shiny tab does not.

---

## Validation approach

Results are validated against the Vermont Oxford Network registry as the
confirmed reference, cross-matched by MRN, with physician review of
registry-versus-pipeline mismatches. Within the app, low-confidence rows are
routed to human review and `no_call` rows are re-run.

Reported results use a single model rather than an ensemble, so that the
reported accuracy is the accuracy of a reproducible pipeline.

---

## Known limitations

- Agreement between two models is not accuracy. Nothing short of chart review
  establishes accuracy, and the reports say so in their own Limitations section.
- The unit of analysis is the note, not the infant. An infant with several
  cranial studies contributes several rows, and rolling those up to a
  patient-level diagnosis is a separate step with its own rules.
- Adjudicating only disagreement rows ranks two models but does not measure
  absolute accuracy, because rows where both models agreed are never checked.
  A sample of concordant rows is needed for that.
- Adjudications live in memory for the session. Export before closing the app.
  The same applies to the Chat thread and the recent-requests table.
- `review_status` can be `failed_status`: the row's JSON parsed, but computing
  its status errored, and `extract_error` holds the reason. It is rare, but any
  downstream query that enumerates statuses explicitly must account for it, or
  those rows will silently fall outside every bucket.
- Chat rows in the request table approximate token counts from character
  length, because the chat path does not parse `usageMetadata`. Pipeline rows
  carry the real counts. Do not mix the two in a cost calculation.
- Generation is not fully deterministic even at temperature 0, so part of any
  observed disagreement is run-to-run variation.

---

## Licence

GNU Affero General Public License v3.0. See `LICENSE`.

AGPL-3.0 is the strongest copyleft in common use: anyone who modifies this code
and makes it available over a network — a hosted Shiny app counts — must publish
their modified source under the same licence. That is usually the intent for
published academic methods work, and it is worth being deliberate about, because
it also means a commercial vendor cannot quietly build a closed product on top
of it.

Confirm the choice is compatible with institutional IP policy before the methods
paper goes out.
