# Changelog

Reconstructed from the surviving snapshots of the application. Each entry
corresponds to a real captured version of the file; intermediate edits between
snapshots were not preserved and are described in aggregate.

**A note on commit dates.** Dates from 2026-11-03 onward are the true timestamps
of the captured artefacts. The first two entries predate the available record,
and their dates are approximate — the *order* is correct, the exact days are
not.

The four 2026-11-05 entries were developed in one sitting and split into
separate commits afterwards, one per logical change. Each resulting commit was
checked to parse and to contain only its own feature, so the history is
reviewable change by change; the timestamps within that day are therefore
ordering, not a record of when each line was typed.

---

## Missing-confidence crash fix — 2026-11-05

- Fixed a note whose JSON omitted `confidence` aborting the entire extraction
  run with `argument is of length zero`. `as.numeric(NULL)` is `numeric(0)`, so
  `is.na()` returned `logical(0)` and `if()` threw on a zero-length condition.
  Absent, empty, list-wrapped and non-numeric confidence are now all read as
  `review_no_conf`.
- Review-status computation is wrapped per row. A failure marks that one row
  `failed_status`, writes the reason to `extract_error` and lets the run
  continue, rather than escaping the row loop and losing everything since the
  last autosave.
- If a preset lacks `present_true`, fall back to the usual truthy set rather
  than silently marking every row `exclude_absent` — a clean-looking run full
  of fabricated negatives is worse than a crash, because nothing alerts you.
- **New `review_status` value: `failed_status`.** Downstream filters that
  enumerate statuses explicitly need to account for it.

## Hardware and live-request monitoring — 2026-11-05

- Usage & Cost gained a panel showing GPU load, VRAM, temperature and power
  (via `nvidia-smi`, every 3s) plus CPU and system RAM, read per platform and
  degrading to `NA` rather than erroring where a tool is missing or blocked.
- Recent-requests table: time, source, prompt/output tokens, duration and
  tok/s for every pipeline call and chat message, capped at 200 rows.
- These figures describe the machine running the app, so they are meaningful
  only on the LM Studio backend; Vertex calls run in Google's datacenter. The
  panel states this rather than implying local GPU use.

## Chat tab — 2026-11-05

- Free-form, multi-turn conversation with whichever backend and model Config
  has active, for testing a prompt or confirming a model/location combination
  answers at all before committing it to a run.
- `call_gemini_chat()` / `call_lmstudio_chat()` are separate from the
  extraction callers, which force JSON output and a response schema — wrong
  for a conversation. Full turn history is resent each call, since neither API
  keeps server-side state.
- Nothing is written to disk; the thread resets on reload.

## Vertex endpoint location fix — 2026-11-05

- Fixed HTTP 404 `NOT_FOUND` on every Gemini model not served from
  `us-central1`. The error names the model, but the cause is the endpoint
  shape: Vertex's global endpoint is **not** region-prefixed
  (`aiplatform.googleapis.com`), while regional ones are
  (`{location}-aiplatform.googleapis.com`). `call_gemini()` built the regional
  hostname unconditionally, so `global` could never resolve.
- Default location changed from `us-central1` to `global`, which serves every
  current Gemini model. Config notes that regional endpoints serve a subset.

## Gold-call mapping fix — 2026-11-04

- Fixed adjudicated `positive` / `negative` calls being scored against the
  extraction's positive-value list (`true,yes,1`), which silently counted every
  adjudicated positive as a negative.
- `uncertain` and `CONFLICT` rows are now excluded from scoring rather than
  treated as negatives. A refusal to call is not a negative call.

## Gold-Standard Adjudication tab — 2026-11-04

- New tab turning a comparison's disagreement worklist into a row-by-row review
  workflow: note text shown alongside both models' calls and evidence spans.
- Per-reviewer records, so several clinicians can score the same rows
  independently; inter-rater conflicts are surfaced with the rows named.
- Exports an adjudication log and a consensus gold-standard file.
- The consensus file uploads back into the Model A/B tab and is matched on the
  same row key, converting an agreement comparison into measured accuracy
  without a manual join.

## Model A/B comparison, reporting, and cost tracking — 2026-11-03

- New Model A/B tab comparing two extraction runs over the same notes: raw
  agreement, Cohen's kappa, exact McNemar, PPV/recall/F1 with Wilson intervals,
  per-field agreement, and a disagreement worklist.
- Integrity audit requiring no ground truth: hallucinated evidence spans,
  positive calls with no quote, quotes on negative calls, schema violations.
- Full report export in HTML and Word, structured as Methods, Statistical
  Analysis, Results, Discussion and a reproducibility appendix, both rendered
  from one source so the formats cannot diverge.
- Token usage and cost tracking, with a Gemini model catalogue driving both the
  model dropdown and the Usage & Cost tab.

Fixes made against findings from a real PVL run:

- Row-key candidates reordered most-granular-first. An encounter-level key had
  been chosen over `ORDER_PROC_ID`, discarding 32 of 51 rows.
- Warning when de-duplication drops 10% or more of rows, naming a finer key.
- Compared fields default to the detected diagnosis preset rather than every
  shared column.
- Exact-match agreement no longer computed on continuous fields.
- "Quote supplied on a negative call" added as its own audit check.
- Narrative fixes for undefined kappa and for zero discordant rows, which had
  produced meaningless boilerplate.
- Fixed a regex that crashed every report download: inside a POSIX bracket
  expression a backslash is a literal character, so `[^A-Za-z0-9_\\- ]` parsed
  as a reversed range and the engine rejected it.

## Hardened extraction backend and diagnosis presets — 2026-11-03

- `maxOutputTokens` raised from 2048 to 8192. Long surveillance reports were
  hitting the old cap mid-JSON and producing unparseable truncated output.
- Schema-free retry when Vertex suppresses a schema-constrained response on
  notes with mixed affirm/deny language; `repair_json` handles the free-form
  result.
- Every failure path returns a specific reason string instead of a catch-all,
  so the log and the `extract_error` column say what actually happened.
- Evidence-span extraction, so each call carries the verbatim text supporting
  it.
- `review_status` routing: low-confidence and no-call rows are separated from
  clean includes and excludes.
- Diagnosis presets for NEC, IVH, PVL, IVH+PVL, ROP, PDA and PDA_CXR.
- Resume support that skips already-processed rows.

## LM Studio provider support — approximate date

- Added a local OpenAI-compatible backend (`call_lmstudio`) for PHI-containing
  work, with a `call_llm` dispatcher routing to the active provider.
- Provider toggle between Vertex AI and LM Studio, each with its own settings
  and connection test; GCP authentication is required only for Vertex.
- Model discovery against `/v1/models` populating a selectable list.
- System prompt enforcing raw JSON output, since smaller local models tend to
  add preamble.
- Removed the `DEFAULT_SQL` constant; the query box remains but starts blank.

## Initial pipeline — approximate date

- Shiny application pulling notes from Epic Clarity over ODBC, sending each to
  Vertex AI with a JSON output schema, and writing one structured row per note.
- Autosave every N rows, and a stop control for long runs.
- Fields: `target_present`, `target_terms`, `where_found`, `negated`,
  `confidence`.
