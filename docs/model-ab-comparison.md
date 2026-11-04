# Model A/B comparison

Compares two extraction runs over the same notes and reports where they agree,
where they diverge, and whether either one is internally consistent.

## What you need

Two CSV or XLSX files, one per model, over the same notes, with the same
columns. Nothing else has to be prepared — no third dataset, no extra model run.

| Requirement | Detail |
|---|---|
| Row key | A column unique **per note**, in both files: `NOTE_ID`, `ORDER_PROC_ID` or an accession number |
| Primary field | The yes/no call, e.g. `cpvl_result`, plus the values counting as positive (default `true,yes,1`) |
| `NOTE_TEXT` | In at least one file. Without it the hallucination check cannot run and adjudication cannot show the note |
| Evidence spans | Any column ending `evidence_span`, already produced by the pipeline |

## Choosing the row key

This is the single most common source of silently wrong results.

`PAT_ENC_CSN_ID` identifies an **encounter**, not a note. An infant with four
cranial ultrasounds during one admission has four rows sharing one CSN. Keying
on it discards three of them and compares one arbitrary note per encounter.

In one real PVL run this dropped 32 of 51 rows, and the comparison reported 19
matched rows as though that were the dataset. Re-keyed on `ORDER_PROC_ID`, all
51 rows matched and nothing was discarded.

The app orders its key suggestions most-granular-first and warns when
de-duplication discards 10% or more of rows, naming a finer key when one exists
in both files. Take the warning seriously; every statistic above it is computed
on the surviving rows.

## Scoring modes

| Mode | What it gives you |
|---|---|
| Agreement only | Kappa, McNemar, per-field agreement. No accuracy |
| Treat A (or B) as reference | Describes how one model differs from the other. Not accuracy — a shared error cannot appear |
| Gold standard | Real precision, recall and F1, on rows where the gold column is filled |

Gold mode accepts either a column already present in the uploaded files, or a
consensus CSV exported from the Adjudication tab. The uploaded file is matched
on the same row key as the comparison, so no manual join is needed.

## Reading the output

**Agreement near 100% with a near-zero positive rate is not a good result.** If
a condition appears in 2% of notes, two models that call almost everything
negative agree on 98% of rows without either being informative. Check the
positive-call count in the integrity audit before reading anything else.

**Kappa of 1.000 on one positive call** is similarly thin. Kappa becomes
undefined, and is reported as such, when a field takes only one value.

**McNemar p = 1.000 with zero discordant rows** is not evidence the models
behave identically. A test with no discordant pairs has no power at all.

**A hallucination rate needs its denominator.** 80% of 35 evidence spans and 0%
of 4 are not comparable claims. A model that rarely quotes anything can post a
perfect hallucination rate while being less useful than one that quotes often
and sometimes gets it wrong.

## Comparing prompts instead of models

The model labels are free text, so the same tab compares two prompt variants
from one model — label them `Zero-shot` and `Few-shot` and run it unchanged.
Testing prompt strategies this way is cheaper than changing models and often
moves accuracy more.
