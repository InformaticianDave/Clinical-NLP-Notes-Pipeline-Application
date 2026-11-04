# Gold-standard adjudication

Produces the chart-reviewed ground truth that turns a model comparison into a
measurement of accuracy.

The gold standard is not produced by running a third model. It is produced by a
human reading the note and deciding what is true.

## The loop

1. Run a comparison in the **Model A/B** tab.
2. Open **Gold-Standard Adjudication**. It loads the disagreement and no-call
   rows from that run automatically.
3. Enter your name or initials.
4. For each row, read the note text shown alongside both models' calls and
   evidence spans, then record Positive, Negative or Uncertain. Add a rationale
   if the decision was not obvious.
5. Download the consensus CSV.
6. Return to **Model A/B**, set Scoring mode to gold, upload that CSV, re-run.

The consensus file is matched on the same row key the comparison used, so there
is no manual merge. Do not change the key between adjudicating and scoring.

## Multiple reviewers

Each reviewer types their own name and works through the same rows. Calls are
stored per reviewer, so the same note can carry independent judgements.

The Progress panel reports how many rows have two or more reviewers and how many
of those conflict, with a table naming the rows and each reviewer's call. In the
exported consensus file, rows where reviewers agreed carry that call; rows where
they disagreed are flagged `CONFLICT` with an empty call, and are excluded from
scoring rather than silently resolved.

Rows marked Uncertain are also excluded. A refusal to call is not a negative.

## Two things that will catch you out

**Adjudications live in memory.** Refreshing the browser or restarting the app
discards them. Export before you close.

**Disagreement-only review ranks models; it does not measure accuracy.** Rows
where both models agreed are never checked, and two models can agree on the same
wrong answer. For absolute precision and recall, also adjudicate a random sample
of concordant rows — load them through the *Upload a file* option, which accepts
any file with a row key and note text.

## Sampling concordant rows

A reasonable starting point is a random sample of 50–100 agreed rows, stratified
so that agreed-positive rows are not swamped by agreed-negative ones. For a
condition at 2% prevalence, an unstratified sample of 50 rows will contain about
one positive and will not estimate precision at all.
