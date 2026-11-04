# Statistical methods

What the comparison reports compute, why those measures were chosen, and what
each one cannot tell you.

## Agreement

**Raw agreement** is the proportion of jointly scored rows where both models
returned the same call. It is reported first and trusted least. For an uncommon
finding it is dominated by shared negatives.

**Cohen's kappa** removes the agreement expected from the two models' marginal
rates. Interpreted with the Landis–Koch bands: <0.20 slight, 0.20–0.40 fair,
0.40–0.60 moderate, 0.60–0.80 substantial, >0.80 almost perfect.

Kappa has two failure modes that appear constantly in this work:

- **Undefined** when a field takes only one value across all rows. The app
  reports it as not estimable rather than printing a number.
- **The kappa paradox** — low kappa alongside high raw agreement when one class
  dominates. A 96% agreement with kappa 0.000 is not a contradiction; it means
  the agreement is entirely explained by the base rate.

Kappa is prevalence-dependent and is not comparable across cohorts.

## Testing for a systematic difference

Both models score the same notes, so the calls are **paired** and an unpaired
test is invalid. **McNemar's test** discards concordant rows and asks whether the
discordant ones split evenly between the two directions.

An **exact binomial** version is used at every sample size, rather than the
chi-square approximation, because discordant counts for rare findings are small
enough that the approximation is not justified.

A significant result means one model calls positive more readily. It does not
say which model is right.

With zero discordant rows the test returns p = 1 by construction. That is an
absence of evidence, not evidence of equivalence; the test has no power at all.

## Classification metrics

Where a reference is nominated, performance is summarised by **PPV (precision)**
and **sensitivity (recall)** rather than a single global figure such as AUC.

The reason is operational. PPV answers the question a reviewer actually faces:
of the notes this model flagged, what share are genuinely positive, and
therefore how much of the review queue is wasted. Sensitivity answers the
complement: of the true positives, what share would be missed if the output were
used unreviewed. **F1** is their harmonic mean.

AUC summarises ranking across all thresholds, which is not the decision being
made here — the pipeline emits a call, not a ranking.

Each proportion carries a **95% Wilson score interval**, which stays inside
[0, 1] at small n and near the boundaries, where the normal approximation
produces impossible bounds.

## Integrity checks requiring no ground truth

These are computable from a single output file, which makes them the only
quality signal available before chart review exists.

**Hallucinated evidence span** — the returned quote is not present verbatim in
the source note, after normalising case, curly quotation marks and runs of
whitespace so cosmetic differences are not flagged. A model can reach the
correct conclusion while fabricating the supporting quote, and that failure
specifically defeats the audit trail the evidence-span design exists to provide.

**Positive call with no quote** — the same defect in a different form.

**Quote supplied on a negative call** — the schema requires null. This check was
added after a run where one model attached evidence to 14 of 18 negative calls
and the other to 1. It separates *ignoring the schema* from *fabricating
evidence for a diagnosis*; the hallucination rate alone conflated them.

**Schema type violations** — cells whose value does not match the declared field
type, indicating the structured output contract was ignored.

Always read a rate against its denominator. A model that rarely quotes anything
posts an excellent hallucination rate while being less useful than one that
quotes often and is sometimes wrong.

## Per-field agreement, split by type

Exact match is the right question for a categorical field — presence, grade,
laterality, modality — including integer-valued ones such as IVH grade or ROP
stage, where matching exactly is genuinely what is being asked.

It is close to meaningless for a continuous field such as a confidence score,
because two models almost never return the identical decimal. Agreement and
kappa are left blank for those, and mean absolute difference, maximum absolute
difference and correlation are reported instead.

A field is treated as continuous only when it parses as numeric **and** carries
a genuinely fractional value, so integer grades stay categorical.

## The limit on all of it

Agreement between two models is not accuracy. Models that share a blind spot
agree perfectly. Nothing in an agreement-only report establishes that either
model is correct, and the generated reports state this in their own Limitations
section rather than leaving it implicit.

Results also reflect a single run of each model. Generation is not fully
deterministic even at temperature 0, so part of any observed disagreement is
run-to-run variation rather than a stable difference. Separating the two
requires repeat runs.
