# =============================================================================
# nlp_scoring_harness.R
# Score the NICU NLP pipeline against a human-adjudicated gold standard.
# Produces a 2x2 + sensitivity / specificity / PPV / NPV (with 95% Wilson CIs),
# Cohen's kappa for two readers, and a disagreement file for adjudication.
#
# Dependencies: readr, dplyr  (everything else is base R)
# -----------------------------------------------------------------------------
#
# DO I NEED TO BUILD DB TABLES FOR 1401222713 AND 1402350643?  -- No.
#
# You don't build per-patient tables. A gold standard is just a CSV of HUMAN
# labels, one row per NOTE_ID. The two patient IDs are SEED CASES you make sure
# are included in the sample (1401222713 = known NEC false positive you want the
# harness to catch; 1402350643 = your XR test patient). The workflow is:
#
#   1. RUN your retrieval query for one morbidity (e.g. the fixed IVH/PVL .sql).
#      Export NOTE_ID, PAT_ID, and the note/narrative text to a CSV.
#
#   2. SAMPLE ~50-100 notes for that morbidity. Stratify so you don't get an
#      all-negative set: take all/most LLM-positive notes + a random batch of
#      LLM-negative notes. Force-include rows for PAT_ID 1401222713 and 1402350643.
#
#   3. ADJUDICATE with Dr. G. For each NOTE_ID, a human records gold_label
#      (present yes/no) and, where relevant, gold_grade / gold_stage. Two readers
#      independently -> you also get Cohen's kappa (inter-rater reliability), which
#      the paper's evaluation section will want. Save as the gold CSV (template
#      below -- run this script once with no gold file and it writes the template).
#
#   4. RUN the Shiny app on the SAME notes -> LLM output CSV (has the present
#      column, confidence, evidence_span, review_status).
#
#   5. RUN this harness. It joins LLM output to gold on NOTE_ID and reports the 2x2.
#
# It scores TWO views so you can see the effect of the review gate:
#   (a) RAW           : the model's present field as-is.
#   (b) AFTER GATING  : review_status == "include" (negation excluded,
#                       low-confidence sent to review). This is the number that
#                       reflects the revised app's actual cohort rule.
# Comparing (a) vs (b) is itself a result: it quantifies how much the negation/
# equivocation gating buys you in precision.
# =============================================================================

suppressMessages({
  library(readr)
  library(dplyr)
})

# =============================================================================
# CONFIG  -- edit these four things, then source the file.
# =============================================================================
CFG <- list(
  morbidity     = "NEC",                                  # NEC | IVH | PVL | ROP | custom | IVH_PVL_ivh | IVH_PVL_pvl
  gold_csv      = "gold_standard_NEC.csv",                # human labels (one row per NOTE_ID)
  llm_csv       = "nec_nlp_extraction.csv",               # Shiny app output for the same notes
  out_dir       = ".",                                    # where to write disagreement file + template
  na_pred_as_negative = TRUE,                             # model "no_call" counts as negative (a miss)
  review_col    = "review_status",                        # gated view; for combined use "ivh_review_status" or "cpvl_review_status"
  conf_threshold_note = 0.7                               # for reference only; gating already in review_status
)

# How "present" is encoded per morbidity (mirrors the app's column for that condition).
# For the COMBINED IVH+PVL preset, score each condition separately by pointing
# morbidity at IVH_PVL_ivh / IVH_PVL_pvl and review_col at the matching *_review_status.
PRESENT_SPEC <- list(
  NEC         = list(col = "nec_present",    positive = c("true")),
  IVH         = list(col = "ivh_present",    positive = c("true")),
  PVL         = list(col = "cpvl_result",    positive = c("yes")),
  ROP         = list(col = "rop_present",    positive = c("true")),
  custom      = list(col = "target_present", positive = c("true")),
  IVH_PVL_ivh = list(col = "ivh_present",    positive = c("true")),
  IVH_PVL_pvl = list(col = "cpvl_result",    positive = c("yes"))
)

# =============================================================================
# HELPERS
# =============================================================================
norm <- function(x) tolower(trimws(as.character(x)))

to_logical_positive <- function(x, positive_vals) {
  v <- norm(x)
  out <- v %in% norm(positive_vals)
  out[v %in% c("", "na", "null", "n/a") | is.na(x)] <- NA   # undefined stays undefined
  out
}

# Wilson score 95% CI for a proportion k/n (base R; no extra packages).
wilson_ci <- function(k, n, conf = 0.95) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- k / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  c(max(0, center - half), min(1, center + half))
}

fmt_ci <- function(p, lo, hi) {
  if (is.na(p)) return("   NA")
  sprintf("%.3f [%.3f-%.3f]", p, lo, hi)
}

# Cohen's kappa for two readers' yes/no labels.
cohen_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) < 2) return(NA_real_)
  tab <- table(factor(a, c(FALSE, TRUE)), factor(b, c(FALSE, TRUE)))
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (pe == 1) return(NA_real_)
  (po - pe) / (1 - pe)
}

# Core 2x2 metrics. gold/pred are logical (TRUE = positive). NA pred handled by caller.
# PPV == Precision; Sensitivity == Recall. F1 is the harmonic mean of the two,
# useful as a single number when you need to rank runs/thresholds rather than
# read a full 2x2 (F1 hides the sensitivity/specificity trade-off, so keep
# reporting the full table alongside it, not instead of it).
binary_metrics <- function(gold, pred) {
  keep <- !is.na(gold) & !is.na(pred)
  g <- gold[keep]; p <- pred[keep]
  TP <- sum(g & p); FP <- sum(!g & p); FN <- sum(g & !p); TN <- sum(!g & !p)
  N <- TP + FP + FN + TN
  metric <- function(k, n) { ci <- wilson_ci(k, n); list(p = if (n > 0) k/n else NA, lo = ci[1], hi = ci[2]) }
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA
  recall    <- if ((TP + FN) > 0) TP / (TP + FN) else NA
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA
  list(
    N = N, TP = TP, FP = FP, FN = FN, TN = TN,
    sensitivity = metric(TP, TP + FN),   # = Recall
    specificity = metric(TN, TN + FP),
    ppv         = metric(TP, TP + FP),   # = Precision
    npv         = metric(TN, TN + FN),
    accuracy    = metric(TP + TN, N),
    f1          = f1
  )
}

print_block <- function(title, m) {
  cat("\n", title, "\n", sep = "")
  cat(sprintf("  N scored: %d   (TP=%d  FP=%d  FN=%d  TN=%d)\n", m$N, m$TP, m$FP, m$FN, m$TN))
  cat("                 confusion\n")
  cat(sprintf("                 pred+   pred-\n"))
  cat(sprintf("       gold+ |  %5d   %5d\n", m$TP, m$FN))
  cat(sprintf("       gold- |  %5d   %5d\n", m$FP, m$TN))
  cat(sprintf("  Recall (Sensitivity) : %s\n", fmt_ci(m$sensitivity$p, m$sensitivity$lo, m$sensitivity$hi)))
  cat(sprintf("  Specificity          : %s\n", fmt_ci(m$specificity$p, m$specificity$lo, m$specificity$hi)))
  cat(sprintf("  Precision (PPV)      : %s\n", fmt_ci(m$ppv$p, m$ppv$lo, m$ppv$hi)))
  cat(sprintf("  NPV                  : %s\n", fmt_ci(m$npv$p, m$npv$lo, m$npv$hi)))
  cat(sprintf("  Accuracy             : %s\n", fmt_ci(m$accuracy$p, m$accuracy$lo, m$accuracy$hi)))
  cat(sprintf("  F1 Score             : %s\n", if (is.na(m$f1)) "   NA" else sprintf("%.3f", m$f1)))
}

write_gold_template <- function(path) {
  tmpl <- data.frame(
    NOTE_ID      = c("<paste a real NOTE_ID>", "<paste another>"),
    PAT_ID       = c("1401222713", "1402350643"),
    morbidity    = c(CFG$morbidity, CFG$morbidity),
    gold_label   = c("no", "no"),     # human truth: yes / no
    gold_grade   = c("", ""),         # optional: IVH grade / ROP stage if applicable
    reader1      = c("no", "no"),     # optional second columns for kappa
    reader2      = c("no", "no"),
    adjudicator  = c("DrG", "DAL"),
    notes        = c("seed: known NEC false positive", "seed: XR test patient"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(tmpl, path)
  cat("No gold file found. Wrote a template you can fill in:\n  ", normalizePath(path), "\n", sep = "")
}

# =============================================================================
# MAIN
# =============================================================================
run_scoring <- function(cfg = CFG) {
  spec <- PRESENT_SPEC[[cfg$morbidity]]
  if (is.null(spec)) stop("Unknown morbidity: ", cfg$morbidity)

  # ---- gold standard ----
  if (!file.exists(cfg$gold_csv)) {
    write_gold_template(file.path(cfg$out_dir, sprintf("gold_standard_%s_TEMPLATE.csv", cfg$morbidity)))
    return(invisible(NULL))
  }
  gold <- readr::read_csv(cfg$gold_csv, show_col_types = FALSE)
  if (!"NOTE_ID" %in% names(gold)) stop("gold CSV must have a NOTE_ID column.")
  if (!"gold_label" %in% names(gold)) stop("gold CSV must have a gold_label column (yes/no).")
  gold$gold_pos <- to_logical_positive(gold$gold_label, c("yes", "true", "1", "present"))

  # inter-rater reliability (optional)
  kappa <- NA_real_
  if (all(c("reader1", "reader2") %in% names(gold))) {
    r1 <- to_logical_positive(gold$reader1, c("yes", "true", "1", "present"))
    r2 <- to_logical_positive(gold$reader2, c("yes", "true", "1", "present"))
    kappa <- cohen_kappa(r1, r2)
  }

  # ---- LLM output ----
  if (!file.exists(cfg$llm_csv)) stop("LLM output CSV not found: ", cfg$llm_csv)
  llm <- readr::read_csv(cfg$llm_csv, show_col_types = FALSE)
  if (!"NOTE_ID" %in% names(llm)) stop("LLM CSV must have a NOTE_ID column.")
  if (!spec$col %in% names(llm)) stop("LLM CSV missing expected present column: ", spec$col)

  llm$llm_pos_raw <- to_logical_positive(llm[[spec$col]], spec$positive)
  review_col <- if (!is.null(cfg$review_col)) cfg$review_col else "review_status"
  llm$llm_pos_gated <- if (review_col %in% names(llm)) {
    norm(llm[[review_col]]) == "include"
  } else {
    warning("Review column '", review_col, "' not in LLM CSV; gated view == raw view.")
    llm$llm_pos_raw
  }

  # ---- join on NOTE_ID ----
  gold$NOTE_ID <- as.character(gold$NOTE_ID)
  llm$NOTE_ID  <- as.character(llm$NOTE_ID)
  keep_llm <- intersect(c("NOTE_ID", "PAT_ID", spec$col, "confidence", "negated",
                          "evidence_span", "where_found", "review_status", review_col,
                          "llm_pos_raw", "llm_pos_gated"), names(llm))
  j <- dplyr::inner_join(gold, llm[, keep_llm], by = "NOTE_ID", suffix = c("_gold", "_llm"))

  cat("=================================================================\n")
  cat(sprintf("  MORBIDITY: %s\n", cfg$morbidity))
  cat(sprintf("  gold rows: %d   llm rows: %d   matched on NOTE_ID: %d\n",
              nrow(gold), nrow(llm), nrow(j)))
  if (nrow(j) < nrow(gold))
    cat(sprintf("  NOTE: %d gold notes had no LLM row (check the run covered the same notes).\n",
                nrow(gold) - nrow(j)))
  if (!is.na(kappa)) cat(sprintf("  Inter-rater kappa (reader1 vs reader2): %.3f\n", kappa))
  cat("=================================================================\n")

  pred_raw <- j$llm_pos_raw
  pred_gated <- j$llm_pos_gated
  if (cfg$na_pred_as_negative) {
    pred_raw[is.na(pred_raw)] <- FALSE
    pred_gated[is.na(pred_gated)] <- FALSE
  }

  m_raw   <- binary_metrics(j$gold_pos, pred_raw)
  m_gated <- binary_metrics(j$gold_pos, pred_gated)
  print_block("VIEW A -- RAW model present field", m_raw)
  print_block("VIEW B -- AFTER review gating (review_status == 'include')", m_gated)

  # ---- disagreements for adjudication ----
  j$pred_raw <- pred_raw
  j$disagree_raw <- (j$gold_pos != j$pred_raw)
  dis <- j[which(j$disagree_raw), , drop = FALSE]
  if (nrow(dis) > 0) {
    dis$error_type <- ifelse(dis$gold_pos & !dis$pred_raw, "false_negative",
                      ifelse(!dis$gold_pos & dis$pred_raw, "false_positive", "other"))
    show_cols <- intersect(c("NOTE_ID", "PAT_ID", "error_type", "gold_label",
                             spec$col, "confidence", "negated", "review_status",
                             "where_found", "evidence_span", "notes"), names(dis))
    out_path <- file.path(cfg$out_dir, sprintf("disagreements_%s.csv", cfg$morbidity))
    readr::write_csv(dis[, show_cols, drop = FALSE], out_path)
    cat(sprintf("\n  %d disagreements (raw view) written for review:\n   %s\n",
                nrow(dis), normalizePath(out_path)))
    cat("  -> Tag each as RETRIEVAL (wrong note pulled / context) or CLASSIFICATION\n")
    cat("     (right note, model misread). The evidence_span column makes this fast.\n")
  } else {
    cat("\n  No raw-view disagreements. (Sanity-check your sample isn't all one class.)\n")
  }

  invisible(list(matched = j, raw = m_raw, gated = m_gated, kappa = kappa, disagreements = dis))
}

# Run when sourced directly.
if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  res <- tryCatch(run_scoring(CFG), error = function(e) { message("ERROR: ", conditionMessage(e)); NULL })
}
