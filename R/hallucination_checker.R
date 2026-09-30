# =============================================================================
# hallucination_checker.R
# Checks whether every evidence_span the model produced is ACTUALLY a verbatim
# substring of the source NOTE_TEXT. This is the cheapest, highest-value
# integrity check for an evidence-grounded pipeline: it needs NO gold standard,
# just the pipeline's own output CSV. Run it on every batch, every time.
#
# A hallucination here means: the model claimed to quote the note, but the
# string it returned does not actually appear in the note. That is a distinct
# failure from a wrong classification — the model can be RIGHT about a
# diagnosis while still fabricating or paraphrasing the "quote," and that
# paraphrase is exactly what defeats the evidence-span design (fast human
# verification, cross-contamination prevention).
#
# Usage:
#   source("hallucination_checker.R")
#   result <- check_hallucinations("nec_nlp_extraction.csv")
#   print(result$summary)
#   View(result$flagged)
# =============================================================================

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# Normalize whitespace/casing so we don't flag trivial differences
# (double spaces, trailing periods, smart quotes) as hallucinations.
.normalize_for_match <- function(x) {
  x <- tolower(x)
  x <- gsub("[\u2018\u2019]", "'", x)   # smart single quotes -> straight
  x <- gsub("[\u201c\u201d]", '"', x)   # smart double quotes -> straight
  x <- gsub("\\s+", " ", x)             # collapse whitespace
  trimws(x)
}

# Fuzzy containment: exact substring match after normalization, with a
# fallback tolerance for a single trailing/leading punctuation mismatch
# (the model sometimes drops or adds a trailing period).
.is_verbatim <- function(evidence, note_text) {
  if (is.na(evidence) || is.na(note_text)) return(NA)
  if (!nzchar(trimws(evidence))) return(NA)
  ev <- .normalize_for_match(evidence)
  nt <- .normalize_for_match(note_text)
  if (nzchar(ev) && grepl(ev, nt, fixed = TRUE)) return(TRUE)
  # tolerate a missing/extra trailing period or comma
  ev_trim <- gsub("[.,;:]+$", "", ev)
  if (nzchar(ev_trim) && grepl(ev_trim, nt, fixed = TRUE)) return(TRUE)
  FALSE
}

# Find every *_evidence_span column in a dataframe (handles combined presets
# like IVH+PVL and PDA which have prefixed evidence columns).
.find_evidence_cols <- function(df) {
  grep("evidence_span$", names(df), value = TRUE)
}

check_hallucinations <- function(csv_path, note_col = "NOTE_TEXT") {
  df <- readr::read_csv(csv_path, show_col_types = FALSE)

  if (!note_col %in% names(df)) stop("Column '", note_col, "' not found in ", csv_path)
  ev_cols <- .find_evidence_cols(df)
  if (length(ev_cols) == 0) stop("No *_evidence_span column found in ", csv_path)

  df$row_id <- seq_len(nrow(df))
  flagged <- list()
  summary_rows <- list()

  for (ec in ev_cols) {
    verbatim <- mapply(.is_verbatim, df[[ec]], df[[note_col]])
    n_checked <- sum(!is.na(verbatim))
    n_hallucinated <- sum(!is.na(verbatim) & !verbatim)
    n_clean <- sum(!is.na(verbatim) & verbatim)
    n_no_evidence <- sum(is.na(verbatim))  # negative rows with null evidence — not applicable

    summary_rows[[ec]] <- data.frame(
      evidence_column       = ec,
      n_rows_with_evidence  = n_checked,
      n_verbatim_confirmed  = n_clean,
      n_hallucinated        = n_hallucinated,
      hallucination_rate    = if (n_checked > 0) round(n_hallucinated / n_checked, 4) else NA,
      n_negative_no_evidence = n_no_evidence
    )

    if (n_hallucinated > 0) {
      bad <- df[!is.na(verbatim) & !verbatim, , drop = FALSE]
      bad$hallucinated_column <- ec
      bad$hallucinated_text   <- bad[[ec]]
      keep_cols <- intersect(c("row_id", "NOTE_ID", "PAT_ID", "hallucinated_column",
                               "hallucinated_text", note_col), names(bad))
      flagged[[ec]] <- bad[, keep_cols, drop = FALSE]
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  flagged_df <- if (length(flagged) > 0) do.call(rbind, flagged) else
    data.frame(row_id = integer(0))

  cat("=================================================================\n")
  cat(sprintf("HALLUCINATION CHECK: %s\n", basename(csv_path)))
  cat("=================================================================\n")
  for (i in seq_len(nrow(summary_df))) {
    r <- summary_df[i, ]
    cat(sprintf("\n  %s\n", r$evidence_column))
    cat(sprintf("    Rows with a claimed evidence span : %d\n", r$n_rows_with_evidence))
    cat(sprintf("    Verified verbatim in note          : %d\n", r$n_verbatim_confirmed))
    cat(sprintf("    HALLUCINATED (not found in note)   : %d  (rate: %.2f%%)\n",
                r$n_hallucinated, 100 * r$hallucination_rate))
  }
  total_hallucinated <- sum(summary_df$n_hallucinated)
  if (total_hallucinated > 0) {
    out_path <- sub("\\.csv$", "_HALLUCINATIONS.csv", csv_path)
    readr::write_csv(flagged_df, out_path)
    cat(sprintf("\n  \u26a0 %d hallucinated evidence span(s) written to:\n    %s\n",
                total_hallucinated, normalizePath(out_path)))
  } else {
    cat("\n  \u2713 No hallucinated evidence spans detected.\n")
  }

  invisible(list(summary = summary_df, flagged = flagged_df))
}

# Run when sourced with a path pre-set, or call check_hallucinations() directly.
if (sys.nframe() == 0L && exists("CSV_PATH")) {
  check_hallucinations(CSV_PATH)
}
