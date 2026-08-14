# Citation gate. Enforces "no ungrounded claims" mechanically: every evidence
# item must carry a non-empty `source_id` (a database accession or citation id).
# Items without one are rejected before they are accepted into a review.
#
# This replaces the Claude Agent SDK hook from the original design; here it is a
# plain validation pass over the tool/agent evidence.

# Split an evidence tibble into (kept, rejected) by whether each row is grounded.
# `evidence` must have a `source_id` column. Returns a list of two tibbles.
validate_evidence <- function(evidence) {
  if (is.null(evidence) || nrow(evidence) == 0) {
    return(list(kept = evidence, rejected = evidence))
  }
  if (!"source_id" %in% names(evidence)) {
    stop("Evidence has no `source_id` column to validate.", call. = FALSE)
  }
  grounded <- !is.na(evidence$source_id) & nzchar(trimws(evidence$source_id))
  list(
    kept = evidence[grounded, , drop = FALSE],
    rejected = evidence[!grounded, , drop = FALSE]
  )
}

# TRUE if a single evidence item is grounded (has a usable source_id).
is_grounded <- function(item) {
  !is.null(item$source_id) && nzchar(trimws(item$source_id %||% ""))
}

# The same gate over the SIGNAL table. Gating only the evidence table left a hole:
# a signal row is what moves the composite and prints a value in the ranked table,
# so an ungrounded one changed a gene's rank while its evidence row was dropped as
# ungrounded - a number on screen with nothing behind it.
#
# An ungrounded row is DEMOTED to a miss rather than deleted: assemble_matrix()
# builds one column per (gene x signal) from this table, so dropping rows would
# change the matrix shape. Demoting makes the cell read exactly like "no data",
# which is what an unciteable value is worth.
#
# Rows that are already absent (present = FALSE) are untouched - a miss legitimately
# carries no source_id. Returns list(signals = <tibble>, n_ungrounded = <int>).
validate_signals <- function(signals) {
  if (is.null(signals) || nrow(signals) == 0) {
    return(list(signals = signals, n_ungrounded = 0L))
  }
  if (!"source_id" %in% names(signals)) {
    stop("Signals have no `source_id` column to validate.", call. = FALSE)
  }
  grounded <- !is.na(signals$source_id) & nzchar(trimws(signals$source_id))
  ungrounded <- signals$present & !grounded
  if (any(ungrounded)) {
    signals$present[ungrounded] <- FALSE
    signals$raw[ungrounded] <- NA_real_
    signals$normalized[ungrounded] <- NA_real_
  }
  list(signals = signals, n_ungrounded = sum(ungrounded))
}
