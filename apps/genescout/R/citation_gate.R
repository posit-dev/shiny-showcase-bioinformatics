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
