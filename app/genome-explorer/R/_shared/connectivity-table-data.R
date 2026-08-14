# Assemble the connectivity results table: join per-perturbation connectivity
# scores to their metadata and classify each as a mimic / reverser / neutral hit.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns a tidy data frame
# the classic Shiny module hands to DT, and that a future shinyreact version can
# render client-side. This is the connectivity-table contract.

# scores: data.frame(pert_id, score) from connectivity_score().
# pert_meta: one row per perturbation (pert_id, drug, dose, time, cell_line, moa).
# mimic_threshold: |score| above which a perturbation is called a Mimic (score
#   > +t) or Reverser (score < -t); between is Neutral.
# Returns the joined table ordered by score descending, with a `direction`
# factor (Reverser/Neutral/Mimic) and a sensible column order.
connectivity_table <- function(scores, pert_meta, mimic_threshold = 0.3) {
  if (!all(c("pert_id", "score") %in% names(scores))) {
    stop("scores must have 'pert_id' and 'score' columns")
  }
  if (!"pert_id" %in% names(pert_meta)) {
    stop("pert_meta must have a 'pert_id' column")
  }

  df <- merge(scores, pert_meta, by = "pert_id", all.x = TRUE, sort = FALSE)

  direction <- rep("Neutral", nrow(df))
  direction[df$score >= mimic_threshold] <- "Mimic"
  direction[df$score <= -mimic_threshold] <- "Reverser"
  df$direction <- factor(direction, levels = c("Reverser", "Neutral", "Mimic"))

  # Lead with the interpretable columns, keep any extra metadata after.
  lead <- intersect(
    c(
      "pert_id",
      "drug",
      "score",
      "direction",
      "moa",
      "dose",
      "time",
      "cell_line"
    ),
    names(df)
  )
  df <- df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
  df[order(df$score, decreasing = TRUE), , drop = FALSE]
}

# Counts per direction, as a named integer vector (Reverser, Neutral, Mimic) --
# handy for summaries / value boxes without recomputing the classification.
connectivity_direction_counts <- function(tbl) {
  table(factor(tbl$direction, levels = c("Reverser", "Neutral", "Mimic")))
}
