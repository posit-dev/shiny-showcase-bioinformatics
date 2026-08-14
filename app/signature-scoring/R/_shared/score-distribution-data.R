# Prepare the connectivity score distribution for plotting: classify every
# perturbation's score as a mimic / reverser / neutral hit so the histogram can
# show where the hits sit against the null bulk.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Mirrors the direction
# classification used by the connectivity table (connectivity-table-data.R).

# scores: data.frame(pert_id, score) from connectivity_score(), or a plain
#   numeric vector of scores. mimic_threshold: |score| cutoff for a hit call.
# Returns data.frame(score, direction [Reverser/Neutral/Mimic factor]).
score_distribution_data <- function(scores, mimic_threshold = 0.3) {
  v <- if (is.data.frame(scores)) {
    if (!"score" %in% names(scores)) {
      stop("scores data frame must have a 'score' column")
    }
    scores$score
  } else {
    as.numeric(scores)
  }

  direction <- rep("Neutral", length(v))
  direction[v >= mimic_threshold] <- "Mimic"
  direction[v <= -mimic_threshold] <- "Reverser"

  data.frame(
    score = v,
    direction = factor(direction, levels = c("Reverser", "Neutral", "Mimic")),
    stringsAsFactors = FALSE
  )
}
