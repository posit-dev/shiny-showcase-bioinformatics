# ggplot2 renderer for the connectivity score distribution (classic Shiny).
#
# This is the module-layer rendering for the classic app; a shinyreact version
# would render the same score_distribution_data() output differently. Pure
# ggplot2 (no shiny::), kept here rather than in shared/R because it's
# version-specific.
#
# Direction palette matches the connectivity table: reverse = blue, mimic =
# orange, neutral = grey.
score_distribution_colours <- c(
  Reverser = "#447099",
  Neutral = "#C8C8C8",
  Mimic = "#EE6331"
)

# df: the data frame from score_distribution_data() (score, direction).
# mimic_threshold: draws the dashed mimic/reverser cutoffs at +/- this value.
# highlight: optional single score to mark with a solid line (a selected hit).
# bins: histogram bin count.
plot_score_distribution <- function(
  df,
  mimic_threshold = 0.3,
  highlight = NULL,
  bins = 40
) {
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = score, fill = direction)
  ) +
    ggplot2::geom_histogram(bins = bins, colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = score_distribution_colours,
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::geom_vline(
      xintercept = c(-mimic_threshold, mimic_threshold),
      linetype = "dashed",
      colour = "grey40"
    ) +
    ggplot2::labs(x = "Connectivity score", y = "Perturbations") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "top")

  if (!is.null(highlight) && length(highlight) == 1 && !is.na(highlight)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = highlight,
        colour = "#404041",
        linewidth = 0.9
      )
  }
  p
}
