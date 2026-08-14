# ggplot2 renderer for the sample PCA (classic Shiny version).
#
# This is the module-layer rendering for the classic app; a shinyreact version
# would render the same `pca_data()` output differently. Pure ggplot2 (no
# shiny::), kept here rather than in shared/R because it is version-specific.
#
# Qualitative group colours come from the shared ltc helper (see
# shared/R/palettes.R) so the gallery reads consistently.

# pca: the list returned by pca_data().
# colour_by: metadata column mapped to point colour (NULL = no colouring).
# x, y: which principal components to plot.
plot_pca <- function(pca, colour_by = "group", x = "PC1", y = "PC2") {
  scores <- pca$scores
  pct <- pca$var_explained

  axis_label <- function(pc) {
    sprintf("%s (%.1f%%)", pc, pct[[pc]])
  }

  # `.data[[...]]` is the tidy-eval idiom for column names held in variables; it
  # resolves inside aes()'s data mask. (Linters may warn it's an undefined
  # global -- a known false positive.)
  has_colour <- !is.null(colour_by) && colour_by %in% names(scores)
  mapping <- if (has_colour) {
    ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      colour = .data[[colour_by]]
    )
  } else {
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  }

  p <- ggplot2::ggplot(scores, mapping) +
    ggplot2::geom_point(size = 3.5, alpha = 0.85) +
    ggplot2::labs(
      x = axis_label(x),
      y = axis_label(y),
      colour = colour_by
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "top")

  if (has_colour) {
    n <- length(unique(scores[[colour_by]]))
    p <- p + ggplot2::scale_colour_manual(values = ltc_categorical(n))
  }

  p
}
