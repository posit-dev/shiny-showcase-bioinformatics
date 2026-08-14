# ggplot2 renderer for the expression heatmap (classic Shiny version).
#
# This is the module-layer rendering for the classic app; a shinyreact version
# would render the same `heatmap_data()` output differently. Pure ggplot2 (no
# shiny::), kept here rather than in shared/R because it is version-specific.
#
# Continuous fill uses the shared ltc gradient (see shared/R/palettes.R) so the
# heatmap reads consistently with the rest of the gallery.

# hd: the list returned by heatmap_data().
# metadata: optional data frame with `sample` + a grouping column; when present,
#   samples are faceted by group so conditions read as labelled blocks (this is
#   the heatmap's column annotation, done with facet strips rather than a
#   separate annotation bar to avoid extra dependencies).
# group_col: the metadata column to facet/annotate by.
# show_samples: draw per-sample column labels. Default TRUE; set FALSE when
#   there are so many samples that the labels collide (e.g. cohort-scale
#   heatmaps) - the group facet strips still label the columns.
# show_features: draw per-row (gene/feature) labels. Default TRUE; set FALSE
#   when there are so many rows that the labels collide (e.g. a full query
#   signature of 150 genes) - the colour field still reads as a landscape.
plot_heatmap <- function(
  hd,
  metadata = NULL,
  group_col = "group",
  show_samples = TRUE,
  show_features = TRUE
) {
  tiles <- hd$tiles

  faceted <- !is.null(metadata) &&
    group_col %in% names(metadata) &&
    "sample" %in% names(metadata)
  if (faceted) {
    grp <- metadata[[group_col]][match(
      as.character(tiles$sample),
      as.character(metadata$sample)
    )]
    tiles$group <- grp
  }

  fill_label <- if (hd$scale == "row") "Row z-score" else "Expression"

  p <- ggplot2::ggplot(
    tiles,
    ggplot2::aes(x = sample, y = gene, fill = value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = NULL, y = NULL, fill = fill_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x = if (show_samples) {
        ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (show_samples) NULL else ggplot2::element_blank(),
      axis.text.y = if (show_features) NULL else ggplot2::element_blank(),
      axis.ticks.y = if (show_features) NULL else ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )

  # One continuous ltc gradient for both scales (z-score and raw expression);
  # the data range maps low -> high across the palette.
  p <- p + ggplot2::scale_fill_gradientn(colours = ltc_gradient())

  if (faceted) {
    p <- p +
      ggplot2::facet_grid(
        cols = ggplot2::vars(group),
        scales = "free_x",
        space = "free_x"
      )
  }

  p
}
