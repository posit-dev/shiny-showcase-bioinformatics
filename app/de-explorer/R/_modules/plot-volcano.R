# ggplot2 renderer for the volcano plot (classic Shiny version).
#
# This is the module-layer rendering for the classic app; a shinyreact version
# would render the same `volcano_data()` output differently. Pure ggplot2 (no
# shiny::), kept here rather than in shared/R because it is version-specific.

# Status palette (Down / NS / Up) comes from the shared ltc helper so every
# view reads consistently; see shared/R/palettes.R.

# xlab: x-axis title (default the log2-fold-change label; override to reuse the
# volcano for other effect sizes, e.g. a signature score difference).
plot_volcano <- function(
  vdata,
  lfc_threshold = 1,
  p_threshold = 0.05,
  xlab = expression(log[2] ~ fold ~ change),
  highlight = character(0)
) {
  p <- ggplot2::ggplot(
    vdata,
    ggplot2::aes(x = logFC, y = neg_log10_p, colour = status)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 1.6) +
    ggplot2::geom_vline(
      xintercept = c(-lfc_threshold, lfc_threshold),
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(p_threshold),
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::scale_colour_manual(
      values = ltc_status_colours(),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = xlab,
      y = expression(-log[10] ~ p),
      colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "top")

  labelled <- vdata[!is.na(vdata$label), , drop = FALSE]
  if (nrow(labelled) > 0) {
    p <- p +
      ggrepel::geom_text_repel(
        data = labelled,
        ggplot2::aes(label = label),
        size = 3.5,
        max.overlaps = Inf,
        show.legend = FALSE,
        colour = "#404041"
      )
  }

  # Emphasise a currently-selected gene (e.g. the one shown in Gene detail), so
  # a click cross-highlights here: a ringed point plus a bold label, even when
  # the gene isn't among the top-N auto-labelled points.
  hl <- vdata[vdata$gene %in% highlight, , drop = FALSE]
  if (nrow(hl) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = hl,
        shape = 21,
        size = 3.4,
        stroke = 1.2,
        colour = "#111111",
        fill = NA,
        show.legend = FALSE
      ) +
      ggrepel::geom_text_repel(
        data = hl,
        ggplot2::aes(label = gene),
        size = 4,
        fontface = "bold",
        max.overlaps = Inf,
        show.legend = FALSE,
        colour = "#111111"
      )
  }

  p
}
