# ggplot2 renderer for the group-comparison boxplot (classic Shiny version).
#
# This is the module-layer rendering for the classic app; a shinyreact version
# would render the same group_comparison_data() output differently. Pure ggplot2
# (no shiny::), kept here rather than in shared/R because it is version-specific.
#
# Qualitative group colours come from the shared ltc helper (see
# shared/R/palettes.R) so the gallery reads consistently.

# df: the tidy data frame from group_comparison_data() (sample, group, value).
# value_label: y-axis label (e.g. a gene or signature name).
# test: optional list from group_comparison_test() for a subtitle annotation.
plot_group_comparison <- function(df, value_label = "value", test = NULL) {
  pal <- ltc_categorical(nlevels(df$group))

  subtitle <- NULL
  if (!is.null(test) && !is.na(test$p)) {
    subtitle <- sprintf("%s p = %s", test$method, signif(test$p, 3))
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = group, y = value, colour = group)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5, fill = NA) +
    ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.7, size = 2) +
    ggplot2::scale_colour_manual(values = pal, guide = "none") +
    ggplot2::labs(x = NULL, y = value_label, subtitle = subtitle) +
    ggplot2::theme_minimal(base_size = 14)
}
