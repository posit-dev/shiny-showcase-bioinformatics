# Plot builders. Shiny-free: shared by the view renderers and the PDF download
# handlers so the exported figure is exactly what is on screen.
#
# Every builder takes `dark`. ggplot2 draws on a white canvas whatever the page
# looks like, so without this the figures glare in dark mode. The colours come
# from _brand.yml through R/logic_brand.R, the same file bslib reads for the
# Bootstrap theme, so the figures and the interface cannot drift apart.
#
# `dark` defaults to FALSE, which is what the PDF downloads want: a figure
# going into a document or onto paper should be light whatever the screen was.
#
# Every builder also takes `font_size`, the one number a plot-heavy screen
# turns out to need most: axis text and tick labels are the first thing to
# become unreadable on a dense figure, well before anything else does.

plot_palette <- function(dark = FALSE) {
  brand_plot_palette(dark)
}

theme_recount <- function(dark = FALSE, base_size = 21) {
  col <- plot_palette(dark)
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      legend.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      legend.key = ggplot2::element_rect(fill = col$bg, colour = NA),
      text = ggplot2::element_text(colour = col$fg),
      axis.text = ggplot2::element_text(colour = col$muted),
      axis.title = ggplot2::element_text(colour = col$fg),
      plot.title = ggplot2::element_text(colour = col$fg),
      legend.text = ggplot2::element_text(colour = col$muted),
      legend.title = ggplot2::element_text(colour = col$fg),
      # theme_minimal() sets strip.text to a fixed dark grey rather than
      # inheriting `text`, so a facet title (the quality metrics panel) stays
      # near-black regardless of mode and disappears against a dark page.
      strip.text = ggplot2::element_text(colour = col$fg),
      strip.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = col$grid),
      panel.grid.minor = ggplot2::element_line(
        colour = col$grid,
        linewidth = 0.25
      )
    )
}

# ---- point labelling ---------------------------------------------------------

# Which points are far enough from the middle to be worth naming.
#
# Distance is measured after scaling both axes, so a wide x range does not
# decide the answer on its own.
outlier_index <- function(x, y, n) {
  scaled <- function(v) {
    v <- as.numeric(v)
    spread <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) {
      return(rep(0, length(v)))
    }
    (v - mean(v, na.rm = TRUE)) / spread
  }
  d <- sqrt(scaled(x)^2 + scaled(y)^2)
  utils::head(order(d, decreasing = TRUE), min(n, length(d)))
}

# Name the points, but only where a name can be read.
#
# ggrepel earns its place on a few dozen labels and turns 500 into a grey mat,
# so past the threshold only the points furthest from the centroid get one.
add_point_labels <- function(
  gg,
  df,
  x,
  y,
  label_col,
  max_labels = 30,
  dark = FALSE,
  font_size = 21
) {
  if (!label_col %in% names(df) || !nrow(df)) {
    return(gg)
  }
  shown <- if (nrow(df) <= max_labels) {
    df
  } else {
    df[outlier_index(df[[x]], df[[y]], max_labels), , drop = FALSE]
  }
  gg +
    ggrepel::geom_text_repel(
      data = shown,
      mapping = ggplot2::aes(label = .data[[label_col]]),
      colour = plot_palette(dark)$fg,
      # geom_text_repel measures size in mm, not the points theme_recount()
      # uses. 3mm was calibrated against a 14pt theme, so the ratio to 14
      # is what carries that calibration forward as font_size moves.
      size = font_size / 14 * 3,
      min.segment.length = 0,
      segment.alpha = 0.4,
      max.overlaps = Inf,
      show.legend = FALSE
    )
}

plot_qc <- function(
  qc,
  dark = FALSE,
  point_size = 2.2,
  label = FALSE,
  font_size = 21
) {
  col <- plot_palette(dark)
  gg <- ggplot2::ggplot(
    qc,
    ggplot2::aes(x = library_size, y = detected_genes)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = point_size, colour = col$point) +
    ggplot2::scale_x_log10(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = "Library size (log scale)", y = "Detected genes") +
    theme_recount(dark, base_size = font_size)
  if (!label) {
    return(gg)
  }
  add_point_labels(
    gg,
    qc,
    "library_size",
    "detected_genes",
    "sample",
    dark = dark,
    font_size = font_size
  )
}

plot_gene_expression <- function(
  df,
  geom = c("box", "violin"),
  gene_label = NULL,
  group_label = NULL,
  dark = FALSE,
  font_size = 21
) {
  geom <- match.arg(geom)
  col <- plot_palette(dark)
  gg <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = group, y = expression, fill = group)
  )
  gg <- if (geom == "violin") {
    gg + ggplot2::geom_violin(alpha = 0.75, colour = NA)
  } else {
    gg +
      ggplot2::geom_boxplot(
        alpha = 0.75,
        outlier.shape = NA,
        colour = col$muted
      )
  }
  gg +
    ggplot2::geom_jitter(
      width = 0.15,
      alpha = 0.45,
      size = 1.5,
      colour = col$fg
    ) +
    ggplot2::labs(x = group_label, y = "log2 CPM", title = gene_label) +
    ggplot2::guides(fill = "none") +
    brand_fill_scale(n = n_levels(df$group)) +
    theme_recount(dark, base_size = font_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
}

plot_pca_scatter <- function(
  scores,
  var_explained,
  color_label = NULL,
  dark = FALSE,
  point_size = 2.5,
  label = FALSE,
  font_size = 21
) {
  col <- plot_palette(dark)
  pct <- function(i) sprintf("PC%d (%.1f%%)", i, 100 * var_explained[i])
  gg <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, color = color)) +
    ggplot2::geom_point(alpha = 0.8, size = point_size) +
    ggplot2::labs(x = pct(1), y = pct(2), color = color_label) +
    theme_recount(dark, base_size = font_size)
  gg <- if (is.null(color_label)) {
    gg +
      ggplot2::guides(color = "none") +
      ggplot2::scale_color_manual(values = col$point)
  } else {
    gg + brand_colour_scale(n = n_levels(scores$color))
  }
  if (!label) {
    return(gg)
  }
  add_point_labels(
    gg,
    scores,
    "PC1",
    "PC2",
    "sample",
    dark = dark,
    font_size = font_size
  )
}

plot_pca_scree <- function(var_explained, dark = FALSE, font_size = 21) {
  col <- plot_palette(dark)
  df <- data.frame(
    pc = factor(seq_along(var_explained)),
    var = 100 * var_explained
  )
  ggplot2::ggplot(df, ggplot2::aes(x = pc, y = var)) +
    ggplot2::geom_col(fill = col$point) +
    ggplot2::labs(x = "PC", y = "% variance") +
    theme_recount(dark, base_size = font_size)
}

# Warm categorical scales, so a grouped plot does not fall back to ggplot2's
# default hues and undo the brand.
#
# `n` is how many levels the caller is about to draw. scale_*_manual errors
# with "Insufficient values in manual scale" when the vector is shorter than
# the data, so every builder counts its own levels and says so, rather than
# hoping eight is enough.
brand_fill_scale <- function(n = NULL) {
  ggplot2::scale_fill_manual(
    values = brand_qualitative(n = n),
    na.value = "#888888"
  )
}

brand_colour_scale <- function(n = NULL) {
  ggplot2::scale_colour_manual(
    values = brand_qualitative(n = n),
    na.value = "#888888"
  )
}

# The continuous equivalent, for the correlation heatmap.
brand_gradient_scale <- function(dark = FALSE, aesthetic = "fill", ...) {
  scale <- if (identical(aesthetic, "fill")) {
    ggplot2::scale_fill_gradientn
  } else {
    ggplot2::scale_colour_gradientn
  }
  scale(colours = brand_sequential(dark), na.value = "#888888", ...)
}

# How many levels a discrete vector will draw as, counted the same way ggplot2
# counts them so the scale is never one short.
n_levels <- function(x) {
  if (is.factor(x)) {
    return(nlevels(x))
  }
  length(unique(x[!is.na(x)]))
}

# ---- failure and empty states ------------------------------------------------

# A plot whose only content is a sentence.
#
# Views call this instead of stopping, so a study missing a column shows the
# reason inside the card. A red Shiny error block covers the whole card and
# does not say which control to change.
plot_message <- function(text, dark = FALSE, font_size = 21) {
  col <- plot_palette(dark)
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = paste(strwrap(text, width = 60), collapse = "\n"),
      colour = col$muted,
      # Same mm-to-pt calibration as add_point_labels(): 4.5mm read right
      # against the plot's original 14pt theme.
      size = font_size / 14 * 4.5,
      lineheight = 1.2
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = col$bg, colour = NA)
    )
}

# Build a plot, or draw why it could not be built.
#
# Wrapped around every renderPlot. Grouping columns come from study metadata
# that nobody validated, so a builder can always meet something unexpected;
# the app should stay usable when it does.
safe_plot <- function(expr, dark = FALSE, font_size = 21) {
  tryCatch(
    expr,
    error = function(e) {
      plot_message(
        paste("This plot could not be drawn:", conditionMessage(e)),
        dark,
        font_size
      )
    }
  )
}

# ---- quality figures ---------------------------------------------------------

# Above this many samples a per-sample axis is a grey smear, so the tick
# labels come off and the axis title says what the axis is instead.
SAMPLE_AXIS_LIMIT <- 60L

sample_axis <- function(n, font_size = 21) {
  if (n <= SAMPLE_AXIS_LIMIT) {
    return(ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        # Kept smaller than the body text even at the default, because these
        # ticks run one per sample and would collide otherwise; but they
        # still grow and shrink with everything else.
        size = font_size * 0.5
      )
    ))
  }
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

plot_biotype_composition <- function(df, dark = FALSE, font_size = 21) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message(
      paste(
        "This study has no gene_type annotation, so its library",
        "composition cannot be shown."
      ),
      dark,
      font_size
    ))
  }
  n <- nlevels(df$sample)
  ggplot2::ggplot(df, ggplot2::aes(x = sample, y = share, fill = biotype)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = function(v) {
      paste0(v, "%")
    }) +
    ggplot2::labs(
      x = sprintf(
        "Sample (%s, ordered by non protein coding share)",
        format(n, big.mark = ",")
      ),
      y = "Share of reads",
      fill = "Biotype"
    ) +
    brand_fill_scale(n = nlevels(df$biotype)) +
    theme_recount(dark, base_size = font_size) +
    sample_axis(n, font_size) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

plot_qc_panel <- function(
  df,
  dark = FALSE,
  group_label = NULL,
  point_size = 1.8,
  font_size = 21
) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message(
      "This study carries none of the recount3 quality metrics.",
      dark,
      font_size
    ))
  }
  col <- plot_palette(dark)
  df$outlier <- flag_outliers(df)
  grouped <- !is.null(group_label) && nzchar(group_label)

  gg <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value)) +
    ggplot2::geom_boxplot(
      outlier.shape = NA,
      alpha = 0.25,
      colour = col$muted,
      fill = col$grid
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(colour = group, shape = outlier),
      width = 0.2,
      height = 0,
      alpha = 0.8,
      size = point_size
    ) +
    # An open circle for a sample past 1.5 IQR, so an outlier is visible
    # without needing the legend to explain it.
    ggplot2::scale_shape_manual(
      values = c(`FALSE` = 16, `TRUE` = 1),
      guide = "none"
    ) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    brand_colour_scale(n = n_levels(df$group)) +
    ggplot2::labs(x = group_label, y = NULL, colour = group_label) +
    theme_recount(dark, base_size = font_size)

  if (!grouped) {
    return(
      gg +
        ggplot2::guides(colour = "none") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank()
        )
    )
  }
  gg +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

plot_sex_check <- function(
  df,
  dark = FALSE,
  point_size = 2.5,
  label = FALSE,
  threshold = 0.3,
  font_size = 21
) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message(
      paste(
        "This study has no chromosome X and Y read percentages,",
        "so donor sex cannot be checked."
      ),
      dark,
      font_size
    ))
  }
  col <- plot_palette(dark)
  above <- sum(df$chry > threshold, na.rm = TRUE)
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = chry, y = chrx)) +
    ggplot2::geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      colour = col$muted,
      linewidth = 0.4
    ) +
    ggplot2::geom_point(alpha = 0.8, size = point_size, colour = col$point) +
    ggplot2::labs(
      x = "Reads on chrY (%)",
      y = "Reads on chrX (%)",
      # The count is the answer people came for. Two clouds mean mixed donors;
      # one cloud means the study is effectively single sex, and saying which
      # beats making the reader squint at the cloud.
      subtitle = sprintf(
        "%d of %d samples are above %.1f%% chrY",
        above,
        nrow(df),
        threshold
      )
    ) +
    theme_recount(dark, base_size = font_size)
  if (!label) {
    return(gg)
  }
  add_point_labels(
    gg,
    df,
    "chry",
    "chrx",
    "sample",
    dark = dark,
    font_size = font_size
  )
}

plot_expression_distribution <- function(df, dark = FALSE, font_size = 21) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message("No expression values to summarise.", dark, font_size))
  }
  col <- plot_palette(dark)
  n <- nlevels(df$sample)
  ggplot2::ggplot(df, ggplot2::aes(x = sample)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        ymin = ymin,
        lower = lower,
        middle = middle,
        upper = upper,
        ymax = ymax
      ),
      stat = "identity",
      fill = col$point,
      colour = col$muted,
      alpha = 0.8,
      linewidth = 0.3
    ) +
    ggplot2::labs(
      x = sprintf("Sample (%s, ordered by median)", format(n, big.mark = ",")),
      y = "log2 CPM"
    ) +
    theme_recount(dark, base_size = font_size) +
    sample_axis(n, font_size) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

plot_sample_correlation <- function(
  df,
  dark = FALSE,
  method = "spearman",
  font_size = 21
) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message(
      "A correlation heatmap needs at least three samples.",
      dark,
      font_size
    ))
  }
  n <- nlevels(df$row)
  gg <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = row, y = col, fill = correlation)
  ) +
    ggplot2::geom_raster() +
    brand_gradient_scale(dark, "fill") +
    # Not coord_fixed(): a fixed-aspect panel narrower than the card leaves a
    # margin that plot.background does not reach, and ggsave papers over it
    # with the theme colour on export but the on-screen renderer does not, so
    # the gap shows as the renderer's own default white in every mode.
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = sprintf("%s r", tools::toTitleCase(method)),
      subtitle = sprintf("%s samples, clustered", format(n, big.mark = ","))
    ) +
    theme_recount(dark, base_size = font_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  # Past forty samples the labels overlap into a solid band, and the grid
  # itself is the message anyway.
  if (n > 40L) {
    return(
      gg +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()
        )
    )
  }
  gg +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = font_size * 0.5
      ),
      axis.text.y = ggplot2::element_text(size = font_size * 0.5)
    )
}

plot_pca_loadings <- function(df, pc = 1, dark = FALSE, font_size = 21) {
  if (is.null(df) || !nrow(df)) {
    return(plot_message("No loadings to show.", dark, font_size))
  }
  df$label <- factor(df$label, levels = df$label[order(df$loading)])
  ggplot2::ggplot(df, ggplot2::aes(x = loading, y = label, fill = direction)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = plot_palette(dark)$muted,
      linewidth = 0.4
    ) +
    brand_fill_scale(n = n_levels(df$direction)) +
    ggplot2::labs(
      x = sprintf("Loading on PC%d", pc),
      y = NULL,
      fill = NULL,
      subtitle = "Genes that push this component hardest, both ways"
    ) +
    theme_recount(dark, base_size = font_size)
}
