# Colour palettes for the gallery's plots, sourced from the {ltc} package
# (loukesio/ltc-color-palettes). Pure logic layer -- NO shiny:: calls. Returns
# plain hex vectors that the component plot code feeds into ggplot2 scales, so
# the classic and any future shinyreact version share one palette.
#
# Uses {ltc} when installed, else falls back to the same hex baked in here, so
# an app still renders (identical colours) in a bare environment and tests never
# break on a missing optional dep. Chosen palettes: "maya" for categorical
# groups, "heatmap0" for the continuous heatmap gradient.

# ltc's ltc() uses non-standard evaluation on the palette name, so it is only
# ever called here with a literal string (a variable would resolve to its own
# name). Returns the full palette, or the baked-in fallback on any failure.
.ltc_palette <- function(name, fallback) {
  if (requireNamespace("ltc", quietly = TRUE)) {
    pal <- tryCatch(
      switch(
        name,
        maya = ltc::ltc("maya"),
        heatmap0 = ltc::ltc("heatmap0"),
        NULL
      ),
      error = function(e) NULL
    )
    if (length(pal) > 0) {
      return(as.character(pal))
    }
  }
  fallback
}

# "maya", reordered so the leading colours contrast strongly (blue, coral, ...).
# The common case here is two groups (tumour vs normal, treated vs control), so
# the first two entries must be clearly distinct.
.maya_ordered <- function() {
  m <- .ltc_palette(
    "maya",
    c("#3d5a80", "#98c1d9", "#e0fbfc", "#ee6c4d", "#293241")
  )
  m[c(1, 4, 2, 5, 3)]
}

# n categorical colours for grouping variables (PCA points, boxplots). Recycles
# if a dataset has more groups than the palette provides.
ltc_categorical <- function(n = 5) {
  pal <- .maya_ordered()
  if (n <= length(pal)) pal[seq_len(max(n, 1))] else rep(pal, length.out = n)
}

# Down / NS / Up colours for DE status (volcano and any status-coloured view).
# Down/Up take maya's contrasting blue/coral ends; NS is a receding neutral grey
# so significant genes pop.
ltc_status_colours <- function() {
  m <- .maya_ordered()
  c(Down = m[[1]], NS = "#B8BCC2", Up = m[[2]])
}

# Anchor colours for a continuous heatmap gradient ("heatmap0", viridis-like).
# Feed to ggplot2::scale_*_gradientn(colours = ltc_gradient()).
ltc_gradient <- function() {
  .ltc_palette(
    "heatmap0",
    c(
      "#001219",
      "#005F73",
      "#0A9396",
      "#94D2BD",
      "#E9D8A6",
      "#EE9B00",
      "#CA6702",
      "#AE2012",
      "#9B2226"
    )
  )
}
