# Colour palettes for the ggplot2 (classic) renderings, matched to the light,
# organic app theme and the LTC color palettes (loukesio/ltc-color-palettes).
# Pure logic, no shiny::. Kept in sync with srcts/lib/theme.ts.

# Organic categorical palette (LTC seafarer / trio / minou / casa_natal hues).
biov_categorical <- function(n = 5) {
  pal <- c("#0E7175", "#ED773C", "#708C69", "#C63F3E", "#808BC5", "#E4A25B",
           "#245E55", "#9E3F71", "#56B4E9", "#EAC119", "#5B5F8D", "#9BB29E",
           "#013D5A", "#DA6B51", "#66A182", "#EAA7C7")
  if (n <= length(pal)) pal[seq_len(max(n, 1))] else rep(pal, length.out = n)
}

# Down / NS / Up status colours for the volcano (teal / warm-grey / red).
biov_status_colours <- function() {
  c(Down = "#0E7175", NS = "#C9C1B1", Up = "#C63F3E")
}

# Alteration classes for the oncoplot, mapped onto LTC hues. Order matters: it
# fixes both the legend order and which colour each class gets, and it is the
# same order the React side receives, so the two engines agree.
biov_variant_colours <- function() {
  c("Missense"       = "#0E7175",
    "Truncating"     = "#233038",
    "Frameshift"     = "#C63F3E",
    "Splice"         = "#ED773C",
    "In-frame indel" = "#E4A25B",
    "Amplification"  = "#9E3F71",
    "Deep deletion"  = "#808BC5",
    "Multi-hit"      = "#245E55",
    "Other"          = "#9BB29E")
}

# The six substitution-class colours used by every published SBS96 figure.
# Deliberately NOT the LTC palette: these hexes are the convention, and
# recolouring them makes the plot unreadable to anyone who knows the format.
biov_sbs_colours <- function() {
  c("C>A" = "#03BCEE", "C>G" = "#010101", "C>T" = "#E32926",
    "T>A" = "#CAC9C9", "T>C" = "#A1CE63", "T>G" = "#EBC6C4")
}

# Sequential gradient for the heatmap - LTC "heatmap0" (earthy viridis-like).
biov_gradient <- function() {
  c("#013D5A", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00", "#CA6702", "#AE2012")
}

# Diverging gradient for z-scored matrices (teal ↔ cream ↔ red).
biov_diverging <- function() {
  c("#0E7175", "#8BC8CB", "#F4EEE0", "#F4A582", "#C63F3E")
}

# ggplot2 theme tuned for the light paper surface, transparent background so the
# PNG blends into the app panel.
biov_theme <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid       = ggplot2::element_line(colour = "#E6DCC8"),
      panel.grid.minor = ggplot2::element_blank(),
      text             = ggplot2::element_text(colour = "#233038"),
      axis.text        = ggplot2::element_text(colour = "#6E7B72"),
      plot.title       = ggplot2::element_text(colour = "#233038", face = "bold"),
      legend.position  = "top",
      legend.key       = ggplot2::element_blank()
    )
}
