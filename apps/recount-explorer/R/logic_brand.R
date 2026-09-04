# The brand, read from _brand.yml.
#
# bslib reads that file to build the Bootstrap theme. This reads the same file
# so the ggplot2 figures use the same colours. One edit to _brand.yml changes
# the buttons, the tables and the plots together, which is the point of keeping
# the palette in a file rather than scattered through the code.
#
# Shiny-free, so the palette is testable without an app.

# RECOUNT_EXPLORER_BRAND points at a different brand file, which is how you try
# another palette without editing the repository.
brand_path <- function() {
  Sys.getenv("RECOUNT_EXPLORER_BRAND", "_brand.yml")
}

# Used when _brand.yml is missing or unreadable, so a broken brand file makes
# the app look plain rather than stop it from starting.
BRAND_FALLBACK <- list(
  espresso = "#2B1D14",
  roast = "#4A3226",
  coffee = "#6F4A32",
  caramel = "#8A5A3B",
  cinnamon = "#A8734A",
  latte = "#C4A484",
  crema = "#D9C4A9",
  foam = "#E7DACA",
  cream = "#FBF7F2",
  steam = "#F5EFE7",
  sage = "#5C7B4E",
  amber = "#B5822E",
  brick = "#9E3B32",
  slate = "#7C6A5C",
  `espresso-dark` = "#1F1611",
  `roast-dark` = "#2A1E17",
  grounds = "#3A2A20",
  tan = "#C89B6E"
)

# Read once for each process. The file does not change while the app runs.
brand_colors <- local({
  cached <- NULL
  cached_path <- NULL
  function(path = brand_path(), refresh = FALSE) {
    if (!refresh && !is.null(cached) && identical(path, cached_path)) {
      return(cached)
    }
    palette <- tryCatch(
      {
        text <- paste(
          readLines(path, warn = FALSE),
          collapse = "
"
        )
        yaml::yaml.load(text)$color$palette
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.list(palette) || !length(palette)) {
      palette <- BRAND_FALLBACK
    } else {
      # Anything the brand file leaves out falls back, so a partial palette
      # cannot produce an empty colour.
      missing <- setdiff(names(BRAND_FALLBACK), names(palette))
      palette[missing] <- BRAND_FALLBACK[missing]
    }
    cached <<- palette
    cached_path <<- path
    cached
  }
})

# One colour by name, with a fallback so a typo shows as grey rather than
# erroring inside a plot builder.
brand_color <- function(name, colors = brand_colors()) {
  value <- colors[[name]]
  if (is.null(value) || !nzchar(value)) {
    return("#888888")
  }
  value
}

# The roles a plot needs, for one mode.
#
# The mapping lives here rather than in _brand.yml because it is about plots,
# not about the brand: a brand says what caramel is, this says that points are
# drawn in it.
brand_plot_palette <- function(dark = FALSE, colors = brand_colors()) {
  pick <- function(name) brand_color(name, colors)
  if (dark) {
    return(list(
      bg = pick("roast-dark"),
      fg = pick("foam"),
      muted = pick("latte"),
      grid = pick("grounds"),
      point = pick("tan")
    ))
  }
  list(
    bg = pick("cream"),
    fg = pick("espresso"),
    muted = pick("slate"),
    grid = pick("foam"),
    point = pick("caramel")
  )
}

# The qualitative scale for grouped plots, warm rather than the usual blues.
#
# `n` is the number of levels the plot actually has. There are eight named
# colours, and metadata_group_choices() will happily offer a column with more
# levels than that, so past eight we ramp between the named colours instead of
# handing ggplot2 a short vector and letting it error.
brand_qualitative <- function(colors = brand_colors(), n = NULL) {
  base <- unname(vapply(
    c("caramel", "sage", "coffee", "amber", "slate", "brick", "latte", "roast"),
    brand_color,
    character(1),
    colors = colors
  ))
  if (is.null(n)) {
    return(base)
  }
  n <- max(as.integer(n), 1L)
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}

# The continuous ramp, low value to high, for the correlation heatmap.
#
# Mode aware because the ends have to sit against the page: a cream low end
# disappears on a dark background, and an espresso low end disappears on a
# light one.
brand_sequential <- function(dark = FALSE, colors = brand_colors()) {
  pick <- function(name) brand_color(name, colors)
  if (dark) {
    return(c(pick("grounds"), pick("cinnamon"), pick("foam")))
  }
  c(pick("foam"), pick("cinnamon"), pick("espresso"))
}
