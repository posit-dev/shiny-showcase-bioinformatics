# Small shared helpers.

# Null coalescing, defined locally so the app also runs on R < 4.4.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Named choices for a checkbox group, with the number of studies behind each
# option. Seeing "gtex (32)" before clicking saves a click that returns almost
# nothing.
labelled_counts <- function(values, column) {
  counts <- table(column)
  labels <- vapply(
    values,
    function(v) {
      n <- if (v %in% names(counts)) counts[[v]] else 0L
      sprintf("%s (%s)", v, format(n, big.mark = ","))
    },
    character(1)
  )
  stats::setNames(values, labels)
}

# Turn a "10-50" or "200-Inf" preset into a range. "any" means no bound.
preset_range <- function(preset) {
  preset <- preset %||% "any"
  if (identical(preset, "any")) {
    return(list(min = NULL, max = NULL))
  }
  parts <- strsplit(preset, "-", fixed = TRUE)[[1L]]
  list(
    min = as.numeric(parts[[1L]]),
    max = if (identical(parts[[2L]], "Inf")) NULL else as.numeric(parts[[2L]])
  )
}

# 8236451 becomes "8.2 M". Read counts run to tens of millions, and the digits
# past the first two carry no meaning for a reader scanning a header.
format_reads <- function(n) {
  n <- as.numeric(n)
  if (length(n) != 1L || is.na(n)) {
    return("unknown")
  }
  if (n >= 1e9) {
    return(sprintf("%.1f B", n / 1e9))
  }
  if (n >= 1e6) {
    return(sprintf("%.1f M", n / 1e6))
  }
  if (n >= 1e3) {
    return(sprintf("%.0f K", n / 1e3))
  }
  format(round(n), big.mark = ",")
}

# The one control every plot-bearing view needs: how large its text is.
#
# A separate helper rather than folded only into plot_controls_ui(), because
# some views (the gene explorer) have no points to size or label but still
# have axis text that can be too small to read.
font_size_ui <- function(ns, default = 21) {
  shiny::sliderInput(
    ns("font_size"),
    "Font size",
    min = 10,
    max = 26,
    value = default,
    step = 1,
    ticks = FALSE
  )
}

# The controls every scatter view needs.
#
# Font size, point size and labelling are what a reader wants to change on a
# scatter, and several views offer them, so the control block is written once
# here rather than several times in the modules. `include_font` is FALSE for
# a view that puts one font-size control somewhere else because it governs
# more than one plot, so the slider is not offered twice under two names.
plot_controls_ui <- function(
  ns,
  size_default = 2.2,
  label_default = FALSE,
  font_default = 21,
  include_font = TRUE
) {
  shiny::tagList(
    if (include_font) font_size_ui(ns, font_default),
    shiny::sliderInput(
      ns("point_size"),
      "Point size",
      min = 0.5,
      max = 6,
      value = size_default,
      step = 0.5,
      ticks = FALSE
    ),
    shiny::checkboxInput(
      ns("label_points"),
      "Label points",
      value = label_default
    ),
    shiny::helpText(
      "Above 30 points only the samples furthest from the middle are named."
    )
  )
}

# The pair of download buttons every plot-bearing card offers.
#
# Right-aligned in a card_header next to its title: `ms-auto` on the first
# button pushes the whole pair to the far edge, and the second follows it
# without needing the same margin.
plot_download_ui <- function(ns, id_prefix) {
  shiny::tagList(
    shiny::downloadButton(
      ns(paste0(id_prefix, "_pdf")),
      "PDF",
      class = "btn-sm ms-auto"
    ),
    shiny::downloadButton(
      ns(paste0(id_prefix, "_png")),
      "PNG",
      class = "btn-sm"
    )
  )
}

# The PDF and PNG handlers behind plot_download_ui(), registered together so
# the pair is never written out by hand at each call site.
#
# `builder` is called with `dark_mode = FALSE` for both formats, the same way
# the screen already asks the on-screen builder for a light figure whatever
# mode the page is showing: a downloaded figure going into a document or a
# slide should not carry the reader's dark mode along with it.
register_plot_downloads <- function(
  output,
  id_prefix,
  filename,
  builder,
  width = 9,
  height = 6,
  dpi = 200
) {
  output[[paste0(id_prefix, "_pdf")]] <- shiny::downloadHandler(
    filename = function() paste0(filename(), ".pdf"),
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = builder(FALSE),
        width = width,
        height = height
      )
    }
  )
  output[[paste0(id_prefix, "_png")]] <- shiny::downloadHandler(
    filename = function() paste0(filename(), ".png"),
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = builder(FALSE),
        width = width,
        height = height,
        dpi = dpi
      )
    }
  )
}
