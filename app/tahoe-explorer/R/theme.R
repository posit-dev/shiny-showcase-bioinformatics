# Shared plotting theme and palette for the app ("Alpine Lake").
#
# Charts are built with the echarts4r helpers further down (tahoe_echart_*),
# which give every chart one look: Inter typography, a cohesive Lake-Tahoe
# palette, rounded gradient bars, a hairline grid, and hover tooltips. The
# ggplot2 theme (tahoe_theme) and plotly helper (tahoe_plotly) are retained for
# any static/ggplot use, but the app's live charts are all echarts4r.

# Chrome + single-series accents. The accent hues are drawn from the CVD-safe
# categorical palette below so single-series bars stay on-palette; `primary`
# keeps the deep lake teal used across the app chrome (_brand.yml).
tahoe_colors <- list(
  primary = "#0B7285", # deep lake teal (brand chrome)
  blue = "#2A78D6", # lake blue
  green = "#1BAF7A", # aqua-green
  sand = "#EDA100", # sandstone / yellow
  orange = "#EB6834",
  violet = "#4A3AA7",
  slate = "#5F6B7A", # granite
  fg = "#212529",
  grid = "#E4E9ED"
)

# Ordered qualitative palette for categorical fills (drugs, organs, ...), from
# the ltc package's "minou" palette (loukesio/ltc-color-palettes): teal, red,
# gold, green, navy, granite-grey -- an on-brand Alpine-Lake set. Colorblind-
# safe: the minimum pairwise ΔE under protan/deutan/tritan simulation is ~14.5
# (well above the ≥12 target, and better than the previous palette's ~11), and
# none of the colours is washed-out enough to disappear as a bar fill on white.
# Re-validate with the dataviz palette validator / ltc::ltc_cvd() before changing.
tahoe_categorical <- as.character(ltc::ltc("minou"))

# Sequential ramp for heatmaps (e.g. the Coverage matrix): ltc's "heatmap0" --
# a deep-water navy -> lake teal -> aqua -> sand -> rust ramp that reads as
# Lake-Tahoe depth to shoreline. Use with scale_fill_gradientn().
tahoe_heatmap_cols <- as.character(ltc::ltc("heatmap0"))

#' n distinct categorical colors, interpolating when n exceeds the base set.
tahoe_pal <- function(n) {
  if (n <= length(tahoe_categorical)) {
    return(tahoe_categorical[seq_len(n)])
  }
  grDevices::colorRampPalette(tahoe_categorical)(n)
}

#' Shared ggplot2 theme: minimal, Inter-ish, restrained gridlines. Fonts are
#' applied in the browser by tahoe_plotly(), so no base_family is set here
#' (avoids R font-registration warnings).
tahoe_theme <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = tahoe_colors$grid,
        linewidth = 0.4
      ),
      axis.title = ggplot2::element_text(
        color = tahoe_colors$slate,
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        color = tahoe_colors$slate,
        size = ggplot2::rel(0.9)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        color = tahoe_colors$fg,
        size = ggplot2::rel(1.05)
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 6),
      legend.position = "none"
    )
}

#' Render a ggplot as a styled, interactive plotly widget. `tooltip` selects
#' which aesthetics appear on hover (defaults to x + y). Chrome is stripped to
#' a small mode bar (hover, zoom, PNG download) with no plotly logo. Pass a
#' `source` id to make the chart emit click events readable in a Shiny server
#' with `plotly::event_data("plotly_click", source = <id>)` -- pair it with a
#' `key` aesthetic on the marks so the click carries the clicked category.
tahoe_plotly <- function(p, tooltip = c("x", "y"), source = NULL) {
  src <- if (is.null(source)) "A" else source
  widget <- plotly::ggplotly(p, tooltip = tooltip, source = src)
  if (!is.null(source)) {
    widget <- plotly::event_register(widget, "plotly_click")
  }
  widget |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "lasso2d",
        "select2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "toggleSpikelines",
        "zoomIn2d",
        "zoomOut2d"
      )
    ) |>
    plotly::layout(
      font = list(
        family = "Inter, system-ui, sans-serif",
        size = 14,
        color = tahoe_colors$fg
      ),
      hoverlabel = list(
        font = list(family = "Inter, system-ui, sans-serif"),
        bgcolor = "white"
      ),
      margin = list(l = 8, r = 8, b = 8, t = 12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

# --- echarts4r: modern, on-brand charts ---------------------------------------
#
# The app's shared charting layer: softer, rounded, gradient-filled echarts4r
# charts that replace the flatter ggplot/plotly bars everywhere. Every chart
# shares one look -- Inter type, the Lake-Tahoe palette, a hairline grid, rounded
# caps, and a gentle load animation -- via the helpers below:
#   * tahoe_echart_hbar()      -- single-series horizontal bars (+ highlight)
#   * tahoe_echart_cat_hbar()  -- horizontal bars in per-category colours
#   * tahoe_echart_vbar()      -- single-series vertical bars
#   * tahoe_echart_hist()      -- histogram of a numeric vector
#   * tahoe_echart_heatmap()   -- a Lake-depth heatmap (Coverage matrix)
# Click-driven charts read echarts4r's `<id>_clicked_row` Shiny input and map it
# back to the plotted (largest-first) data frame -- see the modules.

# A base colour lightened toward white, for the far (light) end of a gradient.
.tahoe_echart_tint <- function(color, amount = 0.5) {
  colorspace::lighten(color, amount)
}

# `color` as an "rgba(r,g,b,a)" string, for dimming de-emphasised bars.
.tahoe_rgba <- function(color, alpha = 1) {
  rgb <- grDevices::col2rgb(color)[, 1]
  sprintf("rgba(%d,%d,%d,%g)", rgb[1], rgb[2], rgb[3], alpha)
}

# A left->right (horizontal) or bottom->top (vertical) linear gradient from
# `color` to a lighter tint, as ECharts gradient JS. `echarts` is the global
# object echarts4r injects, so the expression resolves in the browser.
.tahoe_echart_grad <- function(color, horizontal = TRUE) {
  tint <- .tahoe_echart_tint(color)
  coords <- if (horizontal) "0, 0, 1, 0" else "0, 0, 0, 1"
  stops <- if (horizontal) {
    sprintf("[{offset:0,color:'%s'},{offset:1,color:'%s'}]", color, tint)
  } else {
    sprintf("[{offset:0,color:'%s'},{offset:1,color:'%s'}]", tint, color)
  }
  htmlwidgets::JS(sprintf(
    "new echarts.graphic.LinearGradient(%s, %s)",
    coords,
    stops
  ))
}

# A rounded, gradient-filled bar itemStyle (right cap rounded for horizontal
# bars, top cap for vertical).
.tahoe_echart_bar_style <- function(color, horizontal = TRUE) {
  radius <- if (horizontal) c(0, 6, 6, 0) else c(4, 4, 0, 0)
  list(color = .tahoe_echart_grad(color, horizontal), borderRadius = radius)
}

# JS axis-label formatter that abbreviates thousands (1,200 -> 1.2k).
.tahoe_echart_num_fmt <- htmlwidgets::JS(
  "function(v){ return Math.abs(v) >= 1000 ? (v/1000).toLocaleString() + 'k' : v; }"
)

# Slate axis-label style shared by every axis.
.tahoe_echart_axis_lbl <- list(color = "#5F6B7A", fontSize = 12)

# Value-axis config: no line/ticks, hairline split lines, abbreviated labels.
.tahoe_echart_value_axis <- function(name = NULL) {
  list(
    name = name,
    nameLocation = "middle",
    nameGap = 30,
    nameTextStyle = list(color = "#5F6B7A", fontSize = 12, fontWeight = "bold"),
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = c(
      .tahoe_echart_axis_lbl,
      list(formatter = .tahoe_echart_num_fmt)
    ),
    splitLine = list(lineStyle = list(color = "#E4E9ED"))
  )
}

# Category-axis config: no line/ticks, slate labels, no split lines.
.tahoe_echart_cat_axis <- function(inverse = FALSE, show_labels = TRUE) {
  list(
    inverse = inverse,
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = c(.tahoe_echart_axis_lbl, list(show = show_labels)),
    splitLine = list(show = FALSE)
  )
}

# Apply an axis-config list to an echarts4r widget (axis options travel through
# e_x_axis()/e_y_axis()'s `...`, so splice the list in with do.call).
.tahoe_echart_axis <- function(e, which = c("x", "y"), cfg, index = 0) {
  which <- match.arg(which)
  fn <- if (which == "x") echarts4r::e_x_axis else echarts4r::e_y_axis
  do.call(fn, c(list(e, index = index), cfg))
}

# Shared, axis-agnostic styling: Inter type, a light tooltip with comma-grouped
# values, a soft grid, and a gentle load animation. `legend_pos` places the
# legend at the bottom (default) or top; a horizontal-bar chart with a bottom
# value-axis title puts its legend at the top so the two never overlap.
# `grid_top`/`grid_bottom` widen the plot margins when a legend or an axis title
# needs the room.
.tahoe_echart_common <- function(
  e,
  legend = FALSE,
  legend_pos = c("bottom", "top"),
  grid_top = "12%",
  grid_bottom = "6%"
) {
  legend_pos <- match.arg(legend_pos)
  e <- echarts4r::e_grid(
    e,
    left = "2%",
    right = "6%",
    top = grid_top,
    bottom = grid_bottom,
    containLabel = TRUE
  )
  e <- echarts4r::e_tooltip(
    e,
    trigger = "item",
    backgroundColor = "rgba(255,255,255,0.96)",
    borderColor = tahoe_colors$grid,
    borderWidth = 1,
    textStyle = list(
      color = tahoe_colors$fg,
      fontFamily = "Inter, system-ui, sans-serif"
    ),
    valueFormatter = htmlwidgets::JS(
      "function(v){ return (v==null) ? '' : Number(v).toLocaleString(); }"
    )
  )
  e <- if (identical(legend_pos, "top")) {
    echarts4r::e_legend(
      e,
      show = legend,
      top = 0,
      textStyle = list(color = "#5F6B7A")
    )
  } else {
    echarts4r::e_legend(
      e,
      show = legend,
      bottom = 0,
      textStyle = list(color = "#5F6B7A")
    )
  }
  e <- echarts4r::e_text_style(e, fontFamily = "Inter, system-ui, sans-serif")
  echarts4r::e_animation(e, duration = 650, easing = "cubicOut")
}

# Horizontal-bar core: builds one bar series over a (label, value) frame in the
# frame's own order, colours each bar from `fills` (a list of gradients/colours,
# aligned to the rows), and inverts the category axis so the FIRST row sits at
# the top. Keeping the frame's order means an `<id>_clicked_row` maps straight
# back to the row -- so the click-driven pages need no separate index mapping.
.tahoe_echart_hbar_core <- function(df, fills, value_name = "Count") {
  validate(need(nrow(df) > 0, "No data to plot."))
  df$label <- as.character(df$label)
  e <- df |>
    echarts4r::e_charts(label, reorder = FALSE) |>
    echarts4r::e_bar(value, name = value_name, legend = FALSE, barWidth = "62%")
  # e_bar stores each point as {value: [label, n]}; keep that and only ADD a
  # per-bar itemStyle colour (re-wrapping `value` would double-nest and break it).
  data <- e$x$opts$series[[1]]$data
  e$x$opts$series[[1]]$data <- lapply(seq_along(data), function(i) {
    entry <- data[[i]]
    if (!is.list(entry)) {
      entry <- list(value = entry)
    }
    entry$itemStyle <- list(color = fills[[i]])
    entry
  })
  e$x$opts$series[[1]]$itemStyle <- list(borderRadius = c(0, 6, 6, 0))
  e <- e |> echarts4r::e_flip_coords()
  e <- .tahoe_echart_axis(e, "x", .tahoe_echart_value_axis())
  e <- .tahoe_echart_axis(e, "y", .tahoe_echart_cat_axis(inverse = TRUE))
  .tahoe_echart_common(e)
}

#' Horizontal bar chart of a tidy (label, value) frame, echarts4r-styled: teal
#' gradient bars with rounded caps, largest on top (the frame should arrive
#' largest-first). `color` is the gradient base; `highlight` is an optional
#' vector of labels to paint in `highlight_color` (the rest keep `color`).
tahoe_echart_hbar <- function(
  df,
  color,
  value_name = "Count",
  highlight = NULL,
  highlight_color = tahoe_colors$orange
) {
  validate(need(nrow(df) > 0, "No data to plot."))
  base <- .tahoe_echart_grad(color, horizontal = TRUE)
  fills <- if (is.null(highlight)) {
    rep(list(base), nrow(df))
  } else {
    hl <- .tahoe_echart_grad(highlight_color, horizontal = TRUE)
    lapply(as.character(df$label), function(l) {
      if (l %in% highlight) hl else base
    })
  }
  .tahoe_echart_hbar_core(df, fills, value_name)
}

#' Horizontal bars painted in per-category colours (drugs, organs, variant
#' classes). `colors` is a colour keyed by label; `selected`, when given, keeps
#' that one bar in full colour and dims the rest so a picked category stands out.
tahoe_echart_cat_hbar <- function(
  df,
  colors,
  value_name = "Count",
  selected = NULL
) {
  validate(need(nrow(df) > 0, "No data to plot."))
  fills <- lapply(as.character(df$label), function(l) {
    col <- colors[[l]] %||% tahoe_colors$slate
    if (!is.null(selected) && !identical(l, selected)) {
      .tahoe_rgba(col, 0.28)
    } else {
      .tahoe_echart_grad(col, horizontal = TRUE)
    }
  })
  .tahoe_echart_hbar_core(df, fills, value_name)
}

#' Single-series vertical bar chart of a (label, value) frame in the frame's own
#' order: gradient bars with rounded tops. `x_title` labels the category axis;
#' `hide_labels` drops the category tick labels (for many-category charts).
tahoe_echart_vbar <- function(
  df,
  color,
  value_name = "Count",
  x_title = NULL,
  hide_labels = FALSE
) {
  validate(need(nrow(df) > 0, "No data to plot."))
  df$label <- as.character(df$label)
  cat_axis <- .tahoe_echart_cat_axis(show_labels = !hide_labels)
  cat_axis$name <- x_title
  cat_axis$nameLocation <- "middle"
  cat_axis$nameGap <- 26
  cat_axis$nameTextStyle <- list(
    color = "#5F6B7A",
    fontSize = 12,
    fontWeight = "bold"
  )
  df |>
    echarts4r::e_charts(label, reorder = FALSE) |>
    echarts4r::e_bar(
      value,
      name = value_name,
      legend = FALSE,
      barWidth = "62%",
      itemStyle = .tahoe_echart_bar_style(color, horizontal = FALSE)
    ) |>
    (\(e) .tahoe_echart_axis(e, "x", cat_axis))() |>
    (\(e) .tahoe_echart_axis(e, "y", .tahoe_echart_value_axis(value_name)))() |>
    # Reserve extra bottom room for the x-axis title so it is never clipped.
    .tahoe_echart_common(grid_bottom = if (is.null(x_title)) "6%" else "20%")
}

#' Histogram of a numeric vector, echarts4r-styled to match tahoe_echart_vbar:
#' vertical gradient bars with rounded tops. `x_title` labels the value axis;
#' non-finite values are dropped.
tahoe_echart_hist <- function(values, color, x_title = "Value", bins = 20) {
  vals <- values[is.finite(values)]
  validate(need(length(vals) > 0, "No data to plot."))
  data.frame(value = vals) |>
    echarts4r::e_charts() |>
    echarts4r::e_histogram(
      value,
      name = x_title,
      breaks = bins,
      legend = FALSE,
      bar_width = "97%",
      itemStyle = .tahoe_echart_bar_style(color, horizontal = FALSE)
    ) |>
    (\(e) .tahoe_echart_axis(e, "x", .tahoe_echart_value_axis(x_title)))() |>
    (\(e) .tahoe_echart_axis(e, "y", .tahoe_echart_value_axis("Samples")))() |>
    .tahoe_echart_common()
}

#' Heatmap of a long (x, y, z) frame in the shared Lake-depth ramp, for the
#' Coverage matrix. `x`/`y`/`z` are column names; `x_levels`/`y_levels` fix the
#' axis category order; colour maps on log10(z) (via a hidden `.logz` column) so
#' depth reads like the ggplot version, while `tooltip` (a JS formatter) shows
#' the real counts. Emits `<id>_clicked_data` = c(x_index, y_index, log_z).
tahoe_echart_heatmap <- function(
  df,
  x,
  y,
  z,
  x_levels,
  y_levels,
  tooltip = NULL
) {
  validate(need(nrow(df) > 0, "No data to plot."))
  df[[x]] <- factor(as.character(df[[x]]), levels = x_levels)
  df[[y]] <- factor(as.character(df[[y]]), levels = y_levels)
  df$.logz <- log10(pmax(df[[z]], 1))
  rng <- range(df$.logz, finite = TRUE)
  x_axis <- .tahoe_echart_cat_axis()
  x_axis$axisLabel <- c(.tahoe_echart_axis_lbl, list(rotate = 45))
  e <- df |>
    echarts4r::e_charts_(x, reorder = FALSE) |>
    echarts4r::e_heatmap_(
      y,
      ".logz",
      name = "Cells",
      itemStyle = list(borderColor = "#ffffff", borderWidth = 1)
    ) |>
    echarts4r::e_visual_map(
      min = rng[1],
      max = rng[2],
      show = FALSE,
      inRange = list(color = as.character(tahoe_heatmap_cols))
    )
  e <- .tahoe_echart_axis(e, "x", x_axis)
  e <- .tahoe_echart_axis(e, "y", .tahoe_echart_cat_axis())
  e <- e |>
    echarts4r::e_grid(
      left = "2%",
      right = "4%",
      top = "4%",
      bottom = "16%",
      containLabel = TRUE
    ) |>
    echarts4r::e_text_style(fontFamily = "Inter, system-ui, sans-serif") |>
    echarts4r::e_animation(duration = 500)
  if (!is.null(tooltip)) {
    e <- echarts4r::e_tooltip(
      e,
      trigger = "item",
      backgroundColor = "rgba(255,255,255,0.96)",
      borderColor = tahoe_colors$grid,
      borderWidth = 1,
      textStyle = list(
        color = tahoe_colors$fg,
        fontFamily = "Inter, system-ui, sans-serif"
      ),
      formatter = tooltip
    )
  }
  e
}

#' Alpine Lake reactable theme -- shared look for every table in the app: Inter
#' type, teal highlights, restrained hairline borders, tabular figures.
tahoe_reactable_theme <- function() {
  reactable::reactableTheme(
    color = tahoe_colors$fg,
    borderColor = tahoe_colors$grid,
    highlightColor = "#EAF3F5", # pale teal wash
    cellPadding = "8px 10px",
    style = list(
      fontFamily = "Inter, system-ui, sans-serif",
      fontSize = "14px"
    ),
    headerStyle = list(
      color = tahoe_colors$slate,
      fontWeight = 600,
      borderBottom = paste0("2px solid ", tahoe_colors$grid)
    ),
    searchInputStyle = list(width = "100%")
  )
}

#' A reactable with the app's shared, UI-friendly defaults: searchable,
#' sortable, compact, row highlight, paginated, tabular figures, Alpine theme.
#' Column headers are normalized to Title Case with janitor::clean_names() so
#' every table reads consistently; pass `columns` colDefs to override specific
#' ones (an explicit `name` wins, otherwise the Title Case label is injected).
#' `hidden` names columns to render collapsed (still present, just not shown) --
#' the reusable column chooser (tahoe_table) drives this from a checkbox list.
#' Extra reactable() arguments (e.g. `selection`, `onClick`) pass through `...`.
tahoe_reactable <- function(
  data,
  columns = list(),
  ...,
  page_size = 10,
  hidden = character(0)
) {
  labels <- janitor::make_clean_names(names(data), case = "title")
  names(labels) <- names(data)
  col_defs <- list()
  for (nm in names(data)) {
    override <- columns[[nm]]
    if (is.null(override)) {
      override <- reactable::colDef(name = labels[[nm]])
    } else if (is.null(override$name)) {
      override$name <- labels[[nm]]
    }
    if (nm %in% hidden) {
      override$show <- FALSE
    }
    col_defs[[nm]] <- override
  }
  reactable::reactable(
    data,
    columns = col_defs,
    searchable = TRUE,
    sortable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    borderless = FALSE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50),
    theme = tahoe_reactable_theme(),
    ...
  )
}

# A small "redraw" button for a plot card header. Plotly widgets occasionally
# get stuck at a stale size when their container changes (a sidebar opening, a
# full-screen toggle) without a window-resize firing. Clicking this asks the
# browser to resize every plot to its current container (see the
# `tahoe_resize_plots` handler wired in ui.R). `id` must be the caller's
# namespaced input id.
tahoe_plot_refresh_ui <- function(id) {
  actionButton(
    id,
    label = NULL,
    icon = shiny::icon("arrows-rotate"),
    class = "btn-sm btn-link text-secondary p-0",
    title = "Redraw plots at the current size"
  )
}

# Handle a refresh click: tell the browser to resize plots to their containers.
tahoe_plot_refresh_server <- function(input_value, session) {
  session$sendCustomMessage("tahoe_resize_plots", list())
}

# Make the shared theme the default for every ggplot in the app.
ggplot2::theme_set(tahoe_theme())
