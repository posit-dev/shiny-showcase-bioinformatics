# Coverage module.
#
# The "can I run this experiment?" view: an interactive drug x cell-line matrix
# colored by how many cells were profiled for each combination, with a per-cell
# dose breakdown on click. Tahoe-100M is fully crossed (every drug in every
# cell line), so the signal here is cell depth and dose completeness, not
# presence/absence. All from the local cell grid, so it is fast.

# Cell depth uses the shared Lake-Tahoe sequential ramp (ltc "heatmap0"):
# tahoe_heatmap_cols (see R/theme.R), applied via scale_fill_gradientn below.

coverage_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("filters_sidebar"),
      width = 250,
      gap = "0.4rem",
      padding = "0.6rem",
      title = "Matrix",
      # tour_* ids anchor the guided demo (see R/tour.R).
      div(
        id = ns("tour_controls"),
        selectizeInput(
          ns("drugs"),
          "Drugs (rows)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Search drugs…")
        ),
        selectizeInput(
          ns("organs"),
          "Organs (columns)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All organs")
        ),
        div(
          class = "text-muted small",
          "Color = cells profiled (log scale). Click a tile for its dose",
          "breakdown."
        ),
        actionLink(ns("reset"), "Reset to top drugs", class = "small")
      )
    ),
    bslib::card(
      id = ns("tour_heatmap"),
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Cells profiled -- drug × cell line"),
        div(
          class = "d-flex gap-2 align-items-center",
          tahoe_plot_refresh_ui(ns("refresh_plots")),
          .info_pop(
            paste(
              "Each tile is one drug tested in one cell line; color shows how",
              "many cells were profiled (log scale). Tahoe-100M is fully",
              "crossed, so darker = more statistical power for that combination.",
              "Click a tile to see its per-dose cell counts.",
              "Use the redraw button if the plot looks the wrong size."
            ),
            title = "Coverage matrix"
          )
        )
      ),
      echarts4r::echarts4rOutput(ns("heatmap"), height = "72vh")
    ),
    bslib::card(
      id = ns("tour_detail"),
      full_screen = TRUE,
      height = "40vh",
      bslib::card_header(uiOutput(ns("detail_title"), inline = TRUE)),
      echarts4r::echarts4rOutput(ns("detail_plot"), height = "30vh")
    )
  )
}

# Row (drug) and column (cell line) orderings for the matrix. Cell lines are
# grouped by organ; drugs sorted by total cells. Shared by the plot and the
# click handler (the plotted category order is fed to the echarts axes).
.coverage_orders <- function(df) {
  list(
    line = unique(df$cell_name[order(df$organ, df$cell_name)]),
    drug = names(sort(tapply(df$n_cells, df$drug, sum)))
  )
}

# Heatmap of cells profiled per (drug x cell line), as a shared echarts4r
# Lake-depth heatmap. Colour maps on log10(cells); the tooltip recovers the real
# count. Clicking a tile emits `<id>_clicked_data` = c(cell_name, drug, log10).
.coverage_heatmap <- function(df, line_order = NULL, drug_order = NULL) {
  validate(need(nrow(df) > 0, "No drug × cell-line combinations selected."))
  if (is.null(line_order) || is.null(drug_order)) {
    ord <- .coverage_orders(df)
    line_order <- ord$line
    drug_order <- ord$drug
  }
  tip <- htmlwidgets::JS(paste0(
    "function(p){ return p.value[1] + ' \\u00d7 ' + p.value[0] + '<br/>' + ",
    "Math.round(Math.pow(10, p.value[2])).toLocaleString() + ' cells'; }"
  ))
  tahoe_echart_heatmap(
    df,
    x = "cell_name",
    y = "drug",
    z = "n_cells",
    x_levels = line_order,
    y_levels = drug_order,
    tooltip = tip
  )
}

# Per-dose cell counts for one (drug x cell line), summed over plates.
.coverage_dose_bar <- function(rows) {
  validate(need(nrow(rows) > 0, "No cells for this combination."))
  agg <- dplyr::summarise(
    dplyr::group_by(rows, conc),
    n_cells = sum(n_cells),
    n_plates = dplyr::n_distinct(plate),
    .groups = "drop"
  )
  agg <- agg[order(agg$conc), , drop = FALSE]
  agg$label <- ifelse(agg$conc == 0, "Control (DMSO)", paste0(agg$conc, " µM"))
  plot_df <- data.frame(
    label = agg$label,
    value = agg$n_cells,
    stringsAsFactors = FALSE
  )
  tahoe_echart_vbar(plot_df, tahoe_colors$primary, value_name = "Cells")
}

coverage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    coverage <- reactive(tahoe_coverage())

    # Redraw button: resize the (large) plots to their container if one gets
    # stuck at a stale size after a layout change.
    observeEvent(input$refresh_plots, {
      tahoe_plot_refresh_server(input$refresh_plots, session)
    })

    # Default drug selection: the top drugs by total cells profiled.
    top_drugs <- reactive({
      cov <- coverage()
      totals <- sort(tapply(cov$n_cells, cov$drug, sum), decreasing = TRUE)
      utils::head(names(totals), 30)
    })

    observeEvent(coverage(), once = TRUE, {
      cov <- coverage()
      updateSelectizeInput(
        session,
        "drugs",
        choices = sort(unique(cov$drug)),
        selected = top_drugs(),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "organs",
        choices = sort(unique(cov$organ)),
        selected = character(0),
        server = TRUE
      )
    })

    observeEvent(input$reset, {
      updateSelectizeInput(session, "drugs", selected = top_drugs())
      updateSelectizeInput(session, "organs", selected = character(0))
    })

    filtered <- reactive({
      cov <- coverage()
      if (!is.null(input$drugs) && length(input$drugs) > 0) {
        cov <- cov[cov$drug %in% input$drugs, , drop = FALSE]
      }
      if (!is.null(input$organs) && length(input$organs) > 0) {
        cov <- cov[cov$organ %in% input$organs, , drop = FALSE]
      }
      cov
    })

    orders <- reactive(.coverage_orders(filtered()))

    output$heatmap <- echarts4r::renderEcharts4r({
      o <- orders()
      .coverage_heatmap(filtered(), o$line, o$drug)
    })

    # echarts4r reports a clicked tile's datum as {value: [cell_name, drug,
    # log10cells]}, which arrives as a list with a `value` vector; read the
    # labels off it (no numeric-index mapping needed).
    clicked <- reactiveVal(NULL)
    observeEvent(input$heatmap_clicked_data, {
      d <- input$heatmap_clicked_data
      if (is.list(d) && !is.null(d$value)) {
        d <- d$value
      }
      if (is.null(d) || length(d) < 2) {
        return()
      }
      clicked(c(
        drug = as.character(d[[2]]),
        cell_name = as.character(d[[1]])
      ))
    })

    output$detail_title <- renderUI({
      cl <- clicked()
      if (is.null(cl)) {
        span("Dose breakdown -- click a tile")
      } else {
        tagList(
          "Dose breakdown -- ",
          tags$strong(cl[[1]]),
          " × ",
          tags$strong(cl[[2]])
        )
      }
    })

    output$detail_plot <- echarts4r::renderEcharts4r({
      cl <- clicked()
      validate(need(
        !is.null(cl),
        "Click a tile in the matrix to see its per-dose cell counts."
      ))
      g <- tahoe_cell_grid()
      rows <- g[g$drug == cl[[1]] & g$cell_name == cl[[2]], , drop = FALSE]
      .coverage_dose_bar(rows)
    })

    # --- Chat-assistant bridge: read and drive the matrix's drug/organ picks.
    # These are server-side selectize inputs, so `set` re-sends the choices with
    # the selection to make it stick.
    cov_bridge_get <- function() {
      cov <- isolate(coverage())
      list(
        filters = list(
          drugs = list(
            current = isolate(input$drugs),
            options = sort(unique(cov$drug))
          ),
          organs = list(
            current = isolate(input$organs),
            options = sort(unique(cov$organ))
          )
        )
      )
    }
    cov_bridge_set <- function(request) {
      cov <- isolate(coverage())
      ignored <- list()
      apply_multi <- function(field, input_id, domain) {
        v <- tahoe_bridge_validate(request[[field]], domain)
        if (length(v$bad) > 0) {
          ignored[[field]] <<- v$bad
        }
        if (!is.null(v$good)) {
          updateSelectizeInput(
            session,
            input_id,
            choices = domain,
            selected = v$good,
            server = TRUE
          )
        }
      }
      apply_multi("drugs", "drugs", sort(unique(cov$drug)))
      apply_multi("organs", "organs", sort(unique(cov$organ)))
      out <- list(applied = TRUE)
      if (length(ignored) > 0) {
        out$ignored <- ignored
      }
      out
    }
    tahoe_register_page_bridge(
      session,
      "coverage",
      list(title = "Coverage", get = cov_bridge_get, set = cov_bridge_set)
    )
  })
}
