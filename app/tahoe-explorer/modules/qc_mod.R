# QC guardrails module.
#
# Design/rigor checks to run before committing compute: which conditions are
# underpowered (too few cells), which drug x cell-line combos lack the full
# dose series, how the vehicle control is covered, and how drugs/cell lines map
# onto plates (batch structure). All from the local cell grid.

qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # tour_* ids anchor the guided demo (see R/tour.R).
    div(id = ns("tour_boxes"), uiOutput(ns("value_boxes"))),
    bslib::card(
      id = ns("tour_controls"),
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(8, 4),
          sliderInput(
            ns("min_cells"),
            "Minimum cells per condition (drug × cell line × dose) to consider well-powered",
            min = 0,
            max = 1000,
            value = 100,
            step = 50,
            width = "100%"
          ),
          div(
            class = "d-flex align-items-end h-100 pb-2",
            checkboxInput(
              ns("qc_only"),
              "Count only QC-passing cells (pass_filter = full)",
              value = FALSE
            )
          )
        )
      )
    ),
    bslib::layout_columns(
      id = ns("tour_power"),
      col_widths = c(6, 6),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Underpowered conditions"),
          div(
            class = "d-flex gap-2 align-items-center",
            tahoe_table_columns_ui(ns("under_table")),
            .info_pop(
              paste(
                "Treatment conditions (drug × cell line × dose) with fewer cells",
                "than the threshold above -- the smallest are most at risk of",
                "unstable differential-expression estimates. Raise the threshold",
                "to be more conservative."
              ),
              title = "Underpowered conditions"
            )
          )
        ),
        tahoe_table_ui(ns("under_table"))
      ),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Incomplete dose series"),
          div(
            class = "d-flex gap-2 align-items-center",
            tahoe_table_columns_ui(ns("dose_table")),
            .info_pop(
              paste(
                "Drug × cell-line combinations missing one or more of the three",
                "doses (0.05 / 0.5 / 5 µM) -- dose-response analyses for these are",
                "limited."
              ),
              title = "Incomplete dose series"
            )
          )
        ),
        tahoe_table_ui(ns("dose_table"))
      )
    ),
    bslib::layout_columns(
      id = ns("tour_batch"),
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        height = "46vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Vehicle control coverage (DMSO per cell line)"),
          .info_pop(
            paste(
              "Cells of the DMSO_TF vehicle control per cell line. Robust",
              "controls anchor every drug comparison; a cell line with few",
              "control cells weakens all its contrasts."
            ),
            title = "Control coverage"
          )
        ),
        echarts4r::echarts4rOutput(ns("control_plot"), height = "36vh")
      ),
      bslib::card(
        full_screen = TRUE,
        height = "46vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Plate / batch structure"),
          .info_pop(
            paste(
              "How cell lines and drugs map onto the plates. If a drug sits",
              "on only a few plates, its signal is confounded with those",
              "plates -- compare it against DMSO controls from the same plates."
            ),
            title = "Plate / batch structure"
          )
        ),
        uiOutput(ns("plate_note")),
        echarts4r::echarts4rOutput(ns("plate_plot"), height = "30vh")
      )
    ),
    bslib::card(
      id = ns("tour_phase"),
      full_screen = TRUE,
      height = "50vh",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Cell-cycle phase composition"),
        .info_pop(
          paste(
            "Share of profiled cells in each cell-cycle phase (G1 / S / G2M),",
            "by organ. A tissue or drug skewed toward S/G2M (cycling) vs G1",
            "(arrested) is a phenotype worth accounting for before",
            "differential-expression analysis. From the cell grid's phase",
            "counts."
          ),
          title = "Cell-cycle composition"
        )
      ),
      echarts4r::echarts4rOutput(ns("phase_plot"), height = "40vh")
    )
  )
}

# Stacked 100%-proportion bar of cell-cycle phase (G1/S/G2M) per organ: one
# echarts4r series per phase (stacked), with a phase legend and a percent axis.
# Shares are precomputed so the stack sums to 1.
.qc_phase_bar <- function(df) {
  validate(need(
    nrow(df) > 0,
    "Cell-cycle phase data is not available in this grid."
  ))
  df$total <- df$G1 + df$S + df$G2M
  df <- df[df$total > 0, , drop = FALSE]
  validate(need(nrow(df) > 0, "No cells to plot."))
  df <- df[order(-df$total), , drop = FALSE]
  wide <- data.frame(
    organ = df$organ,
    G1 = df$G1 / df$total,
    S = df$S / df$total,
    G2M = df$G2M / df$total,
    stringsAsFactors = FALSE
  )
  pct <- htmlwidgets::JS("function(v){ return Math.round(v * 100) + '%'; }")
  cols <- c(
    G1 = tahoe_colors$blue,
    S = tahoe_colors$primary,
    G2M = tahoe_colors$green
  )
  e <- echarts4r::e_charts(wide, organ, reorder = FALSE)
  for (ph in c("G1", "S", "G2M")) {
    e <- echarts4r::e_bar_(
      e,
      ph,
      stack = "phase",
      name = ph,
      barWidth = "70%",
      itemStyle = list(color = unname(cols[[ph]]))
    )
  }
  e <- e |> echarts4r::e_flip_coords()
  vaxis <- .tahoe_echart_value_axis("Share of cells")
  vaxis$max <- 1
  vaxis$axisLabel <- c(.tahoe_echart_axis_lbl, list(formatter = pct))
  e <- .tahoe_echart_axis(e, "x", vaxis)
  e <- .tahoe_echart_axis(e, "y", .tahoe_echart_cat_axis(inverse = TRUE))
  # Legend up top; the "Share of cells" title sits under the bottom axis, so a
  # bottom legend would collide with it. Extra bottom room keeps the title clear.
  e <- .tahoe_echart_common(
    e,
    legend = TRUE,
    legend_pos = "top",
    grid_top = "16%",
    grid_bottom = "16%"
  )
  echarts4r::e_tooltip(
    e,
    trigger = "item",
    backgroundColor = "rgba(255,255,255,0.96)",
    borderColor = tahoe_colors$grid,
    borderWidth = 1,
    textStyle = list(
      color = tahoe_colors$fg,
      fontFamily = "Inter, system-ui, sans-serif"
    ),
    valueFormatter = pct
  )
}

# Bar of DMSO control cells per cell line (ascending, so the weakest are first).
# Many cell lines, so the category labels are hidden (hover for the name).
.qc_control_bar <- function(df) {
  validate(need(nrow(df) > 0, "No control (DMSO) cells found."))
  df <- df[order(df$n_cells), , drop = FALSE]
  plot_df <- data.frame(
    label = df$cell_name,
    value = df$n_cells,
    stringsAsFactors = FALSE
  )
  tahoe_echart_vbar(
    plot_df,
    tahoe_colors$primary,
    value_name = "DMSO cells",
    hide_labels = TRUE
  )
}

# Distribution of how many plates each (non-control) drug appears on.
.qc_plate_bar <- function(df) {
  validate(need(nrow(df) > 0, "No drug/plate data."))
  tab <- as.data.frame(table(plates = df$n_plates), stringsAsFactors = FALSE)
  tab$plates <- as.integer(tab$plates)
  tab <- tab[order(tab$plates), , drop = FALSE]
  plot_df <- data.frame(
    label = as.character(tab$plates),
    value = tab$Freq,
    stringsAsFactors = FALSE
  )
  tahoe_echart_vbar(
    plot_df,
    tahoe_colors$blue,
    value_name = "Drugs",
    x_title = "Plates a drug appears on"
  )
}

qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    conditions <- reactive(tahoe_conditions())
    coverage <- reactive(tahoe_coverage())

    # Treatment conditions only (drop the dose-0 vehicle control).
    treatments <- reactive({
      cond <- conditions()
      cond[cond$conc > 0, , drop = FALSE]
    })

    # Which cell count defines "power": all cells, or only QC-passing (full)
    # cells when the toggle is on and the grid carries the QC-tier column.
    power_col <- reactive({
      if (isTRUE(input$qc_only) && "n_full" %in% names(conditions())) {
        "n_full"
      } else {
        "n_cells"
      }
    })

    underpowered <- reactive({
      thr <- input$min_cells %||% 100
      tr <- treatments()
      pc <- power_col()
      tr[tr[[pc]] < thr, , drop = FALSE]
    })

    incomplete <- reactive({
      cov <- coverage()
      cov[cov$drug != "DMSO_TF" & cov$n_doses < 3, , drop = FALSE]
    })

    control_by_line <- reactive({
      cond <- conditions()
      ctrl <- cond[cond$drug == "DMSO_TF", , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(ctrl, cell_name),
        n_cells = sum(n_cells),
        .groups = "drop"
      )
    })

    plates_per_drug <- reactive({
      g <- tahoe_cell_grid()
      d <- g[g$drug != "DMSO_TF", , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(d, drug),
        n_plates = dplyr::n_distinct(plate),
        .groups = "drop"
      )
    })

    # Cell-cycle phase counts per organ, from the grid's phase columns (present
    # only when the grid is the extended build). Empty otherwise -> the plot
    # shows an explanatory message rather than erroring.
    phase_by_organ <- reactive({
      g <- tahoe_cell_grid()
      if (!all(c("n_g1", "n_s", "n_g2m", "organ") %in% names(g))) {
        return(dplyr::tibble(
          organ = character(),
          G1 = numeric(),
          S = numeric(),
          G2M = numeric()
        ))
      }
      d <- g[!is.na(g$organ), , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(d, organ),
        G1 = sum(n_g1),
        S = sum(n_s),
        G2M = sum(n_g2m),
        .groups = "drop"
      )
    })

    output$value_boxes <- renderUI({
      thr <- input$min_cells %||% 100
      n_under <- nrow(underpowered())
      n_total <- nrow(treatments())
      pct <- if (n_total > 0) round(100 * n_under / n_total) else 0
      n_incomplete <- nrow(incomplete())
      ctrl <- control_by_line()
      min_ctrl <- if (nrow(ctrl) > 0) min(ctrl$n_cells) else NA_real_
      cond <- conditions()
      qc_rate <- if ("n_full" %in% names(cond) && sum(cond$n_cells) > 0) {
        sum(cond$n_full) / sum(cond$n_cells)
      } else {
        NA_real_
      }
      cell_word <- if (identical(power_col(), "n_full")) {
        "QC-passing cells"
      } else {
        "cells"
      }
      fmt <- function(x) {
        if (is.na(x)) "-" else format(x, big.mark = ",")
      }
      boxes <- list(
        bslib::value_box(
          "Underpowered conditions",
          fmt(n_under),
          paste0(pct, "% of treatments < ", fmt(thr), " ", cell_word),
          theme = if (pct > 25) "warning" else "primary",
          height = "120px"
        ),
        bslib::value_box(
          "Incomplete dose series",
          fmt(n_incomplete),
          "drug × cell-line combos missing a dose",
          theme = if (n_incomplete > 0) "warning" else "success",
          height = "120px"
        ),
        bslib::value_box(
          "Weakest control",
          fmt(min_ctrl),
          "fewest DMSO cells in any cell line",
          theme = "secondary",
          height = "120px"
        )
      )
      if (!is.na(qc_rate)) {
        boxes <- c(
          boxes,
          list(bslib::value_box(
            "QC pass rate",
            paste0(round(100 * qc_rate), "%"),
            "cells passing the full QC filter",
            theme = "secondary",
            height = "120px"
          ))
        )
      }
      do.call(bslib::layout_columns, c(list(fill = FALSE), boxes))
    })

    # Ordered, labelled view of the underpowered conditions for display.
    under_display <- reactive({
      df <- underpowered()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      has_full <- "n_full" %in% names(df)
      cols <- c(
        "drug",
        "cell_name",
        "organ",
        "conc",
        if (has_full) "n_full",
        "n_cells"
      )
      df <- df[order(df[[power_col()]]), cols]
      df$conc <- paste0(df$conc, " µM")
      df
    })
    under_cols <- function(df) {
      coldefs <- list(conc = reactable::colDef(name = "Dose"))
      if ("n_full" %in% names(df)) {
        coldefs$n_full <- reactable::colDef(name = "QC-pass cells")
      }
      coldefs
    }
    tahoe_table_server(
      "under_table",
      data = under_display,
      columns = under_cols,
      pagination = FALSE,
      height = "34vh",
      empty_message = "No underpowered conditions at this threshold."
    )

    dose_display <- reactive({
      df <- incomplete()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      df[
        order(df$n_doses),
        c("drug", "cell_name", "organ", "doses", "n_doses")
      ]
    })
    tahoe_table_server(
      "dose_table",
      data = dose_display,
      columns = list(
        doses = reactable::colDef(name = "Doses present (µM)"),
        n_doses = reactable::colDef(name = "# Doses")
      ),
      pagination = FALSE,
      height = "34vh",
      empty_message = "Every drug × cell-line combo has the full dose series."
    )

    output$control_plot <- echarts4r::renderEcharts4r({
      .qc_control_bar(control_by_line())
    })

    output$plate_note <- renderUI({
      g <- tahoe_cell_grid()
      validate(need(nrow(g) > 0, "No grid data."))
      n_lines <- dplyr::n_distinct(g$cell_name)
      n_plates <- dplyr::n_distinct(g$plate)
      per_line <- tapply(g$plate, g$cell_name, function(x) length(unique(x)))
      balanced <- all(per_line == n_plates)
      div(
        class = "small text-muted mb-2",
        if (balanced) {
          sprintf(
            paste(
              "All %d cell lines appear on all %d plates -- no cell-line/plate",
              "confounding. Drugs, however, vary (below): pair each with",
              "same-plate DMSO controls."
            ),
            n_lines,
            n_plates
          )
        } else {
          sprintf(
            "Cell lines span %d-%d plates; check for batch imbalance.",
            min(per_line),
            max(per_line)
          )
        }
      )
    })

    output$plate_plot <- echarts4r::renderEcharts4r({
      .qc_plate_bar(plates_per_drug())
    })

    output$phase_plot <- echarts4r::renderEcharts4r({
      .qc_phase_bar(phase_by_organ())
    })
  })
}
