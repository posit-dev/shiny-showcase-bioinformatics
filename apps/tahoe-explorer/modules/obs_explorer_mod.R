# Samples & cell-level obs explorer module.
#
# Two sections:
#   1. Samples/plates -- always available, backed by the small sample_metadata
#      table. Filter by plate and drug, add parsed dose, chart and export.
#   2. Cell-level obs -- a lazy, guarded summary over the huge cell-level obs
#      table via tahoe_obs_summary(), which aggregates in duckdb and never
#      pulls raw cells into R. When the source is remote (HuggingFace) the
#      query is gated behind a "Run query" button so nothing heavy fires on
#      load; for local/fixture sources it computes reactively.

# Group-by choices exposed in the obs section (a friendly subset of the data
# layer's whitelist).
.obs_group_choices <- c(
  "Drug" = "drug",
  "Cell line" = "cell_line",
  "Plate" = "plate",
  "Cell-cycle phase" = "phase",
  "QC filter tier" = "pass_filter"
)

# Metric choices exposed in the obs section.
.obs_metric_choices <- c(
  "Cell count" = "n_cells",
  "Mean mito fraction" = "mean_pcnt_mito",
  "Mean transcript count" = "mean_tscp_count",
  "Mean gene count" = "mean_gene_count"
)

# Human label for the obs source type.
.obs_source_label <- function(type) {
  switch(
    type,
    local = "local file",
    remote = "remote (HuggingFace, slower)",
    fixture = "demo fixture",
    type
  )
}

# Bootstrap badge colour for the obs source type.
.obs_source_theme <- function(type) {
  switch(
    type,
    local = "text-bg-success",
    remote = "text-bg-warning",
    fixture = "text-bg-secondary",
    "text-bg-secondary"
  )
}


obs_explorer_ui <- function(id) {
  ns <- NS(id)
  source_type <- tahoe_obs_source()$type

  samples_tab <- bslib::nav_panel(
    title = "Samples & plates",
    value = "samples",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        id = ns("filters_sidebar"),
        title = "Filters",
        # tour_* ids anchor the guided demo (see R/tour.R).
        div(
          id = ns("tour_filters"),
          selectizeInput(
            ns("sample_plate"),
            "Plate",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All plates")
          ),
          selectizeInput(
            ns("sample_drug"),
            "Drug",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All drugs")
          )
        )
      ),
      bslib::layout_columns(
        id = ns("tour_charts"),
        col_widths = c(6, 6),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Samples per plate"),
          echarts4r::echarts4rOutput(ns("plate_plot"), height = "340px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Samples per drug (top 12)"),
          echarts4r::echarts4rOutput(ns("drug_plot"), height = "340px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Distribution of mean mito fraction"),
          echarts4r::echarts4rOutput(ns("mito_plot"), height = "320px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Distribution of mean transcript count"),
          echarts4r::echarts4rOutput(ns("tscp_plot"), height = "320px")
        )
      ),
      bslib::card(
        id = ns("tour_table"),
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Filtered samples"),
          tahoe_table_columns_ui(ns("sample_table"))
        ),
        tahoe_table_ui(ns("sample_table")),
        tags$hr(),
        subset_export_ui(ns("sample_export"))
      )
    )
  )

  obs_tab <- bslib::nav_panel(
    title = "Cell-level obs",
    value = "cells",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        id = ns("query_sidebar"),
        title = "Query",
        tags$span(
          class = paste("badge", .obs_source_theme(source_type)),
          .obs_source_label(source_type)
        ),
        p(
          class = "text-muted small mt-2",
          "Aggregated lazily in duckdb; raw cells are never loaded."
        ),
        selectInput(
          ns("obs_group"),
          "Group by",
          choices = .obs_group_choices
        ),
        selectInput(
          ns("obs_metric"),
          "Metric",
          choices = .obs_metric_choices
        ),
        selectizeInput(
          ns("obs_drug"),
          "Drug filter (optional)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All drugs")
        ),
        if (identical(source_type, "remote")) {
          actionButton(
            ns("run"),
            "Run query",
            class = "btn-warning btn-sm",
            icon = icon("play")
          )
        },
        uiOutput(ns("obs_status"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Summary"),
        echarts4r::echarts4rOutput(ns("obs_plot"), height = "380px")
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Summary table"),
          tahoe_table_columns_ui(ns("obs_table"))
        ),
        tahoe_table_ui(ns("obs_table")),
        tags$hr(),
        subset_export_ui(ns("obs_export"))
      )
    )
  )

  tagList(
    div(
      class = "p-2",
      h3("Samples & cells"),
      p(
        class = "text-muted",
        "Filter samples and their plates, then drill into lazily aggregated",
        "cell-level metadata without loading the full cell table."
      )
    ),
    div(
      id = ns("tour_views"),
      # id lets the server react when the sub-tab changes, so the newly shown
      # sub-tab's filter sidebar can be re-laid-out (each sub-tab owns its own).
      bslib::navset_card_tab(id = ns("view"), samples_tab, obs_tab)
    )
  )
}

obs_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    source_type <- tahoe_obs_source()$type

    # ---- Samples/plates section ------------------------------------------

    # Full sample table with parsed dose columns attached once.
    samples_all <- reactive({
      df <- tahoe_sample()
      dose <- tahoe_parse_dose(df$drugname_drugconc)
      df$conc <- dose$conc
      df$unit <- dose$unit
      df
    })

    # Populate the filter dropdowns from the loaded data.
    observe({
      df <- samples_all()
      updateSelectizeInput(
        session,
        "sample_plate",
        choices = sort(unique(df$plate)),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "sample_drug",
        choices = sort(unique(df$drug)),
        server = TRUE
      )
    })

    # Samples narrowed by the active plate/drug filters.
    samples_filtered <- reactive({
      df <- samples_all()
      if (length(input$sample_plate) > 0) {
        df <- df[df$plate %in% input$sample_plate, , drop = FALSE]
      }
      if (length(input$sample_drug) > 0) {
        df <- df[df$drug %in% input$sample_drug, , drop = FALSE]
      }
      df
    })

    output$plate_plot <- echarts4r::renderEcharts4r({
      df <- samples_filtered()
      counts <- sort(table(df$plate), decreasing = TRUE)
      plot_df <- data.frame(
        label = names(counts),
        value = as.integer(counts),
        stringsAsFactors = FALSE
      )
      tahoe_echart_hbar(plot_df, tahoe_colors$primary, "Samples")
    })

    output$drug_plot <- echarts4r::renderEcharts4r({
      df <- samples_filtered()
      counts <- utils::head(sort(table(df$drug), decreasing = TRUE), 12)
      plot_df <- data.frame(
        label = names(counts),
        value = as.integer(counts),
        stringsAsFactors = FALSE
      )
      tahoe_echart_hbar(plot_df, tahoe_colors$green, "Samples")
    })

    output$mito_plot <- echarts4r::renderEcharts4r({
      tahoe_echart_hist(
        samples_filtered()$mean_pcnt_mito,
        tahoe_colors$blue,
        "Mean mito fraction"
      )
    })

    output$tscp_plot <- echarts4r::renderEcharts4r({
      tahoe_echart_hist(
        samples_filtered()$mean_tscp_count,
        tahoe_colors$violet,
        "Mean transcript count"
      )
    })

    tahoe_table_server("sample_table", data = samples_filtered)

    subset_export_server(
      "sample_export",
      data_reactive = samples_filtered,
      file_stem = "samples_subset"
    )

    # ---- Cell-level obs section ------------------------------------------

    # Enumerate drug choices for the obs filter from the obs table itself
    # (grouping by drug returns one row per distinct drug). This is a small,
    # lazy query, so it is safe on local/fixture; for remote we defer it until
    # the section is used by wiring it to the same query trigger below.
    obs_drug_choices <- reactive({
      # Drug names come from the small drug table, not a full scan of the
      # 2.29 GB obs parquet.
      sort(unique(tahoe_drug()$drug))
    })

    # For local/fixture sources populate the drug filter eagerly; for remote,
    # wait until the user first runs a query so nothing heavy fires on load.
    obs_drug_choices_gated <- if (identical(source_type, "remote")) {
      bindEvent(obs_drug_choices, input$run, ignoreNULL = TRUE)
    } else {
      obs_drug_choices
    }

    observe({
      updateSelectizeInput(
        session,
        "obs_drug",
        choices = obs_drug_choices_gated(),
        server = TRUE
      )
    })

    # Compute the aggregated summary. Wrapped so we can gate it behind the
    # explicit "Run query" button when the source is remote.
    obs_query <- function() {
      req(input$obs_group, input$obs_metric)
      filters <- list()
      if (length(input$obs_drug) > 0) {
        filters$drug <- input$obs_drug
      }
      tahoe_obs_summary(
        group_by = input$obs_group,
        filters = filters,
        metric = input$obs_metric,
        limit = 100
      )
    }

    # When async is enabled AND the source is remote, run the heavy summary in a
    # background worker so it never blocks the main process (triple-gated:
    # TAHOE_ASYNC + remote + explicit "Run query"). Otherwise keep the original
    # synchronous behaviour exactly. The drug-choices helper above stays
    # synchronous -- it fires once on first run; async-ing it too is a follow-up.
    use_async_obs <- .tahoe_async_enabled() && identical(source_type, "remote")
    obs_task <- if (use_async_obs) tahoe_make_obs_task() else NULL

    obs_summary <- if (use_async_obs) {
      observeEvent(
        input$run,
        {
          req(input$obs_group, input$obs_metric)
          filters <- list()
          if (length(input$obs_drug) > 0) {
            filters$drug <- input$obs_drug
          }
          obs_task$invoke(input$obs_group, filters, input$obs_metric, 100)
        },
        ignoreNULL = TRUE
      )
      reactive(obs_task$result())
    } else if (identical(source_type, "remote")) {
      eventReactive(input$run, obs_query(), ignoreNULL = TRUE)
    } else {
      reactive(obs_query())
    }

    # Background-query progress hint (async remote only; NULL otherwise).
    output$obs_status <- renderUI({
      if (is.null(obs_task) || !identical(obs_task$status(), "running")) {
        return(NULL)
      }
      div(
        class = "text-muted small mt-2",
        "Running query in the background…"
      )
    })

    # Tidy frame for plotting/tabling: friendly, stable column names and a
    # graceful message when the underlying query failed.
    obs_result <- reactive({
      res <- obs_summary()
      req(res)
      err <- attr(res, "tahoe_error")
      validate(need(
        is.null(err),
        paste0(
          "Could not read the cell-level obs source (",
          .obs_source_label(source_type),
          "). ",
          if (identical(source_type, "remote")) {
            "Check your network connection and try again."
          } else {
            "The data may be unavailable."
          }
        )
      ))
      validate(need(nrow(res) > 0, "No cells match the current selection."))
      res
    })

    # Plot data with the group column renamed to `label` for ggplot.
    obs_plot_df <- reactive({
      res <- obs_result()
      group_col <- names(res)[[1]]
      utils::head(
        data.frame(
          label = as.character(res[[group_col]]),
          value = res$value,
          stringsAsFactors = FALSE
        ),
        20
      )
    })

    output$obs_plot <- echarts4r::renderEcharts4r({
      metric_label <- names(.obs_metric_choices)[
        match(input$obs_metric, .obs_metric_choices)
      ]
      tahoe_echart_hbar(
        obs_plot_df(),
        tahoe_colors$sand,
        value_name = metric_label
      )
    })

    tahoe_table_server("obs_table", data = obs_result)

    subset_export_server(
      "obs_export",
      data_reactive = obs_result,
      file_stem = "obs_summary"
    )

    # --- Chat-assistant bridge: read and drive the sample plate/drug filters.
    # Server-side selectize inputs, so `set` re-sends choices with the selection.
    obs_bridge_get <- function() {
      df <- isolate(samples_all())
      list(
        filters = list(
          plates = list(
            current = isolate(input$sample_plate),
            options = sort(unique(df$plate))
          ),
          drugs = list(
            current = isolate(input$sample_drug),
            options = sort(unique(df$drug))
          )
        ),
        matched_samples = nrow(isolate(samples_filtered()))
      )
    }
    obs_bridge_set <- function(request) {
      df <- isolate(samples_all())
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
      apply_multi("plates", "sample_plate", sort(unique(df$plate)))
      apply_multi("drugs", "sample_drug", sort(unique(df$drug)))
      out <- list(applied = TRUE)
      if (length(ignored) > 0) {
        out$ignored <- ignored
      }
      out
    }
    tahoe_register_page_bridge(
      session,
      "obs",
      list(
        title = "Samples & cells",
        get = obs_bridge_get,
        set = obs_bridge_set
      )
    )

    # Expose reactives so tests (and callers) can inspect module state.
    list(
      samples_filtered = samples_filtered,
      obs_result = obs_result
    )
  })
}
