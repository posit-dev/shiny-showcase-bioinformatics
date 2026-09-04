# Quality module: the checks you run before trusting a study.
#
# Everything here comes out of data the study already downloaded. recount3
# ships 109 numeric quality metrics per sample and a biotype for every gene,
# and until now the app plotted none of it.
#
# The module returns its current settings as a reactive so the export module
# can put them in the reproduction script.

quality_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Settings",
      width = 280,
      selectInput(
        ns("group_by"),
        "Group by",
        choices = c("No grouping" = "")
      ),
      sliderInput(
        ns("top_biotypes"),
        "Biotypes shown",
        min = 3,
        max = 12,
        value = 8,
        step = 1,
        ticks = FALSE
      ),
      selectInput(
        ns("cor_method"),
        "Correlation",
        choices = c("Spearman" = "spearman", "Pearson" = "pearson")
      ),
      plot_controls_ui(ns, size_default = 2.5)
    ),
    layout_columns(
      col_widths = c(12, 6, 6, 12),
      card(
        full_screen = TRUE,
        card_header("Quality metrics", plot_download_ui(ns, "metrics")),
        card_body(plotOutput(ns("metrics"), height = "460px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Library composition", plot_download_ui(ns, "biotype")),
        card_body(plotOutput(ns("biotype"), height = "460px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Donor sex", plot_download_ui(ns, "sex")),
        card_body(plotOutput(ns("sex"), height = "460px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Sample correlation", plot_download_ui(ns, "correlation")),
        card_body(plotOutput(ns("correlation"), height = "620px"))
      )
    )
  )
}

quality_server <- function(id, study, dark = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    observeEvent(study(), {
      updateSelectInput(
        session,
        "group_by",
        choices = c("No grouping" = "", metadata_group_choices(study()$rse))
      )
    })

    group_label <- reactive({
      if (isTruthy(input$group_by)) input$group_by else NULL
    })

    # Each of these is a reactive rather than recomputed inside renderPlot,
    # because the dark mode toggle re-renders every plot and none of this work
    # depends on the colour of the page.
    metrics <- reactive({
      s <- study()
      req(s)
      qc_metrics(s$rse, group_by = input$group_by)
    })

    biotype <- reactive({
      s <- study()
      req(s)
      biotype_composition(s$rse, top_n = input$top_biotypes %||% 8)
    })

    sex <- reactive({
      s <- study()
      req(s)
      sex_signal(s$rse)
    })

    correlation <- reactive({
      s <- study()
      req(s)
      m <- sample_correlation(
        s$log_expr,
        method = input$cor_method %||% "spearman"
      )
      if (is.null(m)) {
        return(NULL)
      }
      correlation_long(m)
    })

    # Built as functions taking `dark_mode`, so the download handlers can ask
    # for the same figure in light mode whatever the screen is showing.
    metrics_plot <- function(dark_mode = FALSE) {
      plot_qc_panel(
        metrics(),
        dark = dark_mode,
        group_label = group_label(),
        font_size = input$font_size %||% 21
      )
    }
    biotype_plot <- function(dark_mode = FALSE) {
      plot_biotype_composition(
        biotype(),
        dark = dark_mode,
        font_size = input$font_size %||% 21
      )
    }
    sex_plot <- function(dark_mode = FALSE) {
      plot_sex_check(
        sex(),
        dark = dark_mode,
        point_size = input$point_size %||% 2.5,
        label = isTRUE(input$label_points),
        font_size = input$font_size %||% 21
      )
    }
    correlation_plot <- function(dark_mode = FALSE) {
      plot_sample_correlation(
        correlation(),
        dark = dark_mode,
        method = input$cor_method %||% "spearman",
        font_size = input$font_size %||% 21
      )
    }

    needs_study <- "Load a study from the Browse view first."

    output$metrics <- renderPlot({
      validate(need(study(), needs_study))
      safe_plot(metrics_plot(dark()), dark())
    })

    output$biotype <- renderPlot({
      validate(need(study(), needs_study))
      safe_plot(biotype_plot(dark()), dark())
    })

    output$sex <- renderPlot({
      validate(need(study(), needs_study))
      safe_plot(sex_plot(dark()), dark())
    })

    output$correlation <- renderPlot({
      validate(need(study(), needs_study))
      safe_plot(correlation_plot(dark()), dark())
    })

    quality_downloads <- function(id_prefix, suffix, builder, height = 6) {
      register_plot_downloads(
        output,
        id_prefix,
        filename = function() paste0(req(study())$project, "_", suffix),
        builder = builder,
        height = height
      )
    }

    quality_downloads("metrics", "quality_metrics", metrics_plot)
    quality_downloads("biotype", "biotype_composition", biotype_plot)
    quality_downloads("sex", "donor_sex", sex_plot)
    # Taller than the rest, matching the card's own 620px against their
    # 460px, since a correlation grid reads better with more vertical room.
    quality_downloads(
      "correlation",
      "sample_correlation",
      correlation_plot,
      height = 8
    )

    reactive({
      list(
        group_by = input$group_by %||% "",
        top_biotypes = input$top_biotypes %||% 8,
        cor_method = input$cor_method %||% "spearman"
      )
    })
  })
}
