# PCA module: sample-level principal component analysis on the genes with the
# highest variance, coloured by any categorical metadata column.
#
# The module returns its current settings as a reactive so the export module
# can put them in the reproduction script.

pca_explorer_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Settings",
      width = 280,
      sliderInput(
        ns("n_genes"),
        "Top variable genes",
        min = 100,
        max = 2000,
        value = 500,
        step = 100,
        # A PCA over 2,000 genes is not something to run sixty times during one
        # drag, so it recomputes on release.
        ticks = FALSE
      ),
      selectInput(
        ns("color_by"),
        "Colour by",
        choices = c("No colouring" = "")
      ),
      numericInput(
        ns("loading_pc"),
        "Loadings for PC",
        value = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      plot_controls_ui(ns, size_default = 2.5)
    ),
    layout_columns(
      col_widths = c(8, 4, 12),
      card(
        full_screen = TRUE,
        card_header("PC1 against PC2", plot_download_ui(ns, "scatter")),
        card_body(plotOutput(ns("scatter"), height = "500px"))
      ),
      card(
        full_screen = TRUE,
        card_header("Variance explained", plot_download_ui(ns, "scree")),
        card_body(plotOutput(ns("scree"), height = "500px"))
      ),
      # The scatter shows that samples separate. This shows why, which is the
      # question a reader asks next and the app could not answer before.
      card(
        full_screen = TRUE,
        card_header(
          "Genes driving this component",
          plot_download_ui(ns, "loadings")
        ),
        card_body(plotOutput(ns("loadings"), height = "520px"))
      )
    )
  )
}

pca_explorer_server <- function(id, study, dark = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    observeEvent(study(), {
      updateSelectInput(
        session,
        "color_by",
        choices = c("No colouring" = "", metadata_group_choices(study()$rse))
      )
    })

    pca <- reactive({
      s <- study()
      req(s)
      run_pca(s$log_expr, n_genes = input$n_genes)
    })

    scores <- reactive({
      s <- study()
      req(s)
      df <- pca()$scores
      # Carried so a labelled point can be named. run_pca() leaves it out of
      # the scores frame on purpose, so the frame stays purely principal
      # components.
      df$sample <- rownames(df)
      df$color <- if (isTruthy(input$color_by)) {
        collapse_levels(
          as.data.frame(SummarizedExperiment::colData(s$rse))[[input$color_by]]
        )
      } else {
        "sample"
      }
      df
    })

    loadings <- reactive({
      s <- study()
      req(s)
      pca_loadings(
        pca(),
        symbols = gene_symbols(s$rse),
        pc = input$loading_pc %||% 1
      )
    })

    # An argument rather than baked in, so the PDF stays light.
    current_plot <- function(dark_mode = FALSE) {
      plot_pca_scatter(
        scores(),
        pca()$var_explained,
        color_label = if (isTruthy(input$color_by)) input$color_by else NULL,
        dark = dark_mode,
        point_size = input$point_size %||% 2.5,
        label = isTRUE(input$label_points),
        font_size = input$font_size %||% 21
      )
    }

    scree_plot <- function(dark_mode = FALSE) {
      plot_pca_scree(
        pca()$var_explained,
        dark = dark_mode,
        font_size = input$font_size %||% 21
      )
    }

    loadings_plot <- function(dark_mode = FALSE) {
      plot_pca_loadings(
        loadings(),
        pc = input$loading_pc %||% 1,
        dark = dark_mode,
        font_size = input$font_size %||% 21
      )
    }

    output$scatter <- renderPlot({
      validate(need(study(), "Load a study from the Browse view first."))
      safe_plot(current_plot(dark()), dark())
    })

    output$scree <- renderPlot({
      validate(need(study(), "Load a study from the Browse view first."))
      safe_plot(scree_plot(dark()), dark())
    })

    output$loadings <- renderPlot({
      validate(need(study(), "Load a study from the Browse view first."))
      safe_plot(loadings_plot(dark()), dark())
    })

    register_plot_downloads(
      output,
      "scatter",
      filename = function() paste0(req(study())$project, "_pca"),
      builder = current_plot
    )
    register_plot_downloads(
      output,
      "scree",
      filename = function() paste0(req(study())$project, "_pca_scree"),
      builder = scree_plot
    )
    register_plot_downloads(
      output,
      "loadings",
      filename = function() {
        paste0(
          req(study())$project,
          "_pc",
          input$loading_pc %||% 1,
          "_loadings"
        )
      },
      builder = loadings_plot,
      width = 8,
      height = 7
    )

    reactive({
      list(
        n_genes = input$n_genes %||% 500,
        color_by = input$color_by %||% ""
      )
    })
  })
}
