# Study overview module: a header for the loaded study, the sample metadata,
# and a library-size against detected-genes quality plot.
#
# The header is one strip rather than a row of tiles. Four coloured boxes each
# holding a bare number is a dashboard convention that says very little: 63,856
# means nothing without "genes measured" beside it. Here the study title leads,
# the accession and source sit under it as quiet metadata, and every number
# carries a unit and a caption.

study_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("header")),
    # A sidebar rather than a standalone control bar, matching how Quality,
    # PCA and Genes all put their plot settings: one font-size control here
    # governs both plots below, the same way dark mode already does, and
    # point size and labelling apply to the QC scatter specifically.
    layout_sidebar(
      sidebar = sidebar(
        title = "Plot settings",
        width = 280,
        plot_controls_ui(ns, size_default = 2.2)
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(
            "Library size against detected genes",
            plot_download_ui(ns, "qc")
          ),
          card_body(plotOutput(ns("qc"), height = "440px"))
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Expression distribution",
            plot_download_ui(ns, "distribution")
          ),
          card_body(plotOutput(ns("distribution"), height = "440px"))
        )
      )
    ),
    # The metadata gets the full width of the page. It used to share a row with
    # the plot and carry forty columns, which meant a horizontal scrollbar that
    # went on for screens. Now it shows a chosen few and the rest are one click
    # down, in the detail card below.
    card(
      card_header("Sample metadata"),
      card_body(
        selectizeInput(
          ns("columns"),
          "Columns",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(plugins = list("remove_button"))
        ),
        DT::DTOutput(ns("metadata"))
      )
    ),
    card(
      card_header("Sample detail"),
      card_body(DT::DTOutput(ns("detail")))
    )
  )
}

# One number, with the words that make it mean something.
stat_figure <- function(value, unit, caption) {
  div(
    class = "stat-figure",
    div(class = "stat-figure-value", value),
    div(class = "stat-figure-unit", unit),
    div(class = "stat-figure-caption", caption)
  )
}

study_overview_server <- function(id, study, dark = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    qc <- reactive({
      s <- study()
      req(s)
      sample_qc(s$rse)
    })

    # How many genes are seen in every sample, and in none.
    #
    # This is a full pass over a 63,856 by n matrix, so it is a reactive rather
    # than part of renderUI: without this the whole scan ran again on every
    # re-render, including every flip of the dark mode switch.
    detection <- reactive({
      s <- study()
      req(s)
      present <- rowSums(SummarizedExperiment::assay(s$rse, "counts") > 0)
      list(
        everywhere = sum(present == ncol(s$rse)),
        nowhere = sum(present == 0)
      )
    })

    output$header <- renderUI({
      s <- study()
      if (is.null(s)) {
        return(card(card_body(
          class = "text-muted",
          "Load a study from the Browse view first."
        )))
      }
      everywhere <- detection()$everywhere
      nowhere <- detection()$nowhere
      links <- study_external_links(study_link_row(s))

      div(
        class = "study-header",
        div(
          class = "study-header-top",
          div(
            h4(class = "study-header-title", s$title %||% s$project),
            div(
              class = "study-header-meta",
              paste(s$project, s$organism, toupper(s$source), sep = " · ")
            )
          ),
          div(
            class = "study-header-links",
            lapply(seq_along(links), function(i) {
              tags$a(
                href = links[[i]],
                target = "_blank",
                rel = "noopener",
                class = "btn btn-sm btn-outline-secondary",
                names(links)[i],
                bsicons::bs_icon("box-arrow-up-right")
              )
            })
          )
        ),
        div(
          class = "stat-figures",
          stat_figure(
            format(ncol(s$rse), big.mark = ","),
            "samples",
            "in this study"
          ),
          stat_figure(
            format(nrow(s$rse), big.mark = ","),
            "genes",
            "measured"
          ),
          stat_figure(
            format_reads(stats::median(qc()$library_size)),
            "median library",
            "reads per sample"
          ),
          stat_figure(
            format(everywhere, big.mark = ","),
            "genes",
            "detected in every sample"
          ),
          stat_figure(
            sprintf("%.0f%%", 100 * nowhere / nrow(s$rse)),
            "genes",
            "detected in no sample"
          )
        )
      )
    })

    # An argument rather than baked in, so the PDF stays light.
    qc_plot <- function(dark_mode = FALSE) {
      plot_qc(
        qc(),
        dark = dark_mode,
        point_size = input$point_size %||% 2.2,
        label = isTRUE(input$label_points),
        font_size = input$font_size %||% 14
      )
    }

    distribution <- reactive({
      s <- study()
      req(s)
      expression_quantiles(s$log_expr)
    })

    distribution_plot <- function(dark_mode = FALSE) {
      plot_expression_distribution(
        distribution(),
        dark = dark_mode,
        font_size = input$font_size %||% 14
      )
    }

    output$qc <- renderPlot({
      validate(need(study(), "Load a study from the Browse view first."))
      safe_plot(qc_plot(dark()), dark())
    })

    output$distribution <- renderPlot({
      validate(need(study(), "Load a study from the Browse view first."))
      safe_plot(distribution_plot(dark()), dark())
    })

    register_plot_downloads(
      output,
      "qc",
      filename = function() paste0(req(study())$project, "_qc"),
      builder = qc_plot
    )
    register_plot_downloads(
      output,
      "distribution",
      filename = function() paste0(req(study())$project, "_distribution"),
      builder = distribution_plot,
      width = 11
    )

    # ---- metadata ------------------------------------------------------------

    columns <- reactive({
      s <- study()
      req(s)
      metadata_columns(s$rse)
    })

    observeEvent(study(), {
      choices <- columns()
      updateSelectizeInput(
        session,
        "columns",
        choices = choices$all,
        selected = choices$default,
        server = TRUE
      )
    })

    output$metadata <- DT::renderDT(
      {
        s <- study()
        validate(need(s, "Load a study from the Browse view first."))
        # Before the picker has reported back, show the chosen default rather
        # than an empty table.
        chosen <- if (length(input$columns)) {
          input$columns
        } else {
          columns()$default
        }
        DT::datatable(
          metadata_table(s$rse, chosen),
          rownames = FALSE,
          selection = "single",
          # No scrollX. The point of the column picker is that the table only
          # ever holds what fits, and anything else is one click down.
          options = list(
            pageLength = 15,
            scrollY = "430px",
            scrollCollapse = TRUE,
            autoWidth = FALSE
          )
        )
      },
      server = TRUE
    )

    selected_sample <- reactive({
      s <- study()
      row <- input$metadata_rows_selected
      if (is.null(s) || !length(row)) {
        return(NULL)
      }
      colnames(s$rse)[[row]]
    })

    output$detail <- DT::renderDT(
      {
        s <- study()
        validate(need(s, "Load a study from the Browse view first."))
        sample <- selected_sample()
        validate(need(
          sample,
          sprintf(
            "Select a sample above to see all %d fields recorded for it.",
            ncol(SummarizedExperiment::colData(s$rse))
          )
        ))
        DT::datatable(
          sample_detail(s$rse, sample),
          rownames = FALSE,
          colnames = c("Field", "Value"),
          selection = "none",
          caption = sprintf("Every field recorded for %s", sample),
          options = list(
            pageLength = 15,
            scrollY = "360px",
            scrollCollapse = TRUE,
            columnDefs = list(list(targets = 0, width = "35%"))
          )
        )
      },
      server = TRUE
    )
  })
}
