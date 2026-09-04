# Export module: data downloads and the reproduction session.
#
# The session is offered as a plain R script, a Quarto notebook, or an R
# Markdown notebook. All three come from one builder, so they cannot drift
# apart, and the preview shows whichever is selected.

export_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Downloads"),
      card_body(
        class = "download-list",
        download_row(
          ns("rse"),
          "RangedSummarizedExperiment (.rds)",
          "Counts, metadata and gene annotation in one R object."
        ),
        download_row(
          ns("expression"),
          "log2 CPM matrix (.csv.gz)",
          "Genes by samples, the values every view plots."
        ),
        download_row(
          ns("metadata"),
          "Sample metadata (.csv)",
          "Every colData column, list columns flattened."
        ),
        tags$hr(),
        div(
          class = "small text-muted mb-2",
          "The session as a script or a notebook. All three run on recount3",
          "and ggplot2 alone, with nothing from this app."
        ),
        selectInput(
          ns("format"),
          "Format",
          choices = REPRODUCTION_FORMATS,
          selected = "r"
        ),
        downloadButton(
          ns("session"),
          "Download the session",
          class = "btn-outline-primary w-100"
        )
      )
    ),
    card(
      card_header(textOutput(ns("preview_title"), inline = TRUE)),
      card_body(verbatimTextOutput(ns("preview")))
    )
  )
}

# One download button plus the sentence that says what it gives you.
download_row <- function(id, label, hint) {
  div(
    class = "download-row",
    downloadButton(id, label, class = "btn-outline-primary w-100"),
    div(class = "text-muted small mt-1", hint)
  )
}

export_server <- function(
  id,
  study,
  gene_state,
  pca_state,
  quality_state = NULL
) {
  moduleServer(id, function(input, output, session) {
    format <- reactive(input$format %||% "r")

    session_text <- reactive({
      s <- study()
      req(s)
      build_reproduction(
        s,
        gene_state(),
        pca_state(),
        format = format(),
        quality_state = if (is.null(quality_state)) NULL else quality_state()
      )
    })

    output$preview_title <- renderText({
      name <- names(REPRODUCTION_FORMATS)[REPRODUCTION_FORMATS == format()]
      paste("Preview:", if (length(name)) name else "R script (.R)")
    })

    output$preview <- renderText({
      validate(need(study(), "Load a study from the Browse view first."))
      session_text()
    })

    output$session <- downloadHandler(
      filename = function() {
        paste0(
          req(study())$project,
          "_recount_explorer.",
          reproduction_extension(format())
        )
      },
      content = function(file) writeLines(session_text(), file)
    )

    output$rse <- downloadHandler(
      filename = function() paste0(req(study())$project, ".rds"),
      content = function(file) saveRDS(req(study())$rse, file)
    )

    output$expression <- downloadHandler(
      filename = function() paste0(req(study())$project, "_log2cpm.csv.gz"),
      content = function(file) {
        con <- gzfile(file, "w")
        on.exit(close(con), add = TRUE)
        utils::write.csv(
          expression_export_df(req(study())),
          con,
          row.names = FALSE
        )
      }
    )

    output$metadata <- downloadHandler(
      filename = function() paste0(req(study())$project, "_metadata.csv"),
      content = function(file) {
        utils::write.csv(
          flatten_coldata(req(study())$rse),
          file,
          row.names = FALSE
        )
      }
    )
  })
}
