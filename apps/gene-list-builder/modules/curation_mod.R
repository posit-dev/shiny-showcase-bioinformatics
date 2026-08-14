# Module: AI curation -----------------------------------------------------------
# Owns the "Curate with AI" button. Calls the Gemini curator (which falls back to
# top-N-by-rank when no key/failure). Returns the curated tibble as a reactive.
# `curate_fn` is injectable for tests.

curation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("availability")),
    actionButton(ns("curate"), "Curate with AI", class = "btn-primary"),
    div(class = "mt-3", uiOutput(ns("notes"))),
    shinycssloaders::withSpinner(DT::dataTableOutput(ns("tbl")))
  )
}

curation_server <- function(
  id,
  ranked_reactive,
  disease_reactive,
  controls_reactive,
  curate_fn = curate_gene_list
) {
  moduleServer(id, function(input, output, session) {
    output$availability <- renderUI({
      if (gemini_available()) {
        div(
          class = "text-success small",
          "Gemini API key detected - AI curation enabled."
        )
      } else {
        div(
          class = "text-warning small",
          paste(
            "No GEMINI_API_KEY set - 'Curate' will select the top genes",
            "by combined rank."
          )
        )
      }
    })

    curated <- eventReactive(input$curate, {
      ranked <- ranked_reactive()
      validate(need(
        !is.null(ranked) && nrow(ranked) > 0,
        "Build a gene list first."
      ))
      disease <- disease_reactive()
      top_n <- controls_reactive()$top_n %||% 30
      withProgress(message = "Curating with AI...", value = 0.5, {
        curate_fn(ranked, disease$name %||% "the disease", top_n = top_n)
      })
    })

    output$notes <- renderUI({
      cur <- curated()
      msgs <- if (isTRUE(attr(cur, "ai_used"))) {
        "AI curation applied."
      } else {
        attr(cur, "message") %||% "Fallback selection (top genes by rank)."
      }
      if (!is.null(attr(cur, "error"))) {
        msgs <- c(msgs, paste("AI error:", attr(cur, "error")))
      }
      div(class = "small text-muted", lapply(msgs, function(m) p(m)))
    })

    output$tbl <- DT::renderDT(
      {
        cur <- curated()
        validate(need(nrow(cur) > 0, "No genes selected."))
        DT::datatable(
          cur,
          rownames = FALSE,
          selection = "none",
          options = list(pageLength = 15)
        ) |>
          DT::formatRound(columns = "confidence", digits = 2)
      },
      server = TRUE
    )

    curated
  })
}
