# Open Targets disease-association card. Lists the diseases most strongly
# associated with the gene and their association scores as a reactable.

opentargets_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Disease associations (Open Targets)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "200px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the Ensembl gene ID).
opentargets_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    diseases <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      opentargets_diseases(res$ensembl_gene)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_opentargets_gene(res$ensembl_gene), "Open Targets")
    })

    output$table <- reactable::renderReactable({
      res <- diseases()
      req(res, isTRUE(res$ok))
      df <- res$data
      reactable::reactable(
        df,
        searchable = TRUE,
        compact = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        columns = list(
          disease_id = reactable::colDef(show = FALSE),
          disease = reactable::colDef(
            name = "Disease",
            minWidth = 160,
            html = TRUE,
            # Link each disease to its Open Targets page.
            cell = function(value, index) {
              id <- df$disease_id[index]
              if (is.na(id) || id == "") {
                return(value)
              }
              sprintf(
                paste0(
                  '<a href="https://platform.opentargets.org/disease/%s"',
                  ' target="_blank" rel="noopener noreferrer">%s</a>'
                ),
                id,
                value
              )
            }
          ),
          score = reactable::colDef(
            name = "Association score",
            align = "right",
            format = reactable::colFormat(digits = 3)
          )
        )
      )
    })

    output$content <- renderUI({
      res <- diseases()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see disease associations."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        reactable::reactableOutput(ns("table")),
        if (!is_blank(res$count)) {
          tags$p(
            class = "text-muted small mt-2 mb-0",
            sprintf(
              "Showing top %d of %s associated diseases.",
              nrow(res$data),
              format(res$count, big.mark = ",")
            )
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    diseases
  })
}
