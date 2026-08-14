# Known-drugs card. Lists the drugs and clinical candidates that target the
# gene's protein (Open Targets), with their highest clinical stage and the
# indication(s) they have been tried against, as a reactable.

drugs_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Known drugs (Open Targets)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "200px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the Ensembl gene ID).
drugs_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    drugs <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      opentargets_drugs(res$ensembl_gene)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_opentargets_drugs(res$ensembl_gene), "Open Targets")
    })

    output$table <- reactable::renderReactable({
      res <- drugs()
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
          drug_id = reactable::colDef(show = FALSE),
          drug = reactable::colDef(
            name = "Drug",
            minWidth = 140,
            html = TRUE,
            # Link each drug to its Open Targets page.
            cell = function(value, index) {
              id <- df$drug_id[index]
              if (is.na(id) || id == "") {
                return(value)
              }
              sprintf(
                paste0(
                  '<a href="https://platform.opentargets.org/drug/%s"',
                  ' target="_blank" rel="noopener noreferrer">%s</a>'
                ),
                id,
                value
              )
            }
          ),
          drug_type = reactable::colDef(name = "Type", maxWidth = 130),
          max_phase = reactable::colDef(name = "Max phase", maxWidth = 110),
          disease = reactable::colDef(name = "Indication(s)", minWidth = 160)
        )
      )
    })

    output$content <- renderUI({
      res <- drugs()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see known drugs."))
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
              "Showing %d of %s drugs and clinical candidates.",
              nrow(res$data),
              format(res$count, big.mark = ",")
            )
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    drugs
  })
}
