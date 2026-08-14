# STRING protein-protein interaction table. Lists interaction partners and
# evidence scores for the gene as a reactable.

string_ppi_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Protein interactions (STRING)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "200px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
string_ppi_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    partners <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      string_interaction_partners(res$symbol)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_string(res$symbol), "STRING")
    })

    output$table <- reactable::renderReactable({
      res <- partners()
      req(res, isTRUE(res$ok))
      score_col <- function(name) {
        reactable::colDef(
          name = name,
          format = reactable::colFormat(digits = 3),
          align = "right"
        )
      }
      reactable::reactable(
        res$data,
        searchable = TRUE,
        compact = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        columns = list(
          partner = reactable::colDef(name = "Partner", minWidth = 120),
          score = score_col("Combined"),
          experimental = score_col("Experimental"),
          database = score_col("Database"),
          coexpression = score_col("Coexpression"),
          textmining = score_col("Text mining")
        )
      )
    })

    output$content <- renderUI({
      res <- partners()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see interaction partners."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      reactable::reactableOutput(ns("table"))
    })

    # Returned so the parent can surface this card's data to the assistant.
    partners
  })
}
