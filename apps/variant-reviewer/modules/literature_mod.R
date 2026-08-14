# Literature card. Lists recent publications for the gene (Europe PMC), refined
# by the loaded variant's rsID when there is one, as a reactable with each title
# linked to its Europe PMC article page.

literature_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Literature (Europe PMC)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "200px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
# variant_rsid: reactive() -> the loaded variant's rsID (or NULL); when present
# it refines the search to that variant.
literature_server <- function(id, resolved, variant_rsid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    literature <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      europepmc_search(res$symbol, refine = variant_rsid())
    })

    output$source <- renderUI({
      res <- literature()
      req(res, isTRUE(res$ok))
      vr_source_link(src_europepmc_search(res$query), "Europe PMC")
    })

    output$table <- reactable::renderReactable({
      res <- literature()
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
          id = reactable::colDef(show = FALSE),
          source = reactable::colDef(show = FALSE),
          doi = reactable::colDef(show = FALSE),
          title = reactable::colDef(
            name = "Title",
            minWidth = 240,
            html = TRUE,
            # Link each title to its Europe PMC article page.
            cell = function(value, index) {
              src <- df$source[index]
              aid <- df$id[index]
              if (is.na(src) || is.na(aid) || src == "" || aid == "") {
                return(value)
              }
              sprintf(
                paste0(
                  '<a href="https://europepmc.org/article/%s/%s"',
                  ' target="_blank" rel="noopener noreferrer">%s</a>'
                ),
                src,
                aid,
                value
              )
            }
          ),
          authors = reactable::colDef(name = "Authors", minWidth = 140),
          journal = reactable::colDef(name = "Journal", minWidth = 120),
          year = reactable::colDef(name = "Year", maxWidth = 70),
          cited_by = reactable::colDef(
            name = "Cited by",
            maxWidth = 90,
            align = "right"
          )
        )
      )
    })

    output$content <- renderUI({
      res <- literature()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see recent literature."))
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
              "Showing %d of %s matching publications.",
              nrow(res$data),
              format(res$count, big.mark = ",")
            )
          )
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    literature
  })
}
