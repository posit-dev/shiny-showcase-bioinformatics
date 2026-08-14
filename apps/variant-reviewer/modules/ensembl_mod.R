# Ensembl VEP card. Shows the most severe consequence plus a table of
# protein-coding transcript consequences for the variant.

ensembl_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Variant consequences (Ensembl VEP)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "160px"
    ))
  )
}

# rsid: reactive() -> dbSNP rsID string (or NULL when none is available).
ensembl_server <- function(id, rsid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    vep <- reactive({
      retry$dep()
      id_value <- rsid()
      if (is_blank(id_value)) {
        return(NULL)
      }
      ensembl_vep(id_value)
    })

    output$source <- renderUI({
      id_value <- rsid()
      req(!is_blank(id_value))
      vr_source_link(src_ensembl_variant(id_value), "Ensembl")
    })

    output$table <- reactable::renderReactable({
      res <- vep()
      req(res, isTRUE(res$ok), !is.null(res$data))
      reactable::reactable(
        res$data,
        searchable = TRUE,
        compact = TRUE,
        highlight = TRUE,
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        columns = list(
          gene = reactable::colDef(name = "Gene", maxWidth = 90),
          transcript = reactable::colDef(name = "Transcript", minWidth = 130),
          consequence = reactable::colDef(name = "Consequence", minWidth = 150),
          impact = reactable::colDef(name = "Impact", maxWidth = 90),
          sift = reactable::colDef(name = "SIFT"),
          polyphen = reactable::colDef(name = "PolyPhen")
        )
      )
    })

    output$content <- renderUI({
      res <- vep()
      if (is.null(res)) {
        return(vr_empty(
          "Enter a variant (rsID or HGVS) to see VEP consequences."
        ))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        tags$p(
          class = "mb-2",
          tags$strong("Most severe consequence: "),
          tags$span(class = "fw-semibold", gsub("_", " ", res$most_severe)),
          if (!is_blank(res$assembly)) {
            tags$span(class = "text-muted", paste0("  (", res$assembly, ")"))
          }
        ),
        if (is.null(res$data)) {
          vr_empty("No protein-coding transcript consequences.")
        } else {
          reactable::reactableOutput(ns("table"))
        }
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    vep
  })
}
