# ClinVar clinical-significance card. Shows the germline classification,
# review status, and associated conditions for the variant.

clinvar_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Clinical significance (ClinVar)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "120px"
    ))
  )
}

# rsid: reactive() -> dbSNP rsID string (or NULL when none is available).
clinvar_server <- function(id, rsid) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    classification <- reactive({
      retry$dep()
      id_value <- rsid()
      if (is_blank(id_value)) {
        return(NULL)
      }
      clinvar_classification(id_value)
    })

    output$source <- renderUI({
      res <- classification()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_clinvar_variation(res$uid), "ClinVar")
    })

    output$content <- renderUI({
      res <- classification()
      if (is.null(res)) {
        return(vr_empty("Enter a variant (rsID or HGVS) to see ClinVar data."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      link <- if (!is_blank(res$uid)) {
        tags$a(
          href = paste0(
            "https://www.ncbi.nlm.nih.gov/clinvar/variation/",
            res$uid,
            "/"
          ),
          target = "_blank",
          rel = "noopener noreferrer",
          res$accession
        )
      } else {
        res$accession
      }
      tagList(
        if (!is_blank(res$significance)) {
          tags$p(
            class = "mb-2",
            tags$strong("Significance: "),
            tags$span(class = "fw-semibold", res$significance)
          )
        },
        vr_field("Review status", res$review_status),
        vr_field("Condition(s)", res$conditions),
        vr_field("Last evaluated", res$last_evaluated),
        vr_field("Variant", res$title),
        tags$p(class = "mb-0", tags$strong("Accession: "), link)
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    classification
  })
}
