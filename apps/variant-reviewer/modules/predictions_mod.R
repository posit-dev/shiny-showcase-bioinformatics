# In-silico predictions card. Pathogenicity/impact scores for the variant from
# dbNSFP (+ CADD) via MyVariant: REVEL, AlphaMissense, CADD, PolyPhen-2, SIFT,
# MetaLR, MetaSVM.

predictions_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("In-silico predictions", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "160px"
    ))
  )
}

# search: reactive() -> list(gene, variant) (uses the variant string).
predictions_server <- function(id, search) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    predictions <- reactive({
      retry$dep()
      query <- search()
      if (is.null(query) || is_blank(query$variant)) {
        return(NULL)
      }
      myvariant_predictions(query$variant)
    })

    output$source <- renderUI({
      res <- predictions()
      req(!is.null(res), isTRUE(res$ok))
      variant <- trimws((search())$variant %||% "")
      rsid <- if (grepl("^rs[0-9]+$", variant, ignore.case = TRUE)) {
        tolower(variant)
      }
      vr_source_link(src_dbsnp(rsid), "dbSNP")
    })

    output$content <- renderUI({
      res <- predictions()
      if (is.null(res)) {
        return(vr_empty(
          "Enter a variant (rsID or HGVS) to see in-silico predictions."
        ))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      rows <- lapply(res$predictions, function(p) {
        tags$tr(
          tags$td(tags$strong(p$name)),
          tags$td(
            class = "text-end",
            if (is.na(p$score)) "—" else vr_num(p$score, 3)
          ),
          tags$td(class = "text-muted", if (is.na(p$call)) "" else p$call)
        )
      })
      tagList(
        tags$table(
          class = "table table-sm align-middle mb-2",
          tags$thead(
            tags$tr(
              tags$th("Predictor"),
              tags$th(class = "text-end", "Score"),
              tags$th("Call")
            )
          ),
          tags$tbody(rows)
        ),
        tags$p(
          class = "text-muted small mb-0",
          "Scores from dbNSFP (via MyVariant). Thresholds differ by tool; use",
          " alongside other evidence, not on their own."
        )
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    predictions
  })
}
