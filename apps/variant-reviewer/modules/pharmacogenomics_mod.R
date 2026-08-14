# Pharmacogenomics card. Lists variant/genotype -> drug-response annotations for
# the gene (Open Targets), with an evidence level and the full genotype note in
# an expandable row. When a variant is loaded, its matching annotations are
# sorted to the top and called out. Most genes have none, so the empty state is
# the common case for a well-behaved gene.

pharmacogenomics_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Pharmacogenomics (Open Targets)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "200px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the Ensembl gene ID).
# variant_rsid: reactive() -> the loaded variant's rsID (or NULL), used only to
# highlight matching rows; the fetch itself is gene-level.
pharmacogenomics_server <- function(id, resolved, variant_rsid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    pgx <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      opentargets_pgx(res$ensembl_gene)
    })

    # Rows for the loaded variant (case-insensitive rsID match), or none.
    matches <- function(df) {
      rsid <- variant_rsid()
      if (is_blank(rsid)) {
        return(rep(FALSE, nrow(df)))
      }
      !is.na(df$rsid) & tolower(df$rsid) == tolower(rsid)
    }

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_opentargets_pgx(res$ensembl_gene), "Open Targets")
    })

    output$table <- reactable::renderReactable({
      res <- pgx()
      req(res, isTRUE(res$ok))
      df <- res$data
      # Surface the loaded variant's annotations first.
      hit <- matches(df)
      df <- df[order(!hit), , drop = FALSE]
      reactable::reactable(
        df,
        searchable = TRUE,
        compact = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        # The full genotype annotation is long, so it lives in an expandable row.
        details = function(index) {
          note <- df$genotype[index]
          if (is_blank(note)) {
            return(NULL)
          }
          htmltools::div(class = "p-2 small text-muted", note)
        },
        columns = list(
          genotype = reactable::colDef(show = FALSE),
          rsid = reactable::colDef(
            name = "Variant",
            maxWidth = 130,
            html = TRUE,
            cell = function(value) {
              if (is.na(value) || value == "") {
                return("—")
              }
              sprintf(
                '<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>',
                src_dbsnp(value),
                value
              )
            }
          ),
          drug = reactable::colDef(name = "Drug(s)", minWidth = 130),
          phenotype = reactable::colDef(name = "Effect", minWidth = 180),
          evidence = reactable::colDef(name = "Evidence", maxWidth = 100)
        )
      )
    })

    output$content <- renderUI({
      res <- pgx()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see pharmacogenomics."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      n_hit <- sum(matches(res$data))
      rsid <- variant_rsid()
      tagList(
        reactable::reactableOutput(ns("table")),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          if (!is_blank(rsid) && n_hit > 0) {
            sprintf(
              "%d of %d annotation(s) match the loaded variant %s.",
              n_hit,
              nrow(res$data),
              rsid
            )
          } else {
            sprintf("%d pharmacogenomics annotation(s).", nrow(res$data))
          }
        )
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    pgx
  })
}
