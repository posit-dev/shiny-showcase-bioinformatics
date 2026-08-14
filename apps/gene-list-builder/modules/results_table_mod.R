# Module: ranked results table -------------------------------------------------
# Renders the ranked gene table (renders directly; takes a reactive in).

results_table_ui <- function(id) {
  ns <- NS(id)
  shinycssloaders::withSpinner(DT::dataTableOutput(ns("tbl")))
}

results_table_server <- function(id, ranked_reactive) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT(
      {
        ranked <- ranked_reactive()
        validate(need(
          !is.null(ranked) && nrow(ranked) > 0,
          "Build a gene list to see ranked candidates."
        ))

        score_cols <- grep("^score_", names(ranked), value = TRUE)
        cols <- c(
          "rank",
          "gene_symbol",
          "n_sources",
          "sources",
          score_cols,
          "combined_score",
          "evidence"
        )
        cols <- intersect(cols, names(ranked))
        show <- ranked[, cols, drop = FALSE]

        round_cols <- intersect(c(score_cols, "combined_score"), names(show))
        DT::datatable(
          show,
          rownames = FALSE,
          selection = "none",
          extensions = "Buttons",
          options = list(
            pageLength = 15,
            dom = "Bfrtip",
            buttons = c("copy", "csv")
          )
        ) |>
          DT::formatRound(columns = round_cols, digits = 3)
      },
      server = TRUE
    )
  })
}
