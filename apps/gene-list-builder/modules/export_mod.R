# Module: export ----------------------------------------------------------------
# Download the final gene list (CSV) and a markdown run report.

export_ui <- function(id) {
  ns <- NS(id)
  # Compact: just the two download links, small + outline, meant to sit in a
  # slim toolbar row (see builder_ui.R) rather than a full card.
  tagList(
    downloadButton(
      ns("csv"),
      "Gene list (CSV)",
      class = "btn-outline-primary btn-sm"
    ),
    downloadButton(
      ns("report"),
      "Report (.md)",
      class = "btn-outline-primary btn-sm"
    )
  )
}

export_server <- function(
  id,
  ranked_reactive,
  curated_reactive,
  disease_reactive,
  status_reactive,
  controls_reactive
) {
  moduleServer(id, function(input, output, session) {
    slug <- function() {
      gsub("[^A-Za-z0-9]+", "_", disease_reactive()$id %||% "disease")
    }

    output$csv <- downloadHandler(
      filename = function() paste0("gene_list_", slug(), ".csv"),
      content = function(file) {
        ranked <- ranked_reactive()
        req(!is.null(ranked), nrow(ranked) > 0)
        # Curation is optional; fall back to the full ranked list if the
        # "Curate with AI" step has not run yet.
        cur <- tryCatch(curated_reactive(), error = function(e) NULL)
        utils::write.csv(
          build_export_csv(ranked, cur, included_only = TRUE),
          file,
          row.names = FALSE
        )
      }
    )

    output$report <- downloadHandler(
      filename = function() paste0("gene_list_report_", slug(), ".md"),
      content = function(file) {
        ranked <- ranked_reactive()
        req(!is.null(ranked), nrow(ranked) > 0)
        cur <- tryCatch(curated_reactive(), error = function(e) NULL)
        md <- build_report_md(
          disease_reactive(),
          status_reactive(),
          controls_reactive(),
          ranked,
          cur,
          ai_used = isTRUE(attr(cur, "ai_used"))
        )
        writeLines(md, file)
      }
    )
  })
}
