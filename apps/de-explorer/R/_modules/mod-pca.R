# Sample PCA Shiny module (classic). Canonical home of the pca-explorer module;
# full apps (e.g. DE Explorer, Signature Scoring) reuse this rather than forking
# a private copy.
#
# All arguments after `id` are reactives, so the module composes cleanly into a
# larger app: pass the dataset and the display choices in, get the rendered PCA
# (and its variance summary) out.

pca_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("plot"), height = "560px")
}

# dataset: reactive de_dataset (expression + metadata).
# colour_by: reactive metadata column name to colour points by.
# n_top: reactive number of most-variable genes to use (NULL/Inf = all).
# Returns the reactive pca_data() output so callers can drive variance summaries.
pca_server <- function(id, dataset, colour_by, n_top) {
  shiny::moduleServer(id, function(input, output, session) {
    pca <- shiny::reactive({
      expr <- de_expression_matrix(dataset())
      pca_data(
        expr,
        metadata = dataset()$metadata,
        n_top = n_top()
      )
    })

    output$plot <- shiny::renderPlot(
      {
        plot_pca(pca(), colour_by = colour_by())
      },
      res = 96
    )

    pca
  })
}
