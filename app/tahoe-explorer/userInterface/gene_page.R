# Genes tab -- searchable lookup of the measured features (gene metadata).

register_page(
  id = "genes",
  title = "Genes",
  # Sits just before About (order 70), after Samples & cells (order 60).
  order = 65,
  ui = div(
    class = "p-2",
    h3("Gene explorer"),
    p(
      class = "text-muted",
      "Search the measured features and check whether specific genes were",
      "quantified before planning a targeted reanalysis."
    ),
    gene_explorer_ui("genes")
  ),
  server = function() gene_explorer_server("genes")
)
