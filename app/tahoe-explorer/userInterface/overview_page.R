# Overview tab -- registered first in the navbar.

register_page(
  id = "overview",
  title = "Overview",
  order = 0,
  ui = div(
    class = "p-2",
    h3("Tahoe-100M metadata explorer"),
    p(
      class = "text-muted",
      "Explore drug, cell-line, sample, and cell-level metadata; summarize",
      "in quick charts; and pull a subset for downstream analysis."
    ),
    overview_ui("overview")
  ),
  server = function() overview_server("overview")
)
