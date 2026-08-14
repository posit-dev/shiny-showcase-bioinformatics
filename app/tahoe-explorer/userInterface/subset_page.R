# Subset builder tab -- cross-table selection, coverage preview, filtered
# download, and a copy-paste R/Python analysis recipe.

register_page(
  id = "subset",
  title = "Subset builder",
  order = 10,
  ui = div(
    class = "p-2",
    h3("Subset builder"),
    p(
      class = "text-muted",
      "Pick a slice across drug, cell line, dose, and plate; preview how many",
      "samples and cells it covers; then download the slice and a",
      "copy-paste recipe to extract it from the full Tahoe-100M dataset."
    ),
    subset_builder_ui("subset")
  ),
  server = function() subset_builder_server("subset")
)
