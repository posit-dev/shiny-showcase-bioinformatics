# Cell lines tab -- filterable explorer of the cell-line metadata table.

register_page(
  id = "cell_lines",
  title = "Cell lines",
  order = 50,
  ui = div(
    class = "p-2",
    h3("Cell-line explorer"),
    p(
      class = "text-muted",
      "Filter the Tahoe-100M cell lines by organ, driver gene, and variant",
      "type; inspect them in a table with DepMap links; and export a subset."
    ),
    cell_line_explorer_ui("cell_lines")
  ),
  server = function() cell_line_explorer_server("cell_lines")
)
