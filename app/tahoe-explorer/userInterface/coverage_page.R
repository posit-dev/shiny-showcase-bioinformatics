# Coverage tab -- the drug × cell-line coverage matrix for planning analyses.

register_page(
  id = "coverage",
  title = "Coverage",
  order = 30,
  ui = div(
    class = "p-2",
    h3("Coverage matrix"),
    p(
      class = "text-muted",
      "How many cells were profiled for each drug × cell-line combination, and",
      "at which doses -- use it to check whether a planned comparison has enough",
      "cells before pulling the data."
    ),
    coverage_ui("coverage")
  ),
  server = function() coverage_server("coverage")
)
