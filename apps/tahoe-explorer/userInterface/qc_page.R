# QC tab -- experimental-design guardrails to check before running an analysis.

register_page(
  id = "qc",
  title = "QC",
  order = 20,
  ui = div(
    class = "p-2",
    h3("Design & QC guardrails"),
    p(
      class = "text-muted",
      "Catch underpowered conditions, incomplete dose series, thin controls,",
      "and batch structure before spending compute on the full dataset."
    ),
    qc_ui("qc")
  ),
  server = function() qc_server("qc")
)
