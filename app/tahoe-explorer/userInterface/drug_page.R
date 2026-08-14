# Drug & MOA explorer tab.

register_page(
  id = "drugs",
  title = "Drugs",
  order = 40,
  ui = div(
    class = "p-2",
    h3("Drug & MOA explorer"),
    p(
      class = "text-muted",
      "Filter the drug metadata by mechanism, approval, clinical-trial status,",
      "target, and name; browse the matches with PubChem links; summarize in",
      "charts; and export the current subset."
    ),
    drug_explorer_ui("drugs")
  ),
  server = function() drug_explorer_server("drugs")
)
