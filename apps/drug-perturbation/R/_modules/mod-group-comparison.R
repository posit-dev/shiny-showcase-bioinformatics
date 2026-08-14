# Group-comparison Shiny module (classic). Canonical home of the group-comparison
# module; full apps (Signature Scoring boxplots, DE Explorer gene detail) reuse
# this rather than forking a private copy.
#
# All arguments after `id` are reactives, so the module composes cleanly: pass a
# per-sample value and the metadata in, get the boxplot out.

group_comparison_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("plot"), height = "460px")
}

# values: reactive per-sample numeric value (named by sample, or aligned to
#   metadata$sample). metadata: reactive data frame. value_label: reactive label
#   for the y-axis (the gene / signature being shown). group_col: reactive
#   metadata column defining the groups.
# Returns the reactive tidy data frame so callers can drive summaries.
group_comparison_server <- function(
  id,
  values,
  metadata,
  value_label = shiny::reactive("value"),
  group_col = shiny::reactive("group")
) {
  shiny::moduleServer(id, function(input, output, session) {
    gdata <- shiny::reactive({
      df <- group_comparison_data(values(), metadata(), group_col = group_col())
      shiny::validate(shiny::need(
        nrow(df) > 0,
        "No values to compare for the current selection."
      ))
      df
    })

    output$plot <- shiny::renderPlot(
      {
        plot_group_comparison(
          gdata(),
          value_label = value_label(),
          test = group_comparison_test(gdata())
        )
      },
      res = 96
    )

    gdata
  })
}
