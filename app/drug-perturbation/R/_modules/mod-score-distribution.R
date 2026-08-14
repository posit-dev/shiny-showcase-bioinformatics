# Score distribution Shiny module (classic). Canonical home of the
# score-distribution module; the Drug Perturbation app reuses this rather than
# forking a private copy.
#
# All arguments after `id` are reactives, so the module composes cleanly: pass
# the connectivity scores in, get the distribution histogram out.

score_distribution_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("plot"), height = "460px")
}

# scores: reactive data.frame(pert_id, score) (or numeric vector).
# mimic_threshold: reactive |score| cutoff drawn as dashed lines.
# highlight: reactive single score to mark (a selected hit), or NULL.
score_distribution_server <- function(
  id,
  scores,
  mimic_threshold,
  highlight = shiny::reactive(NULL)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ddata <- shiny::reactive({
      score_distribution_data(scores(), mimic_threshold = mimic_threshold())
    })

    output$plot <- shiny::renderPlot(
      {
        plot_score_distribution(
          ddata(),
          mimic_threshold = mimic_threshold(),
          highlight = highlight()
        )
      },
      res = 96
    )

    ddata
  })
}
