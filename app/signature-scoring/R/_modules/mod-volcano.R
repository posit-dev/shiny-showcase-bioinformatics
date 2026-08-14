# Volcano plot Shiny module (classic). Canonical home of the volcano module;
# full apps (e.g. DE Explorer) reuse this rather than forking a private copy.
#
# All arguments after `id` are reactives, so the module composes cleanly into a
# larger app: pass app-level inputs in, get the prepared data back out.

volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  # `click` makes points selectable: the server maps the click to the nearest
  # gene and exposes it, so an app can cross-highlight / drive a detail view.
  shiny::plotOutput(ns("plot"), height = "560px", click = ns("click"))
}

# de_results, lfc_threshold, p_threshold, p_col, label_top_n: reactives.
# xlab: optional reactive x-axis title (default the log2-fold-change label), so
# the same module renders a signature-score volcano as well as a DE volcano.
# highlight: optional reactive of gene names to emphasise (ringed + bold label).
# Returns a list of reactives:
#   $data    -- the volcano_data() data frame (for summaries / status counts)
#   $clicked -- the gene under the most recent point click (NULL until a click)
volcano_server <- function(
  id,
  de_results,
  lfc_threshold,
  p_threshold,
  p_col,
  label_top_n,
  xlab = shiny::reactive(expression(log[2] ~ fold ~ change)),
  highlight = shiny::reactive(character(0))
) {
  shiny::moduleServer(id, function(input, output, session) {
    vdata <- shiny::reactive({
      volcano_data(
        de_results(),
        lfc_threshold = lfc_threshold(),
        p_threshold = p_threshold(),
        p_col = p_col(),
        label_top_n = label_top_n()
      )
    })

    output$plot <- shiny::renderPlot(
      {
        plot_volcano(
          vdata(),
          lfc_threshold(),
          p_threshold(),
          xlab = xlab(),
          highlight = highlight()
        )
      },
      res = 96
    )

    # Map a plot click to the nearest gene (by logFC / -log10 p).
    clicked <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$click, {
      np <- shiny::nearPoints(
        vdata(),
        input$click,
        xvar = "logFC",
        yvar = "neg_log10_p",
        maxpoints = 1
      )
      if (nrow(np) > 0) {
        clicked(np$gene[1])
      }
    })

    list(data = vdata, clicked = clicked)
  })
}
