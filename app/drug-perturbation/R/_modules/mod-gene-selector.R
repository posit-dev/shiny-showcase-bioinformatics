# Gene selector Shiny module (classic). Canonical home of the gene-selector
# module; full apps reuse it for picking genes (DE heatmap), building a query
# signature (Drug Perturbation), or any searchable multi-select of genes.
#
# A searchable, server-side multi-select plus an optional paste box (paste a
# list, click Apply, unknown genes are reported). Returns the reactive selection.

gene_selector_ui <- function(id, label = "Genes") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("genes"),
      label,
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "type to search…")
    ),
    shiny::tags$details(
      shiny::tags$summary("Paste a list"),
      shiny::textAreaInput(
        ns("paste"),
        label = NULL,
        rows = 3,
        placeholder = "GENE1, GENE2, GENE3…"
      ),
      shiny::actionButton(ns("apply"), "Apply", class = "btn-sm"),
      shiny::div(shiny::textOutput(ns("msg")), class = "small text-muted mt-1")
    )
  )
}

# choices: reactive character vector of selectable genes.
# selected: reactive initial selection (default none).
# server: server-side selectize (TRUE for large gene lists).
# Returns a reactive of the currently selected genes.
gene_selector_server <- function(
  id,
  choices,
  selected = shiny::reactive(character(0)),
  server = TRUE
) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateSelectizeInput(
        session,
        "genes",
        choices = choices(),
        selected = selected(),
        server = server
      )
    })

    # Paste -> resolve against the available genes, set the selection, and
    # report anything that didn't match.
    shiny::observeEvent(input$apply, {
      parsed <- parse_gene_input(input$paste, choices())
      shiny::updateSelectizeInput(
        session,
        "genes",
        choices = choices(),
        selected = parsed$found,
        server = server
      )
      output$msg <- shiny::renderText({
        if (length(parsed$unknown) == 0) {
          sprintf("Matched %d gene(s).", length(parsed$found))
        } else {
          sprintf(
            "Matched %d; %d not found: %s",
            length(parsed$found),
            length(parsed$unknown),
            toString(utils::head(parsed$unknown, 5))
          )
        }
      })
    })

    shiny::reactive(input$genes)
  })
}
