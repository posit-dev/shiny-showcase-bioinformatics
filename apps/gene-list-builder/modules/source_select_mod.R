# Module: data source selection ------------------------------------------------
# Checkbox group built from the source registry; returns selected source ids.

source_select_ui <- function(id) {
  ns <- NS(id)
  checkboxGroupInput(ns("sources"), "Data sources", choices = NULL)
}

source_select_server <- function(id, available = NULL) {
  moduleServer(id, function(input, output, session) {
    srcs <- available %||% list_sources()
    choices <- stats::setNames(
      vapply(srcs, function(s) s$id, character(1)),
      vapply(srcs, function(s) s$label, character(1))
    )
    updateCheckboxGroupInput(
      session,
      "sources",
      choices = choices,
      selected = unname(choices)
    )

    reactive(input$sources)
  })
}
