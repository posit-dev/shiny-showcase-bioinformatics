# Reusable table module: an Alpine-Lake reactable plus a "Columns" chooser.
#
# Every data table in the app goes through this module so they all share one look
# (via tahoe_reactable) AND one interaction: a right-aligned "Columns" dropdown
# of checkboxes that toggles which columns are shown. Column visibility is driven
# server-side (re-render with tahoe_reactable(hidden = ...)) so the user's choice
# survives a data change (e.g. a filter), while row selection is preserved across
# that re-render via `default_selected`.
#
# It optionally supports single/ multiple row selection and returns a small API
# (selected(), set_selected(), visible()) so a caller can drive a master-detail
# layout (see the Drugs tab).

#' The "Columns" chooser as a standalone dropdown, meant to sit in the card
#' header (so it costs no vertical space above the table). Shares the module
#' namespace with tahoe_table_ui()/tahoe_table_server() -- pass the same `id`.
tahoe_table_columns_ui <- function(id, columns_label = "Columns") {
  ns <- NS(id)
  div(
    class = "dropdown",
    tags$button(
      class = "btn btn-sm btn-outline-secondary dropdown-toggle",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `data-bs-auto-close` = "outside",
      `aria-expanded` = "false",
      columns_label
    ),
    div(
      class = "dropdown-menu dropdown-menu-end p-2",
      style = "max-height:320px; overflow-y:auto; min-width:220px;",
      div(
        class = "text-muted small text-uppercase mb-1",
        style = "letter-spacing:.03em;",
        "Show columns"
      ),
      uiOutput(ns("col_menu"))
    )
  )
}

#' The table output. Pair it with tahoe_table_columns_ui(id) in the card header
#' for the column chooser.
tahoe_table_ui <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("table"))
}

#' Table server. `data` is a reactive data frame. `columns` is a colDef list (or
#' a function(df) -> colDef list) for per-column overrides. `hidden` is the set
#' of columns hidden by default. `selection` / `on_click` enable row selection
#' (e.g. "single" / "select"). `default_selected` is an optional function(df) ->
#' integer row index used to re-apply the selection after a data-driven
#' re-render (read under isolate, so toggling columns/selecting a row does not
#' loop). Returns list(selected, set_selected, visible).
tahoe_table_server <- function(
  id,
  data,
  columns = list(),
  page_size = 10,
  hidden = character(0),
  selection = NULL,
  on_click = NULL,
  default_selected = NULL,
  empty_message = "No rows to display",
  ...
) {
  extra_args <- list(...)
  moduleServer(id, function(input, output, session) {
    all_columns <- reactive({
      df <- data()
      if (is.null(df)) character(0) else names(df)
    })

    output$col_menu <- renderUI({
      cols <- all_columns()
      validate(need(length(cols) > 0, ""))
      labels <- janitor::make_clean_names(cols, case = "title")
      choices <- stats::setNames(cols, labels)
      checkboxGroupInput(
        session$ns("cols"),
        label = NULL,
        choices = choices,
        selected = setdiff(cols, hidden)
      )
    })
    # The chooser lives in a collapsed dropdown menu (display:none until opened);
    # without this its output would be suspended and the checkboxes (and the
    # `cols` input that drives visibility) would never render.
    outputOptions(output, "col_menu", suspendWhenHidden = FALSE)

    output$table <- reactable::renderReactable({
      df <- data()
      validate(need(!is.null(df) && nrow(df) > 0, empty_message))
      cols <- if (is.function(columns)) columns(df) else columns
      chosen <- input$cols
      hide <- if (is.null(chosen)) hidden else setdiff(names(df), chosen)
      args <- c(
        list(df, columns = cols, page_size = page_size, hidden = hide),
        extra_args
      )
      if (!is.null(selection)) {
        args$selection <- selection
      }
      if (!is.null(on_click)) {
        args$onClick <- on_click
      }
      if (!is.null(default_selected)) {
        idx <- isolate(default_selected(df))
        if (!is.null(idx)) {
          args$defaultSelected <- idx
        }
      }
      do.call(tahoe_reactable, args)
    })

    list(
      selected = reactive(reactable::getReactableState("table", "selected")),
      set_selected = function(idx) {
        reactable::updateReactable("table", selected = idx)
      },
      visible = reactive(input$cols %||% setdiff(all_columns(), hidden))
    )
  })
}
