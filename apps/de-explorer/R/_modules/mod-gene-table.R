# Gene results table Shiny module (classic). Canonical home of the gene-table
# module; full apps (e.g. DE Explorer) reuse this rather than forking a private
# copy.
#
# All arguments after `id` are reactives, so the module composes cleanly into a
# larger app: pass app-level inputs in, get the prepared table and the row
# selection back out. Selecting rows is how the table feeds downstream views
# (heatmap of selected genes, gene detail).

# fill: whether the table is a fill item (TRUE, fills a card in a fill layout) or
# sizes to its content (FALSE, so the card grows and the page scrolls -- avoids a
# card-internal scrollbar on top of the page scroll).
gene_table_ui <- function(id, fill = TRUE) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns("tbl"), fill = fill)
}

# de_results, lfc_threshold, p_threshold, p_col, sig_only: reactives.
# Returns a list of two reactives:
#   $data     -- the displayed (classified, filtered, ordered) results table
#   $selected -- character vector of genes for the currently selected rows
gene_table_server <- function(
  id,
  de_results,
  lfc_threshold,
  p_threshold,
  p_col,
  sig_only
) {
  shiny::moduleServer(id, function(input, output, session) {
    tbl <- shiny::reactive({
      de_results_table(
        de_results(),
        lfc_threshold = lfc_threshold(),
        p_threshold = p_threshold(),
        p_col = p_col(),
        sig_only = sig_only()
      )
    })

    # Round the numeric stat columns for display only; DT keeps the underlying
    # values for correct sorting.
    num_cols <- shiny::reactive({
      intersect(c("logFC", "AveExpr", "t", "pvalue", "padj"), names(tbl()))
    })

    output$tbl <- DT::renderDT(
      {
        shiny::validate(shiny::need(nrow(tbl()) > 0, "No genes to display."))
        dt <- DT::datatable(
          tbl(),
          selection = "multiple",
          rownames = FALSE,
          class = "cell-border stripe hover",
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = list("copy", "csv"),
            pageLength = 15,
            order = list(),
            columnDefs = list(list(className = "dt-right", targets = "_all"))
          )
        )
        if (length(num_cols())) {
          dt <- DT::formatRound(dt, columns = num_cols(), digits = 3)
        }
        dt
      },
      server = TRUE
    )

    selected <- shiny::reactive({
      rows <- input$tbl_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        return(character(0))
      }
      tbl()$gene[rows]
    })

    # The single gene of the most recently clicked row -- a focus gene an app
    # can feed to a detail view, distinct from the multi-row `selected` set.
    clicked <- shiny::reactive({
      r <- input$tbl_row_last_clicked
      if (is.null(r) || r < 1 || r > nrow(tbl())) {
        return(NULL)
      }
      tbl()$gene[r]
    })

    list(data = tbl, selected = selected, clicked = clicked)
  })
}
