# Connectivity table Shiny module (classic). Canonical home of the
# connectivity-table module; the Drug Perturbation app reuses this rather than
# forking a private copy.
#
# All arguments after `id` are reactives, so the module composes cleanly: pass
# the connectivity scores + perturbation metadata in, get the ranked table and
# the row selection (a chosen perturbation, for the drug-detail view) back out.

# Direction colours, matching the gallery palette: reverse = blue, mimic = orange.
connectivity_direction_colours <- c(
  Reverser = "#447099",
  Neutral = "#6C757D",
  Mimic = "#EE6331"
)

connectivity_table_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns("tbl"))
}

# scores: reactive data.frame(pert_id, score). pert_meta: reactive metadata.
# mimic_threshold: reactive |score| cutoff for the mimic/reverser call.
# Returns a list of two reactives:
#   $data     -- the displayed (joined, classified, ordered) table
#   $selected -- the pert_id of the selected row (length-0 if none)
connectivity_table_server <- function(id, scores, pert_meta, mimic_threshold) {
  shiny::moduleServer(id, function(input, output, session) {
    tbl <- shiny::reactive({
      connectivity_table(
        scores(),
        pert_meta(),
        mimic_threshold = mimic_threshold()
      )
    })

    output$tbl <- DT::renderDT(
      {
        shiny::validate(shiny::need(
          nrow(tbl()) > 0,
          "No perturbations to display."
        ))
        dt <- DT::datatable(
          tbl(),
          selection = "single",
          rownames = FALSE,
          class = "cell-border stripe hover",
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = list("copy", "csv"),
            pageLength = 15,
            order = list()
          )
        )
        dt <- DT::formatRound(dt, "score", digits = 3)
        DT::formatStyle(
          dt,
          "direction",
          color = DT::styleEqual(
            names(connectivity_direction_colours),
            unname(connectivity_direction_colours)
          ),
          fontWeight = "bold"
        )
      },
      server = TRUE
    )

    selected <- shiny::reactive({
      rows <- input$tbl_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        return(character(0))
      }
      tbl()$pert_id[rows]
    })

    list(data = tbl, selected = selected)
  })
}
