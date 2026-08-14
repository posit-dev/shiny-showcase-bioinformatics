# Module: ranking controls -----------------------------------------------------
# Per-source weight sliders, the multi-source coverage bonus, and target list
# size. Returns a reactive list consumed by rank_genes(). Cheap to recompute, so
# tuning these re-ranks instantly without re-querying any source. Annotation
# sources (prioritizers like gnomAD/Pharos) default to a lower weight.

ranking_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("weights")),
    sliderInput(
      ns("coverage_bonus"),
      "Multi-source bonus",
      min = 0,
      max = 1,
      value = 0.5,
      step = 0.1
    ),
    numericInput(
      ns("top_n"),
      "Target list size",
      value = 30,
      min = 1,
      max = 500,
      step = 1
    )
  )
}

ranking_controls_server <- function(id, available = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    srcs <- available %||% list_sources()
    ids <- vapply(srcs, function(s) s$id, character(1))
    labels <- vapply(srcs, function(s) s$label, character(1))
    roles <- vapply(srcs, function(s) s$role %||% "evidence", character(1))
    defaults <- ifelse(roles == "annotation", 0.5, 1)
    names(defaults) <- ids

    output$weights <- renderUI({
      tagList(lapply(seq_along(ids), function(i) {
        lab <- if (roles[i] == "annotation") {
          paste0("Weight: ", labels[i], " (prioritizer)")
        } else {
          paste0("Weight: ", labels[i])
        }
        sliderInput(
          ns(paste0("w_", ids[i])),
          lab,
          min = 0,
          max = 1,
          value = defaults[[i]],
          step = 0.1
        )
      }))
    })

    reactive({
      w <- vapply(
        ids,
        function(sid) {
          v <- input[[paste0("w_", sid)]]
          if (is.null(v)) defaults[[sid]] else v
        },
        numeric(1)
      )
      names(w) <- ids
      list(
        weights = w,
        coverage_bonus = input$coverage_bonus %||% 0.5,
        top_n = input$top_n %||% 30
      )
    })
  })
}
