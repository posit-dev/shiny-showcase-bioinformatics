# Expression heatmap Shiny module (classic). Canonical home of the heatmap
# module; full apps reuse this rather than forking a private copy.
#
# Generic on the matrix: rows can be genes (DE Explorer), signatures (Signature
# Scoring), or query genes (Drug Perturbation top-hits) -- the module only needs
# a numeric matrix, the rows to show, and the sample metadata. All arguments
# after `id` are reactives, so it composes cleanly into a larger app.

heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("plot"), height = "560px")
}

# matrix: reactive numeric matrix, rows = features (rownames), cols = samples.
# rows: reactive character vector of row names to display.
# metadata: reactive data frame with `sample` + a grouping column (or NULL for
#   no grouping). scale: reactive "row"/"none". cluster_rows: reactive logical.
# group_col: reactive metadata column to order/facet samples by.
# show_samples: reactive logical - draw per-sample column labels (default TRUE;
#   pass FALSE for cohort-scale heatmaps where the labels would collide).
# show_features: reactive logical - draw per-row labels (default TRUE; pass
#   FALSE when there are too many rows to label, e.g. a full query signature).
# Returns the reactive heatmap_data() output so callers can drive summaries.
heatmap_server <- function(
  id,
  matrix,
  rows,
  metadata,
  scale,
  cluster_rows,
  group_col = shiny::reactive("group"),
  show_samples = shiny::reactive(TRUE),
  show_features = shiny::reactive(TRUE)
) {
  shiny::moduleServer(id, function(input, output, session) {
    hd <- shiny::reactive({
      sel <- rows()
      shiny::validate(shiny::need(
        length(sel) > 0,
        "Select one or more rows to draw the heatmap."
      ))
      m <- matrix()
      meta <- metadata()
      gcol <- group_col()

      # Order sample columns by group so conditions sit together; the renderer
      # then facets on the same grouping. Skip when there's no usable grouping.
      if (
        !is.null(meta) && "sample" %in% names(meta) && gcol %in% names(meta)
      ) {
        ord <- order(meta[[gcol]], meta$sample)
        keep <- as.character(meta$sample)[ord]
        keep <- keep[keep %in% colnames(m)]
        m <- m[, keep, drop = FALSE]
      }

      heatmap_data(
        m,
        genes = sel,
        scale = scale(),
        cluster_rows = cluster_rows(),
        cluster_cols = FALSE
      )
    })

    output$plot <- shiny::renderPlot(
      {
        plot_heatmap(
          hd(),
          metadata = metadata(),
          group_col = group_col(),
          show_samples = show_samples(),
          show_features = show_features()
        )
      },
      res = 96
    )

    hd
  })
}
