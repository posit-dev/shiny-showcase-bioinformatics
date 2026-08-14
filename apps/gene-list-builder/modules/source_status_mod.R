# Module: per-source status -----------------------------------------------------
# Renders a value box per source (status, gene count, latency).

# biohttp::STATUS_LEVELS -> bslib value-box theme. The distinction that matters
# is "no_data" (grey: the source answered, and had nothing) versus "error" or
# "timeout" (red/amber: the source never answered, so the gene list is
# incomplete and the user needs to know). Before biohttp both looked identical.
GLB_STATUS_THEME <- c(
  ok = "success",
  no_data = "secondary",
  stale = "warning",
  rate_limited = "warning",
  timeout = "warning",
  skipped = "secondary",
  error = "danger"
)

# Short human label for each status, shown under the count.
GLB_STATUS_LABEL <- c(
  ok = "ok",
  no_data = "no genes found",
  stale = "served stale",
  rate_limited = "rate limited",
  timeout = "timed out",
  skipped = "skipped",
  error = "failed"
)

source_status_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("boxes"))
}

source_status_server <- function(id, status_reactive) {
  moduleServer(id, function(input, output, session) {
    output$boxes <- renderUI({
      status <- status_reactive()
      validate(need(
        !is.null(status) && nrow(status) > 0,
        "Build a gene list to query sources."
      ))

      boxes <- lapply(seq_len(nrow(status)), function(i) {
        # Fall back to the derived `ok` for any status tibble that predates the
        # status column (e.g. a gene table restored from an old cache file).
        st <- if ("status" %in% names(status)) {
          status$status[i]
        } else if (isTRUE(status$ok[i])) {
          "ok"
        } else {
          "error"
        }
        bslib::value_box(
          title = status$label[i],
          value = paste0(status$n_genes[i], " genes"),
          theme = unname(GLB_STATUS_THEME[st] %||% "danger"),
          p(unname(GLB_STATUS_LABEL[st] %||% st)),
          p(status$message[i]),
          p(paste0(status$latency_ms[i], " ms"))
        )
      })

      do.call(
        bslib::layout_column_wrap,
        c(list(width = "220px"), boxes)
      )
    })
  })
}
