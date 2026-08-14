# GTEx tissue expression card. Plots median TPM across tissues for the gene.

gtex_expression_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Tissue expression (GTEx)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "300px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result (uses the gene symbol).
gtex_expression_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    expression <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      gtex_median_expression(res$symbol)
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gtex(res$symbol), "GTEx")
    })

    output$plot <- renderPlot({
      res <- expression()
      req(res, isTRUE(res$ok))
      df <- res$data
      df$tissue <- factor(df$tissue, levels = df$tissue[order(df$median_tpm)])
      ggplot2::ggplot(df, ggplot2::aes(x = median_tpm, y = tissue)) +
        ggplot2::geom_col(fill = vr_colors$primary) +
        ggplot2::labs(x = "Median TPM", y = NULL) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 14)
        )
    })

    output$content <- renderUI({
      res <- expression()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see tissue expression."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      # Scale height to the number of tissues so labels stay legible.
      height <- max(320, nrow(res$data) * 22)
      plotOutput(ns("plot"), height = paste0(height, "px"))
    })

    # Returned so the parent can surface this card's data to the assistant.
    expression
  })
}
