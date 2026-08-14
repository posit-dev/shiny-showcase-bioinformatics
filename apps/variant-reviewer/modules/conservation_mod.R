# Evolutionary conservation card. Plots the variant position's conservation
# metrics (phyloP, phastCons, GERP++, SiPhy) from dbNSFP via MyVariant, as
# ranked bars, where a higher rank means a more conserved, less tolerant position.

conservation_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Conservation", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "220px"
    ))
  )
}

# rsid: reactive() -> dbSNP rsID string (or NULL when none is available).
conservation_server <- function(id, rsid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    conservation <- reactive({
      retry$dep()
      id_value <- rsid()
      if (is_blank(id_value)) {
        return(NULL)
      }
      myvariant_conservation(id_value)
    })

    output$source <- renderUI({
      id_value <- rsid()
      req(!is_blank(id_value))
      vr_source_link(src_dbsnp(id_value), "dbSNP")
    })

    output$plot <- renderPlot({
      res <- conservation()
      req(res, isTRUE(res$ok))
      df <- res$metrics
      df$metric <- factor(df$metric, levels = rev(df$metric))
      df$lab <- ifelse(
        is.na(df$score),
        "",
        formatC(df$score, format = "g", digits = 3)
      )
      ggplot2::ggplot(df, ggplot2::aes(x = rankscore, y = metric)) +
        ggplot2::geom_col(fill = vr_colors$primary, width = 0.65) +
        ggplot2::geom_text(
          ggplot2::aes(label = lab),
          hjust = -0.15,
          size = 4.5
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, 1.12),
          breaks = c(0, 0.5, 1)
        ) +
        ggplot2::labs(
          x = "Conservation rank (0–1; higher = more conserved)",
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 13),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    output$content <- renderUI({
      res <- conservation()
      if (is.null(res)) {
        return(vr_empty(
          "Enter a variant (rsID or HGVS) to see conservation scores."
        ))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        plotOutput(
          ns("plot"),
          height = paste0(nrow(res$metrics) * 46 + 70, "px")
        ),
        tags$p(
          class = "text-muted small mb-0 mt-1",
          "Bars show the dbNSFP rank (0–1); the raw score is labelled.",
          " Conserved positions support a deleterious effect (ACMG PP3)."
        )
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    conservation
  })
}
