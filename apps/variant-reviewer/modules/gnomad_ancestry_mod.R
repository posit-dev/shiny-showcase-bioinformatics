# gnomAD ancestry card. Breaks the variant's allele frequency down by genetic-
# ancestry group (bar chart), so ancestry-specific or founder effects are
# visible. Reuses the shared gnomAD result (populations are fetched alongside
# the overall frequencies), so it adds no extra network call.

gnomad_ancestry_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Ancestry frequency (gnomAD)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "260px"
    ))
  )
}

# gnomad_data: reactive() -> gnomad_frequency() result (carries $populations).
# retry_gnomad: the gnomAD card's own retry-bump function (see gnomad_server()
# in gnomad_mod.R). This card has no fetch of its own -- it renders the same
# result the gnomAD card fetched -- so its refresh button has to retry that.
gnomad_ancestry_server <- function(id, gnomad_data, retry_gnomad) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vr_card_refresh_observer(input, retry_gnomad)

    output$source <- renderUI({
      res <- gnomad_data()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gnomad_variant(res$variant_id, res$dataset), "gnomAD")
    })

    # TRUE when the variant is present in at least one ancestry group.
    observed <- function(res) {
      pops <- res$populations
      !is.null(pops) && any(!is.na(pops$af) & pops$af > 0)
    }

    output$plot <- renderPlot({
      res <- gnomad_data()
      req(res, isTRUE(res$ok), observed(res))
      df <- res$populations[res$populations$af > 0, , drop = FALSE]
      df$label <- factor(df$label, levels = df$label[order(df$af)])
      ggplot2::ggplot(df, ggplot2::aes(x = af, y = label)) +
        ggplot2::geom_col(fill = vr_colors$accent, width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = gnomad_fmt_af(af)),
          hjust = -0.15,
          size = 4.2
        ) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.2))
        ) +
        ggplot2::labs(x = "Allele frequency (exome + genome)", y = NULL) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 13),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    output$content <- renderUI({
      res <- gnomad_data()
      if (is.null(res)) {
        return(vr_empty(
          "Enter a variant (rsID or HGVS) to see the ancestry breakdown."
        ))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      if (!observed(res)) {
        return(vr_empty(paste(
          "This variant is not observed in gnomAD ancestry samples",
          "(allele frequency zero across all groups)."
        )))
      }
      n <- sum(res$populations$af > 0)
      plotOutput(ns("plot"), height = paste0(n * 30 + 70, "px"))
    })

    # No separate data reactive: the shared gnomAD result already flows to the
    # assistant via the gnomAD card.
    invisible(NULL)
  })
}
