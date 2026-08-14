# Gene model card. Draws the canonical transcript's exons as a schematic map
# (equal-width boxes in transcription 5'->3' order, not to scale, so every exon
# is visible) and highlights the exon the variant falls in. Exon coordinates
# come from Ensembl; the variant's genomic position is reused from gnomAD.

gene_model_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Gene model (Ensembl)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "180px"
    ))
  )
}

# resolved:    reactive() -> mygene_resolve() (uses the Ensembl gene id)
# gnomad_data: reactive() -> gnomad_frequency() result. Its variant_id
#              ("chrom-pos-ref-alt", GRCh38) places the variant on the model,
#              a more reliable source than VEP, which can time out.
gene_model_server <- function(id, resolved, gnomad_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    model <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok) || is_blank(res$ensembl_gene)) {
        return(NULL)
      }
      ensembl_gene_model(res$ensembl_gene)
    })

    # The variant's genomic position, only when it lies on the gene's contig.
    variant_pos <- reactive({
      m <- model()
      gd <- gnomad_data()
      if (is.null(m) || !isTRUE(m$ok) || is.null(gd) || !isTRUE(gd$ok)) {
        return(NA_real_)
      }
      if (is_blank(gd$variant_id)) {
        return(NA_real_)
      }
      parts <- strsplit(as.character(gd$variant_id), "-", fixed = TRUE)[[1]]
      if (length(parts) < 2) {
        return(NA_real_)
      }
      if (!identical(parts[[1]], as.character(m$region))) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(parts[[2]]))
    })

    hit_exon <- reactive({
      m <- model()
      pos <- variant_pos()
      if (is.null(m) || !isTRUE(m$ok) || is.na(pos)) {
        return(NA_integer_)
      }
      ex <- m$exons
      idx <- which(ex$start <= pos & ex$end >= pos)
      if (length(idx) == 0) NA_integer_ else ex$number[idx[[1]]]
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_ensembl_gene(res$ensembl_gene), "Ensembl")
    })

    output$plot <- renderPlot({
      m <- model()
      req(m, isTRUE(m$ok))
      ex <- m$exons
      ex$hit <- !is.na(hit_exon()) & ex$number == hit_exon()
      ggplot2::ggplot(ex) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = min(number) - 0.5,
            xend = max(number) + 0.5,
            y = 0,
            yend = 0
          ),
          colour = "grey60",
          linewidth = 0.6
        ) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = number - 0.42,
            xmax = number + 0.42,
            ymin = -0.5,
            ymax = 0.5,
            fill = hit
          ),
          colour = "grey30"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(x = number, y = 0, label = number),
          size = 3.1,
          colour = "white"
        ) +
        ggplot2::scale_fill_manual(
          values = c("FALSE" = vr_colors$primary, "TRUE" = vr_colors$accent),
          guide = "none"
        ) +
        ggplot2::scale_y_continuous(limits = c(-1.2, 1.2)) +
        ggplot2::labs(x = "Exon (5′→3′, not to scale)", y = NULL) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(size = 12),
          panel.grid = ggplot2::element_blank()
        )
    })

    output$content <- renderUI({
      m <- model()
      if (is.null(m)) {
        return(vr_empty("Search for a gene to see its exon model."))
      }
      if (!isTRUE(m$ok)) {
        return(vr_error(m$error))
      }
      strand <- if (isTRUE(m$strand < 0)) "− strand" else "+ strand"
      exon_note <- if (!is.na(hit_exon())) {
        sprintf(
          "The variant falls in exon %d of %d.",
          hit_exon(),
          nrow(m$exons)
        )
      } else {
        sprintf("%d exons; no variant positioned on the gene.", nrow(m$exons))
      }
      tagList(
        plotOutput(ns("plot"), height = "150px"),
        tags$p(
          class = "text-muted small mb-0 mt-1",
          sprintf(
            "Canonical transcript %s (chr%s, %s). ",
            m$transcript,
            m$region,
            strand
          ),
          exon_note
        )
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    model
  })
}
