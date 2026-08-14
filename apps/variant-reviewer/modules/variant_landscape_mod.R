# Protein "lollipop" card. Places every ClinVar variant for the gene at its
# protein residue (coloured by clinical significance, height = number of
# variants there), over a track of the protein's domains, with the queried
# variant marked, so you can see at a glance whether it sits in a mutational
# hotspot or a functional domain.

# Coarse-significance colours and their severity order (most severe first, used
# to pick a residue's dominant colour when several categories share a position).
.vl_palette <- c(
  "Pathogenic / likely" = "#b2182b",
  "Conflicting" = "#e08214",
  "Uncertain" = "#7a7468",
  "Benign / likely" = "#2166ac",
  "Other" = "#bdbdbd"
)
.vl_severity <- stats::setNames(seq_along(.vl_palette), names(.vl_palette))

# UniProt feature types drawn as spans in the domain track (point sites are
# excluded, since they add nothing to a whole-protein overview).
.vl_domain_types <- c(
  "DOMAIN",
  "REGION",
  "REPEAT",
  "ZN_FING",
  "DNA_BIND",
  "CA_BIND",
  "NP_BIND"
)

variant_landscape_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Variant landscape (ClinVar)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "320px"
    ))
  )
}

# resolved:   reactive() -> mygene_resolve() (gene symbol + UniProt accession)
# search:     reactive() -> list(gene, variant)
# annotation: reactive() -> myvariant_annotate() (for the queried residue)
variant_landscape_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    # Aggregate ClinVar variants to one row per residue (count + dominant
    # significance), and assemble the domain track and queried-residue marker.
    landscape <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok) || is_blank(res$symbol)) {
        return(NULL)
      }
      cv <- gnomad_clinvar_variants(res$symbol)
      if (!isTRUE(cv$ok)) {
        return(cv)
      }
      v <- cv$variants
      v$rank <- .vl_severity[v$category]
      idx <- split(seq_len(nrow(v)), v$residue)
      agg <- do.call(
        rbind,
        lapply(idx, function(rows) {
          data.frame(
            residue = v$residue[rows[[1]]],
            n = length(rows),
            category = v$category[rows[[which.min(v$rank[rows])]]],
            stringsAsFactors = FALSE
          )
        })
      )
      agg$category <- factor(agg$category, levels = names(.vl_palette))
      rownames(agg) <- NULL

      # Domain track from the UniProt features (cached from the domains card).
      domains <- NULL
      feats <- proteins_features(res$uniprot)
      if (isTRUE(feats$ok) && nrow(feats$features) > 0) {
        d <- feats$features
        d <- d[
          d$type %in%
            .vl_domain_types &
            !is.na(d$begin) &
            !is.na(d$end) &
            d$end > d$begin,
          ,
          drop = FALSE
        ]
        if (nrow(d) > 0) {
          domains <- d
        }
      }

      query <- search()
      queried <- protein_resolve_position(query$variant, annotation())

      list(
        ok = TRUE,
        variants = agg,
        domains = domains,
        queried = queried,
        queried_label = if (!is.null(query) && !is_blank(query$variant)) {
          query$variant
        } else {
          NULL
        },
        xmax = max(
          agg$residue,
          domains$end,
          queried,
          na.rm = TRUE
        )
      )
    })

    output$source <- renderUI({
      res <- resolved()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gnomad_gene(res$ensembl_gene), "gnomAD")
    })

    output$plot <- renderPlot({
      res <- landscape()
      req(res, isTRUE(res$ok))
      agg <- res$variants
      max_n <- max(agg$n)
      band <- max(1, max_n * 0.16) # domain-track height, below the baseline

      p <- ggplot2::ggplot()
      # Domain track (below zero), labelled.
      if (!is.null(res$domains)) {
        d <- res$domains
        d$mid <- (d$begin + d$end) / 2
        p <- p +
          ggplot2::geom_rect(
            data = d,
            ggplot2::aes(
              xmin = begin,
              xmax = end,
              ymin = -band,
              ymax = -band * 0.2
            ),
            fill = vr_colors$stone,
            colour = "grey30",
            alpha = 0.55
          ) +
          ggplot2::geom_text(
            data = d,
            ggplot2::aes(x = mid, y = -band * 0.6, label = description),
            size = 3.4,
            colour = "grey20"
          )
      }
      # Queried variant marker.
      if (!is.null(res$queried) && !is.na(res$queried)) {
        p <- p +
          ggplot2::geom_vline(
            xintercept = res$queried,
            linetype = "dashed",
            colour = vr_colors$accent,
            linewidth = 0.7
          )
        if (!is.null(res$queried_label)) {
          p <- p +
            ggplot2::annotate(
              "text",
              x = res$queried,
              y = max_n * 1.08,
              label = res$queried_label,
              colour = vr_colors$accent,
              fontface = "bold",
              size = 4.2,
              hjust = 0.5
            )
        }
      }
      # Lollipops: stem + head per residue.
      p +
        ggplot2::geom_segment(
          data = agg,
          ggplot2::aes(x = residue, xend = residue, y = 0, yend = n),
          colour = "grey75",
          linewidth = 0.4
        ) +
        ggplot2::geom_point(
          data = agg,
          ggplot2::aes(x = residue, y = n, colour = category),
          size = 2.6
        ) +
        ggplot2::geom_hline(
          yintercept = 0,
          colour = "grey50",
          linewidth = 0.4
        ) +
        ggplot2::scale_colour_manual(
          values = .vl_palette,
          drop = TRUE,
          name = NULL
        ) +
        ggplot2::scale_y_continuous(
          limits = c(-band, max_n * 1.15),
          breaks = scales::breaks_pretty(),
          labels = function(b) ifelse(b < 0, "", as.character(b))
        ) +
        ggplot2::labs(x = "Protein residue", y = "ClinVar variants") +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          legend.position = "top",
          axis.text = ggplot2::element_text(size = 12),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    output$content <- renderUI({
      res <- landscape()
      if (is.null(res)) {
        return(vr_empty("Search for a gene to see its variant landscape."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        plotOutput(ns("plot"), height = "340px"),
        tags$p(
          class = "text-muted small mb-0 mt-1",
          sprintf(
            paste(
              "%d ClinVar variants placed by protein residue; height =",
              "variants at that residue. Domains shown below the axis."
            ),
            sum(res$variants$n)
          )
        )
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    landscape
  })
}
