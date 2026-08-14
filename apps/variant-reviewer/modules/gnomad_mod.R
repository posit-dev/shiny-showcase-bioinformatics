# gnomAD population-frequency card. Shows exome/genome allele frequencies for
# the variant.

gnomad_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Population frequency (gnomAD)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "120px"
    ))
  )
}

# rsid: reactive() -> dbSNP rsID string (or NULL when none is available).
#
# Returns list(data, retry): `data` is the frequency reactive, exactly as
# before; `retry` is this card's retry-bump function, exposed so the gnomAD
# ancestry card -- which renders this same fetch rather than making its own --
# can also retry it from its own header button (see gnomad_ancestry_server()).
gnomad_server <- function(id, rsid) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    frequency <- reactive({
      retry$dep()
      id_value <- rsid()
      if (is_blank(id_value)) {
        return(NULL)
      }
      gnomad_frequency(id_value)
    })

    output$source <- renderUI({
      res <- frequency()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_gnomad_variant(res$variant_id, res$dataset), "gnomAD")
    })

    output$content <- renderUI({
      res <- frequency()
      if (is.null(res)) {
        return(vr_empty(
          "Enter a variant (rsID or HGVS) to see gnomAD frequencies."
        ))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      link <- tags$a(
        href = paste0(
          "https://gnomad.broadinstitute.org/variant/",
          res$variant_id,
          "?dataset=",
          res$dataset
        ),
        target = "_blank",
        rel = "noopener noreferrer",
        res$variant_id
      )
      tagList(
        tags$p(class = "mb-2", tags$strong("Variant: "), link),
        gnomad_part_ui("Exome", res$exome),
        gnomad_part_ui("Genome", res$genome),
        if (is.null(res$exome) && is.null(res$genome)) {
          vr_empty("No allele-frequency data for this variant.")
        }
      )
    })

    # data: so the parent can surface this card's data to the assistant, same
    # as every other module. retry: see the function comment above.
    list(data = frequency, retry = retry$bump)
  })
}

# Render one sample set's allele frequency line, or nothing when absent.
gnomad_part_ui <- function(label, part) {
  if (is.null(part) || is_blank(part$af)) {
    return(NULL)
  }
  tags$p(
    class = "mb-1",
    tags$strong(paste0(label, " AF: ")),
    tags$span(gnomad_fmt_af(part$af)),
    tags$span(
      class = "text-muted",
      sprintf(
        "  (%s / %s alleles)",
        format(part$ac, big.mark = ","),
        format(part$an, big.mark = ",")
      )
    )
  )
}

# Format an allele frequency, using significant digits so tiny values stay
# readable (e.g. 1.37e-06 rather than 0.00).
gnomad_fmt_af <- function(af) {
  if (is_blank(af)) {
    return("—")
  }
  formatC(as.numeric(af), format = "g", digits = 3)
}
