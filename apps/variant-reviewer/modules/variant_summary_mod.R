# Variant summary card (MyVariant). Annotates the optional variant from the
# search query.

variant_summary_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Variant", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "120px"
    ))
  )
}

# annotation: reactive() returning the myvariant_annotate() result, or NULL when
# no variant was supplied.
# retry_annotation: the shared `annotation_raw` reactive's retry-bump function
# (see app_server.R). This card has no fetch of its own -- it just renders
# `annotation` directly -- so its refresh button has to retry that instead.
variant_summary_server <- function(id, annotation, retry_annotation) {
  moduleServer(id, function(input, output, session) {
    vr_card_refresh_observer(input, retry_annotation)

    output$source <- renderUI({
      res <- annotation()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_dbsnp(res$rsid), "dbSNP")
    })

    output$content <- renderUI({
      res <- annotation()
      if (is.null(res)) {
        return(vr_empty("Enter a variant (rsID or HGVS) to annotate it."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        vr_field("Variant", res$id),
        vr_field("dbSNP", res$rsid),
        vr_field("Gene", res$gene),
        vr_field("Protein change", res$hgvsp),
        vr_field(
          "CADD (phred)",
          if (is_blank(res$cadd_phred)) NULL else vr_num(res$cadd_phred, 1)
        ),
        vr_field("ClinVar", res$clinvar_significance)
      )
    })
  })
}
