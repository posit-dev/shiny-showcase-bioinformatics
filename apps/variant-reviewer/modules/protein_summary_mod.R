# Protein summary card (ProtVar). Shows functional context and known variants
# at the residue, given the gene's UniProt accession + a protein position.

protein_summary_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("Protein (ProtVar)", ns),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "120px"
    ))
  )
}

# resolved:   reactive() -> mygene_resolve() result (for UniProt accession).
# search:     reactive() -> list(gene, variant) (for the protein position).
# annotation: reactive() -> myvariant_annotate() result (shared; supplies the
#             protein position for rsID/HGVS inputs via its hgvsp).
protein_summary_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    protein <- reactive({
      retry$dep()
      res <- resolved()
      query <- search()
      if (
        is.null(res) ||
          !isTRUE(res$ok) ||
          is.null(query) ||
          is_blank(query$variant)
      ) {
        return(NULL)
      }
      if (is_blank(res$uniprot)) {
        return(list(ok = FALSE, error = "No UniProt accession for this gene."))
      }
      position <- protein_resolve_position(query$variant, annotation())
      if (is.null(position)) {
        return(list(
          ok = FALSE,
          error = "Could not determine a protein position from the variant."
        ))
      }
      protvar_annotate(res$uniprot, position)
    })

    output$source <- renderUI({
      res <- protein()
      req(!is.null(res), isTRUE(res$ok))
      vr_source_link(src_uniprot(res$accession), "UniProt")
    })

    output$content <- renderUI({
      res <- protein()
      if (is.null(res)) {
        return(vr_empty("Enter a variant to see protein-level context."))
      }
      if (!isTRUE(res$ok)) {
        return(vr_error(res$error))
      }
      tagList(
        vr_field("Accession", res$accession),
        vr_field("Position", res$position),
        if (!is_blank(res$function_text)) {
          tags$p(class = "mt-2", tags$strong("Function: "), res$function_text)
        },
        protein_variants_ui(res$variants)
      )
    })

    # Returned so the parent can surface this card's data to the assistant.
    protein
  })
}

# Derive a protein position from the variant string. For an rsID/HGVS input the
# digits are not a protein position, so use the supplied MyVariant annotation's
# protein change (hgvsp); otherwise treat the input as a protein change
# (e.g. "R175H") and parse it directly.
protein_resolve_position <- function(variant, annotation = NULL) {
  if (myvariant_is_queryable(variant)) {
    if (isTRUE(annotation$ok) && !is_blank(annotation$hgvsp)) {
      return(protvar_parse_position(annotation$hgvsp))
    }
    return(NULL)
  }
  protvar_parse_position(variant)
}

# Render the known-variants-at-residue block.
protein_variants_ui <- function(variants) {
  if (is.null(variants) || nrow(variants) == 0) {
    return(vr_empty("No catalogued variants at this residue."))
  }
  tagList(
    tags$p(class = "mt-2 mb-1", tags$strong("Known variants at this residue:")),
    tags$ul(
      class = "mb-0",
      lapply(seq_len(nrow(variants)), function(i) {
        tags$li(
          tags$strong(variants$change[i]),
          if (!is_blank(variants$sources[i])) {
            tags$span(
              class = "text-muted",
              paste0(" (", variants$sources[i], ")")
            )
          }
        )
      })
    )
  )
}
