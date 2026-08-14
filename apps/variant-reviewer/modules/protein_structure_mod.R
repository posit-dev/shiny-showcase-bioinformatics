# 3D structure card. Shows the AlphaFold-predicted structure for the gene's
# protein (by UniProt accession) with the variant residue highlighted, via the
# r3dmol viewer. The coordinate file is downloaded only when the viewer renders.

protein_structure_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    vr_card_header("3D structure (AlphaFold)", ns),
    card_body(
      # The viewer lives in the UI permanently instead of being rebuilt by a
      # renderUI on every search. An htmlwidget re-created that way comes back as
      # a fresh, empty element, and the value the server already sent has nowhere
      # to land -- which is why the structure only ever appeared on the first
      # search. Keeping one element and swapping its contents avoids that
      # entirely. conditionalPanel only toggles display, so the node (and its
      # WebGL context) survives; suspendWhenHidden = FALSE in the server keeps it
      # rendering while hidden so it is ready the moment it is shown again.
      shinycssloaders::withSpinner(
        uiOutput(ns("status")),
        proxy.height = "40px"
      ),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("has_model")),
        r3dmol::r3dmolOutput(ns("viewer"), height = "360px"),
        uiOutput(ns("note"))
      )
    )
  )
}

# resolved:   reactive() -> mygene_resolve() result (uses the UniProt accession)
# search:     reactive() -> list(gene, variant)
# annotation: reactive() -> myvariant_annotate() result (for the HGVS position)
protein_structure_server <- function(id, resolved, search, annotation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    retry <- vr_retry_counter()
    vr_card_refresh_observer(input, retry$bump)

    # Lightweight metadata only (model URL + residue) -- no coordinate download,
    # so this is safe to read from the assistant snapshot on every search.
    meta <- reactive({
      retry$dep()
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(NULL)
      }
      if (is_blank(res$uniprot)) {
        return(list(ok = FALSE, error = "No UniProt accession for this gene."))
      }
      out <- alphafold_model(res$uniprot)
      if (isTRUE(out$ok)) {
        out$accession <- res$uniprot
        query <- search()
        out$position <- if (is.null(query) || is_blank(query$variant)) {
          NULL
        } else {
          protein_resolve_position(query$variant, annotation())
        }
      }
      out
    })

    # The coordinate download, kept separate from `meta` so a model that exists
    # but whose file won't download reports that instead of blanking the card.
    coords <- reactive({
      info <- meta()
      if (is.null(info) || !isTRUE(info$ok)) {
        return(NULL)
      }
      alphafold_pdb_text(info$pdb_url)
    })

    # Drives the conditionalPanel wrapping the viewer. Kept alive while hidden so
    # the widget renders in the background and is ready when the card reappears.
    output$has_model <- reactive({
      pdb <- coords()
      !is.null(pdb) && isTRUE(pdb$ok)
    })
    outputOptions(output, "has_model", suspendWhenHidden = FALSE)

    output$viewer <- r3dmol::renderR3dmol({
      info <- meta()
      req(info, isTRUE(info$ok))
      pdb <- coords()
      req(isTRUE(pdb$ok))
      viewer <- r3dmol::r3dmol(backgroundColor = "#faf8f3") |>
        r3dmol::m_add_model(data = pdb$text, format = "pdb") |>
        r3dmol::m_set_style(
          style = r3dmol::m_style_cartoon(color = "#7a7468")
        ) |>
        r3dmol::m_zoom_to()
      pos <- suppressWarnings(as.integer(info$position %||% NA))
      if (!is.na(pos)) {
        viewer <- viewer |>
          r3dmol::m_add_style(
            sel = r3dmol::m_sel(resi = pos),
            style = r3dmol::m_style_stick(color = "#c07a52", radius = 0.3)
          ) |>
          r3dmol::m_add_style(
            sel = r3dmol::m_sel(resi = pos),
            style = r3dmol::m_style_sphere(color = "#c07a52", scale = 0.5)
          ) |>
          r3dmol::m_zoom_to(sel = r3dmol::m_sel(resi = pos))
      }
      viewer
    })
    # Set after the output exists (outputOptions errors otherwise). Keeping the
    # viewer unsuspended while its conditionalPanel is hidden means it re-renders
    # in place on a new search instead of waiting to be shown.
    outputOptions(output, "viewer", suspendWhenHidden = FALSE)

    output$source <- renderUI({
      info <- meta()
      req(!is.null(info), isTRUE(info$ok))
      vr_source_link(src_alphafold(info$accession), "AlphaFold")
    })

    # Only the messages: nothing here builds the viewer, so the widget element is
    # never torn down and rebuilt.
    output$status <- renderUI({
      info <- meta()
      if (is.null(info)) {
        return(vr_empty("Search for a gene to see its predicted 3D structure."))
      }
      if (!isTRUE(info$ok)) {
        return(vr_error(info$error))
      }
      pdb <- coords()
      # The model exists but its coordinates would not download -- say so rather
      # than leaving an empty card with no explanation.
      if (!is.null(pdb) && !isTRUE(pdb$ok)) {
        return(vr_error(
          pdb$error %||%
            "Could not download the structure coordinates from AlphaFold."
        ))
      }
      NULL
    })

    output$note <- renderUI({
      info <- meta()
      req(info, isTRUE(info$ok))
      pos <- info$position
      note <- if (!is.null(pos) && !is.na(suppressWarnings(as.integer(pos)))) {
        sprintf(
          "AlphaFold model for %s; variant residue %s highlighted in clay.",
          info$accession,
          pos
        )
      } else {
        sprintf("AlphaFold model for %s.", info$accession)
      }
      tags$p(
        class = "text-muted small mb-0 mt-2",
        note,
        " Predicted (computed) structure from AlphaFold DB, not experimental."
      )
    })

    # Returned so the parent can surface this card's metadata to the assistant.
    meta
  })
}
