# Guided demo walkthrough.
#
# The navbar "Demo" button loads a worked example (BRAF V600E), then runs this
# {cicerone} tour: it highlights the search box, steps through each result card
# explaining what it shows, and finishes on the AI assistant. The step elements
# are the `tour_*` anchor ids added in userInterface/dashboard_ui.R and ui.R.
#
# cicerone is optional: vr_demo_tour() returns NULL when it isn't installed, and
# the demo handler in server.R falls back to a plain modal in that case.

# One tour step per card, in the dashboard's own top-to-bottom order, so the
# highlight moves down the page as the user clicks Next. Each entry is
# list(el = <anchor id>, title, description).
.vr_demo_steps <- list(
  list(
    el = "tour_search",
    title = "1. Search",
    description = paste(
      "We've filled in <b>BRAF V600E (rs113488022)</b> for you. Click",
      "<b>Review</b> to load it, then use <b>Next</b> to walk through the cards",
      "below as they populate."
    )
  ),
  list(
    el = "tour_gene_summary",
    title = "Gene summary",
    description = "What the gene is and does, with identifiers (MyGene)."
  ),
  list(
    el = "tour_variant_summary",
    title = "Variant",
    description = paste(
      "The variant's annotation: dbSNP, clinical significance, and scores",
      "(MyVariant)."
    )
  ),
  list(
    el = "tour_clinvar",
    title = "ClinVar",
    description = "Clinical significance and review status for the variant."
  ),
  list(
    el = "tour_gnomad",
    title = "gnomAD",
    description = "Population allele frequencies: how common the variant is."
  ),
  list(
    el = "tour_protein_summary",
    title = "Protein",
    description = "Functional and structural context at the residue (ProtVar)."
  ),
  list(
    el = "tour_landscape",
    title = "Variant landscape",
    description = paste(
      "Every ClinVar variant placed along the protein (coloured by",
      "significance) over the domain track, so you can see if the variant sits",
      "in a hotspot."
    )
  ),
  list(
    el = "tour_predictions",
    title = "In-silico predictions",
    description = "Computational pathogenicity predictions for the variant."
  ),
  list(
    el = "tour_constraint",
    title = "Gene constraint",
    description = "How intolerant the gene is to loss-of-function (gnomAD)."
  ),
  list(
    el = "tour_conservation",
    title = "Conservation",
    description = "Evolutionary conservation at the residue (phyloP, GERP++, …)."
  ),
  list(
    el = "tour_gnomad_ancestry",
    title = "Ancestry frequency",
    description = "Allele frequency broken down by genetic-ancestry group."
  ),
  list(
    el = "tour_domains",
    title = "Protein domains",
    description = "Domains and features along the protein sequence."
  ),
  list(
    el = "tour_structure",
    title = "3D structure",
    description = "The predicted structure, with the residue highlighted."
  ),
  list(
    el = "tour_genemodel",
    title = "Gene model",
    description = "The canonical transcript's exons, with the variant's exon marked."
  ),
  list(
    el = "tour_ensembl",
    title = "Consequences",
    description = "Predicted molecular consequences per transcript (Ensembl VEP)."
  ),
  list(
    el = "tour_gtex",
    title = "Expression",
    description = "Median expression across tissues (GTEx)."
  ),
  list(
    el = "tour_string_ppi",
    title = "Interactions",
    description = "Top protein-protein interaction partners (STRING)."
  ),
  list(
    el = "tour_opentargets",
    title = "Diseases",
    description = "Disease associations for the gene (Open Targets)."
  ),
  list(
    el = "tour_phenotypes",
    title = "Phenotypes (HPO)",
    description = "Human phenotypes associated with the gene (HPO via Monarch)."
  ),
  list(
    el = "tour_drugs",
    title = "Known drugs",
    description = paste(
      "Drugs and clinical candidates that target the gene, with their highest",
      "clinical stage (Open Targets)."
    )
  ),
  list(
    el = "tour_pharmacogenomics",
    title = "Pharmacogenomics",
    description = paste(
      "Variant and genotype effects on drug response, with an evidence level",
      "(Open Targets)."
    )
  ),
  list(
    el = "tour_literature",
    title = "Literature",
    description = paste(
      "Recent publications for the gene, refined by the variant when one is",
      "loaded (Europe PMC)."
    )
  ),
  list(
    el = "tour_links",
    title = "External links",
    description = "Jump out to the source databases for the gene and variant."
  ),
  list(
    el = "tour_chat",
    title = "AI assistant",
    description = paste(
      "Ask about the loaded gene or variant here. Open <b>Model &amp; key</b>,",
      "connect a provider (a key in the environment is used automatically),",
      "then type a question or click one of the example prompts. The assistant",
      "can read these cards and load new genes/variants for you."
    ),
    position = "left"
  )
)

# Fallback shown when cicerone isn't installed: a plain modal that names the
# card groups and explains the assistant, so the demo still guides the user.
vr_demo_modal <- function(example_label) {
  modalDialog(
    title = paste0("Demo: ", example_label),
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Got it"),
    tags$p(
      "Filled in ",
      tags$strong(example_label),
      " for you. Click ",
      tags$strong("Review"),
      " to load it, then scroll the results to explore each card:"
    ),
    tags$ul(
      tags$li(
        tags$strong("Gene / Variant / Protein"),
        ": identity, annotation, and protein context."
      ),
      tags$li(
        tags$strong("ClinVar / gnomAD / Consequences"),
        ": clinical significance, population frequency, predicted effect."
      ),
      tags$li(
        tags$strong("Expression / Interactions / Diseases"),
        ": GTEx tissue expression, STRING partners, Open Targets associations."
      )
    ),
    tags$hr(),
    tags$p(tags$strong("Using the AI assistant")),
    tags$ol(
      tags$li("Open ", tags$strong("Model & key"), " on the chat panel."),
      tags$li(
        "Connect a provider. An environment key is used automatically,",
        " otherwise paste your own (kept only in this session)."
      ),
      tags$li(
        "Ask about the loaded gene/variant, or click an example prompt."
      )
    )
  )
}

# Build the cicerone guide for the demo, or NULL when cicerone is unavailable.
vr_demo_tour <- function() {
  if (!requireNamespace("cicerone", quietly = TRUE)) {
    return(NULL)
  }
  guide <- cicerone::Cicerone$new(
    opacity = 0.6,
    padding = 6,
    allow_close = TRUE
  )
  # Skip steps for cards that start un-ticked (their tour anchor is absent, and
  # the demo doesn't enable them) so the walkthrough never highlights a missing
  # element or triggers a default-off source.
  off_anchors <- paste0("tour_", .dashboard_cards_default_off)
  for (s in .vr_demo_steps) {
    if (s$el %in% off_anchors) {
      next
    }
    # Every card (and the chat panel) is highlighted by its header only, so the
    # spotlight stays compact; the search box has no header, so it's highlighted
    # whole (showing the filled inputs and the Review button).
    use_header <- !identical(s$el, "tour_search")
    guide$step(
      el = if (use_header) paste0("#", s$el, " .card-header") else s$el,
      title = s$title,
      description = s$description,
      is_id = !use_header,
      position = s$position %||% "bottom"
    )
  }
  guide
}
