# Home layout building blocks: the scrollable stack of result cards
# (`dashboard_page`, left column) and the chat panel (`chat_panel`, right
# column). ui.R composes them side by side.

# The result cards the user can show/hide, id -> display label. The id is both
# the module id and the value tracked by the `visible_cards` checkbox group.
.dashboard_cards <- c(
  gene_summary = "Gene summary",
  variant_summary = "Variant",
  predictions = "In-silico predictions",
  protein_summary = "Protein (ProtVar)",
  landscape = "Variant landscape",
  conservation = "Conservation",
  gnomad_ancestry = "Ancestry frequency",
  domains = "Protein domains",
  structure = "3D structure",
  clinvar = "ClinVar",
  gnomad = "gnomAD",
  constraint = "Gene constraint",
  genemodel = "Gene model",
  ensembl = "Ensembl VEP",
  gtex = "GTEx expression",
  string_ppi = "STRING",
  opentargets = "Open Targets",
  phenotypes = "Phenotypes (HPO)",
  drugs = "Known drugs",
  pharmacogenomics = "Pharmacogenomics",
  literature = "Literature",
  links = "External links"
)

# Cards that start un-ticked, so they cost no API call until the user turns them
# on. The Ensembl gene model is here because rest.ensembl.org is slow and often
# times out; it stays one click away in the Cards popover.
.dashboard_cards_default_off <- c("genemodel")

# The cards shown on load (everything except the default-off set). Reused by the
# Demo so its walkthrough enables the same set.
.dashboard_cards_default_on <- setdiff(
  names(.dashboard_cards),
  .dashboard_cards_default_off
)

# Show a card only when its id is ticked in the `visible_cards` control. The
# JS runs client-side, so toggling is instant and needs no server round-trip.
# The inner `tour_<id>` div is the anchor the Demo walkthrough highlights (see
# R/demo_tour.R); it must stay a plain, height-auto block so it doesn't stretch
# a card to fill the tall left column (which would leave large gaps). It exists
# only while the card is shown, which is why the demo re-ticks the default-on
# cards before starting the tour.
.card_when_shown <- function(id, ui) {
  conditionalPanel(
    condition = sprintf(
      "input.visible_cards && input.visible_cards.indexOf('%s') > -1",
      id
    ),
    div(id = paste0("tour_", id), ui)
  )
}

# Popover to pick which result cards are shown (all but the default-off set are
# ticked on load).
.cards_toggle <- div(
  class = "d-flex justify-content-end mb-2",
  popover(
    actionButton(
      "cards_btn",
      "Cards",
      icon = icon("table-cells"),
      class = "btn-outline-secondary btn-sm"
    ),
    checkboxGroupInput(
      "visible_cards",
      "Show cards",
      choices = stats::setNames(
        names(.dashboard_cards),
        unname(.dashboard_cards)
      ),
      selected = .dashboard_cards_default_on
    ),
    title = "Show cards",
    placement = "bottom"
  )
)

# What the annotations are scoped to. Shown under the search so users know the
# species/assembly/transcript basis before reading the cards.
.annotation_note <- tags$p(
  class = "text-muted small mb-2",
  icon("circle-info"),
  paste(
    " Annotating human (Homo sapiens), genome assembly GRCh38 (hg38);",
    "variant consequences are reported per Ensembl transcript (VEP)."
  )
)

# Left column: search bar, a scope note and any gene/variant warning, the card
# picker, then the grid of result cards.
dashboard_page <- tagList(
  div(id = "tour_search", gene_search_ui("search")),
  .annotation_note,
  uiOutput("search_notice"),
  .cards_toggle,
  # Gene (left) and Protein (right) flank a middle column that stacks the
  # variant card with its clinical-significance (ClinVar) and population-
  # frequency (gnomAD) detail. Those cards are short, so stacking them here
  # fills the whitespace they used to leave beside the taller gene/protein
  # summaries. The vstack keeps them spaced whichever ones are toggled on.
  layout_columns(
    col_widths = c(4, 4, 4),
    .card_when_shown("gene_summary", gene_summary_ui("gene_summary")),
    div(
      class = "vstack gap-3",
      .card_when_shown(
        "variant_summary",
        variant_summary_ui("variant_summary")
      ),
      .card_when_shown("clinvar", clinvar_ui("clinvar")),
      .card_when_shown("gnomad", gnomad_ui("gnomad"))
    ),
    .card_when_shown("protein_summary", protein_summary_ui("protein_summary"))
  ),
  # Whole-protein ClinVar "lollipop": where the variant sits among known
  # variants and domains. Full width, since it spans the protein.
  .card_when_shown("landscape", variant_landscape_ui("landscape")),
  layout_columns(
    col_widths = c(6, 6),
    .card_when_shown("predictions", predictions_ui("predictions")),
    .card_when_shown("constraint", gene_constraint_ui("constraint"))
  ),
  # Two variant-level visualizations: conservation at the residue and the
  # gnomAD allele-frequency breakdown by genetic ancestry.
  layout_columns(
    col_widths = c(6, 6),
    .card_when_shown("conservation", conservation_ui("conservation")),
    .card_when_shown("gnomad_ancestry", gnomad_ancestry_ui("gnomad_ancestry"))
  ),
  # Protein domains/features table beside the 3D structure, each half width so
  # the structure viewer stays roughly square rather than stretching full width.
  layout_columns(
    col_widths = c(6, 6),
    .card_when_shown("domains", protein_domains_ui("domains")),
    .card_when_shown("structure", protein_structure_ui("structure"))
  ),
  # Exon map (Ensembl) next to the per-transcript consequences (both Ensembl).
  .card_when_shown("genemodel", gene_model_ui("genemodel")),
  .card_when_shown("ensembl", ensembl_ui("ensembl")),
  .card_when_shown("gtex", gtex_expression_ui("gtex")),
  layout_columns(
    col_widths = c(6, 6),
    .card_when_shown("string_ppi", string_ppi_ui("string_ppi")),
    .card_when_shown("opentargets", opentargets_ui("opentargets"))
  ),
  # Gene-level knowledge cards (phenotypes, drugs, pharmacogenomics, literature)
  # are full width: their tables are wide (several columns each), so they read
  # better across the whole column than squeezed into a half.
  .card_when_shown("phenotypes", phenotypes_ui("phenotypes")),
  .card_when_shown("drugs", drugs_ui("drugs")),
  .card_when_shown(
    "pharmacogenomics",
    pharmacogenomics_ui("pharmacogenomics")
  ),
  .card_when_shown("literature", literature_ui("literature")),
  .card_when_shown("links", external_links_ui("links"))
)

# Right column: bring-your-own-key chat, grounded in the current search via a
# context tool (see server.R). It is pinned in view while the left column
# scrolls (see the .vr-chat-col rule in www/css/app.css); the chat height is
# viewport-relative so it fills the pinned column.
chat_panel <- byok_chat_ui(
  "chat",
  title = "Ask the assistant",
  height = "calc(100vh - 9rem)",
  # Widen the Model & key drawer so the provider/model controls have room.
  sidebar_width = 470,
  greeting = paste(
    "Hi! Open **Model & key** (the gear button), paste your API key, choose a",
    "model, and click **Connect**. Then ask me about the gene or variant you're",
    "reviewing. Once connected, clickable example prompts appear right here in",
    "the chat. (If a key is set in the environment, I connect on my own.)"
  )
)
