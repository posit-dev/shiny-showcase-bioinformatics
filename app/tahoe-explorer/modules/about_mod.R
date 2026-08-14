# About module.
#
# An explainer tab: what Tahoe-100M is, its dimensions (live), how the
# experiment is laid out, the metadata tables/columns, key concepts, and what
# the data is useful for. Includes two Mermaid diagrams (experimental design and
# data model) rendered offline via DiagrammeR.

# Diagrams are Graphviz (DOT), styled with the Alpine Lake palette and rendered
# offline by DiagrammeR's grViz / viz.js (more reliable in Shiny than mermaid).
.about_node_style <- paste0(
  "  node [shape = box, style = \"rounded,filled\", fontname = \"Inter\", ",
  "fontcolor = white, penwidth = 0, margin = \"0.18,0.12\"]"
)

# Experimental design flow (left to right).
.about_design_spec <- paste(
  c(
    "digraph design {",
    "  graph [rankdir = LR, bgcolor = \"transparent\"]",
    .about_node_style,
    "  edge [color = \"#5F6B7A\", arrowsize = 0.8]",
    "  CL    [label = \"50 cancer\\ncell lines\", fillcolor = \"#0B7285\"]",
    "  POOL  [label = \"Pooled &\\nbarcoded\", fillcolor = \"#0B7285\"]",
    "  PLATE [label = \"14 plates\\n96 wells each\", fillcolor = \"#0B7285\"]",
    "  DMSO  [label = \"DMSO_TF\\nvehicle control\", fillcolor = \"#5F6B7A\"]",
    "  WELL  [label = \"Each well =\\n1 drug @ 1 dose\", fillcolor = \"#1C7ED6\"]",
    "  DOSE  [label = \"3 doses\\n0.05 / 0.5 / 5 uM\", fillcolor = \"#2F9E44\"]",
    "  SEQ   [label = \"scRNA-seq\\nMosaic platform\", fillcolor = \"#1C7ED6\"]",
    "  CELLS [label = \"~100.6M\\nsingle cells\", fillcolor = \"#E8590C\"]",
    "  CL -> POOL -> PLATE -> WELL -> DOSE -> SEQ -> CELLS",
    "  DMSO -> PLATE",
    "}"
  ),
  collapse = "\n"
)

# Metadata tables and how they relate to the cell-level obs table.
.about_model_spec <- paste(
  c(
    "digraph model {",
    "  graph [rankdir = TB, bgcolor = \"transparent\"]",
    .about_node_style,
    "  edge [color = \"#5F6B7A\", fontname = \"Inter\", fontcolor = \"#5F6B7A\"]",
    paste0(
      "  OBS [label = \"obs_metadata\\n~100.6M rows, one per cell\\n",
      "drug, cell_line, plate, dose, QC\", fillcolor = \"#0B7285\"]"
    ),
    "  DRUG [label = \"drug_metadata\\n379 drugs\", fillcolor = \"#1C7ED6\"]",
    paste0(
      "  CELLTBL [label = \"cell_line_metadata\\norgan, drivers\", ",
      "fillcolor = \"#2F9E44\"]"
    ),
    paste0(
      "  SAMP [label = \"sample_metadata\\n1,344 samples\", ",
      "fillcolor = \"#E8590C\"]"
    ),
    "  GENE [label = \"gene_metadata\\n62,710 genes\", fillcolor = \"#5F6B7A\"]",
    "  OBS -> DRUG    [label = \"drug\"]",
    "  OBS -> CELLTBL [label = \"cell_name\"]",
    "  OBS -> SAMP    [label = \"sample\"]",
    "  OBS -> GENE    [label = \"features\", style = dashed]",
    "}"
  ),
  collapse = "\n"
)

# One row of the dimensions table: label, live count, description.
.about_dim_row <- function(label, value, description) {
  tags$tr(
    tags$td(tags$strong(label)),
    tags$td(class = "text-end", value),
    tags$td(class = "text-muted", description)
  )
}

# A metadata-table entry in the glossary accordion.
.about_table_panel <- function(title, purpose, columns) {
  bslib::accordion_panel(
    title,
    tags$p(class = "text-muted mb-2", purpose),
    tags$ul(lapply(columns, function(c) tags$li(HTML(c))))
  )
}

about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-2",
      h2("What is Tahoe-100M?"),
      p(
        class = "lead",
        "Tahoe-100M is a giga-scale single-cell drug-perturbation atlas:",
        "over 100 million single-cell transcriptomes from 50 cancer cell",
        "lines exposed to ~1,100 small-molecule treatments at multiple doses."
      ),
      p(
        "It was produced on Vevo/Tahoe's high-throughput Mosaic platform (with",
        "Parse Biosciences and Ultima Genomics) and open-sourced in February",
        "2025 as the inaugural dataset of Arc Institute's Virtual Cell Atlas.",
        "It is ~50x larger than all previously public drug-perturbed",
        "single-cell data combined, making it a resource for studying",
        "context-dependent gene function, drug mechanisms, and for training",
        "virtual-cell / perturbation-prediction models."
      )
    ),
    # tour_* ids anchor the guided demo (see R/tour.R).
    bslib::card(
      id = ns("tour_glance"),
      bslib::card_header("At a glance"),
      uiOutput(ns("dims")),
      bslib::card_footer(
        class = "text-muted small",
        paste(
          "Counts reflect the data currently loaded -- the bundled demo",
          "fixtures unless you have downloaded the real metadata (see the",
          "provenance badge, top-right). The figures elsewhere on this page",
          "describe the full published Tahoe-100M dataset."
        )
      )
    ),
    bslib::layout_columns(
      id = ns("tour_design"),
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("How the experiment is laid out"),
        DiagrammeR::grVizOutput(ns("design"), height = "300px"),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          "All 50 cell lines are pooled into every sample, so a sample is one",
          "drug at one dose on one plate -- not a single cell line."
        )
      ),
      bslib::card(
        bslib::card_header("Data model"),
        DiagrammeR::grVizOutput(ns("model"), height = "300px"),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          "The cell-level obs table links to four small annotation tables.",
          "This app reads the small tables directly and aggregates obs lazily."
        )
      )
    ),
    bslib::card(
      id = ns("tour_tables"),
      bslib::card_header("The metadata tables"),
      bslib::accordion(
        open = FALSE,
        .about_table_panel(
          "drug_metadata (379 drugs)",
          "One row per perturbing compound.",
          c(
            "<code>drug</code> -- compound name",
            "<code>targets</code> -- known protein targets",
            "<code>moa-broad</code> / <code>moa-fine</code> -- mechanism of action",
            "<code>human-approved</code>, <code>clinical-trials</code> -- status",
            "<code>canonical_smiles</code>, <code>pubchem_cid</code> -- structure"
          )
        ),
        .about_table_panel(
          "cell_line_metadata (driver-level)",
          paste(
            "Driver-mutation annotations -- many rows per cell line",
            "(~102 annotated lines; 50 were assayed)."
          ),
          c(
            "<code>cell_name</code> -- cell line",
            "<code>Organ</code> -- tissue of origin",
            "<code>Driver_Gene_Symbol</code>, <code>Driver_VarType</code> -- driver mutation",
            "<code>Cell_ID_DepMap</code>, <code>Cell_ID_Cellosaur</code> -- external IDs"
          )
        ),
        .about_table_panel(
          "sample_metadata (1,344 samples)",
          "One row per drug x dose x plate condition.",
          c(
            "<code>sample</code>, <code>plate</code> -- identifiers",
            "<code>drug</code>, <code>drugname_drugconc</code> -- treatment + dose",
            paste(
              "<code>mean_gene_count</code>, <code>mean_tscp_count</code>,",
              "<code>mean_pcnt_mito</code> -- per-sample QC means"
            )
          )
        ),
        .about_table_panel(
          "obs_metadata (~100.6M cells)",
          "One row per cell -- the full cell-level table (~2.29 GB).",
          c(
            "<code>drug</code>, <code>cell_line</code>, <code>plate</code>, <code>sample</code>",
            "<code>drugname_drugconc</code> -- encodes the dose",
            paste(
              "<code>gene_count</code>, <code>tscp_count</code>,",
              "<code>pcnt_mito</code>, <code>pass_filter</code> -- QC"
            ),
            "<code>S_score</code>, <code>G2M_score</code>, <code>phase</code> -- cell cycle"
          )
        ),
        .about_table_panel(
          "gene_metadata (62,710 genes)",
          "The measured features.",
          c(
            "<code>gene_symbol</code>, <code>ensembl_id</code>, <code>token_id</code>"
          )
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Key concepts"),
        tags$dl(
          tags$dt("Plate"),
          tags$dd(
            class = "text-muted",
            "A 96-well experimental batch. There are 14; each holds ~93-95",
            "drug treatments plus DMSO_TF vehicle-control wells."
          ),
          tags$dt("Dose"),
          tags$dd(
            class = "text-muted",
            "Concentration of a drug (0.05 / 0.5 / 5 uM), parsed from",
            tags$code("drugname_drugconc"),
            "."
          ),
          tags$dt("DMSO control"),
          tags$dd(
            class = "text-muted",
            "The vehicle (no active drug) baseline present on every plate."
          ),
          tags$dt("Driver mutation"),
          tags$dd(
            class = "text-muted",
            "A cancer-driver gene alteration annotating each cell line."
          )
        )
      ),
      bslib::card(
        bslib::card_header("What can you use it for?"),
        tags$ul(
          tags$li("Map drug mechanisms of action and dose responses"),
          tags$li("Study context-dependent gene function across cell lines"),
          tags$li("Train / benchmark virtual-cell & perturbation models"),
          tags$li("Select cell lines by tissue or driver mutation"),
          tags$li("Plan an analysis and pull a reproducible subset")
        )
      )
    ),
    bslib::card(
      id = ns("tour_using"),
      bslib::card_header("Using this app"),
      tags$ul(
        tags$li(tags$b("Overview"), " -- headline dimensions and quick charts"),
        tags$li(tags$b("Drugs"), " -- filter drugs by MOA, approval, target"),
        tags$li(tags$b("Cell lines"), " -- filter by organ, driver, variant"),
        tags$li(
          tags$b("Samples & cells"),
          " -- sample QC + lazy cell aggregation"
        ),
        tags$li(tags$b("Subset builder"), " -- plan a slice + export a recipe")
      ),
      tags$hr(),
      tags$p(
        class = "mb-0",
        tags$b("Sources: "),
        tags$a(
          href = "https://huggingface.co/datasets/tahoebio/Tahoe-100M",
          target = "_blank",
          rel = "noopener",
          "Hugging Face dataset"
        ),
        " · ",
        tags$a(
          href = "https://arcinstitute.org/tools/virtualcellatlas",
          target = "_blank",
          rel = "noopener",
          "Arc Virtual Cell Atlas"
        ),
        " · ",
        tags$a(
          href = "https://www.biorxiv.org/content/10.1101/2025.02.20.639398",
          target = "_blank",
          rel = "noopener",
          "Preprint"
        ),
        " · ",
        tags$a(
          href = "https://depmap.org/portal/",
          target = "_blank",
          rel = "noopener",
          "DepMap 24Q4"
        ),
        " · ",
        tags$a(
          href = "https://www.cellosaurus.org",
          target = "_blank",
          rel = "noopener",
          "Cellosaurus"
        ),
        " (cell-line somatic variants, both CC BY 4.0)"
      ),
      tags$p(
        class = "small text-muted mb-0 mt-2",
        sprintf(
          "Dataset pinned to Tahoe-100M revision %s for reproducibility.",
          substr(tahoe_dataset_pin()$revision, 1, 7)
        )
      )
    )
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counts <- reactive(tahoe_summary_counts())

    output$dims <- renderUI({
      cc <- counts()
      fmt <- function(x) {
        if (is.null(x) || is.na(x)) "-" else format(x, big.mark = ",")
      }
      cells <- if (is.null(cc$cells) || is.na(cc$cells)) {
        "-"
      } else {
        scales::label_number(
          accuracy = 0.1,
          scale_cut = scales::cut_short_scale()
        )(cc$cells)
      }
      tags$table(
        class = "table table-sm align-middle mb-0",
        tags$thead(tags$tr(
          tags$th("Dimension"),
          tags$th(class = "text-end", "Count"),
          tags$th("What it is")
        )),
        tags$tbody(
          .about_dim_row("Cells", cells, "single-cell transcriptomes"),
          .about_dim_row(
            "Cell lines",
            fmt(cc$cell_lines),
            "cancer cell lines assayed"
          ),
          .about_dim_row(
            "Drugs",
            fmt(cc$drugs),
            "small molecules (~1,100 drug-dose treatments)"
          ),
          .about_dim_row("Doses", "3", "0.05 / 0.5 / 5 uM"),
          .about_dim_row(
            "Plates",
            fmt(cc$plates),
            "96-well experimental batches"
          ),
          .about_dim_row(
            "Samples",
            fmt(cc$samples),
            "drug x dose x plate conditions"
          ),
          .about_dim_row("Genes", fmt(cc$genes), "measured features")
        )
      )
    })

    output$design <- DiagrammeR::renderGrViz(
      DiagrammeR::grViz(.about_design_spec)
    )
    output$model <- DiagrammeR::renderGrViz(
      DiagrammeR::grViz(.about_model_spec)
    )
  })
}
