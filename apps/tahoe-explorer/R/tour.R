# Guided product tours (cicerone).
#
# A click-through demo that walks a first-time user across a page's key
# controls. One tour per page; the navbar "Demo" button (see server.R) starts
# the tour for whichever tab is active. Each step targets a page module's
# namespaced element ids -- the `tour_*` anchors added in the module UIs.
#
# tahoe_tours() maps a page id (the register_page id / navbar value) to a
# zero-arg builder returning a cicerone::Cicerone guide. server.R builds and
# initialises each once per session.

# Small helper: a Cicerone guide from a list of step lists. Each step is
# list(el, title, description, position); `el` is the raw (un-#'d) element id.
.tahoe_guide <- function(id, steps) {
  # "Skip" ends this page's tour and hands off to the next page's (see
  # server.R); "Next ▸" on the last step does the same on finish.
  guide <- cicerone::Cicerone$new(
    id = id,
    close_btn_text = "Skip",
    done_btn_text = "Next ▸",
    next_btn_text = "Next",
    prev_btn_text = "Back"
  )
  for (s in steps) {
    guide$step(
      el = s$el,
      title = s$title,
      description = s$description,
      position = s$position %||% "auto"
    )
  }
  guide
}

overview_tour <- function() {
  .tahoe_guide(
    "overview_tour",
    list(
      list(
        el = "overview-tour_summary",
        title = "Dataset at a glance",
        description = paste(
          "The headline dimensions of Tahoe-100M -- cells, cell lines, drugs,",
          "samples, plates, and genes -- computed from the data you have loaded."
        ),
        position = "bottom"
      ),
      list(
        el = "overview-tour_organs",
        title = "Cell lines by organ",
        description = paste(
          "How the 50 assayed cell lines break down by tissue of origin. Click a",
          "bar to filter the table to that organ; click it again to clear."
        ),
        position = "right"
      ),
      list(
        el = "overview-tour_table",
        title = "Cell-line table",
        description = paste(
          "The assayed cell lines and their driver mutations, with DepMap links.",
          "Click a row to break that line's drivers down in the charts below."
        ),
        position = "left"
      ),
      list(
        el = "overview-tour_drivers",
        title = "Driver profile",
        description = paste(
          "For the cell line you clicked: its oncogenic driver genes (colored by",
          "role) and how those mutations split by variant class."
        ),
        position = "top"
      )
    )
  )
}

about_tour <- function() {
  .tahoe_guide(
    "about_tour",
    list(
      list(
        el = "about-tour_glance",
        title = "The numbers",
        description = paste(
          "Live dataset dimensions and what each one means -- a quick orientation",
          "before you dive in."
        ),
        position = "bottom"
      ),
      list(
        el = "about-tour_design",
        title = "Design & data model",
        description = paste(
          "How the experiment is laid out (cell lines → plates → wells → cells)",
          "and how the metadata tables relate to the cell-level obs table."
        ),
        position = "top"
      ),
      list(
        el = "about-tour_tables",
        title = "The metadata tables",
        description = paste(
          "Expand each table to see its columns -- drugs, cell lines, samples,",
          "obs, and genes. This app reads the small tables and aggregates obs",
          "lazily."
        ),
        position = "top"
      ),
      list(
        el = "about-tour_using",
        title = "Using this app",
        description = paste(
          "A map of the tabs and what each is for. Sources and the pinned dataset",
          "revision are here too."
        ),
        position = "top"
      )
    )
  )
}

drug_tour <- function() {
  .tahoe_guide(
    "drug_tour",
    list(
      list(
        el = "drugs-tour_filters",
        title = "Filter the catalog",
        description = paste(
          "Narrow the drug list by mechanism of action, approval status,",
          "clinical-trial phase, target gene, or name. Filters combine, and an",
          "empty filter means no restriction."
        ),
        position = "right"
      ),
      list(
        el = "drugs-tour_picker",
        title = "Pick a drug",
        description = paste(
          "Or jump straight to one drug here. This picker and the table's row",
          "selection always stay in sync."
        ),
        position = "right"
      ),
      list(
        el = "drugs-tour_table",
        title = "Browse the matches",
        description = paste(
          "Every filtered drug, with PubChem links. Click a row to select it and",
          "the whole right column updates to that drug. Use Columns to choose",
          "what to show."
        ),
        position = "top"
      ),
      list(
        el = "drugs-tour_detail",
        title = "Drug details",
        description = paste(
          "Targets, mechanism, approval, trials, and a PubChem link for the",
          "selected drug."
        ),
        position = "left"
      ),
      list(
        el = "drugs-tour_mut",
        title = "Target mutations",
        description = paste(
          "Which assayed cell lines carry a somatic variant in the drug's target",
          "genes -- the lines where a mutant-vs-wild-type contrast could be",
          "designed."
        ),
        position = "left"
      ),
      list(
        el = "drugs-tour_charts",
        title = "Summary charts",
        description = paste(
          "Mechanism, approval, and top targets across the filtered set. The",
          "selected drug's categories are highlighted -- and clicking any bar",
          "filters the table by that category."
        ),
        position = "left"
      ),
      list(
        el = "drugs-tour_export",
        title = "Export the subset",
        description = paste(
          "Download the current filtered drugs as CSV or Parquet for downstream",
          "analysis."
        ),
        position = "left"
      )
    )
  )
}

cell_line_tour <- function() {
  .tahoe_guide(
    "cell_line_tour",
    list(
      list(
        el = "cell_lines-tour_filters",
        title = "Filter cell lines",
        description = paste(
          "Narrow the lines by organ, driver gene, variant type, or name to find",
          "a model system of interest."
        ),
        position = "right"
      ),
      list(
        el = "cell_lines-tour_table",
        title = "Matching cell lines",
        description = paste(
          "The lines that match your filters. Choose columns to show and export",
          "the current selection right from the card."
        ),
        position = "top"
      ),
      list(
        el = "cell_lines-tour_charts",
        title = "Composition charts",
        description = paste(
          "The filtered set summarised by organ, top driver genes, and variant",
          "type."
        ),
        position = "left"
      ),
      list(
        el = "cell_lines-tour_variants",
        title = "Somatic variants",
        description = paste(
          "Per-variant mutation calls (DepMap / Cellosaurus) for the matching",
          "lines, plus the most frequently mutated genes across them."
        ),
        position = "top"
      )
    )
  )
}

obs_tour <- function() {
  .tahoe_guide(
    "obs_tour",
    list(
      list(
        # Target the tab strip itself (small), not the whole tabset wrapper,
        # so the popover sits next to it instead of spanning the page.
        el = "obs-view",
        title = "Two views",
        description = paste(
          "Switch between Samples & plates (per-sample QC) and Cell-level obs,",
          "which aggregates the ~100M-row cell table lazily in duckdb -- raw cells",
          "are never loaded."
        ),
        position = "bottom"
      ),
      list(
        el = "obs-tour_filters",
        title = "Filter samples",
        description = "Restrict to specific plates or drugs.",
        position = "right"
      ),
      list(
        el = "obs-tour_charts",
        title = "Sample QC charts",
        description = paste(
          "Samples per plate and per drug, plus the distribution of mean mito",
          "fraction and mean transcript count across samples."
        ),
        position = "top"
      ),
      list(
        el = "obs-tour_table",
        title = "Filtered samples",
        description = paste(
          "The matching samples with their QC means; export the selection for",
          "downstream use."
        ),
        position = "top"
      )
    )
  )
}

coverage_tour <- function() {
  .tahoe_guide(
    "coverage_tour",
    list(
      list(
        el = "coverage-tour_controls",
        title = "Choose the matrix",
        description = paste(
          "Pick which drugs (rows) and organs (columns) to show. Tahoe-100M is",
          "fully crossed, so every drug was tested in every cell line."
        ),
        position = "right"
      ),
      list(
        el = "coverage-tour_heatmap",
        title = "Coverage heatmap",
        description = paste(
          "Each tile is one drug × cell line; color is the number of cells",
          "profiled (log scale) -- darker means more statistical power. Click a",
          "tile to break it down by dose."
        ),
        position = "left"
      ),
      list(
        el = "coverage-tour_detail",
        title = "Per-dose detail",
        description = paste(
          "The clicked combination's cell counts at each dose, including the DMSO",
          "vehicle control."
        ),
        position = "top"
      )
    )
  )
}

qc_tour <- function() {
  .tahoe_guide(
    "qc_tour",
    list(
      list(
        el = "qc-tour_boxes",
        title = "QC headline",
        description = "Top-line counts summarising the dataset's power and coverage.",
        position = "bottom"
      ),
      list(
        el = "qc-tour_controls",
        title = "Set the threshold",
        description = paste(
          "Define what counts as well-powered (minimum cells per condition) and",
          "optionally restrict to QC-passing cells; the tables below update live."
        ),
        position = "bottom"
      ),
      list(
        el = "qc-tour_power",
        title = "Power & dose gaps",
        description = paste(
          "Underpowered conditions (too few cells) and drug × cell-line",
          "combinations missing one or more doses."
        ),
        position = "top"
      ),
      list(
        el = "qc-tour_batch",
        title = "Controls & batch structure",
        description = paste(
          "DMSO vehicle-control coverage per cell line, and how drugs map onto",
          "the 14 plates -- both matter for confounding."
        ),
        position = "top"
      ),
      list(
        el = "qc-tour_phase",
        title = "Cell-cycle composition",
        description = paste(
          "The share of cells in each cell-cycle phase by organ -- a phenotype",
          "worth accounting for before differential expression."
        ),
        position = "top"
      )
    )
  )
}

subset_tour <- function() {
  .tahoe_guide(
    "subset_tour",
    list(
      list(
        el = "subset-tour_builder",
        title = "Build a subset",
        description = paste(
          "Slice the dataset by tissue, driver gene, cell line, drug, dose, and",
          "plate. Because all 50 lines are pooled into every sample, tissue /",
          "driver / cell-line filters change the cell count, not the sample",
          "count."
        ),
        position = "right"
      ),
      list(
        el = "subset-tour_estimate",
        title = "Size estimate",
        description = paste(
          "How many cells and samples your current selection covers, updated as",
          "you filter."
        ),
        position = "bottom"
      ),
      list(
        el = "subset-tour_preview",
        title = "Preview the slice",
        description = paste(
          "Cells in the selection by cell line, alongside the matched samples."
        ),
        position = "top"
      ),
      list(
        el = "subset-tour_export",
        title = "Export & recipe",
        description = paste(
          "Download the matched samples, and get a reproducible analysis recipe --",
          "copy-paste code or a ready-to-run R / Python notebook that pulls this",
          "exact subset from the full dataset."
        ),
        position = "top"
      )
    )
  )
}

genes_tour <- function() {
  .tahoe_guide(
    "genes_tour",
    list(
      list(
        el = "genes-tour_lookup",
        title = "Check genes",
        description = paste(
          "Paste gene symbols (comma or space separated) to see which were",
          "measured; leave it empty to browse all 62,710 features."
        ),
        position = "right"
      ),
      list(
        el = "genes-tour_summary",
        title = "How many genes",
        description = paste(
          "The number of measured features, and -- once you run a lookup -- how",
          "many of your genes were found versus missing."
        ),
        position = "bottom"
      ),
      list(
        el = "genes-tour_table",
        title = "Browse & export",
        description = paste(
          "The gene table (symbol, Ensembl id, token id). Export the current",
          "view for a downstream targeted reanalysis."
        ),
        position = "top"
      )
    )
  )
}
#' Named list mapping each page id (register_page id / navbar value) to its
#' tour builder. server.R uses this to start the right tour for the active tab.
#' The assistant is an app-wide sidebar rather than a tab, so it has no tour.
tahoe_tours <- function() {
  list(
    overview = overview_tour,
    about = about_tour,
    drugs = drug_tour,
    genes = genes_tour,
    cell_lines = cell_line_tour,
    obs = obs_tour,
    coverage = coverage_tour,
    qc = qc_tour,
    subset = subset_tour
  )
}
