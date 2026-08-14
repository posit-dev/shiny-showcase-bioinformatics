# Builder page: compact sidebar + a single vertical workflow -----------------
#
# Rendered as a nav_panel of the app-wide page_navbar (see ui.R), so this is a
# bslib::layout_sidebar (not a page_*).
#
# Sidebar is kept SHORT so disease -> sources -> Build are all visible without
# scrolling: the many ranking-weight controls live in a collapsed accordion
# (they're optional - defaults work, and re-ranking is instant afterwards).
#
# Main area is a single vertical stack of cards (no tabs): 1 Sources -> 2 Ranked
# -> 3 AI curation, with a slim Export toolbar at the very bottom. The `tour_*`
# element ids are anchors targeted by the cicerone guided demo (R/tour.R).

builder_page <- bslib::layout_sidebar(
  sidebar = bslib::sidebar(
    # Wider than the bslib default: the disease dropdown shows a full term plus
    # its ontology id, and the source checkbox labels are long enough to wrap
    # awkwardly at 340.
    width = 420,
    class = "glb-build-sidebar",
    div(id = "tour_disease", disease_search_ui("disease")),
    div(id = "tour_sources", source_select_ui("sources")),
    div(id = "tour_run", run_pipeline_ui("run")),
    # Optional weights, collapsed so they don't push Build below the fold.
    div(
      id = "tour_weights",
      bslib::accordion(
        open = FALSE,
        class = "mt-2",
        bslib::accordion_panel(
          "Ranking weights (optional)",
          icon = shiny::icon("sliders"),
          ranking_controls_ui("controls")
        )
      )
    )
  ),
  div(
    id = "tour_workflow",
    class = "d-flex flex-column gap-3",
    # Orientation card. A first-time user landing here saw three numbered
    # steps with no statement of what the tool actually produces, so this says
    # it plainly before the workflow starts.
    bslib::card(
      class = "glb-intro",
      bslib::card_body(
        class = "py-3",
        tags$h5(class = "mb-2", "Build a ranked gene list for a disease"),
        markdown(paste(
          "Name a disease and this resolves it to an ontology term, queries",
          "several gene-disease association databases at once, merges the",
          "answers into one row per gene, and ranks them by weight of",
          "evidence and how many independent sources agree. It replaces",
          "hand-querying one database at a time."
        )),
        tags$p(
          class = "small mb-0",
          tags$strong("Evidence sources"),
          " (Open Targets, PanelApp, DISEASES, ClinVar, DGIdb) add candidate",
          " genes and count toward the multi-source bonus. ",
          tags$strong("Annotation sources"),
          " (gnomAD constraint, Pharos druggability) only adjust the score,",
          " so nothing looks well-supported on constraint alone. Start on the",
          " left with a disease name."
        )
      )
    ),
    # Step 1: per-source status.
    bslib::card(
      bslib::card_header("1 · Data sources"),
      bslib::card_body(
        markdown(paste(
          "Per-source status for the most recent run. Each source returns",
          "genes in a common schema; failures degrade gracefully and the run",
          "continues."
        )),
        div(id = "tour_status", source_status_ui("status"))
      )
    ),
    # Step 2: ranked genes.
    bslib::card(
      bslib::card_header("2 · Ranked genes"),
      bslib::card_body(
        markdown(paste(
          "Genes ranked by weighted, source-normalized evidence with a",
          "multi-source coverage bonus. Tune the sidebar weights to re-rank",
          "instantly (no re-querying)."
        )),
        div(id = "tour_results", results_table_ui("results"))
      )
    ),
    # Step 3: optional AI curation of the final panel.
    bslib::card(
      bslib::card_header("3 · AI curation"),
      bslib::card_body(div(id = "tour_curation", curation_ui("curation")))
    ),
    # Export: a slim toolbar, not a card - download once you've built/curated.
    div(
      id = "tour_export",
      class = "d-flex justify-content-end align-items-center gap-2 pe-1",
      span(class = "text-muted small me-1", "Export:"),
      export_ui("export")
    )
  )
)
