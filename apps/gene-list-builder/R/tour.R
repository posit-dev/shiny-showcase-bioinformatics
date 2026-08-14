# Guided product tour (cicerone).
#
# A click-through demo that walks a first-time user across the Build page's key
# controls. The navbar "Demo" button (see server.R) prefills "breast cancer",
# resolves it, then starts this tour. Steps target the `tour_*` anchor ids added
# in userInterface/builder_ui.R. cicerone is loaded lazily (cicerone:: +
# cicerone::use_cicerone() in ui.R) so it adds no hard dependency at load time.

# A Cicerone guide from a list of step lists. Each step is
# list(el, title, description, position); `el` is the raw (un-#'d) element id.
.glb_guide <- function(id, steps) {
  guide <- cicerone::Cicerone$new(id = id)
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

#' The Build-page demo guide. Assumes the caller has already prefilled and
#' resolved "breast cancer" (see server.R), so the first step lands on the
#' resolved disease candidates.
glb_build_tour <- function() {
  .glb_guide(
    "glb_build_tour",
    list(
      list(
        el = "tour_disease",
        title = "1. Pick a disease",
        description = paste(
          "We've filled in <strong>breast cancer</strong> and resolved it",
          "to an ontology term (via Open Targets). Pick the best-matching",
          "candidate to seed the gene search."
        ),
        position = "right"
      ),
      list(
        el = "tour_sources",
        title = "2. Choose data sources",
        description = paste(
          "Select which gene-disease-association databases to query. Evidence",
          "sources add candidate genes; annotation sources only re-prioritize",
          "them. Leaving the defaults is a good start."
        ),
        position = "right"
      ),
      list(
        el = "tour_run",
        title = "3. Build the gene list",
        description = paste(
          "Click <strong>Build gene list</strong> to fetch genes from the",
          "selected sources, aggregate them into one row per gene, and rank",
          "them. This is the only step that hits the network."
        ),
        position = "right"
      ),
      list(
        el = "tour_weights",
        title = "4. Tune the ranking (optional)",
        description = paste(
          "Open this collapsible section to adjust how much each source",
          "counts and the multi-source bonus. Changes re-rank instantly,",
          "with no re-querying - defaults are a fine start."
        ),
        position = "right"
      ),
      list(
        el = "tour_workflow",
        title = "5. The results workflow",
        description = paste(
          "Everything flows in one scroll: per-source <em>status</em>, the",
          "<em>ranked</em> gene table, then optional <em>AI curation</em> of",
          "the final panel. No tabs - just scroll down the steps."
        ),
        position = "left"
      ),
      list(
        el = "tour_export",
        title = "6. Export",
        description = paste(
          "When you're happy with the list, download it (CSV) and a run",
          "report from the toolbar at the bottom."
        ),
        position = "top"
      ),
      list(
        el = "toggle_assistant",
        title = "Ask the assistant",
        description = paste(
          "Open the AI assistant any time to ask about the sources, the scoring,",
          "or the current ranked genes - it can read your live results."
        ),
        position = "bottom"
      )
    )
  )
}
