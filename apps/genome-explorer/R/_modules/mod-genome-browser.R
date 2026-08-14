# Genome browser Shiny module (classic). Canonical home of the genome-browser
# module; full apps reuse it to show variants/features in genomic context.
#
# A thin wrapper around igv.js (via igvShiny) -- the value is the embedding + the
# Shiny wiring, not the rendering (igv.js is best-in-class). All arguments after
# `id` are reactives, so it composes cleanly: pass a mutation track + a locus to
# navigate to.
#
# Note on namespacing: igvShiny targets the widget by its DOM id, so inside a
# module the session-based calls must use `session$ns("igv")`, not "igv".

genome_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  igvShiny::igvShinyOutput(ns("igv"), height = "340px")
}

# mutations: reactive data.frame in the mutation-data contract.
# locus: reactive gene symbol / locus string to navigate to (NULL = stay).
# genome: reactive genome name (default hg19, matching the mutation coordinates).
# track_name / track_color: the mutation track's label and colour.
genome_browser_server <- function(
  id,
  mutations,
  locus = shiny::reactive(NULL),
  genome = shiny::reactive("hg19"),
  track_name = "BRCA mutations",
  track_color = "#EE6331"
) {
  shiny::moduleServer(id, function(input, output, session) {
    igv_id <- session$ns("igv")

    output$igv <- igvShiny::renderIgvShiny({
      igvShiny::igvShiny(
        igvShiny::parseAndValidateGenomeSpec(
          genomeName = genome(),
          initialLocus = "PIK3CA"
        )
      )
    })

    # Load (or refresh) the mutation track. Sent as a custom message to the
    # widget; igvShiny writes the BED to its served tracks dir.
    shiny::observe({
      igvShiny::loadBedTrack(
        session,
        id = igv_id,
        trackName = track_name,
        tbl = mutations_to_bed(mutations()),
        color = track_color
      )
    })

    # Navigate to a gene / locus when the caller changes it.
    shiny::observeEvent(
      locus(),
      {
        shiny::req(nzchar(locus()))
        igvShiny::showGenomicRegion(session, id = igv_id, region = locus())
      },
      ignoreInit = TRUE
    )
  })
}
