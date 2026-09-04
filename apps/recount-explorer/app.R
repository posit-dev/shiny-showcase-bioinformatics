# Recount Explorer: browse the recount3 catalog of uniformly processed RNA-seq
# studies and visualize any of them without writing code.
#
# App layer: page layout and module wiring only. Computation lives in
# R/logic_*.R, which is Shiny-free and testable on its own. Each view is a
# module in R/mod_*.R. The single data contract between modules is the `study`
# reactive the browser module returns:
# list(project, organism, source, rse, log_expr), NULL until a study loads.
#
# The interface is bslib. An R contributor can change any part of it without a
# JavaScript toolchain, which is the whole reason it is bslib.

library(shiny)
library(bslib)
library(ggplot2)

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

# Study loading runs on mirai daemons through ExtendedTask, so a slow download
# never blocks this process. Two daemons: two studies can load at once across
# sessions, and further requests queue.
mirai::daemons(2)

# Prefetching gets its own pool with a single worker, so warming a study the
# user is only reading about can never take a slot from a study they asked for.
mirai::daemons(1, .compute = "prefetch")

onStop(function() {
  mirai::daemons(0)
  mirai::daemons(0, .compute = "prefetch")
})

# The gene annotation is shared by every study of one organism and is 1.8 MB.
# Fetching both once at startup takes a step off every first study load. Fire
# and forget on the prefetch worker, so it never delays the app coming up.
local({
  m <- mirai::mirai(
    prefetch_annotations(),
    prefetch_annotations = prefetch_annotations,
    .compute = "prefetch"
  )
  # Held only so it is not collected before it resolves.
  assign(".annotation_warmup", m, envir = globalenv())
})

# Read the catalog and warm the search text once at startup rather than on the
# first keystroke. Building the search text costs about half a second.
local({
  catalog <- read_catalog()
  if (is.null(catalog)) {
    warning(
      "No catalog snapshot at ",
      catalog_snapshot_path(),
      ". Run: Rscript data-raw/build_catalog.R",
      call. = FALSE
    )
  } else {
    invisible(catalog_haystack(catalog))
    message("Recount Explorer: ", catalog_summary_line(catalog))
  }
})

# The palette lives in _brand.yml, which bslib finds on its own. Keeping it
# there rather than in this call means the ggplot2 figures can read the same
# file, so the interface and the plots cannot drift apart. brand = TRUE rather
# than the default NULL, so a missing or broken brand file fails loudly here
# instead of quietly falling back to stock Bootstrap blue.
#
# Fonts stay on the system stack. Google Fonts would add two requests to a
# third party on every page load and leave the app looking wrong with no
# outbound network.
app_theme <- bs_theme(version = 5, brand = TRUE)

ui <- page_navbar(
  title = "Recount Explorer",
  id = "nav",
  theme = app_theme,
  fillable = "Browse",
  # A fixed coffee-dark bar, independent of the page's own light or dark
  # mode. Without this the navbar just took the body background, which read
  # as one flat block with the rest of the page.
  navbar_options = navbar_options(bg = "var(--brand-roast)", theme = "dark"),
  header = tagList(
    # Spinners on every recalculating output and a pulse on the page, without
    # wiring a single one by hand.
    useBusyIndicators(),
    busyIndicatorOptions(spinner_type = "dots", spinner_delay = "0.2s"),
    tags$head(
      # driver.js, vendored in www/ so the tour needs no network call. See
      # www/driver.js for the version and where to get an update. Loaded
      # before app.css, so app.css's popover overrides win the cascade at
      # equal specificity instead of driver.css's own white/black defaults.
      tags$link(rel = "stylesheet", href = "driver.css"),
      tags$script(src = "driver.js"),
      tags$script(src = "tour.js"),
      tags$link(rel = "stylesheet", href = "app.css")
    )
  ),
  nav_panel("Browse", study_browser_ui("browser")),
  nav_panel("Overview", study_overview_ui("overview")),
  nav_panel("Quality", quality_ui("quality")),
  nav_panel("Genes", gene_explorer_ui("gene")),
  nav_panel("PCA", pca_explorer_ui("pca")),
  nav_panel("Export", export_ui("export")),
  nav_panel("About", about_ui()),
  nav_spacer(),
  nav_item(uiOutput("study_chip", inline = TRUE)),
  nav_item(
    tags$button(
      id = "take_tour",
      type = "button",
      # outline-light, not outline-secondary: secondary stays a fixed brown
      # that barely shows up against the navbar's own coffee background.
      class = "btn btn-sm btn-outline-light",
      bsicons::bs_icon("compass"),
      "Take a tour"
    )
  ),
  nav_item(input_dark_mode(id = "dark_mode")),
  footer = div(
    class = "app-footer",
    div(
      class = "app-footer-row",
      span(
        "Built by ",
        tags$a(
          href = "https://www.samuelbharti.com",
          target = "_blank",
          rel = "noopener",
          "Samuel Bharti"
        ),
        " · ",
        tags$a(
          href = "https://orcid.org/0000-0003-4190-7058",
          target = "_blank",
          rel = "noopener",
          "ORCID"
        ),
        " · ",
        tags$a(
          href = "https://github.com/samuelbharti/recount-explorer",
          target = "_blank",
          rel = "noopener",
          "Source"
        ),
        " · MIT licence"
      ),
      span(
        "Data from the ",
        tags$a(
          href = "https://rna.recount.bio/",
          target = "_blank",
          rel = "noopener",
          "recount3"
        ),
        " project. Cite Wilks et al. 2021, Genome Biology 22:323."
      )
    )
  )
)

server <- function(input, output, session) {
  # recount3_installed(), not recount3_available(): the second one loads the
  # package, and nothing on this path needs it loaded.
  if (!recount3_installed()) {
    showNotification(
      paste(
        "The recount3 package is not installed.",
        "Run BiocManager::install(\"recount3\") and restart the app."
      ),
      type = "error",
      duration = NULL
    )
  }

  # ggplot2 draws on a white canvas whatever the page looks like, so a plot in
  # dark mode glares. The builders in logic_plots.R take the mode and colour
  # themselves. The PDF downloads keep the light theme, since a figure going
  # into a document should not carry a dark background.
  dark <- reactive(identical(input$dark_mode, "dark"))

  study <- study_browser_server("browser")
  study_overview_server("overview", study, dark)
  quality_state <- quality_server("quality", study, dark)
  gene_state <- gene_explorer_server("gene", study, dark)
  pca_state <- pca_explorer_server("pca", study, dark)
  export_server("export", study, gene_state, pca_state, quality_state)

  # The loaded study follows the user across every view, so it belongs in the
  # navbar rather than on one page.
  output$study_chip <- renderUI({
    s <- study()
    if (is.null(s)) {
      return(span(class = "study-chip study-chip-empty", "No study loaded"))
    }
    span(
      class = "study-chip",
      bsicons::bs_icon("database-check"),
      tags$strong(s$project),
      sprintf("%s samples", format(ncol(s$rse), big.mark = ","))
    )
  })
}

shinyApp(ui, server)
