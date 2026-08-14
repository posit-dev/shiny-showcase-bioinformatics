# Gene explorer module.
#
# Search the measured features (gene metadata: symbol, ensembl_id, token_id) so
# the headline "62,710 genes" is actionable: check whether specific genes were
# quantified before planning a targeted reanalysis, and browse/export the table.
# Reads only the small gene table, so it is fast.

# Upper-cased gene symbols for case-insensitive presence checks.
.gene_symbols_upper <- function(df) {
  if (!"gene_symbol" %in% names(df)) {
    return(character())
  }
  toupper(as.character(df$gene_symbol))
}

# Split a free-text gene list (comma / semicolon / whitespace separated) into
# upper-cased, de-duplicated, non-empty tokens.
.gene_parse_query <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character())
  }
  toks <- unlist(strsplit(text, "[,;[:space:]]+"))
  toks <- toupper(trimws(toks))
  unique(toks[nzchar(toks)])
}

gene_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Gene lookup",
      width = 320,
      # tour_* ids anchor the guided demo (see R/tour.R).
      div(
        id = ns("tour_lookup"),
        textAreaInput(
          ns("query"),
          "Check genes (symbols)",
          rows = 4,
          placeholder = "e.g. TP53, EGFR, KRAS"
        ),
        div(
          class = "text-muted small",
          "Paste gene symbols (comma or space separated) to check which were",
          "measured. Leave empty to browse the full table."
        )
      ),
      tags$hr(),
      actionButton(
        ns("clear"),
        "Clear",
        class = "btn-sm btn-outline-secondary"
      )
    ),
    div(id = ns("tour_summary"), uiOutput(ns("summary"))),
    uiOutput(ns("lookup")),
    bslib::card(
      id = ns("tour_table"),
      bslib::card_header("Genes"),
      subset_export_ui(ns("export")),
      tags$hr(),
      reactable::reactableOutput(ns("table"))
    )
  )
}

gene_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    genes <- reactive(tahoe_gene())

    query <- reactive(.gene_parse_query(input$query))

    observeEvent(input$clear, {
      updateTextAreaInput(session, "query", value = "")
    })

    # Present vs missing symbols for the current query (NULL when no query).
    lookup <- reactive({
      q <- query()
      if (length(q) == 0) {
        return(NULL)
      }
      sym <- .gene_symbols_upper(genes())
      list(found = q[q %in% sym], missing = q[!q %in% sym])
    })

    # Table contents: the full gene table, or just the queried genes present.
    shown <- reactive({
      g <- genes()
      q <- query()
      if (length(q) == 0 || !"gene_symbol" %in% names(g)) {
        return(g)
      }
      g[toupper(as.character(g$gene_symbol)) %in% q, , drop = FALSE]
    })

    output$summary <- renderUI({
      n <- nrow(genes())
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          "Genes measured",
          format(n, big.mark = ","),
          theme = "primary"
        )
      )
    })

    output$lookup <- renderUI({
      res <- lookup()
      if (is.null(res)) {
        return(NULL)
      }
      badges <- function(x, cls) {
        lapply(x, function(g) span(class = paste("badge me-1 mb-1", cls), g))
      }
      bslib::card(
        bslib::card_header(sprintf(
          "%d of %d queried genes measured",
          length(res$found),
          length(res$found) + length(res$missing)
        )),
        if (length(res$found) > 0) {
          div(
            class = "mb-2",
            tags$strong("Present: "),
            badges(res$found, "text-bg-success")
          )
        },
        if (length(res$missing) > 0) {
          div(
            tags$strong("Not found: "),
            badges(res$missing, "text-bg-secondary")
          )
        }
      )
    })

    output$table <- reactable::renderReactable({
      df <- shown()
      validate(need(nrow(df) > 0, "No genes match the current lookup."))
      tahoe_reactable(df)
    })

    subset_export_server(
      "export",
      data_reactive = shown,
      file_stem = "genes_subset"
    )

    # Expose the shown reactive for callers / tests.
    shown
  })
}
