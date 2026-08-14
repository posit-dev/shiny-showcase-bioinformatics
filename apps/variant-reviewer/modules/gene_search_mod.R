# Gene-first search box. Returns a reactive carrying the submitted query so the
# parent can fan it out to the result modules.

# A coherent, well-supported example (BRAF V600E) that populates every card:
# the gene resolves, and the rsID drives the variant, ClinVar, gnomAD, and VEP
# lookups. Shared by the UI label and the server handler.
.gene_search_example <- list(
  label = "BRAF V600E",
  gene = "BRAF",
  variant = "rs113488022"
)

gene_search_ui <- function(id) {
  ns <- NS(id)

  card(
    card_body(
      layout_columns(
        col_widths = c(5, 4, 3),
        textInput(
          ns("gene"),
          label = "Gene symbol",
          placeholder = "e.g. TP53",
          width = "100%"
        ),
        # Variant picker: suggestions are pathogenic/likely-pathogenic variants
        # for the entered gene (populated server-side), but any rsID/HGVS can be
        # typed (create = TRUE), so it doubles as a free-text field.
        selectizeInput(
          ns("variant"),
          label = "Variant",
          choices = NULL,
          multiple = FALSE,
          width = "100%",
          options = list(
            create = TRUE,
            placeholder = "e.g. V600E, R175H, or rs113488022",
            onInitialize = I('function() { this.setValue(""); }'),
            render = I(
              "{ option_create: function(data, escape) {
                 return '<div class=\"create\">Use \"' + escape(data.input) +
                   '\"</div>'; } }"
            )
          )
        ),
        div(
          class = "d-grid align-self-end mb-1",
          actionButton(
            ns("submit"),
            label = "Review",
            icon = icon("magnifying-glass"),
            class = "btn-primary"
          )
        )
      ),
      # Either field is enough: a gene, a variant (rsID or HGVS), or both. A
      # lone variant resolves its own gene to fill the gene-level cards.
      tags$div(
        class = "small text-muted",
        "Enter a gene, a variant (rsID or HGVS), or both."
      ),
      # One-click example so a first-time visitor can see a populated dashboard
      # without knowing a gene/variant off-hand.
      tags$div(
        class = "small text-muted",
        "Not sure where to start? ",
        actionLink(
          ns("example"),
          paste0("Load an example (", .gene_search_example$label, ")")
        )
      ),
      # Count of variant suggestions loaded for the current gene.
      uiOutput(ns("variant_hint")),
      # Inline validation feedback: shown when the gene/variant fails the
      # format check, in which case no search is submitted.
      uiOutput(ns("validation"))
    )
  )
}

# Returns a reactive carrying list(gene = <chr>, variant = <chr|NULL>), or NULL
# before the first submit (and when the gene is blank). A reactiveVal is used
# instead of eventReactive so reading it before any submit yields NULL rather
# than a silent error, which lets the result cards show their initial
# placeholder messages.
#
# `requested` is an optional reactive carrying list(gene, variant, nonce) from
# outside the module (the assistant). It is handled exactly like a Review click:
# the visible inputs are filled and the same submit path runs. Callers cannot
# write the query directly, so the search box stays the only way a search
# starts.
gene_search_server <- function(id, requested = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    query <- reactiveVal(NULL)
    # Validation messages from the last submit (character vector), or NULL.
    validation <- reactiveVal(NULL)
    # Parsed variant suggestions for the current gene, or NULL.
    suggestions <- reactiveVal(NULL)

    # Update the variant selectize's choices while preserving whatever the user
    # has already typed/selected (kept as an extra option so it stays visible
    # even when it isn't among the suggestions).
    refresh_variant_choices <- function(choices = character()) {
      current <- isolate(input$variant) %||% ""
      if (nzchar(current) && !(current %in% choices)) {
        choices <- c(stats::setNames(current, current), choices)
      }
      updateSelectizeInput(
        session,
        "variant",
        choices = choices,
        selected = if (nzchar(current)) current else "",
        server = FALSE
      )
    }

    # Prefetch this gene's notable variants when the gene field settles, so the
    # variant box can suggest them. Debounced to avoid firing mid-typing, and
    # gated on the same format check used at submit so we never query on junk.
    gene_debounced <- debounce(reactive(trimws(input$gene %||% "")), 600)
    observeEvent(gene_debounced(), {
      gene <- gene_debounced()
      if (!isTRUE(vr_validate_gene(gene)$ok)) {
        suggestions(NULL)
        refresh_variant_choices()
        return()
      }
      parsed <- myvariant_gene_variants(gene)
      suggestions(parsed)
      refresh_variant_choices(myvariant_variant_choices(parsed))
    })

    output$variant_hint <- renderUI({
      parsed <- suggestions()
      if (is.null(parsed) || !isTRUE(parsed$ok)) {
        return(NULL)
      }
      n <- nrow(parsed$variants)
      tags$div(
        class = "small text-muted mt-1",
        sprintf(
          "%d known pathogenic/likely-pathogenic variant%s for %s. Type to filter, or enter any rsID/HGVS.",
          n,
          if (n == 1) "" else "s",
          trimws(input$gene %||% "")
        )
      )
    })

    # The one place a query is published. Gates every downstream API call on a
    # format-level check of the inputs, so a malformed gene/variant is caught
    # here rather than firing failing lookups across the cards.
    submit_query <- function(gene, variant) {
      check <- vr_validate_query(gene, if (variant == "") NULL else variant)
      if (!isTRUE(check$ok)) {
        validation(check$errors)
        query(NULL)
        return(invisible(FALSE))
      }
      validation(NULL)
      query(list(
        gene = gene,
        variant = if (variant == "") NULL else variant
      ))
      invisible(TRUE)
    }

    observeEvent(input$submit, {
      submit_query(trimws(input$gene %||% ""), trimws(input$variant %||% ""))
    })

    # An outside request (the assistant) fills the search box and then goes
    # through the same submit as a Review click, so it gets the same validation
    # and the user can see what was searched. `nonce` makes a repeat of the same
    # gene/variant a fresh event.
    observeEvent(requested(), {
      req <- requested()
      if (is.null(req)) {
        return()
      }
      gene <- trimws(as.character(req$gene %||% ""))
      variant <- trimws(as.character(req$variant %||% ""))
      updateTextInput(session, "gene", value = gene)
      updateSelectizeInput(
        session,
        "variant",
        choices = if (nzchar(variant)) {
          stats::setNames(variant, variant)
        } else {
          character()
        },
        selected = variant,
        server = FALSE
      )
      submit_query(gene, variant)
    })

    output$validation <- renderUI({
      msgs <- validation()
      if (is.null(msgs)) {
        return(NULL)
      }
      div(
        class = "alert alert-warning py-2 px-3 small mt-2 mb-0",
        role = "alert",
        lapply(msgs, tags$div)
      )
    })

    # Fill the inputs with the example values but leave submitting to the user,
    # so they can review or tweak the gene/variant before clicking Review. The
    # gene change also triggers the suggestion prefetch above.
    observeEvent(input$example, {
      updateTextInput(session, "gene", value = .gene_search_example$gene)
      updateSelectizeInput(
        session,
        "variant",
        choices = stats::setNames(
          .gene_search_example$variant,
          .gene_search_example$variant
        ),
        selected = .gene_search_example$variant,
        server = FALSE
      )
    })

    query
  })
}
