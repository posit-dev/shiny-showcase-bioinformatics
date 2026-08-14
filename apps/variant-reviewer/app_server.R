# Shiny Server
function(input, output, session) {
  # Search requested by the assistant. The search module fills its inputs from
  # this and runs its own submit, so the assistant drives the search box rather
  # than writing the query (or any card) itself.
  assistant_search <- reactiveVal(NULL)

  # Submitted search query: reactive(list(gene, variant)) or NULL.
  search <- gene_search_server("search", requested = assistant_search)

  # Raw MyVariant annotation for the entered variant. Shared so the fetch runs
  # once; the card-facing variant_annotation below gates it on the gene and
  # variant being consistent.
  #
  # annotation_retry: the Variant card has no fetch of its own (it just
  # renders variant_annotation directly), so its refresh button bumps this to
  # retry the fetch below. See vr_retry_counter() in R/source_links.R. The
  # protein/domains/structure/landscape cards also read variant_annotation
  # (for the residue position), so a retry here recomputes those too --
  # harmless (a card whose own fetch already succeeded just re-reads its own
  # cached result) and usually exactly what you want, since a failed
  # MyVariant fetch would have been the reason they were empty too.
  annotation_retry <- vr_retry_counter()
  annotation_raw <- reactive({
    annotation_retry$dep()
    query <- search()
    if (is.null(query) || is_blank(query$variant)) {
      return(NULL)
    }
    myvariant_annotate(query$variant)
  })

  # Work out which gene the dashboard is about and whether the inputs conflict.
  # A search can be gene-only, variant-only, or both:
  #   * gene present            -> that is the gene.
  #   * variant only            -> the gene the variant belongs to (from
  #                                MyVariant), so a lone variant still fills the
  #                                gene-level cards.
  #   * both, naming different genes -> a mismatch, which blocks the search.
  gene_context <- reactive({
    query <- search()
    if (is.null(query)) {
      return(NULL)
    }
    has_gene <- !is_blank(query$gene)
    has_variant <- !is_blank(query$variant)
    ann <- if (has_variant) annotation_raw() else NULL
    variant_gene <- if (isTRUE(ann$ok)) ann$gene else NULL
    mismatch <- has_gene &&
      has_variant &&
      !is_blank(variant_gene) &&
      !identical(toupper(trimws(query$gene)), toupper(trimws(variant_gene)))
    list(
      has_gene = has_gene,
      has_variant = has_variant,
      variant_gene = variant_gene,
      effective_gene = if (has_gene) query$gene else variant_gene,
      mismatch = mismatch
    )
  })

  ok_context <- function() {
    ctx <- gene_context()
    if (is.null(ctx) || isTRUE(ctx$mismatch)) NULL else ctx
  }

  # The query the result cards see: NULL on a mismatch, so nothing loads a mixed
  # gene/variant result. Passed to the variant-level modules in place of search.
  search_effective <- reactive({
    if (is.null(ok_context())) NULL else search()
  })

  # Gene identifiers for the effective gene (typed, or the variant's own gene).
  #
  # resolved_retry: the Gene card has no fetch of its own (it just renders
  # resolved directly), so its refresh button bumps this to retry the fetch
  # below. See vr_retry_counter() in R/source_links.R. Every gene-scoped card
  # reads resolved(), so a retry here recomputes those too -- harmless (a
  # card whose own fetch already succeeded just re-reads its own cached
  # result) and usually exactly what you want, since a failed MyGene lookup
  # would have been the reason they were empty too.
  resolved_retry <- vr_retry_counter()
  resolved <- reactive({
    resolved_retry$dep()
    ctx <- ok_context()
    if (is.null(ctx) || is_blank(ctx$effective_gene)) {
      return(NULL)
    }
    mygene_resolve(ctx$effective_gene)
  })

  # Variant annotation for the variant cards. NULL on a mismatch.
  variant_annotation <- reactive({
    if (is.null(ok_context())) NULL else annotation_raw()
  })

  # The dbSNP rsID drives the gnomAD and ClinVar lookups: use the input directly
  # when it is an rsID, otherwise the one MyVariant resolved.
  variant_rsid <- reactive({
    ctx <- ok_context()
    if (is.null(ctx) || !ctx$has_variant) {
      return(NULL)
    }
    variant <- trimws(search()$variant)
    if (grepl("^rs[0-9]+$", variant, ignore.case = TRUE)) {
      return(tolower(variant))
    }
    annotation <- annotation_raw()
    if (isTRUE(annotation$ok) && !is_blank(annotation$rsid)) {
      return(annotation$rsid)
    }
    NULL
  })

  # Block, rather than just warn, when a typed gene and the variant disagree: the
  # cards stay empty and this explains why. A lone variant is never a mismatch
  # (its gene is used), so this only fires when both were entered.
  output$search_notice <- renderUI({
    ctx <- gene_context()
    if (is.null(ctx) || !isTRUE(ctx$mismatch)) {
      return(NULL)
    }
    query <- search()
    vr_error(sprintf(
      paste(
        "Variant %s is in %s, not %s.",
        "Clear the gene to review the variant on its own,",
        "or enter a variant that is in %s."
      ),
      query$variant,
      ctx$variant_gene,
      query$gene,
      query$gene
    ))
  })

  # Each result module returns its data reactive so the assistant can read what
  # each card shows (gene_summary/variant_summary just render the shared
  # resolved/annotation reactives, so those are reused directly).
  gene_summary_server("gene_summary", resolved, resolved_retry$bump)
  variant_summary_server(
    "variant_summary",
    variant_annotation,
    annotation_retry$bump
  )
  predictions_data <- predictions_server("predictions", search_effective)
  protein_data <- protein_summary_server(
    "protein_summary",
    resolved,
    search_effective,
    variant_annotation
  )
  domains_data <- protein_domains_server(
    "domains",
    resolved,
    search_effective,
    variant_annotation
  )
  structure_data <- protein_structure_server(
    "structure",
    resolved,
    search_effective,
    variant_annotation
  )
  clinvar_data <- clinvar_server("clinvar", variant_rsid)
  # gnomad_server() also returns its retry-bump function, so the ancestry card
  # below -- which renders this same result rather than fetching its own --
  # can wire its own refresh button to retry it too.
  gnomad_result <- gnomad_server("gnomad", variant_rsid)
  gnomad_data <- gnomad_result$data
  constraint_data <- gene_constraint_server("constraint", resolved)
  ensembl_data <- ensembl_server("ensembl", variant_rsid)
  gtex_data <- gtex_expression_server("gtex", resolved)
  string_data <- string_ppi_server("string_ppi", resolved)
  opentargets_data <- opentargets_server("opentargets", resolved)
  phenotypes_data <- phenotypes_server("phenotypes", resolved)
  drugs_data <- drugs_server("drugs", resolved)
  pharmacogenomics_data <- pharmacogenomics_server(
    "pharmacogenomics",
    resolved,
    variant_rsid
  )
  literature_data <- literature_server("literature", resolved, variant_rsid)
  external_links_server("links", resolved)
  # Visualization cards. The ancestry card reuses the shared gnomAD result (no
  # extra fetch); the gene model reuses the VEP result for the variant position.
  landscape_data <- variant_landscape_server(
    "landscape",
    resolved,
    search_effective,
    variant_annotation
  )
  conservation_data <- conservation_server("conservation", variant_rsid)
  genemodel_data <- gene_model_server("genemodel", resolved, gnomad_data)
  gnomad_ancestry_server("gnomad_ancestry", gnomad_data, gnomad_result$retry)

  # --- AI assistant ---------------------------------------------------------
  # Mirror each card's current data into a plain (non-reactive) store so the
  # assistant's tools can read it during async streaming, which runs outside any
  # reactive context. One observer per card keeps its slot in step; reading the
  # reactives here also means the cards populate on search whether or not they
  # are currently visible.
  dash <- new.env(parent = emptyenv())
  dash$selection <- "Nothing is loaded yet."
  observe({
    query <- search()
    ctx <- gene_context()
    dash$selection <- if (is.null(query)) {
      "Nothing is loaded yet."
    } else if (isTRUE(ctx$mismatch)) {
      paste0(
        "Nothing loaded: gene ",
        query$gene,
        " and variant ",
        query$variant,
        " (in ",
        ctx$variant_gene,
        ") do not match."
      )
    } else {
      gene <- ctx$effective_gene
      paste0(
        "Gene: ",
        if (is_blank(gene)) "unknown" else gene,
        if (!is_blank(query$variant)) {
          paste0("; Variant: ", query$variant)
        } else {
          "; no variant"
        }
      )
    }
  })
  # Every source in the dashboard, in the order they are fetched. `variant` marks
  # the ones that need a variant as well as a gene -- a gene-only search skips
  # them, so they are left out of the progress total rather than counted and
  # never reached. `label` is what the progress popup names as it works through
  # them.
  sources <- list(
    list(id = "gene", label = "MyGene", variant = FALSE, get = resolved),
    list(
      id = "constraint",
      label = "gnomAD constraint",
      variant = FALSE,
      get = constraint_data
    ),
    list(
      id = "landscape",
      label = "ClinVar landscape",
      variant = FALSE,
      get = landscape_data
    ),
    list(
      id = "domains",
      label = "UniProt domains",
      variant = FALSE,
      get = domains_data
    ),
    list(
      id = "structure",
      label = "AlphaFold structure",
      variant = FALSE,
      get = structure_data
    ),
    list(
      id = "expression",
      label = "GTEx expression",
      variant = FALSE,
      get = gtex_data
    ),
    list(
      id = "interactions",
      label = "STRING interactions",
      variant = FALSE,
      get = string_data
    ),
    list(
      id = "diseases",
      label = "Open Targets diseases",
      variant = FALSE,
      get = opentargets_data
    ),
    list(
      id = "phenotypes",
      label = "HPO phenotypes",
      variant = FALSE,
      get = phenotypes_data
    ),
    list(
      id = "drugs",
      label = "Open Targets drugs",
      variant = FALSE,
      get = drugs_data
    ),
    list(
      id = "pharmacogenomics",
      label = "Open Targets pharmacogenomics",
      variant = FALSE,
      get = pharmacogenomics_data
    ),
    list(
      id = "literature",
      label = "Europe PMC literature",
      variant = FALSE,
      get = literature_data
    ),
    list(
      id = "variant",
      label = "MyVariant annotation",
      variant = TRUE,
      get = variant_annotation
    ),
    list(
      id = "predictions",
      label = "In-silico predictions",
      variant = TRUE,
      get = predictions_data
    ),
    list(
      id = "protein",
      label = "ProtVar protein context",
      variant = TRUE,
      get = protein_data
    ),
    list(
      id = "clinvar",
      label = "ClinVar significance",
      variant = TRUE,
      get = clinvar_data
    ),
    list(
      id = "gnomad",
      label = "gnomAD frequency",
      variant = TRUE,
      get = gnomad_data
    ),
    list(
      id = "consequences",
      label = "Ensembl VEP",
      variant = TRUE,
      get = ensembl_data
    ),
    list(
      id = "conservation",
      label = "Conservation scores",
      variant = TRUE,
      get = conservation_data
    ),
    # Reads the gnomAD result for the variant position, so it comes last.
    list(
      id = "genemodel",
      label = "Ensembl gene model",
      variant = FALSE,
      get = genemodel_data
    )
  )

  # Fetching is synchronous, so the R process is busy for the whole search and
  # cannot flush outputs until it is done -- which is why the cards arrive in a
  # couple of clumps rather than one by one. shiny::Progress is the exception:
  # it writes straight to the websocket instead of waiting for the flush cycle,
  # so the count below advances while the fetches are still running.
  #
  # This has to be ONE observer rather than one per source. Whichever consumer
  # touches a reactive first is the one that pays for its fetch, so if the card
  # outputs got there first they would do all the waiting and this loop would
  # only ever report an instant 16/16. Running it as a single high-priority
  # observer makes it the first thing to touch them, which is what keeps the
  # order, and therefore the count, honest.
  # Which card each source feeds. Unticking a card in the Cards popover hides it
  # client-side, which suspends its outputs so they never compute -- the only
  # reason a hidden card still cost an API call was this loop forcing its
  # reactive. Skipping it here is what turns the toggle into "don't fetch"
  # rather than "fetch and don't show".
  source_card <- c(
    gene = "gene_summary",
    constraint = "constraint",
    landscape = "landscape",
    domains = "domains",
    structure = "structure",
    expression = "gtex",
    interactions = "string_ppi",
    diseases = "opentargets",
    phenotypes = "phenotypes",
    drugs = "drugs",
    pharmacogenomics = "pharmacogenomics",
    literature = "literature",
    variant = "variant_summary",
    predictions = "predictions",
    protein = "protein_summary",
    clinvar = "clinvar",
    gnomad = "gnomad",
    consequences = "ensembl",
    conservation = "conservation",
    genemodel = "genemodel"
  )

  # Whether `src` is currently fetched: its card is on screen and the query
  # can actually answer it (variant-level sources need a variant). A plain
  # function rather than a reactive, so each caller below chooses whether
  # reading it should count as a dependency (the search-driven walk) or not
  # (the per-source retry mirror, via isolate() -- see its comment).
  applies <- function(src) {
    query <- search_effective()
    shown <- input$visible_cards %||% names(.dashboard_cards)
    has_variant <- !is.null(query) && !is_blank(query$variant)
    (!src$variant || has_variant) && source_card[[src$id]] %in% shown
  }

  observe(
    {
      query <- search_effective()
      total <- length(Filter(applies, sources))

      bar <- NULL
      if (!is.null(query) && total > 0) {
        bar <- Progress$new(session, min = 0, max = total)
        on.exit(bar$close(), add = TRUE)
      }

      done <- 0L
      for (src in sources) {
        if (!applies(src)) {
          # Leave no stale value behind for the assistant to read as current.
          dash[[src$id]] <- NULL
          next
        }
        if (!is.null(bar)) {
          bar$set(
            value = done,
            message = sprintf("Loading sources (%d of %d)", done, total),
            detail = src$label
          )
        }
        # Forcing the reactive here is what triggers the fetch; the card
        # outputs then render from the cached value. isolate()d so this
        # observer's only real dependencies stay search_effective() and
        # input$visible_cards: a single card's retry button must not restart
        # this whole ordered walk (and its progress popup) for every source --
        # see the per-source mirror below, which is what keeps `dash` in sync
        # for a retry instead.
        dash[[src$id]] <- isolate(src$get())
        done <- done + 1L
      }

      if (!is.null(bar)) {
        bar$set(
          value = total,
          message = sprintf("Loaded %d of %d sources", total, total),
          detail = "Done"
        )
      }
    },
    priority = 100
  )

  # Re-mirror one source into `dash` when its own reactive changes for a
  # reason other than the walk above -- in practice, a card's own retry
  # button. Each observer depends on nothing but that one source's reactive,
  # so a retry updates only that source's dash entry, without re-running the
  # ordered walk/progress popup for every card (default priority: it runs
  # after the walk above during an actual new search, by which point the
  # value is already computed, so this just re-reads the cached result rather
  # than racing it for who "pays" for the fetch).
  for (.src in sources) {
    local({
      src <- .src
      observe({
        value <- src$get()
        if (isolate(applies(src))) {
          dash[[src$id]] <- value
        }
      })
    })
  }

  # The assistant's only way to change the dashboard: type a gene/variant into
  # the search box and click Review. It hands the request to the search module,
  # which fills its inputs and submits; the cards then update as they would for
  # any search. The assistant never writes card state and never fetches directly.
  search_counter <- reactiveVal(0L)
  load_selection <- function(gene = NULL, variant = NULL) {
    gene <- trimws(as.character(gene %||% ""))
    variant <- trimws(as.character(variant %||% ""))
    if (!nzchar(gene) && !nzchar(variant)) {
      return("No gene or variant provided; nothing was searched.")
    }
    # Same format-level gate the search box uses, so the assistant can't fire
    # lookups on a malformed identifier either.
    check <- vr_validate_query(
      if (nzchar(gene)) gene else NULL,
      if (nzchar(variant)) variant else NULL
    )
    if (!isTRUE(check$ok)) {
      return(paste0(
        "Nothing was searched — invalid input: ",
        paste(check$errors, collapse = " ")
      ))
    }
    search_counter(search_counter() + 1L)
    assistant_search(list(
      gene = gene,
      variant = if (nzchar(variant)) variant else NULL,
      nonce = search_counter()
    ))
    paste0(
      "Searched ",
      gene,
      if (nzchar(variant)) paste0(" / ", variant) else "",
      " in the search box. The cards are refreshing; read them (read_card) to",
      " see the results."
    )
  }

  # Demo button (navbar): a guided walkthrough. Fill the search inputs with a
  # worked example, "click" Review a beat later so the fill is visible before the
  # cards load, then start the cicerone tour that steps through each card and
  # ends on the assistant. A card's tour anchor only exists while it is shown, so
  # re-tick the default-on cards first; the tour skips the default-off ones to
  # match. Falls back to a modal when cicerone is absent.
  demo_guide <- vr_demo_tour()
  observeEvent(input$demo, {
    ex <- .gene_search_example
    # Re-seed the demo snapshot in case the cache has been running long enough
    # for the TTL or LRU to have dropped it, so clicking Review stays instant.
    vr_seed_demo_cache()
    updateCheckboxGroupInput(
      session,
      "visible_cards",
      selected = .dashboard_cards_default_on
    )
    updateTextInput(session, "search-gene", value = ex$gene)
    updateSelectizeInput(
      session,
      "search-variant",
      choices = stats::setNames(ex$variant, ex$variant),
      selected = ex$variant,
      server = FALSE
    )
    # Fill the search inputs but do NOT submit: the walkthrough asks the user to
    # click Review themselves. Its first step highlights the search box, and
    # driver.js keeps the highlighted element interactive, so Review is clickable
    # from within the tour; the cards then load on the user's own click and
    # populate as they step through. (search() is deliberately not called here.)
    if (is.null(demo_guide)) {
      showModal(vr_demo_modal(ex$label))
    } else {
      demo_guide$init(session)$start(session = session)
    }
  })

  # Assistant tools, scoped to this app: read the loaded selection, read any
  # card's data, and load a gene/variant. Guarded on ellmer being installed;
  # the chat module itself degrades gracefully when it is not.
  chat_tools <- list()
  if (requireNamespace("ellmer", quietly = TRUE)) {
    chat_tools <- list(
      ellmer::tool(
        function() dash$selection,
        paste(
          "Report the gene and variant currently loaded in the dashboard —",
          "what the user is reviewing."
        ),
        name = "get_current_selection"
      ),
      ellmer::tool(
        function(card) vr_chat_card_text(card, dash[[card]]),
        paste(
          "Read the information currently shown in one dashboard card for the",
          "loaded gene/variant. Returns a text summary of that card's data."
        ),
        arguments = list(
          card = ellmer::type_enum(
            paste0(
              "Which card to read. One of: ",
              paste(names(VR_CHAT_CARDS), collapse = ", "),
              "."
            ),
            values = names(VR_CHAT_CARDS)
          )
        ),
        name = "read_card"
      ),
      ellmer::tool(
        function(gene = NULL, variant = NULL) load_selection(gene, variant),
        paste(
          "Type a human gene and/or variant into the app's search box and click",
          "Review, exactly as the user would. Provide a gene, a variant, or",
          "both; a variant on its own resolves its own gene. The app runs its",
          "own lookups and the cards refresh on their own. This is the only way",
          "you can change the dashboard; you cannot write to a card. Do not",
          "perform your own external searches; always search here and read the",
          "cards."
        ),
        arguments = list(
          gene = ellmer::type_string(
            "Human gene symbol, e.g. TP53 or BRAF. Omit to search by variant.",
            required = FALSE
          ),
          variant = ellmer::type_string(
            "An rsID (rs...) or HGVS string. Omit for a gene-only search.",
            required = FALSE
          )
        ),
        name = "set_selection"
      )
    )
  }

  byok_chat_server(
    "chat",
    system_prompt = paste(
      "You are a genomics assistant embedded in Variant Reviewer, a gene and",
      "variant interpretation dashboard.",
      "",
      "SCOPE — you help with human gene and variant interpretation:",
      "clinical significance, molecular mechanism, population frequency,",
      "functional impact, gene biology, and explaining the annotations shown in",
      "the app.",
      "",
      "TOOLS — work through the app, never through your own external lookups:",
      "use get_current_selection to see what gene/variant is loaded; use",
      "read_card to read what a specific card shows (gene, variant, protein,",
      "clinvar, gnomad, consequences, expression, interactions, diseases); and",
      "use set_selection to type a gene/variant into the search box and click",
      "Review for the user. Reading is unrestricted; searching is the ONLY",
      "change you can make. You cannot write to a card or edit what one shows —",
      "the app fills the cards itself from the search. After set_selection the",
      "cards refresh asynchronously, so read them again (read_card) on the next",
      "exchange to report results.",
      "",
      "OUT OF SCOPE — politely decline and steer back on topic if asked for",
      "anything unrelated to genomics or variant interpretation. You do not",
      "provide medical diagnosis, treatment, or personal genetic-counseling",
      "advice; for those, direct the user to a qualified clinician or genetic",
      "counselor.",
      "",
      "STYLE — be concise, flag uncertainty, and never fabricate identifiers,",
      "statistics, or citations; say when you don't know."
    ),
    tools = chat_tools,
    # Example prompts: fill-to-edit chips at the input and suggestion cards in
    # the connected greeting (shared constant, see R/chat_tools.R).
    suggestions = VR_CHAT_SUGGESTIONS
  )
}
