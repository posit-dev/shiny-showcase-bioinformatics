# Input module: collect gene lists (paste + optional upload), a free-text study
# description, and per-source ranking weights. Exposes them as reactives. The
# weight sliders re-rank the cached result live (no re-query, because the
# normalization is absolute); the description is stored for the later AI step.

# The input UI is split into placement-agnostic component cards so the Review page
# can arrange them in a fluidPage grid (candidate genes; study context + run) with
# the secondary controls (data sources, advanced weights, API key) in the sidebar.
# Every widget id is namespaced under the same `input` module, so input_server()
# reads them regardless of where each card is rendered on the page.

# Row 1: the candidate-gene sources (paste / upload / extra tagged sources), each
# with a per-list weight (how much its genes count toward cross-source
# corroboration - see the info icon).
candidate_genes_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "mb-3 gs-card-genes",
    bslib::card_header("Candidate genes"),
    bslib::card_body(
      textAreaInput(
        ns("paste"),
        NULL,
        rows = 6,
        placeholder = "Paste gene symbols, one per line\nNF1\nSUZ12\nCDKN2A"
      ),
      div(
        class = "gs-weight",
        tags$label(`for` = ns("w_pasted"), "List weight"),
        numericInput(
          ns("w_pasted"),
          label = NULL,
          value = 1,
          min = 0,
          max = 5,
          step = 0.5,
          width = "84px"
        ),
        gs_info(
          tags$p(
            tags$b("Per-list weight. "),
            "How much a gene's appearance in this list counts toward",
            "cross-source corroboration."
          ),
          tags$p(
            "Raise it for an assay you trust more, lower it for a noisier one.",
            "It only affects ranking when you provide two or more lists, and a",
            "high weight can never manufacture support from a single list."
          )
        )
      ),
      div(
        class = "d-flex gap-2 align-items-center flex-wrap",
        actionButton(
          ns("load_example"),
          "Load NF1 example",
          class = "btn-outline-secondary btn-sm"
        ),
        actionButton(
          ns("load_multisource"),
          "Load 4-source example",
          class = "btn-outline-secondary btn-sm"
        ),
        tags$span(class = "text-muted small", "or upload a list")
      ),
      # The file input lives in a uiOutput so the Clear button can re-render it to
      # a fresh (empty) control - a Shiny fileInput cannot otherwise be reset.
      uiOutput(ns("main_upload")),
      helpText(
        class = "small",
        "Upload accepts one gene symbol per line - a single-column",
        tags$b(".txt, .tsv, or .csv"),
        "with no header row. Pasted and uploaded genes are combined into this one",
        "list (the List weight above covers both)."
      ),
      # Progressive disclosure: the paste box above is the default single source;
      # "add a source" reveals named/typed rows for genes from a different
      # analysis (WES calls, DEGs, ATAC-seq hits, ...). A gene found in more of
      # your sources ranks higher (cross-source corroboration, automatic).
      actionLink(
        ns("add_source"),
        "+ add another source (tag by assay)",
        class = "small d-block"
      ),
      tags$div(id = ns("sources_anchor"), class = "mt-2"),
      helpText(
        class = "small",
        "One gene symbol per line. Add more lists to reward genes corroborated",
        "across your own sources; the deterministic ranking needs no API key."
      )
    )
  )
}

# Row 2 (left): the study description, curated context, disease discovery, tissues.
study_context_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "mb-3 gs-card-context",
    bslib::card_header("Study context"),
    bslib::card_body(
      textAreaInput(
        ns("description"),
        "What are you studying? (optional)",
        rows = 3,
        placeholder = paste(
          "e.g. germline drivers of NF1-associated MPNST",
          "in peripheral nerve"
        )
      ),
      selectInput(
        ns("study_context"),
        "Study context (applies disease priors)",
        choices = context_choices(),
        selected = "none"
      ),
      helpText(
        class = "small",
        "Applies a curated context - relevant pathways, known drivers,",
        "artifact-gene (FLAGS) flags, and tissues of interest - so ranking is",
        "disease-aware (pathway/function evidence is matched to the context and",
        "the veto extends to its FLAGS genes). Independent of the disease",
        "discovery box below."
      ),
      tags$label(
        class = "form-label small text-muted mb-1",
        "Discovery (optional): seed genes from a disease"
      ),
      # A flex row rather than a Bootstrap `.input-group`: Shiny wraps each input
      # in a `.shiny-input-container`, which breaks input-group's flush styling
      # (the Find button ends up detached). Flex + a matching button avoids that.
      div(
        class = "gs-inline-field",
        textInput(
          ns("disease"),
          label = NULL,
          placeholder = "e.g. neurofibromatosis type 1"
        ),
        actionButton(
          ns("resolve_disease"),
          "Find",
          class = "gs-btn gs-btn-soft"
        )
      ),
      uiOutput(ns("disease_matches")),
      textInput(
        ns("tissues"),
        "Tissue(s) of interest (optional)",
        placeholder = "e.g. peripheral nerve, Schwann cell"
      ),
      helpText(
        class = "small",
        "Comma-separated. Genes expressed there (GTEx) score higher; genes",
        "expressed only elsewhere are flagged."
      )
    )
  )
}

# The agent-involvement selector + the Rank button are placed directly by the
# review page (in the collapsible setup foot), reading the "input" namespace:
# `uiOutput(agent_mode_ui)` (rendered by input_server) and `actionButton("run")`.

# Sidebar: which per-gene connectors to query.
data_sources_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "mb-3",
    bslib::card_header("Data sources"),
    bslib::card_body(
      helpText(
        class = "small",
        "Choose which sources to query for per-gene evidence. An unchecked",
        "source is not fetched (saving its network cost); the default set is a",
        "lean, fast core. In a disease-context review the disease still seeds",
        "the candidate list."
      ),
      source_picker_ui(ns)
    )
  )
}

# Sidebar: per-source weights + the coverage/caveats toggles (collapsed by default
# to keep the sidebar compact). `registry` defaults to a freshly built registry
# (not the `genescout_registry` global, which global.R defines only after the UI is
# sourced); its keys match the global so the slider ids line up with input_server().
advanced_ui <- function(id, registry = genescout_signal_registry()) {
  ns <- NS(id)
  bslib::accordion(
    class = "mb-3",
    open = FALSE,
    bslib::accordion_panel(
      "Advanced - weights & caveats",
      helpText(
        class = "small",
        "Adjust how much each source counts; the table re-ranks instantly",
        "with no re-query."
      ),
      weight_sliders_ui(ns, registry),
      checkboxInput(
        ns("coverage_bonus"),
        "Reward genes supported by many evidence sources",
        value = FALSE
      ),
      checkboxInput(
        ns("caveats"),
        paste(
          "Apply caveats & veto (sink FLAGS sequencing-artifact genes;",
          "down-weight single-weak-source genes)"
        ),
        value = TRUE
      )
    )
  )
}

# A checkbox picker of the selectable (per-gene) source connectors, pre-checked to
# the default_on + available subset. Key-gated sources with no key are shown but
# labeled "needs API key" (and left unchecked). The input/network auto-signals
# (cross-source, STRING) are not shown - they append from the run data, not a
# checkbox. Built from genescout_source_catalog(), so new connectors appear here for free.
source_picker_ui <- function(ns) {
  catalog <- tryCatch(genescout_source_catalog(), error = function(e) list())
  # Only runnable, per-gene connectors are selectable. Stubs (key-gated sources
  # with no live client yet) are catalog/introspection-only - offering one as a
  # checkbox would be a silent no-op, so they are listed separately as "planned".
  sel_cat <- Filter(
    function(s) identical(s$needs %||% "gene", "gene") && !isTRUE(s$stub),
    catalog
  )
  if (length(sel_cat) == 0) {
    return(NULL)
  }
  keys <- vapply(sel_cat, function(s) s$key, character(1))
  labels <- vapply(
    sel_cat,
    function(s) {
      lab <- paste0(s$label, " · ", s$source)
      if (!signal_available(s)) paste0(lab, " (needs API key)") else lab
    },
    character(1)
  )
  selected <- keys[vapply(
    sel_cat,
    function(s) isTRUE(s$default_on %||% TRUE) && signal_available(s),
    logical(1)
  )]
  picker <- checkboxGroupInput(
    ns("sources"),
    NULL,
    choices = stats::setNames(keys, labels),
    selected = selected
  )
  # Key-gated sources GeneScout knows about but cannot query yet (no client). Shown as
  # an informational note, never a checkbox, so the picker offers only working ones.
  stub_cat <- Filter(function(s) isTRUE(s$stub), catalog)
  if (length(stub_cat) == 0) {
    return(picker)
  }
  stub_names <- paste(
    vapply(stub_cat, function(s) s$source, character(1)),
    collapse = ", "
  )
  tagList(
    picker,
    helpText(
      class = "fst-italic",
      paste0("Planned (needs an API key): ", stub_names, ".")
    )
  )
}

# Choices for the study-context picker: "none" plus every context/*.yaml, labeled
# by its human `label` (falling back to the id). Read once at UI-build time from the
# catalog of contexts, so a new context/<id>.yaml appears here for free.
context_choices <- function() {
  ids <- tryCatch(unname(list_contexts()), error = function(e) character())
  labels <- vapply(
    ids,
    function(id) {
      tryCatch(
        as.character(load_context(id)$label %||% id),
        error = function(e) {
          id
        }
      )
    },
    character(1)
  )
  stats::setNames(c("none", ids), c("None (no study priors)", labels))
}

# The weight sliders, grouped by the evidence domain of each signal so related
# sources sit together, and laid out in a full-width responsive grid rather than a
# single stacked column. Group headings reuse the report's domain labels.
weight_sliders_ui <- function(ns, registry) {
  domains <- vapply(registry, function(s) s$domain %||% "other", character(1))
  # Order groups by the canonical domain order, then any extras alphabetically.
  order_keys <- names(GENESCOUT_DOMAIN_LABELS)
  present <- unique(domains)
  ordered <- c(
    intersect(order_keys, present),
    sort(setdiff(present, order_keys))
  )
  groups <- lapply(ordered, function(d) {
    sigs <- registry[domains == d]
    label <- GENESCOUT_DOMAIN_LABELS[[d]] %||% d
    tags$div(
      class = "gs-weights-group",
      tags$div(class = "gs-weights-title", label),
      tags$div(
        class = "gs-weights-grid",
        lapply(sigs, function(s) {
          sliderInput(
            ns(paste0("w_", s$key)),
            s$label,
            min = 0,
            max = 2,
            value = s$weight,
            step = 0.05
          )
        })
      )
    )
  })
  tags$div(class = "gs-weights", groups)
}

# One removable extra-source row: a name, an assay type, and a genes box. Accepts
# prefilled values so the "Load 4-source example" button can render tagged rows
# directly (the values are baked into the initial render, so no updateInput needed).
extra_source_row <- function(
  ns,
  rid,
  name = "",
  type = "unspecified",
  genes = "",
  weight = 1
) {
  tags$div(
    class = "border rounded p-2 mb-2",
    id = ns(paste0("src_row_", rid)),
    div(
      class = "gs-source-head",
      div(
        class = "d-flex gap-2",
        style = "flex:1 1 auto; min-width:0;",
        textInput(
          ns(paste0("src_name_", rid)),
          label = NULL,
          value = name,
          placeholder = "source name (e.g. my DEGs)"
        ),
        selectInput(
          ns(paste0("src_type_", rid)),
          label = NULL,
          choices = genescout_source_types(),
          selected = type
        )
      ),
      div(
        class = "gs-weight",
        tags$label(`for` = ns(paste0("src_w_", rid)), "Weight"),
        numericInput(
          ns(paste0("src_w_", rid)),
          label = NULL,
          value = weight,
          min = 0,
          max = 5,
          step = 0.5,
          width = "84px"
        )
      )
    ),
    textAreaInput(
      ns(paste0("src_genes_", rid)),
      label = NULL,
      value = genes,
      rows = 3,
      placeholder = "paste genes for this source, one per line"
    ),
    fileInput(
      ns(paste0("src_file_", rid)),
      NULL,
      accept = c(".tsv", ".csv", ".txt"),
      placeholder = "or upload a single-column list (no header)"
    ),
    actionLink(
      ns(paste0("src_rm_", rid)),
      "remove this source",
      class = "small text-danger"
    )
  )
}

# The "Agent involvement" selector, rendered from the current LLM availability. The
# input/both modes are offered only when a key is available (env or a session BYOK
# key); the default "final" preserves today's behavior (final curator only).
# Cross-source corroboration needs no toggle - it applies automatically when the run
# has two or more sources. `selected` keeps the user's current pick across
# re-renders (e.g. when a key is pasted), defaulting to "final".
agent_mode_control <- function(ns, llm_ok, selected = "final") {
  choices <- if (isTRUE(llm_ok)) {
    c(
      "None - deterministic only" = "none",
      "Curate my input up front" = "input",
      "Curate the final list (default)" = "final",
      "Both (input + final)" = "both"
    )
  } else {
    c(
      "None - deterministic only" = "none",
      "Curate the final list (default)" = "final"
    )
  }
  if (!selected %in% choices) {
    selected <- "final"
  }
  tagList(
    radioButtons(
      ns("agent_mode"),
      "Agent involvement",
      choices = choices,
      selected = selected
    ),
    if (!isTRUE(llm_ok)) {
      helpText(
        class = "small",
        "Add an API key above (or set one in .Renviron) to enable AI curation,",
        "the specialists, and the input-curation agent."
      )
    }
  )
}

input_server <- function(
  id,
  registry = genescout_registry,
  llm_ready = reactive(FALSE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The agent-mode radio reacts to LLM availability so pasting a key on the Review
    # tab unlocks the input/both modes without a reload. isolate() the current pick
    # so re-rendering on a key change preserves the user's selection.
    output$agent_mode_ui <- renderUI({
      agent_mode_control(
        ns,
        llm_ok = tryCatch(llm_ready(), error = function(e) FALSE),
        selected = isolate(input$agent_mode) %||% "final"
      )
    })

    # The picker's selectable (runnable, per-gene) source keys - used only to tell a
    # genuine deselect-all (character(0) -> query nothing) from a picker that never
    # rendered (NULL -> fall back to defaults). Stubs are excluded (see the picker).
    selectable_source_keys <- tryCatch(
      vapply(
        Filter(
          function(s) identical(s$needs %||% "gene", "gene") && !isTRUE(s$stub),
          genescout_source_catalog()
        ),
        function(s) s$key,
        character(1)
      ),
      error = function(e) character(0)
    )

    # Discovery: resolve a free-text disease/phenotype to ontology candidates the
    # user confirms, then feed the chosen one into the pipeline as context.
    disease_matches <- reactiveVal(NULL)

    # Extra tagged sources (beyond the paste box). Rows are added/removed with
    # insertUI/removeUI so typed values survive; `extra_sources` tracks live ids.
    extra_sources <- reactiveVal(character(0))
    row_seq <- reactiveVal(0L)

    # The main file input renders inside a uiOutput keyed to `reset_seq` so the
    # Clear button can re-render it to a fresh (empty) control - the only reliable
    # way to reset a Shiny fileInput without an extra dependency.
    reset_seq <- reactiveVal(0L)
    output$main_upload <- renderUI({
      reset_seq()
      fileInput(
        ns("file"),
        NULL,
        accept = c(".tsv", ".csv", ".txt"),
        placeholder = "gene list (single column, no header)"
      )
    })

    # Insert one extra tagged-source row (optionally prefilled) and wire its
    # remove link. Shared by the "+ add another source" link and the "Load
    # 4-source example" button.
    add_extra_source <- function(
      name = "",
      type = "unspecified",
      genes = "",
      weight = 1
    ) {
      n <- row_seq() + 1L
      row_seq(n)
      rid <- as.character(n)
      extra_sources(c(extra_sources(), rid))
      insertUI(
        selector = paste0("#", session$ns("sources_anchor")),
        where = "beforeEnd",
        ui = extra_source_row(
          ns,
          rid,
          name = name,
          type = type,
          genes = genes,
          weight = weight
        )
      )
      observeEvent(
        input[[paste0("src_rm_", rid)]],
        {
          removeUI(
            selector = paste0("#", session$ns(paste0("src_row_", rid)))
          )
          extra_sources(setdiff(extra_sources(), rid))
        },
        ignoreInit = TRUE,
        once = TRUE
      )
    }

    observeEvent(input$add_source, add_extra_source())

    # Populate the four-assay example as tagged sources: clear the paste box, add
    # one row per list, and set the NF1 study context + a describing line.
    # Demonstrates cross-source corroboration and the caveats/veto stage on
    # real-shaped input. Shared by the "Load 4-source example" button and the demo.
    load_multisource_example <- function() {
      updateTextAreaInput(session, "paste", value = "")
      for (rid in extra_sources()) {
        removeUI(selector = paste0("#", session$ns(paste0("src_row_", rid))))
      }
      extra_sources(character(0))
      for (src in genescout_multisource_example()) {
        add_extra_source(
          name = src$label,
          type = src$type,
          genes = paste(src$genes, collapse = "\n")
        )
      }
      updateSelectInput(session, "study_context", selected = "nf1")
      updateTextAreaInput(
        session,
        "description",
        value = paste(
          "Drivers of NF1-associated MPNST across WES, bulk RNA-seq,",
          "ATAC-seq, and a CRISPR dependency screen"
        )
      )
    }

    observeEvent(input$load_multisource, {
      load_multisource_example()
      showNotification(
        paste(
          "Loaded a 4-source example (WES · RNA-seq · ATAC-seq · CRISPR).",
          "Genes shared across lists rank higher; click Rank genes."
        ),
        type = "message",
        duration = 6
      )
    })

    # Clear everything back to a blank slate: inputs, tagged sources, weights,
    # context, and the uploaded file. The review module clears the ranked result
    # (and thus any AI panels) when it sees this fire.
    observeEvent(input$reset, {
      updateTextAreaInput(session, "paste", value = "")
      updateTextAreaInput(session, "description", value = "")
      updateTextInput(session, "disease", value = "")
      updateTextInput(session, "tissues", value = "")
      updateSelectInput(session, "study_context", selected = "none")
      updateNumericInput(session, "w_pasted", value = 1)
      for (s in registry) {
        updateSliderInput(session, paste0("w_", s$key), value = s$weight)
      }
      updateCheckboxInput(session, "coverage_bonus", value = FALSE)
      updateCheckboxInput(session, "caveats", value = TRUE)
      for (rid in extra_sources()) {
        removeUI(selector = paste0("#", session$ns(paste0("src_row_", rid))))
      }
      extra_sources(character(0))
      disease_matches(NULL)
      reset_seq(reset_seq() + 1L) # re-render the main file input -> cleared
      showNotification(
        "Cleared. Set up a new review.",
        type = "message",
        duration = 4
      )
    })

    observeEvent(input$resolve_disease, {
      term <- input$disease
      if (is_blank(term)) {
        showNotification(
          "Enter a disease or phenotype to search.",
          type = "message"
        )
        return()
      }
      r <- tryCatch(
        resolve_disease(term),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
      if (!isTRUE(r$ok) || nrow(r$matches) == 0) {
        disease_matches(NULL)
        showNotification(
          paste("No disease match:", r$error %||% "none found"),
          type = "error"
        )
        return()
      }
      disease_matches(r$matches)
    })

    output$disease_matches <- renderUI({
      m <- disease_matches()
      if (is.null(m) || nrow(m) == 0) {
        return(NULL)
      }
      choices <- stats::setNames(m$id, sprintf("%s (%s)", m$name, m$id))
      radioButtons(
        ns("disease_pick"),
        "Pick the disease context:",
        choices = choices,
        selected = m$id[1]
      )
    })

    # Pre-fill the paste box with the bundled NF1 gene list, wrapped so a
    # missing/renamed example surfaces a notice rather than crashing.
    observeEvent(input$load_example, {
      loaded <- tryCatch(
        example_text("nf1_candidates"),
        error = function(e) {
          showNotification(
            paste("Could not load example:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      req(loaded)
      updateTextAreaInput(session, "paste", value = loaded)
      showNotification(
        "Loaded the NF1 example gene list. Click Rank genes.",
        type = "message",
        duration = 4
      )
    })

    keys <- vapply(registry, function(s) s$key, character(1))

    # The canonical input: a candidate_set built from the paste box (source
    # "pasted"), an optional upload ("uploaded"), and any extra tagged sources.
    # Empty sources are dropped by collect_candidate_set().
    candidate_set_r <- reactive({
      specs <- list()
      # The paste box and the main upload are ONE source ("pasted"): their genes are
      # combined, so an overlap between them is not mistaken for cross-source
      # corroboration. collect_candidate_set() unions text + file within a spec.
      paste_txt <- input$paste
      main_file <- input$file$datapath
      has_paste <- !is.null(paste_txt) && nzchar(trimws(paste_txt %||% ""))
      has_main_file <- !is.null(main_file) && nzchar(main_file)
      if (has_paste || has_main_file) {
        specs[[length(specs) + 1L]] <- list(
          name = "pasted",
          type = "unspecified",
          text = if (has_paste) paste_txt else NULL,
          file = if (has_main_file) main_file else NULL
        )
      }
      for (rid in extra_sources()) {
        gv <- input[[paste0("src_genes_", rid)]]
        fp <- input[[paste0("src_file_", rid)]]$datapath
        has_text <- !is.null(gv) && nzchar(trimws(gv %||% ""))
        has_file <- !is.null(fp) && nzchar(fp)
        if (has_text || has_file) {
          nm <- input[[paste0("src_name_", rid)]]
          specs[[length(specs) + 1L]] <- list(
            name = if (is_blank(nm)) paste0("source ", rid) else nm,
            type = input[[paste0("src_type_", rid)]] %||% "unspecified",
            text = if (has_text) gv else NULL,
            file = if (has_file) fp else NULL
          )
        }
      }
      collect_candidate_set(specs)
    })

    list(
      run = reactive(input$run),
      # The "Clear all" click; the review module clears the ranked result on it.
      reset = reactive(input$reset),
      # The rich candidate_set (canonical) plus the old named-list view for any
      # back-compat caller. run_enrich() accepts either.
      candidate_set = candidate_set_r,
      gene_lists = reactive(candidate_set_to_named_lists(candidate_set_r())),
      agent_mode = reactive(input$agent_mode %||% "final"),
      # The study's tissue(s) of interest (comma-separated), for the GTEx signal.
      tissues = reactive({
        x <- trimws(strsplit(input$tissues %||% "", ",")[[1]])
        x[nzchar(x)]
      }),
      description = reactive(input$description),
      # The selected study-context id (context/<id>.yaml) whose priors run_enrich
      # loads, or NULL for "none". Distinct from `disease` (ontology discovery).
      priors_id = reactive({
        v <- input$study_context %||% "none"
        if (identical(v, "none") || is_blank(v)) NULL else v
      }),
      # The confirmed disease context (list(id, name)) or NULL. NULL keeps the
      # run in plain enrichment mode.
      disease = reactive({
        m <- disease_matches()
        pick <- input$disease_pick
        if (is.null(m) || is.null(pick) || is_blank(pick)) {
          return(NULL)
        }
        row <- m[m$id == pick, , drop = FALSE]
        if (nrow(row) == 0) {
          return(NULL)
        }
        list(id = row$id[1], name = row$name[1])
      }),
      # Named key -> weight vector from the sliders; falls back to the registry
      # default before the sliders have rendered.
      weights = reactive({
        w <- vapply(
          registry,
          function(s) {
            val <- input[[paste0("w_", s$key)]]
            if (is.null(val)) s$weight else as.numeric(val)
          },
          numeric(1)
        )
        stats::setNames(w, keys)
      }),
      # Per-source trust weights (source label -> weight), keyed to match the labels
      # candidate_set_r() builds so the weighted cross-source corroboration lines up.
      # Missing/blank -> 1 (neutral). Only affects runs with two or more sources.
      source_weights = reactive({
        norm <- function(v) {
          if (is.null(v) || length(v) == 0 || is.na(v[1])) {
            1
          } else {
            as.numeric(v[1])
          }
        }
        w <- list(pasted = norm(input$w_pasted))
        for (rid in extra_sources()) {
          nm <- input[[paste0("src_name_", rid)]]
          label <- if (is_blank(nm)) paste0("source ", rid) else nm
          w[[label]] <- norm(input[[paste0("src_w_", rid)]])
        }
        w
      }),
      coverage_bonus = reactive(isTRUE(input$coverage_bonus)),
      # Caveats/veto on by default; NULL (pre-render) counts as on.
      caveats = reactive(isTRUE(input$caveats %||% TRUE)),
      # The selected source-connector keys (the Data sources picker). The picker is
      # statically rendered, so by the time "Rank genes" is clicked input$sources is
      # the checked set, or NULL when every box was unchecked - which we map to
      # character(0) so a deselect-all queries nothing (run_enrich errors) instead
      # of silently falling back to the full default set. When the picker never
      # rendered (no selectable sources), NULL correctly means "use defaults".
      enabled = reactive({
        if (length(selectable_source_keys) == 0) {
          return(NULL)
        }
        input$sources %||% character(0)
      })
    )
  })
}
