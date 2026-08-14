# Shiny Server
function(input, output, session) {
  # --- Controls (return reactives) ---
  disease <- disease_search_server("disease")
  sources <- source_select_server("sources")
  controls <- ranking_controls_server("controls")

  # --- Pipeline: the only place network I/O fires (Run button) ---
  pipeline <- run_pipeline_server(
    "run",
    disease_reactive = disease,
    sources_reactive = sources
  )

  # --- Ranking: cheap reactive; re-runs on slider change, not on the network ---
  ranked <- reactive({
    req(pipeline())
    ctl <- controls()
    rank_genes(
      pipeline()$aggregated,
      weights = ctl$weights,
      coverage_bonus = ctl$coverage_bonus,
      evidence_ids = pipeline()$evidence_ids
    )
  })

  status <- reactive({
    req(pipeline())
    pipeline()$status
  })

  # --- Display + downstream modules ---
  source_status_server("status", status_reactive = status)
  results_table_server("results", ranked_reactive = ranked)

  curated <- curation_server(
    "curation",
    ranked_reactive = ranked,
    disease_reactive = disease,
    controls_reactive = controls
  )

  export_server(
    "export",
    ranked_reactive = ranked,
    curated_reactive = curated,
    disease_reactive = disease,
    status_reactive = status,
    controls_reactive = controls
  )

  # --- Assistant: BYOK chat with tools that READ and DRIVE the app ---
  # Plain (non-reactive) snapshot the tools READ at call time. ellmer invokes
  # tools from inside the async streaming turn, so a tool must read a plain env
  # (not a reactive) to avoid "no active reactive context" errors; an observer
  # keeps the snapshot current with the live inputs + results.
  chat_ctx <- new.env(parent = emptyenv())
  chat_ctx$disease <- NULL
  chat_ctx$sources <- NULL
  chat_ctx$controls <- NULL
  chat_ctx$ranked <- NULL
  observe({
    chat_ctx$disease <- tryCatch(disease(), error = function(e) NULL)
    chat_ctx$sources <- tryCatch(sources(), error = function(e) NULL)
    chat_ctx$controls <- tryCatch(controls(), error = function(e) NULL)
    chat_ctx$ranked <- tryCatch(ranked(), error = function(e) NULL)
  })

  # Available source ids/labels (from the registry) - the set the tools accept.
  glb_src <- stats::setNames(
    vapply(list_sources(), function(s) s$id, character(1)),
    vapply(list_sources(), function(s) s$label, character(1))
  )

  # Tools DRIVE the app by writing inputs on the main session (updateXxxInput)
  # and clicking Resolve/Build via the glb_click custom message (ui.R). These
  # are fire-and-forget: an action starts the reactive cycle, and the result is
  # read back on a later tool call (get_app_state / get_ranked_genes).
  gene_list_tools <- list(
    ellmer::tool(
      function() {
        d <- chat_ctx$disease
        srcs <- chat_ctx$sources
        ctl <- chat_ctx$controls
        r <- chat_ctx$ranked
        paste(
          c(
            paste0(
              "Disease: ",
              if (is.null(d)) {
                "none resolved yet"
              } else {
                paste0(d$name, " (", d$id, ")")
              }
            ),
            paste0("Available sources: ", paste(glb_src, collapse = ", ")),
            paste0(
              "Selected sources: ",
              if (length(srcs)) paste(srcs, collapse = ", ") else "none"
            ),
            paste0(
              "Ranking: coverage_bonus=",
              (ctl$coverage_bonus %||% 0.5),
              ", target size=",
              (ctl$top_n %||% 30)
            ),
            if (is.null(r) || nrow(r) == 0) {
              "Results: no gene list built yet."
            } else {
              paste0("Results: ", nrow(r), " ranked genes available.")
            }
          ),
          collapse = "\n"
        )
      },
      description = paste(
        "Read the app's current state: resolved disease, available vs selected",
        "data sources, ranking settings, and whether a ranked gene list exists.",
        "Call this first to see what's set and whether results are available."
      ),
      name = "get_app_state"
    ),
    ellmer::tool(
      function(top_n = 25L) {
        r <- chat_ctx$ranked
        if (is.null(r) || nrow(r) == 0) {
          return(paste(
            "No gene list has been built yet. Use set_disease + set_sources +",
            "build_gene_list, then read results here."
          ))
        }
        top_n <- max(1L, min(as.integer(top_n), nrow(r)))
        cols <- intersect(
          c("rank", "gene_symbol", "combined_score", "n_sources"),
          names(r)
        )
        tbl <- as.data.frame(r[seq_len(top_n), cols, drop = FALSE])
        if ("combined_score" %in% cols) {
          tbl$combined_score <- round(tbl$combined_score, 4)
        }
        d <- chat_ctx$disease
        header <- if (!is.null(d)) {
          paste0("Disease: ", d$name, " (", d$id, "). ")
        } else {
          ""
        }
        rows <- apply(tbl, 1, function(x) paste(trimws(x), collapse = " | "))
        paste0(
          header,
          "Top ",
          top_n,
          " of ",
          nrow(r),
          " ranked genes ",
          "(columns: ",
          paste(cols, collapse = ", "),
          "):\n",
          paste(rows, collapse = "\n")
        )
      },
      description = paste(
        "Return the current ranked gene list as rank, gene symbol, combined",
        "score, and number of contributing evidence sources. Higher",
        "combined_score = higher priority. Use for any question about which",
        "genes are in the list or how/why they rank."
      ),
      arguments = list(
        top_n = ellmer::type_integer(
          "How many top-ranked genes to return (default 25).",
          required = FALSE
        )
      ),
      name = "get_ranked_genes"
    ),
    ellmer::tool(
      function(disease) {
        disease <- trimws(disease %||% "")
        if (!nzchar(disease)) {
          return("Provide a disease name or an EFO/MONDO id.")
        }
        updateTextInput(session, "disease-term", value = disease)
        session$sendCustomMessage("glb_click", "disease-resolve")
        paste0(
          "Entered '",
          disease,
          "' and started resolving; the best-matching ",
          "ontology term is auto-selected. Call get_app_state shortly to ",
          "confirm the resolved disease."
        )
      },
      description = paste(
        "Set the Disease field and resolve it to an ontology term. Pass a",
        "disease name (e.g. 'breast cancer') or an EFO/MONDO id. The top match",
        "is auto-selected. Resolution is async - confirm via get_app_state."
      ),
      arguments = list(
        disease = ellmer::type_string("Disease name or EFO/MONDO id.")
      ),
      name = "set_disease"
    ),
    ellmer::tool(
      function(sources) {
        sel <- intersect(as.character(sources), unname(glb_src))
        if (length(sel) == 0) {
          return(paste0(
            "No valid source ids. Choose from: ",
            paste(glb_src, collapse = ", "),
            "."
          ))
        }
        updateCheckboxGroupInput(session, "sources-sources", selected = sel)
        paste0("Selected sources: ", paste(sel, collapse = ", "), ".")
      },
      description = paste(
        "Choose which data sources to query. Pass a list of source ids from:",
        "opentargets, panelapp, diseases, clinvar, dgidb, gnomad, pharos."
      ),
      arguments = list(
        sources = ellmer::type_array(
          "Source ids to enable.",
          items = ellmer::type_string("A source id.")
        )
      ),
      name = "set_sources"
    ),
    ellmer::tool(
      function(coverage_bonus = NULL, target_size = NULL) {
        msgs <- character(0)
        if (!is.null(coverage_bonus)) {
          updateSliderInput(
            session,
            "controls-coverage_bonus",
            value = coverage_bonus
          )
          msgs <- c(msgs, paste0("coverage_bonus=", coverage_bonus))
        }
        if (!is.null(target_size)) {
          updateNumericInput(
            session,
            "controls-top_n",
            value = as.integer(target_size)
          )
          msgs <- c(msgs, paste0("target size=", as.integer(target_size)))
        }
        if (length(msgs) == 0) {
          return("Pass coverage_bonus (0-1) and/or target_size to change.")
        }
        paste0(
          "Updated ranking (",
          paste(msgs, collapse = ", "),
          "); re-ranks instantly, no re-querying."
        )
      },
      description = paste(
        "Adjust ranking: coverage_bonus (0-1 multi-source bonus) and/or",
        "target_size (target list size). Re-ranks instantly without re-querying."
      ),
      arguments = list(
        coverage_bonus = ellmer::type_number(
          "Multi-source coverage bonus, 0 to 1.",
          required = FALSE
        ),
        target_size = ellmer::type_integer(
          "Target gene-list size.",
          required = FALSE
        )
      ),
      name = "set_ranking"
    ),
    ellmer::tool(
      function() {
        d <- chat_ctx$disease
        srcs <- chat_ctx$sources
        if (is.null(d)) {
          return(paste(
            "No disease resolved yet. Call set_disease first and wait for it",
            "to resolve (check with get_app_state)."
          ))
        }
        if (length(srcs) == 0) {
          return("No data sources selected. Call set_sources first.")
        }
        session$sendCustomMessage("glb_click", "run-run")
        paste0(
          "Started building the gene list for ",
          d$name,
          " from ",
          length(srcs),
          " source(s). This queries the network; call ",
          "get_ranked_genes shortly to read the results."
        )
      },
      description = paste(
        "Build (run) the gene list using the currently selected disease and",
        "sources. Requires a resolved disease and >=1 source. Results appear",
        "shortly after; read them with get_ranked_genes."
      ),
      name = "build_gene_list"
    )
  )

  byok_chat_server(
    "chat",
    system_prompt = paste(
      "You are the assistant for Gene List Builder, a tool that builds a",
      "curated gene list for a disease from gene-disease-association sources",
      "(Open Targets, PanelApp, DISEASES, ClinVar, DGIdb) plus annotation",
      "sources (gnomAD constraint, Pharos). You can BOTH explain the app and",
      "DRIVE it with tools. Typical flow: call get_app_state to see what's set",
      "and whether results exist; if the user wants a list built, use",
      "set_disease, set_sources (and optionally set_ranking), then",
      "build_gene_list; read results with get_ranked_genes. Actions are async -",
      "after set_disease or build_gene_list, tell the user it's in progress and",
      "re-check with get_app_state / get_ranked_genes. When a question is about",
      "the current genes or state, call a tool rather than guessing. Be concise."
    ),
    tools = gene_list_tools
  )

  # The navbar "Assistant" button toggles the sidebar client-side (onclick in
  # ui.R), so it stays responsive even while a build is holding the server
  # thread -- no server observer needed here.

  # --- Guided demo: navbar "Demo" button ---
  # Prefill and resolve "breast cancer", then start the cicerone tour (R/tour.R).
  # The start is deferred briefly so the resolve round-trip and layout settle
  # before cicerone measures its first target. Built once per session.
  demo_guide <- glb_build_tour()
  demo_guide$init()
  observeEvent(input$demo_tour, {
    bslib::nav_select("main_nav", "Build")
    updateTextInput(session, "disease-term", value = "breast cancer")
    # Click the disease module's Resolve button by its namespaced DOM id.
    shinyjs::click("disease-resolve")
    later::later(
      function() demo_guide$start(session = session),
      delay = 0.5
    )
  })

  # --- About page: live API-key status ---
  output$about_key_status <- renderUI({
    if (gemini_available()) {
      div(class = "alert alert-success", "GEMINI_API_KEY detected.")
    } else {
      div(
        class = "alert alert-warning",
        "No GEMINI_API_KEY set: AI curation falls back to top-N by rank."
      )
    }
  })
}
