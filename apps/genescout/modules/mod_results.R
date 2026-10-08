# Results module: the "scientific-clean" master-detail workbench. A context strip
# summarizes the run; a toolbar carries the optional AI actions and export; the
# ranked table sits left and a sticky evidence pane sits right (select a row to
# read the grounded evidence). Optional AI curation renders below the grid. Empty
# state until a ranking exists. Custom markup (R/review_render.R) styled by
# www/css/genescout.css; row selection is wired in www/js/genescout.js.

results_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # The strip renders only once there is a ranking (req() inside), so it is
    # empty before the first run.
    uiOutput(ns("context_strip")),
    uiOutput(ns("empty_state")),
    conditionalPanel(
      condition = "output.has_result == true",
      ns = ns,
      uiOutput(ns("toolbar")),
      div(
        class = "gs-results",
        div(class = "gs-pane", uiOutput(ns("ranked_table"))),
        tags$aside(class = "gs-pane gs-detail", uiOutput(ns("detail_pane")))
      ),
      uiOutput(ns("ai_panels")),
      uiOutput(ns("legend"))
    )
  )
}

# `result` is a reactive returning the run_review() output (or NULL). `config_r`
# is a reactive of the effective provider/model config the AI curator uses - it
# folds any session BYOK credential (key + provider + models) onto the static
# config, so curation/specialists run under the user's pasted key. `agent_mode`
# is a reactive of the selected agent involvement; the final curator + specialists
# are offered only for final/both. `specialists` is the shared run_specialists()
# reactiveVal (owned by the review module so the export can embed the verdict).
results_server <- function(
  id,
  result,
  config_r = reactive(genescout_config),
  agent_mode = reactive("final"),
  specialists = reactiveVal(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    final_curator_on <- reactive(agent_mode() %in% c("final", "both"))
    verdict_map <- reactive(specialist_verdicts(specialists() %||% list()))
    spec_ran <- reactive(
      !is.null(specialists()) && !is.null(specialists()$by_gene)
    )

    # Drives the conditionalPanel that reveals the results block.
    output$has_result <- reactive(!is.null(result()))
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    output$empty_state <- renderUI({
      if (is.null(result())) {
        div(
          class = "gs-empty",
          tags$p(
            "No ranking yet - set up the review below, then click ",
            tags$b("Rank genes"),
            "."
          )
        )
      }
    })

    output$context_strip <- renderUI({
      req(result())
      gs_context_strip(result())
    })

    # --- toolbar (title + optional AI actions + export) -----------------------
    output$toolbar <- renderUI({
      req(result())
      n <- nrow(result()$genes)
      ai_actions <- if (final_curator_on()) {
        tagList(
          div(
            class = "gs-numinput",
            tags$span("Shortlist to"),
            numericInput(
              ns("target_size"),
              label = NULL,
              value = min(GENESCOUT_CURATE_TARGET_DEFAULT, n),
              min = 1,
              max = n,
              step = 1,
              width = "64px"
            ),
            gs_info("The target size for the AI-curated shortlist.")
          ),
          # Task buttons grey out while their step runs in the background,
          # so a second click cannot start a second run.
          bslib::input_task_button(
            ns("do_curate"),
            "✦ Curate with AI",
            label_busy = "Curating...",
            type = "default",
            class = "gs-btn gs-btn-soft"
          ),
          gs_info(
            "Filter the top ranked genes down to about your shortlist size with",
            "the model — it chooses only from them and cites the evidence shown."
          ),
          bslib::input_task_button(
            ns("do_specialists"),
            "◎ Analyze with specialists",
            label_busy = "Running specialists...",
            type = "default",
            class = "gs-btn gs-btn-soft"
          ),
          gs_info(
            tags$p(
              "Three grounded specialists (variant, pathway/disease, literature)",
              "synthesize each candidate into one plausibility verdict and a",
              "suggested next experiment."
            ),
            tags$p(
              "Adds the Plausibility column and fills the evidence pane. Reads only",
              "the evidence the pipeline already retrieved and cited."
            )
          )
        )
      }
      export <- tags$details(
        class = "gs-export",
        tags$summary(
          class = "gs-btn gs-btn-soft",
          "↓ Export"
        ),
        div(
          class = "gs-export-menu",
          downloadButton(
            ns("dl_report"),
            "Report (HTML)",
            class = "btn btn-outline-secondary btn-sm"
          ),
          downloadButton(
            ns("dl_csv"),
            "Ranking table (CSV)",
            class = "btn btn-outline-secondary btn-sm"
          )
        )
      )
      # A plain button (no server input): the JS in genescout.js routes its click
      # to the setup's Clear button, so reset lives in one place.
      reset_btn <- tags$button(
        type = "button",
        class = "gs-btn gs-btn-soft",
        `data-gs-reset` = "1",
        "↺ Reset"
      )
      div(
        class = "gs-toolbar",
        div(
          tags$h1("Ranked candidates"),
          div(
            class = "sub",
            sprintf(
              "%d gene%s scored - select a row to read the grounded evidence beside the table.",
              n,
              if (n == 1) "" else "s"
            )
          )
        ),
        div(class = "actions", ai_actions, export, reset_btn)
      )
    })

    # --- ranked table + detail pane ------------------------------------------
    output$ranked_table <- renderUI({
      req(result())
      genes <- result()$genes
      # isolate() the selection so a row click re-renders only the detail pane,
      # not the whole table; default the highlight to the top-ranked gene so it
      # matches the detail pane's default.
      selected <- isolate(input$selected_symbol) %||% genes$symbol[1]
      gs_ranked_table(
        genes,
        result()$registry,
        verdicts = verdict_map(),
        selected = selected,
        input_id = ns("selected_symbol")
      )
    })

    output$detail_pane <- renderUI({
      req(result())
      genes <- result()$genes
      sel <- input$selected_symbol %||% genes$symbol[1]
      row <- genes[genes$symbol == sel, , drop = FALSE]
      if (nrow(row) == 0) {
        row <- genes[1, , drop = FALSE]
      }
      row <- row[1, , drop = FALSE]
      verdict <- verdict_map()[[toupper(row$symbol)]]
      # The full per-domain specialist breakdown for this gene (bslib card,
      # restyled by `.gs .card`), placed near the top with the verdict.
      sa <- render_specialist_analysis(row, specialists())
      gs_detail_pane(
        row,
        result()$evidence,
        result()$registry,
        verdict = verdict,
        spec_ran = spec_ran(),
        specialist_analysis = sa
      )
    })

    output$legend <- renderUI({
      req(result())
      gs_legend(result()$registry)
    })

    # --- exports (auditable HTML report + flat CSV) --------------------------
    output$dl_report <- downloadHandler(
      filename = function() "genescout_report.html",
      content = function(file) {
        req(result())
        render_report(result(), file, specialists = specialists())
      }
    )
    output$dl_csv <- downloadHandler(
      filename = function() "genescout_ranking.csv",
      content = function(file) {
        req(result())
        utils::write.csv(
          build_export_csv(result(), verdicts = verdict_map()),
          file,
          row.names = FALSE
        )
      }
    )

    # --- AI curation (the final compaction step) -----------------------------
    # The AI steps run in the background (R/llm_offload.R): this session waits for
    # its own result, and other visitors are not held up.
    curated <- reactiveVal(NULL)
    curate_job <- genescout_llm_task(session)
    bslib::bind_task_button(curate_job$task, "do_curate")
    # The ranking and config a run was started with: a result that arrives after
    # the ranking changed is dropped, and a failure message is redacted with the
    # key the run used.
    curate_for <- NULL
    curate_cfg <- NULL
    # Reset any prior curation when the ranking changes (ignoreNULL = FALSE so a
    # failed re-run that clears the result also clears a stale curated card/CSV),
    # and stop a run that was started for the old ranking.
    observeEvent(
      result(),
      {
        curate_job$cancel()
        curated(NULL)
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$do_curate, {
      req(result())
      req(final_curator_on())
      ts <- input$target_size
      if (is.null(ts) || is.na(ts) || ts < 1) {
        ts <- GENESCOUT_CURATE_TARGET_DEFAULT
      }
      curate_for <<- result()
      curate_cfg <<- config_r()
      curate_job$task$invoke(curate_gene_list, result(), curate_cfg, top_n = ts)
    })

    observeEvent(curate_job$task$status(), {
      out <- curate_job$outcome()
      req(out, identical(curate_for, result()))
      if (!out$ok) {
        if (!is.null(out$message)) {
          showNotification(
            paste(
              "Curation failed:",
              genescout_redact_secret(out$message, curate_cfg$api_key %||% "")
            ),
            type = "error"
          )
        }
        return()
      }
      cur <- out$value
      curated(cur)
      # A model failure still returns the rank-based list, with the reason in an
      # `error` attribute that the card shows; only a real curation is announced.
      if (is.null(attr(cur, "error"))) {
        showNotification(
          "Curated shortlist ready - shown below the ranked table.",
          type = "message",
          duration = 4
        )
      }
    })

    output$ai_panels <- renderUI({
      req(result())
      cur <- curated()
      if (is.null(cur)) {
        return(NULL)
      }
      card_id <- ns("ai_panels_card")
      div(
        class = "gs-ai card",
        id = card_id,
        div(class = "card-header", "AI-curated shortlist"),
        div(
          class = "card-body",
          downloadButton(
            ns("curated_download"),
            "Download curated list (CSV)",
            class = "btn btn-outline-secondary btn-sm mb-2"
          ),
          render_curation(cur, result()$genes)
        ),
        # Bring the freshly-curated shortlist into view - it renders below the
        # table, so a click on "Curate" would otherwise leave it off-screen.
        tags$script(HTML(sprintf(
          "var el=document.getElementById('%s'); if(el){el.scrollIntoView({behavior:'smooth',block:'center'});}",
          card_id
        )))
      )
    })

    output$curated_download <- downloadHandler(
      filename = function() "genescout_curated.csv",
      content = function(file) {
        req(result())
        req(curated())
        utils::write.csv(build_curated_csv(curated()), file, row.names = FALSE)
      }
    )

    # --- Specialist analysis (optional LLM synthesis over the evidence) -------
    spec_job <- genescout_llm_task(session)
    bslib::bind_task_button(spec_job$task, "do_specialists")
    spec_for <- NULL
    spec_cfg <- NULL
    # Clear stale analysis when the ranking changes (ignoreNULL = FALSE so a
    # failed re-run that clears the result also clears the specialist cards), and
    # stop a run that was started for the old ranking.
    observeEvent(
      result(),
      {
        spec_job$cancel()
        specialists(NULL)
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$do_specialists, {
      req(result())
      req(final_curator_on())
      # If the user curated first, analyze the top of THAT list; otherwise the top
      # ranked genes. So the flow reads rank -> curate to N -> specialists on the best.
      cur <- curated()
      restrict <- if (!is.null(cur) && "include" %in% names(cur)) {
        inc <- cur$gene_symbol[which(cur$include)]
        if (length(inc) > 0) inc else NULL
      } else {
        NULL
      }
      spec_for <<- result()
      spec_cfg <<- config_r()
      spec_job$task$invoke(
        run_specialists,
        result(),
        spec_cfg,
        restrict_to = restrict
      )
    })

    observeEvent(spec_job$task$status(), {
      out <- spec_job$outcome()
      req(out, identical(spec_for, result()))
      if (!out$ok) {
        if (!is.null(out$message)) {
          showNotification(
            paste(
              "Specialist analysis failed:",
              genescout_redact_secret(out$message, spec_cfg$api_key %||% "")
            ),
            type = "error"
          )
        }
        return()
      }
      sp <- out$value
      # A graceful "no findings / no credentials" outcome is a notice, not an error.
      if (!is.null(sp) && !isTRUE(sp$ai_used) && !is.null(sp$message)) {
        showNotification(sp$message, type = "message")
      }
      specialists(sp)
    })
  })
}
