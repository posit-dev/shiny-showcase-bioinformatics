# Study browser module: search the recount3 catalog and load one study.
#
# Three panes. Filters on the left, results in the middle, the selected study
# on the right. The right pane is a bslib sidebar rather than a card under the
# table, so reading an abstract never pushes the results off the screen.
#
# Loading runs off the main R process (ExtendedTask plus mirai daemon), so a
# slow download never blocks this session or any other. The server returns the
# app-wide `study` reactive:
# list(project, organism, source, rse, log_expr), NULL until a study is loaded.

# Presets rather than a slider. The median study has 11 samples and the largest
# has 28,706, so a linear control over that range is unusable.
# Named buckets rather than a number box. Download size is the thing that
# decides whether a study is worth clicking, and "under 10 MB" is a decision a
# reader can make instantly.
SIZE_PRESETS <- c(
  "Any size" = "any",
  "Under 10 MB" = "0-10",
  "10 to 50 MB" = "10-50",
  "50 to 200 MB" = "50-200",
  "Over 200 MB" = "200-Inf"
)

SAMPLE_PRESETS <- c(
  "Any size" = "any",
  "Up to 20 samples" = "0-20",
  "20 to 100" = "20-100",
  "100 to 500" = "100-500",
  "Over 500" = "500-Inf"
)

study_browser_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      width = 260,
      accordion(
        id = ns("filters"),
        multiple = TRUE,
        # Organism and source open by default. Size is the one people reach
        # for least, so it starts folded.
        open = c("organism", "source"),
        accordion_panel(
          "Organism",
          value = "organism",
          icon = bsicons::bs_icon("bezier2"),
          checkboxGroupInput(ns("organisms"), NULL, choices = character(0))
        ),
        accordion_panel(
          "Data source",
          value = "source",
          icon = bsicons::bs_icon("hdd-stack"),
          checkboxGroupInput(ns("sources"), NULL, choices = character(0))
        ),
        accordion_panel(
          "Study size",
          value = "size",
          icon = bsicons::bs_icon("rulers"),
          # Defaults to the smallest bucket. The median study has 11 samples,
          # so this is where most people want to start, and it keeps a first
          # visit away from the studies that take minutes to load.
          radioButtons(
            ns("samples"),
            "Samples",
            choices = SAMPLE_PRESETS,
            selected = "0-20"
          ),
          radioButtons(
            ns("download"),
            "Download",
            choices = SIZE_PRESETS,
            selected = "any"
          )
        )
      ),
      actionLink(ns("clear"), "Clear all filters", class = "small")
    ),
    layout_sidebar(
      sidebar = sidebar(
        title = "Selected study",
        position = "right",
        width = 360,
        open = "closed",
        id = ns("details_pane"),
        uiOutput(ns("load_progress")),
        uiOutput(ns("loaded_next")),
        uiOutput(ns("details"))
      ),
      div(
        class = "browse-main",
        div(
          class = "search-row",
          textInput(
            ns("q"),
            NULL,
            placeholder = "Search accession, title or abstract",
            width = "100%"
          )
        ),
        div(
          class = "catalog-meta",
          textOutput(ns("match_count"), inline = TRUE)
        ),
        DT::DTOutput(ns("catalog"), fill = TRUE)
      )
    )
  )
}

study_browser_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Read once for each process. Every session shares this one data frame.
    catalog <- read_catalog()

    observeEvent(TRUE, once = TRUE, {
      if (is.null(catalog)) {
        showNotification(
          paste(
            "No catalog snapshot at",
            paste0(catalog_snapshot_path(), "."),
            "Run Rscript data-raw/build_catalog.R to build one."
          ),
          type = "error",
          duration = NULL
        )
        return()
      }
      # Choices come from the data, so a new recount3 source appears on its own
      # rather than after someone edits a hardcoded list.
      organisms <- sort(unique(catalog$organism))
      sources <- catalog_sources(catalog)
      updateCheckboxGroupInput(
        session,
        "organisms",
        choices = labelled_counts(organisms, catalog$organism)
      )
      updateCheckboxGroupInput(
        session,
        "sources",
        choices = labelled_counts(sources, catalog$file_source)
      )
    })

    # Typing greps roughly 20 MB of abstract text in R. Waiting for a pause
    # turns that into one pass for each search rather than one for each letter.
    query <- reactive(input$q) |> debounce(350)

    sample_range <- reactive(preset_range(input$samples))
    download_range <- reactive(preset_range(input$download))

    filtered <- reactive({
      req(!is.null(catalog))
      rng <- sample_range()
      out <- catalog_search(
        catalog,
        query = query() %||% "",
        organisms = input$organisms,
        sources = input$sources,
        min_samples = rng$min,
        max_samples = rng$max
      )
      dl <- download_range()
      if (!is.null(dl$min) || !is.null(dl$max)) {
        mb <- catalog_download_mb(out)
        keep <- rep(TRUE, nrow(out))
        if (!is.null(dl$min)) {
          keep <- keep & mb >= dl$min
        }
        if (!is.null(dl$max)) {
          keep <- keep & mb <= dl$max
        }
        out <- out[keep, , drop = FALSE]
      }
      out
    })

    observeEvent(input$clear, {
      updateTextInput(session, "q", value = "")
      updateCheckboxGroupInput(session, "organisms", selected = character(0))
      updateCheckboxGroupInput(session, "sources", selected = character(0))
      updateRadioButtons(session, "samples", selected = "0-20")
      updateRadioButtons(session, "download", selected = "any")
    })

    output$catalog <- DT::renderDT(
      {
        validate(need(
          !is.null(catalog),
          "No catalog snapshot. Run Rscript data-raw/build_catalog.R"
        ))
        DT::datatable(
          catalog_display(filtered()),
          selection = "single",
          rownames = FALSE,
          colnames = c(
            "Study",
            "Organism",
            "Source",
            "Samples",
            "Download",
            "Title"
          ),
          escape = TRUE,
          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
            order = list(list(3, "desc")),
            scrollX = TRUE,
            columnDefs = list(list(targets = 5, width = "45%"))
          )
        )
      },
      server = TRUE
    )

    proxy <- DT::dataTableProxy("catalog")

    # Clearing the selection whenever the filters change is what keeps
    # selection honest. input$catalog_rows_selected indexes the frame that was
    # last rendered, so leaving a stale index in place while the data underneath
    # changes is how a click ends up resolving to the wrong study.
    observeEvent(filtered(), {
      DT::selectRows(proxy, NULL)
    })

    output$match_count <- renderText({
      req(!is.null(catalog))
      total <- format(nrow(catalog), big.mark = ",")
      shown <- nrow(filtered())
      if (shown == nrow(catalog)) {
        sprintf("Showing all %s studies.", total)
      } else {
        sprintf("%s of %s studies match.", format(shown, big.mark = ","), total)
      }
    })

    selected_row <- reactive({
      df <- filtered()
      i <- input$catalog_rows_selected
      if (!length(i) || i < 1L || i > nrow(df)) {
        return(NULL)
      }
      df[i, , drop = FALSE]
    })

    # Open the right pane on selection and close it when the selection goes.
    observeEvent(selected_row(), ignoreNULL = FALSE, {
      toggle_sidebar(
        "details_pane",
        open = !is.null(selected_row()),
        session = session
      )
    })

    # After a load the next move is not obvious from the browse view, so say it
    # and offer the click rather than leaving the user to find the tab.
    output$loaded_next <- renderUI({
      s <- study()
      if (is.null(s)) {
        return(NULL)
      }
      div(
        class = "loaded-next",
        div(
          class = "small",
          bsicons::bs_icon("check-circle-fill"),
          tags$strong(paste(" ", s$project)),
          " is loaded."
        ),
        actionButton(
          session$ns("go_overview"),
          "Open the Overview",
          icon = bsicons::bs_icon("arrow-right-circle"),
          class = "btn-sm btn-primary w-100 mt-2"
        )
      )
    })

    observeEvent(input$go_overview, {
      nav_select("nav", "Overview", session = session$rootScope())
    })

    output$details <- renderUI({
      row <- selected_row()
      if (is.null(row)) {
        return(p(class = "text-muted small", "Select a study in the table."))
      }
      links <- study_external_links(row)
      blocked <- study_load_block(row)
      tagList(
        h5(row$study_title),
        div(
          class = "study-badges",
          span(class = "badge text-bg-light", row$project),
          span(class = "badge text-bg-light", row$organism),
          span(class = "badge text-bg-light", toupper(row$file_source)),
          span(
            class = "badge text-bg-light",
            paste(format(row$n_samples, big.mark = ","), "samples")
          )
        ),
        if (is.null(blocked)) {
          input_task_button(
            session$ns("load"),
            "Load this study",
            icon = bsicons::bs_icon("download"),
            label_busy = "Loading...",
            class = "w-100 mb-2"
          )
        } else {
          div(
            class = "alert alert-warning py-2 px-3 small mb-2",
            tags$strong(bsicons::bs_icon("exclamation-triangle"), " Too large"),
            tags$div(blocked$reason),
            tags$div(class = "mt-1", blocked$detail)
          )
        },
        div(
          class = "study-cost small text-muted",
          bsicons::bs_icon("download"),
          sprintf(
            " %s to download, %s. Needs roughly %s of memory.",
            format_size_mb(study_download_estimate_mb(row)),
            format_download_time(study_download_estimate_mb(row)),
            format_size_mb(estimated_memory_mb(row$n_samples, row$organism))
          )
        ),
        div(
          class = "abstract-box",
          if (nzchar(row$study_abstract)) {
            row$study_abstract
          } else {
            em("recount3 has no abstract for this study.")
          }
        ),
        div(
          class = "study-links",
          lapply(seq_along(links), function(i) {
            tags$a(
              href = links[[i]],
              target = "_blank",
              rel = "noopener",
              class = "small",
              names(links)[i],
              bsicons::bs_icon("box-arrow-up-right")
            )
          })
        ),
      )
    })

    study <- reactiveVal(NULL)
    pending <- reactiveVal(NULL)
    prefetch <- reactiveVal(NULL)
    progress_file <- reactiveVal(NULL)

    # Start warming the selected study while the user reads its abstract.
    # About 70 percent of a cold load is the download, and a reader usually
    # spends longer than that deciding.
    #
    # The previous prefetch is cancelled first. Without that, clicking through
    # a page of results would queue a download for every row. Nothing is
    # prefetched for a study over the cap, since it can never be loaded, and
    # nothing is prefetched while a load is running, so the prefetch worker and
    # the load worker never write BiocFileCache at the same time.
    observeEvent(selected_row(), ignoreNULL = FALSE, {
      old <- prefetch()
      if (!is.null(old)) {
        try(mirai::stop_mirai(old), silent = TRUE)
        prefetch(NULL)
      }
      row <- selected_row()
      if (is.null(row) || identical(load_task$status(), "running")) {
        return()
      }
      if (!is.null(study_load_block(row))) {
        return()
      }
      prefetch(mirai::mirai(
        prefetch_study_files(proj_info),
        prefetch_study_files = prefetch_study_files,
        recount3_counts_url = recount3_counts_url,
        .args = list(proj_info = catalog_proj_info(row)),
        .compute = "prefetch"
      ))
    })

    # Download and log2 CPM both run on the daemon. mirai evaluates in a clean
    # process, so the two logic functions travel with the call and already
    # namespace-qualify their recount3 and SummarizedExperiment calls.
    # Everything the daemon needs travels through `...` rather than `.args`.
    # mirai puts `...` in the daemon's global environment, which is the
    # enclosure these functions resolve against; `.args` binds only in the
    # evaluation frame, so the nested calls would not find each other.
    load_task <- ExtendedTask$new(function(proj_info, progress_file) {
      mirai::mirai(
        load_study_staged(proj_info, progress_file),
        load_study_staged = load_study_staged,
        prefetch_study_files = prefetch_study_files,
        recount3_counts_url = recount3_counts_url,
        recount3_annotation_code = recount3_annotation_code,
        counts_label = counts_label,
        format_size_mb = format_size_mb,
        write_load_progress = write_load_progress,
        log_cpm = log_cpm,
        LOAD_STAGES = LOAD_STAGES,
        .args = list(proj_info = proj_info, progress_file = progress_file)
      )
    }) |>
      bind_task_button("load")

    observeEvent(input$load, {
      row <- selected_row()
      if (is.null(row)) {
        showNotification("Select a study in the table first.", type = "warning")
        return()
      }
      # The hidden button is not the guard. Anything can send this input, and
      # a study over the cap would take the server down with it.
      blocked <- study_load_block(row)
      if (!is.null(blocked)) {
        showNotification(
          paste(blocked$reason, blocked$detail),
          type = "error",
          duration = 12
        )
        return()
      }
      # create_rse() reaches match.arg() through annotation_options(), and
      # match.arg() rejects a factor. catalog_proj_info() rebuilds the row as
      # plain character columns and restores the dropped project_type.
      info <- catalog_proj_info(row)
      # Stop any warm-up first. Whatever it already finished stays in the
      # cache, and the load picks up from there.
      running <- prefetch()
      if (!is.null(running)) {
        try(mirai::stop_mirai(running), silent = TRUE)
        prefetch(NULL)
      }
      pf <- tempfile(pattern = "recount-load-", fileext = ".progress")
      progress_file(pf)
      pending(info)
      load_task$invoke(info, pf)
      showNotification(
        paste(
          "Loading",
          info$project,
          "in the background.",
          "Keep browsing; you will be told when it is ready."
        ),
        type = "message"
      )
    })

    # Poll only while a load is running. The daemon cannot push a value back
    # before it finishes, so it writes its step to a file and this reads it.
    load_progress <- reactive({
      if (!identical(load_task$status(), "running")) {
        return(NULL)
      }
      invalidateLater(400, session)
      read_load_progress(progress_file())
    })

    output$load_progress <- renderUI({
      if (!identical(load_task$status(), "running")) {
        return(NULL)
      }
      info <- pending()
      p <- load_progress()
      step <- p$step %||% 0L
      total <- p$total %||% LOAD_STAGES
      pct <- max(4, round(100 * step / total))
      div(
        class = "load-progress",
        div(
          class = "d-flex justify-content-between small",
          tags$strong(paste("Loading", info$project)),
          span(class = "text-muted", sprintf("%d of %d", step, total))
        ),
        div(
          class = "progress mt-1",
          role = "progressbar",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            style = paste0("width:", pct, "%")
          )
        ),
        div(
          class = "small text-muted mt-1",
          p$label %||% "Starting"
        )
      )
    })

    observeEvent(load_task$status(), {
      status <- load_task$status()
      if (!status %in% c("success", "error")) {
        return()
      }
      info <- pending()
      pending(NULL)
      pf <- progress_file()
      if (!is.null(pf)) {
        unlink(c(pf, paste0(pf, ".tmp")))
        progress_file(NULL)
      }
      if (status == "error") {
        msg <- tryCatch(
          {
            load_task$result()
            "unknown error"
          },
          error = function(e) conditionMessage(e)
        )
        showNotification(
          paste("Failed to load", info$project, ":", msg),
          type = "error",
          duration = 10
        )
        return()
      }
      res <- load_task$result()
      study(list(
        project = info$project,
        organism = info$organism,
        source = info$file_source,
        title = info$study_title,
        download_mb = info$download_mb,
        rse = res$rse,
        log_expr = res$log_expr
      ))
      showNotification(
        paste(
          info$project,
          "loaded:",
          ncol(res$rse),
          "samples.",
          "See the Overview view."
        ),
        type = "message"
      )
    })

    study
  })
}
