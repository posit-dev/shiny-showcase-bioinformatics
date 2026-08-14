# Subset builder module.
#
# Plan an analysis by slicing the dataset across tissue/organ, driver mutation,
# cell line, drug, dose, and plate. Coverage value boxes and a live plot update
# as the selection changes, then export the matched samples and a copy-paste
# recipe (R + Python) to reproduce the slice on the full Tahoe-100M dataset.
#
# Cell counts come from tahoe_cell_grid() (a small cached per drug x cell line x
# plate x dose aggregate of the obs data), so everything updates live and
# locally without re-querying 100M cells. All cell lines are pooled into every
# sample, so organ / driver / cell-line filters change the cell count, not the
# number of samples.

# Distinct, sorted, non-missing character values of a column.
.subset_choices <- function(df, column) {
  if (is.null(df) || !column %in% names(df)) {
    return(character())
  }
  vals <- as.character(df[[column]])
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(unique(vals))
}

subset_builder_ui <- function(id) {
  ns <- NS(id)

  # In async mode the grid may be a slow remote build, so we do NOT touch it at
  # UI-construction time (that would block startup for every session); the server
  # populates the grid-derived choices once the background task resolves. In the
  # default synchronous mode this reads the fast local/fixture grid as before.
  grid <- if (.tahoe_async_enabled()) {
    NULL
  } else {
    tryCatch(tahoe_cell_grid(), error = function(e) NULL)
  }
  lines <- tryCatch(tahoe_cell_line(), error = function(e) NULL)
  samples <- tryCatch(tahoe_sample(), error = function(e) NULL)

  assayed <- if (!is.null(grid)) unique(grid$cell_name) else character()
  organ_choices <- .subset_choices(grid, "organ")
  driver_choices <- if (!is.null(lines)) {
    .subset_choices(
      lines[lines$cell_name %in% assayed, , drop = FALSE],
      "Driver_Gene_Symbol"
    )
  } else {
    character()
  }
  drug_choices <- .subset_choices(grid, "drug")
  dose_choices <- sort(unique(grid$conc[!is.na(grid$conc)]))

  # Plate labels carry the number of distinct drug treatments per plate.
  plate_choices <- NULL
  if (!is.null(samples) && all(c("plate", "drug") %in% names(samples))) {
    per_plate <- tapply(samples$drug, samples$plate, function(d) {
      length(unique(d))
    })
    ord <- order(as.integer(gsub("\\D", "", names(per_plate))))
    per_plate <- per_plate[ord]
    plate_choices <- stats::setNames(
      names(per_plate),
      sprintf("%s · %d drugs", names(per_plate), per_plate)
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("filters_sidebar"),
      title = "Build a subset",
      width = 330,
      # tour_* ids anchor the guided demo (see R/tour.R).
      div(
        id = ns("tour_builder"),
        selectizeInput(
          ns("organs"),
          "Tissue / organ",
          choices = organ_choices,
          multiple = TRUE,
          options = list(placeholder = "All tissues")
        ),
        selectizeInput(
          ns("drivers"),
          "Driver mutation (gene)",
          choices = driver_choices,
          multiple = TRUE,
          options = list(placeholder = "Any driver gene")
        ),
        selectizeInput(
          ns("cell_lines"),
          "Cell lines",
          choices = assayed,
          multiple = TRUE,
          options = list(placeholder = "All (matching tissue/driver)")
        ),
        selectizeInput(
          ns("drugs"),
          "Drugs",
          choices = drug_choices,
          multiple = TRUE,
          options = list(placeholder = "All drugs")
        ),
        selectizeInput(
          ns("doses"),
          "Dose (µM)",
          choices = dose_choices,
          multiple = TRUE,
          options = list(placeholder = "All doses")
        ),
        selectizeInput(
          ns("plates"),
          "Plates",
          choices = plate_choices,
          multiple = TRUE,
          options = list(placeholder = "All plates")
        )
      ),
      tags$hr(),
      actionButton(
        ns("reset"),
        "Reset selection",
        class = "btn-sm btn-outline-secondary"
      )
    ),
    div(id = ns("tour_estimate"), uiOutput(ns("coverage_boxes"))),
    div(
      class = "text-muted small mb-2",
      tags$strong("How the experiment is laid out: "),
      "14 plates (96-well), each a batch of ~93-95 drug treatments; ",
      tags$code("DMSO_TF"),
      " is the vehicle control on every plate. Doses are 0.05 / 0.5 / 5 µM. ",
      "All 50 cell lines are pooled into every sample, so tissue / driver / ",
      "cell-line filters change the ",
      tags$em("cell count"),
      ", not the number of samples."
    ),
    bslib::layout_columns(
      id = ns("tour_preview"),
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Cells in selection, by cell line"),
        echarts4r::echarts4rOutput(ns("live_plot"), height = "420px")
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Matched samples"),
          tahoe_table_columns_ui(ns("preview"))
        ),
        tahoe_table_ui(ns("preview"))
      )
    ),
    bslib::card(
      id = ns("tour_export"),
      bslib::card_header("Export subset and analysis recipe"),
      uiOutput(ns("estimate")),
      subset_export_ui(ns("export"), show_recipe = TRUE)
    )
  )
}

# The subset-recipe constants and helpers (.subset_obs_hf,
# .subset_obs_bytes_per_cell, .subset_fmt, .subset_sql_vec) and the recipe
# builder (tahoe_subset_recipe) now live in R/subset_recipe.R, shared with the
# Chat assistant's build_subset_recipe tool.

# Compact format for large counts (e.g. 28.5M) so value boxes don't wrap.
.subset_fmt_big <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return("-")
  }
  scales::label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())(x)
}

subset_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    driver_lines <- reactive(tahoe_cell_line())

    if (.tahoe_async_enabled()) {
      # Build the grid in a background worker so a slow remote scan never blocks
      # the main process. Reading $result() suspends dependent reactives (like
      # req()) until the build finishes, then yields the same tibble the sync
      # path returns; a worker failure re-throws here and surfaces as an error.
      grid_task <- tahoe_make_grid_task()
      grid_task$invoke()
      grid <- reactive(grid_task$result())

      # The UI built the grid-derived selectize inputs empty (see the UI note);
      # populate them once the grid resolves. tahoe_cell_line() is a small local
      # table, so the driver choices stay synchronous. Plate choices come from
      # tahoe_sample() and are already populated by the UI in both modes.
      observe({
        g <- grid()
        assayed <- unique(g$cell_name)
        dl <- driver_lines()
        driver_choices <- if (
          all(c("cell_name", "Driver_Gene_Symbol") %in% names(dl))
        ) {
          .subset_choices(
            dl[dl$cell_name %in% assayed, , drop = FALSE],
            "Driver_Gene_Symbol"
          )
        } else {
          character()
        }
        updateSelectizeInput(
          session,
          "organs",
          choices = .subset_choices(
            g,
            "organ"
          )
        )
        updateSelectizeInput(session, "drivers", choices = driver_choices)
        updateSelectizeInput(session, "cell_lines", choices = sort(assayed))
        updateSelectizeInput(
          session,
          "drugs",
          choices = .subset_choices(
            g,
            "drug"
          )
        )
        updateSelectizeInput(
          session,
          "doses",
          choices = sort(unique(g$conc[!is.na(g$conc)]))
        )
      })
    } else {
      grid <- reactive(tahoe_cell_grid())
    }

    sel_organs <- reactive(input$organs %||% character())
    sel_drivers <- reactive(input$drivers %||% character())
    sel_cell_lines <- reactive(input$cell_lines %||% character())
    sel_drugs <- reactive(input$drugs %||% character())
    sel_doses <- reactive({
      d <- suppressWarnings(as.numeric(input$doses))
      d[!is.na(d)]
    })
    sel_plates <- reactive(input$plates %||% character())

    # The current selection across the six subset dimensions, in the plain-list
    # shape the shared recipe logic (R/subset_recipe.R) expects.
    selection <- reactive(
      list(
        organs = sel_organs(),
        drivers = sel_drivers(),
        cell_lines = sel_cell_lines(),
        drugs = sel_drugs(),
        doses = sel_doses(),
        plates = sel_plates()
      )
    )

    # Cell lines implied by the tissue + driver + explicit-cell-line filters,
    # restricted to the assayed lines that actually appear in the data.
    matched_cell_names <- reactive(
      tahoe_subset_matched_lines(selection(), grid(), driver_lines())
    )

    # Keep the cell-line picker in sync with the tissue / driver filters.
    observeEvent(
      list(sel_organs(), sel_drivers()),
      {
        g <- grid()
        choices <- sort(unique(g$cell_name))
        if (length(sel_organs()) > 0 && "organ" %in% names(g)) {
          choices <- sort(unique(g$cell_name[g$organ %in% sel_organs()]))
        }
        if (length(sel_drivers()) > 0) {
          dl <- driver_lines()
          hit <- unique(dl$cell_name[dl$Driver_Gene_Symbol %in% sel_drivers()])
          choices <- intersect(choices, hit)
        }
        updateSelectizeInput(
          session,
          "cell_lines",
          choices = choices,
          selected = intersect(sel_cell_lines(), choices)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$reset, {
      for (ctrl in c(
        "organs",
        "drivers",
        "cell_lines",
        "drugs",
        "doses",
        "plates"
      )) {
        updateSelectizeInput(session, ctrl, selected = character())
      }
    })

    # The cell grid narrowed by every active filter. Empty on a dimension means
    # no restriction there.
    grid_filtered <- reactive({
      g <- grid()
      if (nrow(g) == 0) {
        return(g)
      }
      g <- g[g$cell_name %in% matched_cell_names(), , drop = FALSE]
      if (length(sel_drugs()) > 0) {
        g <- g[g$drug %in% sel_drugs(), , drop = FALSE]
      }
      if (length(sel_doses()) > 0) {
        g <- g[!is.na(g$conc) & g$conc %in% sel_doses(), , drop = FALSE]
      }
      if (length(sel_plates()) > 0) {
        g <- g[g$plate %in% sel_plates(), , drop = FALSE]
      }
      g
    })

    # Sample-level slice for the table and export: samples matching the drug /
    # dose / plate dimensions (cell lines are pooled across all samples).
    matched_samples <- reactive({
      df <- tahoe_sample()
      keep <- rep(TRUE, nrow(df))
      if (length(sel_drugs()) > 0 && "drug" %in% names(df)) {
        keep <- keep & df$drug %in% sel_drugs()
      }
      if (length(sel_plates()) > 0 && "plate" %in% names(df)) {
        keep <- keep & df$plate %in% sel_plates()
      }
      if (length(sel_doses()) > 0 && "drugname_drugconc" %in% names(df)) {
        conc <- tahoe_parse_dose(df$drugname_drugconc)$conc
        keep <- keep & !is.na(conc) & conc %in% sel_doses()
      }
      df[keep, , drop = FALSE]
    })

    output$coverage_boxes <- renderUI({
      g <- grid_filtered()
      n_cells <- as.integer(sum(g$n_cells, na.rm = TRUE))
      n_lines <- dplyr::n_distinct(g$cell_name)
      n_drugs <- dplyr::n_distinct(g$drug)
      n_samples <- nrow(matched_samples())
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          "Cells",
          .subset_fmt_big(n_cells),
          theme = "primary"
        ),
        bslib::value_box(
          "Cell lines",
          .subset_fmt(n_lines),
          theme = "primary"
        ),
        bslib::value_box(
          "Drugs",
          .subset_fmt(n_drugs),
          theme = "secondary"
        ),
        bslib::value_box(
          "Samples",
          .subset_fmt(n_samples),
          theme = "secondary"
        )
      )
    })

    output$live_plot <- echarts4r::renderEcharts4r({
      g <- grid_filtered()
      validate(need(
        nrow(g) > 0 && sum(g$n_cells, na.rm = TRUE) > 0,
        "No cells match the current selection."
      ))
      by_line <- stats::aggregate(n_cells ~ cell_name, data = g, FUN = sum)
      by_line <- by_line[order(-by_line$n_cells), , drop = FALSE]
      by_line <- utils::head(by_line, 25)
      plot_df <- data.frame(
        label = by_line$cell_name,
        value = by_line$n_cells,
        stringsAsFactors = FALSE
      )
      tahoe_echart_hbar(plot_df, tahoe_colors$primary, value_name = "Cells")
    })

    tahoe_table_server("preview", data = matched_samples, page_size = 8)

    # Estimated size of the subset: cells (from the grid), matched samples, and
    # the approximate obs metadata to scan. Expression is separate and larger.
    estimate <- reactive({
      cells <- as.integer(sum(grid_filtered()$n_cells, na.rm = TRUE))
      list(
        cells = cells,
        samples = nrow(matched_samples()),
        obs_mb = round(cells * .subset_obs_bytes_per_cell / 1e6)
      )
    })

    output$estimate <- renderUI({
      e <- estimate()
      div(
        class = "text-muted small mb-2",
        tags$strong("Estimated pull: "),
        sprintf(
          paste(
            "~%s cells across %s samples · ~%s MB of obs metadata to scan.",
            "The expression matrix is downloaded separately and is much larger."
          ),
          .subset_fmt(e$cells),
          .subset_fmt(e$samples),
          .subset_fmt(e$obs_mb)
        )
      )
    })

    # Copy-paste recipe reproducing the selection straight from HuggingFace,
    # built by the shared recipe logic in R/subset_recipe.R (also used by the
    # Chat assistant's build_subset_recipe tool). The tissue / driver filters are
    # resolved to their concrete cell_name set inside tahoe_subset_recipe().
    recipe_parts <- reactive(
      tahoe_subset_recipe(
        selection(),
        grid(),
        driver_lines(),
        tahoe_sample()
      )
    )

    subset_export_server(
      "export",
      data_reactive = matched_samples,
      file_stem = "tahoe_subset",
      recipe_parts = recipe_parts
    )

    # --- Chat-assistant bridge -------------------------------------------------
    # Expose the live selection (read) and a validated setter (drive) so the
    # assistant's get_page_controls / set_page_controls tools can inspect and
    # change what the user has picked here. Registered into session$userData
    # (shared across module sessions); see R/agent_bridge.R. These closures are
    # called from the chat module's async tool call, so every reactive read is
    # isolated and every input write targets THIS module's session.

    # Estimated cells/samples for an arbitrary selection list, via the shared
    # recipe logic (the same numbers the Export card shows). NA on any failure.
    bridge_counts <- function(sel) {
      r <- tryCatch(
        isolate(tahoe_subset_recipe(
          sel,
          grid(),
          driver_lines(),
          tahoe_sample()
        )),
        error = function(e) NULL
      )
      list(
        cells = if (is.null(r)) NA_integer_ else r$cells,
        samples = if (is.null(r)) NA_integer_ else r$samples
      )
    }

    bridge_get <- function() {
      sel <- isolate(selection())
      cnt <- bridge_counts(sel)
      list(
        available = TRUE,
        selection = sel,
        estimated_cells = cnt$cells,
        estimated_samples = cnt$samples
      )
    }

    # Apply a partial selection request: NULL for a dimension leaves it untouched,
    # any vector (including empty) replaces it. Values outside the dataset are
    # dropped and reported in `ignored`, so the LLM can correct itself.
    bridge_set <- function(request) {
      # Accept the canonical cross-page field name (driver_genes) as well as the
      # subset-native `drivers`.
      if (is.null(request$drivers)) {
        request$drivers <- request$driver_genes
      }
      g <- isolate(tryCatch(grid(), error = function(e) NULL))
      dl <- isolate(tryCatch(driver_lines(), error = function(e) {
        tahoe_cell_line()
      }))
      cur <- isolate(selection())
      if (is.null(g)) {
        g <- data.frame()
      }
      assayed <- if ("cell_name" %in% names(g)) {
        unique(g$cell_name)
      } else {
        character()
      }
      samples <- tryCatch(tahoe_sample(), error = function(e) NULL)

      # Valid value domain per dimension, mirroring the UI's choice construction.
      dom <- list(
        organs = .subset_choices(g, "organ"),
        drivers = if (
          all(c("cell_name", "Driver_Gene_Symbol") %in% names(dl))
        ) {
          .subset_choices(
            dl[dl$cell_name %in% assayed, , drop = FALSE],
            "Driver_Gene_Symbol"
          )
        } else {
          character()
        },
        cell_lines = sort(assayed),
        drugs = .subset_choices(g, "drug"),
        doses = sort(unique(g$conc[!is.na(g$conc)])),
        plates = if (!is.null(samples) && "plate" %in% names(samples)) {
          sort(unique(as.character(samples$plate)))
        } else {
          character()
        }
      )

      applied <- cur
      ignored <- list()
      # Keep only the requested values that exist in the dimension's domain;
      # record the rest. Returns NULL (leave untouched) when not provided.
      validate_dim <- function(name, numeric = FALSE) {
        vals <- request[[name]]
        if (is.null(vals)) {
          return(NULL)
        }
        vals <- if (numeric) {
          suppressWarnings(as.numeric(unlist(vals)))
        } else {
          as.character(unlist(vals))
        }
        vals <- vals[!is.na(vals)]
        good <- unique(vals[vals %in% dom[[name]]])
        bad <- unique(vals[!(vals %in% dom[[name]])])
        if (length(bad) > 0) {
          ignored[[name]] <<- as.character(bad)
        }
        good
      }

      for (nm in c("organs", "drivers", "drugs", "doses", "plates")) {
        good <- validate_dim(nm, numeric = identical(nm, "doses"))
        if (!is.null(good)) {
          applied[[nm]] <- good
        }
      }

      # Cell lines are further constrained by the RESULTING tissue / driver
      # filters (as the UI narrows them). Compute that choice set, keep only the
      # requested lines within it, and report any that fall outside.
      cl_req <- validate_dim("cell_lines")
      cl_choices <- sort(assayed)
      if (length(applied$organs) > 0 && "organ" %in% names(g)) {
        cl_choices <- sort(unique(g$cell_name[g$organ %in% applied$organs]))
      }
      if (
        length(applied$drivers) > 0 &&
          all(c("cell_name", "Driver_Gene_Symbol") %in% names(dl))
      ) {
        hit <- unique(dl$cell_name[dl$Driver_Gene_Symbol %in% applied$drivers])
        cl_choices <- intersect(cl_choices, hit)
      }
      if (!is.null(cl_req)) {
        dropped <- setdiff(cl_req, cl_choices)
        if (length(dropped) > 0) {
          ignored$cell_lines <- unique(c(ignored$cell_lines, dropped))
        }
        applied$cell_lines <- intersect(cl_req, cl_choices)
      } else {
        # Not requested, but the filters above may have narrowed the valid set;
        # keep the current lines that still qualify (matches the sync observer).
        applied$cell_lines <- intersect(applied$cell_lines, cl_choices)
      }

      # Push each dimension to its input. Filters first, then cell_lines with its
      # recomputed choice set, so the tissue/driver sync observer -- when it fires
      # on the organ/driver change -- re-applies the SAME narrowed set idempotently.
      updateSelectizeInput(session, "organs", selected = applied$organs)
      updateSelectizeInput(session, "drivers", selected = applied$drivers)
      updateSelectizeInput(session, "drugs", selected = applied$drugs)
      updateSelectizeInput(
        session,
        "doses",
        selected = as.character(applied$doses)
      )
      updateSelectizeInput(session, "plates", selected = applied$plates)
      updateSelectizeInput(
        session,
        "cell_lines",
        choices = cl_choices,
        selected = applied$cell_lines
      )

      cnt <- bridge_counts(applied)
      out <- list(
        applied = TRUE,
        selection = applied,
        estimated_cells = cnt$cells,
        estimated_samples = cnt$samples
      )
      if (length(ignored) > 0) {
        out$ignored <- ignored
      }
      out
    }

    tahoe_register_page_bridge(
      session,
      "subset",
      list(title = "Subset builder", get = bridge_get, set = bridge_set)
    )

    list(matched_samples = matched_samples, grid_filtered = grid_filtered)
  })
}
