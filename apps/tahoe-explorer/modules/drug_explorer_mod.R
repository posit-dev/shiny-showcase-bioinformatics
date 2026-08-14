# Drug & MOA explorer module.
#
# Browse the drug metadata table: filter by mechanism of action (broad/fine),
# approval status, clinical-trial status, target, and name; then view the
# filtered set as a table (with PubChem links) plus summary charts and export
# the current subset. Reads only the small drug metadata table, so it is fast.
#
# The page is cross-linked: selecting a drug row (or the sidebar picker -- the
# two stay in sync) drives a detail card and the target-mutation cross-reference,
# and highlights that drug's mechanism / approval / targets in the charts.
# Clicking a chart bar filters the table to that category (charts -> filters).

# Distinct, sorted, non-missing values of a column, or character(0) if absent.
.drug_choices <- function(df, column) {
  if (!column %in% names(df)) {
    return(character(0))
  }
  vals <- df[[column]]
  vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
  sort(unique(as.character(vals)))
}

# Distinct targets, splitting the comma-separated `targets` column into atoms.
.drug_target_atoms <- function(df) {
  if (!"targets" %in% names(df)) {
    return(character(0))
  }
  atoms <- unlist(stringr::str_split(df[["targets"]], ","))
  atoms <- stringr::str_trim(atoms)
  atoms <- atoms[!is.na(atoms) & nzchar(atoms)]
  sort(unique(atoms))
}

# A single field value from a one-row drug data frame, or NA if the column is
# absent / empty. Used to build the detail card defensively.
.drug_field <- function(row, col) {
  if (is.null(row) || !col %in% names(row)) {
    return(NA_character_)
  }
  v <- row[[col]][[1]]
  if (is.null(v) || is.na(v) || !nzchar(as.character(v))) {
    return(NA_character_)
  }
  as.character(v)
}

# Ordered (label, value) value counts of `column` in `df`, largest first. Shared
# by the bar and its click handler (which maps a clicked row back to this frame).
.drug_count_data <- function(df, column, top_n = 12) {
  validate(need(column %in% names(df), "Column not available"))
  validate(need(nrow(df) > 0, "No drugs match the current filters"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  data.frame(
    label = names(counts),
    value = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

# Ordered (label, value) counts of the top-N most frequent targets in `df`.
.drug_target_data <- function(df, top_n = 12) {
  validate(need("targets" %in% names(df), "Targets not available"))
  validate(need(nrow(df) > 0, "No drugs match the current filters"))
  atoms <- unlist(stringr::str_split(df[["targets"]], ","))
  atoms <- stringr::str_trim(atoms)
  atoms <- atoms[!is.na(atoms) & nzchar(atoms)]
  validate(need(length(atoms) > 0, "No targets to summarize"))
  counts <- sort(table(atoms), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  data.frame(
    label = names(counts),
    value = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

# Shared echarts4r bar for the summary charts: base-coloured bars with the
# `highlight` categories (the selected drug's) painted in the accent colour.
.drug_bar <- function(
  plot_df,
  base_fill,
  highlight = character(0),
  value_name = "Count"
) {
  tahoe_echart_hbar(
    plot_df,
    base_fill,
    value_name = value_name,
    highlight = if (length(highlight) == 0) NULL else highlight,
    highlight_color = tahoe_colors$orange
  )
}

# Value-count bar of `column` (used by tests; the server splits data + bar so a
# click can be mapped back to the plotted frame).
.drug_count_bar <- function(
  df,
  column,
  base_fill,
  highlight = character(0),
  top_n = 12
) {
  .drug_bar(.drug_count_data(df, column, top_n), base_fill, highlight, "Count")
}

# Top-N target bar (used by tests).
.drug_target_bar <- function(
  df,
  base_fill,
  highlight = character(0),
  top_n = 12
) {
  .drug_bar(.drug_target_data(df, top_n), base_fill, highlight, "Drugs")
}

# Build the selected-drug detail card body from a one-row drug data frame.
.drug_detail_ui <- function(row) {
  item <- function(label, value) {
    if (is.na(value)) {
      return(NULL)
    }
    div(
      class = "col-6 col-md-4 mb-2",
      div(
        class = "text-muted small text-uppercase",
        style = "letter-spacing:.03em;",
        label
      ),
      div(value)
    )
  }
  cid <- .drug_field(row, "pubchem_cid")
  pubchem <- if (!is.na(cid)) {
    div(
      class = "col-6 col-md-4 mb-2",
      div(
        class = "text-muted small text-uppercase",
        style = "letter-spacing:.03em;",
        "PubChem"
      ),
      tags$a(
        href = paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid),
        target = "_blank",
        rel = "noopener",
        paste0("CID ", cid)
      )
    )
  } else {
    NULL
  }
  notes <- .drug_field(row, "gpt-notes-approval")
  tags$div(
    tags$h5(.drug_field(row, "drug"), class = "mb-2"),
    div(
      class = "row",
      item("Targets", .drug_field(row, "targets")),
      item("MOA (broad)", .drug_field(row, "moa-broad")),
      item("MOA (fine)", .drug_field(row, "moa-fine")),
      item("Human approved", .drug_field(row, "human-approved")),
      item("Clinical trials", .drug_field(row, "clinical-trials")),
      pubchem
    ),
    if (!is.na(notes)) div(class = "text-muted small mt-1", notes) else NULL
  )
}

drug_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("filters_sidebar"),
      title = "Filters",
      width = 250,
      gap = "0.4rem",
      padding = "0.6rem",
      # tour_* ids anchor the guided demo (see R/tour.R).
      div(
        id = ns("tour_filters"),
        uiOutput(ns("filter_moa_broad")),
        uiOutput(ns("filter_moa_fine")),
        uiOutput(ns("filter_approval")),
        uiOutput(ns("filter_trials")),
        textInput(
          ns("target_search"),
          "Target contains",
          placeholder = "e.g. EGFR"
        ),
        textInput(
          ns("name_search"),
          "Drug name contains",
          placeholder = "e.g. Synthdrug"
        )
      ),
      tags$hr(),
      div(
        id = ns("tour_picker"),
        selectizeInput(
          ns("focus_drug"),
          "Selected drug",
          choices = NULL,
          options = list(placeholder = "Pick a drug…")
        ),
        div(
          class = "text-muted small",
          "Click a table row or pick here -- the two stay in sync."
        )
      )
    ),
    # Two columns: the table (browse) on the left; the selected drug's detail,
    # target mutations, and summary charts + export stacked on the right, so a
    # row click updates that column top-to-bottom.
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        id = ns("tour_table"),
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Filtered drugs"),
          tahoe_table_columns_ui(ns("tbl"))
        ),
        div(
          class = "text-muted small px-1 pb-1",
          "Click a row to select a drug -- its details, target mutations, and",
          " summary charts appear in the next column, with its mechanism /",
          " approval / targets highlighted. Click a chart bar to filter the table."
        ),
        tahoe_table_ui(ns("tbl"))
      ),
      div(
        bslib::card(
          id = ns("tour_detail"),
          bslib::card_header("Selected drug"),
          uiOutput(ns("drug_detail"))
        ),
        bslib::card(
          id = ns("tour_mut"),
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Target mutations in assayed cell lines"),
            div(
              class = "d-flex gap-2 align-items-center",
              tahoe_table_columns_ui(ns("muttbl")),
              .info_pop(
                paste(
                  "For the selected drug, the assayed cell lines that",
                  "carry a somatic variant in one of its target genes -- the",
                  "lines over which a target-mutant vs wild-type contrast could",
                  "be designed. Restricted to lines present in the obs grid;",
                  "variants from DepMap / Cellosaurus. Empty until variant data",
                  "is loaded."
                ),
                title = "Target mutations"
              )
            )
          ),
          uiOutput(ns("target_mut_caption")),
          tahoe_table_ui(ns("muttbl"))
        ),
        div(
          id = ns("tour_charts"),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Drugs by mechanism (MOA, broad)"),
            echarts4r::echarts4rOutput(ns("moa_broad_plot"), height = "320px")
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Approval status"),
            echarts4r::echarts4rOutput(ns("approval_plot"), height = "320px")
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Top targets"),
            echarts4r::echarts4rOutput(ns("targets_plot"), height = "320px")
          )
        ),
        bslib::card(
          id = ns("tour_export"),
          bslib::card_header("Export current subset"),
          subset_export_ui(ns("export"))
        )
      )
    )
  )
}

drug_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    drugs <- reactive(tahoe_drug())

    # Data-driven filter controls, built from the loaded table.
    output$filter_moa_broad <- renderUI({
      selectizeInput(
        ns("moa_broad"),
        "MOA (broad)",
        choices = .drug_choices(drugs(), "moa-broad"),
        multiple = TRUE,
        options = list(placeholder = "All mechanisms")
      )
    })
    output$filter_moa_fine <- renderUI({
      selectizeInput(
        ns("moa_fine"),
        "MOA (fine)",
        choices = .drug_choices(drugs(), "moa-fine"),
        multiple = TRUE,
        options = list(placeholder = "All fine mechanisms")
      )
    })
    output$filter_approval <- renderUI({
      selectizeInput(
        ns("approval"),
        "Human approved",
        choices = .drug_choices(drugs(), "human-approved"),
        multiple = TRUE,
        options = list(placeholder = "Any")
      )
    })
    output$filter_trials <- renderUI({
      selectizeInput(
        ns("trials"),
        "Clinical trials",
        choices = .drug_choices(drugs(), "clinical-trials"),
        multiple = TRUE,
        options = list(placeholder = "Any")
      )
    })

    # Apply every active filter defensively; an empty filter is no restriction.
    filtered <- reactive({
      df <- drugs()
      validate(need(nrow(df) > 0, "No drug metadata available"))

      keep_in <- function(df, column, selected) {
        if (is.null(selected) || length(selected) == 0) {
          return(df)
        }
        if (!column %in% names(df)) {
          return(df)
        }
        df[as.character(df[[column]]) %in% selected, , drop = FALSE]
      }

      df <- keep_in(df, "moa-broad", input$moa_broad)
      df <- keep_in(df, "moa-fine", input$moa_fine)
      df <- keep_in(df, "human-approved", input$approval)
      df <- keep_in(df, "clinical-trials", input$trials)

      target_search <- input$target_search
      if (
        !is.null(target_search) &&
          nzchar(target_search) &&
          "targets" %in% names(df)
      ) {
        hit <- stringr::str_detect(
          df[["targets"]],
          stringr::fixed(target_search, ignore_case = TRUE)
        )
        df <- df[!is.na(hit) & hit, , drop = FALSE]
      }

      name_search <- input$name_search
      if (
        !is.null(name_search) &&
          nzchar(name_search) &&
          "drug" %in% names(df)
      ) {
        hit <- stringr::str_detect(
          df[["drug"]],
          stringr::fixed(name_search, ignore_case = TRUE)
        )
        df <- df[!is.na(hit) & hit, , drop = FALSE]
      }

      df
    })

    # The selected drug's full record (from the whole table, so it survives a
    # filter that would hide the row), or NULL when nothing is selected.
    selected_row <- reactive({
      foc <- input$focus_drug
      if (is.null(foc) || !nzchar(foc)) {
        return(NULL)
      }
      d <- drugs()
      r <- d[as.character(d$drug) == foc, , drop = FALSE]
      if (nrow(r) == 0) NULL else r[1, , drop = FALSE]
    })

    # Per-column overrides for the drugs table: render pubchem_cid as a link.
    drug_table_cols <- function(df) {
      col_defs <- list()
      if ("pubchem_cid" %in% names(df)) {
        col_defs[["pubchem_cid"]] <- reactable::colDef(
          name = "PubChem",
          html = TRUE,
          cell = function(value) {
            if (is.na(value) || !nzchar(as.character(value))) {
              return("")
            }
            url <- paste0(
              "https://pubchem.ncbi.nlm.nih.gov/compound/",
              value
            )
            sprintf(
              '<a href="%s" target="_blank" rel="noopener">%s</a>',
              url,
              value
            )
          }
        )
      }
      col_defs
    }

    # The drugs table, with a column chooser and single-row selection. SMILES
    # and the free-text approval notes are hidden by default (both appear in the
    # detail card). `default_selected` re-highlights the focus drug's row after a
    # filter re-render.
    tbl <- tahoe_table_server(
      "tbl",
      data = filtered,
      columns = drug_table_cols,
      page_size = 25,
      hidden = c("canonical_smiles", "gpt-notes-approval"),
      selection = "single",
      on_click = "select",
      default_selected = function(df) {
        foc <- isolate(input$focus_drug)
        if (is.null(foc) || !nzchar(foc)) {
          return(NULL)
        }
        i <- which(as.character(df$drug) == foc)
        if (length(i) == 1) i else NULL
      },
      empty_message = "No drugs match the current filters"
    )

    # --- Table <-> picker two-way sync ---------------------------------------
    # A row click updates the picker (the canonical selection); guard against a
    # no-op update so the two observers can't ping-pong.
    observeEvent(tbl$selected(), {
      sel <- tbl$selected()
      if (length(sel) == 0) {
        return()
      }
      df <- filtered()
      if (sel < 1 || sel > nrow(df)) {
        return()
      }
      drug <- as.character(df$drug[[sel]])
      if (!identical(drug, input$focus_drug %||% "")) {
        updateSelectizeInput(session, "focus_drug", selected = drug)
      }
    })

    # A picker change highlights the matching table row (isolate the state read
    # so this observer fires only on the picker, not on selection changes).
    observeEvent(input$focus_drug, {
      df <- filtered()
      idx <- which(as.character(df$drug) == (input$focus_drug %||% ""))
      cur <- isolate(tbl$selected())
      if (length(idx) == 1) {
        if (!identical(as.integer(cur), as.integer(idx))) {
          tbl$set_selected(idx)
        }
      } else if (length(cur) > 0) {
        tbl$set_selected(NA)
      }
    })

    # Ordered plot frames, shared by each chart and its click handler.
    moa_df <- reactive(.drug_count_data(filtered(), "moa-broad"))
    approval_df <- reactive(.drug_count_data(filtered(), "human-approved"))
    targets_df <- reactive(.drug_target_data(filtered()))

    # --- Chart -> filter (click a bar to filter the table) -------------------
    # echarts4r reports the clicked bar's 1-based row in the plotted frame.
    observeEvent(input$moa_broad_plot_clicked_row, {
      d <- isolate(moa_df())
      row <- input$moa_broad_plot_clicked_row
      if (is.null(row) || row < 1 || row > nrow(d)) {
        return()
      }
      updateSelectizeInput(
        session,
        "moa_broad",
        selected = union(input$moa_broad, as.character(d$label[[row]]))
      )
    })
    observeEvent(input$approval_plot_clicked_row, {
      d <- isolate(approval_df())
      row <- input$approval_plot_clicked_row
      if (is.null(row) || row < 1 || row > nrow(d)) {
        return()
      }
      updateSelectizeInput(
        session,
        "approval",
        selected = union(input$approval, as.character(d$label[[row]]))
      )
    })
    observeEvent(input$targets_plot_clicked_row, {
      d <- isolate(targets_df())
      row <- input$targets_plot_clicked_row
      if (is.null(row) || row < 1 || row > nrow(d)) {
        return()
      }
      updateTextInput(
        session,
        "target_search",
        value = as.character(d$label[[row]])
      )
    })

    # --- Charts (highlight the selected drug's categories) -------------------
    output$moa_broad_plot <- echarts4r::renderEcharts4r({
      hl <- .drug_field(selected_row(), "moa-broad")
      .drug_bar(
        moa_df(),
        tahoe_colors$primary,
        highlight = if (is.na(hl)) character(0) else hl,
        value_name = "Count"
      )
    })

    output$approval_plot <- echarts4r::renderEcharts4r({
      hl <- .drug_field(selected_row(), "human-approved")
      .drug_bar(
        approval_df(),
        tahoe_colors$green,
        highlight = if (is.na(hl)) character(0) else hl,
        value_name = "Count"
      )
    })

    output$targets_plot <- echarts4r::renderEcharts4r({
      .drug_bar(
        targets_df(),
        tahoe_colors$sand,
        highlight = focus_targets(),
        value_name = "Drugs"
      )
    })

    # Populate the picker with drugs that declare a target.
    observeEvent(drugs(), once = TRUE, {
      d <- drugs()
      choices <- if ("targets" %in% names(d)) {
        sort(unique(d$drug[
          !is.na(d$targets) & nzchar(as.character(d$targets))
        ]))
      } else {
        character(0)
      }
      updateSelectizeInput(
        session,
        "focus_drug",
        choices = choices,
        selected = if (length(choices)) choices[[1]] else character(0),
        server = TRUE
      )
    })

    output$drug_detail <- renderUI({
      r <- selected_row()
      if (is.null(r)) {
        return(div(
          class = "text-muted small",
          "Select a drug row (or use the sidebar picker) to see its details."
        ))
      }
      .drug_detail_ui(r)
    })

    focus_targets <- reactive(tahoe_drug_targets(input$focus_drug))
    target_hits <- reactive(tahoe_target_mutations(focus_targets()))

    output$target_mut_caption <- renderUI({
      genes <- focus_targets()
      if (length(genes) == 0) {
        return(div(
          class = "text-muted small mb-2",
          "Pick a drug with known targets in the sidebar to see which assayed",
          "cell lines carry a mutation in them."
        ))
      }
      n_lines <- dplyr::n_distinct(target_hits()$cell_name)
      n_assayed <- dplyr::n_distinct(tahoe_cell_grid()$cell_name)
      div(
        class = "small mb-2",
        tags$b(input$focus_drug),
        " targets ",
        tags$b(paste(genes, collapse = ", ")),
        sprintf(
          " -- %d of %d assayed cell lines carry a somatic variant in %s.",
          n_lines,
          n_assayed,
          if (length(genes) == 1) "it" else "one of them"
        )
      )
    })

    # Ordered, column-trimmed view of the target-mutation hits for display.
    mut_display <- reactive({
      hits <- target_hits()
      if (is.null(hits) || nrow(hits) == 0) {
        return(hits)
      }
      pref <- c(
        "cell_name",
        "gene",
        "protein_change",
        "variant_type",
        "consequence",
        "source"
      )
      df <- hits[, intersect(pref, names(hits)), drop = FALSE]
      df[order(df$gene, df$cell_name), , drop = FALSE]
    })

    tahoe_table_server(
      "muttbl",
      data = mut_display,
      empty_message = paste(
        "No assayed cell line carries a mutation in this drug's target(s)."
      )
    )

    subset_export_server(
      "export",
      data_reactive = filtered,
      file_stem = "drugs_subset"
    )

    # --- Chat-assistant bridge: let the assistant read and drive these filters.
    drug_bridge_get <- function() {
      d <- isolate(drugs())
      list(
        filters = list(
          moa = list(
            current = isolate(input$moa_broad),
            options = .drug_choices(d, "moa-broad")
          ),
          moa_fine = list(
            current = isolate(input$moa_fine),
            options = .drug_choices(d, "moa-fine")
          ),
          approval = list(
            current = isolate(input$approval),
            options = .drug_choices(d, "human-approved")
          ),
          trials = list(
            current = isolate(input$trials),
            options = .drug_choices(d, "clinical-trials")
          ),
          target = list(current = isolate(input$target_search), kind = "text"),
          name = list(current = isolate(input$name_search), kind = "text")
        ),
        matches = nrow(isolate(filtered()))
      )
    }
    drug_bridge_set <- function(request) {
      d <- isolate(drugs())
      ignored <- list()
      apply_multi <- function(field, input_id, col) {
        v <- tahoe_bridge_validate(request[[field]], .drug_choices(d, col))
        if (length(v$bad) > 0) {
          ignored[[field]] <<- v$bad
        }
        if (!is.null(v$good)) {
          updateSelectizeInput(session, input_id, selected = v$good)
        }
      }
      apply_multi("moa", "moa_broad", "moa-broad")
      apply_multi("moa_fine", "moa_fine", "moa-fine")
      apply_multi("approval", "approval", "human-approved")
      apply_multi("trials", "trials", "clinical-trials")
      if (!is.null(request$target)) {
        updateTextInput(
          session,
          "target_search",
          value = as.character(request$target)
        )
      }
      if (!is.null(request$name)) {
        updateTextInput(
          session,
          "name_search",
          value = as.character(request$name)
        )
      }
      out <- list(applied = TRUE)
      if (length(ignored) > 0) {
        out$ignored <- ignored
      }
      out
    }
    tahoe_register_page_bridge(
      session,
      "drugs",
      list(title = "Drugs", get = drug_bridge_get, set = drug_bridge_set)
    )

    # Return the filtered reactive so callers (and testServer) can read it.
    filtered
  })
}
