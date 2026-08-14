# Cell-line explorer module.
#
# Filterable view of the cell-line metadata table: a sidebar of filters drives
# a reactable of the matching cell lines plus a few summary charts, and embeds
# the reusable export module so a subset can be pulled for downstream analysis.
# Reads only the small cell-line metadata table, so it is fast.

# DepMap portal base URL; cell IDs (ACH-XXXXXX) link to their portal page.
.cell_line_depmap_url <- "https://depmap.org/portal/cell_line/"

# Distinct, sorted, non-missing values of one column of `df` for filter choices.
.cell_line_choices <- function(df, column) {
  if (!column %in% names(df)) {
    return(character())
  }
  vals <- df[[column]]
  vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
  sort(unique(as.character(vals)))
}

# Horizontal bar chart of the value counts of one column of `df`, as a shared
# echarts4r bar (largest on top). Precomputes counts into a clean (label, value)
# frame before plotting (mirrors overview_mod.R).
.cell_line_bar <- function(df, column, fill, top_n = 15) {
  validate(need(column %in% names(df), "Column not available"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  validate(need(length(counts) > 0, "No data to plot"))
  plot_df <- data.frame(
    label = names(counts),
    value = as.integer(counts),
    stringsAsFactors = FALSE
  )
  tahoe_echart_hbar(plot_df, fill, value_name = "Count")
}

cell_line_explorer_ui <- function(id) {
  ns <- NS(id)
  df <- tahoe_cell_line()
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
        selectizeInput(
          ns("organ"),
          "Organ",
          choices = .cell_line_choices(df, "Organ"),
          multiple = TRUE,
          options = list(placeholder = "All organs")
        ),
        selectizeInput(
          ns("gene"),
          "Driver gene",
          choices = .cell_line_choices(df, "Driver_Gene_Symbol"),
          multiple = TRUE,
          options = list(placeholder = "All genes")
        ),
        selectizeInput(
          ns("var_type"),
          "Variant type",
          choices = .cell_line_choices(df, "Driver_VarType"),
          multiple = TRUE,
          options = list(placeholder = "All variant types")
        ),
        textInput(
          ns("cell_name"),
          "Cell name contains",
          placeholder = "e.g. SYNTH"
        )
      )
    ),
    # Tables on the left, their charts on the right, in two compact rows so the
    # tab reads in a screen or two instead of one long scroll.
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        id = ns("tour_table"),
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Matching cell lines"),
          tahoe_table_columns_ui(ns("table"))
        ),
        subset_export_ui(ns("export")),
        tags$hr(),
        tahoe_table_ui(ns("table"))
      ),
      div(
        id = ns("tour_charts"),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Cell lines by organ"),
          echarts4r::echarts4rOutput(ns("organ_plot"), height = "320px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Top driver genes"),
          echarts4r::echarts4rOutput(ns("gene_plot"), height = "320px")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Variant-type breakdown"),
          echarts4r::echarts4rOutput(ns("var_type_plot"), height = "320px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        id = ns("tour_variants"),
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Somatic variants"),
          div(
            class = "d-flex gap-2 align-items-center",
            tahoe_table_columns_ui(ns("variants")),
            .info_pop(
              paste(
                "Somatic mutation calls for the matching cell lines. Full",
                "somatic profiles from DepMap 24Q4 (CC BY 4.0) where available,",
                "with curated driver variants from Cellosaurus for lines DepMap",
                "does not cover. One row per variant; the source column shows the",
                "origin. Empty until dev/download_variants.R is run; the demo",
                "uses synthetic data."
              ),
              title = "Somatic variants"
            )
          )
        ),
        tahoe_table_ui(ns("variants"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Most frequently mutated genes"),
        echarts4r::echarts4rOutput(ns("mutated_genes_plot"), height = "360px")
      )
    )
  )
}

cell_line_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    all_cell_lines <- reactive(tahoe_cell_line())

    filtered <- reactive({
      df <- all_cell_lines()

      # Each filter is applied only when the user has supplied a value, so an
      # empty control never restricts the result set.
      if (length(input$organ) > 0 && "Organ" %in% names(df)) {
        df <- df[df$Organ %in% input$organ, , drop = FALSE]
      }
      if (length(input$gene) > 0 && "Driver_Gene_Symbol" %in% names(df)) {
        df <- df[df$Driver_Gene_Symbol %in% input$gene, , drop = FALSE]
      }
      if (length(input$var_type) > 0 && "Driver_VarType" %in% names(df)) {
        df <- df[df$Driver_VarType %in% input$var_type, , drop = FALSE]
      }
      term <- input$cell_name
      if (!is.null(term) && nzchar(term) && "cell_name" %in% names(df)) {
        keep <- stringr::str_detect(
          df$cell_name,
          stringr::fixed(term, ignore_case = TRUE)
        )
        df <- df[!is.na(keep) & keep, , drop = FALSE]
      }
      df
    })

    # Collapse the matched driver-level rows to one row per cell line (with a
    # drivers summary) for the table, organ chart, and export, so "cell lines"
    # counts distinct lines rather than the driver-level rows of the source
    # table. The driver-gene and variant-type charts keep the driver-level
    # `filtered` set below.
    filtered_lines <- reactive({
      names_matched <- unique(filtered()$cell_name)
      unique_lines <- tahoe_cell_line_unique()
      unique_lines[
        unique_lines$cell_name %in% names_matched,
        ,
        drop = FALSE
      ]
    })

    # Turn the DepMap ID into a clickable portal link when present, keeping
    # the original ID text as the link label.
    cell_line_cols <- function(df) {
      col_defs <- list()
      if ("Cell_ID_DepMap" %in% names(df)) {
        col_defs[["Cell_ID_DepMap"]] <- reactable::colDef(
          name = "DepMap ID",
          html = TRUE,
          cell = function(value) {
            if (is.na(value) || !nzchar(as.character(value))) {
              return("")
            }
            url <- paste0(.cell_line_depmap_url, value)
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

    tahoe_table_server(
      "table",
      data = filtered_lines,
      columns = cell_line_cols,
      empty_message = "No cell lines match the current filters."
    )

    # Somatic variants for the matching lines: an inner join of the external
    # variant table onto the filtered cell lines by cell name (every line has
    # one, so DepMap- and Cellosaurus-sourced rows both attach). Empty (not an
    # error) when the variant table is absent or nothing matches.
    variants <- reactive({
      v <- tryCatch(tahoe_cell_variants(), error = function(e) NULL)
      lines <- filtered_lines()
      if (
        is.null(v) ||
          nrow(v) == 0 ||
          !"cell_name" %in% names(v) ||
          !"cell_name" %in% names(lines)
      ) {
        return(dplyr::tibble(cell_name = character(), gene = character()))
      }
      keep <- intersect(c("cell_name", "Organ"), names(lines))
      dplyr::inner_join(lines[, keep, drop = FALSE], v, by = "cell_name")
    })

    # Column-trimmed, preferred-order view of the matching variants.
    variants_display <- reactive({
      v <- variants()
      if (is.null(v) || nrow(v) == 0) {
        return(v)
      }
      pref <- c(
        "cell_name",
        "source",
        "gene",
        "protein_change",
        "variant_type",
        "consequence",
        "zygosity",
        "hotspot",
        "likely_lof",
        "dbsnp"
      )
      v[, intersect(pref, names(v)), drop = FALSE]
    })

    tahoe_table_server(
      "variants",
      data = variants_display,
      empty_message = paste(
        "No somatic variants for the matching cell lines.",
        "Run dev/download_variants.R to load DepMap / Cellosaurus variants",
        "(the offline demo shows synthetic variants)."
      )
    )

    output$mutated_genes_plot <- echarts4r::renderEcharts4r({
      v <- variants()
      validate(need(
        nrow(v) > 0 && "gene" %in% names(v),
        "No variant data to plot."
      ))
      .cell_line_bar(v, "gene", tahoe_colors$orange)
    })

    output$organ_plot <- echarts4r::renderEcharts4r({
      df <- filtered_lines()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Organ", tahoe_colors$green)
    })

    output$gene_plot <- echarts4r::renderEcharts4r({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Driver_Gene_Symbol", tahoe_colors$blue)
    })

    output$var_type_plot <- echarts4r::renderEcharts4r({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Driver_VarType", tahoe_colors$orange)
    })

    subset_export_server(
      "export",
      data_reactive = filtered_lines,
      file_stem = "cell_lines_subset"
    )

    # --- Chat-assistant bridge: read and drive these filters.
    cl_bridge_get <- function() {
      df <- isolate(all_cell_lines())
      list(
        filters = list(
          organs = list(
            current = isolate(input$organ),
            options = .cell_line_choices(df, "Organ")
          ),
          driver_genes = list(
            current = isolate(input$gene),
            options = .cell_line_choices(df, "Driver_Gene_Symbol")
          ),
          variant_types = list(
            current = isolate(input$var_type),
            options = .cell_line_choices(df, "Driver_VarType")
          ),
          name = list(current = isolate(input$cell_name), kind = "text")
        ),
        matched_cell_lines = nrow(isolate(filtered_lines()))
      )
    }
    cl_bridge_set <- function(request) {
      df <- isolate(all_cell_lines())
      ignored <- list()
      apply_multi <- function(field, input_id, col) {
        v <- tahoe_bridge_validate(
          request[[field]],
          .cell_line_choices(df, col)
        )
        if (length(v$bad) > 0) {
          ignored[[field]] <<- v$bad
        }
        if (!is.null(v$good)) {
          updateSelectizeInput(session, input_id, selected = v$good)
        }
      }
      apply_multi("organs", "organ", "Organ")
      apply_multi("driver_genes", "gene", "Driver_Gene_Symbol")
      apply_multi("variant_types", "var_type", "Driver_VarType")
      if (!is.null(request$name)) {
        updateTextInput(
          session,
          "cell_name",
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
      "cell_lines",
      list(title = "Cell lines", get = cl_bridge_get, set = cl_bridge_set)
    )

    # Expose the collapsed one-row-per-cell-line reactive for callers; the
    # driver-level `filtered` remains available in-scope for tests.
    filtered_lines
  })
}
