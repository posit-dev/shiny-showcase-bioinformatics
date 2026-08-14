# Reusable export module.
#
# Downloads the current (filtered) data frame as CSV and/or parquet, and can
# optionally render a copy-paste "analysis recipe" supplied by the caller.
# Every explorer embeds this so the "pull a subset" behaviour is consistent.

subset_export_ui <- function(id, show_recipe = FALSE) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex gap-2 flex-wrap align-items-center",
      downloadButton(
        ns("csv"),
        "Download CSV",
        class = "btn-sm btn-primary"
      ),
      downloadButton(
        ns("parquet"),
        "Download Parquet",
        class = "btn-sm btn-outline-primary"
      ),
      tags$span(
        class = "text-muted small",
        textOutput(ns("n_rows"), inline = TRUE)
      )
    ),
    if (show_recipe) {
      recipe_id <- ns("recipe")
      tagList(
        tags$hr(),
        div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$strong("Analysis recipe"),
          tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf(
              paste0(
                "navigator.clipboard.writeText(",
                "document.getElementById('%s').innerText)"
              ),
              recipe_id
            ),
            "Copy"
          )
        ),
        tags$p(
          class = "text-muted small",
          "Pick a language, copy-paste the code to reproduce this selection on",
          " the full dataset, or download it as a notebook to start from."
        ),
        div(
          class = "d-flex gap-2 flex-wrap align-items-end mb-2",
          div(
            style = "min-width: 150px;",
            selectInput(
              ns("recipe_lang"),
              "Language",
              choices = c(
                "R (duckdb)" = "r",
                "Python (scanpy)" = "python"
              )
            )
          ),
          div(
            style = "min-width: 190px;",
            selectInput(
              ns("recipe_format"),
              "Notebook format",
              choices = c(
                "Quarto (.qmd)" = "qmd",
                "R Markdown (.Rmd)" = "rmd",
                "Jupyter (.ipynb)" = "ipynb"
              )
            )
          ),
          downloadButton(
            ns("recipe_dl"),
            "Download",
            class = "btn-sm btn-outline-primary mb-3"
          )
        ),
        verbatimTextOutput(ns("recipe"))
      )
    }
  )
}

# Resolve a value that may be a plain object or a (reactive) function.
.export_resolve <- function(x, default = NULL) {
  if (is.function(x)) {
    return(tryCatch(x(), error = function(e) default))
  }
  if (is.null(x)) default else x
}

subset_export_server <- function(
  id,
  data_reactive,
  file_stem = "tahoe_subset",
  recipe = NULL,
  recipe_parts = NULL
) {
  moduleServer(id, function(input, output, session) {
    current_data <- reactive({
      df <- .export_resolve(data_reactive, default = NULL)
      if (is.null(df)) data.frame() else df
    })

    stem <- reactive({
      value <- .export_resolve(file_stem, default = "tahoe_subset")
      if (is.null(value) || !nzchar(value)) "tahoe_subset" else value
    })

    output$n_rows <- renderText({
      sprintf("%s rows selected", format(nrow(current_data()), big.mark = ","))
    })

    output$csv <- downloadHandler(
      filename = function() paste0(stem(), ".csv"),
      content = function(file) {
        utils::write.csv(current_data(), file, row.names = FALSE)
      }
    )

    output$parquet <- downloadHandler(
      filename = function() paste0(stem(), ".parquet"),
      content = function(file) {
        df <- current_data()
        if (nrow(df) == 0 && ncol(df) == 0) {
          df <- data.frame(note = character())
        }
        con <- tahoe_con()
        # Unique view name: the duckdb connection is shared process-wide, so a
        # fixed name could clash if downloads ever run re-entrantly (async).
        view <- paste0("export_tmp_", as.integer(stats::runif(1, 0, 2e9)))
        duckdb::duckdb_register(con, view, df)
        on.exit(duckdb::duckdb_unregister(con, view), add = TRUE)
        DBI::dbExecute(
          con,
          sprintf(
            "COPY %s TO '%s' (FORMAT PARQUET)",
            view,
            gsub("'", "''", file, fixed = TRUE)
          )
        )
      }
    )

    # The recipe text: prefer the structured parts (which also power the
    # notebook download) and fall back to a plain recipe string. When parts are
    # available the display follows the selected language, so the on-screen code
    # matches what a download would contain.
    if (!is.null(recipe_parts)) {
      output$recipe <- renderText({
        parts <- .export_resolve(recipe_parts, default = NULL)
        if (is.null(parts)) {
          return("")
        }
        lang <- input$recipe_lang %||% "r"
        code <- if (identical(lang, "r")) parts$r_code else parts$py_code
        if (is.null(code)) {
          parts$recipe %||% ""
        } else {
          paste(c(parts$header, "", code), collapse = "\n")
        }
      })
    } else if (!is.null(recipe)) {
      output$recipe <- renderText({
        .export_resolve(recipe, default = "")
      })
    }

    if (!is.null(recipe_parts)) {
      recipe_ext <- c(rmd = ".Rmd", qmd = ".qmd", ipynb = ".ipynb")
      output$recipe_dl <- downloadHandler(
        filename = function() {
          fmt <- input$recipe_format %||% "qmd"
          paste0(stem(), "_recipe", recipe_ext[[fmt]])
        },
        content = function(file) {
          parts <- .export_resolve(recipe_parts, default = NULL)
          fmt <- input$recipe_format %||% "qmd"
          lang <- input$recipe_lang %||% "r"
          text <- if (is.null(parts)) {
            "No selection to export."
          } else {
            tahoe_subset_document(parts, format = fmt, language = lang)
          }
          writeLines(text, file, useBytes = TRUE)
        }
      )
    }

    invisible(current_data)
  })
}
