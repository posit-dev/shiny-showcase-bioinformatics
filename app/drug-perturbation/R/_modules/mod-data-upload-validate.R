# Data upload + validate Shiny module (classic). Canonical home of the
# data-upload-validate module; apps reuse it so users can bring their own data.
#
# Generic on the contract: pass the app's `read_*()` contract function (which
# reads a directory of CSVs, validates, and errors loudly) plus the expected
# file names. The module renders one file picker per contract file, writes the
# uploads to a temp dir under their expected names, runs the reader, shows a
# clear pass/fail, and returns the validated dataset (or NULL) as a reactive.
#
# The module needs no shared logic of its own -- the contract's `read_*()` /
# `validate_*()` do the work (see shared/R/*-data.R).

# id: module id. files: named character vector mapping a logical key -> the
#   expected filename (e.g. c(de_results = "de_results.csv", ...)). The label
#   shown is the filename.
data_upload_validate_ui <- function(id, files, accept = ".csv") {
  ns <- shiny::NS(id)
  shiny::tagList(
    lapply(names(files), function(k) {
      shiny::fileInput(ns(paste0("file_", k)), files[[k]], accept = accept)
    }),
    shiny::div(
      shiny::textOutput(ns("status")),
      class = "small text-muted"
    )
  )
}

# reader: function(dir) returning a validated dataset (erroring on invalid data).
# files: the same named vector passed to the UI.
# Returns a reactive: the validated dataset once every file is uploaded and
# valid, else NULL (with a status message explaining what's needed / wrong).
data_upload_validate_server <- function(id, reader, files) {
  shiny::moduleServer(id, function(input, output, session) {
    dataset <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("Upload the file(s) above to validate.")

    shiny::observe({
      uploads <- lapply(names(files), function(k) input[[paste0("file_", k)]])
      names(uploads) <- names(files)

      if (any(vapply(uploads, is.null, logical(1)))) {
        dataset(NULL)
        status(sprintf(
          "Upload all %d file(s) to validate: %s",
          length(files),
          toString(unname(files))
        ))
        return()
      }

      dir <- file.path(tempdir(), paste0("upload-", session$token))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      unlink(list.files(dir, full.names = TRUE))
      for (k in names(files)) {
        file.copy(uploads[[k]]$datapath, file.path(dir, files[[k]]), TRUE)
      }

      result <- tryCatch(reader(dir), error = function(e) e)
      if (inherits(result, "error")) {
        dataset(NULL)
        status(paste0("✗ Invalid: ", conditionMessage(result)))
      } else {
        dataset(result)
        status("✓ Valid - dataset loaded.")
      }
    })

    output$status <- shiny::renderText(status())
    dataset
  })
}
