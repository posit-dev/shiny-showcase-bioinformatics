source_dir <- function(path, exclude = character()) {
  if (!dir.exists(path)) {
    return(invisible(NULL))
  }

  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  files <- sort(setdiff(files, exclude))

  lapply(files, source)
  invisible(files)
}

# Load utility functions from R/ (excluding this loader itself), then the
# modules and page-level UI definitions.
source_dir("R", exclude = file.path("R", "load_components.R"))
source_dir("modules")
source_dir("userInterface")
