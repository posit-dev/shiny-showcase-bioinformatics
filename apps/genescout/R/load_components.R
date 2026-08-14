source_dir <- function(path, exclude = character()) {
  if (!dir.exists(path)) {
    return(invisible(NULL))
  }

  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  files <- sort(setdiff(files, exclude))

  lapply(files, source)
  invisible(files)
}

# Load utility functions and the engine from R/ (excluding this loader itself),
# then the tool clients in R/tools/, then the modules and page-level UI. The
# top-level R/ scan is non-recursive, so R/tools/ is sourced explicitly.
source_dir("R", exclude = file.path("R", "load_components.R"))
source_dir(file.path("R", "tools"))
source_dir("modules")
source_dir("userInterface")
