# Disease-context loader. A context is a YAML file of priors (relevant pathways,
# known drivers, tissues of interest) that scoring reads so the same engine
# generalizes across diseases. NF1 ships as the reference example.

# List available contexts as a named character vector (id -> id), for UI pickers.
list_contexts <- function(dir = genescout_app_path("context")) {
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = FALSE)
  ids <- sub("\\.ya?ml$", "", files)
  stats::setNames(ids, ids)
}

# Load a single context by id (e.g. "nf1") or by path, as a plain list.
load_context <- function(id, dir = genescout_app_path("context")) {
  path <- if (file.exists(id)) id else file.path(dir, paste0(id, ".yaml"))
  if (!file.exists(path)) {
    stop("Context not found: ", id, call. = FALSE)
  }
  yaml::read_yaml(path)
}
