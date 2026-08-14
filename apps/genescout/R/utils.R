app_version <- function() {
  "0.1.0"
}

safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }

  readRDS(path)
}

# Signal that a pipeline stage is not implemented yet. Raised by engine stubs so
# callers (the Shiny app, the CLI) can catch `genescout_not_implemented` and degrade
# gracefully instead of crashing.
not_implemented <- function(what) {
  msg <- sprintf("%s is not implemented yet - see PLAN.md.", what)
  cnd <- structure(
    class = c("genescout_not_implemented", "error", "condition"),
    list(message = msg, call = sys.call(-1))
  )
  stop(cnd)
}

# Read a system-prompt markdown file from prompts/ and return it as one string.
read_prompt <- function(name) {
  path <- file.path("prompts", paste0(name, ".md"))
  if (!file.exists(path)) {
    stop("Prompt file not found: ", path, call. = FALSE)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
