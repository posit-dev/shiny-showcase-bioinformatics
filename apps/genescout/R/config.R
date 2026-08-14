# Configuration loader. Reads config.yml (provider + per-role model map) so the
# rest of the engine never hardcodes provider/model strings.
#
# config.yml shape:
#   default:
#     provider: anthropic
#     models:
#       orchestrator: <model>
#       caveats:      <model>
#       specialist:   <model>

# Absolute path to a file that lives under the app root. The Shiny app, the CLI,
# and the tests all run with the app root as the working directory, so a plain
# relative path works and GENESCOUT_APP_ROOT is unset. A hosted service, though (the
# plumber engine in dev/serve.R), serves requests with a working directory it does
# not control, so it sets GENESCOUT_APP_ROOT once at startup and the lazily-loaded
# files (rubric.yml, context/*.yaml) resolve regardless of the request-time CWD.
genescout_app_path <- function(rel) {
  root <- Sys.getenv("GENESCOUT_APP_ROOT", "")
  if (nzchar(root)) file.path(root, rel) else rel
}

# Load the active configuration profile as a plain list.
load_config <- function(path = "config.yml", profile = "default") {
  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)
  if (is.null(cfg[[profile]])) {
    stop("Config profile not found: ", profile, call. = FALSE)
  }
  cfg[[profile]]
}

# Resolve the model id for a pipeline role (orchestrator, caveats, specialist).
model_for <- function(role, config = load_config()) {
  model <- config$models[[role]]
  if (is.null(model)) {
    stop("No model configured for role: ", role, call. = FALSE)
  }
  model
}

# The per-provider role -> model map for BYOK (config.yml `byok:` block). A sibling
# of the profiles, so it is read from the whole file rather than a single profile.
# Returns a named list with the pipeline roles plus a `chat` model. Model strings
# stay in config; the BYOK layer (R/byok.R) reads them here, never hardcodes them.
load_byok_models <- function(provider, path = "config.yml") {
  path <- genescout_app_path(path)
  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)
  models <- cfg$byok[[provider]]
  if (is.null(models)) {
    stop("No BYOK model map configured for provider: ", provider, call. = FALSE)
  }
  models
}
