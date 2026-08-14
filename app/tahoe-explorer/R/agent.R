# Optional LLM assistant ("Chat" tab): configuration, gating, system prompt, and
# the ellmer client builder.
#
# Like R/async.R this is an OPT-IN, gracefully-degrading layer that must never
# break the default offline / CI-green app:
#   * The agent packages (ellmer, shinychat) are NOT attached in global.R; they
#     are used only via `requireNamespace()` + `pkg::` inside the enabled path.
#   * `tahoe_agent_available()` is FALSE unless the packages are installed AND a
#     Vertex project is configured AND the kill switch is off. When FALSE the Chat
#     page renders a setup panel instead of the chat widget (see modules/chat_mod.R).
#   * No credential/network check happens here: Vertex ADC is resolved lazily by
#     ellmer on the first turn, so a misconfigured host degrades to a clean
#     turn-time error, never a load-time crash.
#
# Backend: ellmer::chat_google_vertex() with Gemini (gemini-2.5-flash by default).
# Auth = gcloud Application Default Credentials (no API key). Config lives in the
# gitignored .Renviron (see .Renviron.example).

# First non-empty env var among `vars`, else `default`. Mirrors tahoe_hf_token().
.tahoe_env_first <- function(vars, default = "") {
  for (v in vars) {
    val <- Sys.getenv(v, unset = "")
    if (nzchar(val)) {
      return(val)
    }
  }
  default
}

#' GCP project id for Vertex AI. Required to enable the assistant.
.tahoe_vertex_project <- function() {
  .tahoe_env_first(c(
    "TAHOE_VERTEX_PROJECT",
    "VERTEX_PROJECT_ID",
    "GOOGLE_CLOUD_PROJECT",
    "GCLOUD_PROJECT"
  ))
}

#' Vertex region. Defaults to us-central1 (Gemini flash is broadly available).
.tahoe_vertex_location <- function() {
  .tahoe_env_first(
    c(
      "TAHOE_VERTEX_LOCATION",
      "VERTEX_LOCATION",
      "GOOGLE_CLOUD_LOCATION",
      "GOOGLE_CLOUD_REGION"
    ),
    default = "us-central1"
  )
}

#' Gemini model id (env-overridable; default per the locked decision).
tahoe_agent_model <- function() {
  .tahoe_env_first(c("TAHOE_VERTEX_MODEL", "GEMINI_MODEL"), "gemini-2.5-flash")
}

#' Sampling temperature (0-2). Low default for factual, grounded answers.
tahoe_agent_temperature <- function() {
  raw <- .tahoe_env_first(
    c("TAHOE_AGENT_TEMPERATURE", "GEMINI_TEMPERATURE"),
    "0.2"
  )
  val <- suppressWarnings(as.numeric(raw))
  if (is.na(val) || val < 0) 0.2 else val
}

# Bounds that keep cost/latency in check (also enforced tool-side).
.tahoe_agent_max_tokens <- function() 1024L
.tahoe_agent_row_cap <- function() 50L
.tahoe_agent_max_turns <- function() 25L

# --- Availability gating ------------------------------------------------------
#
# The assistant has two credential paths, and the Chat page is active if EITHER
# is usable:
#   * the shared server DEFAULT (Vertex ADC -- the operator's bill), and/or
#   * BYOK -- the user pastes their own provider API key (their bill).
# Splitting the gate lets a public deployment offer BYOK even when no shared
# default is configured. Everything here is UNCACHED (cheap Sys.getenv /
# requireNamespace) so env-toggling tests work, and NEVER touches the network.

# Global kill switch (the test suite sets TAHOE_AGENT_DISABLE=1).
.tahoe_agent_disabled <- function() {
  .tahoe_env_truthy(Sys.getenv("TAHOE_AGENT_DISABLE", unset = ""))
}

#' The chat packages are present -- the floor for ANY assistant path.
tahoe_agent_packages_ok <- function() {
  requireNamespace("ellmer", quietly = TRUE) &&
    requireNamespace("shinychat", quietly = TRUE)
}

#' Whether the shared DEFAULT (Vertex) assistant is usable: packages + gargle
#' (for ADC) + a configured project, and not force-disabled. gargle is a
#' Suggests of ellmer; requiring it here also pins it into renv.lock.
tahoe_agent_default_available <- function() {
  !.tahoe_agent_disabled() &&
    tahoe_agent_packages_ok() &&
    requireNamespace("gargle", quietly = TRUE) &&
    nzchar(.tahoe_vertex_project())
}

#' Back-compat alias: historically "available" meant the shared default path.
tahoe_agent_available <- function() {
  tahoe_agent_default_available()
}

#' Whether BYOK (user-supplied key) is offered: packages present, not
#' force-disabled, and not explicitly turned off. Defaults ON so a public
#' deployment can always fall back to a user's own key.
tahoe_agent_byok_enabled <- function() {
  !.tahoe_agent_disabled() &&
    tahoe_agent_packages_ok() &&
    .tahoe_env_truthy(Sys.getenv("TAHOE_AGENT_BYOK", unset = "1"))
}

#' Whether the Chat page is active (chat UI vs. static setup panel): either path.
tahoe_agent_enabled <- function() {
  tahoe_agent_default_available() || tahoe_agent_byok_enabled()
}

# BYOK providers we know how to build (all take a plain api_key). Vertex is the
# server default and is intentionally NOT a BYOK option: it needs ADC / service
# account credentials, not a pasteable key.
.tahoe_agent_known_byok <- c("gemini", "openai", "anthropic")

#' The BYOK providers to offer, from TAHOE_AGENT_BYOK_PROVIDERS (a comma list),
#' intersected with the known set so an unknown name can never reach a backend.
tahoe_agent_byok_providers <- function() {
  raw <- Sys.getenv(
    "TAHOE_AGENT_BYOK_PROVIDERS",
    unset = paste(.tahoe_agent_known_byok, collapse = ",")
  )
  parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  intersect(parts[nzchar(parts)], .tahoe_agent_known_byok)
}

#' UI metadata for a BYOK provider: a display label, where to get a key, and the
#' environment variables checked for a server-side key (so a user with a key
#' already in the environment can connect without pasting one).
.tahoe_agent_provider_meta <- function(provider) {
  switch(
    provider,
    gemini = list(
      label = "Google Gemini (your key)",
      key_url = "https://aistudio.google.com/apikey",
      env = c("GEMINI_API_KEY", "GOOGLE_API_KEY")
    ),
    openai = list(
      label = "OpenAI (your key)",
      key_url = "https://platform.openai.com/api-keys",
      env = "OPENAI_API_KEY"
    ),
    anthropic = list(
      label = "Anthropic Claude (your key)",
      key_url = "https://console.anthropic.com/settings/keys",
      env = "ANTHROPIC_API_KEY"
    ),
    list(label = provider, key_url = NULL, env = character(0))
  )
}

#' The environment key for a BYOK provider, or "" if none is set.
.tahoe_agent_provider_env_key <- function(provider) {
  .tahoe_env_first(.tahoe_agent_provider_meta(provider)$env)
}

#' The first offered BYOK provider that already has a key in the environment, or
#' NULL. Used to default the chat's "Model source" to a ready-to-connect
#' provider when no shared (Vertex) assistant is configured.
tahoe_agent_env_provider <- function() {
  for (p in tahoe_agent_byok_providers()) {
    if (nzchar(.tahoe_agent_provider_env_key(p))) {
      return(p)
    }
  }
  NULL
}

#' Suggested model ids for a BYOK provider's picker, overridable via
#' TAHOE_AGENT_MODELS_<PROVIDER> (a comma list). These are only a convenience:
#' the picker also accepts a typed-in id, and a blank model makes each backend
#' fall back to the provider's own default. Keep in sync with current model ids.
.tahoe_agent_provider_models <- function(provider) {
  env <- Sys.getenv(
    paste0("TAHOE_AGENT_MODELS_", toupper(provider)),
    unset = ""
  )
  if (nzchar(env)) {
    parts <- trimws(strsplit(env, ",", fixed = TRUE)[[1]])
    return(parts[nzchar(parts)])
  }
  switch(
    provider,
    gemini = c("gemini-2.5-flash", "gemini-2.5-pro", "gemini-2.0-flash"),
    openai = c("gpt-4.1", "gpt-4o", "gpt-4o-mini"),
    anthropic = c("claude-sonnet-5", "claude-opus-4-8", "claude-haiku-4-5"),
    character(0)
  )
}

# ellmer's live model-listing function for a BYOK provider (hits the provider's
# /models endpoint, scoped to the supplied key). NULL for an unknown provider.
# Looked up only on the enabled path, so ellmer is present.
.tahoe_agent_lister <- function(provider) {
  switch(
    provider,
    gemini = ellmer::models_google_gemini,
    openai = ellmer::models_openai,
    anthropic = ellmer::models_anthropic,
    NULL
  )
}

#' Fetch the CURRENT model ids a BYOK key can access, so the picker never offers
#' a stale or inaccessible model. Returns a character vector, or NULL on any
#' failure (unknown provider, blank/bad key, offline, unexpected shape) so the
#' caller falls back to the curated suggestions. The provider endpoints are
#' lightweight and the call is user-initiated (a button press).
.tahoe_agent_fetch_models <- function(provider, api_key) {
  fn <- .tahoe_agent_lister(provider)
  if (is.null(fn) || !nzchar(api_key %||% "")) {
    return(NULL)
  }
  df <- tryCatch(fn(api_key = api_key), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || !"id" %in% names(df)) {
    return(NULL)
  }
  # ellmer keeps the endpoint's own order (newest-first for OpenAI); strip any
  # "models/" prefix Gemini returns so the id round-trips to the chat backend.
  ids <- sub("^models/", "", as.character(df$id))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) NULL else ids
}

# --- System prompt -----------------------------------------------------------

# Markdown context files, assembled in order into the system prompt.
.tahoe_agent_prompt_files <- c(
  "00-persona.md",
  "10-dataset.md",
  "20-app-features.md",
  "30-data-schema.md",
  "40-subset-playbook.md",
  "50-guardrails.md",
  "60-followups.md"
)

# Where the prompt markdown lives: inst/agent/prompts under the app root. The app
# is sourced (not installed as a package), so this is always the on-disk path.
.tahoe_agent_prompt_dir <- function() {
  file.path(.tahoe_root, "inst", "agent", "prompts")
}

# Format a headline count for the live context block.
.tahoe_agent_num <- function(x) {
  if (length(x) != 1 || is.na(x)) "unknown" else format(x, big.mark = ",")
}

# A live "current session" block so the model uses real headline numbers and can
# caveat fixture vs real provenance. Degrades to "" if the data layer is
# unreachable, so prompt assembly never fails.
.tahoe_agent_live_context <- function() {
  cc <- tryCatch(tahoe_summary_counts(), error = function(e) NULL)
  if (is.null(cc)) {
    return("")
  }
  pin <- tryCatch(
    tahoe_dataset_pin(),
    error = function(e) list(repo = "tahoebio/Tahoe-100M", revision = "unknown")
  )
  paste0(
    "## Current session (live numbers -- prefer these over guessing)\n\n",
    sprintf("- Dataset: %s @ %s\n", pin$repo, substr(pin$revision, 1, 7)),
    sprintf(
      paste0(
        "- Live counts: drugs %s; assayed cell lines %s; samples %s; ",
        "plates %s; genes %s; cells %s.\n"
      ),
      .tahoe_agent_num(cc$drugs),
      .tahoe_agent_num(cc$cell_lines),
      .tahoe_agent_num(cc$samples),
      .tahoe_agent_num(cc$plates),
      .tahoe_agent_num(cc$genes),
      .tahoe_agent_num(cc$cells)
    ),
    sprintf(
      paste0(
        "- Provenance: small tables = %s, obs = %s. If 'fixture', these are ",
        "ILLUSTRATIVE synthetic numbers -- say so, and note real numbers need ",
        "the full dataset loaded.\n"
      ),
      cc$data_source %||% "unknown",
      cc$obs_source %||% "unknown"
    ),
    paste0(
      "\n## Driving the app\n\n",
      "You can see and control the app's interactive controls. When the user ",
      "asks to filter, select, or build something, actually DO it with the ",
      "page tools -- don't just describe it. Use get_active_page to find the ",
      "page the user is on; act on THAT page by default, and only target a ",
      "different page when the user names one (set_page_controls switches to it ",
      "for them). Check get_page_controls for the valid option values first, ",
      "then call set_page_controls. Report briefly what you changed.\n"
    )
  )
}

#' Assemble the assistant's system prompt from the markdown context files plus a
#' live headline-counts block. Returns a single string.
tahoe_agent_system_prompt <- function() {
  dir <- .tahoe_agent_prompt_dir()
  parts <- vapply(
    .tahoe_agent_prompt_files,
    function(f) {
      p <- file.path(dir, f)
      if (file.exists(p)) {
        paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      } else {
        ""
      }
    },
    character(1)
  )
  parts <- parts[nzchar(parts)]
  live <- .tahoe_agent_live_context()
  pieces <- c(parts, if (nzchar(live)) live else NULL)
  paste(pieces, collapse = "\n\n---\n\n")
}

# --- ellmer client -----------------------------------------------------------

# A backend is a constructor(system_prompt, params, echo, model, api_key) that
# returns an ellmer Chat. Kept as a small registry so tahoe_agent_client() is
# provider-agnostic and tests can inject fakes (no network, no real keys). An
# empty `model` means "use the provider's own default"; `api_key` is ignored by
# the Vertex backend, which authenticates via ADC.
.tahoe_agent_backends <- function() {
  list(
    vertex = function(system_prompt, params, echo, model = "", api_key = "") {
      ellmer::chat_google_vertex(
        location = .tahoe_vertex_location(),
        project_id = .tahoe_vertex_project(),
        model = if (nzchar(model)) model else tahoe_agent_model(),
        system_prompt = system_prompt,
        params = params,
        echo = echo
      )
    },
    gemini = function(system_prompt, params, echo, model = "", api_key = "") {
      .tahoe_agent_build_keyed(
        ellmer::chat_google_gemini,
        system_prompt,
        params,
        echo,
        model,
        api_key
      )
    },
    openai = function(system_prompt, params, echo, model = "", api_key = "") {
      .tahoe_agent_build_keyed(
        ellmer::chat_openai,
        system_prompt,
        params,
        echo,
        model,
        api_key
      )
    },
    anthropic = function(
      system_prompt,
      params,
      echo,
      model = "",
      api_key = ""
    ) {
      .tahoe_agent_build_keyed(
        ellmer::chat_anthropic,
        system_prompt,
        params,
        echo,
        model,
        api_key
      )
    }
  )
}

# Shared builder for the key-based providers: pass `model` only when supplied,
# otherwise let ellmer pick the provider default.
.tahoe_agent_build_keyed <- function(
  fn,
  system_prompt,
  params,
  echo,
  model,
  api_key
) {
  args <- list(
    system_prompt = system_prompt,
    api_key = api_key,
    params = params,
    echo = echo
  )
  if (nzchar(model)) {
    args$model <- model
  }
  do.call(fn, args)
}

#' The shared-default credential (Vertex ADC). See tahoe_agent_client().
tahoe_agent_default_credential <- function() {
  list(kind = "vertex")
}

#' A BYOK credential from a user-supplied key.
tahoe_agent_byok_credential <- function(provider, api_key, model = "") {
  list(kind = "byok", provider = provider, api_key = api_key, model = model)
}

# Redact a known secret from a message before it is shown or logged. Provider
# errors can echo the key (e.g. in a URL); this keeps it out of the UI and logs.
.tahoe_agent_redact <- function(msg, secret = "") {
  msg <- paste(as.character(msg), collapse = " ")
  if (length(secret) == 1 && nzchar(secret)) {
    msg <- gsub(secret, "<redacted-key>", msg, fixed = TRUE)
  }
  msg
}

# Map a raw provider/client error to a short, user-safe message. The known
# secret is redacted first, then common failure modes (bad key, quota / rate
# limit, model not found or unsupported, overloaded) are matched to friendly
# text; anything else falls back to a trimmed raw message. Used so a turn-time
# error surfaces as a chat message instead of crashing the Shiny session.
.tahoe_agent_friendly_error <- function(msg, secret = "") {
  raw <- .tahoe_agent_redact(msg, secret)
  low <- tolower(raw)
  has <- function(...) {
    any(vapply(c(...), function(p) grepl(p, low, fixed = TRUE), logical(1)))
  }
  if (
    has(
      "api key not valid",
      "api_key_invalid",
      "invalid api key",
      "invalid authentication",
      "unauthenticated",
      "unauthorized",
      "permission_denied",
      "permission denied",
      "401",
      "403"
    )
  ) {
    "Your API key was rejected or lacks access to this model. Check the key and try again."
  } else if (
    has(
      "credit",
      "prepay",
      "billing",
      "depleted",
      "insufficient_quota",
      "insufficient funds",
      "payment required",
      "402"
    )
  ) {
    "This key's billing or prepaid credits are exhausted. Top up billing, or switch to a different key or model."
  } else if (
    has(
      "resource_exhausted",
      "rate limit",
      "rate_limit",
      "quota",
      "429",
      "too many requests"
    )
  ) {
    "Rate limit or quota reached for this key. Wait a moment, or try a different model."
  } else if (
    has(
      "overloaded",
      "unavailable",
      "503",
      "try again later",
      "high demand"
    )
  ) {
    "The model is busy or temporarily unavailable. Try again shortly, or switch models."
  } else if (
    has(
      "not found",
      "model_not_found",
      "does not exist",
      "unknown model",
      "invalid model",
      "404"
    )
  ) {
    "That model isn't available for your account. Pick another model or type a valid id."
  } else if (has("invalid_argument", "unsupported", "400", "bad request")) {
    "The request was rejected (often an unsupported model or parameter). Try a different model."
  } else {
    paste0("The assistant hit an error: ", substr(raw, 1, 300))
  }
}

#' Build a per-session ellmer Chat for a `credential`, with the Tahoe tool suite
#' registered. `credential` is either the Vertex default or a BYOK spec (see the
#' `tahoe_agent_*_credential` helpers); the backend is chosen from its provider.
#' Constructed per Shiny session so each user has an isolated conversation.
#' `backends` is injectable so tests exercise selection without a network or a
#' real key. Only called on an enabled path, so the chat packages are present.
tahoe_agent_client <- function(
  credential = tahoe_agent_default_credential(),
  system_prompt = tahoe_agent_system_prompt(),
  tools = tahoe_agent_tools(),
  backends = .tahoe_agent_backends()
) {
  provider <- if (identical(credential$kind, "byok")) {
    credential$provider
  } else {
    "vertex"
  }
  ctor <- backends[[provider]]
  if (is.null(ctor)) {
    stop("unknown chat provider: ", provider)
  }
  if (
    identical(credential$kind, "byok") && !nzchar(credential$api_key %||% "")
  ) {
    stop("an API key is required")
  }
  params <- ellmer::params(
    temperature = tahoe_agent_temperature(),
    max_tokens = .tahoe_agent_max_tokens()
  )
  chat <- ctor(
    system_prompt = system_prompt,
    params = params,
    echo = "none",
    model = credential$model %||% "",
    api_key = credential$api_key %||% ""
  )
  for (spec in tools) {
    chat$register_tool(.tahoe_agent_ellmer_tool(spec))
  }
  chat
}
