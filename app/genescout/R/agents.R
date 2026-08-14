# Agents (ellmer) - the LLM layer, reserved for the Phase 2 subjective-ranking
# agent that re-ranks/filters the deterministic gene_matrix to a usable 20-100.
# The deterministic pipeline (R/orchestrate.R) uses NO LLM; nothing here runs
# until that stage lands. Provider/model come from config (never hardcoded), so
# switching providers is a config change.
#
# Live pieces used today: build_chat() + provider_credentials_ready(), which back
# genescout_llm_available(). The narrate_* helpers and the specialist_tools allowlist
# are scaffolding for the agent stage. Everything is guarded: if {ellmer} is not
# installed or no credentials are set, the app runs deterministically.

# Roles -> which tool clients they may call. The specialist only ever sees its
# own allowlist, keeping contexts isolated.
specialist_tools <- list(
  `variant-effect` = c("vep_consequence", "gnomad_frequency", "clinvar_lookup"),
  `pathway-disease` = c("gene_disease_assoc"),
  literature = c("europepmc_search")
)

# Build an ellmer Chat for a role, using the provider + model from config and the
# matching system prompt from prompts/. Kept thin so provider swaps are config-only.
genescout_chat <- function(
  role_prompt,
  model_role = "specialist",
  config = load_config()
) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required for LLM narration.", call. = FALSE)
  }
  build_chat(
    provider = config$provider %||% "anthropic",
    model = model_for(model_role, config),
    system_prompt = read_prompt(role_prompt),
    # A session-scoped BYOK key rides on the config as `api_key` (R/byok.R);
    # NULL for a plain .Renviron deploy, where ellmer reads the key from the env.
    api_key = config$api_key
  )
}

# First non-empty environment variable among `vars`, or "". Lets a setting be read
# under more than one accepted name.
env_first <- function(...) {
  for (v in c(...)) {
    val <- Sys.getenv(v)
    if (nzchar(val)) {
      return(val)
    }
  }
  ""
}

# Vertex project + region from the environment. Accept ellmer's documented VERTEX_*
# names first, then the Google-standard GOOGLE_CLOUD_* names, so either convention in
# .Renviron works without renaming.
vertex_project_id <- function() {
  env_first("VERTEX_PROJECT_ID", "GOOGLE_CLOUD_PROJECT")
}
vertex_location <- function() {
  env_first("VERTEX_LOCATION", "GOOGLE_CLOUD_LOCATION")
}

# Map the configured provider to its ellmer constructor. Credentials come from the
# environment (see .Renviron.example) by default; a user-pasted BYOK key may instead
# be passed as `api_key` (held in the session, never written to disk/env - see
# R/byok.R). Adding a provider is a new case here plus its credential env var in
# provider_credentials_ready(); the rest of the engine is untouched.
#
# `api_key`: NULL/"" -> ellmer resolves the key from the environment (unchanged
# behavior). A non-empty value is passed straight to the constructor, superseding
# the env for this session. `echo`: NULL keeps ellmer's default; the chat assistant
# passes "none" so a streamed key can never reach the console/logs.
build_chat <- function(
  provider,
  model,
  system_prompt,
  api_key = NULL,
  echo = NULL
) {
  key <- if (is.null(api_key) || !nzchar(api_key)) NULL else api_key
  # Assemble the shared constructor args, adding api_key/echo only when supplied so
  # a plain deploy calls the constructor exactly as before.
  keyed_args <- function() {
    a <- list(model = model, system_prompt = system_prompt)
    if (!is.null(key)) {
      a$api_key <- key
    }
    if (!is.null(echo)) {
      a$echo <- echo
    }
    a
  }
  switch(
    provider,
    anthropic = do.call(ellmer::chat_anthropic, keyed_args()),
    google_gemini = do.call(ellmer::chat_google_gemini, keyed_args()),
    # Vertex authenticates with Application Default Credentials (OAuth), NOT an API
    # key, and requires the project + region positionally - passing them from the
    # environment (this call previously omitted them and errored on every use). ellmer
    # only *suggests* gargle (its Google OAuth backend), so guard with a clear message
    # instead of an opaque "package required" error; the requireNamespace() call also
    # keeps gargle in renv's dependency scan so renv.lock installs it.
    google_vertex = {
      if (!requireNamespace("gargle", quietly = TRUE)) {
        stop(
          "Vertex AI needs the 'gargle' package for Google OAuth. Install it ",
          "(renv::install('gargle')) or use provider google_gemini with an API key.",
          call. = FALSE
        )
      }
      ellmer::chat_google_vertex(
        location = vertex_location(),
        project_id = vertex_project_id(),
        model = model,
        system_prompt = system_prompt
      )
    },
    openai = do.call(ellmer::chat_openai, keyed_args()),
    stop(
      sprintf(
        paste0(
          "Unsupported provider '%s' in config.yml ",
          "(use anthropic, google_gemini, google_vertex, or openai)."
        ),
        provider
      ),
      call. = FALSE
    )
  )
}

# Does a credential exist for the given provider? A presence check only (nonblank
# key / env vars), not a live auth test. A session-scoped BYOK key (`api_key`)
# satisfies the check directly - it supersedes the environment. Otherwise the env
# is consulted: Vertex uses Application Default Credentials, so we gate on project +
# region being set; the actual ADC token (service-account JSON or `gcloud auth
# application-default login`) is resolved by ellmer/gargle at call time.
provider_credentials_ready <- function(provider, api_key = NULL) {
  if (!is.null(api_key) && nzchar(api_key)) {
    return(TRUE)
  }
  switch(
    provider,
    anthropic = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    google_gemini = nzchar(Sys.getenv("GEMINI_API_KEY")) ||
      nzchar(Sys.getenv("GOOGLE_API_KEY")),
    google_vertex = nzchar(vertex_project_id()) && nzchar(vertex_location()),
    openai = nzchar(Sys.getenv("OPENAI_API_KEY")),
    FALSE
  )
}

# Produce a short, grounded narrative for a candidate from its evidence. The
# prompt is constrained to the supplied evidence (each line carries a source id),
# so the model summarizes rather than recalls. Returns NA_character_ on any
# failure, so narration never breaks a review.
narrate_candidate <- function(
  symbol,
  evidence,
  context,
  config = load_config()
) {
  if (is.null(evidence) || nrow(evidence) == 0) {
    return(NA_character_)
  }
  tryCatch(
    {
      chat <- genescout_chat("pathway-disease", "specialist", config)
      chat$chat(build_narrative_prompt(symbol, evidence, context))
    },
    error = function(e) NA_character_
  )
}

# Assemble the grounding prompt: only the retrieved evidence rows (across all
# domains), each with its source id, plus the disease context label.
build_narrative_prompt <- function(symbol, evidence, context) {
  detail <- ifelse(
    is.na(evidence$detail) | !nzchar(evidence$detail),
    "",
    paste0(" - ", evidence$detail)
  )
  lines <- sprintf(
    "- [%s] %s%s [source: %s]",
    evidence$domain,
    evidence$title,
    detail,
    evidence$source_id
  )
  paste0(
    "Disease context: ",
    context$label %||% context$id %||% "unspecified",
    ".\n",
    "Gene: ",
    symbol,
    ".\n",
    "Grounded evidence (the ONLY evidence you may use):\n",
    paste(lines, collapse = "\n"),
    "\n\nIn 2-3 sentences, summarize how plausibly this gene relates to the ",
    "context, citing the evidence above. Do not introduce any fact not listed. ",
    "If support is weak or off-context, say so."
  )
}
