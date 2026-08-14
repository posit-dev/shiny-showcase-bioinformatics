# Bring your own key (BYOK): the session-scoped credential layer.
#
# Users paste their own LLM API key in the app (Review tab). The key is held only
# in the Shiny session (a reactiveVal), passed straight to the ellmer constructor as
# `api_key` (never Sys.setenv - that is process-global and would leak across
# sessions on a hosted server), never written to disk, and redacted from any error
# surfaced to the UI. This honors CLAUDE.md rule #4 (no keys in the repo, none
# hardcoded): the key lives in memory for one session and nowhere else.
#
# A credential is a plain list(provider, api_key, model). byok_effective_config()
# folds it onto the static config as `provider` + per-provider `models` (from
# config.yml `byok:`) + `api_key`, so every existing LLM stage - which already
# threads `config` into build_chat() - picks up the user's provider, models, and
# key with no signature change, and it serializes cleanly into the mirai offload
# daemon (R/llm_offload.R) as a normal argument.

# The providers offered for BYOK: key-based only. google_vertex is intentionally
# excluded - it authenticates with OAuth / Application Default Credentials, not a
# pasteable key.
genescout_byok_providers <- function() {
  c("anthropic", "google_gemini", "openai")
}

# UI metadata per provider: a human label, the "get a key" console URL, and the
# environment variable the key would otherwise come from. Labels/URLs are UI copy,
# not engine logic - the model strings themselves stay in config.yml.
genescout_provider_meta <- function(provider) {
  switch(
    provider,
    anthropic = list(
      label = "Anthropic (Claude)",
      key_url = "https://console.anthropic.com/settings/keys",
      env = "ANTHROPIC_API_KEY"
    ),
    google_gemini = list(
      label = "Google (Gemini)",
      key_url = "https://aistudio.google.com/apikey",
      env = "GEMINI_API_KEY"
    ),
    openai = list(
      label = "OpenAI (GPT)",
      key_url = "https://platform.openai.com/api-keys",
      env = "OPENAI_API_KEY"
    ),
    list(label = provider, key_url = NULL, env = NA_character_)
  )
}

# Named choices (label -> provider id) for the provider selectInput.
genescout_provider_choices <- function() {
  ids <- genescout_byok_providers()
  labels <- vapply(
    ids,
    function(p) genescout_provider_meta(p)$label,
    character(1)
  )
  stats::setNames(ids, labels)
}

# The provider to preselect: the deploy's configured provider when it is one of the
# BYOK three (so an Anthropic-configured deploy defaults the picker to Anthropic),
# else the first offered provider.
genescout_default_byok_provider <- function(config = load_config()) {
  p <- config$provider %||% ""
  if (p %in% genescout_byok_providers()) p else genescout_byok_providers()[1]
}

# Suggested model ids for a provider's model picker: the distinct models from that
# provider's config.yml `byok:` map (capable + fast tiers + chat). Derived from
# config, so no model string is hardcoded here; the picker also lets the user type
# any id.
genescout_provider_model_suggestions <- function(provider) {
  models <- tryCatch(load_byok_models(provider), error = function(e) list())
  unique(unlist(models, use.names = FALSE))
}

# Construct a BYOK credential. `model` is an optional single-model override that, if
# set, supersedes every role (and the chat model) in byok_effective_config().
genescout_byok_credential <- function(provider, api_key, model = NULL) {
  list(
    provider = provider,
    api_key = trimws(api_key %||% ""),
    model = {
      m <- trimws(model %||% "")
      if (nzchar(m)) m else NULL
    }
  )
}

# TRUE when a credential carries a usable (non-empty) key.
genescout_credential_ready <- function(credential) {
  !is.null(credential) && nzchar(credential$api_key %||% "")
}

# Fold a BYOK credential onto the static config, producing the effective config the
# LLM stages run with. With no usable credential the base config is returned
# unchanged (so a plain .Renviron deploy is untouched and ellmer reads the key from
# the environment). Otherwise provider + per-role models + api_key are overridden; a
# single-model override (credential$model) is applied to every role.
byok_effective_config <- function(base_config, credential) {
  if (!genescout_credential_ready(credential)) {
    return(base_config)
  }
  models <- load_byok_models(credential$provider)
  if (!is.null(credential$model) && nzchar(credential$model)) {
    models <- lapply(models, function(...) credential$model)
  }
  eff <- base_config
  eff$provider <- credential$provider
  eff$models <- models
  eff$api_key <- credential$api_key
  eff
}

# The chat-assistant model for a credential: the single-model override if set, else
# the provider's configured `chat` model.
genescout_chat_model <- function(credential) {
  if (!is.null(credential$model) && nzchar(credential$model)) {
    return(credential$model)
  }
  load_byok_models(credential$provider)$chat
}

# Remove a secret from a message before it is shown or logged. Provider errors can
# echo the key back (e.g. inside a request URL), so redact literally.
genescout_redact_secret <- function(msg, secret = "") {
  msg <- paste(as.character(msg), collapse = " ")
  if (length(secret) == 1 && nzchar(secret)) {
    msg <- gsub(secret, "<redacted-key>", msg, fixed = TRUE)
  }
  msg
}

# Build the ellmer chat client for the assistant from a BYOK credential: the chat
# model + the grounded chat system prompt, with the key passed directly and echo
# off (so a streamed key can never reach the console/logs).
genescout_build_chat_client <- function(credential) {
  build_chat(
    provider = credential$provider,
    model = genescout_chat_model(credential),
    system_prompt = read_prompt("chat"),
    api_key = credential$api_key,
    echo = "none"
  )
}

# A compact, grounded snapshot of the current run for the chat assistant. The chat
# is scoped (by prompt and by this context) to the run's own grounded rankings; it
# has no free rein to assert biology. Grade is derived from the composite so this
# stays decoupled from the display table's exact columns.
genescout_chat_grounding <- function(result, max_genes = 15L) {
  if (is.null(result) || is.null(result$genes) || nrow(result$genes) == 0) {
    return(paste(
      "No review has been run in this session yet.",
      "You may explain how GeneScout works and how to read its results, but you have",
      "no gene rankings or evidence to discuss until the user runs a review on the",
      "Review tab. Do not invent genes, scores, or citations."
    ))
  }
  g <- result$genes
  n <- nrow(g)
  ctx_label <- pluck_at(result, "context", "label") %||%
    pluck_at(result, "context", "disease", "name") %||%
    "none"
  take <- head(g, max_genes)
  score <- suppressWarnings(as.numeric(take$composite))
  grade <- vapply(
    score,
    function(s) if (is.na(s)) "Insufficient" else grade_for_score(s),
    character(1)
  )
  lines <- sprintf(
    "%d. %s - Grade %s (composite %s)",
    seq_along(take$symbol),
    take$symbol,
    grade,
    ifelse(is.na(score), "NA", formatC(score, format = "f", digits = 2))
  )
  paste0(
    "CURRENT RUN (the only grounded material you may discuss):\n",
    sprintf(
      "Study context: %s. %d gene%s ranked.\n",
      ctx_label,
      n,
      if (n == 1) "" else "s"
    ),
    "Top candidates by composite rank:\n",
    paste(lines, collapse = "\n"),
    if (n > length(take$symbol)) {
      sprintf("\n...and %d more.", n - length(take$symbol))
    } else {
      ""
    },
    "\n\nPer-signal evidence and its source ids (PMIDs, ClinVar/Open Targets",
    " accessions) are shown in the app's ranked table and per-gene drill-down.",
    " Ground every claim in this material; if the user asks for something not",
    " present here, say it is not in the current evidence rather than guessing."
  )
}
