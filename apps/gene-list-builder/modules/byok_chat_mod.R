# BYOK Chat module
#
# A shinychat chat UI backed by a per-session ellmer Chat, with "bring your own
# key" (BYOK) inputs: any user can paste their own Google Gemini, OpenAI, or
# Anthropic API key (their bill, held only in this session's server memory).
# Keys may also be read from the environment (OPENAI_API_KEY, ANTHROPIC_API_KEY,
# GEMINI_API_KEY / GOOGLE_API_KEY) so the same module doubles as a shared,
# operator-funded assistant without any pasting.
#
# Opt-in and gracefully degrading: when ellmer/shinychat are not installed the
# UI renders a setup panel built purely from bslib/shiny -- it references NO
# ellmer/shinychat symbols -- so an app that merely sources this module still
# loads fine without those packages.
#
# Drop-in generic: pass a `system_prompt` to steer the assistant, and a list of
# `ellmer::tool()` objects as `tools` to give it tools over your app's data.

# `%||%` lives in R/utils.R, which load_components.R sources before modules/.

# --- Provider registry --------------------------------------------------------
#
# Providers we know how to build. Each takes a plain, pasteable api_key (Google
# Vertex is intentionally excluded -- it needs ADC / service-account creds, not
# a pasteable key). Everything here is uncached (cheap Sys.getenv /
# requireNamespace) and NEVER touches the network, so gating stays load-safe.
.byok_chat_known_providers <- c("gemini", "openai", "anthropic")

# UI metadata for a provider: a display label, where to get a key, and the env
# vars checked for a server-side fallback key.
.byok_chat_provider_meta <- function(provider) {
  switch(
    provider,
    gemini = list(
      label = "Google Gemini",
      key_url = "https://aistudio.google.com/apikey",
      env = c("GEMINI_API_KEY", "GOOGLE_API_KEY")
    ),
    openai = list(
      label = "OpenAI",
      key_url = "https://platform.openai.com/api-keys",
      env = "OPENAI_API_KEY"
    ),
    anthropic = list(
      label = "Anthropic Claude",
      key_url = "https://console.anthropic.com/settings/keys",
      env = "ANTHROPIC_API_KEY"
    ),
    list(label = provider, key_url = NULL, env = character(0))
  )
}

# Suggested model ids for a provider's picker. Only a convenience: the picker
# also accepts a typed-in id (create = TRUE), and a blank model makes each
# backend fall back to the provider's own default. Keep in sync with live ids.
.byok_chat_provider_models <- function(provider) {
  switch(
    provider,
    gemini = c(
      "gemini-flash-lite-latest",
      "gemini-2.5-flash",
      "gemini-2.5-pro",
      "gemini-2.0-flash"
    ),
    openai = c("gpt-4.1", "gpt-4o", "gpt-4o-mini"),
    anthropic = c("claude-sonnet-5", "claude-opus-4-8", "claude-haiku-4-5"),
    character(0)
  )
}

# The default model id to use for a provider when the user hasn't picked one.
# Gemini defaults to the cheap/fast flash-lite; other providers keep "" so the
# backend uses its own default (avoids sending a provider-specific id elsewhere).
.byok_chat_default_model <- function(provider) {
  switch(provider, gemini = "gemini-flash-lite-latest", "")
}

# The ellmer constructor for a provider (looked up only on the enabled path, so
# ellmer is present). Returns NULL for an unknown provider.
.byok_chat_constructor <- function(provider) {
  switch(
    provider,
    gemini = ellmer::chat_google_gemini,
    openai = ellmer::chat_openai,
    anthropic = ellmer::chat_anthropic,
    NULL
  )
}

# ellmer's live model-listing function for a provider (hits the provider's
# /models endpoint, scoped to the supplied key). NULL for an unknown provider.
.byok_chat_lister <- function(provider) {
  switch(
    provider,
    gemini = ellmer::models_google_gemini,
    openai = ellmer::models_openai,
    anthropic = ellmer::models_anthropic,
    NULL
  )
}

# Fetch the CURRENT model ids a key can access, newest-first, so the picker
# never offers a stale or unavailable model. Returns a character vector, or NULL
# on any failure (bad key, offline, unexpected shape) so the caller falls back
# to the curated suggestions. The provider endpoints are lightweight; the call
# is user-initiated (a button), and any error degrades to the fallback.
.byok_chat_fetch_models <- function(provider, api_key) {
  fn <- .byok_chat_lister(provider)
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

# Normalize a caller-supplied provider vector against the known set, so an
# unknown name can never reach a backend.
.byok_chat_providers <- function(providers) {
  intersect(providers, .byok_chat_known_providers)
}

# The chat packages are present -- the floor for the interactive path.
.byok_chat_packages_ok <- function() {
  requireNamespace("ellmer", quietly = TRUE) &&
    requireNamespace("shinychat", quietly = TRUE)
}

# First non-empty environment variable among `vars`, else "".
.byok_chat_env_first <- function(vars) {
  for (v in vars) {
    val <- Sys.getenv(v, unset = "")
    if (nzchar(val)) {
      return(val)
    }
  }
  ""
}

# Name of the first non-empty environment variable among `vars`, else "". Used
# to tell the user WHICH env var a key was picked up from.
.byok_chat_env_var <- function(vars) {
  for (v in vars) {
    if (nzchar(Sys.getenv(v, unset = ""))) {
      return(v)
    }
  }
  ""
}

# The first offered provider that already has a key in the environment, or NULL.
# Lets the UI default the picker to a ready-to-connect provider and the server
# connect without the user pasting anything.
.byok_chat_env_provider <- function(providers) {
  for (p in providers) {
    if (nzchar(.byok_chat_env_first(.byok_chat_provider_meta(p)$env))) {
      return(p)
    }
  }
  NULL
}

# --- ellmer client ------------------------------------------------------------

# Build a per-session ellmer Chat for a provider + key, registering any tools.
# `model` == "" means "use the provider's own default". Only called on the
# enabled path, so ellmer is present.
.byok_chat_build_client <- function(
  provider,
  api_key,
  model = "",
  system_prompt = "",
  tools = list(),
  temperature = 0.2,
  max_tokens = 1024L
) {
  ctor <- .byok_chat_constructor(provider)
  if (is.null(ctor)) {
    stop("unknown chat provider: ", provider)
  }
  if (!nzchar(api_key %||% "")) {
    stop("an API key is required")
  }
  args <- list(
    system_prompt = system_prompt,
    api_key = api_key,
    params = ellmer::params(
      temperature = temperature,
      max_tokens = max_tokens
    ),
    echo = "none"
  )
  if (nzchar(model)) {
    args$model <- model
  }
  chat <- do.call(ctor, args)
  for (tool in tools) {
    chat$register_tool(tool)
  }
  chat
}

# Redact a known secret from a message before it is shown or logged. Provider
# errors can echo the key (e.g. in a URL); this keeps it out of the UI and logs.
.byok_chat_redact <- function(msg, secret = "") {
  msg <- paste(as.character(msg), collapse = " ")
  if (length(secret) == 1 && nzchar(secret)) {
    msg <- gsub(secret, "<redacted-key>", msg, fixed = TRUE)
  }
  msg
}

# Map a raw provider/client error to a short, user-safe message. Classification
# runs on the RAW text (so a short secret that happens to be a substring of the
# error can't corrupt the match); the secret is redacted only from the raw echo
# in the fallback branch. Used so a turn-time error surfaces as a chat message
# instead of crashing the Shiny session.
.byok_chat_friendly_error <- function(msg, secret = "") {
  raw <- paste(as.character(msg), collapse = " ")
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
    has("overloaded", "unavailable", "503", "try again later", "high demand")
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
    paste0(
      "The assistant hit an error: ",
      substr(.byok_chat_redact(raw, secret), 1, 300)
    )
  }
}

# --- UI ------------------------------------------------------------------------

# Setup panel shown when the assistant is off. Uses only bslib/shiny, so it is
# safe to evaluate at UI-build time even when ellmer/shinychat are not installed.
.byok_chat_disabled_panel <- function() {
  bslib::card(
    bslib::card_header("Enable the AI assistant"),
    bslib::card_body(
      p("The AI assistant is off because its packages are not installed:"),
      tags$ol(
        tags$li(
          "Install the ",
          tags$code("ellmer"),
          " and ",
          tags$code("shinychat"),
          " R packages."
        ),
        tags$li(
          "Then bring your own API key (Google Gemini, OpenAI, or Anthropic) in",
          " the chat sidebar, or set the matching environment variable (",
          tags$code("OPENAI_API_KEY"),
          ", ",
          tags$code("ANTHROPIC_API_KEY"),
          ", ",
          tags$code("GEMINI_API_KEY"),
          ")."
        )
      )
    )
  )
}

byok_chat_ui <- function(
  id,
  title = "AI assistant",
  subtitle = NULL,
  providers = c("gemini", "openai", "anthropic"),
  height = "100%",
  sidebar_width = 320,
  list_models = TRUE,
  placeholder = "Ask me anything...",
  greeting = NULL
) {
  ns <- NS(id)

  header <- NULL
  if (!is.null(title) || !is.null(subtitle)) {
    header <- div(
      class = "px-2 pt-1 pb-2",
      if (!is.null(title)) h3(title, class = "h5 mb-1"),
      if (!is.null(subtitle)) p(class = "text-muted small mb-0", subtitle)
    )
  }

  providers <- .byok_chat_providers(providers)
  # If the chat packages are missing (or no known provider was requested) render
  # the static setup panel -- crucially with NO shinychat/ellmer symbols.
  if (!.byok_chat_packages_ok() || length(providers) == 0) {
    return(tagList(header, .byok_chat_disabled_panel()))
  }

  # Names are user-facing labels; values are stable provider ids.
  choices <- stats::setNames(
    providers,
    vapply(
      providers,
      function(p) .byok_chat_provider_meta(p)$label,
      character(1)
    )
  )

  # Model / key controls, folded into a collapsed accordion so the chat gets
  # most of the room. This "dock" layout (accordion on top, chat filling the
  # rest) lets the whole module drop into an app-wide bslib::sidebar without a
  # sidebar-inside-a-sidebar. Expand the section to switch provider or paste a
  # key. `sidebar_width` is unused in dock form (kept for call-site compat).
  # Default the picker to a provider whose key is already in the environment, so
  # the assistant can connect without any pasting (see the server auto-connect).
  default_provider <- .byok_chat_env_provider(providers) %||% unname(choices)[1]
  config <- tagList(
    selectInput(
      ns("provider"),
      "Provider",
      choices = choices,
      selected = default_provider
    ),
    uiOutput(ns("key_help")),
    passwordInput(
      ns("api_key"),
      "API key",
      placeholder = "Paste your key (kept in this session only)"
    ),
    # Model picker: choose a suggested model or type any id the key supports
    # (create = TRUE). Left empty -> the provider's own default. Repopulated
    # per provider in the server.
    selectizeInput(
      ns("model"),
      "Model",
      choices = character(0),
      selected = character(0),
      multiple = FALSE,
      options = list(
        create = TRUE,
        placeholder = "Provider default - pick or type a model"
      )
    ),
    tags$p(
      class = "text-muted small mb-1",
      "Not listed? Type any model id your key supports and press Enter."
    ),
    # Pull the CURRENT, key-scoped model list from the provider so the picker
    # never offers a stale or inaccessible model.
    if (isTRUE(list_models)) {
      actionButton(
        ns("refresh_models"),
        "List models for this key",
        class = "btn-outline-secondary btn-sm mb-1"
      )
    },
    if (isTRUE(list_models)) uiOutput(ns("model_source")),
    div(
      class = "d-flex gap-2 mb-2",
      actionButton(ns("connect"), "Connect", class = "btn-primary btn-sm"),
      actionButton(
        ns("forget"),
        "Forget key",
        class = "btn-outline-secondary btn-sm"
      )
    ),
    uiOutput(ns("cred_status")),
    tags$p(
      class = "text-muted small mb-0",
      "Your key is held only in this browser session's server memory and is",
      " never written to disk. Switching provider starts a fresh conversation."
    )
  )

  bslib::as_fill_carrier(div(
    class = "byok-chat-dock",
    tags$style(HTML(paste0(
      ".byok-chat-dock .form-group,",
      ".byok-chat-dock .shiny-input-container{margin-bottom:0.4rem !important;}",
      ".byok-chat-dock .accordion-body{padding:0.6rem 0.8rem;}"
    ))),
    header,
    bslib::accordion(
      open = FALSE,
      class = "mb-2",
      bslib::accordion_panel(
        "Model & key",
        icon = shiny::icon("gear"),
        config
      )
    ),
    shinychat::chat_ui(
      ns("chat"),
      height = height,
      fill = TRUE,
      placeholder = placeholder,
      greeting = greeting %||%
        paste(
          "Hi! Pick a provider and click **Connect** (a key in your environment",
          "is used automatically) to start chatting."
        )
    )
  ))
}

# --- Server --------------------------------------------------------------------

#' Chat server. `system_prompt` steers the assistant; `tools` is a list of
#' `ellmer::tool()` objects registered on the client. `client_factory(provider,
#' api_key, model)` builds the ellmer Chat (defaults to the built-in builder);
#' `append` sends a response to the chat widget (defaults to
#' shinychat::chat_append). Both are injectable so tests drive the module with a
#' stub client and a recorder -- no network, no credentials. Injecting a
#' `client_factory` also forces the enabled path even when the packages are
#' unavailable.
byok_chat_server <- function(
  id,
  system_prompt = "You are a helpful assistant.",
  tools = list(),
  providers = c("gemini", "openai", "anthropic"),
  temperature = 0.2,
  max_tokens = 1024L,
  max_turns = 25L,
  list_models = TRUE,
  auto_connect_env = TRUE,
  fetch_models = NULL,
  client_factory = NULL,
  append = NULL
) {
  moduleServer(id, function(input, output, session) {
    enabled <- !is.null(client_factory) || .byok_chat_packages_ok()
    if (!enabled) {
      return(invisible(NULL))
    }

    providers <- .byok_chat_providers(providers)

    factory <- if (is.null(client_factory)) {
      function(provider, api_key, model) {
        .byok_chat_build_client(
          provider,
          api_key,
          model,
          system_prompt = system_prompt,
          tools = tools,
          temperature = temperature,
          max_tokens = max_tokens
        )
      }
    } else {
      client_factory
    }

    do_append <- if (is.null(append)) {
      # Pass the PLAIN id "chat" -- inside moduleServer shinychat namespaces
      # against the module session; session$ns("chat") would double-namespace.
      function(response) shinychat::chat_append("chat", response)
    } else {
      append
    }
    do_clear <- function() {
      tryCatch(shinychat::chat_clear("chat"), error = function(e) NULL)
    }

    client <- reactiveVal(NULL)
    n_turns <- reactiveVal(0L)
    status <- reactiveVal(list(ok = FALSE, msg = "Enter a key and Connect."))
    # The active key, held only to redact it from any surfaced error. "" before
    # a key is connected.
    active_secret <- reactiveVal("")
    # Note shown under the "List models" button (fallback vs. live-loaded count).
    model_note <- reactiveVal(NULL)

    # Live model lister -- injectable so tests avoid the network. Defaults to the
    # real ellmer-backed fetch.
    lister <- if (is.null(fetch_models)) {
      .byok_chat_fetch_models
    } else {
      fetch_models
    }

    # Build the session client for a provider + key and reset the conversation.
    # `from_env` tags a key picked up from the environment so the status line can
    # say which variable it came from. `secret` is redacted from any error.
    connect_client <- function(prov, key, model = "", from_env = FALSE) {
      cl <- tryCatch(
        factory(prov, key, model),
        error = function(e) {
          status(list(
            ok = FALSE,
            msg = paste0(
              "Could not connect: ",
              .byok_chat_redact(conditionMessage(e), key)
            )
          ))
          NULL
        }
      )
      if (!is.null(cl)) {
        meta <- .byok_chat_provider_meta(prov)
        env_var <- .byok_chat_env_var(meta$env)
        msg <- if (from_env && nzchar(env_var)) {
          paste0("Connected: ", meta$label, " (using ", env_var, " from env).")
        } else {
          paste0("Connected: ", meta$label, ".")
        }
        status(list(ok = TRUE, msg = msg))
        active_secret(key)
      }
      client(cl)
      n_turns(0L)
      do_clear()
    }

    # React to the provider selector: reset the conversation, repopulate the
    # model picker, and -- when a key for this provider is already in the
    # environment -- connect immediately (auto_connect_env) so the assistant is
    # ready without any pasting. Otherwise wait for the user to Connect.
    observeEvent(input$provider, {
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return()
      }
      client(NULL)
      active_secret("")
      do_clear()
      default_model <- .byok_chat_default_model(prov)
      updateSelectizeInput(
        session,
        "model",
        choices = .byok_chat_provider_models(prov),
        # Pre-select the provider's default (e.g. gemini-flash-lite-latest) so
        # the picker shows what will be used; "" leaves it at provider default.
        selected = if (nzchar(default_model)) default_model else character(0),
        server = FALSE
      )
      model_note(NULL)
      env_key <- .byok_chat_env_first(.byok_chat_provider_meta(prov)$env)
      if (isTRUE(auto_connect_env) && nzchar(env_key)) {
        connect_client(prov, env_key, default_model, from_env = TRUE)
      } else if (nzchar(env_key)) {
        status(list(
          ok = FALSE,
          msg = paste(
            "A key was found in your environment - pick a model and click",
            "Connect (no need to paste)."
          )
        ))
      } else {
        status(list(ok = FALSE, msg = "Enter your key and click Connect."))
      }
    })

    # Fetch the provider's live, key-scoped model list and repopulate the picker.
    # Keeps whatever the user already typed selected; on failure we keep the
    # curated fallback and say so, so this can never strand the user.
    observeEvent(input$refresh_models, {
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      if (!nzchar(key)) {
        key <- .byok_chat_env_first(.byok_chat_provider_meta(prov)$env)
      }
      if (!nzchar(key)) {
        model_note(list(
          ok = FALSE,
          msg = "Enter a key first to list its models."
        ))
        return()
      }
      model_note(list(ok = FALSE, msg = "Fetching models..."))
      ids <- tryCatch(lister(prov, key), error = function(e) NULL)
      if (is.null(ids) || length(ids) == 0) {
        model_note(list(
          ok = FALSE,
          msg = "Couldn't fetch models for this key -- showing suggestions. Type an id if needed."
        ))
        return()
      }
      updateSelectizeInput(
        session,
        "model",
        choices = ids,
        selected = isolate(trimws(input$model %||% "")),
        server = FALSE
      )
      model_note(list(
        ok = TRUE,
        msg = paste0(
          length(ids),
          " models loaded live from ",
          .byok_chat_provider_meta(prov)$label,
          "."
        )
      ))
    })

    observeEvent(input$connect, {
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      from_env <- FALSE
      # Fall back to a server-side env key so the module works without pasting.
      if (!nzchar(key)) {
        key <- .byok_chat_env_first(.byok_chat_provider_meta(prov)$env)
        from_env <- nzchar(key)
      }
      if (!nzchar(key)) {
        status(list(
          ok = FALSE,
          msg = "Paste an API key (or set the provider's environment variable)."
        ))
        return()
      }
      model <- trimws(input$model %||% "")
      # Blank field -> the provider's default model (gemini-flash-lite-latest
      # for Gemini), not the backend's own default.
      if (!nzchar(model)) {
        model <- .byok_chat_default_model(prov)
      }
      connect_client(prov, key, model, from_env = from_env)
      # Clear the visible key field; the built client already holds what it needs.
      updateTextInput(session, "api_key", value = "")
    })

    observeEvent(input$forget, {
      client(NULL)
      active_secret("")
      updateTextInput(session, "api_key", value = "")
      status(list(ok = FALSE, msg = "Key forgotten. Enter a key and Connect."))
      do_clear()
    })

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      if (is.null(msg) || !nzchar(trimws(msg))) {
        return()
      }
      cl <- client()
      if (is.null(cl)) {
        do_append("Connect a provider and key in the sidebar before chatting.")
        return()
      }
      if (n_turns() >= max_turns) {
        do_append(paste(
          "_Turn limit reached for this session._",
          "_Reload the page to start a new conversation._"
        ))
        return()
      }
      n_turns(n_turns() + 1L)
      secret <- active_secret()

      # Guard BOTH failure paths so a provider error (bad key, quota, unknown
      # model, overload) surfaces as a chat message instead of an unhandled
      # observer error -- which would tear down the Shiny session:
      #   * stream_async() / chat_append() throwing synchronously -> tryCatch
      #   * the streaming promise rejecting mid-turn -> onRejected
      p <- tryCatch(
        # stream = "text" yields only the assistant's prose, so tool calls and
        # tool results are never rendered in the chat (shinychat shows those
        # only when the stream is "content").
        do_append(cl$stream_async(msg, stream = "text")),
        error = function(e) {
          do_append(.byok_chat_friendly_error(conditionMessage(e), secret))
          NULL
        }
      )
      if (!is.null(p) && inherits(p, "promise")) {
        promises::then(
          p,
          onRejected = function(err) {
            do_append(.byok_chat_friendly_error(conditionMessage(err), secret))
          }
        )
      }
    })

    output$key_help <- renderUI({
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return(NULL)
      }
      meta <- .byok_chat_provider_meta(prov)
      env_found <- nzchar(.byok_chat_env_first(meta$env))
      tagList(
        if (!is.null(meta$key_url)) {
          tags$p(
            class = "small mb-1",
            tags$a(
              href = meta$key_url,
              target = "_blank",
              rel = "noopener",
              "Get a key"
            ),
            " for ",
            meta$label,
            "."
          )
        },
        if (env_found) {
          tags$p(
            class = "small text-success mb-1",
            "✓ A key was found in your environment - leave the field",
            " blank and just Connect."
          )
        }
      )
    })

    output$model_source <- renderUI({
      note <- model_note()
      if (is.null(note)) {
        return(NULL)
      }
      cls <- if (isTRUE(note$ok)) "text-success" else "text-muted"
      div(class = paste("small mb-2", cls), note$msg)
    })

    output$cred_status <- renderUI({
      st <- status()
      cls <- if (isTRUE(st$ok)) "text-success" else "text-muted"
      div(class = paste("small mb-2", cls), st$msg)
    })

    # These status outputs live in a collapsed accordion, so they would suspend
    # and never render until first opened; the connection status must stay live
    # regardless (e.g. the auto-connect note shown before the user expands it).
    for (nm in c("key_help", "model_source", "cred_status")) {
      outputOptions(output, nm, suspendWhenHidden = FALSE)
    }

    invisible(list(
      client = client,
      n_turns = n_turns,
      status = status
    ))
  })
}
