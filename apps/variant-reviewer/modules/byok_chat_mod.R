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
#
# `%||%` (null-coalescing) is provided by R/ui_helpers.R, sourced before this
# module, so it is intentionally not redefined here.

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
      "gemini-flash-latest",
      "gemini-2.5-flash",
      "gemini-2.5-pro"
    ),
    openai = c("gpt-4.1", "gpt-4o", "gpt-4o-mini"),
    anthropic = c("claude-sonnet-5", "claude-opus-4-8", "claude-haiku-4-5"),
    character(0)
  )
}

# The model preselected for a provider. "" means "leave the picker empty and let
# the backend use the provider's own default". Gemini defaults to the cheapest
# fast tier, which is plenty for summarizing the dashboard's cards.
.byok_chat_default_model <- function(provider) {
  switch(
    provider,
    gemini = "gemini-flash-lite-latest",
    ""
  )
}

# Keep the provider default selectable (and selected) even when the live model
# list comes back without it, so the picker never silently drops the default.
.byok_chat_with_default <- function(ids, default) {
  if (!nzchar(default %||% "")) {
    return(ids)
  }
  if (default %in% ids) {
    return(c(default, setdiff(ids, default)))
  }
  c(default, ids)
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
# is debounced behind key entry, and any error degrades to the fallback.
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
          " the chat settings, or set the matching environment variable (",
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

  # The provider/key/model controls live in an offcanvas drawer (Bootstrap 5)
  # toggled from a gear button in the chat header, so the conversation uses the
  # full width of its container and the controls never crowd it. It uses
  # Bootstrap's default backdrop, which dims the rest of the app while the drawer
  # is open. bslib bundles Bootstrap's JS, so the data-bs-* toggle/dismiss
  # attributes need no extra deps.
  cfg_id <- ns("config")

  # Default to a provider that already has a key in the environment, so the
  # "key found" hint shows on load without the user hunting for it. Env vars are
  # read here at UI-build time (server-side R); a key added later needs a restart.
  env_ready <- Filter(
    function(p) nzchar(.byok_chat_env_first(.byok_chat_provider_meta(p)$env)),
    providers
  )
  default_provider <- if (length(env_ready) > 0) {
    env_ready[[1]]
  } else {
    providers[[1]]
  }

  controls <- tagList(
    selectInput(
      ns("provider"),
      "Provider",
      choices = choices,
      selected = default_provider,
      width = "100%"
    ),
    uiOutput(ns("key_help")),
    passwordInput(
      ns("api_key"),
      "API key",
      placeholder = "Paste your key; models load automatically",
      width = "100%"
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
      width = "100%",
      options = list(
        create = TRUE,
        placeholder = "Provider default - pick or type a model"
      )
    ),
    tags$p(
      class = "text-muted small mb-2",
      "Not listed? Type any model id your key supports and press Enter."
    ),
    # Status line for the key-scoped model list, which loads on its own as soon
    # as a key is entered (see the server) rather than behind a button.
    if (isTRUE(list_models)) uiOutput(ns("model_source")),
    div(
      class = "d-flex gap-2 mb-2",
      actionButton(ns("connect"), "Connect", class = "btn-primary btn-sm")
    ),
    uiOutput(ns("cred_status")),
    tags$p(
      class = "text-muted small mb-0",
      "Your key is held only in this browser session's server memory and is",
      " never written to disk. Switching provider starts a fresh",
      " conversation."
    )
  )

  config_offcanvas <- div(
    class = "offcanvas offcanvas-end",
    tabindex = "-1",
    id = cfg_id,
    style = paste0("width:", sidebar_width, "px;"),
    `aria-labelledby` = paste0(cfg_id, "_label"),
    div(
      class = "offcanvas-header",
      h2(
        "Model & key",
        class = "offcanvas-title h5 mb-0",
        id = paste0(cfg_id, "_label")
      ),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    div(class = "offcanvas-body", controls)
  )

  settings_button <- tags$button(
    type = "button",
    class = "btn btn-outline-secondary btn-sm flex-shrink-0",
    `data-bs-toggle` = "offcanvas",
    `data-bs-target` = paste0("#", cfg_id),
    `aria-controls` = cfg_id,
    icon("gear"),
    " Model & key"
  )

  # A fixed-height card so the chat fills it and its message list scrolls; the
  # header carries the title and the settings trigger. Pass an explicit `height`
  # (e.g. "600px") when embedding in a non-fill page.
  bslib::as_fill_carrier(tagList(
    config_offcanvas,
    bslib::card(
      height = height,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center gap-2",
        div(
          if (!is.null(title)) tags$span(class = "fw-semibold", title),
          if (!is.null(subtitle)) div(class = "text-muted small", subtitle)
        ),
        div(
          class = "d-flex align-items-center gap-2 flex-shrink-0",
          # Always-visible connection status, so the user knows they're
          # connected without reopening the Model & key drawer.
          uiOutput(ns("conn_badge"), inline = TRUE),
          settings_button
        )
      ),
      bslib::card_body(
        class = "p-0",
        shinychat::chat_ui(
          ns("chat"),
          height = "100%",
          placeholder = placeholder,
          greeting = greeting %||%
            paste(
              "Hi! Open **Model & key**, paste an API key, and click **Connect**",
              "to start chatting."
            )
        )
      )
    )
  ))
}

# --- Example prompts -----------------------------------------------------------
#
# shinychat (>= 0.4.0) renders a markdown list whose items are each a
# `<span class="suggestion">` as a grid of clickable cards; clicking one submits
# its body text as the user's message. We surface the caller's example prompts
# this way, set as the greeting once connected (so a click actually runs).

# Build the markdown suggestion list from `suggestions` (a character vector;
# names, when present, become the short card headings). Returns NULL if empty.
.byok_chat_suggestions_md <- function(suggestions) {
  if (is.null(suggestions)) {
    return(NULL)
  }
  suggestions <- suggestions[nzchar(suggestions)]
  if (length(suggestions) == 0) {
    return(NULL)
  }
  titles <- names(suggestions)
  items <- vapply(
    seq_along(suggestions),
    function(i) {
      body <- htmltools::htmlEscape(suggestions[[i]])
      title <- if (!is.null(titles) && nzchar(titles[[i]])) {
        titles[[i]]
      } else {
        NULL
      }
      span <- if (is.null(title)) {
        sprintf('<span class="suggestion">%s</span>', body)
      } else {
        sprintf(
          '<span class="suggestion" title="%s">%s</span>',
          htmltools::htmlEscape(title, attribute = TRUE),
          body
        )
      }
      paste0("- ", span)
    },
    character(1)
  )
  paste(items, collapse = "\n")
}

# Greeting shown once connected: a short line plus the clickable example cards.
.byok_chat_connected_greeting <- function(provider_label, suggestions) {
  head <- sprintf(
    "Connected to **%s**. Ask about the gene or variant you're reviewing",
    provider_label
  )
  md <- .byok_chat_suggestions_md(suggestions)
  if (is.null(md)) {
    return(paste0(head, "."))
  }
  paste0(head, ". Or try one of these:\n\n", md)
}

# Set the chat greeting, degrading silently on older shinychat (no-op) or any
# transient error. Uses the plain "chat" id (namespaced by the module session).
.byok_chat_set_greeting <- function(text) {
  if (!"chat_set_greeting" %in% getNamespaceExports("shinychat")) {
    return(invisible(NULL))
  }
  tryCatch(shinychat::chat_set_greeting("chat", text), error = function(e) NULL)
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
  suggestions = NULL,
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
    # Note shown under the model picker (fallback vs. live-loaded count).
    model_note <- reactiveVal(NULL)
    # Guards the on-load auto-connect so it fires at most once (for the starting
    # provider). Later provider switches are explicit and connect manually.
    auto_connect_pending <- reactiveVal(TRUE)

    # Live model lister -- injectable so tests avoid the network. Defaults to the
    # real ellmer-backed fetch.
    lister <- if (is.null(fetch_models)) {
      .byok_chat_fetch_models
    } else {
      fetch_models
    }

    # Is a server-side key available for a provider (via its environment vars)?
    env_key_present <- function(prov) {
      nzchar(.byok_chat_env_first(.byok_chat_provider_meta(prov)$env))
    }
    # Disconnected status: nudge toward Connect, and when a key is already in the
    # environment, say so and point the user straight at model selection.
    disconnected_msg <- function(prov) {
      if (env_key_present(prov)) {
        paste0(
          "A ",
          .byok_chat_provider_meta(prov)$label,
          " key was found in the environment. Pick a model and click Connect."
        )
      } else {
        "Enter your key and click Connect."
      }
    }

    # Fetch the provider's live, key-scoped model list and repopulate the picker.
    # Keeps whatever the user already typed selected, otherwise falls back to the
    # provider default; on failure we keep the curated suggestions and say so, so
    # this can never strand the user.
    refresh_models <- function(prov, key) {
      if (!isTRUE(list_models)) {
        return(invisible(NULL))
      }
      if (is.null(prov) || !nzchar(prov) || !nzchar(key %||% "")) {
        return(invisible(NULL))
      }
      model_note(list(ok = FALSE, msg = "Loading models for this key..."))
      ids <- tryCatch(lister(prov, key), error = function(e) NULL)
      if (is.null(ids) || length(ids) == 0) {
        model_note(list(
          ok = FALSE,
          msg = "Couldn't load models for this key -- showing suggestions. Type an id if needed."
        ))
        return(invisible(NULL))
      }
      default <- .byok_chat_default_model(prov)
      ids <- .byok_chat_with_default(ids, default)
      keep <- trimws(isolate(input$model) %||% "")
      updateSelectizeInput(
        session,
        "model",
        choices = ids,
        selected = if (nzchar(keep)) {
          keep
        } else if (nzchar(default)) {
          default
        } else {
          character(0)
        },
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
    }

    # React to the provider selector: reset the conversation, repopulate the
    # model picker with this provider's suggestions (preselecting its default),
    # and load the live list when a key is already available.
    observeEvent(input$provider, {
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return()
      }
      client(NULL)
      active_secret("")
      do_clear()
      default <- .byok_chat_default_model(prov)
      updateSelectizeInput(
        session,
        "model",
        choices = .byok_chat_with_default(
          .byok_chat_provider_models(prov),
          default
        ),
        selected = if (nzchar(default)) default else character(0),
        server = FALSE
      )
      model_note(NULL)
      status(list(ok = FALSE, msg = disconnected_msg(prov)))
      key <- trimws(input$api_key %||% "")
      if (!nzchar(key)) {
        key <- .byok_chat_env_first(.byok_chat_provider_meta(prov)$env)
      }
      refresh_models(prov, key)

      # On the initial load only, if the starting provider already has a
      # server-side key, connect right away so the assistant is chat-ready with
      # no clicks (the default model is preselected). Skipped when the user has
      # pasted their own key; later provider switches always connect manually.
      if (isTRUE(auto_connect_pending())) {
        auto_connect_pending(FALSE)
        pasted <- nzchar(trimws(input$api_key %||% ""))
        if (!pasted && env_key_present(prov)) {
          model <- trimws(input$model %||% "")
          if (!nzchar(model)) {
            model <- .byok_chat_default_model(prov)
          }
          connect_with(
            prov,
            .byok_chat_env_first(.byok_chat_provider_meta(prov)$env),
            model
          )
        }
      }
    })

    # Pasting (or typing) a key loads that key's models on its own -- there is no
    # button. Debounced so a paste, or a burst of keystrokes, costs one lookup.
    key_typed <- debounce(reactive(trimws(input$api_key %||% "")), 600)
    observeEvent(key_typed(), {
      refresh_models(isolate(input$provider), key_typed())
    })

    # Build the client for prov/key/model and reflect it in the UI (status badge,
    # cleared key field, connected greeting with the example prompts). Shared by
    # the Connect button and the on-load auto-connect below.
    connect_with <- function(prov, key, model) {
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
        status(list(
          ok = TRUE,
          msg = paste0("Connected: ", .byok_chat_provider_meta(prov)$label, ".")
        ))
        active_secret(key)
      }
      client(cl)
      n_turns(0L)
      do_clear()
      # Clear the visible key field; the built client already holds what it needs.
      updateTextInput(session, "api_key", value = "")
      # Once connected, surface the clickable example prompts as the greeting so
      # a click actually runs (clicking submits the prompt as a user message).
      if (!is.null(cl)) {
        .byok_chat_set_greeting(.byok_chat_connected_greeting(
          .byok_chat_provider_meta(prov)$label,
          suggestions
        ))
      }
      invisible(cl)
    }

    observeEvent(input$connect, {
      prov <- input$provider
      if (is.null(prov) || !nzchar(prov)) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      # Fall back to a server-side env key so the module works without pasting.
      if (!nzchar(key)) {
        key <- .byok_chat_env_first(.byok_chat_provider_meta(prov)$env)
      }
      if (!nzchar(key)) {
        status(list(
          ok = FALSE,
          msg = "Paste an API key (or set the provider's environment variable)."
        ))
        return()
      }
      connect_with(prov, key, trimws(input$model %||% ""))
    })

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      if (is.null(msg) || !nzchar(trimws(msg))) {
        return()
      }
      cl <- client()
      if (is.null(cl)) {
        do_append(
          "Open **Model & key** and connect a provider before chatting."
        )
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
        do_append(cl$stream_async(msg)),
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
      # When a server-side key is set, tell the user they can skip pasting.
      env_note <- if (env_key_present(prov)) {
        tags$p(
          class = "small text-success mb-1",
          icon("circle-check"),
          paste0(
            " A ",
            meta$label,
            " key is set in the environment. Leave the",
            " key field blank, pick a model, and click Connect."
          )
        )
      }
      link <- if (!is.null(meta$key_url)) {
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
      }
      if (is.null(env_note) && is.null(link)) {
        return(NULL)
      }
      tagList(env_note, link)
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

    # Compact status pill shown in the chat header, always visible. The full
    # status message is exposed as a tooltip via the title attribute.
    output$conn_badge <- renderUI({
      st <- status()
      ok <- isTRUE(st$ok)
      span(
        class = paste(
          "badge rounded-pill",
          if (ok) "text-bg-success" else "text-bg-secondary"
        ),
        title = st$msg,
        if (ok) "Connected" else "Not connected"
      )
    })

    invisible(list(
      client = client,
      n_turns = n_turns,
      status = status
    ))
  })
}
