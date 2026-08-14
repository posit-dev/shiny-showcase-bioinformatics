# Chat assistant module.
#
# A shinychat chat UI backed by a per-session ellmer Chat with hand-written tools
# over the app's data layer (see R/agent.R, R/agent_tools.R). Two credential
# paths, chosen live via a "Model source" selector:
#   * the shared server DEFAULT (Gemini on Vertex, the operator's bill), and
#   * BYOK -- the user pastes their own Gemini/OpenAI/Anthropic key (their bill),
#     held only in this session's memory.
#
# Opt-in and gracefully degrading: when neither path is available (packages
# missing or force-disabled) the page renders a setup panel built purely from
# bslib -- it references NO ellmer/shinychat symbols -- so the app (and the CI
# smoke test) load fine without those packages.

# Setup panel shown when the assistant is off. Uses only bslib/shiny, so it is
# safe to evaluate at UI-build time even when ellmer/shinychat are not installed.
.chat_disabled_panel <- function() {
  bslib::card(
    bslib::card_header("Enable the AI assistant"),
    bslib::card_body(
      p("The AI assistant is off in this environment. To enable it:"),
      tags$ol(
        tags$li(
          "Install the ",
          tags$code("ellmer"),
          " and ",
          tags$code("shinychat"),
          " R packages (both are in renv.lock)."
        ),
        tags$li(
          "Then either configure the shared assistant (Google Vertex -- run ",
          tags$code("gcloud auth application-default login"),
          " and set ",
          tags$code("TAHOE_VERTEX_PROJECT"),
          " in ",
          tags$code(".Renviron"),
          "), or simply bring your own API key in the chat sidebar."
        )
      ),
      p(
        class = "text-muted small",
        "The rest of Tahoe Explorer works normally without the assistant."
      )
    )
  )
}

# Model-source choices: the shared default (when configured) plus every offered
# BYOK provider. Names are user-facing labels; values are stable ids.
.chat_source_choices <- function() {
  choices <- character(0)
  if (tahoe_agent_default_available()) {
    choices <- c(choices, "Shared assistant (Gemini on Vertex)" = "shared")
  }
  for (p in tahoe_agent_byok_providers()) {
    choices[.tahoe_agent_provider_meta(p)$label] <- p
  }
  choices
}

# Prefer a source that connects without pasting: the shared Vertex default if
# configured, otherwise a BYOK provider whose key is already in the environment,
# otherwise the first offered provider.
.chat_default_source <- function(choices) {
  if (tahoe_agent_default_available()) {
    "shared"
  } else {
    tahoe_agent_env_provider() %||% unname(choices)[1]
  }
}

# The model-source config: provider select + BYOK key/model form + status. Lives
# in the navbar settings popover (chat_agent_config_ui); its ids are namespaced
# to the chat instance so the same chat_agent_server() reads them.
.chat_config_controls <- function(ns, choices, default_source) {
  tagList(
    selectInput(
      ns("source"),
      label = NULL,
      choices = choices,
      selected = default_source
    ),
    # Key form: shown for any BYOK provider, hidden for the shared default.
    conditionalPanel(
      condition = "input.source && input.source != 'shared'",
      ns = ns,
      uiOutput(ns("key_help")),
      passwordInput(
        ns("api_key"),
        "API key",
        placeholder = "Paste your key (kept in this session only)"
      ),
      # Model picker: choose a suggested model or type any id the key supports
      # (create = TRUE). Left empty -> the provider's own default. Choices are
      # repopulated per provider in the server; the button below can replace
      # them with the key's live, current model list.
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
      actionButton(
        ns("refresh_models"),
        "List models for this key",
        class = "btn-outline-secondary btn-sm mb-1"
      ),
      uiOutput(ns("model_source")),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton(ns("connect"), "Connect", class = "btn-primary btn-sm"),
        actionButton(
          ns("forget"),
          "Forget key",
          class = "btn-outline-secondary btn-sm"
        )
      )
    ),
    uiOutput(ns("cred_status"))
  )
}

# Example prompts shown as clickable cards in the greeting. shinychat renders a
# markdown list whose items are each a <span class="suggestion"> as a grid of
# suggestion cards; the "submit" class makes a click send the prompt straight
# away (the span's text is both the label and the submitted message).
#
# Suggestions are page-aware: the default set (Overview / anything without its
# own entry) explains the dataset and app, while each interactive page gets
# prompts tuned to what the assistant can DO there -- mostly ones that exercise
# the page-control tools (see R/agent_bridge.R), so a first click both answers
# and drives the page. chat_agent_server swaps the greeting to match the active
# page (via shinychat::chat_set_greeting) whenever no conversation has started.
.chat_default_suggestions <- c(
  "What is Tahoe-100M, and how was the experiment designed?",
  "Which drugs in the dataset target EGFR?",
  "Which assayed cell lines carry a KRAS driver mutation?",
  "Help me build a subset of lung-cancer cells and write the pull recipe."
)

.chat_page_suggestions <- list(
  drugs = c(
    "Select breast-cancer drugs on this page.",
    "Show me the approved kinase inhibitors.",
    "Filter to drugs that target EGFR.",
    "Which drugs are in active clinical trials?"
  ),
  cell_lines = c(
    "Select the lung-cancer cell lines.",
    "Show cell lines carrying a KRAS driver mutation.",
    "Filter to breast-tissue cell lines.",
    "Which assayed cell lines have a TP53 mutation?"
  ),
  subset = c(
    "Build a subset of lung-cancer cells.",
    "Select breast-tissue cell lines for my subset.",
    "Add EGFR-inhibitor drugs at a low dose.",
    "Write the pull recipe for the current subset."
  ),
  coverage = c(
    "Focus the matrix on breast-cancer drugs.",
    "Show coverage for lung and liver organs.",
    "Which drug × cell-line combos have the most cells?",
    "What does a darker tile mean here?"
  ),
  qc = c(
    "What do these QC metrics mean?",
    "How is the pass_filter tier defined?",
    "Explain the cell-cycle phase breakdown.",
    "Which plates look like quality outliers?"
  ),
  obs = c(
    "Filter the samples to a single drug.",
    "Show me the samples on a specific plate.",
    "What does the mean % mito distribution tell me?",
    "How is the cell-level obs summarised without loading it?"
  ),
  about = c(
    "What can this app help me do?",
    "How is the Tahoe-100M data loaded and queried?",
    "Where does the metadata in these tables come from?",
    "How do I export a subset recipe as a notebook?"
  )
)

# The clickable example prompts for a page id (falls back to the default set).
.chat_suggestions_for <- function(page = NULL) {
  if (!is.null(page) && !is.null(.chat_page_suggestions[[page]])) {
    .chat_page_suggestions[[page]]
  } else {
    .chat_default_suggestions
  }
}

# Human title for a page id, from the page registry (for the greeting lead-in).
.chat_page_title <- function(page = NULL) {
  if (is.null(page)) {
    return(NULL)
  }
  p <- tryCatch(app_pages()[[page]], error = function(e) NULL)
  p$title
}

# Build the greeting markdown for a page: a short lead-in naming the page (when
# known) followed by that page's clickable example prompts.
.chat_greeting <- function(page = NULL) {
  title <- .chat_page_title(page)
  intro <- if (is.null(title) || identical(page, "overview")) {
    paste(
      "Hi! I can explain the **Tahoe-100M** dataset and this app, and help",
      "you plan a subset for your analysis. Try one of these -- or just ask:"
    )
  } else {
    paste0(
      "Hi! You're on the **",
      title,
      "** page -- I can answer questions and drive its controls for you. ",
      "Try one of these -- or just ask:"
    )
  }
  bullets <- paste0(
    '- <span class="suggestion submit">',
    .chat_suggestions_for(page),
    "</span>"
  )
  paste(c(intro, "", bullets), collapse = "\n")
}

# The chat window for the assistant sidebar. The greeting opens with a few
# clickable example prompts so a new user can start with one click; it defaults
# to the general set and is swapped per active page by the server.
.chat_window <- function(ns, height, fill = FALSE) {
  shinychat::chat_ui(
    ns("chat"),
    height = height,
    fill = fill,
    placeholder = "Ask about the dataset, drugs, cell lines, or a subset...",
    greeting = .chat_greeting(NULL)
  )
}

#' The app-wide "Tahoe assistant" sidebar UI (see ui.R). The model / key /
#' model-id controls sit in a collapsed "Model & key" section at the top; the
#' conversation fills the rest. Falls back to the setup panel when the assistant
#' can't be offered.
chat_agent_ui <- function(id) {
  ns <- NS(id)
  if (!tahoe_agent_enabled() || length(.chat_source_choices()) == 0) {
    return(.chat_disabled_panel())
  }
  choices <- .chat_source_choices()
  config <- .chat_config_controls(ns, choices, .chat_default_source(choices))
  help_note <- tags$p(
    class = "text-muted small mb-0",
    "Switching source starts a fresh conversation."
  )
  bslib::as_fill_carrier(div(
    class = "tahoe-chat-dock",
    tags$style(HTML(paste0(
      ".tahoe-chat-dock .form-group,",
      ".tahoe-chat-dock .shiny-input-container",
      "{margin-bottom:0.4rem !important;}",
      ".tahoe-chat-dock .accordion-body{padding:0.6rem 0.8rem;}",
      # Clickable suggestions (greeting examples and the follow-ups the model
      # appends to each answer) render as a plain bulleted list of links rather
      # than shinychat's default grid of cards.
      ".tahoe-chat-dock .shiny-chat-suggestion-list",
      "{display:block !important;grid-template-columns:none !important;",
      "list-style:disc !important;padding-left:1.25rem !important;",
      "gap:0 !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list>li",
      "{display:list-item !important;margin:0.15rem 0 !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item",
      "{display:inline !important;padding:0 !important;border:0 !important;",
      "border-radius:0 !important;background:none !important;",
      "box-shadow:none !important;transform:none !important;",
      "animation:none !important;color:var(--bs-link-color,#007bc2) !important;",
      "text-decoration:underline dotted 2px !important;",
      "text-underline-offset:3px !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item:hover",
      "{background:none !important;box-shadow:none !important;",
      "transform:none !important;text-decoration-style:solid !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item:before,",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item:after",
      "{display:none !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item-title",
      "{display:inline !important;font-weight:600 !important;}",
      ".tahoe-chat-dock .shiny-chat-suggestion-list-item-body",
      "{display:inline !important;color:inherit !important;}"
    ))),
    # Settings first (Model & key), then the assistant heading, then the chat.
    # The top margin keeps the accordion clear of the sidebar's collapse toggle.
    bslib::accordion(
      open = FALSE,
      class = "mt-4 mb-2",
      bslib::accordion_panel(
        "Model & key",
        icon = shiny::icon("gear"),
        config,
        help_note
      )
    ),
    div(
      class = "d-inline-flex align-items-center gap-2 mb-2 fw-semibold",
      shiny::icon("robot"),
      "Tahoe assistant"
    ),
    .chat_window(ns, height = "calc(100vh - 210px)", fill = TRUE)
  ))
}

#' Chat assistant server. `client_factory(credential)` builds the ellmer Chat
#' for a credential (defaults to tahoe_agent_client); `append` sends a response
#' to the chat widget (defaults to shinychat::chat_append). Both are injectable
#' so tests drive the module with a stub client and a recorder -- no network, no
#' credentials. Injecting a `client_factory` also forces the enabled path even
#' when the assistant is otherwise unavailable (e.g. the disabled test suite).
#' `fetch_models(provider, api_key)` returns the key's live, current model ids
#' (or NULL to fall back to the curated suggestions); it defaults to the real
#' ellmer-backed fetch and is injectable so tests avoid the network.
#' `active_page` is an optional reactive returning the id of the page the user
#' is currently viewing; when supplied, the greeting's example prompts are
#' swapped to that page's set (see .chat_greeting) as long as no conversation
#' has started, so opening the assistant on a page shows prompts tuned to it.
chat_agent_server <- function(
  id,
  client_factory = NULL,
  append = NULL,
  fetch_models = NULL,
  active_page = NULL
) {
  moduleServer(id, function(input, output, session) {
    enabled <- !is.null(client_factory) || tahoe_agent_enabled()
    if (!enabled) {
      return(invisible(NULL))
    }

    # Live model lister -- injectable so tests avoid the network. Defaults to the
    # real ellmer-backed fetch.
    lister <- if (is.null(fetch_models)) {
      .tahoe_agent_fetch_models
    } else {
      fetch_models
    }

    # Session-aware tools that read the active page and drive the filters /
    # selections on every interactive page (found lazily via session$userData;
    # see R/agent_bridge.R). Appended to the base data-layer tool suite.
    # `session` here is the chat session -- used only to LOOK UP the page
    # bridges; each page owns the actual input writes.
    page_tools <- tahoe_page_control_tools(session)
    factory <- if (is.null(client_factory)) {
      function(credential) {
        tahoe_agent_client(
          credential = credential,
          tools = c(tahoe_agent_tools(), page_tools)
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
    tools_used <- reactiveVal(character())
    n_turns <- reactiveVal(0L)
    status <- reactiveVal(list(ok = FALSE, msg = "Not connected."))
    # The active BYOK key, held only to redact it from any surfaced error.
    # "" for the shared path or before a key is connected.
    active_secret <- reactiveVal("")
    # Note shown under the "List models" button (fallback vs. live-loaded count).
    model_note <- reactiveVal(NULL)

    # Attach the per-turn tool-name recorder to a freshly built client (for the
    # "tools used" footer). Re-attached on every rebuild.
    wire_tools <- function(cl) {
      if (!is.null(cl) && is.function(cl$on_tool_request)) {
        cl$on_tool_request(function(request) {
          nm <- tryCatch(
            request@name,
            error = function(e) {
              tryCatch(request$name, error = function(e2) NULL)
            }
          )
          if (!is.null(nm) && nzchar(nm)) {
            isolate(tools_used(c(tools_used(), nm)))
          }
        })
      }
      invisible(cl)
    }

    # Build (or clear) the session client for a credential and reset the convo.
    # `secret` is redacted from any error surfaced to the user.
    set_client <- function(credential, ok_msg, secret = "") {
      cl <- tryCatch(
        factory(credential),
        error = function(e) {
          status(list(
            ok = FALSE,
            msg = paste0(
              "Could not connect: ",
              .tahoe_agent_redact(conditionMessage(e), secret)
            )
          ))
          NULL
        }
      )
      if (!is.null(cl)) {
        wire_tools(cl)
        status(list(ok = TRUE, msg = ok_msg))
      }
      client(cl)
      n_turns(0L)
      tools_used(character())
      do_clear()
    }

    # React to the model-source selector. "shared" builds the Vertex client
    # immediately; a BYOK provider waits for the user to Connect a key.
    observeEvent(input$source, {
      src <- input$source
      if (is.null(src) || !nzchar(src)) {
        return()
      }
      active_secret("")
      if (identical(src, "shared")) {
        set_client(
          tahoe_agent_default_credential(),
          "Connected: shared assistant."
        )
      } else {
        client(NULL)
        do_clear()
        # Repopulate the model picker with this provider's suggestions.
        updateSelectizeInput(
          session,
          "model",
          choices = .tahoe_agent_provider_models(src),
          selected = character(0),
          server = FALSE
        )
        model_note(NULL)
        # If a key is already in the environment, tell the user they can skip
        # pasting and just pick a model + Connect.
        if (nzchar(.tahoe_agent_provider_env_key(src))) {
          status(list(
            ok = FALSE,
            msg = paste(
              "A key was found in your environment -- pick a model and click",
              "Connect (no need to paste)."
            )
          ))
        } else {
          status(list(ok = FALSE, msg = "Enter your key and click Connect."))
        }
      }
    })

    # Fetch the provider's live, key-scoped model list and repopulate the picker.
    # Keeps whatever the user already typed selected; on failure we keep the
    # curated fallback and say so, so this can never strand the user. (BYOK only;
    # the shared Vertex source has no pasteable key to scope a listing to.)
    observeEvent(input$refresh_models, {
      src <- input$source
      if (is.null(src) || identical(src, "shared") || !nzchar(src)) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      # Fall back to a key already in the environment, so the live model list
      # works without pasting.
      if (!nzchar(key)) {
        key <- .tahoe_agent_provider_env_key(src)
      }
      if (!nzchar(key)) {
        model_note(list(
          ok = FALSE,
          msg = "Enter a key first to list its models."
        ))
        return()
      }
      model_note(list(ok = FALSE, msg = "Fetching models..."))
      ids <- tryCatch(lister(src, key), error = function(e) NULL)
      if (is.null(ids) || length(ids) == 0) {
        model_note(list(
          ok = FALSE,
          msg = paste(
            "Couldn't fetch models for this key -- showing suggestions.",
            "Type an id if needed."
          )
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
          .tahoe_agent_provider_meta(src)$label,
          "."
        )
      ))
    })

    observeEvent(input$connect, {
      src <- input$source
      if (is.null(src) || identical(src, "shared")) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      # Fall back to a key already in the environment so the user can connect
      # without pasting one.
      if (!nzchar(key)) {
        key <- .tahoe_agent_provider_env_key(src)
      }
      if (!nzchar(key)) {
        status(list(
          ok = FALSE,
          msg = "Paste an API key, or set the provider's environment variable."
        ))
        return()
      }
      model <- trimws(input$model %||% "")
      label <- .tahoe_agent_provider_meta(src)$label
      set_client(
        tahoe_agent_byok_credential(src, key, model),
        paste0("Connected: ", label, "."),
        secret = key
      )
      active_secret(key)
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

    # Swap the greeting's example prompts to match the page the user is on, so
    # opening the assistant on a page surfaces prompts tuned to it. Only while no
    # conversation has started -- once the user has sent a turn we leave the
    # transcript (and its opening bubble) alone.
    if (!is.null(active_page)) {
      observeEvent(active_page(), {
        if (isolate(n_turns()) > 0L) {
          return()
        }
        shinychat::chat_set_greeting("chat", .chat_greeting(active_page()))
      })
    }

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      if (is.null(msg) || !nzchar(trimws(msg))) {
        return()
      }
      cl <- client()
      if (is.null(cl)) {
        do_append("Select a model source and connect before chatting.")
        return()
      }
      if (n_turns() >= .tahoe_agent_max_turns()) {
        do_append(paste(
          "_Turn limit reached for this session._",
          "_Reload the page to start a new conversation._"
        ))
        return()
      }
      n_turns(n_turns() + 1L)
      tools_used(character())
      secret <- active_secret()

      # Guard BOTH failure paths so a provider error (bad key, quota, unknown
      # model, overload) surfaces as a chat message instead of an unhandled
      # observer error -- which would tear down the Shiny session:
      #   * stream_async() / chat_append() throwing synchronously -> tryCatch
      #   * the streaming promise rejecting mid-turn -> onRejected
      p <- tryCatch(
        do_append(cl$stream_async(msg)),
        error = function(e) {
          do_append(.tahoe_agent_friendly_error(conditionMessage(e), secret))
          NULL
        }
      )

      if (!is.null(p) && inherits(p, "promise")) {
        # A rejected streaming promise must still surface as a chat message
        # rather than an unhandled error. (The tools a turn used are tracked in
        # tools_used() but intentionally not shown in the response.)
        promises::then(
          p,
          onRejected = function(err) {
            do_append(.tahoe_agent_friendly_error(
              conditionMessage(err),
              secret
            ))
          }
        )
      }
    })

    output$key_help <- renderUI({
      src <- input$source
      if (is.null(src) || identical(src, "shared")) {
        return(NULL)
      }
      meta <- .tahoe_agent_provider_meta(src)
      env_found <- nzchar(.tahoe_agent_provider_env_key(src))
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
            "✓ A key was found in your environment -- leave the field",
            " blank and just pick a model and Connect."
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

    # The settings live in a navbar popover that is collapsed by default, so
    # these outputs would otherwise suspend and never render until first opened
    # (and the connection status must stay live regardless).
    for (nm in c("key_help", "model_source", "cred_status")) {
      outputOptions(output, nm, suspendWhenHidden = FALSE)
    }

    invisible(list(
      client = client,
      n_turns = n_turns,
      tools_used = tools_used,
      status = status,
      model_note = model_note
    ))
  })
}
