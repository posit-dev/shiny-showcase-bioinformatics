# Keys module: the "bring your own key" card on the Review tab. The user picks a
# provider (Anthropic / Google / OpenAI), pastes their API key, and optionally names
# a model. The key is held only in the session-scoped `creds` reactiveVal (shared
# with the results pipeline and the Chat tab), passed straight to the ellmer
# constructor, never written to disk/env, and cleared from the field after connect.
# See R/byok.R for the credential layer and the security rationale.

keys_ui <- function(id) {
  ns <- NS(id)
  default_provider <- tryCatch(
    genescout_default_byok_provider(),
    error = function(e) "anthropic"
  )
  model_choices <- tryCatch(
    genescout_provider_model_suggestions(default_provider),
    error = function(e) character(0)
  )

  bslib::card(
    class = "mb-3",
    bslib::card_header("AI provider (your key)"),
    bslib::card_body(
      # Provider + model share a row; the key spans the full width beneath them.
      div(
        class = "row g-2",
        div(
          class = "col-sm-6",
          selectInput(
            ns("provider"),
            "Provider",
            choices = genescout_provider_choices(),
            selected = default_provider
          )
        ),
        div(
          class = "col-sm-6",
          selectizeInput(
            ns("model"),
            "Model (optional)",
            choices = model_choices,
            selected = character(0),
            multiple = FALSE,
            options = list(
              create = TRUE,
              placeholder = "Provider default"
            )
          )
        )
      ),
      passwordInput(
        ns("api_key"),
        "API key",
        placeholder = "Paste your key (kept in this session only)"
      ),
      uiOutput(ns("key_help")),
      div(
        class = "d-flex gap-2 mb-2",
        actionButton(
          ns("connect"),
          "Use this key",
          class = "btn-primary btn-sm"
        ),
        actionButton(
          ns("forget"),
          "Forget key",
          class = "btn-outline-secondary btn-sm"
        )
      ),
      uiOutput(ns("status")),
      helpText(
        class = "small",
        "Powers AI curation, the specialists, and the Chat tab. Your key stays in",
        "this browser session - it is never stored, logged, or sent anywhere but",
        "the provider you choose. The deterministic ranking needs no key."
      )
    )
  )
}

# `creds` is the shared session reactiveVal(NULL) the whole app reads for BYOK.
keys_server <- function(id, creds, config = genescout_config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial status reflects whether a server-side (.Renviron) key already exists
    # for the configured provider, so a keyless local dev deploy and a hosted BYOK
    # deploy each read sensibly before the user does anything.
    server_key <- tryCatch(
      genescout_llm_available(config),
      error = function(e) {
        FALSE
      }
    )
    status <- reactiveVal(
      if (isTRUE(server_key)) {
        list(
          ok = TRUE,
          msg = "A server key is set; AI features are available. Paste your own to override it."
        )
      } else {
        list(
          ok = FALSE,
          msg = "Paste a key to enable AI curation, specialists, and chat."
        )
      }
    )

    # Repopulate model suggestions when the provider changes.
    observeEvent(input$provider, {
      choices <- tryCatch(
        genescout_provider_model_suggestions(input$provider),
        error = function(e) character(0)
      )
      updateSelectizeInput(
        session,
        "model",
        choices = choices,
        selected = character(0)
      )
    })

    output$key_help <- renderUI({
      meta <- genescout_provider_meta(input$provider %||% "anthropic")
      if (is.null(meta$key_url)) {
        return(NULL)
      }
      div(
        class = "small text-muted mb-2",
        "Need a key? ",
        tags$a(
          href = meta$key_url,
          target = "_blank",
          rel = "noopener",
          "Get one here"
        ),
        sprintf(" (or set %s in .Renviron).", meta$env)
      )
    })

    observeEvent(input$connect, {
      provider <- input$provider %||% "anthropic"
      key <- trimws(input$api_key %||% "")
      if (!nzchar(key)) {
        status(list(ok = FALSE, msg = "Please paste an API key first."))
        return()
      }
      cred <- genescout_byok_credential(provider, key, model = input$model)
      creds(cred)
      # Clear the visible field once the credential is captured - a shoulder-surfer
      # or screen-share should not see the key after connecting.
      updateTextInput(session, "api_key", value = "")
      model_note <- if (!is.null(cred$model)) {
        sprintf(" · model %s", cred$model)
      } else {
        ""
      }
      status(list(
        ok = TRUE,
        msg = sprintf(
          "Connected: %s%s.",
          genescout_provider_meta(provider)$label,
          model_note
        )
      ))
    })

    observeEvent(input$forget, {
      creds(NULL)
      updateTextInput(session, "api_key", value = "")
      status(list(
        ok = FALSE,
        msg = "Key forgotten. Paste a key and click Use this key."
      ))
    })

    output$status <- renderUI({
      st <- status()
      div(
        class = paste(
          "small mb-2",
          if (isTRUE(st$ok)) "text-success" else "text-muted"
        ),
        st$msg
      )
    })

    invisible(creds)
  })
}
