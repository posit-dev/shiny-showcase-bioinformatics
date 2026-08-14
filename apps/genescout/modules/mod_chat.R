# Chat module: the shinychat assistant on the Chat tab. It uses the same session
# BYOK credential the Review tab captures (shared `creds`), builds a per-session
# ellmer chat client from it, and streams answers. The assistant is deliberately
# grounded: each turn is prefixed with a compact snapshot of the CURRENT run's
# ranked results (R/byok.R::genescout_chat_grounding) and the system prompt
# (prompts/chat.md) forbids ungrounded biological claims and clinical output, per
# GeneScout's non-negotiables. See R/byok.R for the credential/redaction layer.

# TRUE when the chat can run at all (its packages are installed). shinychat + ellmer
# are pinned deps; guard anyway so a stripped install degrades to a notice, not an
# error (mirrors how the rest of the app treats optional LLM pieces).
chat_available <- function() {
  requireNamespace("shinychat", quietly = TRUE) &&
    requireNamespace("ellmer", quietly = TRUE)
}

# Cap turns per session to bound cost on a pasted key; generous for a help chat.
GENESCOUT_CHAT_MAX_TURNS <- 30L

chat_ui <- function(id) {
  ns <- NS(id)
  if (!chat_available()) {
    return(div(
      class = "p-4",
      bslib::card(
        bslib::card_header("Chat unavailable"),
        bslib::card_body(
          "The chat needs the 'shinychat' and 'ellmer' packages, which are not",
          "installed in this deployment."
        )
      )
    ))
  }
  div(
    class = "gs-chat px-3 pt-3 pb-1",
    div(class = "mb-2", uiOutput(ns("status"))),
    shinychat::chat_ui(
      ns("chat"),
      height = "62vh",
      placeholder = "Ask about your ranked genes, how GeneScout works, or how to read the results...",
      greeting = paste(
        "Hi! I'm the GeneScout assistant. I can explain how the ranking, grades, and",
        "veto work, and help you interpret **your current run's** cited results.",
        "I only discuss the grounded evidence in front of us - no clinical advice,",
        "no ungrounded gene facts. Set your API key on the **Review** tab, then ask away."
      )
    ),
    helpText(
      class = "small mt-1 mb-0",
      "Research use only. Grounded in your current run's evidence; not for clinical",
      "or diagnostic use."
    )
  )
}

# `creds` is the shared session credential (reactiveVal). `result_r` is a reactive
# of the current ranked run (or NULL) used to ground each answer.
chat_server <- function(
  id,
  creds,
  result_r = reactive(NULL),
  config = genescout_config
) {
  moduleServer(id, function(input, output, session) {
    if (!chat_available()) {
      return(invisible(NULL))
    }
    ns <- session$ns
    client <- reactiveVal(NULL)
    n_turns <- reactiveVal(0L)
    status <- reactiveVal(list(
      ok = FALSE,
      msg = "Set your API key on the Review tab to start chatting."
    ))

    do_append <- function(response) shinychat::chat_append("chat", response)

    # (Re)build the client whenever the credential changes. Rebuilding starts a fresh
    # conversation (the grounding snapshot is injected per-turn, so a new run does not
    # require a rebuild). ignoreNULL = FALSE so "Forget key" tears the client down.
    observeEvent(
      creds(),
      {
        cred <- creds()
        if (!genescout_credential_ready(cred)) {
          client(NULL)
          n_turns(0L)
          status(list(
            ok = FALSE,
            msg = "Set your API key on the Review tab to start chatting."
          ))
          return()
        }
        cl <- tryCatch(
          genescout_build_chat_client(cred),
          error = function(e) {
            status(list(
              ok = FALSE,
              msg = paste0(
                "Could not connect: ",
                genescout_redact_secret(conditionMessage(e), cred$api_key)
              )
            ))
            NULL
          }
        )
        client(cl)
        n_turns(0L)
        if (!is.null(cl)) {
          status(list(
            ok = TRUE,
            msg = sprintf(
              "Connected via %s. Grounded in your current run.",
              genescout_provider_meta(cred$provider)$label
            )
          ))
        }
      },
      ignoreNULL = FALSE
    )

    output$status <- renderUI({
      st <- status()
      div(
        class = paste(
          "small",
          if (isTRUE(st$ok)) "text-success" else "text-muted"
        ),
        st$msg
      )
    })

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      cl <- client()
      if (is.null(cl)) {
        do_append("Set your API key on the **Review** tab, then ask again.")
        return()
      }
      if (n_turns() >= GENESCOUT_CHAT_MAX_TURNS) {
        do_append(
          "_Turn limit reached for this session. Reload to start a new chat._"
        )
        return()
      }
      n_turns(n_turns() + 1L)
      secret <- creds()$api_key %||% ""
      # Prefix the current grounded snapshot so every answer is tied to this run.
      grounded <- genescout_chat_grounding(result_r())
      full <- paste0(grounded, "\n\n---\nUser question: ", msg)

      p <- tryCatch(
        do_append(cl$stream_async(full)),
        error = function(e) {
          do_append(paste0(
            "Sorry - that request failed: ",
            genescout_redact_secret(conditionMessage(e), secret)
          ))
          NULL
        }
      )
      if (!is.null(p) && inherits(p, "promise")) {
        promises::then(
          p,
          onRejected = function(err) {
            do_append(paste0(
              "Sorry - that request failed: ",
              genescout_redact_secret(conditionMessage(err), secret)
            ))
          }
        )
      }
    })

    invisible(NULL)
  })
}
