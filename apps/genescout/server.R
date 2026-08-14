# GeneScout Shiny server. The Review tab owns the pipeline; the Chat tab is a grounded
# assistant. Two pieces of state are shared across tabs at the session level: the
# BYOK credential the user pastes on the Review tab (`creds`) and the current ranked
# run (`shared_result`), which grounds the chat.
function(input, output, session) {
  # Behind a reconnect-capable host (shinyapps.io, Shiny Server, ShinyProxy), let a
  # session survive a transient network drop. Opt-in via GENESCOUT_PRODUCTION so
  # plain local `runApp()` (which cannot reconnect) is unaffected. See
  # docs/deployment.md.
  if (nzchar(Sys.getenv("GENESCOUT_PRODUCTION"))) {
    session$allowReconnect(TRUE)
  }
  creds <- reactiveVal(NULL)
  shared_result <- reactiveVal(NULL)
  review_server("review", creds = creds, shared_result = shared_result)
  chat_server("chat", creds = creds, result_r = shared_result)
}
