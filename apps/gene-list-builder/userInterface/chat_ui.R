# Assistant greeting: example prompts as clickable suggestion cards -----------
#
# The AI assistant is docked in an app-wide sidebar (see ui.R), not a page. Its
# example prompts live INSIDE the shinychat widget: shinychat renders a markdown
# list whose items are each a <span class="suggestion"> as a grid of clickable
# cards, and the "submit" class sends the prompt on click. The greeting survives
# the module's chat_clear() (which defaults to greeting = FALSE = keep greeting),
# so the cards stay available across reconnects.

# Gene-list-builder example prompts. The 2nd exercises the get_ranked_genes tool
# wired in server.R.
glb_example_prompts <- c(
  "How are genes ranked and scored?",
  "What are the top 10 genes in the current list, and why?",
  "What's the difference between an evidence source and an annotation source?",
  "What does the multi-source coverage bonus do to the ranking?"
)

# Markdown greeting: an intro line followed by one suggestion card per prompt.
glb_chat_greeting <- paste(
  c(
    paste(
      "Hi! I'm the Gene List Builder assistant. Pick a provider and Connect",
      "(a key in your environment is used automatically), then ask me anything.",
      "Try one of these - or just type your own:"
    ),
    "",
    paste0(
      '- <span class="suggestion submit">',
      glb_example_prompts,
      "</span>"
    )
  ),
  collapse = "\n"
)
