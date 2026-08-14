# Module: disease search + resolution -----------------------------------------
# Resolves a free-text disease name (or pasted ontology id) to candidate terms
# and returns the selected disease as a reactive one-row list.

# Ontology prefixes we let biobouncer check, mapped to its source names.
#
# Deliberately NOT the full DISEASE_ID_RX set. biobouncer's patterns want the
# colon form, and two of the app's accepted prefixes do not line up:
#   Orphanet:158673 -> biobouncer says FALSE (it expects a different form)
#   OTAR:...        -> no matching biobouncer source
# Validating those would reject input the resolver handles perfectly well, so
# they pass through unchecked. This map holds only prefixes verified to agree.
GLB_ID_SOURCES <- c(
  mondo = "mondo",
  efo = "efo",
  hp = "hp",
  doid = "doid",
  ncit = "ncit",
  go = "go"
)

# A shinyvalidate rule for the disease box. Returns NULL (valid) for anything
# it should not judge: blanks, free text, and prefixes outside the map above.
# It only ever rejects a string that LOOKS like an ontology id of a known
# prefix but is malformed, turning a wasted network round-trip and an empty
# candidate list into instant inline feedback.
glb_disease_id_rule <- function(value) {
  value <- trimws(value %||% "")
  if (!nzchar(value) || !is_disease_id(value)) {
    return(NULL)
  }
  prefix <- tolower(sub("[:_].*$", "", value))
  # Single-bracket + is.na, not [[: `[[` on a named vector RAISES for a name
  # that is not present, which would crash the validator on any unmapped
  # prefix (e.g. a pasted Orphanet id) instead of passing it through.
  db <- unname(GLB_ID_SOURCES[prefix])
  if (is.na(db) || !requireNamespace("biobouncer", quietly = TRUE)) {
    return(NULL)
  }
  # The app accepts EFO_0000305 and EFO:0000305 alike; biobouncer's patterns
  # only accept the colon form, so normalise before delegating.
  canonical <- sub("_", ":", value)
  ok <- tryCatch(
    biobouncer::is_valid_id(canonical, db, how = "pattern"),
    error = function(e) TRUE
  )
  if (isTRUE(ok)) {
    return(NULL)
  }
  paste0("Not a well-formed ", toupper(prefix), " id.")
}

disease_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      ns("term"),
      "Disease",
      placeholder = "e.g. lung cancer or MONDO:0008903"
    ),
    actionButton(
      ns("resolve"),
      "Resolve disease",
      class = "btn-secondary",
      width = "100%"
    ),
    div(class = "mt-3", uiOutput(ns("candidates")))
  )
}

disease_search_server <- function(id, resolve_fn = resolve_disease) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Inline validation for pasted ontology ids, so a typo shows up
    # immediately instead of costing a network round-trip that returns an
    # empty candidate list. Free text is untouched.
    if (requireNamespace("shinyvalidate", quietly = TRUE)) {
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("term", glb_disease_id_rule)
      iv$enable()
    }

    candidates <- eventReactive(input$resolve, {
      validate(need(
        nzchar(trimws(input$term %||% "")),
        "Enter a disease name."
      ))
      withProgress(message = "Resolving disease...", value = 0.5, {
        resolve_fn(input$term)
      })
    })

    output$candidates <- renderUI({
      cand <- candidates()
      if (nrow(cand) == 0) {
        return(div(
          class = "text-danger small",
          "No disease matched. Try a broader term or paste an EFO/MONDO id."
        ))
      }
      choices <- stats::setNames(
        cand$id,
        paste0(cand$name, "  [", cand$id, "]")
      )
      # A single compact dropdown (auto-selected to the top match) rather than a
      # stack of radio buttons, so resolving doesn't push the rest of the sidebar
      # controls down. The best match is pre-selected, so usually no extra click.
      selectInput(
        ns("choice"),
        "Matched disease",
        choices = choices,
        selected = cand$id[1],
        width = "100%"
      )
    })

    reactive({
      cand <- candidates()
      req(nrow(cand) > 0, input$choice)
      row <- cand[cand$id == input$choice, , drop = FALSE]
      if (nrow(row) == 0) {
        return(NULL)
      }
      as.list(row[1, ])
    })
  })
}
