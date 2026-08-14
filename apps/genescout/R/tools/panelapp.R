# Genomics England PanelApp client - disease-driven diagnostic gene panels.
# Endpoint: https://panelapp.genomicsengland.co.uk/api/v1 (see docs/data_sources.md)
# PanelApp curates diagnostic gene panels with a green/amber/red confidence
# rating. The list endpoint's `search` param does not actually filter, so we
# fetch the panel index and token-match the disease name client-side, then pull
# the best-matching panel's GREEN + AMBER (diagnostic-grade) genes as a
# gene-level relevance SIGNAL, never as a clinical call. Pure client through the
# shared HTTP wrapper; no {ellmer} imports. The fetch/parse split keeps the
# parsers testable offline against a JSON fixture.

PANELAPP_BASE <- "https://panelapp.genomicsengland.co.uk/api/v1"
PANELAPP_WEB_BASE <- "https://panelapp.genomicsengland.co.uk"

# Kept confidence levels (green + amber) mapped to a 0-1 weight. Red ("1") and
# anything else are dropped.
PANELAPP_CONFIDENCE <- c("3" = 1.0, "2" = 0.5)

# Words dropped from a disease/panel name before token matching, so "lung
# cancer" matches "Inherited lung cancer", not every panel.
PANELAPP_STOP_WORDS <- c(
  "disease",
  "diseases",
  "disorder",
  "disorders",
  "syndrome",
  "syndromes",
  "the",
  "and",
  "for",
  "with",
  "type",
  "familial",
  "hereditary",
  "inherited",
  "congenital",
  "related",
  "early",
  "onset",
  "adult",
  "childhood"
)

# Green + amber genes for the PanelApp panel that best matches a disease name.
# Walks the (paginated) panel index, token-matches, then pulls the panel detail.
# Returns:
#   list(ok = TRUE, panel_id, panel_name,
#        genes = tibble(symbol, confidence, source_id, source_url),
#        source_url)
#   list(ok = FALSE, error = "...")
panelapp_disease_genes <- function(disease_name, max_pages = 6) {
  if (is_blank(disease_name)) {
    return(list(ok = FALSE, error = "No disease name for PanelApp lookup."))
  }

  collected <- panelapp_collect_panels(max_pages = max_pages)
  if (!is.null(collected$error) && length(collected$panels) == 0) {
    return(list(ok = FALSE, error = collected$error))
  }

  choice <- panelapp_pick_panel(collected$panels, disease_name)
  if (is.null(choice)) {
    return(list(
      ok = FALSE,
      error = paste0("No PanelApp panel matched '", disease_name, "'.")
    ))
  }

  res <- http_get_json(
    PANELAPP_BASE,
    path = paste0("panels/", choice$id, "/"),
    source = "PanelApp"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }

  genes <- panelapp_panel_parse(res$data)
  source_url <- paste0(PANELAPP_WEB_BASE, "/panels/", choice$id, "/")
  genes$source_id <- paste0(
    "PanelApp:panel:",
    choice$id,
    ":",
    toupper(genes$symbol)
  )
  genes$source_url <- rep(source_url, nrow(genes))

  list(
    ok = TRUE,
    panel_id = choice$id,
    panel_name = choice$name,
    genes = genes,
    source_url = source_url
  )
}

# Walk the paginated panel index (capped at max_pages) into a flat list of panel
# objects. Returns list(panels = <list>, error = <chr|NULL>); `error` is only set
# when the very first page failed (so callers can surface a real outage).
panelapp_collect_panels <- function(max_pages = 6) {
  panels <- list()
  error <- NULL
  for (page in seq_len(max_pages)) {
    res <- http_get_json(
      PANELAPP_BASE,
      path = "panels/",
      query = list(page_size = 100, page = page),
      source = "PanelApp"
    )
    if (!res$ok) {
      if (page == 1) {
        error <- res$error
      }
      break
    }
    panels <- c(panels, pluck_at(res$data, "results", default = list()))
    if (is_blank(pluck_at(res$data, "next", default = NULL))) {
      break
    }
  }
  list(panels = panels, error = error)
}

# Pure parser: pick the panel whose name / relevant-disorders best token-match the
# disease name. `panels_json` may be a full index page (with a `results` field) or
# a bare flat list of panel objects. Requires a strict majority of disease tokens
# to match (and at least one) so a panel sharing one broad token is not treated as
# disease-specific. Returns list(id, name) or NULL.
panelapp_pick_panel <- function(panels_json, disease_name) {
  panels <- if (!is.null(panels_json$results)) {
    panels_json$results
  } else {
    panels_json
  }
  tokens <- panelapp_tokens(disease_name)
  if (length(tokens) == 0 || length(panels) == 0) {
    return(NULL)
  }

  score <- vapply(
    panels,
    function(p) {
      hay <- panelapp_tokens(paste(
        pluck_at(p, "name", default = ""),
        paste(
          unlist(pluck_at(p, "relevant_disorders", default = list())),
          collapse = " "
        )
      ))
      sum(tokens %in% hay)
    },
    integer(1)
  )

  best <- which.max(score)
  min_match <- floor(length(tokens) / 2) + 1
  if (length(best) == 0 || score[[best]] < min_match) {
    return(NULL)
  }

  chosen <- panels[[best]]
  list(
    id = pluck_at(chosen, "id", default = NULL),
    name = pluck_at(chosen, "name", default = NA_character_)
  )
}

# Pure parser: a panel-detail JSON body -> tibble(symbol, confidence) of its
# GREEN (confidence_level "3" -> 1.0) and AMBER ("2" -> 0.5) genes. Red ("1") and
# any other level are dropped. Separated from the fetch so it is testable offline
# against a JSON fixture.
panelapp_panel_parse <- function(panel_detail_json) {
  empty <- tibble::tibble(symbol = character(), confidence = numeric())
  genes <- pluck_at(panel_detail_json, "genes", default = list())
  if (length(genes) == 0) {
    return(empty)
  }

  conf_raw <- vapply(
    genes,
    function(g) as.character(pluck_at(g, "confidence_level", default = NA)),
    character(1)
  )
  keep <- conf_raw %in% names(PANELAPP_CONFIDENCE)
  genes <- genes[keep]
  conf_raw <- conf_raw[keep]
  if (length(genes) == 0) {
    return(empty)
  }

  symbol <- vapply(
    genes,
    function(g) {
      as.character(
        pluck_at(g, "gene_data", "gene_symbol", default = NULL) %||%
          pluck_at(g, "entity_name", default = NA_character_)
      )
    },
    character(1)
  )

  tibble::tibble(
    symbol = symbol,
    confidence = unname(PANELAPP_CONFIDENCE[conf_raw])
  )
}

# Meaningful lowercase tokens of a disease/panel name (drops short + generic
# words). Used by both the panel picker and its tests.
panelapp_tokens <- function(text) {
  words <- unlist(strsplit(tolower(text %||% ""), "[^a-z0-9]+"))
  words <- words[nchar(words) >= 3 & !words %in% PANELAPP_STOP_WORDS]
  unique(words)
}
