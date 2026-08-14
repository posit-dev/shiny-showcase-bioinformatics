# Source adapter: Genomics England PanelApp ------------------------------------
#
# Disease-driven. PanelApp curates diagnostic gene panels with a green/amber/red
# confidence rating. The list endpoint's `search` param does not actually filter,
# so we fetch the panel index and token-match the disease name client-side, then
# pull the best-matching panel's green/amber genes.

PANELAPP_CONFIDENCE <- c("3" = 1.0, "2" = 0.5, "1" = 0.25)
PANELAPP_COLOR <- c("3" = "green", "2" = "amber", "1" = "red")

# Meaningful lowercase tokens of a disease/panel name (drops short + generic
# words so "lung cancer" matches "Inherited lung cancer", not every panel).
panelapp_tokens <- function(text) {
  stop_words <- c(
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
  words <- unlist(strsplit(tolower(text %||% ""), "[^a-z0-9]+"))
  words <- words[nchar(words) >= 3 & !words %in% stop_words]
  unique(words)
}

# Fetch the panel index via bioclients and reshape it into the list-of-lists
# that panelapp_best_panel() expects.
#
# The reshape is deliberate. The token-overlap + strict-majority heuristic below
# decides the ENTIRE gene list for this source and has no bioclients
# equivalent, so it is left untouched and fed the same shape as before rather
# than rewritten against a tibble.
#
# NOTE: max_pages is passed explicitly. bioclients defaults to 6 where this app
# has always used 8; relying on the default would silently shrink the searchable
# index and could change which panel is selected.
panelapp_collect_panels <- function(
  index_fn = bioclients::panelapp_all_panels,
  max_pages = 8
) {
  d <- glb_client_body(index_fn(max_pages = max_pages, page_size = 100))
  if (is.null(d) || nrow(d) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(d)), function(i) {
    list(
      id = d$id[[i]],
      name = d$name[[i]],
      relevant_disorders = as.list(d$disorders[[i]] %||% character())
    )
  })
}

# Pick the panel whose name best matches the disease name, or NULL if no panel
# shares enough tokens.
panelapp_best_panel <- function(panels, disease_name) {
  tokens <- panelapp_tokens(disease_name)
  if (length(tokens) == 0 || length(panels) == 0) {
    return(NULL)
  }
  score <- vapply(
    panels,
    function(p) {
      hay <- panelapp_tokens(paste(
        p$name %||% "",
        paste(unlist(p$relevant_disorders %||% list()), collapse = " ")
      ))
      sum(tokens %in% hay)
    },
    integer(1)
  )
  best <- which.max(score)
  # Require a strict majority of disease tokens to match (and at least one), so
  # a generic panel sharing one broad token (e.g. only "cancer") is not treated
  # as disease-specific.
  min_match <- floor(length(tokens) / 2) + 1
  if (length(best) == 0 || score[[best]] < min_match) {
    return(NULL)
  }
  panels[[best]]
}

fetch_panelapp <- function(
  disease,
  gene_symbols = NULL,
  index_fn = bioclients::panelapp_all_panels,
  panel_fn = bioclients::panelapp_panel,
  max_pages = 8
) {
  disease_name <- disease$name %||% ""
  if (!nzchar(disease_name)) {
    return(empty_gene_table())
  }

  panels <- panelapp_collect_panels(index_fn, max_pages = max_pages)
  panel <- panelapp_best_panel(panels, disease_name)
  if (is.null(panel)) {
    return(empty_gene_table())
  }

  d <- glb_client_body(panel_fn(panel$id))
  if (is.null(d) || nrow(d) == 0) {
    return(empty_gene_table())
  }

  # Green + amber only (diagnostic-grade). bioclients exposes a `level` string
  # where the raw API used confidence_level "3"/"2"/"1"; red is excluded, as
  # before. PANELAPP_CONFIDENCE is still keyed by the numeric confidence.
  keep <- d$level %in% c("green", "amber")
  d <- d[keep, , drop = FALSE]
  if (nrow(d) == 0) {
    return(empty_gene_table())
  }

  df <- data.frame(
    gene_symbol = as.character(d$symbol),
    source_score_raw = unname(
      PANELAPP_CONFIDENCE[as.character(d$confidence)]
    ),
    evidence_type = paste0("panelapp_", d$level),
    url = as.character(d$source_url),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$source_score_raw), , drop = FALSE]
  if (nrow(df) == 0) {
    return(empty_gene_table())
  }

  as_gene_table(df, "panelapp")
}
