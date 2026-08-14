# DISEASES (Jensen Lab) client - disease-gene associations with confidence scores.
# Endpoint: https://api.jensenlab.org (see docs/data_sources.md)
# Text-mined + curated disease-gene associations (confidence 0-5), keyed by
# Disease Ontology id (DOID). Used as a gene-level relevance SIGNAL for a disease
# context, never as a clinical call. Two channels are queried and the strongest
# score per gene is kept. Pure client through the shared HTTP wrapper; the DOID is
# resolved upstream. No {ellmer} imports - callable and testable in plain R.

DISEASES_BASE <- "https://api.jensenlab.org"
DISEASES_VIEWER <- "https://diseases.jensenlab.org"

# The two association channels: curated knowledge + text-mined literature.
DISEASES_CHANNELS <- c("Knowledge", "Textmining")

# Query type codes: -26 = Disease Ontology term, 9606 = human (NCBI taxon).
DISEASES_DISEASE_TYPE <- -26L
DISEASES_HUMAN_TAXON <- 9606L

# Disease-gene associations for a DOID, taking the MAX confidence score per gene
# across the Knowledge and Textmining channels. The DOID is passed in (a caller
# resolves it from the disease context / cross-references).
# Returns:
#   list(ok = TRUE, doid, genes = tibble(symbol, score, source_id, source_url),
#        source_url)
#   list(ok = FALSE, error = "...")
# A disease with no associated genes is ok = TRUE with a 0-row tibble - kept
# distinct from a lookup failure (ok = FALSE) by callers.
diseases_gene_associations <- function(doid, limit = 300) {
  if (is_blank(doid)) {
    return(list(ok = FALSE, error = "No DOID for DISEASES lookup."))
  }
  channel_tbls <- vector("list", length(DISEASES_CHANNELS))
  names(channel_tbls) <- DISEASES_CHANNELS
  for (channel in DISEASES_CHANNELS) {
    res <- http_get_json(
      DISEASES_BASE,
      path = channel,
      query = list(
        type1 = DISEASES_DISEASE_TYPE,
        id1 = doid,
        type2 = DISEASES_HUMAN_TAXON,
        limit = limit,
        format = "json"
      ),
      source = "DISEASES (Jensen Lab)"
    )
    if (!res$ok) {
      return(list(ok = FALSE, error = res$error))
    }
    channel_tbls[[channel]] <- diseases_channel_parse(res$data)
  }

  merged <- diseases_merge(channel_tbls)
  genes <- tibble::tibble(
    symbol = merged$symbol,
    score = merged$score,
    source_id = paste0("DISEASES:", doid, ":", toupper(merged$symbol)),
    source_url = paste0(
      DISEASES_VIEWER,
      "/Search?query=",
      vapply(
        merged$symbol,
        function(s) utils::URLencode(s, reserved = TRUE),
        character(1)
      )
    )
  )

  list(
    ok = TRUE,
    doid = doid,
    genes = genes,
    source_url = paste0(
      DISEASES_VIEWER,
      "/Entity?type1=",
      DISEASES_DISEASE_TYPE,
      "&type2=",
      DISEASES_HUMAN_TAXON,
      "&id1=",
      utils::URLencode(doid, reserved = TRUE)
    )
  )
}

# Pure parser: one channel's parsed JSON body -> tibble(symbol, score).
# The API returns a JSON array whose first element maps a protein id (ENSP) to an
# entry carrying {name, score, url}; only name + score are distilled here. Rows
# with a blank symbol or a non-finite score are dropped. Separated from the fetch
# so it is testable offline against a JSON fixture.
diseases_channel_parse <- function(data) {
  empty <- tibble::tibble(symbol = character(), score = numeric())
  entries <- if (is.list(data) && length(data) >= 1) data[[1]] else NULL
  if (is.null(entries) || length(entries) == 0) {
    return(empty)
  }
  symbol <- vapply(
    entries,
    function(e) as.character(pluck_at(e, "name", default = NA_character_)),
    character(1)
  )
  score <- vapply(
    entries,
    function(e) as.numeric(pluck_at(e, "score", default = NA_real_)),
    numeric(1)
  )
  keep <- !is.na(symbol) & nzchar(symbol) & is.finite(score)
  # vapply over the ENSP-keyed map carries those keys as names; strip them so the
  # score column is a plain numeric (named columns break downstream equality).
  tibble::tibble(symbol = unname(symbol[keep]), score = unname(score[keep]))
}

# Merge per-channel tibbles into one, keeping the MAX score per gene symbol and
# ordering by score, highest first. NULL / empty channels are ignored.
diseases_merge <- function(list_of_channel_tibbles) {
  empty <- tibble::tibble(symbol = character(), score = numeric())
  symbols <- unlist(
    lapply(list_of_channel_tibbles, function(t) t$symbol),
    use.names = FALSE
  )
  scores <- unlist(
    lapply(list_of_channel_tibbles, function(t) t$score),
    use.names = FALSE
  )
  if (length(symbols) == 0) {
    return(empty)
  }
  max_score <- tapply(scores, symbols, max)
  ord <- order(-max_score)
  tibble::tibble(
    symbol = names(max_score)[ord],
    score = as.numeric(max_score)[ord]
  )
}
