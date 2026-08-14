# PubTator3 client - entity-tagged literature count.
# Endpoint: https://www.ncbi.nlm.nih.gov/research/pubtator3-api  (see docs/data_sources.md)
# The number of PubMed/PMC articles in which PubTator3 has TAGGED a specific gene
# ENTITY (not a free-text string match), used as a gene-level literature SIGNAL for
# prioritization - the precise successor to the Europe PMC symbol count, which
# matches on the raw symbol and so over-counts ambiguous names. Research evidence
# for prioritization, never a clinical call. Pure client through http_get_json();
# no {ellmer} imports. The count parser is separated so it is testable offline.

PUBTATOR_BASE <- "https://www.ncbi.nlm.nih.gov/research/pubtator3-api"
PUBTATOR_WEB <- "https://www.ncbi.nlm.nih.gov/research/pubtator3/"

# The PubTator3 entity token for a gene. Prefer the NCBI Gene id (`@GENE_<entrez>`,
# e.g. `@GENE_7157` for TP53) - the pipeline already resolved it via MyGene, and it
# disambiguates aliases exactly - and fall back to the symbol (`@GENE_<SYMBOL>`)
# when no entrez is in hand. NA for a blank/invalid gene (no query is attempted).
pubtator_entity_token <- function(symbol, entrez = NULL) {
  ez <- trimws(as.character(entrez %||% ""))
  if (nzchar(ez) && !is.na(ez) && grepl("^[0-9]+$", ez)) {
    return(paste0("@GENE_", ez))
  }
  sym <- toupper(trimws(as.character(symbol %||% "")))
  if (!nzchar(sym) || is.na(sym) || !grepl("^[A-Za-z0-9._-]+$", sym)) {
    return(NA_character_)
  }
  paste0("@GENE_", sym)
}

# Count of articles PubTator3 has tagged with a gene entity. Returns:
#   list(ok = TRUE, symbol, entity, count, source_id, source_url)
#   list(ok = FALSE, error = "...")
# A gene PubTator3 knows but has tagged in 0 articles is ok = TRUE, count = 0 (a
# real zero, like the Europe PMC count); only a failed fetch is ok = FALSE.
pubtator_gene_literature <- function(symbol, entrez = NULL) {
  token <- pubtator_entity_token(symbol, entrez)
  if (is_blank(token)) {
    return(list(ok = FALSE, error = "No gene entity for PubTator3 lookup."))
  }
  res <- http_get_json(
    PUBTATOR_BASE,
    path = "search/",
    query = list(text = token),
    source = "PubTator3"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  count <- pubtator_search_count_parse(res$data)
  if (is.na(count)) {
    return(list(ok = FALSE, error = "PubTator3 returned no count."))
  }
  list(
    ok = TRUE,
    symbol = toupper(trimws(as.character(symbol %||% ""))),
    entity = token,
    count = count,
    source_id = paste0("PubTator3:", token),
    source_url = paste0(
      PUBTATOR_WEB,
      "?query=",
      utils::URLencode(token, reserved = TRUE)
    )
  )
}

# Pure parser: a PubTator3 search body -> the integer article count (top-level
# `count`), NA if absent. 0 parses to 0L (a real zero). Testable offline against a
# recorded fixture.
pubtator_search_count_parse <- function(data) {
  raw <- pluck_at(data, "count", default = NA)
  if (is_blank(raw)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(raw))
}
