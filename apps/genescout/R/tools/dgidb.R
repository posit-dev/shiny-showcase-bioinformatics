# DGIdb client - drug-gene interaction count.
# Endpoint: https://dgidb.org/api/graphql  (see docs/data_sources.md)
# The number of curated drug-gene interactions DGIdb records for a gene, used as
# a gene-level druggability / actionability SIGNAL for prioritization, never as a
# clinical call. Pure client through http_post_json(); no {ellmer} imports.

DGIDB_URL <- "https://dgidb.org/api/graphql"
DGIDB_WEB <- "https://dgidb.org/genes"

DGIDB_QUERY <- paste(
  "query($names: [String!]) {",
  "  genes(names: $names) {",
  "    nodes { name conceptId interactions { interactionScore } }",
  "  }",
  "}",
  sep = "\n"
)

# Count of drug-gene interactions for a gene symbol. Returns:
#   list(ok = TRUE, symbol, count, source_id, source_url)
#   list(ok = FALSE, error = "...")
# A gene present in DGIdb with 0 interactions is ok = TRUE, count = 0 (a real
# zero); a gene absent from DGIdb entirely is ok = FALSE (a miss).
dgidb_gene_interactions <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for DGIdb lookup."))
  }
  sym <- toupper(trimws(symbol))
  res <- http_post_json(
    DGIDB_URL,
    body = list(query = DGIDB_QUERY, variables = list(names = list(sym))),
    source = "DGIdb"
  )
  err <- graphql_error(res, "DGIdb")
  if (!is.null(err)) {
    return(err)
  }
  dgidb_interactions_parse(res$data, sym)
}

# Pure parser: a DGIdb genes body -> the interaction-count signal. Separated from
# the fetch so it is testable offline against a JSON fixture.
dgidb_interactions_parse <- function(data, symbol) {
  nodes <- pluck_at(data, "data", "genes", "nodes")
  if (is.null(nodes) || length(nodes) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("DGIdb has no record for '", symbol, "'.")
    ))
  }
  node <- nodes[[1]]
  inter <- pluck_at(node, "interactions", default = list())
  concept <- pluck_at(node, "conceptId", default = symbol)
  list(
    ok = TRUE,
    symbol = symbol,
    count = length(inter),
    source_id = paste0("DGIdb:gene:", symbol),
    source_url = paste0(DGIDB_WEB, "/", concept)
  )
}
