# CIViC (Clinical Interpretation of Variants in Cancer): the count of expert-curated
# clinical evidence items CIViC holds for a gene - a grounded measure of how much
# cancer variant-interpretation literature the community has curated for it. CIViC
# is CC0. This is RESEARCH evidence (curated literature weight), never a clinical
# call about a patient.
#
# One GraphQL POST, keyed by HUGO symbol. No {ellmer} here - a plain, testable client.

CIVIC_GRAPHQL <- "https://civicdb.org/api/graphql"
CIVIC_WEB <- "https://civicdb.org"

# The curated clinical-evidence weight CIViC holds for `symbol`. Returns a
# normalized list (ok / evidence_items / assertions / variants / source_id /
# source_url) or ok = FALSE.
civic_gene_evidence <- function(symbol) {
  symbol <- toupper(trimws(as.character(symbol %||% "")))
  # Only pass a clean gene token into the GraphQL string (defensive: symbols are
  # alphanumeric with . _ -; strip anything else so the query can't be injected).
  symbol <- gsub("[^A-Z0-9._-]", "", symbol)
  if (!nzchar(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol."))
  }
  query <- sprintf(
    paste0(
      "{ gene(entrezSymbol: \"%s\") { id name entrezId link ",
      "stats { evidenceItemCount assertionCount variantCount } } }"
    ),
    symbol
  )
  res <- http_post_json(CIVIC_GRAPHQL, list(query = query), source = "CIViC")
  err <- graphql_error(res, "CIViC")
  if (!is.null(err)) {
    return(err)
  }
  civic_gene_parse(res$data)
}

# Distill the GraphQL gene payload. Pure + offline. A gene CIViC does not track
# comes back as data$gene = null -> ok = FALSE (no evidence, never invented).
civic_gene_parse <- function(data) {
  g <- pluck_at(data, "data", "gene")
  if (is.null(g)) {
    return(list(ok = FALSE, error = "No CIViC gene record."))
  }
  id <- pluck_at(g, "id")
  link <- as.character(pluck_at(g, "link", default = ""))
  list(
    ok = TRUE,
    id = id,
    name = as.character(pluck_at(g, "name") %||% ""),
    evidence_items = as.numeric(
      pluck_at(g, "stats", "evidenceItemCount", default = 0)
    ),
    assertions = as.numeric(pluck_at(
      g,
      "stats",
      "assertionCount",
      default = 0
    )),
    variants = as.numeric(pluck_at(g, "stats", "variantCount", default = 0)),
    source_id = paste0("CIViC:gene:", id %||% "NA"),
    source_url = if (nzchar(link)) paste0(CIVIC_WEB, link) else CIVIC_WEB
  )
}
