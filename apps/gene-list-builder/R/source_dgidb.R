# Source adapter: DGIdb drug-gene interactions ---------------------------------
#
# DGIdb is gene-centric (it maps genes to drug interactions), so it is a
# gene-driven source: it annotates candidate genes seeded by the disease-driven
# sources with druggability evidence. The raw score is the number of known drug
# interactions; rank-normalized at aggregation time.

DGIDB_GRAPHQL_URL <- "https://dgidb.org/api/graphql"

DGIDB_GENES_QUERY <- "
query Genes($names: [String!]!) {
  genes(names: $names) {
    nodes {
      name
      conceptId
      interactions { interactionScore }
    }
  }
}"

dgidb_graphql <- function(query, variables = list(), timeout = 20) {
  glb_graphql_data(dgidb_graphql_env(query, variables, timeout))
}

# Envelope-returning form, for callers that need the failure reason.
dgidb_graphql_env <- function(query, variables = list(), timeout = 20) {
  glb_graphql(
    DGIDB_GRAPHQL_URL,
    query = query,
    variables = variables,
    source = "DGIdb",
    timeout = timeout
  )
}

# `gene_symbols` is the union of symbols found by disease-driven sources.
# Returns a canonical gene tibble scored by drug-interaction count.
# Map a bioclients dgidb tibble onto canonical columns. Pure, so it is testable
# without any transport. bioclients returns one row per INPUT symbol, using NA
# for genes DGIdb has never heard of and 0 for known genes with no
# interactions; the original hand-rolled adapter dropped both, so both are
# filtered here to keep scoring identical.
dgidb_rows <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  keep <- !is.na(d$interaction_count) & d$interaction_count > 0
  d <- d[keep, , drop = FALSE]
  if (nrow(d) == 0) {
    return(NULL)
  }
  data.frame(
    gene_symbol = d$symbol,
    source_score_raw = as.numeric(d$interaction_count),
    n_evidence = as.integer(d$interaction_count),
    evidence_type = "drug_interaction",
    url = d$source_url,
    stringsAsFactors = FALSE
  )
}

# `client_fn` takes a character vector of symbols and returns a biohttp
# envelope wrapping the tibble above. Injectable for offline tests.
#
# Chunking is kept at the app's original 100. bioclients::dgidb_genes() sends
# every symbol in a single request, which with a 200-gene Open Targets seed
# would be one very large GraphQL query.
fetch_dgidb <- function(
  disease = NULL,
  gene_symbols = NULL,
  chunk_size = 100,
  client_fn = bioclients::dgidb_genes
) {
  gene_symbols <- unique(toupper(trimws(gene_symbols %||% character())))
  gene_symbols <- gene_symbols[nzchar(gene_symbols)]
  if (length(gene_symbols) == 0) {
    return(empty_gene_table())
  }

  chunks <- split(
    gene_symbols,
    ceiling(seq_along(gene_symbols) / chunk_size)
  )

  parts <- lapply(chunks, function(chunk) {
    dgidb_rows(glb_client_body(client_fn(chunk)))
  })
  df <- do.call(rbind, parts[!vapply(parts, is.null, logical(1))])
  if (is.null(df) || nrow(df) == 0) {
    return(empty_gene_table())
  }

  as_gene_table(df, "dgidb")
}
