# Source adapter: gnomAD gene constraint (annotation) --------------------------
#
# Gene-driven *prioritizer* (role = "annotation"): it does not add new genes or
# count toward coverage, but scores candidate genes by loss-of-function
# intolerance (LOEUF, gnomAD `oe_lof_upper`). Lower LOEUF = more constrained =
# higher priority, handled by the "rank_desc" normalization. The gnomAD API
# takes one gene per query, so we batch many genes per request using aliases.

GNOMAD_URL <- "https://gnomad.broadinstitute.org/api"

# POST a GraphQL query to gnomAD; returns parsed `data` or NULL on error.
gnomad_graphql <- function(query, timeout = 30) {
  glb_graphql_data(gnomad_graphql_env(query, timeout))
}

# Envelope-returning form. Note this now checks the GraphQL `errors` payload,
# which the previous hand-rolled version did not: it returned body$data
# unconditionally, so a query error looked like an empty result.
gnomad_graphql_env <- function(query, timeout = 30) {
  glb_graphql(
    GNOMAD_URL,
    query = query,
    source = "gnomAD",
    timeout = timeout
  )
}

# Map a bioclients gnomad tibble onto canonical columns. Pure. bioclients
# returns one row per input symbol including all-NA rows for genes gnomAD does
# not constrain, so rows without a LOEUF are dropped to match the previous
# behaviour. The score is LOEUF, rank-normalised descending by the registry
# (lower LOEUF = more constrained = higher score).
gnomad_rows <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  d <- d[!is.na(d$loeuf), , drop = FALSE]
  if (nrow(d) == 0) {
    return(NULL)
  }
  data.frame(
    gene_symbol = d$symbol,
    source_score_raw = as.numeric(d$loeuf),
    evidence_type = "gnomad_constraint",
    url = paste0("https://gnomad.broadinstitute.org/gene/", d$symbol),
    stringsAsFactors = FALSE
  )
}

# Drop symbols HGNC does not recognise, BEFORE they reach gnomAD.
#
# This is not cosmetic. gnomAD batches genes as aliased fields in one GraphQL
# query, and a single unresolvable symbol nulls out the WHOLE batch, not just
# its own field. Verified against the live API:
#
#   gnomad_constraints("TP53")                  -> loeuf 0.418
#   gnomad_constraints(c("TP53", "ZZZNOTAGENE"))-> loeuf NA, NA
#
# With a 200-gene seed chunked at 20, one stale symbol therefore silently
# costs constraint scores for the other 19 genes in its chunk. The old
# hand-rolled adapter had the same flaw. biobouncer's bundled HGNC snapshot
# (offline, ~45k symbols) filters those out cheaply.
#
# A symbol HGNC does not know is very unlikely to be in gnomAD, so the recall
# cost of filtering is far smaller than the cost of poisoning a whole chunk.
# Falls open: if biobouncer is unavailable, pass everything through rather
# than drop the source entirely.
gnomad_known_symbols <- function(symbols) {
  if (length(symbols) == 0 || !requireNamespace("biobouncer", quietly = TRUE)) {
    return(symbols)
  }
  keep <- tryCatch(
    biobouncer::is_valid_id(symbols, "hgnc", how = "cache"),
    error = function(e) rep(TRUE, length(symbols))
  )
  symbols[!is.na(keep) & keep]
}

fetch_gnomad <- function(
  disease = NULL,
  gene_symbols = NULL,
  chunk_size = 20,
  client_fn = bioclients::gnomad_constraints
) {
  symbols <- unique(toupper(trimws(gene_symbols %||% character())))
  # Syntactic hygiene. This used to be the ONLY thing standing between a gene
  # symbol and sprintf-interpolation straight into GraphQL query text;
  # bioclients builds the query now, so it is just belt and braces.
  symbols <- symbols[grepl("^[A-Za-z0-9._-]+$", symbols)]
  symbols <- gnomad_known_symbols(symbols)
  if (length(symbols) == 0) {
    return(empty_gene_table())
  }

  df <- gnomad_rows(glb_client_body(
    client_fn(symbols, chunk_size = chunk_size)
  ))
  if (is.null(df) || nrow(df) == 0) {
    return(empty_gene_table())
  }

  as_gene_table(df, "gnomad")
}
