# MyGene.info client - symbol resolution.
# Endpoint: https://mygene.info/v3  (see docs/data_sources.md)
# Resolves a gene symbol (or Ensembl/Entrez id) to stable identifiers used
# downstream. Pure client through http_get_json(); no {ellmer} imports.
# Adapted from the sibling variant-reviewer app.

MYGENE_BASE <- "https://mygene.info/v3"

# Fields fetched for every resolution (single or batch), kept in one place so the
# two paths never drift. `SCOPES` are the fields the batch POST matches each query
# against - covering symbols, aliases, and Ensembl/Entrez ids in one request (the
# single path builds an explicit `ensembl.gene:`/`entrezgene:` term instead).
MYGENE_FIELDS <- "name,symbol,entrezgene,ensembl.gene,uniprot,type_of_gene,summary"
MYGENE_BATCH_SCOPES <- "symbol,alias,ensembl.gene,entrezgene,retired"

# Strip anything that isn't a plausible gene-identifier character.
mygene_clean_symbol <- function(symbol) {
  if (is_blank(symbol)) {
    return(NULL)
  }
  cleaned <- gsub("[^A-Za-z0-9._-]", "", trimws(as.character(symbol)))
  if (cleaned == "") NULL else cleaned
}

# Build the MyGene query term, detecting Ensembl gene and Entrez ids so they
# resolve precisely instead of as free-text symbol matches.
mygene_query_term <- function(symbol) {
  if (grepl("^ENSG\\d+", symbol, ignore.case = TRUE)) {
    paste0("ensembl.gene:", symbol)
  } else if (grepl("^\\d+$", symbol)) {
    paste0("entrezgene:", symbol)
  } else {
    symbol
  }
}

# Resolve one symbol to identifiers.
# Returns:
#   list(ok = TRUE, symbol, name, summary, entrez, ensembl_gene, uniprot,
#        type_of_gene)
#   list(ok = FALSE, error = "...")
resolve_symbol <- function(symbol, species = "human") {
  cleaned <- mygene_clean_symbol(symbol)
  if (is.null(cleaned)) {
    return(list(ok = FALSE, error = "Please provide a valid gene identifier."))
  }

  res <- http_get_json(
    MYGENE_BASE,
    path = "query",
    query = list(
      q = mygene_query_term(cleaned),
      species = species,
      # Fetch a few candidates (not just the top score) so mygene_pick_hit() can
      # prefer the exact symbol over a higher-scored fuzzy match to another gene.
      size = 5,
      fields = MYGENE_FIELDS
    ),
    source = "MyGene"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }

  hits <- res$data$hits
  if (is.null(hits) || length(hits) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No gene found for '", cleaned, "'.")
    ))
  }
  mygene_parse_hit(mygene_pick_hit(hits, cleaned), fallback_symbol = cleaned)
}

# Batch-resolve many identifiers in ONE request (MyGene /query POST). This is the
# first-run latency lever: an N-symbol list is a single round-trip instead of N
# serial GETs. `scopes` matches each query against symbol/alias/Ensembl/Entrez, so
# mixed identifier types resolve without a per-token prefix. Returns a list of
# resolve_symbol()-shaped results aligned to `symbols` (input order); returns NULL
# on a transport failure so the caller can fall back to serial resolution.
resolve_symbols_batch <- function(symbols, species = "human") {
  cleaned <- vapply(
    symbols,
    function(s) mygene_clean_symbol(s) %||% NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  q <- unique(cleaned[!is.na(cleaned)])
  # Every token was blank/invalid: no request to make, but still return an aligned
  # per-token miss so the caller does not treat this as a batch failure and retry.
  if (length(q) == 0) {
    return(mygene_parse_batch(list(), symbols))
  }

  res <- http_post_json(
    paste0(MYGENE_BASE, "/query"),
    body = list(
      q = as.list(q),
      scopes = MYGENE_BATCH_SCOPES,
      fields = MYGENE_FIELDS,
      species = species
    ),
    source = "MyGene"
  )
  if (!res$ok) {
    return(NULL)
  }
  mygene_parse_batch(res$data, symbols)
}

# Pure parser: the batch /query POST returns a flat array where each element echoes
# its input `query`; an ambiguous query yields several elements (best `_score`
# first) and an unmatched one an element with `notfound = true`. Group ALL hits by the
# echoed query, pick the best one per query with mygene_pick_hit() (exact symbol match
# preferred - see below), and map every input symbol back to a resolve_symbol()-shaped
# result in input order - so an unmatched or invalid token becomes a definite
# ok = FALSE (never a wrong gene). Separated from the fetch so it is testable offline.
mygene_parse_batch <- function(hits, symbols) {
  by_query <- list()
  for (h in hits) {
    q <- pluck_at(h, "query")
    if (is_blank(q)) {
      next
    }
    # Keep EVERY hit for a query (in MyGene's best-score-first order), not just the
    # first, so mygene_pick_hit() can override the score with an exact symbol match.
    by_query[[q]] <- c(by_query[[q]], list(h))
  }
  lapply(symbols, function(sym) {
    cleaned <- mygene_clean_symbol(sym)
    if (is.null(cleaned)) {
      return(list(
        ok = FALSE,
        error = "Please provide a valid gene identifier."
      ))
    }
    hit <- mygene_pick_hit(by_query[[cleaned]], cleaned)
    if (is.null(hit) || isTRUE(pluck_at(hit, "notfound", default = FALSE))) {
      return(list(
        ok = FALSE,
        error = paste0("No gene found for '", cleaned, "'.")
      ))
    }
    mygene_parse_hit(hit, fallback_symbol = cleaned)
  })
}

# Choose the best hit for a queried token from its candidate hits. MyGene's `_score`
# can rank a fuzzy alias/retired match to a DIFFERENT gene ABOVE the exact symbol
# match - e.g. querying "TTN" (with alias/retired scopes) returns TTR (entrez 7276,
# score 19.07) before TTN (7273, score 18.29). So prefer a hit whose official symbol
# equals the token (case-insensitive); only when none does - a deliberate alias like
# "p53" - fall back to MyGene's first (best-score) hit. This is what keeps the wrong
# gene out of the results.
mygene_pick_hit <- function(hits, token) {
  if (is.null(hits) || length(hits) == 0) {
    return(NULL)
  }
  for (h in hits) {
    sym <- pluck_at(h, "symbol", default = "")
    if (nzchar(sym) && identical(toupper(sym), toupper(token))) {
      return(h)
    }
  }
  hits[[1]]
}

# Pure parser: a single MyGene hit (parsed JSON list) -> normalized result.
# Separated from the fetch so it is testable without the network.
mygene_parse_hit <- function(hit, fallback_symbol = NA_character_) {
  list(
    ok = TRUE,
    symbol = pluck_at(hit, "symbol", default = fallback_symbol),
    name = pluck_at(hit, "name", default = NA_character_),
    summary = pluck_at(hit, "summary", default = NA_character_),
    entrez = as.character(pluck_at(hit, "entrezgene", default = NA)),
    ensembl_gene = mygene_first(pluck_at(hit, "ensembl", "gene")),
    uniprot = mygene_first(pluck_at(hit, "uniprot", "Swiss-Prot")),
    type_of_gene = pluck_at(hit, "type_of_gene", default = NA_character_)
  )
}

# MyGene fields can be a scalar or a list (multiple mappings); take the first.
mygene_first <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  as.character(x[[1]])
}
