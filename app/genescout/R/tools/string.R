# STRING protein-interaction client - within-list network connectivity.
# Endpoint: https://string-db.org/api  (see docs/data_sources.md)
# For a SET of candidate genes, how connected each gene is to the OTHER candidates
# in the same list via high-confidence STRING interactions - a "does this candidate
# sit inside a connected module with the rest of my list?" SIGNAL. This is
# COHORT-RELATIVE (a gene's value depends on which other genes are in the list), so
# it is used as an annotation that only nudges a connected gene up, never penalizes
# an isolated one, and never as primary evidence. Research evidence for
# prioritization, never a clinical call. Pure client through http_get_json(); no
# {ellmer} imports. The edge parser + connectivity math are pure, testable offline.

STRING_BASE <- "https://string-db.org/api"
STRING_WEB <- "https://string-db.org/cgi/network"
STRING_HUMAN <- 9606L
# required_score is 0-1000; 700 = combined score >= 0.7 (STRING's "high confidence").
STRING_MIN_SCORE <- 700L
# Cap identifiers per network call so the query URL stays bounded on a very large
# list (a discovery seed set is capped upstream at ~200, but a pasted plain-
# enrichment list is not). string_network flags a `truncated` overflow so the
# caller can audit it and mark the dropped genes as not-queried, never as isolates.
STRING_MAX_NODES <- 500L

# The within-list interaction network for a set of gene symbols. Returns:
#   list(ok = TRUE, edges = tibble(gene_a, gene_b, score), queried = <chr>,
#        n_query, truncated, n_dropped, source_id, source_url)
#   list(ok = FALSE, error = "...")
# Fewer than two usable symbols is ok = FALSE (no network to compute). `queried` is
# the exact symbol set STRING was asked about (after the STRING_MAX_NODES cap), so a
# caller can tell a measured isolate from a gene that was never queried. Edge
# endpoints are reconciled from STRING's preferredName space back to the queried
# symbols (string_reconcile_edges), so an HGNC-renamed gene (e.g. SEPTIN9 whose
# STRING preferredName is SEPT9) is credited correctly instead of dropped.
string_network <- function(
  symbols,
  species = STRING_HUMAN,
  required_score = STRING_MIN_SCORE
) {
  syms <- toupper(trimws(as.character(symbols %||% character())))
  syms <- unique(syms[
    !is.na(syms) & nzchar(syms) & grepl("^[A-Za-z0-9._-]+$", syms)
  ])
  if (length(syms) < 2) {
    return(list(ok = FALSE, error = "Need >= 2 genes for a STRING network."))
  }
  n_all <- length(syms)
  truncated <- n_all > STRING_MAX_NODES
  if (truncated) {
    syms <- syms[seq_len(STRING_MAX_NODES)]
  }
  res <- http_get_json(
    STRING_BASE,
    path = "json/network",
    query = list(
      identifiers = paste(syms, collapse = "\r"),
      species = species,
      required_score = required_score
    ),
    source = "STRING"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  edges <- string_network_parse(res$data)
  # Reconcile only when there is something to reconcile (saves the id-map call for a
  # set with no high-confidence edges). Falls back to the preferredName on any map
  # failure, so reconciliation never turns a good result into an error.
  if (nrow(edges) > 0) {
    edges <- string_reconcile_edges(edges, string_map_ids(syms, species))
  }
  list(
    ok = TRUE,
    edges = edges,
    queried = syms,
    n_query = length(syms),
    truncated = truncated,
    n_dropped = if (truncated) n_all - STRING_MAX_NODES else 0L,
    source_id = paste0("STRING:network:", species),
    source_url = paste0(
      STRING_WEB,
      "?identifiers=",
      paste(syms, collapse = "%0d"),
      "&species=",
      species
    )
  )
}

# STRING identifier map for a set of symbols: STRING's canonical preferredName can
# lag HGNC (e.g. queried SEPTIN9 -> STRING preferredName SEPT9), and the /network
# endpoint returns edges keyed by preferredName without echoing the query term, so
# we fetch the query -> preferredName map to translate edges back. Returns
# tibble(query, preferred, string_id) (upper-cased query/preferred), or NULL on a
# failed fetch (the caller then keeps the raw preferredNames).
string_map_ids <- function(symbols, species = STRING_HUMAN) {
  syms <- toupper(trimws(as.character(symbols %||% character())))
  syms <- unique(syms[!is.na(syms) & nzchar(syms)])
  if (length(syms) == 0) {
    return(NULL)
  }
  res <- http_get_json(
    STRING_BASE,
    path = "json/get_string_ids",
    query = list(
      identifiers = paste(syms, collapse = "\r"),
      species = species,
      limit = 1
    ),
    source = "STRING"
  )
  if (!res$ok) {
    return(NULL)
  }
  string_ids_parse(res$data)
}

# Pure parser: a get_string_ids body (a JSON array) -> tibble(query, preferred,
# string_id). One row per queried identifier (limit = 1). Testable offline.
string_ids_parse <- function(data) {
  empty <- tibble::tibble(
    query = character(),
    preferred = character(),
    string_id = character()
  )
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(empty)
  }
  field <- function(key) {
    vapply(
      data,
      function(e) as.character(pluck_at(e, key, default = NA)),
      character(1)
    )
  }
  q <- toupper(field("queryItem"))
  p <- toupper(field("preferredName"))
  s <- field("stringId")
  keep <- !is.na(q) & nzchar(q) & !is.na(p) & nzchar(p)
  out <- tibble::tibble(
    query = q[keep],
    preferred = p[keep],
    string_id = s[keep]
  )
  out[!duplicated(out$query), , drop = FALSE]
}

# Pure: rewrite edge endpoints from STRING's preferredName space into the queried
# symbol space using an id map (tibble with `preferred` + `query`). An endpoint with
# no mapping is left as-is. `id_map` NULL/empty -> edges unchanged. Testable offline.
string_reconcile_edges <- function(edges, id_map) {
  if (is.null(id_map) || nrow(id_map) == 0 || nrow(edges) == 0) {
    return(edges)
  }
  p2q <- stats::setNames(id_map$query, id_map$preferred)
  tr <- function(x) {
    m <- unname(p2q[x])
    ifelse(is.na(m), x, m)
  }
  edges$gene_a <- tr(edges$gene_a)
  edges$gene_b <- tr(edges$gene_b)
  edges
}

# Pure parser: a STRING network body (a JSON array of edges) -> tibble(gene_a,
# gene_b, score). `score` is STRING's combined score on a 0-1 scale. Separated from
# the fetch so it is testable offline against a recorded fixture.
string_network_parse <- function(data) {
  empty <- tibble::tibble(
    gene_a = character(),
    gene_b = character(),
    score = numeric()
  )
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(empty)
  }
  a <- vapply(
    data,
    function(e) {
      toupper(as.character(pluck_at(e, "preferredName_A", default = NA)))
    },
    character(1)
  )
  b <- vapply(
    data,
    function(e) {
      toupper(as.character(pluck_at(e, "preferredName_B", default = NA)))
    },
    character(1)
  )
  s <- vapply(
    data,
    function(e) {
      suppressWarnings(as.numeric(pluck_at(e, "score", default = NA)))
    },
    numeric(1)
  )
  keep <- !is.na(a) & !is.na(b) & nzchar(a) & nzchar(b)
  tibble::tibble(gene_a = a[keep], gene_b = b[keep], score = s[keep])
}

# The within-list connectivity of each gene: how many DISTINCT other genes in
# `symbols` it shares a high-confidence STRING edge with. Pure so it is unit
# testable. `symbols` is the full candidate set (upper-cased), so an isolated gene
# is returned with degree 0 (not dropped). Returns tibble(symbol, degree,
# partners = list<chr>), one row per unique symbol.
string_connectivity <- function(edges, symbols, min_score = 0.7) {
  syms <- unique(toupper(trimws(as.character(symbols %||% character()))))
  syms <- syms[!is.na(syms) & nzchar(syms)]
  in_set <- function(x) x %in% syms
  partners <- stats::setNames(vector("list", length(syms)), syms)
  for (s in syms) {
    partners[[s]] <- character()
  }
  if (nrow(edges) > 0) {
    ok <- !is.na(edges$score) & edges$score >= min_score
    ea <- edges$gene_a[ok]
    eb <- edges$gene_b[ok]
    for (i in seq_along(ea)) {
      x <- ea[i]
      y <- eb[i]
      if (x == y || !in_set(x) || !in_set(y)) {
        next
      }
      partners[[x]] <- union(partners[[x]], y)
      partners[[y]] <- union(partners[[y]], x)
    }
  }
  tibble::tibble(
    symbol = syms,
    degree = vapply(syms, function(s) length(partners[[s]]), integer(1)),
    partners = unname(lapply(syms, function(s) sort(partners[[s]])))
  )
}
