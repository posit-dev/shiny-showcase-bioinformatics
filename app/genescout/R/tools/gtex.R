# GTEx tissue-expression client. Endpoint: https://gtexportal.org/api/v2
# (see docs/data_sources.md). A gene-level SIGNAL of how relevant a gene's
# expression is to the study's tissue(s) of interest: high median expression in a
# tissue of interest (relative to the gene's peak across all tissues) supports
# plausibility; expression only in unrelated tissues is a caveat. Research
# evidence for prioritization, never a clinical call. Pure client through
# http_get_json(); no {ellmer} imports. The relevance math is a pure function so
# it is testable offline against recorded fixtures.

GTEX_BASE <- "https://gtexportal.org/api/v2"
GTEX_WEB <- "https://gtexportal.org/home/gene/"

# Resolve a gene (symbol or Ensembl id) to its versioned GTEx gencode id, which
# the median-expression endpoint requires. One HTTP GET; NA on any miss.
gtex_gencode_id <- function(gene) {
  if (is_blank(gene)) {
    return(NA_character_)
  }
  res <- http_get_json(
    GTEX_BASE,
    path = "reference/gene",
    query = list(geneId = gene),
    source = "GTEx"
  )
  if (!res$ok) {
    return(NA_character_)
  }
  gtex_gencode_parse(res$data)
}

# Pure parser: a reference/gene body -> the first record's gencode id (or NA).
gtex_gencode_parse <- function(data) {
  recs <- pluck_at(data, "data")
  if (is.null(recs) || length(recs) == 0) {
    return(NA_character_)
  }
  gid <- pluck_at(recs[[1]], "gencodeId", default = NA_character_)
  as.character(gid)
}

# Median expression per tissue for a gencode id. Returns list(ok, data) where
# `data` is tibble(tissue, median); ok = FALSE on a failed fetch.
gtex_median_expression <- function(gencode_id, dataset = "gtex_v8") {
  res <- http_get_json(
    GTEX_BASE,
    path = "expression/medianGeneExpression",
    query = list(gencodeId = gencode_id, datasetId = dataset),
    source = "GTEx"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  list(ok = TRUE, data = gtex_expression_parse(res$data))
}

# Pure parser: a medianGeneExpression body -> tibble(tissue, median). Separated
# from the fetch so it is testable offline against a JSON fixture.
gtex_expression_parse <- function(data) {
  recs <- pluck_at(data, "data")
  if (is.null(recs) || length(recs) == 0) {
    return(tibble::tibble(tissue = character(), median = numeric()))
  }
  tibble::tibble(
    tissue = vapply(
      recs,
      function(r) as.character(pluck_at(r, "tissueSiteDetailId", default = NA)),
      character(1)
    ),
    median = vapply(
      recs,
      function(r) {
        suppressWarnings(as.numeric(pluck_at(r, "median", default = NA)))
      },
      numeric(1)
    )
  )
}

# Fetch a gene's per-tissue median expression. Returns:
#   list(ok = TRUE, gene, gencode_id, expression = tibble(tissue, median),
#        source_id, source_url) | list(ok = FALSE, error)
gtex_tissue_expression <- function(gene, dataset = "gtex_v8") {
  if (is_blank(gene)) {
    return(list(ok = FALSE, error = "No gene for GTEx lookup."))
  }
  gid <- gtex_gencode_id(gene)
  if (is_blank(gid)) {
    return(list(
      ok = FALSE,
      error = paste0("GTEx has no gencode id for '", gene, "'.")
    ))
  }
  expr <- gtex_median_expression(gid, dataset)
  if (!isTRUE(expr$ok)) {
    return(expr)
  }
  list(
    ok = TRUE,
    gene = gene,
    gencode_id = gid,
    expression = expr$data,
    source_id = paste0("GTEx:", gid),
    source_url = paste0(GTEX_WEB, gid)
  )
}

# --- Tissue matching + relevance (pure, offline) ----------------------------

# Meaningful lowercase word tokens from free-text tissue terms / GTEx tissue ids
# (e.g. "Nerve_Tibial" -> c("nerve","tibial")). Short and generic words are
# dropped so "Schwann cell" cannot spuriously match a "Cells - ..." GTEx tissue.
gtex_tokens <- function(x) {
  x <- tolower(paste(as.character(x), collapse = " "))
  toks <- unlist(strsplit(x, "[^a-z0-9]+"))
  stop <- c("cell", "cells", "tissue", "tissues", "site", "detail", "gtex")
  toks <- toks[nchar(toks) >= 4 & !(toks %in% stop)]
  unique(toks)
}

# The rows of a GTEx expression tibble whose tissue name shares a meaningful word
# with any tissue-of-interest term (token overlap; the same heuristic Reactome
# uses to match pathway names to context priors).
gtex_relevant <- function(expr, terms) {
  if (nrow(expr) == 0) {
    return(expr)
  }
  want <- gtex_tokens(terms)
  if (length(want) == 0) {
    return(expr[0, , drop = FALSE])
  }
  keep <- vapply(
    expr$tissue,
    function(t) length(intersect(gtex_tokens(t), want)) > 0,
    logical(1)
  )
  expr[keep, , drop = FALSE]
}

# The tissue-relevance signal for one gene, pure so it is unit-testable:
#   present   - the gene is expressed somewhere (>= min_expressed) AND at least
#               one tissue of interest maps to a GTEx tissue.
#   relevance - peak expression in a tissue of interest / peak across all tissues
#               (0..1). ~1 = expressed in the tissue of interest; ~0 = expressed
#               only elsewhere (the "unrelated-tissue-only" pattern the caveat
#               acts on).
#   matched   - the relevant tissue rows (for grounded evidence).
gtex_relevance <- function(expr, terms, min_expressed = 1) {
  miss <- list(
    present = FALSE,
    relevance = 0,
    matched = expr[0, , drop = FALSE]
  )
  if (nrow(expr) == 0 || length(terms) == 0) {
    return(miss)
  }
  matched <- gtex_relevant(expr, terms)
  if (nrow(matched) == 0) {
    return(miss)
  }
  max_all <- suppressWarnings(max(expr$median, na.rm = TRUE))
  if (!is.finite(max_all) || max_all < min_expressed) {
    return(miss)
  }
  max_toi <- suppressWarnings(max(matched$median, na.rm = TRUE))
  if (!is.finite(max_toi)) {
    max_toi <- 0
  }
  list(
    present = TRUE,
    relevance = max(0, min(1, max_toi / max_all)),
    matched = matched
  )
}
