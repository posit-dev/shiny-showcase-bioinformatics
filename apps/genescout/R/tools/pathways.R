# Pathway client - Reactome.
# Endpoint: https://reactome.org/ContentService (see docs/data_sources.md)
# Reactome gives per-gene pathway membership. Pure client through the shared HTTP
# wrapper; no {ellmer} imports.

REACTOME_BASE <- "https://reactome.org/ContentService"
REACTOME_WEB <- "https://reactome.org/content/detail/"

# Gene symbol -> the human Reactome pathways it participates in, as a mechanistic
# pathway-membership signal (never a clinical call). Reactome's HGNC mapping takes
# the gene symbol directly. A 404 ("no pathways for GENE") surfaces as ok = FALSE.
# Returns:
#   list(ok = TRUE, symbol, pathways = tibble(pathway_id, name, in_disease,
#        source_id, source_url))
#   list(ok = FALSE, error = "...")
reactome_pathways <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for Reactome lookup."))
  }
  sym <- toupper(trimws(symbol))
  res <- http_get_json(
    REACTOME_BASE,
    path = paste0(
      "data/mapping/HGNC/",
      utils::URLencode(sym, reserved = TRUE),
      "/pathways"
    ),
    query = list(species = 9606),
    source = "Reactome"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error %||% "Reactome lookup failed."))
  }
  reactome_pathways_parse(res$data, sym)
}

# Pure parser: a Reactome pathways array -> the membership tibble. Every row
# carries a Reactome stable id (source_id). Separated from the fetch so it is
# testable offline against a JSON fixture.
reactome_pathways_parse <- function(data, symbol) {
  empty_err <- list(
    ok = FALSE,
    error = paste0("Reactome has no pathways for '", symbol, "'.")
  )
  if (is.null(data) || length(data) == 0) {
    return(empty_err)
  }
  stid <- vapply(
    data,
    function(p) as.character(pluck_at(p, "stId", default = NA_character_)),
    character(1)
  )
  name <- vapply(
    data,
    function(p) {
      as.character(pluck_at(p, "displayName", default = NA_character_))
    },
    character(1)
  )
  in_dis <- vapply(
    data,
    function(p) isTRUE(pluck_at(p, "isInDisease", default = FALSE)),
    logical(1)
  )
  keep <- !is.na(stid) & nzchar(stid)
  if (!any(keep)) {
    return(empty_err)
  }
  list(
    ok = TRUE,
    symbol = symbol,
    pathways = tibble::tibble(
      pathway_id = stid[keep],
      name = trimws(name[keep]),
      in_disease = in_dis[keep],
      source_id = paste0("Reactome:", stid[keep]),
      source_url = paste0(REACTOME_WEB, stid[keep])
    )
  )
}

# The subset of a gene's Reactome pathways that count toward disease relevance:
# any pathway Reactome flags as disease-associated (isInDisease), plus any whose
# name shares a meaningful token with a context pathway prior (e.g. Reactome
# "Regulation of RAS by GAPs" vs a context "RAS/MAPK"). Pure and testable.
reactome_relevant <- function(pathways, ctx_pathways = NULL) {
  if (is.null(pathways) || nrow(pathways) == 0) {
    return(pathways)
  }
  match_ctx <- pathway_matches_context(pathways$name, ctx_pathways)
  pathways[pathways$in_disease | match_ctx, , drop = FALSE]
}

# TRUE for each pathway name that shares a >=3-char token with any context pathway
# label. Case-insensitive; splits on non-alphanumerics.
pathway_matches_context <- function(names, ctx_pathways) {
  if (is.null(ctx_pathways) || length(ctx_pathways) == 0) {
    return(rep(FALSE, length(names)))
  }
  toks <- unique(unlist(lapply(ctx_pathways, function(p) {
    w <- unlist(strsplit(tolower(as.character(p)), "[^a-z0-9]+"))
    w[nchar(w) >= 3]
  })))
  if (length(toks) == 0) {
    return(rep(FALSE, length(names)))
  }
  vapply(
    names,
    function(nm) {
      nw <- unlist(strsplit(tolower(nm), "[^a-z0-9]+"))
      any(toks %in% nw)
    },
    logical(1),
    USE.NAMES = FALSE
  )
}
