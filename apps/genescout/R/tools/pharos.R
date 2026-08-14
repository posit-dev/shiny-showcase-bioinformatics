# Pharos client - Target Development Level (TDL).
# Endpoint: https://pharos-api.ncats.io/graphql  (see docs/data_sources.md)
# TDL is Pharos/IDG's tractability classification (Tclin > Tchem > Tbio > Tdark);
# used as a gene-level druggability / actionability SIGNAL for prioritization,
# never as a clinical call. Pure client through http_post_json(); no {ellmer}
# imports.

PHAROS_URL <- "https://pharos-api.ncats.io/graphql"
PHAROS_WEB <- "https://pharos.nih.gov/targets"

# TDL -> a 0-1 druggability score (already on a 0-1 scale, normalized by identity).
PHAROS_TDL_SCORE <- c(Tclin = 1.0, Tchem = 0.75, Tbio = 0.5, Tdark = 0.25)

# Batch `targets` query (one symbol here), matching the proven gene-list-builder
# client. Response shape: data.targets.targets[] of { sym, tdl }.
PHAROS_QUERY <- paste(
  "query($syms: [String!]) {",
  "  targets(targets: $syms) { targets { sym tdl } }",
  "}",
  sep = "\n"
)

# TDL score for a gene symbol. Returns:
#   list(ok = TRUE, symbol, tdl, score, source_id, source_url)
#   list(ok = FALSE, error = "...")
pharos_tdl <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for Pharos lookup."))
  }
  sym <- toupper(trimws(symbol))
  res <- http_post_json(
    PHAROS_URL,
    body = list(query = PHAROS_QUERY, variables = list(syms = list(sym))),
    source = "Pharos"
  )
  err <- graphql_error(res, "Pharos")
  if (!is.null(err)) {
    return(err)
  }
  pharos_tdl_parse(res$data, sym)
}

# Pure parser: a Pharos targets body -> the TDL signal. Separated from the fetch
# so it is testable offline against a JSON fixture. An unknown/absent TDL is a
# miss (ok = FALSE).
pharos_tdl_parse <- function(data, symbol) {
  targets <- pluck_at(data, "data", "targets", "targets")
  if (is.null(targets) || length(targets) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("Pharos has no record for '", symbol, "'.")
    ))
  }
  target <- targets[[1]]
  tdl <- as.character(pluck_at(target, "tdl", default = NA_character_))
  if (is_blank(tdl) || is.na(PHAROS_TDL_SCORE[tdl])) {
    return(list(
      ok = FALSE,
      error = paste0("Pharos has no known TDL for '", symbol, "'.")
    ))
  }
  list(
    ok = TRUE,
    symbol = symbol,
    tdl = tdl,
    score = unname(PHAROS_TDL_SCORE[tdl]),
    source_id = paste0("Pharos:gene:", symbol, ":", tdl),
    source_url = paste0(PHAROS_WEB, "/", symbol)
  )
}
