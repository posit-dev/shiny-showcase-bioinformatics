# Source adapter: Pharos / TCRD target development level (annotation) ----------
#
# Gene-driven *prioritizer* (role = "annotation"): scores candidate genes by
# their Target Development Level (TDL) - how far along the druggability spectrum
# a target is (Tclin > Tchem > Tbio > Tdark). Does not add genes or count toward
# coverage.

PHAROS_URL <- "https://pharos-api.ncats.io/graphql"

PHAROS_TDL_SCORE <- c(
  Tclin = 1.0,
  Tchem = 0.75,
  Tbio = 0.5,
  Tdark = 0.25
)

PHAROS_TARGETS_QUERY <- "
query Targets($syms: [String!]) {
  targets(targets: $syms) { targets { sym tdl } }
}"

# POST a GraphQL query to Pharos; returns parsed `data` or NULL on error.
pharos_graphql <- function(query, variables = list(), timeout = 30) {
  glb_graphql_data(pharos_graphql_env(query, variables, timeout))
}

# Envelope-returning form. Like gnomAD, this now checks the GraphQL `errors`
# payload that the previous hand-rolled version ignored.
pharos_graphql_env <- function(query, variables = list(), timeout = 30) {
  glb_graphql(
    PHAROS_URL,
    query = query,
    variables = variables,
    source = "Pharos",
    timeout = timeout
  )
}

fetch_pharos <- function(
  disease = NULL,
  gene_symbols = NULL,
  client_fn = bioclients::pharos_targets
) {
  symbols <- unique(toupper(trimws(gene_symbols %||% character())))
  symbols <- symbols[nzchar(symbols)]
  if (length(symbols) == 0) {
    return(empty_gene_table())
  }

  df <- pharos_rows(glb_client_body(client_fn(symbols)))
  if (is.null(df) || nrow(df) == 0) {
    return(empty_gene_table())
  }

  as_gene_table(df, "pharos")
}

# Map a bioclients pharos tibble onto canonical columns. Pure.
#
# bioclients returns one row per INPUT symbol with `tdl` NA both for symbols
# Pharos does not know and for any level outside PHAROS_TDL_LEVELS. Rows
# without a score are dropped, matching the previous behaviour, which relied on
# PHAROS_TDL_SCORE[tdl] producing NA for the same cases.
pharos_rows <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  score <- unname(PHAROS_TDL_SCORE[d$tdl])
  keep <- !is.na(score)
  if (!any(keep)) {
    return(NULL)
  }
  data.frame(
    gene_symbol = d$symbol[keep],
    source_score_raw = score[keep],
    evidence_type = paste0("pharos_", tolower(d$tdl[keep])),
    url = paste0("https://pharos.nih.gov/targets/", d$symbol[keep]),
    stringsAsFactors = FALSE
  )
}
