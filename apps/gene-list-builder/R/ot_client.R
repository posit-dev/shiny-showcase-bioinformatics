# Open Targets Platform GraphQL client -----------------------------------------

OT_GRAPHQL_URL <- "https://api.platform.opentargets.org/api/v4/graphql"

OT_SOURCE <- "Open Targets"

# Execute a GraphQL query against the Open Targets Platform API. Returns the
# full biohttp envelope, so callers that care about *why* a call failed can
# tell a timeout from an empty result.
ot_graphql_env <- function(query, variables = list(), timeout = 20) {
  glb_graphql(
    OT_GRAPHQL_URL,
    query = query,
    variables = variables,
    source = OT_SOURCE,
    timeout = timeout
  )
}

# Backwards-compatible view: the parsed `data` list, or NULL on transport or
# GraphQL error. Every existing caller and every test stub uses this shape, so
# it stays. Prefer ot_graphql_env() where the failure reason matters.
ot_graphql <- function(query, variables = list(), timeout = 20) {
  glb_graphql_data(ot_graphql_env(query, variables, timeout))
}

OT_XREFS_QUERY <- "
query Xrefs($efoId: String!) {
  disease(efoId: $efoId) { id name dbXRefs }
}"

# Cross-reference ids for a disease (e.g. "DOID:1324", "UMLS:C0242379"). Used to
# translate the resolved MONDO/EFO id into the identifier another source needs.
ot_disease_xrefs <- function(efo_id, graphql_fn = ot_graphql) {
  if (is.null(efo_id) || !nzchar(efo_id)) {
    return(character())
  }
  data <- graphql_fn(OT_XREFS_QUERY, list(efoId = efo_id))
  xrefs <- data$disease$dbXRefs
  if (is.null(xrefs)) {
    return(character())
  }
  unlist(xrefs, use.names = FALSE)
}

# First cross-reference id with the given prefix (e.g. ot_disease_xref_id(id,
# "DOID") -> "DOID:1324"), or NA if none.
ot_disease_xref_id <- function(efo_id, prefix, graphql_fn = ot_graphql) {
  xrefs <- ot_disease_xrefs(efo_id, graphql_fn = graphql_fn)
  hit <- grep(paste0("^", prefix, ":"), xrefs, value = TRUE, ignore.case = TRUE)
  if (length(hit) == 0) NA_character_ else hit[[1]]
}
