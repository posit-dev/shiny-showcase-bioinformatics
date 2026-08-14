# Open Targets disease resolver - free-text / ontology-id -> disease candidates.
# Endpoint: https://api.platform.opentargets.org/api/v4/graphql (see docs/data_sources.md)
# Maps a biological-context term to Open Targets disease/phenotype records (EFO,
# MONDO, HP, Orphanet, ...) so the engine can anchor a review to a grounded
# ontology id. If the caller pastes an ontology id we look it up directly instead
# of searching. Reuses OPENTARGETS_URL from R/tools/opentargets.R. Pure client
# through http_post_json(); no {ellmer} imports.

# Ontology ids Open Targets understands, e.g. "MONDO:0018975", "EFO_0000508".
DISEASE_ID_PATTERN <- "^(EFO|MONDO|Orphanet|ORPHA|HP|DOID|NCIT|GO|OTAR)[:_]"

# Full-text disease search: hits come back pre-sorted, best match first.
OT_DISEASE_SEARCH_QUERY <- paste(
  "query($q: String!, $size: Int!) {",
  "  search(queryString: $q, entityNames: [\"disease\"],",
  "         page: {index: 0, size: $size}) {",
  "    hits { id name score description }",
  "  }",
  "}",
  sep = "\n"
)

# Direct lookup of a single disease by its EFO/MONDO/... id.
OT_DISEASE_LOOKUP_QUERY <- paste(
  "query($efoId: String!) {",
  "  disease(efoId: $efoId) { id name description }",
  "}",
  sep = "\n"
)

# Cross-references for a disease (e.g. "DOID:0111253", "UMLS:C0027831").
OT_DISEASE_XREFS_QUERY <- paste(
  "query($efoId: String!) {",
  "  disease(efoId: $efoId) { id name dbXRefs }",
  "}",
  sep = "\n"
)

# TRUE when `term` looks like an ontology id rather than free text.
is_disease_id <- function(term) {
  grepl(DISEASE_ID_PATTERN, trimws(term %||% ""), ignore.case = TRUE)
}

# Resolve a disease term (free text or ontology id) to candidate diseases,
# best match first. Returns:
#   list(ok = TRUE, matches = tibble(id, name, score, description,
#                                    source_id, source_url))
#   list(ok = FALSE, error = "...")
# A blank term, transport failure, GraphQL error, or no match is ok = FALSE.
resolve_disease <- function(term, limit = 5) {
  term <- trimws(term %||% "")
  if (is_blank(term)) {
    return(list(ok = FALSE, error = "No disease term to resolve."))
  }

  if (is_disease_id(term)) {
    # Open Targets ids are "_"-separated; some ontologies stay mixed-case
    # (e.g. "Orphanet_636"), so only swap the ":" separator.
    efo <- gsub(":", "_", term)
    body <- list(
      query = OT_DISEASE_LOOKUP_QUERY,
      variables = list(efoId = efo)
    )
  } else {
    body <- list(
      query = OT_DISEASE_SEARCH_QUERY,
      variables = list(q = term, size = limit)
    )
  }

  res <- http_post_json(OPENTARGETS_URL, body = body, source = "Open Targets")
  err <- graphql_error(res, "Open Targets")
  if (!is.null(err)) {
    return(err)
  }

  matches <- resolve_disease_parse(res$data)
  if (nrow(matches) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No disease match for '", term, "'.")
    ))
  }
  list(ok = TRUE, matches = matches)
}

# Pure parser: a GraphQL body (search-hits shape OR single-disease shape) -> the
# grounded matches tibble. Separated from the fetch so it is testable offline
# against a JSON fixture. Every row carries a source_id and source_url.
resolve_disease_parse <- function(data) {
  hits <- pluck_at(data, "data", "search", "hits")
  if (!is.null(hits) && length(hits) > 0) {
    return(disease_matches_tibble(
      id = vapply(
        hits,
        function(h) as.character(pluck_at(h, "id", default = NA_character_)),
        character(1)
      ),
      name = vapply(
        hits,
        function(h) as.character(pluck_at(h, "name", default = NA_character_)),
        character(1)
      ),
      score = vapply(
        hits,
        function(h) as.numeric(pluck_at(h, "score", default = NA)),
        numeric(1)
      ),
      description = vapply(
        hits,
        function(h) {
          as.character(pluck_at(h, "description", default = NA_character_))
        },
        character(1)
      )
    ))
  }

  d <- pluck_at(data, "data", "disease")
  if (!is.null(d)) {
    return(disease_matches_tibble(
      id = as.character(pluck_at(d, "id", default = NA_character_)),
      name = as.character(pluck_at(d, "name", default = NA_character_)),
      score = NA_real_,
      description = as.character(pluck_at(
        d,
        "description",
        default = NA_character_
      ))
    ))
  }

  empty_disease_matches()
}

# First DOID cross-reference for a disease id, feeding the DISEASES seeder.
# Returns list(ok = TRUE, doid = "DOID:...") or list(ok = FALSE, error = "...").
ot_disease_doid <- function(efo_id) {
  if (is_blank(efo_id)) {
    return(list(ok = FALSE, error = "No disease id for DOID lookup."))
  }
  efo <- gsub(":", "_", trimws(efo_id))
  res <- http_post_json(
    OPENTARGETS_URL,
    body = list(query = OT_DISEASE_XREFS_QUERY, variables = list(efoId = efo)),
    source = "Open Targets"
  )
  err <- graphql_error(res, "Open Targets")
  if (!is.null(err)) {
    return(err)
  }
  doid <- ot_disease_doid_parse(res$data)
  if (is_blank(doid)) {
    return(list(
      ok = FALSE,
      error = paste0("No DOID cross-reference for '", efo, "'.")
    ))
  }
  list(ok = TRUE, doid = doid)
}

# Pure parser: a dbXRefs GraphQL body -> the first "DOID:..." xref, NA if none.
ot_disease_doid_parse <- function(data) {
  xrefs <- pluck_at(data, "data", "disease", "dbXRefs")
  if (is.null(xrefs) || length(xrefs) == 0) {
    return(NA_character_)
  }
  xrefs <- as.character(unlist(xrefs, use.names = FALSE))
  hit <- grep("^DOID:", xrefs, value = TRUE, ignore.case = TRUE)
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# A grounded disease-matches tibble. source_id / source_url anchor each row to
# its Open Targets disease record for the citation gate.
disease_matches_tibble <- function(id, name, score, description) {
  tibble::tibble(
    id = id,
    name = name,
    score = score,
    description = description,
    source_id = paste0("OpenTargets:disease:", id),
    source_url = paste0("https://platform.opentargets.org/disease/", id)
  )
}

# Empty, correctly typed matches tibble (no match / no data).
empty_disease_matches <- function() {
  tibble::tibble(
    id = character(),
    name = character(),
    score = numeric(),
    description = character(),
    source_id = character(),
    source_url = character()
  )
}
