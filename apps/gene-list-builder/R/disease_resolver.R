# Resolve a free-text disease name to ontology candidates ----------------------
#
# Uses the Open Targets search API to map a query string to disease/phenotype
# terms (EFO, MONDO, HP, ...). If the user pastes an ontology id directly we skip
# the search and look the term up by id.

DISEASE_ID_RX <- "^(EFO|MONDO|Orphanet|ORPHA|HP|DOID|NCIT|GO|OTAR)[:_]"

is_disease_id <- function(term) {
  grepl(DISEASE_ID_RX, trimws(term), ignore.case = TRUE)
}

empty_disease_candidates <- function() {
  tibble::tibble(
    id = character(),
    name = character(),
    score = numeric(),
    description = character()
  )
}

OT_SEARCH_QUERY <- "
query Search($q: String!, $size: Int!) {
  search(queryString: $q, entityNames: [\"disease\"],
         page: {index: 0, size: $size}) {
    hits { id name score description }
  }
}"

OT_DISEASE_QUERY <- "
query Disease($efoId: String!) {
  disease(efoId: $efoId) { id name description }
}"

# Returns a tibble of candidate diseases (id, name, score, description), best
# match first. Empty tibble when nothing matches or the term is blank.
resolve_disease <- function(term, limit = 10, graphql_fn = ot_graphql) {
  term <- trimws(term %||% "")
  if (!nzchar(term)) {
    return(empty_disease_candidates())
  }

  if (truthy(Sys.getenv("GLB_TEST_MODE"))) {
    return(tibble::tibble(
      id = "MONDO_0008903",
      name = "lung cancer",
      score = 1,
      description = "Offline demo disease (GLB_TEST_MODE)."
    ))
  }

  if (is_disease_id(term)) {
    # Preserve case: Open Targets ids are mixed-case for some ontologies
    # (e.g. "Orphanet_158673"), so we only swap the ":" separator for "_".
    efo <- gsub(":", "_", term)
    data <- graphql_fn(OT_DISEASE_QUERY, list(efoId = efo))
    d <- data$disease
    if (is.null(d)) {
      return(empty_disease_candidates())
    }
    return(tibble::tibble(
      id = d$id %||% efo,
      name = d$name %||% efo,
      score = NA_real_,
      description = d$description %||% NA_character_
    ))
  }

  data <- graphql_fn(OT_SEARCH_QUERY, list(q = term, size = limit))
  hits <- data$search$hits
  if (is.null(hits) || length(hits) == 0) {
    return(empty_disease_candidates())
  }

  tibble::tibble(
    id = vapply(hits, function(h) h$id %||% NA_character_, character(1)),
    name = vapply(hits, function(h) h$name %||% NA_character_, character(1)),
    score = vapply(hits, function(h) as.numeric(h$score %||% NA), numeric(1)),
    description = vapply(
      hits,
      function(h) h$description %||% NA_character_,
      character(1)
    )
  )
}
