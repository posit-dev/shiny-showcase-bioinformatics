# STRING client: protein-protein interaction partners for a gene.
# Docs: https://string-db.org/help/api/

STRING_BASE <- "https://string-db.org/api"
STRING_HUMAN <- 9606

# Interaction partners for a gene symbol.
# Returns:
#   list(ok = TRUE, data = data.frame(partner, score, evidence_*))
#   list(ok = FALSE, error = "...")
string_interaction_partners <- function(
  symbol,
  species = STRING_HUMAN,
  limit = 25
) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene supplied."))
  }

  res <- vr_api_get(
    STRING_BASE,
    path = "json/interaction_partners",
    query = list(
      identifiers = trimws(as.character(symbol)),
      species = species,
      limit = limit
    ),
    source = "STRING"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }

  rows <- res$data
  if (is.null(rows) || length(rows) == 0) {
    return(list(ok = FALSE, error = "STRING found no interaction partners."))
  }

  list(ok = TRUE, data = string_parse_rows(rows))
}

# Pure parser: interaction-partner rows -> sorted data.frame of scores.
string_parse_rows <- function(rows) {
  num <- function(r, key) as.numeric(pluck_at(r, key, default = NA))
  df <- data.frame(
    partner = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "preferredName_B", default = NA_character_))
      },
      character(1)
    ),
    score = vapply(rows, function(r) num(r, "score"), numeric(1)),
    experimental = vapply(rows, function(r) num(r, "escore"), numeric(1)),
    database = vapply(rows, function(r) num(r, "dscore"), numeric(1)),
    coexpression = vapply(rows, function(r) num(r, "ascore"), numeric(1)),
    textmining = vapply(rows, function(r) num(r, "tscore"), numeric(1)),
    stringsAsFactors = FALSE
  )
  df[order(-df$score), , drop = FALSE]
}
