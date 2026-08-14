# ClinVar client via NCBI E-utilities (JSON): clinical significance, review
# status, and associated conditions for a variant.
# Docs: https://www.ncbi.nlm.nih.gov/books/NBK25500/

EUTILS_BASE <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# Look up the ClinVar classification for a search term (an rsID works best).
# Returns:
#   list(ok = TRUE, uid, accession, title, significance, review_status,
#        last_evaluated, conditions)
#   list(ok = FALSE, error = "...")
clinvar_classification <- function(term) {
  if (is_blank(term)) {
    return(list(
      ok = FALSE,
      error = "No variant identifier for ClinVar lookup."
    ))
  }

  search <- vr_api_get(
    EUTILS_BASE,
    path = "esearch.fcgi",
    query = list(db = "clinvar", term = term, retmode = "json"),
    source = "ClinVar"
  )
  if (!search$ok) {
    return(list(ok = FALSE, error = search$error))
  }
  ids <- pluck_at(search$data, "esearchresult", "idlist")
  if (is.null(ids) || length(ids) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No ClinVar record for ", term, ".")
    ))
  }
  uid <- as.character(ids[[1]])

  summary <- vr_api_get(
    EUTILS_BASE,
    path = "esummary.fcgi",
    query = list(db = "clinvar", id = uid, retmode = "json"),
    source = "ClinVar"
  )
  if (!summary$ok) {
    return(list(ok = FALSE, error = summary$error))
  }
  record <- pluck_at(summary$data, "result", uid)
  if (is.null(record)) {
    return(list(ok = FALSE, error = "ClinVar summary was unavailable."))
  }

  clinvar_parse_record(record, uid)
}

# Pure parser: an esummary ClinVar record -> normalized classification list.
clinvar_parse_record <- function(record, uid = NA_character_) {
  germline <- pluck_at(record, "germline_classification")
  list(
    ok = TRUE,
    uid = uid,
    accession = pluck_at(record, "accession", default = NA_character_),
    title = pluck_at(record, "title", default = NA_character_),
    significance = pluck_at(germline, "description", default = NA_character_),
    review_status = pluck_at(
      germline,
      "review_status",
      default = NA_character_
    ),
    last_evaluated = pluck_at(
      germline,
      "last_evaluated",
      default = NA_character_
    ),
    conditions = clinvar_conditions(germline)
  )
}

# Collapse the trait set into a readable, comma-separated condition string.
clinvar_conditions <- function(germline) {
  traits <- pluck_at(germline, "trait_set")
  if (is.null(traits) || length(traits) == 0) {
    return(NA_character_)
  }
  names <- vapply(
    traits,
    function(t) {
      as.character(pluck_at(t, "trait_name", default = NA_character_))
    },
    character(1)
  )
  names <- unique(names[!is.na(names) & nzchar(names)])
  if (length(names) == 0) NA_character_ else paste(names, collapse = "; ")
}
