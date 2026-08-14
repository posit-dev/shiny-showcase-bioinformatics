# QuickGO (EBI): Gene Ontology functional annotation for a gene product. We take
# the gene's biological-process GO annotations as a FUNCTION signal - how the gene
# is functionally characterized, and (with a study pathway prior) whether that
# function matches the biology under review (e.g. NF1's "regulation of Ras
# protein signal transduction" against a RAS/MAPK context). Research annotation,
# never a clinical call.
#
# One GET to annotation/search, keyed by the resolved UniProt accession
# (geneProductId = UniProtKB:<acc>), asking for goName so terms are readable AND
# matchable to the context. No {ellmer} here - a plain, testable client.

QUICKGO_BASE <- "https://www.ebi.ac.uk/QuickGO/services"
QUICKGO_TERM_WEB <- "https://www.ebi.ac.uk/QuickGO/term/"

# The gene's biological-process GO annotations for a UniProt accession. Returns a
# normalized list (ok / terms tibble(go_id, go_name, evidence, reference) /
# source_id / source_url) or ok = FALSE. `aspect` defaults to biological_process
# (the axis most informative for disease mechanism); molecular_function /
# cellular_component are reachable the same way if a later signal wants them.
quickgo_annotations <- function(accession, aspect = "biological_process") {
  acc <- toupper(trimws(as.character(accession %||% "")))
  # UniProt accessions are alphanumeric (e.g. P21359, A0A0…); strip anything else.
  acc <- gsub("[^A-Z0-9]", "", acc)
  if (!nzchar(acc)) {
    return(list(ok = FALSE, error = "No UniProt accession for QuickGO lookup."))
  }
  res <- http_get_json(
    QUICKGO_BASE,
    path = "annotation/search",
    query = list(
      geneProductId = paste0("UniProtKB:", acc),
      aspect = aspect,
      includeFields = "goName",
      limit = 100
    ),
    source = "QuickGO"
  )
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, error = res$error %||% "QuickGO request failed."))
  }
  quickgo_annotations_parse(res$data, acc)
}

# Pure parser: an annotation/search body -> the distinct-term tibble. Offline. GO
# annotations repeat one term across evidence lines, so we keep one row per GO id
# (first-seen name / evidence / supporting reference). Every row carries a GO
# stable id (source_id), so each functional claim is grounded.
quickgo_annotations_parse <- function(data, accession = NA_character_) {
  recs <- pluck_at(data, "results")
  empty <- list(
    ok = FALSE,
    error = paste0("QuickGO has no annotations for '", accession, "'.")
  )
  if (is.null(recs) || length(recs) == 0) {
    return(empty)
  }
  field <- function(rec, key) {
    as.character(pluck_at(rec, key, default = NA_character_))
  }
  go_id <- vapply(recs, function(r) field(r, "goId"), character(1))
  go_name <- vapply(recs, function(r) field(r, "goName"), character(1))
  evidence <- vapply(recs, function(r) field(r, "goEvidence"), character(1))
  reference <- vapply(recs, function(r) field(r, "reference"), character(1))
  keep <- !is.na(go_id) & grepl("^GO:", go_id) & !duplicated(go_id)
  if (!any(keep)) {
    return(empty)
  }
  list(
    ok = TRUE,
    accession = accession,
    terms = tibble::tibble(
      go_id = go_id[keep],
      go_name = trimws(go_name[keep]),
      evidence = evidence[keep],
      reference = reference[keep],
      source_id = go_id[keep],
      source_url = paste0(QUICKGO_TERM_WEB, go_id[keep])
    )
  )
}
