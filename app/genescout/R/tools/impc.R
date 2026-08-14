# IMPC (International Mouse Phenotyping Consortium): the significant phenotypes a
# whole-gene KNOCKOUT produces in mouse - direct in-vivo functional-genetics
# evidence, orthogonal to human population constraint (gnomAD) and human curation
# (HPO / UniProt-disease). Unlike literature-count signals it is a hypothesis-free
# systematic screen, so it is NOT a study-popularity proxy (obscure genes can score
# high, famous ones low). Research annotation, never a clinical call.
#
# Two GETs against the public IMPC Solr, both keyed off the HUMAN candidate symbol:
#   1. gene core: human symbol -> the mouse ortholog's MGI marker accession. A human
#      symbol with no IMPC mouse ortholog is a clean miss (the genotype-phenotype
#      core has no human-symbol field, so this mapping step is mandatory - querying
#      it by a human symbol would silently return zero).
#   2. genotype-phenotype core: the significant knockout phenotype associations for
#      that MGI accession.
# A gene IMPC never phenotyped (or phenotyped with no significant abnormality) yields
# zero significant associations -> a MISS, never a 0-that-looks-like-evidence (so an
# untested gene is never scored). No {ellmer}; pure, offline-testable parsers.

IMPC_SOLR <- "https://www.ebi.ac.uk/mi/impc/solr"
IMPC_GENE_WEB <- "https://www.mousephenotype.org/data/genes/"

# Human gene symbol -> mouse ortholog MGI marker accession (+ mouse symbol). Returns
# list(ok, mgi, marker_symbol) or ok = FALSE (no IMPC mouse ortholog).
impc_mouse_ortholog <- function(symbol) {
  sym <- gsub(
    "[^A-Za-z0-9._-]",
    "",
    toupper(trimws(as.character(symbol %||% "")))
  )
  if (!nzchar(sym)) {
    return(list(ok = FALSE, error = "No gene symbol for IMPC lookup."))
  }
  res <- http_get_json(
    IMPC_SOLR,
    path = "gene/select",
    query = list(
      q = paste0("human_gene_symbol:", sym),
      fl = "mgi_accession_id,marker_symbol",
      rows = 1,
      wt = "json"
    ),
    source = "IMPC"
  )
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, error = res$error %||% "IMPC gene lookup failed."))
  }
  impc_ortholog_parse(res$data)
}

# Pure parser: a gene-core body -> the mouse ortholog. Offline. A blank/absent
# mgi_accession_id (no IMPC mouse ortholog) is a miss.
impc_ortholog_parse <- function(data) {
  docs <- pluck_at(data, "response", "docs")
  if (is.null(docs) || length(docs) == 0) {
    return(list(ok = FALSE, error = "No IMPC mouse ortholog."))
  }
  doc <- docs[[1]]
  mgi <- as.character(pluck_at(doc, "mgi_accession_id", default = ""))
  if (!grepl("^MGI:[0-9]+$", mgi)) {
    return(list(ok = FALSE, error = "No IMPC MGI accession."))
  }
  list(
    ok = TRUE,
    mgi = mgi,
    marker_symbol = as.character(pluck_at(doc, "marker_symbol", default = NA))
  )
}

# The significant knockout phenotypes IMPC records for a human gene. Returns
# list(ok, mgi, marker_symbol, n = distinct phenotype terms, phenotypes = tibble,
# source_id, source_url) or ok = FALSE (no ortholog, or no significant phenotype).
impc_gene_phenotypes <- function(symbol) {
  ortho <- impc_mouse_ortholog(symbol)
  if (!isTRUE(ortho$ok)) {
    return(list(ok = FALSE, error = ortho$error))
  }
  res <- http_get_json(
    IMPC_SOLR,
    path = "genotype-phenotype/select",
    query = list(
      q = paste0("marker_accession_id:\"", ortho$mgi, "\""),
      fl = paste(
        "mp_term_id",
        "mp_term_name",
        "allele_accession_id",
        "allele_symbol",
        "zygosity",
        sep = ","
      ),
      rows = 500,
      wt = "json"
    ),
    source = "IMPC"
  )
  if (!isTRUE(res$ok)) {
    return(list(
      ok = FALSE,
      error = res$error %||% "IMPC phenotype lookup failed."
    ))
  }
  ph <- impc_phenotypes_parse(res$data, ortho$mgi)
  if (nrow(ph) == 0) {
    # Untested, or tested with no significant phenotype: both yield nothing to
    # ground, so a MISS (never a 0 masquerading as a real measurement).
    return(list(ok = FALSE, error = "No significant IMPC knockout phenotype."))
  }
  list(
    ok = TRUE,
    mgi = ortho$mgi,
    marker_symbol = ortho$marker_symbol,
    n = nrow(ph),
    phenotypes = ph,
    source_id = paste0("IMPC:", ortho$mgi),
    source_url = paste0(IMPC_GENE_WEB, ortho$mgi)
  )
}

# Pure parser: a genotype-phenotype body -> distinct phenotype terms. Offline. IMPC
# reports one row per (phenotype x sex x zygosity x parameter), so we collapse to
# distinct mp_term_id (MP or MPATH ontology term), keeping each term's first-seen
# name / allele / zygosity. Each term is grounded by the knockout allele + the term
# id (both returned by the query), tied to this gene's MGI accession.
impc_phenotypes_parse <- function(data, mgi = NA_character_) {
  empty <- tibble::tibble(
    mp_id = character(),
    mp_name = character(),
    allele = character(),
    zygosity = character(),
    source_id = character(),
    source_url = character()
  )
  docs <- pluck_at(data, "response", "docs")
  if (is.null(docs) || length(docs) == 0) {
    return(empty)
  }
  field <- function(key) {
    vapply(
      docs,
      function(d) as.character(pluck_at(d, key, default = NA_character_)),
      character(1)
    )
  }
  mp_id <- field("mp_term_id")
  keep <- !is.na(mp_id) & nzchar(mp_id) & !duplicated(mp_id)
  if (!any(keep)) {
    return(empty)
  }
  allele <- field("allele_accession_id")[keep]
  ids <- mp_id[keep]
  tibble::tibble(
    mp_id = ids,
    mp_name = field("mp_term_name")[keep],
    allele = allele,
    zygosity = field("zygosity")[keep],
    # Ground each phenotype on the knockout allele + the ontology term (both real,
    # both returned by the query); fall back to the gene MGI when an allele is absent.
    source_id = paste0(
      "IMPC:",
      ifelse(is.na(allele) | !nzchar(allele), mgi, allele),
      ":",
      ids
    ),
    source_url = paste0(IMPC_GENE_WEB, mgi)
  )
}
