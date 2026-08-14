# ClinVar gene-level client - count of (likely) pathogenic germline variants.
# Endpoint: https://eutils.ncbi.nlm.nih.gov/entrez/eutils (E-utilities esearch)
# Used as a gene-level relevance SIGNAL (how much pathogenic variation ClinVar
# records for a gene), never as a clinical call. Pure client through the shared
# HTTP wrapper; no {ellmer} imports. The variant-level ClinVar lookup lives in
# R/tools/variant_effect.R.

CLINVAR_EUTILS_BASE <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
CLINVAR_WEB_BASE <- "https://www.ncbi.nlm.nih.gov/clinvar"

# Count of pathogenic / likely-pathogenic germline variants ClinVar records for a
# gene symbol. Returns:
#   list(ok = TRUE, symbol, count, query, source_id, source_url)
#   list(ok = FALSE, error = "...")
# A genuine 0 (gene present in ClinVar, no P/LP variants) is ok = TRUE, count = 0
# - kept distinct from a lookup failure (ok = FALSE) by callers.
clinvar_gene_pathogenic_count <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for ClinVar lookup."))
  }
  term <- clinvar_gene_pathogenic_term(symbol)
  query <- list(db = "clinvar", term = term, retmode = "json", retmax = 0)
  key <- Sys.getenv("NCBI_API_KEY")
  if (nzchar(key)) {
    query$api_key <- key
  }
  res <- http_get_json(
    CLINVAR_EUTILS_BASE,
    path = "esearch.fcgi",
    query = query,
    source = "ClinVar"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  count <- clinvar_gene_count_parse(res$data)
  if (is.na(count)) {
    return(list(ok = FALSE, error = "ClinVar returned no count."))
  }
  list(
    ok = TRUE,
    symbol = symbol,
    count = count,
    query = term,
    source_id = paste0("ClinVar:gene:", toupper(symbol), ":path"),
    source_url = paste0(
      CLINVAR_WEB_BASE,
      "/?term=",
      utils::URLencode(term, reserved = TRUE)
    )
  )
}

# The esearch term: germline pathogenic OR likely-pathogenic records for a gene.
clinvar_gene_pathogenic_term <- function(symbol) {
  paste0(
    symbol,
    "[gene] AND (\"pathogenic\"[Clinical significance] OR ",
    "\"likely pathogenic\"[Clinical significance])"
  )
}

# Disease-scoped variant of clinvar_gene_pathogenic_term: pathogenic /
# likely-pathogenic records for a gene AND a disease/phenotype. Used in discovery
# mode so the ClinVar signal reflects the study context, not the gene's total.
clinvar_gene_disease_term <- function(symbol, disease) {
  paste0(clinvar_gene_pathogenic_term(symbol), " AND \"", disease, "\"[dis]")
}

# Count of pathogenic / likely-pathogenic variants ClinVar records for a gene in
# a disease context. Same contract as clinvar_gene_pathogenic_count().
clinvar_gene_disease_pathogenic_count <- function(symbol, disease) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for ClinVar lookup."))
  }
  if (is_blank(disease)) {
    return(clinvar_gene_pathogenic_count(symbol))
  }
  term <- clinvar_gene_disease_term(symbol, disease)
  query <- list(db = "clinvar", term = term, retmode = "json", retmax = 0)
  key <- Sys.getenv("NCBI_API_KEY")
  if (nzchar(key)) {
    query$api_key <- key
  }
  res <- http_get_json(
    CLINVAR_EUTILS_BASE,
    path = "esearch.fcgi",
    query = query,
    source = "ClinVar"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  count <- clinvar_gene_count_parse(res$data)
  if (is.na(count)) {
    return(list(ok = FALSE, error = "ClinVar returned no count."))
  }
  list(
    ok = TRUE,
    symbol = symbol,
    count = count,
    query = term,
    source_id = paste0("ClinVar:gene:", toupper(symbol), ":dis:path"),
    source_url = paste0(
      CLINVAR_WEB_BASE,
      "/?term=",
      utils::URLencode(term, reserved = TRUE)
    )
  )
}

# Pure parser: an esearch JSON body -> the integer hit count, NA if absent.
# "0" parses to 0L (a real zero, not NA). Separated from the fetch so it is
# testable offline against a fixture.
clinvar_gene_count_parse <- function(esearch_data) {
  raw <- pluck_at(esearch_data, "esearchresult", "count", default = NA)
  if (is_blank(raw)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(raw))
}
