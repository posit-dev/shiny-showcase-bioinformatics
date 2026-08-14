# Human Protein Atlas (HPA) client - disease/cancer classification of a gene.
# Endpoint: https://www.proteinatlas.org/<ensembl>.json  (see docs/data_sources.md)
# HPA's per-gene record carries curated "Protein class" and "Disease involvement"
# tags (e.g. "Cancer-related genes", "Tumor suppressor", "Disease related genes").
# The SIGNAL is how many disease/cancer-relevant classifications HPA assigns the
# gene - a research-evidence annotation of disease relevance, never a clinical call.
# Pure client through http_get_json(); no {ellmer} imports. Parser + scoring are
# pure/offline. (HPA also carries tissue/subcellular/cancer-prognostic data,
# reserved for later signals.)

HPA_BASE <- "https://www.proteinatlas.org"
HPA_WEB <- "https://www.proteinatlas.org/"

# The disease/cancer classifications GeneScout counts (lower-cased for matching).
HPA_DISEASE_CLASSES <- c(
  "cancer-related genes",
  "disease related genes",
  "human disease related genes",
  "disease variant",
  "tumor suppressor",
  "proto-oncogene",
  "candidate cancer biomarkers"
)

# Fetch a gene's HPA record by Ensembl id. Returns:
#   list(ok = TRUE, gene, protein_class, disease_involvement, source_id, source_url)
#   list(ok = FALSE, error = "...")
hpa_gene <- function(ensembl) {
  if (is_blank(ensembl) || !grepl("^ENSG[0-9]+", ensembl)) {
    return(list(ok = FALSE, error = "No Ensembl gene id for HPA lookup."))
  }
  res <- http_get_json(
    HPA_BASE,
    path = paste0(ensembl, ".json"),
    source = "HPA"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  p <- hpa_parse(res$data)
  list(
    ok = TRUE,
    gene = p$gene,
    protein_class = p$protein_class,
    disease_involvement = p$disease_involvement,
    source_id = paste0("HPA:", ensembl),
    source_url = paste0(HPA_WEB, ensembl)
  )
}

# Pure parser: an HPA gene body -> the fields we use. `Protein class` and `Disease
# involvement` are string-or-array; normalize both to character vectors. Testable
# offline against a recorded fixture.
hpa_parse <- function(data) {
  as_chr <- function(x) {
    if (is.null(x)) {
      return(character())
    }
    as.character(unlist(x, use.names = FALSE))
  }
  list(
    gene = as.character(pluck_at(data, "Gene", default = NA_character_)),
    protein_class = as_chr(pluck_at(data, "Protein class")),
    disease_involvement = as_chr(pluck_at(data, "Disease involvement"))
  )
}

# The gene's HPA disease/cancer relevance, pure so it is unit-testable:
#   present - HPA assigns >= 1 disease/cancer classification.
#   n       - count of DISTINCT disease/cancer classifications (the raw value).
#   tags    - the matched classification labels (for grounded evidence).
hpa_relevance <- function(hpa) {
  tags <- unique(tolower(c(
    hpa$disease_involvement %||% character(),
    hpa$protein_class %||% character()
  )))
  hits_idx <- tags %in% HPA_DISEASE_CLASSES
  # Return the original-cased labels for the matched tags (nicer evidence).
  orig <- unique(c(
    hpa$disease_involvement %||% character(),
    hpa$protein_class %||% character()
  ))
  matched <- orig[tolower(orig) %in% HPA_DISEASE_CLASSES]
  list(present = length(matched) > 0, n = length(matched), tags = matched)
}
