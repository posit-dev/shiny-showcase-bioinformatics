# Human Phenotype Ontology (HPO) client - gene -> associated diseases.
# Endpoint: https://ontology.jax.org/api  (see docs/data_sources.md)
# A gene-level gene-disease SIGNAL: the Mendelian / phenotype diseases HPO
# associates with a gene (each grounded by an OMIM / ORPHA / MONDO id). In a
# disease-scoped review the signal is whether the gene is HPO-associated with a
# disease matching the study context; otherwise the count of associated diseases.
# Research evidence for prioritization, never a clinical call. Pure client through
# http_get_json(); no {ellmer} imports. The parser + match logic are pure/offline.

HPO_BASE <- "https://ontology.jax.org/api"
HPO_WEB <- "https://ontology.jax.org/app/browse/gene/"

# Diseases HPO associates with an NCBI Gene id. Returns:
#   list(ok = TRUE, diseases = tibble(id, name, mondo), source_id, source_url)
#   list(ok = FALSE, error = "...")
hpo_gene_diseases <- function(entrez) {
  ez <- trimws(as.character(entrez %||% ""))
  if (!nzchar(ez) || is.na(ez) || !grepl("^[0-9]+$", ez)) {
    return(list(ok = FALSE, error = "No NCBI Gene id for HPO lookup."))
  }
  res <- http_get_json(
    HPO_BASE,
    path = paste0("network/annotation/NCBIGene:", ez),
    source = "HPO"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  list(
    ok = TRUE,
    diseases = hpo_diseases_parse(res$data),
    source_id = paste0("HPO:NCBIGene:", ez),
    source_url = paste0(HPO_WEB, "NCBIGene:", ez)
  )
}

# Pure parser: a network/annotation body -> tibble(id, name, mondo). Testable
# offline against a recorded fixture.
hpo_diseases_parse <- function(data) {
  recs <- pluck_at(data, "diseases")
  if (is.null(recs) || length(recs) == 0) {
    return(tibble::tibble(
      id = character(),
      name = character(),
      mondo = character()
    ))
  }
  field <- function(key) {
    vapply(
      recs,
      function(r) as.character(pluck_at(r, key, default = NA_character_)),
      character(1)
    )
  }
  tibble::tibble(
    id = field("id"),
    name = field("name"),
    mondo = field("mondoId")
  )
}

# Generic disease words dropped before matching a disease name, so two unrelated
# diseases do not match merely on "syndrome"/"cancer"/"carcinoma" etc.
HPO_DISEASE_STOP <- c(
  "syndrome",
  "disease",
  "disorder",
  "cancer",
  "carcinoma",
  "tumor",
  "tumour",
  "neoplasm",
  "familial",
  "hereditary",
  "congenital",
  "deficiency",
  "malignant",
  "benign",
  "primary",
  "chronic",
  "acute",
  "type"
)

# Meaningful disease-name tokens: the GTEx word tokenizer minus generic disease
# words, so a match reflects the specific disease, not a shared "syndrome".
hpo_disease_tokens <- function(x) {
  setdiff(gtex_tokens(x), HPO_DISEASE_STOP)
}

# The gene's HPO relevance for a review, pure so it is unit-testable:
#   present   - the gene has >= 1 associated disease (matching the context, if one
#               is given).
#   n         - count of (matching) diseases - the raw signal value.
#   matched   - the disease rows behind it (for grounded evidence).
# With a disease context, matches by MONDO equality OR a shared SPECIFIC word in
# the disease name (generic disease words dropped); without one, all associated
# diseases count.
hpo_relevance <- function(diseases, disease_ctx = NULL) {
  miss <- list(present = FALSE, n = 0, matched = diseases[0, , drop = FALSE])
  if (nrow(diseases) == 0) {
    return(miss)
  }
  if (is.null(disease_ctx) || is_blank(disease_ctx$name %||% disease_ctx$id)) {
    return(list(present = TRUE, n = nrow(diseases), matched = diseases))
  }
  want_tokens <- hpo_disease_tokens(disease_ctx$name %||% "")
  # Normalize the MONDO separator before comparing: the disease-context id is the
  # Open Targets underscore form (MONDO_0018875) while HPO's mondoId is the colon
  # form (MONDO:0018875). Canonicalize both to ':' so the id-match is not dead in
  # production (it revives grounded matches when HPO labels a disease differently).
  want_mondo <- gsub("_", ":", toupper(as.character(disease_ctx$id %||% "")))
  keep <- vapply(
    seq_len(nrow(diseases)),
    function(i) {
      mondo_hit <- nzchar(want_mondo) &&
        identical(gsub("_", ":", toupper(diseases$mondo[i])), want_mondo)
      name_hit <- length(want_tokens) > 0 &&
        length(intersect(gtex_tokens(diseases$name[i]), want_tokens)) > 0
      mondo_hit || name_hit
    },
    logical(1)
  )
  matched <- diseases[keep, , drop = FALSE]
  if (nrow(matched) == 0) {
    return(miss)
  }
  list(present = TRUE, n = nrow(matched), matched = matched)
}
