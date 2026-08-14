# UniProt (Swiss-Prot): the diseases manually curated as involving a gene. Each
# DISEASE comment is an expert-reviewed gene-disease link grounded by a stable
# UniProt disease id (DI-xxxxx) and, usually, an OMIM/MIM cross-reference. It is an
# independent curation channel corroborating HPO / ClinGen / DISEASES from a
# different source. Research evidence for prioritization, never a clinical call.
#
# One GET to the UniProtKB entry JSON (fields = cc_disease), keyed by the resolved
# Swiss-Prot accession. No {ellmer} here - a plain, testable client; the parser and
# the context-relevance match are pure/offline.

UNIPROT_REST <- "https://rest.uniprot.org/uniprotkb"
UNIPROT_DISEASE_WEB <- "https://www.uniprot.org/diseases/"

# The diseases UniProt curates for a Swiss-Prot accession. Returns:
#   list(ok = TRUE, diseases = tibble(id, name, acronym, mim, causal, source_id,
#        source_url), source_url)
#   list(ok = FALSE, error = "...")
uniprot_gene_diseases <- function(accession) {
  acc <- toupper(trimws(as.character(accession %||% "")))
  acc <- gsub("[^A-Z0-9]", "", acc)
  if (!nzchar(acc)) {
    return(list(ok = FALSE, error = "No UniProt accession for lookup."))
  }
  res <- http_get_json(
    UNIPROT_REST,
    path = paste0(acc, ".json"),
    query = list(fields = "cc_disease"),
    source = "UniProt"
  )
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, error = res$error %||% "UniProt request failed."))
  }
  uniprot_disease_parse(res$data)
}

# Pure parser: a UniProtKB entry body -> the disease tibble. Offline. Keeps only
# DISEASE comments that name a disease; `causal` distinguishes "caused by variants"
# (Mendelian, strong) from "may be involved in pathogenesis" (weaker), read from
# the comment note. Each row is grounded by the UniProt disease accession
# (DI-xxxxx); the MIM cross-reference is kept for display.
uniprot_disease_parse <- function(data) {
  comments <- pluck_at(data, "comments")
  acc <- as.character(pluck_at(
    data,
    "primaryAccession",
    default = NA_character_
  ))
  empty <- list(
    ok = FALSE,
    error = "UniProt has no curated disease involvement for this entry."
  )
  if (is.null(comments) || length(comments) == 0) {
    return(empty)
  }
  rows <- list()
  for (cm in comments) {
    if (!identical(pluck_at(cm, "commentType"), "DISEASE")) {
      next
    }
    dis <- pluck_at(cm, "disease")
    di_id <- as.character(pluck_at(dis, "diseaseAccession", default = ""))
    name <- as.character(pluck_at(dis, "diseaseId", default = ""))
    if (!nzchar(di_id) || !nzchar(name)) {
      next
    }
    xref <- pluck_at(dis, "diseaseCrossReference")
    mim <- if (identical(pluck_at(xref, "database"), "MIM")) {
      as.character(pluck_at(xref, "id", default = NA_character_))
    } else {
      NA_character_
    }
    note <- tolower(paste(
      unlist(pluck_at(cm, "note", "texts") %||% list(), use.names = FALSE),
      collapse = " "
    ))
    rows[[length(rows) + 1L]] <- list(
      id = di_id,
      name = name,
      acronym = as.character(pluck_at(dis, "acronym", default = "")),
      mim = mim,
      causal = grepl("caused by", note, fixed = TRUE)
    )
  }
  if (length(rows) == 0) {
    return(empty)
  }
  field <- function(key) vapply(rows, function(r) r[[key]], character(1))
  ids <- field("id")
  list(
    ok = TRUE,
    accession = acc,
    diseases = tibble::tibble(
      id = ids,
      name = field("name"),
      acronym = field("acronym"),
      mim = field("mim"),
      causal = vapply(rows, function(r) isTRUE(r$causal), logical(1)),
      source_id = paste0("UniProt:", ids),
      source_url = paste0(UNIPROT_DISEASE_WEB, ids)
    )
  )
}

# Generic disease words dropped before matching a disease name, so two unrelated
# diseases do not match merely on a shared "syndrome"/"cancer"/"carcinoma".
UNIPROT_DISEASE_STOP <- c(
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
  "susceptibility",
  "type"
)

# The gene's UniProt-disease relevance for a review, pure so it is unit-testable:
#   present - the gene has >= 1 curated disease (matching the context, if given).
#   n       - count of (matching) diseases - the raw signal value.
#   matched - the disease rows behind it (grounded evidence).
# With a disease context, matches on a shared SPECIFIC word in the disease
# name/acronym (generic words dropped); without one, all curated diseases count.
# (UniProt cross-references MIM, but the study context id is a MONDO/EFO id, so
# name-token matching - not id equality - is what aligns the two here.)
uniprot_disease_relevance <- function(diseases, disease_ctx = NULL) {
  miss <- list(present = FALSE, n = 0, matched = diseases[0, , drop = FALSE])
  if (nrow(diseases) == 0) {
    return(miss)
  }
  if (is.null(disease_ctx) || is_blank(disease_ctx$name %||% disease_ctx$id)) {
    return(list(present = TRUE, n = nrow(diseases), matched = diseases))
  }
  want_tokens <- setdiff(
    gtex_tokens(disease_ctx$name %||% ""),
    UNIPROT_DISEASE_STOP
  )
  keep <- vapply(
    seq_len(nrow(diseases)),
    function(i) {
      name_toks <- setdiff(
        gtex_tokens(paste(diseases$name[i], diseases$acronym[i])),
        UNIPROT_DISEASE_STOP
      )
      length(want_tokens) > 0 && length(intersect(name_toks, want_tokens)) > 0
    },
    logical(1)
  )
  matched <- diseases[keep, , drop = FALSE]
  if (nrow(matched) == 0) {
    return(miss)
  }
  list(present = TRUE, n = nrow(matched), matched = matched)
}
