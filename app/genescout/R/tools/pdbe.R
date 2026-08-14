# PDBe (EBI): experimentally determined 3D structures covering a gene's protein. A
# STRUCTURE / tractability ANNOTATION - a target with solved structures is more
# amenable to mechanistic and structure-guided follow-up than a "dark" one (it
# complements the Pharos ligand-tractability level with structural coverage). Each
# structure is a real PDB accession, so the signal is fully grounded. Research
# annotation, never a clinical call.
#
# One GET to the SIFTS best-structures mapping, keyed by the resolved UniProt
# accession. No {ellmer} here - a plain, testable client.

PDBE_BASE <- "https://www.ebi.ac.uk/pdbe/api"
PDBE_ENTRY_WEB <- "https://www.ebi.ac.uk/pdbe/entry/pdb/"

# Distinct experimental structures mapped to a UniProt accession. Returns a
# normalized list (ok / n distinct PDB ids / structures tibble(pdb_id, method,
# resolution, coverage) / source_id / source_url) or ok = FALSE.
pdbe_structures <- function(accession) {
  acc <- toupper(trimws(as.character(accession %||% "")))
  acc <- gsub("[^A-Z0-9]", "", acc)
  if (!nzchar(acc)) {
    return(list(ok = FALSE, error = "No UniProt accession for PDBe lookup."))
  }
  res <- http_get_json(
    PDBE_BASE,
    path = paste0("mappings/best_structures/", acc),
    source = "PDBe"
  )
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, error = res$error %||% "PDBe request failed."))
  }
  pdbe_structures_parse(res$data, acc)
}

# Pure parser: a best_structures body -> the distinct-structure tibble. Offline.
# The body is keyed by the accession, each value a list of per-chain mappings; a
# single PDB entry recurs once per chain, so we collapse to distinct pdb_id (best,
# i.e. first / highest-coverage, chain kept). Every row is grounded by its PDB id.
pdbe_structures_parse <- function(data, accession = NA_character_) {
  recs <- pluck_at(data, accession)
  if (is.null(recs) && length(data) > 0) {
    recs <- data[[1]]
  }
  empty <- list(
    ok = FALSE,
    error = paste0("PDBe has no structures for '", accession, "'.")
  )
  if (is.null(recs) || length(recs) == 0) {
    return(empty)
  }
  pdb_id <- vapply(
    recs,
    function(r) as.character(pluck_at(r, "pdb_id", default = NA_character_)),
    character(1)
  )
  method <- vapply(
    recs,
    function(r) {
      as.character(pluck_at(r, "experimental_method", default = NA_character_))
    },
    character(1)
  )
  resolution <- vapply(
    recs,
    function(r) as.numeric(pluck_at(r, "resolution", default = NA_real_)),
    numeric(1)
  )
  coverage <- vapply(
    recs,
    function(r) as.numeric(pluck_at(r, "coverage", default = NA_real_)),
    numeric(1)
  )
  keep <- !is.na(pdb_id) & nzchar(pdb_id) & !duplicated(pdb_id)
  if (!any(keep)) {
    return(empty)
  }
  ids <- pdb_id[keep]
  list(
    ok = TRUE,
    accession = accession,
    n = length(ids),
    structures = tibble::tibble(
      pdb_id = ids,
      method = method[keep],
      resolution = resolution[keep],
      coverage = coverage[keep],
      source_id = paste0("PDB:", ids),
      source_url = paste0(PDBE_ENTRY_WEB, ids)
    )
  )
}
