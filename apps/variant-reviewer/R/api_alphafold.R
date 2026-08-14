# AlphaFold DB client: the predicted structure for a UniProt accession. The
# lightweight JSON lookup (model URL) is separated from the (larger) PDB
# download so the parent can read structure metadata without pulling the file;
# only the viewer downloads the coordinates.
# Docs: https://alphafold.ebi.ac.uk/api-docs

ALPHAFOLD_API <- "https://alphafold.ebi.ac.uk/api/prediction"

# Model metadata for an accession (no coordinate download).
# Returns:
#   list(ok = TRUE, pdb_url, version)
#   list(ok = FALSE, error = "...")
alphafold_model <- function(accession) {
  if (is_blank(accession)) {
    return(list(
      ok = FALSE,
      error = "No UniProt accession for structure lookup."
    ))
  }
  res <- vr_api_get(ALPHAFOLD_API, path = accession, source = "AlphaFold")
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  alphafold_parse_model(res$data, accession)
}

# Pure parser: the API returns a one-element array of model records; pull the
# PDB URL out of the first record.
alphafold_parse_model <- function(data, accession = NA_character_) {
  entry <- if (is.list(data) && length(data) >= 1 && is.null(names(data))) {
    data[[1]]
  } else {
    data
  }
  pdb_url <- pluck_at(entry, "pdbUrl")
  if (is_blank(pdb_url)) {
    return(list(
      ok = FALSE,
      error = paste0("No AlphaFold model available for ", accession, ".")
    ))
  }
  list(
    ok = TRUE,
    pdb_url = as.character(pdb_url),
    version = pluck_at(entry, "latestVersion", default = NA)
  )
}

# Download the raw PDB text for a model URL. Called only when rendering the
# viewer, so a hidden structure card never pulls the file.
# Returns list(ok = TRUE, text) or list(ok = FALSE, error).
alphafold_pdb_text <- function(url) {
  if (is_blank(url)) {
    return(list(ok = FALSE, error = "No structure URL."))
  }
  req <- httr2::request(url) |>
    httr2::req_timeout(20) |>
    httr2::req_user_agent("variant-reviewer (Shiny app)")
  txt <- tryCatch(
    httr2::resp_body_string(httr2::req_perform(req)),
    error = function(e) NULL
  )
  if (is_blank(txt)) {
    return(list(
      ok = FALSE,
      error = "Could not download the AlphaFold structure."
    ))
  }
  list(ok = TRUE, text = txt)
}
