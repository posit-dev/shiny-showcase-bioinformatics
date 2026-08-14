# Open Targets Platform client - gene/disease associations.
# Endpoint: https://api.platform.opentargets.org/api/v4/graphql (see docs/data_sources.md)
# Returns disease associations for a gene, each tagged with an Open Targets
# source id so the citation gate can ground it. Phase 0 vertical-slice client.
# Pure client through http_post_json(); no {ellmer} imports.

OPENTARGETS_URL <- "https://api.platform.opentargets.org/api/v4/graphql"

OPENTARGETS_QUERY <- paste(
  "query($id: String!, $size: Int!) {",
  "  target(ensemblId: $id) {",
  "    approvedSymbol",
  "    associatedDiseases(page: {index: 0, size: $size}) {",
  "      count",
  "      rows { score disease { id name } }",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

# Disease -> associated targets (the discovery direction): the genes Open Targets
# associates with a disease, each with its overall association score.
OPENTARGETS_TARGETS_QUERY <- paste(
  "query($id: String!, $size: Int!) {",
  "  disease(efoId: $id) {",
  "    name",
  "    associatedTargets(page: {index: 0, size: $size}) {",
  "      count",
  "      rows { score target { id approvedSymbol } }",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

# Disease associations for a gene (symbol or Ensembl id), ordered by association
# score (Open Targets returns them sorted, highest first).
# Returns:
#   list(ok = TRUE, gene, symbol, ensembl_id, count,
#        evidence = tibble(disease, disease_id, score, source_id, source_url))
#   list(ok = FALSE, gene, error = "...")
gene_disease_assoc <- function(gene, size = 20) {
  ensembl <- gene_disease_ensembl(gene)
  if (is.null(ensembl$id)) {
    return(list(ok = FALSE, gene = gene, error = ensembl$error))
  }

  res <- http_post_json(
    OPENTARGETS_URL,
    body = list(
      query = OPENTARGETS_QUERY,
      variables = list(id = ensembl$id, size = size)
    ),
    source = "Open Targets"
  )
  err <- graphql_error(res, "Open Targets")
  if (!is.null(err)) {
    return(c(err, list(gene = gene)))
  }

  target <- pluck_at(res$data, "data", "target")
  if (is.null(target)) {
    return(list(
      ok = FALSE,
      gene = gene,
      error = paste0("Open Targets has no record for '", ensembl$id, "'.")
    ))
  }
  rows <- pluck_at(target, "associatedDiseases", "rows")

  list(
    ok = TRUE,
    gene = gene,
    symbol = pluck_at(target, "approvedSymbol", default = gene),
    ensembl_id = ensembl$id,
    count = pluck_at(target, "associatedDiseases", "count", default = NA),
    evidence = opentargets_parse_rows(rows, ensembl$id)
  )
}

# Resolve a gene argument to an Ensembl gene id. Accepts an Ensembl id directly
# or resolves a symbol via MyGene. Returns list(id = <chr|NULL>, error).
gene_disease_ensembl <- function(gene) {
  if (grepl("^ENSG\\d+", trimws(gene %||% ""), ignore.case = TRUE)) {
    return(list(id = trimws(gene), error = NULL))
  }
  resolved <- resolve_symbol(gene)
  if (!isTRUE(resolved$ok) || is_blank(resolved$ensembl_gene)) {
    return(list(
      id = NULL,
      error = paste0("Could not resolve '", gene, "' to an Ensembl gene id.")
    ))
  }
  list(id = resolved$ensembl_gene, error = NULL)
}

# Pure parser: associated-disease rows -> a grounded evidence tibble. Every row
# carries a source_id and a source_url pointing at the Open Targets record.
# Separated from the fetch so it is testable offline against a JSON fixture.
opentargets_parse_rows <- function(rows, ensembl_id) {
  if (is.null(rows) || length(rows) == 0) {
    return(tibble::tibble(
      disease = character(),
      disease_id = character(),
      score = numeric(),
      source_id = character(),
      source_url = character()
    ))
  }
  disease_id <- vapply(
    rows,
    function(r) {
      as.character(pluck_at(r, "disease", "id", default = NA_character_))
    },
    character(1)
  )
  tibble::tibble(
    disease = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "disease", "name", default = NA_character_))
      },
      character(1)
    ),
    disease_id = disease_id,
    score = vapply(
      rows,
      function(r) as.numeric(pluck_at(r, "score", default = NA)),
      numeric(1)
    ),
    source_id = paste0("OpenTargets:", ensembl_id, ":", disease_id),
    source_url = paste0(
      "https://platform.opentargets.org/evidence/",
      ensembl_id,
      "/",
      disease_id
    )
  )
}

# Disease -> associated targets (discovery direction). `disease_id` is an Open
# Targets efoId (underscore form, e.g. MONDO_0018975). Returns:
#   list(ok = TRUE, disease_id, count,
#        genes = tibble(symbol, ensembl_id, score, source_id, source_url))
#   list(ok = FALSE, error = "...")
ot_disease_targets <- function(disease_id, size = 1000) {
  if (is_blank(disease_id)) {
    return(list(ok = FALSE, error = "No disease id for Open Targets."))
  }
  id <- gsub(":", "_", trimws(disease_id))
  res <- http_post_json(
    OPENTARGETS_URL,
    body = list(
      query = OPENTARGETS_TARGETS_QUERY,
      variables = list(id = id, size = size)
    ),
    source = "Open Targets"
  )
  err <- graphql_error(res, "Open Targets")
  if (!is.null(err)) {
    return(err)
  }
  disease <- pluck_at(res$data, "data", "disease")
  if (is.null(disease)) {
    return(list(
      ok = FALSE,
      error = paste0("Open Targets has no disease '", id, "'.")
    ))
  }
  rows <- pluck_at(disease, "associatedTargets", "rows")
  list(
    ok = TRUE,
    disease_id = id,
    count = pluck_at(disease, "associatedTargets", "count", default = NA),
    genes = opentargets_targets_parse(rows, id)
  )
}

# Pure parser: disease -> associated-target rows -> a grounded gene tibble. Every
# row carries a source_id and a source_url pointing at the Open Targets evidence.
opentargets_targets_parse <- function(rows, disease_id) {
  if (is.null(rows) || length(rows) == 0) {
    return(tibble::tibble(
      symbol = character(),
      ensembl_id = character(),
      score = numeric(),
      source_id = character(),
      source_url = character()
    ))
  }
  ensembl_id <- vapply(
    rows,
    function(r) {
      as.character(pluck_at(r, "target", "id", default = NA_character_))
    },
    character(1)
  )
  tibble::tibble(
    symbol = vapply(
      rows,
      function(r) {
        as.character(pluck_at(
          r,
          "target",
          "approvedSymbol",
          default = NA_character_
        ))
      },
      character(1)
    ),
    ensembl_id = ensembl_id,
    score = vapply(
      rows,
      function(r) as.numeric(pluck_at(r, "score", default = NA)),
      numeric(1)
    ),
    source_id = paste0("OpenTargets:", ensembl_id, ":", disease_id),
    source_url = paste0(
      "https://platform.opentargets.org/evidence/",
      ensembl_id,
      "/",
      disease_id
    )
  )
}
