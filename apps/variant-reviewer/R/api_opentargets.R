# Open Targets Platform client: disease associations for a gene (target).
# GraphQL API docs: https://platform-docs.opentargets.org/data-access/graphql-api

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

# Disease associations for an Ensembl gene id (target), ordered by association
# score (Open Targets returns them already sorted, highest first).
# Returns:
#   list(ok = TRUE, count, data = data.frame(disease, disease_id, score))
#   list(ok = FALSE, error = "...")
opentargets_diseases <- function(ensembl_id, size = 15) {
  if (is_blank(ensembl_id)) {
    return(list(
      ok = FALSE,
      error = "No Ensembl gene ID available for this gene."
    ))
  }

  res <- vr_api_post_json(
    OPENTARGETS_URL,
    body = list(
      query = OPENTARGETS_QUERY,
      variables = list(id = ensembl_id, size = size)
    ),
    source = "Open Targets"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  # GraphQL reports query errors in a 200 response body.
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = "Open Targets returned a query error."))
  }

  target <- pluck_at(res$data, "data", "target")
  if (is.null(target)) {
    return(list(
      ok = FALSE,
      error = paste0("Open Targets has no record for '", ensembl_id, "'.")
    ))
  }
  associated <- pluck_at(target, "associatedDiseases")
  rows <- pluck_at(associated, "rows")
  if (is.null(rows) || length(rows) == 0) {
    return(list(ok = FALSE, error = "No disease associations found."))
  }

  list(
    ok = TRUE,
    count = pluck_at(associated, "count", default = NA),
    data = opentargets_parse_rows(rows)
  )
}

# Known drugs and clinical candidates for a target. The field returns one row
# per drug with its highest clinical stage and the diseases it has been tried
# against; the element type carries `diseaseFromSource` as a fallback label when
# the mapped disease name is blank.
OPENTARGETS_DRUGS_QUERY <- paste(
  "query($id: String!) {",
  "  target(ensemblId: $id) {",
  "    drugAndClinicalCandidates {",
  "      count",
  "      rows {",
  "        maxClinicalStage",
  "        drug { id name drugType }",
  "        diseases { diseaseFromSource disease { id name } }",
  "      }",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

# Known drugs / clinical candidates for an Ensembl gene id (target).
# Returns:
#   list(ok = TRUE, count,
#        data = data.frame(drug, drug_id, drug_type, max_phase, disease))
#   list(ok = FALSE, error = "...")
opentargets_drugs <- function(ensembl_id) {
  if (is_blank(ensembl_id)) {
    return(list(
      ok = FALSE,
      error = "No Ensembl gene ID available for this gene."
    ))
  }

  res <- vr_api_post_json(
    OPENTARGETS_URL,
    body = list(
      query = OPENTARGETS_DRUGS_QUERY,
      variables = list(id = ensembl_id)
    ),
    source = "Open Targets"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = "Open Targets returned a query error."))
  }

  target <- pluck_at(res$data, "data", "target")
  if (is.null(target)) {
    return(list(
      ok = FALSE,
      error = paste0("Open Targets has no record for '", ensembl_id, "'.")
    ))
  }
  known <- pluck_at(target, "drugAndClinicalCandidates")
  rows <- pluck_at(known, "rows")
  if (is.null(rows) || length(rows) == 0) {
    return(list(
      ok = FALSE,
      error = "No known drugs or clinical candidates found."
    ))
  }

  list(
    ok = TRUE,
    count = pluck_at(known, "count", default = NA),
    data = opentargets_parse_drugs(rows)
  )
}

# Pure parser: known-drug rows ->
# data.frame(drug, drug_id, drug_type, max_phase, disease).
opentargets_parse_drugs <- function(rows) {
  # Open Targets does not order/rank a drug's `diseases` array, so there is no
  # single "lead" indication to pick out; list every indication the drug has
  # been tried against instead of arbitrarily picking the first one.
  disease_names <- function(r) {
    diseases <- pluck_at(r, "diseases")
    if (is.null(diseases) || length(diseases) == 0) {
      return(NA_character_)
    }
    nm <- vapply(
      diseases,
      function(d) {
        as.character(pluck_at(
          d,
          "disease",
          "name",
          default = pluck_at(d, "diseaseFromSource", default = NA_character_)
        ))
      },
      character(1)
    )
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm) == 0) NA_character_ else paste(unique(nm), collapse = ", ")
  }
  data.frame(
    drug = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "drug", "name", default = NA_character_))
      },
      character(1)
    ),
    drug_id = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "drug", "id", default = NA_character_))
      },
      character(1)
    ),
    drug_type = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "drug", "drugType", default = NA_character_))
      },
      character(1)
    ),
    max_phase = vapply(
      rows,
      function(r) {
        opentargets_pretty_phase(
          pluck_at(r, "maxClinicalStage", default = NA_character_)
        )
      },
      character(1)
    ),
    disease = vapply(rows, disease_names, character(1)),
    stringsAsFactors = FALSE
  )
}

# "PHASE_2" -> "Phase 2", "PRE_CLINICAL" -> "Pre clinical".
opentargets_pretty_phase <- function(x) {
  if (is_blank(x)) {
    return(NA_character_)
  }
  s <- gsub("_", " ", tolower(as.character(x)))
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# Pharmacogenomics annotations for a target: variant/genotype -> drug-response
# effect, with an evidence level. Most genes have none; a well-studied
# pharmacogene (e.g. CYP2C19) has many.
OPENTARGETS_PGX_QUERY <- paste(
  "query($id: String!) {",
  "  target(ensemblId: $id) {",
  "    pharmacogenomics {",
  "      variantRsId",
  "      genotypeId",
  "      drugs { drugFromSource }",
  "      phenotypeText",
  "      genotypeAnnotationText",
  "      evidenceLevel",
  "    }",
  "  }",
  "}",
  sep = "\n"
)

# Pharmacogenomics annotations for an Ensembl gene id (target).
# Returns:
#   list(ok = TRUE,
#        data = data.frame(rsid, drug, phenotype, genotype, evidence))
#   list(ok = FALSE, error = "...")
opentargets_pgx <- function(ensembl_id) {
  if (is_blank(ensembl_id)) {
    return(list(
      ok = FALSE,
      error = "No Ensembl gene ID available for this gene."
    ))
  }

  res <- vr_api_post_json(
    OPENTARGETS_URL,
    body = list(
      query = OPENTARGETS_PGX_QUERY,
      variables = list(id = ensembl_id)
    ),
    source = "Open Targets"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = "Open Targets returned a query error."))
  }

  target <- pluck_at(res$data, "data", "target")
  if (is.null(target)) {
    return(list(
      ok = FALSE,
      error = paste0("Open Targets has no record for '", ensembl_id, "'.")
    ))
  }
  rows <- pluck_at(target, "pharmacogenomics")
  if (is.null(rows) || length(rows) == 0) {
    return(list(
      ok = FALSE,
      error = "No pharmacogenomics annotations for this gene."
    ))
  }

  list(ok = TRUE, data = opentargets_parse_pgx(rows))
}

# Pure parser: pharmacogenomics rows ->
# data.frame(rsid, drug, phenotype, genotype, evidence). A row can name several
# drugs; they are joined for display.
opentargets_parse_pgx <- function(rows) {
  drug_names <- function(r) {
    drugs <- pluck_at(r, "drugs")
    if (is.null(drugs) || length(drugs) == 0) {
      return(NA_character_)
    }
    nm <- vapply(
      drugs,
      function(d) {
        as.character(pluck_at(d, "drugFromSource", default = NA_character_))
      },
      character(1)
    )
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm) == 0) NA_character_ else paste(unique(nm), collapse = ", ")
  }
  field <- function(name) {
    vapply(
      rows,
      function(r) as.character(pluck_at(r, name, default = NA_character_)),
      character(1)
    )
  }
  data.frame(
    rsid = field("variantRsId"),
    drug = vapply(rows, drug_names, character(1)),
    phenotype = field("phenotypeText"),
    genotype = field("genotypeAnnotationText"),
    evidence = field("evidenceLevel"),
    stringsAsFactors = FALSE
  )
}

# Pure parser: associated-disease rows -> data.frame(disease, disease_id, score).
opentargets_parse_rows <- function(rows) {
  data.frame(
    disease = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "disease", "name", default = NA_character_))
      },
      character(1)
    ),
    disease_id = vapply(
      rows,
      function(r) {
        as.character(pluck_at(r, "disease", "id", default = NA_character_))
      },
      character(1)
    ),
    score = vapply(
      rows,
      function(r) {
        as.numeric(pluck_at(r, "score", default = NA))
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}
