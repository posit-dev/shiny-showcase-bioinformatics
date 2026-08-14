# GTEx Portal client: median gene expression (TPM) across tissues.
# Docs: https://gtexportal.org/api/v2/redoc
#
# GTEx requires a *versioned* GENCODE id (e.g. ENSG00000141510.16) AND a
# datasetId. MyGene only returns the unversioned Ensembl id, so we resolve the
# versioned id from GTEx's own gene reference endpoint first.

GTEX_BASE <- "https://gtexportal.org/api/v2"
GTEX_DATASET <- "gtex_v8"

# Resolve a gene symbol to GTEx's versioned GENCODE id.
# Returns the id string, or NULL if not found.
gtex_gencode_id <- function(symbol) {
  if (is_blank(symbol)) {
    return(NULL)
  }
  res <- vr_api_get(
    GTEX_BASE,
    path = "reference/gene",
    query = list(geneId = trimws(as.character(symbol))),
    source = "GTEx"
  )
  if (!res$ok) {
    return(NULL)
  }
  rows <- res$data$data
  if (is.null(rows) || length(rows) == 0) {
    return(NULL)
  }
  pluck_at(rows[[1]], "gencodeId")
}

# Median expression across tissues for a gene symbol.
# Returns:
#   list(ok = TRUE, gencode_id, data = data.frame(tissue, median_tpm))
#   list(ok = FALSE, error = "...")
gtex_median_expression <- function(symbol) {
  gencode_id <- gtex_gencode_id(symbol)
  if (is.null(gencode_id)) {
    return(list(
      ok = FALSE,
      error = paste0("GTEx has no record for '", symbol, "'.")
    ))
  }

  res <- vr_api_get(
    GTEX_BASE,
    path = "expression/medianGeneExpression",
    query = list(
      gencodeId = gencode_id,
      datasetId = GTEX_DATASET,
      itemsPerPage = 100
    ),
    source = "GTEx"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }

  rows <- res$data$data
  if (is.null(rows) || length(rows) == 0) {
    return(list(ok = FALSE, error = "GTEx returned no expression data."))
  }

  list(ok = TRUE, gencode_id = gencode_id, data = gtex_parse_rows(rows))
}

# Pure parser: median-expression rows -> data.frame(tissue, median_tpm).
gtex_parse_rows <- function(rows) {
  df <- data.frame(
    tissue = vapply(
      rows,
      function(r) {
        gtex_pretty_tissue(pluck_at(
          r,
          "tissueSiteDetailId",
          default = NA_character_
        ))
      },
      character(1)
    ),
    median_tpm = vapply(
      rows,
      function(r) as.numeric(pluck_at(r, "median", default = NA)),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
  df[!is.na(df$median_tpm), , drop = FALSE]
}

# "Adipose_Subcutaneous" -> "Adipose Subcutaneous"
gtex_pretty_tissue <- function(x) {
  if (is_blank(x)) {
    return(NA_character_)
  }
  gsub("_", " ", x)
}
