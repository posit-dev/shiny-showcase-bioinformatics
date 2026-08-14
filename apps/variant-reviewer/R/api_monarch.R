# Monarch Initiative client: HPO phenotypes associated with a gene.
# Docs: https://api.monarchinitiative.org/v3/docs
#
# Gene->phenotype associations are keyed on the HGNC id (e.g. HGNC:11998), which
# mygene_resolve() now provides. The colon-bearing entity path is built into the
# URL directly (no req_url_path_append) so it is sent to Monarch verbatim.

MONARCH_BASE <- "https://api.monarchinitiative.org/v3/api"

# HPO phenotypes associated with a gene, by HGNC id (numeric or the full
# "HGNC:11998" form).
# Returns:
#   list(ok = TRUE, count, data = data.frame(hpo_id, phenotype))
#   list(ok = FALSE, error = "...")
monarch_phenotypes <- function(hgnc, size = 50) {
  if (is_blank(hgnc)) {
    return(list(ok = FALSE, error = "No HGNC id available for this gene."))
  }
  url <- paste0(
    MONARCH_BASE,
    "/entity/",
    monarch_hgnc_id(hgnc),
    "/biolink:GeneToPhenotypicFeatureAssociation"
  )
  res <- vr_api_get(url, query = list(limit = size), source = "Monarch")
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  items <- pluck_at(res$data, "items")
  if (is.null(items) || length(items) == 0) {
    return(list(ok = FALSE, error = "No associated phenotypes found."))
  }
  list(
    ok = TRUE,
    count = pluck_at(res$data, "total", default = NA),
    data = monarch_parse_items(items)
  )
}

# Normalize an HGNC id to the CURIE form Monarch expects ("HGNC:11998").
monarch_hgnc_id <- function(hgnc) {
  id <- trimws(as.character(hgnc))
  if (grepl("^HGNC:", id, ignore.case = TRUE)) {
    toupper(id)
  } else {
    paste0("HGNC:", id)
  }
}

# Pure parser: association items -> data.frame(hpo_id, phenotype). Each item's
# `object` is the HPO term id and `object_label` its name.
monarch_parse_items <- function(items) {
  df <- data.frame(
    hpo_id = vapply(
      items,
      function(x) as.character(pluck_at(x, "object", default = NA_character_)),
      character(1)
    ),
    phenotype = vapply(
      items,
      function(x) {
        as.character(pluck_at(x, "object_label", default = NA_character_))
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  df[!is.na(df$hpo_id) & nzchar(df$hpo_id), , drop = FALSE]
}
