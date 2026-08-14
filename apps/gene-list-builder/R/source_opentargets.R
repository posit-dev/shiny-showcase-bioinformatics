# Source adapter: Open Targets associated targets -----------------------------
#
# Disease-driven source. Returns the targets associated with a disease, scored
# by the Open Targets overall association score (already on 0..1).

OT_ASSOC_QUERY <- "
query Assoc($efoId: String!, $size: Int!) {
  disease(efoId: $efoId) {
    id
    name
    associatedTargets(page: {index: 0, size: $size}) {
      count
      rows {
        score
        target { id approvedSymbol }
      }
    }
  }
}"

# `disease` is a one-row data frame / list with at least `id`. `gene_symbols` is
# ignored (this is a disease-driven source). Returns a canonical gene tibble.
# Map a bioclients opentargets tibble onto canonical columns. Pure.
#
# The `url` is built here rather than taken from bioclients' `source_url`, on
# purpose. bioclients returns the per-evidence page
# (/evidence/<ensembl>/<disease>) whereas this app has always exported the
# target page (/target/<ensembl>). Those URLs go into the results table and the
# CSV export, so switching would silently change user-facing output.
ot_rows <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  data.frame(
    gene_symbol = d$symbol,
    ensembl_id = d$ensembl_id,
    source_score_raw = as.numeric(d$score),
    evidence_type = "overall_association",
    url = ifelse(
      is.na(d$ensembl_id),
      NA_character_,
      paste0("https://platform.opentargets.org/target/", d$ensembl_id)
    ),
    stringsAsFactors = FALSE
  )
}

fetch_opentargets <- function(
  disease,
  gene_symbols = NULL,
  limit = 200,
  client_fn = bioclients::opentargets_disease_targets
) {
  efo <- disease$id %||% NULL
  if (is.null(efo) || !nzchar(efo)) {
    return(empty_gene_table())
  }

  df <- ot_rows(glb_client_body(client_fn(efo, size = limit)))
  if (is.null(df) || nrow(df) == 0) {
    return(empty_gene_table())
  }

  as_gene_table(df, "opentargets")
}
