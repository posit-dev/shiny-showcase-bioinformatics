# Source adapter: DISEASES (Jensen Lab) ----------------------------------------
#
# Disease-driven. Text-mined + curated disease-gene associations with confidence
# scores (0-5). The API is keyed by Disease Ontology id (DOID), which we obtain
# from the Open Targets disease cross-references.

# Query one DISEASES channel ("Knowledge" = curated, "Textmining" = mined) via
# bioclients. Returns a data frame of (gene_symbol, score, url); empty on
# error or no data.
#
# Each channel is fetched SEPARATELY on purpose. bioclients also offers
# diseases_gene_associations(), which fetches both and returns the failing
# envelope if EITHER channel fails. That is stricter than this app wants: if
# Textmining is down, the curated Knowledge results are still worth having, and
# the previous implementation degraded that way. Per-channel calls preserve it.
glb_diseases_channel <- function(
  doid,
  channel = "Knowledge",
  limit = 300,
  client_fn = bioclients::diseases_channel
) {
  d <- glb_client_body(client_fn(doid, channel = channel, limit = limit))
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame())
  }
  # diseases_channel() returns symbol/protein/score/channel and, unlike
  # diseases_gene_associations(), carries NO source_url. Build the entity link
  # from the DOID so the results table and CSV export still have one.
  data.frame(
    gene_symbol = as.character(d$symbol),
    score = as.numeric(d$score),
    url = paste0(
      "https://diseases.jensenlab.org/Entity?type1=-26&type2=9606&id1=",
      utils::URLencode(doid, reserved = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

fetch_diseases <- function(
  disease,
  gene_symbols = NULL,
  doid_fn = ot_disease_xref_id,
  channel_fn = glb_diseases_channel
) {
  efo <- disease$id %||% ""
  doid <- if (!is.null(disease$doid)) disease$doid else doid_fn(efo, "DOID")
  if (is.null(doid) || is.na(doid) || !nzchar(doid)) {
    return(empty_gene_table())
  }

  knowledge <- channel_fn(doid, "Knowledge")
  textmining <- channel_fn(doid, "Textmining")
  both <- dplyr::bind_rows(knowledge, textmining)
  if (nrow(both) == 0) {
    return(empty_gene_table())
  }

  # Keep the strongest score per gene across the two channels.
  agg <- both |>
    dplyr::filter(
      !is.na(.data$gene_symbol),
      nzchar(.data$gene_symbol),
      is.finite(.data$score)
    ) |>
    dplyr::group_by(.data$gene_symbol) |>
    dplyr::summarise(
      source_score_raw = max(.data$score),
      url = first_non_na(.data$url),
      .groups = "drop"
    )
  if (nrow(agg) == 0) {
    return(empty_gene_table())
  }
  agg$evidence_type <- "diseases_association"

  as_gene_table(agg, "diseases")
}
