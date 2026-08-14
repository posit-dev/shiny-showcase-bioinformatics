# Transparent, tunable ranking of aggregated genes -----------------------------
#
#   weighted_evidence = sum over all sources of weight_s * score_norm_s
#   coverage_factor   = 1 + coverage_bonus * (n_sources - 1) / (n_total - 1)
#   combined_score    = weighted_evidence * coverage_factor
#
# Genes present in only some sources are NOT penalized by zero-imputation: an
# absent source contributes nothing to the weighted sum (it is not averaged in).
# Breadth of evidence is rewarded separately and explicitly via coverage_factor.
#
# Annotation-only sources (e.g. gnomAD constraint, Pharos) DO contribute to the
# weighted sum (they nudge the prioritization) but are excluded from
# coverage: `n_sources` (from aggregate) and `n_total` count evidence sources
# only, so a constraint score does not inflate the multi-source bonus.

# `aggregated`     - output of aggregate_sources() (has score_<id> columns)
# `weights`        - named numeric vector keyed by source_id (rescaled to sum 1)
# `coverage_bonus` - 0 = pure weighted evidence; higher rewards multi-source genes
# `evidence_ids`   - source ids that count toward coverage; NULL = all sources
# `n_total`        - coverage denominator; defaults to the number of evidence
#                    score columns present
rank_genes <- function(
  aggregated,
  weights = NULL,
  coverage_bonus = 0.5,
  evidence_ids = NULL,
  n_total = NULL
) {
  score_cols <- grep("^score_", names(aggregated), value = TRUE)

  if (nrow(aggregated) == 0 || length(score_cols) == 0) {
    out <- tibble::as_tibble(aggregated)
    out$combined_score <- rep(0, nrow(aggregated))
    out$rank <- seq_len(nrow(aggregated))
    return(out)
  }

  src_ids <- sub("^score_", "", score_cols)

  if (is.null(weights)) {
    w <- rep(1, length(src_ids))
  } else {
    w <- as.numeric(weights[src_ids])
    w[is.na(w)] <- 0
  }
  names(w) <- src_ids
  if (sum(w) > 0) {
    w <- w / sum(w)
  }

  # Coverage denominator: count evidence score columns only.
  evidence_cols <- if (is.null(evidence_ids)) {
    src_ids
  } else {
    intersect(src_ids, evidence_ids)
  }
  if (is.null(n_total)) {
    n_total <- length(evidence_cols)
  }

  mat <- as.matrix(aggregated[score_cols])
  storage.mode(mat) <- "double"
  mat0 <- ifelse(is.na(mat), 0, mat)
  weighted_evidence <- as.numeric(mat0 %*% w)

  ns <- aggregated$n_sources
  coverage_factor <- if (n_total > 1) {
    1 + coverage_bonus * (ns - 1) / (n_total - 1)
  } else {
    rep(1, length(ns))
  }

  out <- tibble::as_tibble(aggregated)
  out$combined_score <- weighted_evidence * coverage_factor
  out <- out[
    order(-out$combined_score, -out$n_sources, out$gene_symbol),
    ,
    drop = FALSE
  ]
  out$rank <- seq_len(nrow(out))
  out
}
