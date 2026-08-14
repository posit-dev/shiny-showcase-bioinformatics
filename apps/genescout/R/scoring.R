# Composite technical ranking.
#
# Each source contributes a normalized [0,1] signal; the composite is a WEIGHTED
# MEAN across all registry signals, where a missing signal contributes 0 to the
# numerator but its weight stays in the denominator. So a gene supported broadly
# but moderately outranks one that is famous in a single source - this is what
# structurally defeats the familiar-gene bias.
#
# Weights and normalization midpoints live in rubric.yml (edit there to retune -
# no code change). Everything here is pure and deterministic, so rankings are
# reproducible run-to-run and independent of which other genes are in the list.

# Load the ranking rubric (per-signal weights + normalization midpoints).
load_rubric <- function(
  path = genescout_app_path("rubric.yml"),
  profile = "default"
) {
  if (!file.exists(path)) {
    stop("Rubric file not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)
  if (is.null(cfg[[profile]])) {
    stop("Rubric profile not found: ", profile, call. = FALSE)
  }
  cfg[[profile]]
}

# --- Normalizers: raw signal -> [0,1]. NA or negative -> 0 (no positive signal).

# Identity clamp, for signals already on a 0-1 scale (e.g. Open Targets score).
normalize_identity <- function(x) {
  x <- ifelse(is.na(x), 0, x)
  pmin(pmax(x, 0), 1)
}

# Saturating x/(x+m): raw = m normalizes to 0.5, with diminishing returns above.
# Absolute (not cohort-relative): adding or removing genes never changes a gene's
# normalized value, so rankings stay reproducible.
normalize_saturating <- function(m) {
  force(m)
  function(x) {
    x <- ifelse(is.na(x) | x < 0, 0, x)
    x / (x + m)
  }
}

# Log-saturating, for heavy-tailed counts (e.g. PMID hits):
# log1p(x) / (log1p(x) + log1p(m)).
normalize_log_saturating <- function(m) {
  force(m)
  k <- log1p(m)
  function(x) {
    x <- ifelse(is.na(x) | x < 0, 0, x)
    l <- log1p(x)
    l / (l + k)
  }
}

# Descending saturating m/(x+m): x = 0 -> 1, x = m -> 0.5, larger x -> 0. For
# "lower is better" signals (e.g. gnomAD LOEUF, where a small value means strong
# loss-of-function constraint) so that every normalized column stays
# higher-is-better and the composite can treat all signals uniformly. NA or
# negative -> 0 (no positive signal). Absolute, so it stays reproducible.
normalize_saturating_desc <- function(m) {
  force(m)
  function(x) {
    out <- m / (x + m)
    ifelse(is.na(x) | x < 0, 0, out)
  }
}

# Corroboration across the USER'S OWN sources: absolute, saturating on (count-1).
# A gene in 1 source scores 0 (no bonus, and never sub-baseline); 2 -> 0.5;
# 3 -> 0.667; 4 -> 0.75, saturating toward 1. NA or < 1 -> 0. Same closure family
# as normalize_saturating; the (count-1) shift makes a lone source neutral rather
# than penalized, and the saturation caps how far breadth alone can carry a gene.
normalize_corroboration <- function(m) {
  force(m)
  function(x) {
    e <- ifelse(is.na(x) | x < 1, 0, x - 1)
    e / (e + m)
  }
}

# --- Composite + rank -------------------------------------------------------

# Weighted mean of the normalized per-signal columns (`<key>_n`) over the
# registry, role-aware:
#   - EVIDENCE signals: weight is always in the denominator, so a missing one
#     contributes 0 to the numerator but still divides - breadth is rewarded and
#     a narrow-but-famous gene is structurally beaten by a broadly-supported one.
#   - ANNOTATION signals (e.g. constraint, druggability, connectivity): only ever
#     nudge the score UP. Each gene's present annotations are folded into its mean
#     greedily, largest normalized value first, and one is included ONLY while it is
#     at least the gene's RUNNING composite at that point - so an inclusion can
#     never pull the mean down. An absent or a below-running annotation is neutral.
#     This makes "annotations only nudge up" a hard guarantee: a gene is never
#     ranked below an otherwise-identical gene for carrying an extra, weaker
#     annotation (gating on the fixed evidence-only mean instead would let an
#     annotation between that floor and the running mean drag the composite down).
# `weights` (a named key -> weight vector) overrides the registry weights, which
# is how the live UI sliders re-rank without re-querying. `coverage_bonus`
# optionally rewards genes supported by many EVIDENCE sources.
compute_composite <- function(
  gene_matrix,
  registry,
  weights = NULL,
  coverage_bonus = FALSE
) {
  if (nrow(gene_matrix) == 0) {
    gene_matrix$composite <- numeric()
    return(gene_matrix)
  }
  keys <- vapply(registry, function(s) s$key, character(1))
  roles <- vapply(registry, function(s) s$role %||% "evidence", character(1))
  reg_w <- vapply(registry, function(s) s$weight, numeric(1))
  # A weights override is PARTIAL: it overrides only the keys it names (the UI
  # sliders), and any key it omits keeps its registry weight - so a discovery-only
  # signal without a slider still counts at its rubric weight.
  w <- if (is.null(weights)) {
    reg_w
  } else {
    ov <- as.numeric(weights[keys])
    ifelse(is.na(ov), reg_w, ov)
  }
  w[is.na(w)] <- 0

  is_ann <- roles == "annotation"
  norm <- as.matrix(gene_matrix[, paste0(keys, "_n"), drop = FALSE])
  norm[is.na(norm)] <- 0

  # Evidence: a weighted mean whose denominator always carries every evidence
  # weight, so a missing evidence signal contributes 0 but still divides - breadth
  # is rewarded and a narrow-but-famous gene is beaten by a broadly-supported one.
  w_ev <- w
  w_ev[is_ann] <- 0
  denom_ev <- sum(w[!is_ann])
  numer_ev <- as.numeric(norm %*% w_ev)

  n <- nrow(gene_matrix)
  numer <- numer_ev
  denom <- rep(denom_ev, n)

  # Annotations nudge up, never penalize. Fold each gene's PRESENT annotations
  # greedily - largest normalized value first - including one only while its value
  # is at least the gene's RUNNING composite, so every inclusion weakly RAISES the
  # mean and none can pull it down. Because they are sorted descending and the
  # running mean only rises, the first annotation that falls below it means all
  # remaining ones do too (stop there). Gating on the running mean (not the fixed
  # evidence-only mean) is what guarantees a connected/annotated gene is never
  # ranked below an otherwise-identical gene that lacks that annotation.
  if (any(is_ann)) {
    ann_keys <- keys[is_ann]
    ann_w <- w[is_ann]
    ann_norm <- matrix(0, nrow = n, ncol = length(ann_keys))
    ann_pres <- matrix(FALSE, nrow = n, ncol = length(ann_keys))
    for (j in seq_along(ann_keys)) {
      nk <- as.numeric(gene_matrix[[paste0(ann_keys[j], "_n")]])
      nk[is.na(nk)] <- 0
      ann_norm[, j] <- nk
      pcol <- paste0(ann_keys[j], "_present")
      pres <- if (pcol %in% names(gene_matrix)) {
        as.logical(gene_matrix[[pcol]])
      } else {
        rep(TRUE, n) # minimal stub matrix: treat as present
      }
      pres[is.na(pres)] <- FALSE
      ann_pres[, j] <- pres & (ann_w[j] > 0)
    }
    for (i in seq_len(n)) {
      idx <- which(ann_pres[i, ])
      if (length(idx) == 0) {
        next
      }
      ord <- idx[order(ann_norm[i, idx], decreasing = TRUE)]
      num_i <- numer[i]
      den_i <- denom[i]
      for (j in ord) {
        if (ann_norm[i, j] >= num_i / den_i) {
          num_i <- num_i + ann_w[j] * ann_norm[i, j]
          den_i <- den_i + ann_w[j]
        } else {
          break # sorted descending: no later annotation can qualify either
        }
      }
      numer[i] <- num_i
      denom[i] <- den_i
    }
  }

  composite <- ifelse(denom > 0, numer / denom, 0)
  if (isTRUE(coverage_bonus)) {
    n_ev <- sum(!is_ann)
    cov <- if (n_ev > 0 && "n_evidence_present" %in% names(gene_matrix)) {
      gene_matrix$n_evidence_present / n_ev
    } else {
      0
    }
    composite <- composite * (0.5 + 0.5 * cov)
  }
  gene_matrix$composite <- composite
  gene_matrix
}

# The coverage-bonus toggle from the rubric, defaulting to FALSE when the rubric
# file is not on the current path (e.g. in unit tests that pass a stub registry).
rubric_coverage_bonus <- function() {
  tryCatch(isTRUE(load_rubric()$coverage_bonus), error = function(e) FALSE)
}

# Deterministic rank by composite (descending); symbol is the stable tie-break.
# A vetoed candidate (from the caveats stage) is forced below every non-vetoed
# gene via a large rank-score offset - its composite is left untouched (so the
# report shows the real evidence score), only its position changes.
rank_genes <- function(gene_matrix) {
  if (nrow(gene_matrix) == 0) {
    gene_matrix$rank <- integer()
    return(gene_matrix)
  }
  vetoed <- if ("vetoed" %in% names(gene_matrix)) {
    as.logical(gene_matrix$vetoed)
  } else {
    rep(FALSE, nrow(gene_matrix))
  }
  vetoed[is.na(vetoed)] <- FALSE
  rank_score <- ifelse(
    vetoed,
    gene_matrix$composite - 1000,
    gene_matrix$composite
  )
  gene_matrix$rank <- rank(-rank_score, ties.method = "min")
  gene_matrix[order(gene_matrix$rank, gene_matrix$symbol), , drop = FALSE]
}

# Grade thresholds on the composite (0-1) - a UI badge only; rank is by composite.
GENESCOUT_GRADE_BREAKS <- c(high = 0.5, moderate = 0.2)

grade_for_score <- function(score) {
  ifelse(
    is.na(score),
    "Insufficient",
    ifelse(
      score >= GENESCOUT_GRADE_BREAKS[["high"]],
      "High",
      ifelse(score >= GENESCOUT_GRADE_BREAKS[["moderate"]], "Moderate", "Low")
    )
  )
}
