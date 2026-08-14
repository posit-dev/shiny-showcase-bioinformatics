# Signature scoring: reduce an expression matrix + gene sets to per-sample
# signature scores, and rank signatures by between-group difference.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns plain matrices /
# data frames the classic Shiny modules turn into a heatmap, boxplots, and a
# ranking table, and that a future shinyreact version can render client-side.
# All methods are dependency-free base R (no GSVA/AUCell packages required).

# One sample's ssGSEA enrichment score for a gene set (Barbie et al. 2009):
# the integrated difference between the weighted cumulative distribution of the
# set's genes and the uniform cumulative distribution of the rest, over genes
# ordered by expression. `ord` orders genes by decreasing expression, `w` are
# the matching |expression|^alpha weights, `in_set` is a gene-length logical.
ssgsea_es <- function(ord, w, in_set, n) {
  member <- in_set[ord]
  w_in <- sum(w[member])
  if (w_in == 0) {
    return(0)
  }
  p_in <- cumsum(ifelse(member, w, 0)) / w_in
  p_out <- cumsum(ifelse(member, 0, 1)) / (n - sum(member))
  sum(p_in - p_out)
}

# expr: numeric matrix, genes in rows (rownames = gene), samples in columns.
# signatures: named list of gene-set character vectors.
# method:
#   "zscore"    -- z-score each gene across samples, then average over the set's
#                  genes (a module score; the dependency-free default).
#   "mean"      -- mean of the set's genes on the raw expression scale.
#   "ssgsea"    -- single-sample GSEA enrichment score (Barbie 2009): rank genes
#                  within each sample and integrate the set's weighted running
#                  enrichment. The standard single-sample pathway-activity score.
#   "singscore" -- rank-based single-sample score (Foroutan 2018), undirected:
#                  the set's mean gene rank per sample, normalised to [-1, 1].
# alpha: ssGSEA rank-weight exponent (0.25 as in the original method).
# min_genes: a set needs at least this many of its genes present in expr to be
#   scored; otherwise its row is NA (and a warning names the dropped sets).
# Returns: a signatures x samples numeric matrix.
score_signatures <- function(
  expr,
  signatures,
  method = c("zscore", "mean", "ssgsea", "singscore"),
  min_genes = 3,
  alpha = 0.25
) {
  method <- match.arg(method)
  if (!is.matrix(expr)) {
    expr <- as.matrix(expr)
  }
  if (is.null(rownames(expr))) {
    stop("expr must have rownames (genes)")
  }
  n <- nrow(expr)

  # Per-method precompute reused across every gene set.
  if (method == "zscore") {
    centers <- rowMeans(expr, na.rm = TRUE)
    sds <- apply(expr, 1, stats::sd, na.rm = TRUE)
    sds[is.na(sds) | sds == 0] <- 1 # leave zero-variance genes flat at 0
    mat <- (expr - centers) / sds
  } else if (method == "mean") {
    mat <- expr
  } else if (method == "singscore") {
    rankmat <- apply(expr, 2, rank) # genes x samples, ascending (avg ties)
  } else if (method == "ssgsea") {
    ord_list <- lapply(seq_len(ncol(expr)), function(j) {
      order(expr[, j], decreasing = TRUE)
    })
    w_list <- lapply(seq_len(ncol(expr)), function(j) {
      abs(expr[ord_list[[j]], j])^alpha
    })
  }

  out <- matrix(
    NA_real_,
    nrow = length(signatures),
    ncol = ncol(expr),
    dimnames = list(names(signatures), colnames(expr))
  )
  dropped <- character(0)
  for (s in names(signatures)) {
    g <- intersect(signatures[[s]], rownames(expr))
    if (length(g) < min_genes) {
      dropped <- c(dropped, s)
      next
    }
    m <- length(g)
    out[s, ] <- switch(
      method,
      zscore = ,
      mean = colMeans(mat[g, , drop = FALSE], na.rm = TRUE),
      singscore = {
        mean_rank <- colMeans(rankmat[g, , drop = FALSE])
        lo <- (m + 1) / 2 # theoretical min mean rank (set at the bottom)
        hi <- (2 * n - m + 1) / 2 # theoretical max (set at the top)
        2 * (mean_rank - lo) / (hi - lo) - 1
      },
      ssgsea = {
        in_set <- rownames(expr) %in% g
        vapply(
          seq_len(ncol(expr)),
          function(j) ssgsea_es(ord_list[[j]], w_list[[j]], in_set, n),
          numeric(1)
        )
      }
    )
  }
  if (length(dropped) > 0) {
    warning(
      length(dropped),
      " signature(s) had < ",
      min_genes,
      " genes present and were not scored: ",
      toString(utils::head(dropped, 5))
    )
  }
  out
}

# Tidy long form for plotting: one row per signature x sample, joined to
# metadata. scores: the matrix from score_signatures(); metadata: data frame
# with a `sample` column (+ grouping columns).
signature_scores_long <- function(scores, metadata) {
  long <- data.frame(
    signature = rep(rownames(scores), times = ncol(scores)),
    sample = rep(colnames(scores), each = nrow(scores)),
    score = as.vector(scores),
    stringsAsFactors = FALSE
  )
  meta <- metadata
  meta$sample <- as.character(meta$sample)
  merge(long, meta, by = "sample", all.x = TRUE, sort = FALSE)
}

# Rank signatures by the difference in mean score between two groups, with a
# two-sample t-test per signature (BH-adjusted across signatures).
# scores: signatures x samples matrix; group: per-sample vector aligned to
# colnames(scores); group1/group2: the two levels to contrast (diff = g2 - g1).
# Returns a data frame ordered most-significant first.
rank_signatures <- function(scores, group, group1 = NULL, group2 = NULL) {
  group <- as.character(group)
  if (length(group) != ncol(scores)) {
    stop("group must have one value per sample (column of scores)")
  }
  levs <- if (is.null(group1) || is.null(group2)) {
    u <- unique(group[!is.na(group)])
    if (length(u) != 2) {
      stop("need exactly two groups to rank; got ", length(u))
    }
    u
  } else {
    c(group1, group2)
  }
  i1 <- which(group == levs[1])
  i2 <- which(group == levs[2])

  per <- function(row) {
    x1 <- row[i1]
    x2 <- row[i2]
    p <- tryCatch(stats::t.test(x2, x1)$p.value, error = function(e) NA_real_)
    c(
      mean1 = mean(x1, na.rm = TRUE),
      mean2 = mean(x2, na.rm = TRUE),
      diff = mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE),
      pvalue = p
    )
  }
  stats_mat <- t(apply(scores, 1, per))

  res <- data.frame(
    signature = rownames(scores),
    mean1 = round(stats_mat[, "mean1"], 4),
    mean2 = round(stats_mat[, "mean2"], 4),
    diff = round(stats_mat[, "diff"], 4),
    pvalue = signif(stats_mat[, "pvalue"], 4),
    padj = signif(stats::p.adjust(stats_mat[, "pvalue"], "BH"), 4),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  names(res)[names(res) == "mean1"] <- paste0("mean_", levs[1])
  names(res)[names(res) == "mean2"] <- paste0("mean_", levs[2])
  res[order(res$pvalue, na.last = TRUE), , drop = FALSE]
}

# Leading-edge genes for one signature: the member genes' mean expression in
# each of two groups and their difference, ordered by |diff| -- i.e. the genes
# driving the signature's between-group shift. expr: genes x samples matrix;
# genes: the signature's gene set; group: per-sample vector aligned to
# colnames(expr); group1/group2: the two levels to contrast (diff = g2 - g1).
# Returns a data frame (empty if no member genes are present in expr).
signature_leading_edge <- function(expr, genes, group, group1, group2) {
  g <- intersect(as.character(genes), rownames(expr))
  group <- as.character(group)
  empty <- data.frame(
    gene = character(),
    mean1 = numeric(),
    mean2 = numeric(),
    diff = numeric(),
    stringsAsFactors = FALSE
  )
  if (length(g) == 0) {
    return(empty)
  }
  i1 <- which(group == group1)
  i2 <- which(group == group2)
  m1 <- rowMeans(expr[g, i1, drop = FALSE], na.rm = TRUE)
  m2 <- rowMeans(expr[g, i2, drop = FALSE], na.rm = TRUE)
  res <- data.frame(
    gene = g,
    mean1 = round(m1, 3),
    mean2 = round(m2, 3),
    diff = round(m2 - m1, 3),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  names(res)[names(res) == "mean1"] <- paste0("mean_", group1)
  names(res)[names(res) == "mean2"] <- paste0("mean_", group2)
  res[order(-abs(res$diff)), , drop = FALSE]
}

# Mean signature score in each group: a compact signatures x groups matrix (the
# "molecular portrait" of each subtype). scores: signatures x samples matrix;
# group: per-sample vector aligned to colnames(scores). NA groups are dropped.
summarise_scores_by_group <- function(scores, group, fun = mean) {
  group <- as.character(group)
  if (length(group) != ncol(scores)) {
    stop("group must have one value per sample (column of scores)")
  }
  keep <- !is.na(group)
  scores <- scores[, keep, drop = FALSE]
  group <- group[keep]
  levs <- sort(unique(group))
  out <- vapply(
    levs,
    function(g) {
      apply(scores[, group == g, drop = FALSE], 1, fun, na.rm = TRUE)
    },
    numeric(nrow(scores))
  )
  rownames(out) <- rownames(scores)
  colnames(out) <- levs
  out
}

# One-vs-rest markers for a target group: per signature, contrast the target
# group's scores against every other sample (Wilcoxon), returning the score
# difference and a BH-adjusted p. Surfaces the programs most specific to a
# subtype. scores: signatures x samples; group aligned to columns; target: a
# level of group. Ordered most-elevated-in-target first.
signature_group_markers <- function(scores, group, target) {
  group <- as.character(group)
  if (length(group) != ncol(scores)) {
    stop("group must have one value per sample")
  }
  keep <- !is.na(group)
  scores <- scores[, keep, drop = FALSE]
  group <- group[keep]
  i_t <- which(group == target)
  i_r <- which(group != target)
  if (length(i_t) < 1 || length(i_r) < 1) {
    stop("target group and 'rest' must both be non-empty")
  }
  per <- function(row) {
    xt <- row[i_t]
    xr <- row[i_r]
    p <- tryCatch(
      stats::wilcox.test(xt, xr)$p.value,
      error = function(e) NA_real_
    )
    c(
      mean_target = mean(xt, na.rm = TRUE),
      mean_rest = mean(xr, na.rm = TRUE),
      diff = mean(xt, na.rm = TRUE) - mean(xr, na.rm = TRUE),
      pvalue = p
    )
  }
  m <- t(apply(scores, 1, per))
  res <- data.frame(
    signature = rownames(scores),
    mean_target = round(m[, "mean_target"], 4),
    mean_rest = round(m[, "mean_rest"], 4),
    diff = round(m[, "diff"], 4),
    pvalue = signif(m[, "pvalue"], 4),
    padj = signif(stats::p.adjust(m[, "pvalue"], "BH"), 4),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  res[order(-res$diff), , drop = FALSE]
}

# Pairwise correlation between signatures across samples (which programs
# co-activate). scores: signatures x samples matrix. Returns a signatures x
# signatures correlation matrix.
signature_correlation <- function(scores, method = "pearson") {
  stats::cor(t(scores), method = method, use = "pairwise.complete.obs")
}

# Per-sample pathway fingerprint: each signature's score in one sample as a
# z-score against the cohort (how elevated/depressed that program is for this
# sample). scores: signatures x samples; sample_id: a column name. Returns a
# data frame (signature, z) ordered most-elevated first. The per-sample view a
# group-level DE analysis cannot give.
sample_fingerprint <- function(scores, sample_id) {
  if (!sample_id %in% colnames(scores)) {
    stop("sample_id not found in scores")
  }
  mu <- rowMeans(scores, na.rm = TRUE)
  sdv <- apply(scores, 1, stats::sd, na.rm = TRUE)
  sdv[!is.finite(sdv) | sdv == 0] <- NA_real_
  z <- (scores[, sample_id] - mu) / sdv
  res <- data.frame(
    signature = rownames(scores),
    z = round(as.numeric(z), 3),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  res <- res[!is.na(res$z), , drop = FALSE]
  res[order(-res$z), , drop = FALSE]
}
