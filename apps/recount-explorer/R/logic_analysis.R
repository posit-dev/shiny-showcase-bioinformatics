# Analysis layer. Shiny-free: QC summaries, PCA, and per-gene expression
# frames consumed by the view modules.

row_variances <- function(mat) {
  rowSums((mat - rowMeans(mat))^2) / max(ncol(mat) - 1, 1)
}

top_variable_genes <- function(mat, n = 500) {
  v <- row_variances(mat)
  utils::head(order(v, decreasing = TRUE), min(n, nrow(mat)))
}

# PCA on the top variable genes of a genes x samples log-expression matrix.
#
# Returns sample scores for the first PCs, variance explained, and the gene
# loadings. The loadings answer why the samples separate, which the scores
# alone never do, and they cost nothing to keep: the matrix is only the top
# variable genes wide, not the whole genome.
run_pca <- function(log_expr, n_genes = 500, n_pcs = 10) {
  idx <- top_variable_genes(log_expr, n_genes)
  p <- stats::prcomp(t(log_expr[idx, , drop = FALSE]), center = TRUE)
  n_pcs <- min(n_pcs, ncol(p$x))
  list(
    scores = as.data.frame(p$x[, seq_len(n_pcs), drop = FALSE]),
    var_explained = (p$sdev^2 / sum(p$sdev^2))[seq_len(n_pcs)],
    loadings = p$rotation[, seq_len(n_pcs), drop = FALSE]
  )
}

# The genes that push a principal component hardest, both ways.
#
# Ranked on absolute loading but the sign is kept, because "high in one group"
# and "high in the other" are different answers and a reader needs both.
pca_loadings <- function(pca, symbols = NULL, pc = 1, n = 20) {
  pc <- min(max(as.integer(pc), 1L), ncol(pca$loadings))
  v <- pca$loadings[, pc]
  idx <- utils::head(order(abs(v), decreasing = TRUE), min(n, length(v)))
  gene_id <- names(v)[idx]
  label <- if (!is.null(symbols) && all(gene_id %in% names(symbols))) {
    unname(symbols[gene_id])
  } else {
    gene_id
  }
  data.frame(
    gene_id = gene_id,
    label = label,
    loading = unname(v[idx]),
    direction = ifelse(v[idx] >= 0, "positive", "negative"),
    stringsAsFactors = FALSE
  )
}

# Per-sample QC: library size and number of detected genes.
sample_qc <- function(rse) {
  counts <- SummarizedExperiment::assay(rse, "counts")
  data.frame(
    sample = colnames(rse),
    library_size = colSums(counts),
    detected_genes = colSums(counts > 0)
  )
}

# Keep the most common levels and lump the rest, so a 30-level column still
# produces a legend you can read.
#
# Widening the colour palette stops the plot erroring, but thirty boxes and
# thirty legend keys are unreadable whatever colour they are. NA becomes its
# own level rather than disappearing, because "not recorded" is usually the
# group worth seeing.
collapse_levels <- function(x, max_levels = 12) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "not recorded"
  present <- unique(x)
  if (length(present) <= max_levels) {
    return(factor(x, levels = sort(present)))
  }
  counts <- sort(table(x), decreasing = TRUE)
  keep <- names(counts)[seq_len(max_levels - 1L)]
  other <- sprintf("other (%d more)", length(present) - length(keep))
  x[!x %in% keep] <- other
  factor(x, levels = c(sort(keep), other))
}

# Long data frame for one gene: expression plus a grouping column pulled from
# the sample metadata (or a single "all samples" group when none is chosen).
gene_expression_df <- function(
  study,
  gene_id,
  group_by = NULL,
  max_levels = 12
) {
  cd <- as.data.frame(SummarizedExperiment::colData(study$rse))
  group <- if (
    !is.null(group_by) && nzchar(group_by) && group_by %in% names(cd)
  ) {
    collapse_levels(cd[[group_by]], max_levels)
  } else {
    factor(rep("all samples", ncol(study$rse)))
  }
  data.frame(
    sample = colnames(study$rse),
    expression = as.numeric(study$log_expr[gene_id, ]),
    group = group
  )
}

# ---- quality metrics ---------------------------------------------------------

# The QC columns worth showing, mapped to readable names.
#
# recount3 ships 109 numeric recount_qc columns per sample. These six are the
# ones a reader actually judges a sample on: how much of it mapped, how much
# landed in genes, and how much is mitochondrial or intronic. The same list
# picks the default columns for the metadata table, so the two views agree on
# what matters.
QC_METRICS <- c(
  "Uniquely mapped %" = "recount_qc.star.uniquely_mapped_reads_.",
  "Assigned to genes %" = "recount_qc.gene_fc.all_.",
  "Mitochondrial %" = "recount_qc.aligned_reads..chrm",
  "Intronic %" = "recount_qc.intron_sum_.",
  "Multi-mapping %" = "recount_qc.star.._of_reads_mapped_to_multiple_loci",
  "Mean fragment length" = "recount_qc.bc_frag.mean_length"
)

# Which of the wanted columns this study actually has something to show for.
#
# recount3 repeats several star.* columns with a ".2" suffix and fills them
# with zeros. Plotting one draws a flat line at zero and looks like a broken
# study rather than a duplicated column, so anything constant is dropped here.
usable_metric_columns <- function(cd, wanted = QC_METRICS) {
  present <- wanted[wanted %in% names(cd)]
  keep <- vapply(
    present,
    function(name) {
      v <- suppressWarnings(as.numeric(cd[[name]]))
      v <- v[is.finite(v)]
      length(v) > 0 && stats::var(v) > 0
    },
    logical(1)
  )
  present[keep]
}

# Per-sample QC metrics in long form, ready to facet.
qc_metrics <- function(
  rse,
  wanted = QC_METRICS,
  group_by = NULL,
  max_levels = 12
) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  cols <- usable_metric_columns(cd, wanted)
  if (!length(cols)) {
    return(NULL)
  }
  group <- if (
    !is.null(group_by) && nzchar(group_by) && group_by %in% names(cd)
  ) {
    collapse_levels(cd[[group_by]], max_levels)
  } else {
    factor(rep("all samples", ncol(rse)))
  }
  out <- do.call(
    rbind,
    lapply(seq_along(cols), function(i) {
      data.frame(
        sample = colnames(rse),
        metric = names(cols)[i],
        value = suppressWarnings(as.numeric(cd[[cols[[i]]]])),
        group = group,
        stringsAsFactors = FALSE
      )
    })
  )
  out$metric <- factor(out$metric, levels = names(cols))
  out[is.finite(out$value), , drop = FALSE]
}

# Flag values past 1.5 IQR from the quartiles, within each metric.
#
# The usual boxplot rule. Applied per metric because a mapping percentage and
# a fragment length share no scale.
flag_outliers <- function(df, value = "value", within = "metric") {
  split_on <- if (within %in% names(df)) df[[within]] else rep(1L, nrow(df))
  unsplit(
    lapply(split(df[[value]], split_on), function(v) {
      q <- stats::quantile(v, c(0.25, 0.75), na.rm = TRUE)
      span <- 1.5 * (q[[2]] - q[[1]])
      v < q[[1]] - span | v > q[[2]] + span
    }),
    split_on
  )
}

# ---- library composition -----------------------------------------------------

# Share of each sample's reads by gene biotype, as a percentage.
#
# The standard library-composition check: a sample sitting at 40% rRNA is a
# sample to drop, and nothing else in the app would show it. Biotypes are
# ranked by mean share so the lumped "other" is always the small tail.
biotype_composition <- function(rse, top_n = 8) {
  rd <- SummarizedExperiment::rowData(rse)
  if (!"gene_type" %in% names(rd)) {
    return(NULL)
  }
  counts <- SummarizedExperiment::assay(rse, "counts")
  biotype <- as.character(rd$gene_type)
  biotype[is.na(biotype) | !nzchar(biotype)] <- "unannotated"

  totals <- rowsum(counts, group = biotype, reorder = FALSE)
  share <- sweep(totals, 2, pmax(colSums(totals), 1), "/") * 100

  ranked <- names(sort(rowMeans(share), decreasing = TRUE))
  keep <- utils::head(ranked, top_n)
  if (length(ranked) > length(keep)) {
    other <- colSums(share[setdiff(ranked, keep), , drop = FALSE])
    share <- rbind(share[keep, , drop = FALSE], other)
    rownames(share) <- c(
      keep,
      sprintf("other (%d more)", length(ranked) - length(keep))
    )
  } else {
    share <- share[keep, , drop = FALSE]
  }

  long <- data.frame(
    sample = rep(colnames(share), each = nrow(share)),
    biotype = factor(
      rep(rownames(share), times = ncol(share)),
      levels = rownames(share)
    ),
    share = as.numeric(share),
    stringsAsFactors = FALSE
  )
  # Order samples by how much is not protein coding, so anything contaminated
  # gathers at one end instead of hiding in the middle.
  impurity <- 100 - share[rownames(share) == "protein_coding", , drop = TRUE]
  if (!length(impurity)) {
    impurity <- stats::setNames(rep(0, ncol(share)), colnames(share))
  }
  long$sample <- factor(long$sample, levels = names(sort(impurity)))
  long
}

# ---- sex signal --------------------------------------------------------------

SEX_COLUMNS <- c(
  chrx = "recount_qc.aligned_reads..chrx",
  chry = "recount_qc.aligned_reads..chry"
)

# Percentage of reads on each sex chromosome, per sample.
#
# Two clouds mean the study mixes donors, which is worth knowing before
# designing any comparison. One cloud is also an answer.
sex_signal <- function(rse, columns = SEX_COLUMNS) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  if (!all(columns %in% names(cd))) {
    return(NULL)
  }
  out <- data.frame(
    sample = colnames(rse),
    chrx = suppressWarnings(as.numeric(cd[[columns[["chrx"]]]])),
    chry = suppressWarnings(as.numeric(cd[[columns[["chry"]]]])),
    stringsAsFactors = FALSE
  )
  out[is.finite(out$chrx) & is.finite(out$chry), , drop = FALSE]
}

# ---- expression distribution -------------------------------------------------

# A five-number summary of log2 CPM per sample.
#
# Summarising here rather than handing ggplot2 every value is what keeps this
# affordable: a 500-sample study is 32 million numbers, and the boxplot only
# ever draws five of them per sample.
expression_quantiles <- function(
  log_expr,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
) {
  q <- apply(log_expr, 2, stats::quantile, probs = probs, na.rm = TRUE)
  out <- data.frame(
    sample = colnames(log_expr),
    ymin = q[1, ],
    lower = q[2, ],
    middle = q[3, ],
    upper = q[4, ],
    ymax = q[5, ],
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out$sample <- factor(out$sample, levels = out$sample[order(out$middle)])
  out
}

# ---- sample correlation ------------------------------------------------------

# Sample-to-sample correlation on the top variable genes, clustered.
#
# Shows batch structure and lone outliers that PCA can compress away. Ordered
# by hierarchical clustering so related samples sit together, which is the
# whole point of drawing it as a grid.
sample_correlation <- function(log_expr, n_genes = 2000, method = "spearman") {
  if (ncol(log_expr) < 3) {
    return(NULL)
  }
  idx <- top_variable_genes(log_expr, n_genes)
  m <- stats::cor(log_expr[idx, , drop = FALSE], method = method)
  ord <- tryCatch(
    stats::hclust(stats::as.dist(1 - m))$order,
    error = function(e) seq_len(ncol(m))
  )
  m[ord, ord, drop = FALSE]
}

# The clustered matrix as one row per cell, which is what geom_tile wants.
correlation_long <- function(m) {
  samples <- colnames(m)
  data.frame(
    row = factor(rep(samples, times = length(samples)), levels = samples),
    col = factor(rep(samples, each = length(samples)), levels = rev(samples)),
    correlation = as.numeric(m),
    stringsAsFactors = FALSE
  )
}
