# Prepare data for an expression heatmap: subset to genes of interest, optionally
# z-score each gene across samples, and order rows/columns by hierarchical
# clustering.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns a tidy data frame
# the classic Shiny module turns into a ggplot geom_tile, and that a future
# shinyreact version can render client-side. This is the heatmap contract.

# expr: numeric matrix, genes in rows (rownames = gene), samples in columns.
#   (de_expression_matrix() in de-data.R produces exactly this shape.)
# genes: genes to display; defaults to every row. Unknown genes are dropped.
# scale: "row" z-scores each gene across samples (the usual expression-heatmap
#   view, so genes on different baselines are comparable); "none" keeps values.
# cluster_rows / cluster_cols: reorder by hierarchical clustering (euclidean,
#   complete). Falls back to input order when there are fewer than 3 items.
# Returns a list:
#   $tiles   -- long data frame: gene, sample (both ordered factors), value
#   $genes   -- gene levels in display (row) order
#   $samples -- sample levels in display (column) order
#   $scale   -- the scale that was applied (for the renderer's legend/title)
heatmap_data <- function(
  expr,
  genes = NULL,
  scale = c("row", "none"),
  cluster_rows = TRUE,
  cluster_cols = FALSE
) {
  scale <- match.arg(scale)
  if (!is.matrix(expr)) {
    expr <- as.matrix(expr)
  }
  if (is.null(rownames(expr))) {
    stop("expr must have rownames (genes)")
  }

  # Subset to requested genes, preserving the caller's order and silently
  # dropping any that are absent from the matrix.
  if (!is.null(genes)) {
    genes <- as.character(genes)
    keep <- genes[genes %in% rownames(expr)]
    if (length(keep) == 0) {
      stop("none of the requested genes are present in the expression matrix")
    }
    expr <- expr[keep, , drop = FALSE]
  }

  # Row z-score. Zero-variance genes (sd 0) would divide by zero, so center
  # only and leave them flat at 0.
  if (scale == "row") {
    centers <- rowMeans(expr, na.rm = TRUE)
    sds <- apply(expr, 1, stats::sd, na.rm = TRUE)
    sds[is.na(sds) | sds == 0] <- 1
    expr <- (expr - centers) / sds
  }

  gene_order <- rownames(expr)
  sample_order <- colnames(expr)
  if (cluster_rows && nrow(expr) >= 3) {
    gene_order <- gene_order[stats::hclust(stats::dist(expr))$order]
  }
  if (cluster_cols && ncol(expr) >= 3) {
    sample_order <- sample_order[stats::hclust(stats::dist(t(expr)))$order]
  }

  tiles <- data.frame(
    gene = factor(rep(rownames(expr), times = ncol(expr)), levels = gene_order),
    sample = factor(
      rep(colnames(expr), each = nrow(expr)),
      levels = sample_order
    ),
    value = as.vector(expr),
    stringsAsFactors = FALSE
  )

  list(
    tiles = tiles,
    genes = gene_order,
    samples = sample_order,
    scale = scale
  )
}
