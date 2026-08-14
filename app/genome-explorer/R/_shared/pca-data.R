# Prepare data for a sample PCA: optionally restrict to the most variable genes,
# run prcomp on the samples, and return per-sample scores joined to metadata plus
# the variance explained by each component.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns plain data frames
# the classic Shiny module turns into a ggplot, and that a future shinyreact
# version can render client-side. This is the PCA contract.

# expr: numeric matrix, genes in rows (rownames = gene), samples in columns.
#   (de_expression_matrix() in de-data.R produces exactly this shape.)
# metadata: data frame with a `sample` column (+ grouping columns to colour by);
#   joined onto the scores by sample.
# n_top: keep the n_top most-variable genes before running PCA (the usual
#   sample-QC choice -- cuts noise from flat genes). NULL/Inf uses every gene.
# center / scale: passed to prcomp(); centering on, gene scaling off by default.
# Returns a list:
#   $scores        -- data frame: sample, PC1, PC2, ... plus all metadata columns
#   $var_explained -- named numeric, percent variance per PC (PC1, PC2, ...)
#   $n_genes       -- how many genes actually went into the PCA
#   $pcs           -- the PC column names, in order
pca_data <- function(
  expr,
  metadata,
  n_top = 500,
  center = TRUE,
  scale = FALSE
) {
  if (!is.matrix(expr)) {
    expr <- as.matrix(expr)
  }
  if (!"sample" %in% names(metadata)) {
    stop("metadata must have a 'sample' column")
  }
  if (ncol(expr) < 2) {
    stop("need at least 2 samples for PCA")
  }

  # Restrict to the most variable genes (rows). Drop zero-variance genes always,
  # since they carry no signal and break scaling.
  vars <- apply(expr, 1, stats::var, na.rm = TRUE)
  vars <- vars[is.finite(vars) & vars > 0]
  if (length(vars) < 2) {
    stop("fewer than 2 genes with non-zero variance; cannot run PCA")
  }
  keep <- names(sort(vars, decreasing = TRUE))
  if (!is.null(n_top) && is.finite(n_top)) {
    keep <- utils::head(keep, n_top)
  }
  expr <- expr[keep, , drop = FALSE]

  # PCA over samples: samples are observations, genes are features, so transpose.
  pca <- stats::prcomp(t(expr), center = center, scale. = scale)

  pct <- pca$sdev^2 / sum(pca$sdev^2) * 100
  pcs <- colnames(pca$x)
  names(pct) <- pcs

  scores <- data.frame(
    sample = rownames(pca$x),
    pca$x,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Join metadata (group, batch, ...) by sample for colouring/shaping.
  meta <- metadata
  meta$sample <- as.character(meta$sample)
  scores <- merge(scores, meta, by = "sample", all.x = TRUE, sort = FALSE)

  list(
    scores = scores,
    var_explained = pct,
    n_genes = nrow(expr),
    pcs = pcs
  )
}
