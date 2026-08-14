# DE Explorer data contract: construct, validate, and load a differential-
# expression dataset. This is the single source of truth for the shape of data
# that every DE Explorer view (volcano, table, heatmap, PCA) relies on.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Shared by the classic
# Shiny and shinyreact versions of the app.

# Required columns for each table in the contract.
de_results_required <- c("gene", "logFC", "AveExpr", "pvalue", "padj")
de_metadata_required <- c("sample", "group")

# Construct a de_dataset from three data frames:
#   de_results: one row per gene (gene, logFC, AveExpr, pvalue, padj, ...)
#   expression: a "gene" column + one numeric column per sample (log2-normalized)
#   metadata:   one row per sample (sample, group, ...)
new_de_dataset <- function(de_results, expression, metadata) {
  structure(
    list(
      de_results = de_results,
      expression = expression,
      metadata = metadata
    ),
    class = "de_dataset"
  )
}

# Validate the contract. Errors loudly on the first problem it finds so data
# issues surface at load time rather than deep inside a plot.
validate_de_dataset <- function(x) {
  stopifnot(inherits(x, "de_dataset"))

  miss_de <- setdiff(de_results_required, names(x$de_results))
  if (length(miss_de) > 0) {
    stop("de_results is missing required column(s): ", toString(miss_de))
  }
  miss_meta <- setdiff(de_metadata_required, names(x$metadata))
  if (length(miss_meta) > 0) {
    stop("metadata is missing required column(s): ", toString(miss_meta))
  }
  if (!"gene" %in% names(x$expression)) {
    stop("expression must have a 'gene' column")
  }

  # Sample columns of the expression matrix must match the metadata samples.
  expr_samples <- setdiff(names(x$expression), "gene")
  meta_samples <- as.character(x$metadata$sample)
  if (!setequal(expr_samples, meta_samples)) {
    stop(
      "expression sample columns do not match metadata$sample (",
      length(expr_samples),
      " vs ",
      length(meta_samples),
      ")"
    )
  }

  # Every gene with a DE result must exist in the expression matrix.
  missing_genes <- setdiff(x$de_results$gene, x$expression$gene)
  if (length(missing_genes) > 0) {
    stop(
      length(missing_genes),
      " gene(s) in de_results are absent from the expression matrix (e.g. ",
      toString(utils::head(missing_genes, 3)),
      ")"
    )
  }

  x
}

# Public constructor: build and validate in one step.
de_dataset <- function(de_results, expression, metadata) {
  validate_de_dataset(new_de_dataset(de_results, expression, metadata))
}

# Read a de_dataset from a directory holding the three contract CSVs.
read_de_dataset <- function(dir) {
  read_one <- function(name) {
    path <- file.path(dir, name)
    if (!file.exists(path)) {
      stop("expected file not found: ", path)
    }
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
  de_dataset(
    de_results = read_one("de_results.csv"),
    expression = read_one("expression.csv"),
    metadata = read_one("metadata.csv")
  )
}

# Classify genes Up / Down / NS by the dual cutoff every DE view shares: a gene
# is significant when its p is below p_threshold, then Up/Down by the sign of a
# |logFC| >= lfc_threshold change. Returns a factor with fixed levels (Down, NS,
# Up) so volcano, table, and any future view agree on a gene gene-for-gene.
#
# logFC, p: equal-length numeric vectors. NA in either leaves the gene at NS
# (logical-NA positions are simply never promoted out of the seeded "NS").
de_status <- function(logFC, p, lfc_threshold = 1, p_threshold = 0.05) {
  sig <- !is.na(p) & p < p_threshold
  status <- rep("NS", length(logFC))
  status[sig & logFC >= lfc_threshold] <- "Up"
  status[sig & logFC <= -lfc_threshold] <- "Down"
  factor(status, levels = c("Down", "NS", "Up"))
}

# Convenience: the expression matrix as a numeric matrix with gene rownames
# (samples as columns), the shape most plotting/stats code wants.
de_expression_matrix <- function(x) {
  stopifnot(inherits(x, "de_dataset"))
  m <- as.matrix(x$expression[,
    setdiff(names(x$expression), "gene"),
    drop = FALSE
  ])
  rownames(m) <- x$expression$gene
  m
}
