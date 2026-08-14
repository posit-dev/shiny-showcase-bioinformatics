# Signature Scoring data contract: construct, validate, and load a dataset for
# scoring gene-set signatures across samples. The single source of truth for the
# shape of data every Signature Scoring view (score heatmap, group comparison,
# ranking) relies on.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Shared by the classic
# Shiny and shinyreact versions of the app.

# metadata needs at least these; expression mirrors the DE contract (a "gene"
# column + one numeric column per sample, log2-normalized).
signature_metadata_required <- c("sample", "group")

# Construct a signature_dataset from:
#   expression: a "gene" column + one numeric column per sample.
#   metadata:   one row per sample (sample, group, ...).
#   signatures: a named list of gene-set character vectors (e.g. MSigDB Hallmark).
new_signature_dataset <- function(expression, metadata, signatures) {
  structure(
    list(
      expression = expression,
      metadata = metadata,
      signatures = signatures
    ),
    class = "signature_dataset"
  )
}

# Validate the contract. Errors loudly on the first problem it finds so data
# issues surface at load time rather than deep inside a score.
validate_signature_dataset <- function(x) {
  stopifnot(inherits(x, "signature_dataset"))

  if (!"gene" %in% names(x$expression)) {
    stop("expression must have a 'gene' column")
  }
  miss_meta <- setdiff(signature_metadata_required, names(x$metadata))
  if (length(miss_meta) > 0) {
    stop("metadata is missing required column(s): ", toString(miss_meta))
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

  # Signatures: a non-empty, uniquely-named list of non-empty gene-set vectors.
  if (!is.list(x$signatures) || length(x$signatures) == 0) {
    stop("signatures must be a non-empty named list of gene-set vectors")
  }
  nms <- names(x$signatures)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("signatures must have unique, non-empty names")
  }
  empty <- nms[lengths(x$signatures) == 0]
  if (length(empty) > 0) {
    stop("signature(s) contain no genes: ", toString(utils::head(empty, 5)))
  }

  # At least one signature must share genes with the expression matrix, or the
  # gene identifiers don't match (e.g. symbols vs Ensembl) and nothing scores.
  any_overlap <- any(vapply(
    x$signatures,
    function(g) any(g %in% x$expression$gene),
    logical(1)
  ))
  if (!any_overlap) {
    stop("no signature shares a gene with the expression matrix (id mismatch?)")
  }

  x
}

# Public constructor: build and validate in one step.
signature_dataset <- function(expression, metadata, signatures) {
  validate_signature_dataset(
    new_signature_dataset(expression, metadata, signatures)
  )
}

# Read a signature_dataset from a directory holding the contract files:
#   expression.csv, metadata.csv, and signatures.csv (long form: signature, gene).
read_signature_dataset <- function(dir) {
  read_one <- function(name) {
    path <- file.path(dir, name)
    if (!file.exists(path)) {
      stop("expected file not found: ", path)
    }
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
  sig_df <- read_one("signatures.csv")
  if (!all(c("signature", "gene") %in% names(sig_df))) {
    stop("signatures.csv must have 'signature' and 'gene' columns")
  }
  # Long form -> named list, preserving signature order of first appearance.
  signatures <- split(
    as.character(sig_df$gene),
    factor(sig_df$signature, levels = unique(sig_df$signature))
  )
  signature_dataset(
    expression = read_one("expression.csv"),
    metadata = read_one("metadata.csv"),
    signatures = signatures
  )
}

# Convenience: the expression matrix as a numeric matrix with gene rownames
# (samples as columns), the shape scoring code wants.
signature_expression_matrix <- function(x) {
  stopifnot(inherits(x, "signature_dataset"))
  m <- as.matrix(x$expression[,
    setdiff(names(x$expression), "gene"),
    drop = FALSE
  ])
  rownames(m) <- x$expression$gene
  m
}
