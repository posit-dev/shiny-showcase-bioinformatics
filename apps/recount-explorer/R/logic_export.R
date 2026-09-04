# Export layer. Shiny-free: reproducible-script generation and data frames
# ready for CSV download.

# Full sample metadata with list columns collapsed, so it survives write.csv.
flatten_coldata <- function(rse) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  is_list <- vapply(cd, is.list, logical(1))
  cd[is_list] <- lapply(cd[is_list], function(col) {
    vapply(
      col,
      function(x) paste(as.character(x), collapse = "; "),
      character(1)
    )
  })
  cbind(sample = colnames(rse), cd)
}

# log2 CPM matrix as a data.frame with gene ids in the first column.
expression_export_df <- function(study) {
  cbind(
    data.frame(gene_id = rownames(study$log_expr)),
    as.data.frame(study$log_expr)
  )
}

# A standalone R script that reproduces the current session: study load,
# log2 CPM, QC scatter, and (when set in the app) the PCA and gene views.
# The script only needs recount3 and ggplot2, not this app.
build_reproduction_script <- function(
  study,
  gene_state = NULL,
  pca_state = NULL,
  quality_state = NULL
) {
  header <- c(
    sprintf(
      "# Reproduces a Recount Explorer session for study %s.",
      study$project
    ),
    sprintf(
      "# Generated on %s with R %s, recount3 %s.",
      format(Sys.Date()),
      getRversion(),
      utils::packageVersion("recount3")
    ),
    "#",
    "# Data: recount3 (Wilks et al. 2021, Genome Biology 22:323,",
    "# https://doi.org/10.1186/s13059-021-02533-6).",
    sprintf(
      "# Study accession: %s (%s, %s).",
      study$project,
      toupper(study$source),
      study$organism
    ),
    "",
    "library(recount3)",
    "library(ggplot2)",
    ""
  )

  load_block <- c(
    "# Load the study and scale the raw counts.",
    sprintf('projects <- available_projects(organism = "%s")', study$organism),
    "proj_info <- subset(",
    "  projects,",
    sprintf('  project == "%s" &', study$project),
    sprintf('    file_source == "%s" &', study$source),
    '    project_type == "data_sources"',
    ")",
    "rse <- create_rse(proj_info)",
    'assay(rse, "counts") <- transform_counts(rse)',
    "",
    "# log2 CPM, as used by every view in the app.",
    'lib <- pmax(colSums(assay(rse, "counts")), 1)',
    'log_expr <- log2(sweep(assay(rse, "counts"), 2, lib, "/") * 1e6 + 1)',
    ""
  )

  qc_block <- c(
    "# QC: library size vs detected genes.",
    "qc <- data.frame(",
    '  library_size = colSums(assay(rse, "counts")),',
    '  detected_genes = colSums(assay(rse, "counts") > 0)',
    ")",
    "ggplot(qc, aes(library_size, detected_genes)) +",
    "  geom_point(alpha = 0.6) +",
    "  scale_x_log10() +",
    '  labs(x = "Library size (log scale)", y = "Detected genes")',
    ""
  )

  quality_block <- NULL
  if (!is.null(quality_state)) {
    quality_block <- c(
      "# Library composition: share of each sample's reads by gene biotype.",
      "biotype <- as.character(rowData(rse)$gene_type)",
      'totals <- rowsum(assay(rse, "counts"), group = biotype)',
      "share <- sweep(totals, 2, colSums(totals), \"/\") * 100",
      sprintf(
        "top <- head(names(sort(rowMeans(share), decreasing = TRUE)), %d)",
        quality_state$top_biotypes %||% 8
      ),
      "composition <- data.frame(",
      "  sample = rep(colnames(share), each = length(top)),",
      "  biotype = rep(top, times = ncol(share)),",
      "  share = as.numeric(share[top, ])",
      ")",
      "ggplot(composition, aes(sample, share, fill = biotype)) +",
      "  geom_col(width = 1) +",
      '  labs(x = "Sample", y = "Share of reads (%)")',
      "",
      "# Quality metrics that recount3 computed during alignment.",
      "qc_columns <- c(",
      paste0(
        "  ",
        sprintf('"%s"', unname(QC_METRICS)),
        c(rep(",", length(QC_METRICS) - 1L), "")
      ),
      ")",
      "qc_columns <- qc_columns[qc_columns %in% colnames(colData(rse))]",
      "metrics <- data.frame(",
      "  sample = rep(colnames(rse), times = length(qc_columns)),",
      "  metric = rep(qc_columns, each = ncol(rse)),",
      "  value = unlist(lapply(qc_columns, function(x) colData(rse)[[x]]))",
      ")",
      "ggplot(metrics, aes(metric, value)) +",
      "  geom_boxplot(outlier.shape = NA) +",
      "  geom_jitter(width = 0.2, alpha = 0.6) +",
      '  facet_wrap(~metric, scales = "free") +',
      "  labs(x = NULL, y = NULL)",
      ""
    )
  }

  pca_block <- NULL
  if (!is.null(pca_state)) {
    color_code <- if (nzchar(pca_state$color_by)) {
      sprintf(
        'pca_df$color <- as.character(colData(rse)[["%s"]])',
        pca_state$color_by
      )
    } else {
      'pca_df$color <- "sample"'
    }
    pca_block <- c(
      sprintf("# PCA on the top %d variable genes.", pca_state$n_genes),
      "vars <- rowSums((log_expr - rowMeans(log_expr))^2)",
      sprintf(
        "top <- head(order(vars, decreasing = TRUE), %d)",
        pca_state$n_genes
      ),
      "pca <- prcomp(t(log_expr[top, ]), center = TRUE)",
      "pca_df <- as.data.frame(pca$x[, 1:2])",
      color_code,
      "ggplot(pca_df, aes(PC1, PC2, color = color)) +",
      "  geom_point(alpha = 0.7)",
      ""
    )
  }

  gene_block <- NULL
  if (!is.null(gene_state) && nzchar(gene_state$gene)) {
    group_code <- if (nzchar(gene_state$group_by)) {
      sprintf(
        '  group = as.character(colData(rse)[["%s"]])',
        gene_state$group_by
      )
    } else {
      '  group = "all samples"'
    }
    geom_code <- if (identical(gene_state$geom, "violin")) {
      "  geom_violin(alpha = 0.7) +"
    } else {
      "  geom_boxplot(alpha = 0.7, outlier.shape = NA) +"
    }
    gene_block <- c(
      sprintf("# Expression of %s.", gene_state$gene_label),
      "gene_df <- data.frame(",
      sprintf('  expression = log_expr["%s", ],', gene_state$gene),
      group_code,
      ")",
      "ggplot(gene_df, aes(group, expression, fill = group)) +",
      geom_code,
      "  geom_jitter(width = 0.15, alpha = 0.4) +",
      sprintf('  labs(y = "log2 CPM", title = "%s") +', gene_state$gene_label),
      '  guides(fill = "none")',
      ""
    )
  }

  paste(
    c(header, load_block, qc_block, quality_block, pca_block, gene_block),
    collapse = "\n"
  )
}
