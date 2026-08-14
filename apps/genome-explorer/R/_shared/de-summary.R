# Pure, Shiny-free text summaries of DE Explorer's views, for the AI assistant.
#
# Each function takes plain data (never reactives) and returns a compact,
# human-readable string the model can quote and interpret. Kept free of shiny::
# so it stays in the logic layer -- unit-testable and reusable by a future React
# port. The DE app wraps these as ellmer tools, feeding them a snapshot of the
# current app state (see apps/de-explorer/shiny/app.R).

.de_fmt_p <- function(x) formatC(x, format = "g", digits = 3)

# Cohort meta: sample/gene counts, group sizes, metadata columns.
de_summarise_dataset <- function(dataset, label = NULL) {
  meta <- dataset$metadata
  expr <- de_expression_matrix(dataset)
  grp <- if ("group" %in% names(meta)) table(meta$group) else NULL
  paste(
    c(
      if (!is.null(label)) paste0("Dataset: ", label, "."),
      paste0("Samples: ", nrow(meta), "; genes measured: ", nrow(expr), "."),
      if (!is.null(grp)) {
        paste0(
          "Groups: ",
          paste(
            sprintf("%s (n=%d)", names(grp), as.integer(grp)),
            collapse = ", "
          ),
          "."
        )
      },
      paste0(
        "Metadata columns: ",
        paste(setdiff(names(meta), "sample"), collapse = ", "),
        "."
      ),
      paste0("Genes in the DE results table: ", nrow(dataset$de_results), ".")
    ),
    collapse = "\n"
  )
}

# PCA: variance explained by PC1/PC2 and mean PC1 by the colour-by group.
de_summarise_pca <- function(dataset, colour_by = "group", n_top = 500) {
  pd <- pca_data(de_expression_matrix(dataset), dataset$metadata, n_top = n_top)
  ve <- pd$var_explained
  lines <- c(
    paste0("PCA on the top ", pd$n_genes, " most-variable genes."),
    paste0(
      "Variance explained: PC1 ",
      .de_fmt_p(ve[["PC1"]]),
      "%, PC2 ",
      .de_fmt_p(ve[["PC2"]]),
      "%."
    )
  )
  if (colour_by %in% names(pd$scores) && !is.numeric(pd$scores[[colour_by]])) {
    by <- split(pd$scores$PC1, pd$scores[[colour_by]])
    means <- vapply(by, mean, numeric(1))
    lines <- c(
      lines,
      paste0(
        "Mean PC1 by ",
        colour_by,
        ": ",
        paste(sprintf("%s %.1f", names(means), means), collapse = ", "),
        " (clear separation along PC1 suggests this axis captures the contrast)."
      )
    )
  }
  paste(lines, collapse = "\n")
}

# Results table: status counts + the top up/down genes at the given thresholds.
de_summarise_table <- function(
  de_results,
  lfc = 1,
  p = 0.05,
  p_col = "padj",
  n = 10
) {
  tbl <- de_results_table(de_results, lfc, p, p_col)
  counts <- de_results_status_counts(tbl)
  rows <- function(status) {
    d <- utils::head(tbl[tbl$status == status, , drop = FALSE], n)
    if (nrow(d) == 0) {
      return("(none)")
    }
    paste(
      sprintf(
        "%s (logFC %.2f, %s %s)",
        d$gene,
        d$logFC,
        p_col,
        .de_fmt_p(
          d[[p_col]]
        )
      ),
      collapse = "; "
    )
  }
  paste(
    sprintf("Thresholds: |log2 fold change| >= %g, %s <= %g.", lfc, p_col, p),
    sprintf(
      "Up: %d; Down: %d; not-significant: %d.",
      counts[["Up"]],
      counts[["Down"]],
      counts[["NS"]]
    ),
    paste0("Top up-regulated: ", rows("Up"), "."),
    paste0("Top down-regulated: ", rows("Down"), "."),
    sep = "\n"
  )
}

# Volcano: status counts, most significant, and largest fold-change genes.
de_summarise_volcano <- function(
  de_results,
  lfc = 1,
  p = 0.05,
  p_col = "padj"
) {
  vd <- volcano_data(de_results, lfc, p, p_col, label_top_n = 0)
  counts <- volcano_status_counts(vd)
  sig <- vd[vd$status != "NS", , drop = FALSE]
  join <- function(d) {
    if (nrow(d) == 0) "(none)" else paste(d$gene, collapse = ", ")
  }
  paste(
    sprintf("Volcano at |log2 fold change| >= %g, %s <= %g.", lfc, p_col, p),
    sprintf(
      "Up: %d; Down: %d; not-significant: %d.",
      counts[["Up"]],
      counts[["Down"]],
      counts[["NS"]]
    ),
    paste0(
      "Most significant: ",
      join(utils::head(sig[order(sig$p), , drop = FALSE], 8)),
      "."
    ),
    paste0(
      "Largest fold changes: ",
      join(utils::head(sig[order(-abs(sig$logFC)), , drop = FALSE], 8)),
      "."
    ),
    sep = "\n"
  )
}

# Heatmap: which genes are shown and the sample groups they span.
de_summarise_heatmap <- function(dataset, genes) {
  m <- de_expression_matrix(dataset)
  present <- intersect(genes, rownames(m))
  meta <- dataset$metadata
  grp <- if ("group" %in% names(meta)) table(meta$group) else NULL
  paste(
    c(
      sprintf(
        "Heatmap shows %d gene(s): %s.",
        length(present),
        if (length(present)) {
          paste(utils::head(present, 40), collapse = ", ")
        } else {
          "(none)"
        }
      ),
      if (!is.null(grp)) {
        paste0(
          "Across sample groups: ",
          paste(
            sprintf("%s (n=%d)", names(grp), as.integer(grp)),
            collapse = ", "
          ),
          "."
        )
      }
    ),
    collapse = "\n"
  )
}

# Enrichment: top Hallmark pathways over-represented in the significant genes.
de_summarise_enrichment <- function(
  de_results,
  gene_sets,
  lfc = 1,
  p = 0.05,
  p_col = "padj",
  n = 10
) {
  status <- de_status(de_results$logFC, de_results[[p_col]], lfc, p)
  hits <- de_results$gene[status != "NS"]
  res <- enrichment_ora(
    hits = hits,
    gene_sets = gene_sets,
    universe = de_results$gene
  )
  if (nrow(res) == 0) {
    return(
      "No enriched Hallmark sets (needs gene symbols overlapping Hallmark)."
    )
  }
  top <- utils::head(res, n)
  rows <- paste(
    sprintf(
      "%s (%d hits, p_adj %s)",
      top$gene_set,
      top$n_hits,
      .de_fmt_p(top$p_adj)
    ),
    collapse = "; "
  )
  sprintf(
    "Top %d enriched Hallmark pathways among %d significant genes: %s.",
    nrow(top),
    length(hits),
    rows
  )
}
