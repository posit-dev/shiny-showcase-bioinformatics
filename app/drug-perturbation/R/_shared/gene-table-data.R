# Prepare a DE results table for display: classify, optionally filter to hits,
# and order by significance.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns a tidy data
# frame the classic Shiny module hands to DT, and that a future shinyreact
# version can render client-side. This is the gene-table contract.
#
# Classification is delegated to de_status() (de-data.R), the single dual-cutoff
# rule the volcano shares, so a gene's status matches across every view.

# de_results: a data frame with at least `gene`, `logFC`, and the chosen p column.
# lfc_threshold, p_threshold: dual cutoff for calling a gene Up/Down.
# p_col: which p column drives significance and ordering ("padj" or "pvalue").
# sig_only: keep only significant (Up/Down) genes when TRUE.
# Returns the input columns plus a `status` factor (Down/NS/Up), ordered by the
# chosen p (most significant first; NA p last).
de_results_table <- function(
  de_results,
  lfc_threshold = 1,
  p_threshold = 0.05,
  p_col = c("padj", "pvalue"),
  sig_only = FALSE
) {
  p_col <- match.arg(p_col)
  required <- c("gene", "logFC", p_col)
  missing <- setdiff(required, names(de_results))
  if (length(missing) > 0) {
    stop("de_results is missing required column(s): ", toString(missing))
  }

  df <- de_results
  df$gene <- as.character(df$gene)

  # Shared classification (de-data.R) so table and volcano agree per gene.
  df$status <- de_status(df$logFC, df[[p_col]], lfc_threshold, p_threshold)

  if (isTRUE(sig_only)) {
    df <- df[df$status != "NS", , drop = FALSE]
  }

  # Most significant first; genes with NA p sort to the bottom.
  df[order(df[[p_col]], na.last = TRUE), , drop = FALSE]
}

# Counts per status, as a named integer vector (Down, NS, Up) -- handy for
# summaries / value boxes without recomputing the classification.
de_results_status_counts <- function(tbl) {
  table(factor(tbl$status, levels = c("Down", "NS", "Up")))
}
