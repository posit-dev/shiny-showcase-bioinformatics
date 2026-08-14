# Over-representation analysis (ORA): which gene sets are enriched among a hit
# list (e.g. the significant genes from a DE contrast), by the hypergeometric
# test. Dependency-free (base `stats`) -- the "which pathways?" follow-up to a
# gene list, using the gene sets an app already has (e.g. MSigDB Hallmark).
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md).

# hits: character vector of genes of interest.
# gene_sets: named list of gene-set character vectors (e.g. Hallmark).
# universe: background genes (e.g. all tested genes); hits/sets are restricted
#   to it so the test is well-formed.
# min_overlap: drop sets with fewer than this many hits (noise control).
# Returns data.frame(gene_set, set_size, n_hits, expected, p_value, p_adj)
# ordered most-significant first (empty data frame if nothing overlaps).
enrichment_ora <- function(hits, gene_sets, universe, min_overlap = 2L) {
  universe <- unique(as.character(universe))
  hits <- intersect(unique(as.character(hits)), universe)
  n_universe <- length(universe)
  n_hits <- length(hits)

  empty <- data.frame(
    gene_set = character(),
    set_size = integer(),
    n_hits = integer(),
    expected = numeric(),
    p_value = numeric(),
    p_adj = numeric(),
    stringsAsFactors = FALSE
  )
  if (n_hits == 0 || n_universe == 0 || length(gene_sets) == 0) {
    return(empty)
  }

  rows <- lapply(names(gene_sets), function(s) {
    set <- intersect(as.character(gene_sets[[s]]), universe)
    m <- length(set)
    if (m == 0) {
      return(NULL)
    }
    x <- length(intersect(hits, set))
    # P(X >= x) under the hypergeometric null: x-1 with lower.tail = FALSE.
    p <- stats::phyper(x - 1L, m, n_universe - m, n_hits, lower.tail = FALSE)
    data.frame(
      gene_set = s,
      set_size = m,
      n_hits = x,
      expected = round(n_hits * m / n_universe, 2),
      p_value = p,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df)) {
    return(empty)
  }
  df <- df[df$n_hits >= min_overlap, , drop = FALSE]
  if (nrow(df) == 0) {
    return(empty)
  }
  df$p_adj <- stats::p.adjust(df$p_value, "BH")
  df$p_value <- signif(df$p_value, 3)
  df$p_adj <- signif(df$p_adj, 3)
  df[order(df$p_value), , drop = FALSE]
}
