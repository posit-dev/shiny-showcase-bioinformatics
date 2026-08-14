# Canonical gene table -------------------------------------------------------
#
# Every data source adapter returns a tibble with exactly these columns, in this
# order. Keeping a single, type-stable schema lets us bind results from any mix
# of sources, dedupe on `gene_symbol`, and rank without special-casing.

GENE_TABLE_COLS <- c(
  gene_symbol = "character",
  ensembl_id = "character",
  entrez_id = "character",
  source_id = "character",
  source_score_raw = "numeric",
  source_score_norm = "numeric",
  evidence_type = "character",
  n_evidence = "integer",
  url = "character",
  retrieved_at = "character"
)

# A zero-row tibble of the canonical shape. Sources that error or find nothing
# return this, so downstream binds and joins stay type-stable.
empty_gene_table <- function() {
  tibble::tibble(
    gene_symbol = character(),
    ensembl_id = character(),
    entrez_id = character(),
    source_id = character(),
    source_score_raw = numeric(),
    source_score_norm = numeric(),
    evidence_type = character(),
    n_evidence = integer(),
    url = character(),
    retrieved_at = character()
  )
}

# Coerce an adapter's partial data frame into the canonical schema: fill missing
# columns with typed NA, drop extras, order columns, stamp the source id and
# retrieval time, normalize the gene symbol, and drop rows without a symbol.
as_gene_table <- function(df, source_id, retrieved_at = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(empty_gene_table())
  }
  df <- tibble::as_tibble(df)
  retrieved_at <- retrieved_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  pull <- function(name, type) {
    if (name %in% names(df)) {
      methods::as(df[[name]], type)
    } else {
      methods::as(rep(NA, nrow(df)), type)
    }
  }

  out <- tibble::tibble(
    gene_symbol = toupper(trimws(as.character(pull(
      "gene_symbol",
      "character"
    )))),
    ensembl_id = pull("ensembl_id", "character"),
    entrez_id = pull("entrez_id", "character"),
    source_id = as.character(source_id),
    source_score_raw = pull("source_score_raw", "numeric"),
    source_score_norm = pull("source_score_norm", "numeric"),
    evidence_type = pull("evidence_type", "character"),
    n_evidence = pull("n_evidence", "integer"),
    url = pull("url", "character"),
    retrieved_at = as.character(retrieved_at)
  )

  out <- out[!is.na(out$gene_symbol) & nzchar(out$gene_symbol), , drop = FALSE]
  out
}

# TRUE when `tbl` has exactly the canonical columns (order-independent).
validate_gene_table <- function(tbl) {
  is.data.frame(tbl) && setequal(names(tbl), names(GENE_TABLE_COLS))
}
