# ClinGen gene-disease validity: the strength of the expert-curated evidence that
# a gene causes a disease (Definitive > Strong > Moderate > Limited). ClinGen has
# no per-gene JSON API, so we fetch the public CC0 bulk gene-validity CSV once
# (cached by the HTTP layer) and filter it per gene. RESEARCH evidence (curated
# gene-disease validity), never a clinical/diagnostic call about a patient.
#
# No {ellmer} here - a plain, testable client. The CSV parse and the context
# relevance are pure functions, unit-tested against a recorded fixture.

CLINGEN_GV_URL <- "https://search.clinicalgenome.org/kb/gene-validity/download"

# Classification -> ordinal strength. Only the "established" tiers (Limited+)
# carry a positive signal; Disputed / Refuted / No Known Disease Relationship /
# Animal Model Only map to 0 (no positive gene-disease evidence).
CLINGEN_STRENGTH <- c(
  "DEFINITIVE" = 4L,
  "STRONG" = 3L,
  "MODERATE" = 2L,
  "LIMITED" = 1L
)

# Ordinal strength for a classification label (vectorized; unknown/none -> 0).
clingen_strength <- function(classification) {
  key <- toupper(trimws(as.character(classification %||% "")))
  out <- unname(CLINGEN_STRENGTH[key])
  ifelse(is.na(out), 0L, out)
}

clingen_empty <- function() {
  tibble::tibble(
    gene = character(),
    hgnc = character(),
    disease = character(),
    mondo = character(),
    moi = character(),
    classification = character(),
    report_url = character(),
    date = character()
  )
}

# Parse the ClinGen bulk CSV to a tidy tibble. The file has a 3-line banner, a
# row of '++++' decoration, the real header, another '++++' row, then the data -
# so we start at the header and drop the decoration rows. Pure + offline.
clingen_read_csv <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(clingen_empty())
  }
  lines <- strsplit(text, "\r?\n")[[1]]
  hdr <- which(grepl('"GENE SYMBOL"', lines))
  if (length(hdr) == 0) {
    return(clingen_empty())
  }
  body <- lines[hdr[1]:length(lines)]
  body <- body[!grepl('^"\\++"', body) & nzchar(trimws(body))]
  if (length(body) <= 1) {
    return(clingen_empty())
  }
  df <- tryCatch(
    utils::read.csv(
      text = paste(body, collapse = "\n"),
      check.names = FALSE,
      colClasses = "character"
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) {
    return(clingen_empty())
  }
  col <- function(name) {
    if (name %in% names(df)) {
      as.character(df[[name]])
    } else {
      rep(NA_character_, nrow(df))
    }
  }
  tibble::tibble(
    gene = col("GENE SYMBOL"),
    hgnc = col("GENE ID (HGNC)"),
    disease = col("DISEASE LABEL"),
    mondo = col("DISEASE ID (MONDO)"),
    moi = col("MOI"),
    classification = col("CLASSIFICATION"),
    report_url = col("ONLINE REPORT"),
    date = col("CLASSIFICATION DATE")
  )
}

# The gene's ClinGen validity curations (fetches the bulk CSV, cached per URL,
# then filters). Returns list(ok, symbol, curations, n) or ok = FALSE on failure.
clingen_gene_validity <- function(symbol) {
  symbol <- toupper(trimws(as.character(symbol %||% "")))
  if (!nzchar(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol."))
  }
  res <- http_get_text(CLINGEN_GV_URL, source = "ClinGen")
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, error = res$error %||% "ClinGen request failed."))
  }
  rows <- clingen_read_csv(res$text)
  hit <- rows[toupper(rows$gene) == symbol, , drop = FALSE]
  list(ok = TRUE, symbol = symbol, curations = hit, n = nrow(hit))
}

# The gene's validity relevance for a review: the established (Limited+) curations,
# scoped to the disease context when one is given (by MONDO id, separator-
# normalized, OR a shared specific disease-name word), else all established ones.
# `strength` is the strongest matching classification. Pure so it is testable.
clingen_relevance <- function(curations, disease_ctx = NULL) {
  miss <- list(
    present = FALSE,
    matched = curations[0, , drop = FALSE],
    strength = 0L
  )
  if (nrow(curations) == 0) {
    return(miss)
  }
  strengths <- clingen_strength(curations$classification)
  established <- curations[strengths >= 1L, , drop = FALSE]
  if (nrow(established) == 0) {
    return(miss)
  }
  if (is.null(disease_ctx) || is_blank(disease_ctx$name %||% disease_ctx$id)) {
    return(list(
      present = TRUE,
      matched = established,
      strength = max(clingen_strength(established$classification))
    ))
  }
  # Normalize the MONDO separator (Open Targets underscore vs ClinGen colon), the
  # same fix as HPO; the shared token helpers scope by specific disease word.
  want_mondo <- gsub("_", ":", toupper(as.character(disease_ctx$id %||% "")))
  want_tokens <- hpo_disease_tokens(disease_ctx$name %||% "")
  keep <- vapply(
    seq_len(nrow(established)),
    function(i) {
      mondo_hit <- nzchar(want_mondo) &&
        identical(gsub("_", ":", toupper(established$mondo[i])), want_mondo)
      name_hit <- length(want_tokens) > 0 &&
        length(intersect(gtex_tokens(established$disease[i]), want_tokens)) > 0
      mondo_hit || name_hit
    },
    logical(1)
  )
  matched <- established[keep, , drop = FALSE]
  if (nrow(matched) == 0) {
    return(miss)
  }
  list(
    present = TRUE,
    matched = matched,
    strength = max(clingen_strength(matched$classification))
  )
}
