# cBioPortal: cross-cancer somatic mutation frequency for a gene. We query ONE
# large, stable, public pan-cancer cohort (MSK-IMPACT, ~10,945 tumors) so the signal
# is a single grounded number - the fraction of profiled tumors carrying >= 1
# mutation in the gene - rather than an unbounded multi-study crawl. This is
# RESEARCH evidence (recurrent somatic mutation), never a clinical call.
#
# One POST to the study-view mutation-data-counts endpoint, keyed by HUGO symbol
# (no id lookup needed). No {ellmer} here - a plain, testable client.

CBIOPORTAL_BASE <- "https://www.cbioportal.org/api"
CBIOPORTAL_STUDY <- "msk_impact_2017"
CBIOPORTAL_STUDY_URL <-
  "https://www.cbioportal.org/study/summary?id=msk_impact_2017"

# Mutated-sample frequency for `symbol` in the fixed cohort. Returns a normalized
# list (ok / mutated / total / frequency / source_id / source_url) or ok = FALSE.
cbioportal_gene_frequency <- function(symbol, study = CBIOPORTAL_STUDY) {
  symbol <- toupper(trimws(as.character(symbol %||% "")))
  if (!nzchar(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol."))
  }
  body <- list(
    studyViewFilter = list(studyIds = list(study)),
    genomicDataFilters = list(list(
      hugoGeneSymbol = symbol,
      profileType = "mutations"
    ))
  )
  res <- http_post_json(
    paste0(CBIOPORTAL_BASE, "/mutation-data-counts/fetch"),
    body,
    source = "cBioPortal"
  )
  if (!isTRUE(res$ok)) {
    return(list(
      ok = FALSE,
      error = res$error %||% "cBioPortal request failed."
    ))
  }
  cbioportal_frequency_parse(res$data, symbol, study)
}

# Distill the study-view counts payload to a mutated fraction. Pure + offline. The
# payload is a list with one entry per requested gene, each carrying a `counts`
# list of {value: MUTATED|NOT_MUTATED, uniqueCount}. Frequency = mutated distinct
# samples / all profiled samples.
cbioportal_frequency_parse <- function(data, symbol, study = CBIOPORTAL_STUDY) {
  symbol <- toupper(trimws(as.character(symbol %||% "")))
  entry <- NULL
  for (e in data) {
    hit <- toupper(as.character(pluck_at(e, "hugoGeneSymbol") %||% ""))
    if (identical(hit, symbol)) {
      entry <- e
      break
    }
  }
  entry <- entry %||% (if (length(data) > 0) data[[1]] else NULL)
  counts <- pluck_at(entry, "counts")
  if (is.null(counts) || length(counts) == 0) {
    return(list(ok = FALSE, error = "No mutation counts for the gene."))
  }
  mutated <- 0
  total <- 0
  for (cnt in counts) {
    n <- as.numeric(
      pluck_at(cnt, "uniqueCount") %||% pluck_at(cnt, "count", default = 0)
    )
    total <- total + n
    if (identical(pluck_at(cnt, "value"), "MUTATED")) {
      mutated <- n
    }
  }
  if (total <= 0) {
    return(list(ok = FALSE, error = "Empty cohort."))
  }
  list(
    ok = TRUE,
    symbol = symbol,
    mutated = mutated,
    total = total,
    frequency = mutated / total,
    study = study,
    source_id = paste0("cbioportal:", study),
    source_url = CBIOPORTAL_STUDY_URL
  )
}
