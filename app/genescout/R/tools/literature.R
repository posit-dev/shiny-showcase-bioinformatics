# Literature client - Europe PMC.
# Endpoint: https://www.ebi.ac.uk/europepmc/webservices/rest  (see docs/data_sources.md)
# Retrieves citations for a gene in a disease context; every returned item
# carries a PMID (or Europe PMC source:id) so the citation gate can ground it.
# Pure client through http_get_json(); no {ellmer} imports.
#
# PubTator (pre-annotated mentions) is a planned addition - see docs/data_sources.md.

EUROPEPMC_BASE <- "https://www.ebi.ac.uk/europepmc/webservices/rest"

# Search Europe PMC. Returns:
#   list(ok = TRUE, count, data = tibble(title, authors, year, journal, pmid,
#        source, source_id, source_url))
#   list(ok = FALSE, error = "...")
europepmc_search <- function(query, limit = 10) {
  if (is_blank(query)) {
    return(list(ok = FALSE, error = "Empty literature query."))
  }
  res <- http_get_json(
    EUROPEPMC_BASE,
    path = "search",
    query = list(
      query = query,
      format = "json",
      pageSize = limit,
      resultType = "lite"
    ),
    source = "Europe PMC"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  results <- pluck_at(res$data, "resultList", "result")
  if (is.null(results) || length(results) == 0) {
    return(list(ok = FALSE, error = "No literature found."))
  }
  list(
    ok = TRUE,
    count = pluck_at(res$data, "hitCount", default = NA),
    data = europepmc_parse_results(results)
  )
}

# Total Europe PMC hit count for a query - a gene-level literature signal. Uses
# pageSize = 1 (we only need hitCount, not the rows). A genuine 0 hits is
# ok = TRUE, count = 0 - distinct from a source failure - so a gene with no
# literature is not mistaken for an outage (unlike europepmc_search(), which
# reports ok = FALSE on an empty result list). Returns:
#   list(ok = TRUE, count, query, source_id, source_url) | list(ok = FALSE, error)
europepmc_count <- function(query) {
  if (is_blank(query)) {
    return(list(ok = FALSE, error = "Empty literature query."))
  }
  res <- http_get_json(
    EUROPEPMC_BASE,
    path = "search",
    query = list(
      query = query,
      format = "json",
      pageSize = 1,
      resultType = "idlist"
    ),
    source = "Europe PMC"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  count <- europepmc_count_parse(res$data)
  if (is.na(count)) {
    return(list(ok = FALSE, error = "Europe PMC returned no count."))
  }
  list(
    ok = TRUE,
    count = count,
    query = query,
    source_id = paste0("EuropePMC:query:", query),
    source_url = paste0(
      "https://europepmc.org/search?query=",
      utils::URLencode(query, reserved = TRUE)
    )
  )
}

# Pure parser: a Europe PMC search body -> the integer hitCount, NA if absent.
# 0 parses to 0L (a real zero). Testable offline against a fixture.
europepmc_count_parse <- function(data) {
  raw <- pluck_at(data, "hitCount", default = NA)
  if (is_blank(raw)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(raw))
}

# Pure parser: Europe PMC result rows -> a grounded citation tibble. Every row
# has a source_id (PMID:<n> or <source>:<id>) and a europepmc.org link.
# Separated from the fetch so it is testable offline against a JSON fixture.
europepmc_parse_results <- function(results) {
  field <- function(key) {
    vapply(
      results,
      function(r) as.character(pluck_at(r, key, default = NA_character_)),
      character(1)
    )
  }
  pmid <- field("pmid")
  source <- field("source")
  id <- field("id")
  tibble::tibble(
    title = field("title"),
    authors = field("authorString"),
    year = field("pubYear"),
    journal = field("journalTitle"),
    pmid = pmid,
    source = source,
    source_id = ifelse(
      !is.na(pmid) & nzchar(pmid),
      paste0("PMID:", pmid),
      paste0(source, ":", id)
    ),
    source_url = paste0("https://europepmc.org/article/", source, "/", id)
  )
}
