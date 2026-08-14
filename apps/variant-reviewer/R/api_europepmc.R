# Europe PMC client: recent literature for a gene, optionally refined by a
# variant. Docs: https://europepmc.org/RestfulWebService
#
# The search is by gene symbol; when a variant rsID is loaded it is ANDed in to
# focus the results. Results come back newest-first from the "lite" result type,
# which carries just the citation fields the card needs.

EUROPEPMC_BASE <- "https://www.ebi.ac.uk/europepmc/webservices/rest"

# Literature for a gene symbol, optionally refined by a `refine` term (e.g. an
# rsID). Returns:
#   list(ok = TRUE, count, query,
#        data = data.frame(title, authors, journal, year, id, source, doi,
#                          cited_by))
#   list(ok = FALSE, error = "...")
europepmc_search <- function(gene, refine = NULL, size = 15) {
  if (is_blank(gene)) {
    return(list(ok = FALSE, error = "No gene to search literature for."))
  }
  query <- europepmc_query(gene, refine)
  res <- vr_api_get(
    EUROPEPMC_BASE,
    path = "search",
    query = list(
      query = query,
      format = "json",
      resultType = "lite",
      pageSize = size
    ),
    source = "Europe PMC"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  results <- pluck_at(res$data, "resultList", "result")
  if (is.null(results) || length(results) == 0) {
    return(list(ok = FALSE, error = "No publications found."))
  }
  list(
    ok = TRUE,
    count = pluck_at(res$data, "hitCount", default = NA),
    query = query,
    data = europepmc_parse_results(results)
  )
}

# Build the Europe PMC query: the gene symbol, ANDed with a refinement term
# (e.g. an rsID) when one is supplied. Both are quoted so multi-token terms
# match as phrases. `sort_date:y` is appended so results come back newest first;
# without it Europe PMC ranks by relevance, which is not the "recent
# literature" the card promises.
europepmc_query <- function(gene, refine = NULL) {
  g <- trimws(as.character(gene))
  base <- if (is_blank(refine)) {
    paste0("\"", g, "\"")
  } else {
    paste0("\"", g, "\" AND \"", trimws(as.character(refine)), "\"")
  }
  paste(base, "sort_date:y")
}

# Europe PMC titles carry inline markup (e.g. "<i>BRAF</i>") HTML-escaped
# ("&lt;i&gt;BRAF&lt;/i&gt;"). The title cell renders with html = TRUE so the
# tags display as intended (e.g. an italicized gene symbol) instead of showing
# up as literal "<i>...</i>" text once the browser decodes the entities.
europepmc_decode_title <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;|&apos;", "'", x)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Pure parser: search results -> a citation data.frame.
europepmc_parse_results <- function(results) {
  field <- function(name) {
    vapply(
      results,
      function(r) as.character(pluck_at(r, name, default = NA_character_)),
      character(1)
    )
  }
  data.frame(
    title = europepmc_decode_title(field("title")),
    authors = field("authorString"),
    journal = field("journalTitle"),
    year = field("pubYear"),
    id = field("id"),
    source = field("source"),
    doi = field("doi"),
    cited_by = vapply(
      results,
      function(r) {
        suppressWarnings(as.integer(pluck_at(r, "citedByCount", default = NA)))
      },
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
}
