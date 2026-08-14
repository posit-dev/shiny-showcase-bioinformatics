# MyGene.info client: resolve a gene symbol (or Ensembl/Entrez id) to a
# normalized set of identifiers and summary used across the app.
# Docs: https://docs.mygene.info/en/latest/

MYGENE_BASE <- "https://mygene.info/v3"

# Strip anything that isn't a plausible gene-identifier character.
mygene_clean_symbol <- function(symbol) {
  if (is_blank(symbol)) {
    return(NULL)
  }
  cleaned <- gsub("[^A-Za-z0-9._-]", "", trimws(as.character(symbol)))
  if (cleaned == "") NULL else cleaned
}

# Build the MyGene query term, detecting Ensembl gene and Entrez ids so they
# resolve precisely instead of as free-text symbol matches.
mygene_query_term <- function(symbol) {
  if (grepl("^ENSG\\d+", symbol, ignore.case = TRUE)) {
    paste0("ensembl.gene:", symbol)
  } else if (grepl("^\\d+$", symbol)) {
    paste0("entrezgene:", symbol)
  } else {
    symbol
  }
}

# Returns:
#   list(ok = TRUE, symbol, name, summary, entrez, ensembl_gene, uniprot,
#        hgnc, type_of_gene)
#   list(ok = FALSE, error = "...")
mygene_resolve <- function(symbol, species = "human") {
  cleaned <- mygene_clean_symbol(symbol)
  if (is.null(cleaned)) {
    return(list(ok = FALSE, error = "Please enter a valid gene identifier."))
  }

  res <- vr_api_get(
    MYGENE_BASE,
    path = "query",
    query = list(
      q = mygene_query_term(cleaned),
      species = species,
      size = 1,
      fields = "name,symbol,entrezgene,ensembl.gene,uniprot,type_of_gene,summary,HGNC"
    ),
    source = "MyGene"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }

  hits <- res$data$hits
  if (is.null(hits) || length(hits) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No gene found for '", cleaned, "'.")
    ))
  }
  mygene_parse_hit(hits[[1]], fallback_symbol = cleaned)
}

# Pure parser: turn a single MyGene hit (parsed JSON list) into the normalized
# result. Separated from the fetch so it can be tested without the network.
mygene_parse_hit <- function(hit, fallback_symbol = NA_character_) {
  list(
    ok = TRUE,
    symbol = pluck_at(hit, "symbol", default = fallback_symbol),
    name = pluck_at(hit, "name", default = NA_character_),
    summary = pluck_at(hit, "summary", default = NA_character_),
    entrez = as.character(pluck_at(hit, "entrezgene", default = NA)),
    ensembl_gene = mygene_first(pluck_at(hit, "ensembl", "gene")),
    uniprot = mygene_first(pluck_at(hit, "uniprot", "Swiss-Prot")),
    hgnc = mygene_first(pluck_at(hit, "HGNC")),
    type_of_gene = pluck_at(hit, "type_of_gene", default = NA_character_)
  )
}

# MyGene fields can be a scalar or a list (multiple mappings); take the first.
mygene_first <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  as.character(x[[1]])
}
