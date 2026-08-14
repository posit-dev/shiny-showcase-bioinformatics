# Source adapter: ClinVar (via NCBI E-utilities) -------------------------------
#
# Disease-driven. Finds ClinVar records with pathogenic / likely-pathogenic
# classifications for the disease, then scores genes by how many such records
# name them. No key required; an optional ENTREZ_KEY raises the rate limit.

EUTILS_BASE <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

# GET an E-utilities endpoint as JSON, adding tool/email/api_key params and a
# throttle that respects NCBI's rate limits (3/s without a key, 10/s with one).
eutils_get <- function(endpoint, query, timeout = 20) {
  biohttp::body_or_null(eutils_get_env(endpoint, query, timeout))
}

# Envelope-returning form.
#
# The API key now rides in `secret_query` rather than the visible query string.
# biohttp applies it at dispatch only, so it stays out of the response-cache key
# and is redacted from any error text. Previously it was interpolated into the
# URL, where it would land in both.
#
# Throttle preserves NCBI's documented limits and the app's previous rates:
# 9/s with a key, 2.5/s without (5 tokens per 2s).
eutils_get_env <- function(endpoint, query, timeout = 20) {
  key <- Sys.getenv("ENTREZ_KEY")
  email <- Sys.getenv("ENTREZ_EMAIL")
  query <- c(
    query,
    list(tool = "gene-list-builder", retmode = "json")
  )
  if (nzchar(email)) {
    query$email <- email
  }

  glb_get(
    EUTILS_BASE,
    path = endpoint,
    query = query,
    source = "ClinVar",
    timeout = timeout,
    throttle = if (nzchar(key)) {
      list(capacity = 9, fill_time_s = 1, realm = "ncbi")
    } else {
      list(capacity = 5, fill_time_s = 2, realm = "ncbi")
    },
    secret_query = if (nzchar(key)) list(api_key = key) else NULL
  )
}

# ClinVar variation ids for pathogenic records associated with the disease term.
clinvar_esearch <- function(term, retmax = 300, get_fn = eutils_get) {
  data <- get_fn(
    "esearch.fcgi",
    list(
      db = "clinvar",
      term = term,
      retmax = retmax
    )
  )
  ids <- data$esearchresult$idlist
  if (is.null(ids)) character() else unlist(ids, use.names = FALSE)
}

# Gene symbols named by a set of ClinVar records (one entry per record-gene).
clinvar_esummary <- function(ids, chunk_size = 200, get_fn = eutils_get) {
  if (length(ids) == 0) {
    return(character())
  }
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  symbols <- character()
  for (chunk in chunks) {
    data <- get_fn(
      "esummary.fcgi",
      list(
        db = "clinvar",
        id = paste(chunk, collapse = ",")
      )
    )
    result <- data$result
    if (is.null(result)) {
      next
    }
    uids <- unlist(result$uids %||% list(), use.names = FALSE)
    for (uid in uids) {
      genes <- result[[uid]]$genes %||% list()
      for (g in genes) {
        sym <- g$symbol %||% NA_character_
        if (!is.na(sym)) symbols <- c(symbols, sym)
      }
    }
  }
  symbols
}

fetch_clinvar <- function(
  disease,
  gene_symbols = NULL,
  retmax = 300,
  esearch_fn = clinvar_esearch,
  esummary_fn = clinvar_esummary
) {
  disease_name <- disease$name %||% ""
  if (!nzchar(disease_name)) {
    return(empty_gene_table())
  }

  term <- paste0("\"", disease_name, "\"[dis] AND clinsig_pathogenic[Prop]")
  ids <- esearch_fn(term, retmax = retmax)
  if (length(ids) == 0) {
    return(empty_gene_table())
  }

  symbols <- esummary_fn(ids)
  if (length(symbols) == 0) {
    return(empty_gene_table())
  }

  counts <- as.data.frame(
    table(gene_symbol = symbols),
    stringsAsFactors = FALSE
  )
  names(counts)[2] <- "n"
  df <- data.frame(
    gene_symbol = counts$gene_symbol,
    source_score_raw = counts$n,
    n_evidence = as.integer(counts$n),
    evidence_type = "clinvar_pathogenic",
    url = paste0(
      "https://www.ncbi.nlm.nih.gov/clinvar/?term=",
      utils::URLencode(counts$gene_symbol, reserved = TRUE),
      "%5Bgene%5D"
    ),
    stringsAsFactors = FALSE
  )

  as_gene_table(df, "clinvar")
}
