# gnomAD gene-constraint client - LOEUF (loss-of-function observed/expected upper
# bound). Endpoint: https://gnomad.broadinstitute.org/api (see docs/data_sources.md)
# A gene-level constraint SIGNAL: a low LOEUF means strong selection against
# loss-of-function, i.e. the gene is more likely essential/dosage-sensitive. Used
# as research evidence for prioritization, never as a clinical call. Pure client
# through http_post_json(); no {ellmer} imports. The variant-level gnomAD
# frequency lookup lives in R/tools/variant_effect.R.

GNOMAD_URL <- "https://gnomad.broadinstitute.org/api"
GNOMAD_WEB <- "https://gnomad.broadinstitute.org/gene"
GNOMAD_GENE_DATASET <- "gnomad_r4"

# Predicted loss-of-function (pLoF) VEP consequence terms. A COMMON variant in one
# of these means the general population tolerates losing the gene, which argues
# against it being a plausible driver - the basis of the "common in gnomAD" caveat.
# Consequence-based (not LOFTEE-filtered) to keep the client simple and the response
# small; the downstream caveat is a soft down-weight, not a gate.
GNOMAD_PLOF_CONSEQUENCES <- c(
  "transcript_ablation",
  "splice_acceptor_variant",
  "splice_donor_variant",
  "stop_gained",
  "frameshift_variant",
  "stop_lost",
  "start_lost"
)

# The gnomAD API takes one gene per query (batching is done with GraphQL aliases;
# here we query one gene at a time and rely on the shared HTTP cache). The symbol
# is inlined, so it is validated against a strict allow-list first to avoid any
# query injection.
gnomad_constraint_query <- function(symbol) {
  sprintf(
    paste0(
      "{ gene(gene_symbol: \"%s\", reference_genome: GRCh38) ",
      "{ gnomad_constraint { oe_lof_upper pli } } }"
    ),
    symbol
  )
}

# LOEUF constraint for a gene symbol. Returns:
#   list(ok = TRUE, symbol, loeuf, pli, source_id, source_url)
#   list(ok = FALSE, error = "...")
# A gene with no constraint record (e.g. very short genes) is ok = FALSE, so it
# is treated as a missing annotation, not a spurious 0.
gnomad_loeuf <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for gnomAD lookup."))
  }
  sym <- toupper(trimws(symbol))
  if (!grepl("^[A-Za-z0-9._-]+$", sym)) {
    return(list(
      ok = FALSE,
      error = paste0("Invalid gene symbol '", symbol, "'.")
    ))
  }
  res <- http_post_json(
    GNOMAD_URL,
    body = list(query = gnomad_constraint_query(sym)),
    source = "gnomAD"
  )
  err <- graphql_error(res, "gnomAD")
  if (!is.null(err)) {
    return(err)
  }
  gnomad_constraint_parse(res$data, sym)
}

# Pure parser: a gnomAD gene-constraint body -> the LOEUF signal. Separated from
# the fetch so it is testable offline against a JSON fixture.
gnomad_constraint_parse <- function(data, symbol) {
  gene <- pluck_at(data, "data", "gene")
  if (is.null(gene)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD has no record for '", symbol, "'.")
    ))
  }
  con <- pluck_at(gene, "gnomad_constraint")
  loeuf <- suppressWarnings(as.numeric(
    pluck_at(con, "oe_lof_upper", default = NA)
  ))
  if (is.na(loeuf)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD has no LOEUF for '", symbol, "'.")
    ))
  }
  pli <- suppressWarnings(as.numeric(pluck_at(con, "pli", default = NA)))
  list(
    ok = TRUE,
    symbol = symbol,
    loeuf = loeuf,
    pli = pli,
    source_id = paste0("gnomAD:gene:", symbol, ":constraint"),
    source_url = paste0(GNOMAD_WEB, "/", symbol)
  )
}

# The gene's variants query (one gene per call; the shared HTTP cache dedupes
# repeats). Only `consequence` + the two AFs are requested, to keep the response as
# small as this per-gene scan allows. Symbol is inlined, so validate it strictly.
gnomad_variants_query <- function(symbol) {
  sprintf(
    paste0(
      "{ gene(gene_symbol: \"%s\", reference_genome: GRCh38) ",
      "{ variants(dataset: %s) { consequence exome { af } genome { af } } } }"
    ),
    symbol,
    GNOMAD_GENE_DATASET
  )
}

# Max allele frequency among a gene's predicted loss-of-function variants - a
# GENE-LEVEL "how common is losing this gene" signal, distinct from the LOEUF
# constraint above (constraint != frequency) and from the variant-level rsID lookup
# in R/tools/variant_effect.R. Returns:
#   list(ok = TRUE, symbol, max_lof_af, n_lof, source_id, source_url)
#   list(ok = FALSE, error = "...")
# A gene present in gnomAD with no common pLoF variant is a REAL 0 (ok = TRUE), the
# distinction the caveat needs; only an absent gene / failed query is a miss, so the
# signal never emits a spurious 0.
gnomad_gene_max_lof_af <- function(symbol) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for gnomAD lookup."))
  }
  sym <- toupper(trimws(symbol))
  if (!grepl("^[A-Za-z0-9._-]+$", sym)) {
    return(list(
      ok = FALSE,
      error = paste0("Invalid gene symbol '", symbol, "'.")
    ))
  }
  res <- http_post_json(
    GNOMAD_URL,
    body = list(query = gnomad_variants_query(sym)),
    source = "gnomAD"
  )
  err <- graphql_error(res, "gnomAD")
  if (!is.null(err)) {
    return(err)
  }
  gnomad_lof_af_parse(res$data, sym)
}

# Pure parser: a gnomAD gene-variants body -> the max pLoF allele frequency. Filters
# to pLoF consequences first (so a common intron/missense variant never trips the
# caveat), then takes each kept variant's higher of exome/genome AF. Separated from
# the fetch so it is testable offline against a JSON fixture.
gnomad_lof_af_parse <- function(data, symbol) {
  gene <- pluck_at(data, "data", "gene")
  if (is.null(gene)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD has no record for '", symbol, "'.")
    ))
  }
  variants <- pluck_at(gene, "variants")
  if (is.null(variants)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD returned no variants for '", symbol, "'.")
    ))
  }
  afs <- vapply(
    variants,
    function(v) {
      if (!isTRUE(pluck_at(v, "consequence") %in% GNOMAD_PLOF_CONSEQUENCES)) {
        return(NA_real_)
      }
      vals <- suppressWarnings(as.numeric(c(
        pluck_at(v, "exome", "af", default = NA),
        pluck_at(v, "genome", "af", default = NA)
      )))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) NA_real_ else max(vals)
    },
    numeric(1)
  )
  afs <- afs[!is.na(afs)]
  list(
    ok = TRUE,
    symbol = symbol,
    max_lof_af = if (length(afs) == 0) 0 else max(afs),
    n_lof = length(afs),
    source_id = paste0("gnomAD:gene:", symbol, ":lof_af"),
    source_url = paste0(GNOMAD_WEB, "/", symbol)
  )
}
