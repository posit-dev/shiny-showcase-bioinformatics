# Normalized evidence model + specialist gatherers.
#
# Every specialist returns rows in ONE schema so the citation gate, scoring, and
# report all operate uniformly:
#   tibble(domain, title, detail, score, source_id, source_url)
# domain in {"pathway-disease", "literature", "variant-effect"}. `score` is the
# Open Targets association score for pathway-disease rows, NA otherwise. Every
# row carries a non-empty source_id (enforced by the citation gate).

evidence_tibble <- function(
  domain = character(),
  title = character(),
  detail = character(),
  score = numeric(),
  source_id = character(),
  source_url = character()
) {
  tibble::tibble(
    domain = as.character(domain),
    title = as.character(title),
    detail = as.character(detail),
    score = as.numeric(score),
    source_id = as.character(source_id),
    source_url = as.character(source_url)
  )
}

empty_evidence <- function() evidence_tibble()

# --- Long evidence for the gene-list pipeline -------------------------------
# The same evidence rows, tagged with the gene and the signal they support, so
# the UI can drill from a ranked gene down to the records behind each signal.

evidence_long_rows <- function(
  gene_id,
  signal_key,
  domain,
  title,
  detail,
  score,
  source_id,
  source_url
) {
  base <- evidence_tibble(domain, title, detail, score, source_id, source_url)
  if (nrow(base) == 0) {
    return(empty_evidence_long())
  }
  tibble::tibble(
    gene_id = gene_id,
    signal_key = signal_key,
    domain = base$domain,
    title = base$title,
    detail = base$detail,
    score = base$score,
    source_id = base$source_id,
    source_url = base$source_url
  )
}

empty_evidence_long <- function() {
  tibble::tibble(
    gene_id = character(),
    signal_key = character(),
    domain = character(),
    title = character(),
    detail = character(),
    score = numeric(),
    source_id = character(),
    source_url = character()
  )
}

# --- pathway-disease specialist (Open Targets) ------------------------------

gather_pathway_disease <- function(gene, size = 20) {
  a <- gene_disease_assoc(gene, size = size)
  if (!isTRUE(a$ok)) {
    return(list(
      ok = FALSE,
      error = a$error,
      symbol = gene,
      ensembl_id = NULL,
      evidence = empty_evidence()
    ))
  }
  ev <- a$evidence
  rows <- evidence_tibble(
    domain = "pathway-disease",
    title = ev$disease,
    detail = sprintf("Open Targets association score %.2f", ev$score),
    score = ev$score,
    source_id = ev$source_id,
    source_url = ev$source_url
  )
  list(ok = TRUE, symbol = a$symbol, ensembl_id = a$ensembl_id, evidence = rows)
}

# --- literature specialist (Europe PMC) -------------------------------------

gather_literature <- function(gene, context, limit = 8) {
  q <- literature_query(gene, context)
  r <- europepmc_search(q, limit = limit)
  if (!isTRUE(r$ok)) {
    return(list(
      ok = FALSE,
      error = r$error,
      evidence = empty_evidence(),
      query = q
    ))
  }
  d <- r$data
  year_suffix <- ifelse(
    is.na(d$year) | !nzchar(d$year),
    "",
    paste0(" (", d$year, ")")
  )
  rows <- evidence_tibble(
    domain = "literature",
    title = d$title,
    detail = paste0(ifelse(is.na(d$journal), "", d$journal), year_suffix),
    score = NA_real_,
    source_id = d$source_id,
    source_url = d$source_url
  )
  list(ok = TRUE, evidence = rows, query = q)
}

# Build a Europe PMC query: gene AND (context search terms).
literature_query <- function(gene, context) {
  terms <- context$search_terms %||% context$label %||% context$id
  terms <- unlist(terms, use.names = FALSE)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(terms) == 0) {
    return(sprintf('"%s"', gene))
  }
  quoted <- sprintf('"%s"', terms)
  sprintf('"%s" AND (%s)', gene, paste(quoted, collapse = " OR "))
}

# --- variant-effect specialist (VEP + gnomAD + ClinVar) ---------------------

gather_variant_effect <- function(variant) {
  if (is_blank(variant)) {
    return(list(
      ok = FALSE,
      error = "No variant provided.",
      evidence = empty_evidence()
    ))
  }
  parts <- list(
    vep_evidence(variant, vep_consequence(variant)),
    gnomad_evidence(gnomad_frequency(variant)),
    clinvar_evidence(clinvar_lookup(variant))
  )
  parts <- parts[!vapply(parts, is.null, logical(1))]
  ev <- if (length(parts) == 0) empty_evidence() else dplyr::bind_rows(parts)
  list(ok = nrow(ev) > 0, evidence = ev)
}

vep_evidence <- function(variant, vep) {
  if (!isTRUE(vep$ok) || is_blank(vep$most_severe)) {
    return(NULL)
  }
  impact <- if (!is.null(vep$data) && nrow(vep$data) > 0) {
    vep$data$impact[1]
  } else {
    NA_character_
  }
  evidence_tibble(
    domain = "variant-effect",
    title = paste0("Predicted consequence: ", vep$most_severe),
    detail = paste0(
      "Ensembl VEP",
      if (!is.na(vep$assembly)) paste0(" (", vep$assembly, ")") else "",
      if (!is.na(impact)) paste0("; impact ", impact) else ""
    ),
    score = NA_real_,
    source_id = paste0("Ensembl:VEP:", variant),
    source_url = paste0(
      "https://www.ensembl.org/Homo_sapiens/Variation/Explore?v=",
      variant
    )
  )
}

gnomad_evidence <- function(gf) {
  if (!isTRUE(gf$ok)) {
    return(NULL)
  }
  part <- gf$exome %||% gf$genome
  if (is.null(part) || is.na(part$af)) {
    return(NULL)
  }
  evidence_tibble(
    domain = "variant-effect",
    title = sprintf("gnomAD allele frequency %.2g", part$af),
    detail = sprintf(
      "Population frequency (%s; AC %g / AN %g)",
      gf$dataset,
      part$ac,
      part$an
    ),
    score = NA_real_,
    source_id = paste0("gnomAD:", gf$variant_id),
    source_url = paste0(
      "https://gnomad.broadinstitute.org/variant/",
      gf$variant_id,
      "?dataset=",
      gf$dataset
    )
  )
}

clinvar_evidence <- function(cv) {
  if (!isTRUE(cv$ok) || is_blank(cv$significance)) {
    return(NULL)
  }
  evidence_tibble(
    domain = "variant-effect",
    title = paste0("ClinVar: ", cv$significance),
    detail = paste0(
      if (!is.na(cv$conditions)) paste0(cv$conditions, "; ") else "",
      "review: ",
      cv$review_status %||% "n/a"
    ),
    score = NA_real_,
    source_id = paste0("ClinVar:", cv$accession),
    source_url = paste0(
      "https://www.ncbi.nlm.nih.gov/clinvar/variation/",
      cv$uid,
      "/"
    )
  )
}
