# Variant-effect clients - Ensembl VEP, gnomAD, ClinVar.
# Endpoints (see docs/data_sources.md):
#   Ensembl VEP  https://rest.ensembl.org
#   gnomAD       https://gnomad.broadinstitute.org/api  (GraphQL)
#   ClinVar      https://eutils.ncbi.nlm.nih.gov  (E-utilities)
# All used as research evidence, never as a clinical call. Lookups are by rsID
# to avoid assembly/coordinate mismatches. Pure clients through the shared HTTP
# wrapper; no {ellmer} imports. Adapted from the sibling variant-reviewer app.

ENSEMBL_BASE <- "https://rest.ensembl.org"
GNOMAD_URL <- "https://gnomad.broadinstitute.org/api"
GNOMAD_DATASET <- "gnomad_r4"
EUTILS_BASE <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# --- Ensembl VEP ------------------------------------------------------------

# Predicted functional consequence of a variant (rsID).
# Returns list(ok, most_severe, assembly, data = df|NULL) or list(ok = FALSE, error).
vep_consequence <- function(rsid) {
  if (is_blank(rsid)) {
    return(list(ok = FALSE, error = "No rsID available for VEP lookup."))
  }
  res <- http_get_json(
    ENSEMBL_BASE,
    path = paste0("vep/human/id/", rsid),
    query = list(`content-type` = "application/json"),
    source = "Ensembl VEP"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  records <- res$data
  if (is.null(records) || length(records) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("Ensembl VEP has no record for ", rsid, ".")
    ))
  }
  ensembl_parse_vep(records[[1]])
}

# Pure parser: a VEP record -> normalized result.
ensembl_parse_vep <- function(record) {
  list(
    ok = TRUE,
    most_severe = pluck_at(
      record,
      "most_severe_consequence",
      default = NA_character_
    ),
    assembly = pluck_at(record, "assembly_name", default = NA_character_),
    data = ensembl_consequences_df(pluck_at(record, "transcript_consequences"))
  )
}

# Protein-coding transcript consequences as a data.frame, or NULL when none.
ensembl_consequences_df <- function(consequences) {
  if (is.null(consequences) || length(consequences) == 0) {
    return(NULL)
  }
  is_coding <- vapply(
    consequences,
    function(x) identical(pluck_at(x, "biotype"), "protein_coding"),
    logical(1)
  )
  consequences <- consequences[is_coding]
  if (length(consequences) == 0) {
    return(NULL)
  }
  chr_field <- function(key) {
    vapply(
      consequences,
      function(x) as.character(pluck_at(x, key, default = NA_character_)),
      character(1)
    )
  }
  data.frame(
    gene = chr_field("gene_symbol"),
    transcript = chr_field("transcript_id"),
    consequence = vapply(
      consequences,
      function(x) {
        terms <- pluck_at(x, "consequence_terms")
        if (is.null(terms)) {
          NA_character_
        } else {
          paste(unlist(terms), collapse = ", ")
        }
      },
      character(1)
    ),
    impact = chr_field("impact"),
    sift = chr_field("sift_prediction"),
    polyphen = chr_field("polyphen_prediction"),
    stringsAsFactors = FALSE
  )
}

# --- gnomAD -----------------------------------------------------------------

# Population allele frequency for a variant (rsID) - a rarity signal.
# Returns list(ok, variant_id, dataset, exome, genome) or list(ok = FALSE, error).
gnomad_frequency <- function(rsid, dataset = GNOMAD_DATASET) {
  if (is_blank(rsid)) {
    return(list(ok = FALSE, error = "No rsID available for gnomAD lookup."))
  }
  query <- sprintf(
    paste(
      "query($rsid: String!) {",
      "  variant(rsid: $rsid, dataset: %s) {",
      "    variant_id",
      "    exome { af ac an }",
      "    genome { af ac an }",
      "  }",
      "}",
      sep = "\n"
    ),
    dataset
  )
  res <- http_post_json(
    GNOMAD_URL,
    body = list(query = query, variables = list(rsid = rsid)),
    source = "gnomAD"
  )
  err <- graphql_error(res, "gnomAD")
  if (!is.null(err)) {
    return(err)
  }
  variant <- pluck_at(res$data, "data", "variant")
  if (is.null(variant)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD has no record for ", rsid, ".")
    ))
  }
  list(
    ok = TRUE,
    variant_id = pluck_at(variant, "variant_id", default = NA_character_),
    dataset = dataset,
    exome = gnomad_freq_part(pluck_at(variant, "exome")),
    genome = gnomad_freq_part(pluck_at(variant, "genome"))
  )
}

# Normalize one frequency block (exome or genome), or NULL when absent.
gnomad_freq_part <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  list(
    af = as.numeric(pluck_at(x, "af", default = NA)),
    ac = as.numeric(pluck_at(x, "ac", default = NA)),
    an = as.numeric(pluck_at(x, "an", default = NA))
  )
}

# --- ClinVar ----------------------------------------------------------------

# ClinVar classification for a variant (rsID works best). Research evidence, not
# a clinical call. Returns list(ok, uid, accession, title, significance,
# review_status, last_evaluated, conditions) or list(ok = FALSE, error).
clinvar_lookup <- function(term) {
  if (is_blank(term)) {
    return(list(
      ok = FALSE,
      error = "No variant identifier for ClinVar lookup."
    ))
  }
  search <- http_get_json(
    EUTILS_BASE,
    path = "esearch.fcgi",
    query = list(db = "clinvar", term = term, retmode = "json"),
    source = "ClinVar"
  )
  if (!search$ok) {
    return(list(ok = FALSE, error = search$error))
  }
  ids <- pluck_at(search$data, "esearchresult", "idlist")
  if (is.null(ids) || length(ids) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No ClinVar record for ", term, ".")
    ))
  }
  uid <- as.character(ids[[1]])

  summary <- http_get_json(
    EUTILS_BASE,
    path = "esummary.fcgi",
    query = list(db = "clinvar", id = uid, retmode = "json"),
    source = "ClinVar"
  )
  if (!summary$ok) {
    return(list(ok = FALSE, error = summary$error))
  }
  record <- pluck_at(summary$data, "result", uid)
  if (is.null(record)) {
    return(list(ok = FALSE, error = "ClinVar summary was unavailable."))
  }
  clinvar_parse_record(record, uid)
}

# Pure parser: an esummary ClinVar record -> normalized classification list.
clinvar_parse_record <- function(record, uid = NA_character_) {
  germline <- pluck_at(record, "germline_classification")
  list(
    ok = TRUE,
    uid = uid,
    accession = pluck_at(record, "accession", default = NA_character_),
    title = pluck_at(record, "title", default = NA_character_),
    significance = pluck_at(germline, "description", default = NA_character_),
    review_status = pluck_at(
      germline,
      "review_status",
      default = NA_character_
    ),
    last_evaluated = pluck_at(
      germline,
      "last_evaluated",
      default = NA_character_
    ),
    conditions = clinvar_conditions(germline)
  )
}

# Collapse the trait set into a readable, comma-separated condition string.
clinvar_conditions <- function(germline) {
  traits <- pluck_at(germline, "trait_set")
  if (is.null(traits) || length(traits) == 0) {
    return(NA_character_)
  }
  names <- vapply(
    traits,
    function(t) {
      as.character(pluck_at(t, "trait_name", default = NA_character_))
    },
    character(1)
  )
  names <- unique(names[!is.na(names) & nzchar(names)])
  if (length(names) == 0) NA_character_ else paste(names, collapse = "; ")
}
