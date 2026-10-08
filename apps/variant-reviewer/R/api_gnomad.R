# gnomAD client: population allele frequencies for a variant, looked up by its
# GRCh38 gnomAD variant id when the allele is known, else by rsID. GraphQL API:
# https://gnomad.broadinstitute.org/api

GNOMAD_URL <- "https://gnomad.broadinstitute.org/api"
GNOMAD_DATASET <- "gnomad_r4"

# Allele frequencies for a variant. `variant_id` is gnomAD's id for one allele
# ("7-140753336-A-T"); when it is known it is used in place of the rsID, which
# gnomAD refuses to resolve when it covers several alleles.
# Returns:
#   list(ok = TRUE, variant_id, dataset, exome = list(af, ac, an)|NULL,
#        genome = list(af, ac, an)|NULL)
#   list(ok = FALSE, error = "...")
gnomad_frequency <- function(
  rsid,
  dataset = GNOMAD_DATASET,
  variant_id = NULL
) {
  by_id <- !is_blank(variant_id)
  if (!by_id && is_blank(rsid)) {
    return(list(ok = FALSE, error = "No rsID available for gnomAD lookup."))
  }
  label <- if (by_id) variant_id else rsid

  query <- sprintf(
    paste(
      "query($id: String!) {",
      "  variant(%s: $id, dataset: %s) {",
      "    variant_id",
      "    exome { af ac an populations { id ac an } }",
      "    genome { af ac an populations { id ac an } }",
      "  }",
      "}",
      sep = "\n"
    ),
    if (by_id) "variantId" else "rsid",
    dataset
  )

  res <- vr_api_post_json(
    GNOMAD_URL,
    body = list(query = query, variables = list(id = label)),
    source = "gnomAD"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  no_record <- list(
    ok = FALSE,
    missing = TRUE,
    error = paste0("gnomAD has no record for ", label, ".")
  )
  if (!is.null(res$data$errors)) {
    kind <- gnomad_error_kind(res$data$errors)
    if (identical(kind, "missing")) {
      return(no_record)
    }
    return(list(
      ok = FALSE,
      error = if (identical(kind, "multiple")) {
        paste0(
          "gnomAD has more than one variant for ",
          label,
          ". Pick one allele in the Variant box to see its frequency."
        )
      } else {
        "gnomAD returned a query error."
      }
    ))
  }

  variant <- pluck_at(res$data, "data", "variant")
  if (is.null(variant)) {
    return(no_record)
  }

  list(
    ok = TRUE,
    variant_id = pluck_at(variant, "variant_id", default = NA_character_),
    dataset = dataset,
    exome = gnomad_freq_part(pluck_at(variant, "exome")),
    genome = gnomad_freq_part(pluck_at(variant, "genome")),
    populations = gnomad_parse_populations(
      pluck_at(variant, "exome", "populations"),
      pluck_at(variant, "genome", "populations")
    )
  )
}

# What a gnomAD GraphQL error means: "missing" (no such variant), "multiple"
# (an rsID that covers more than one variant) or "other". gnomAD reports the
# first two as errors, not as empty data.
gnomad_error_kind <- function(errors) {
  messages <- unlist(
    lapply(errors, function(e) pluck_at(e, "message")),
    use.names = FALSE
  )
  text <- tolower(paste(messages, collapse = " "))
  if (grepl("not found", text, fixed = TRUE)) {
    return("missing")
  }
  if (grepl("multiple variants", text, fixed = TRUE)) {
    return("multiple")
  }
  "other"
}

# Allele frequencies for one picked allele, by its chrom-pos-ref-alt id.
#
# gnomAD stores an indel at its leftmost position in a repeat, and MyVariant
# can hold the same indel further right (BRCA2 c.5073del is 13-32339421-CA-C
# in gnomAD and 13-32339427-AA-A in MyVariant). So an indel gnomAD does not
# know is moved left against the reference and tried again. When the
# reference cannot be fetched, the rsID is tried and its answer kept only
# when it is the same change; failing that, the card says the absence was not
# checked, since "no record" would read as evidence the variant is rare.
# Without an id there is no safe lookup: the rsID alone can return another
# allele.
gnomad_allele_frequency <- function(rsid, vcf_id, dataset = GNOMAD_DATASET) {
  if (is_blank(vcf_id)) {
    return(vr_allele_unplaced("gnomAD"))
  }
  v <- .mv_parse_vcf_id(vcf_id)
  # Two kinds of change gnomAD does keep, but not where this lookup asks, so
  # "no record" would be false: mitochondrial variants sit in a separate
  # dataset, and a multi-base substitution is listed one base at a time.
  if (!is.null(v) && v$chrom %in% c("MT", "M")) {
    return(list(
      ok = FALSE,
      error = paste(
        "gnomAD keeps mitochondrial variants in a separate dataset,",
        "which this card does not read yet."
      )
    ))
  }
  if (!is.null(v) && nchar(v$ref) == nchar(v$alt) && nchar(v$ref) > 1) {
    return(list(
      ok = FALSE,
      error = paste(
        "gnomAD lists each base of a multi-base change on its own, so",
        vcf_id,
        "has no single gnomAD record to show."
      )
    ))
  }
  res <- gnomad_frequency(rsid, dataset, variant_id = vcf_id)
  is_indel <- !is.null(v) && nchar(v$ref) != nchar(v$alt)
  if (isTRUE(res$ok) || !isTRUE(res$missing) || !is_indel) {
    return(res)
  }

  aligned <- ensembl_left_align(vcf_id)
  if (identical(aligned, vcf_id)) {
    return(res)
  }
  if (!is.null(aligned)) {
    return(gnomad_frequency(rsid, dataset, variant_id = aligned))
  }

  if (!is_blank(rsid)) {
    by_rsid <- gnomad_frequency(rsid, dataset)
    if (
      isTRUE(by_rsid$ok) && myvariant_same_vcf_id(by_rsid$variant_id, vcf_id)
    ) {
      return(by_rsid)
    }
  }
  list(
    ok = FALSE,
    error = paste0(
      "gnomAD has no record for ",
      vcf_id,
      " as written. gnomAD lists an insertion or deletion at its leftmost",
      " position in a repeat, and that position could not be checked, so",
      " this does not show the variant is missing from gnomAD."
    )
  )
}

# Display labels for gnomAD's genetic-ancestry group codes.
.gnomad_pop_labels <- c(
  afr = "African / African-American",
  ami = "Amish",
  amr = "Admixed American",
  asj = "Ashkenazi Jewish",
  eas = "East Asian",
  fin = "European (Finnish)",
  mid = "Middle Eastern",
  nfe = "European (non-Finnish)",
  sas = "South Asian",
  remaining = "Remaining"
)

# Pure parser: combine exome + genome per-ancestry counts into one allele-
# frequency table, summing the two sample sets. Sex-split ids (…_XX/…_XY) are
# dropped (they carry an underscore) so only the main ancestry groups remain.
# Returns a data.frame(pop, label, ac, an, af) sorted by frequency, or NULL.
gnomad_parse_populations <- function(exome_pops, genome_pops) {
  # Builds and returns its own local accumulator, rather than mutating an
  # outer-scope variable with `<<-`. Exome and genome rows are just two lists
  # of the same shape, so concatenating them up front means one pass instead
  # of one call (and one reassignment) per source.
  add <- function(pops) {
    acc <- list()
    for (p in pops) {
      id <- pluck_at(p, "id")
      # Keep only the known genetic-ancestry groups: this drops the sex
      # breakdowns (XX/XY), per-ancestry sex splits (…_XX/…_XY), and any
      # sub-populations, leaving one bar per ancestry.
      if (is_blank(id) || !(id %in% names(.gnomad_pop_labels))) {
        next
      }
      ac <- as.numeric(pluck_at(p, "ac", default = 0))
      an <- as.numeric(pluck_at(p, "an", default = 0))
      prev <- acc[[id]] %||% c(0, 0)
      acc[[id]] <- c(prev[[1]] + ac, prev[[2]] + an)
    }
    acc
  }
  acc <- add(c(exome_pops, genome_pops))
  if (length(acc) == 0) {
    return(NULL)
  }
  rows <- lapply(names(acc), function(id) {
    v <- acc[[id]]
    data.frame(
      pop = id,
      label = unname(.gnomad_pop_labels[id]),
      ac = v[[1]],
      an = v[[2]],
      af = if (v[[2]] > 0) v[[1]] / v[[2]] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df$label[is.na(df$label)] <- df$pop[is.na(df$label)]
  df <- df[df$an > 0, , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  df <- df[order(-df$af), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Normalize one frequency block (exome or genome) to a numeric list, or NULL
# when gnomAD has no data for that sample set.
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

# Gene-level constraint: how intolerant the gene is to variation. pLI/LOEUF
# summarize loss-of-function intolerance; the Z-scores summarize missense and
# synonymous depletion. Looked up by gene symbol.
# Returns:
#   list(ok = TRUE, pli, loeuf, oe_lof, oe_mis, mis_z, syn_z, lof_z)
#   list(ok = FALSE, error = "...")
gnomad_gene_constraint <- function(symbol, reference_genome = "GRCh38") {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for constraint lookup."))
  }
  query <- sprintf(
    paste(
      "query($sym: String!) {",
      "  gene(gene_symbol: $sym, reference_genome: %s) {",
      "    gnomad_constraint {",
      "      pli oe_lof oe_lof_upper mis_z syn_z oe_mis lof_z",
      "    }",
      "  }",
      "}",
      sep = "\n"
    ),
    reference_genome
  )
  res <- vr_api_post_json(
    GNOMAD_URL,
    body = list(query = query, variables = list(sym = symbol)),
    source = "gnomAD"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = "gnomAD returned a query error."))
  }
  gnomad_parse_constraint(res$data, symbol)
}

# Pure parser: pull the constraint block out of a gnomAD response. Separated
# from the fetch so it can be tested against a recorded fixture.
gnomad_parse_constraint <- function(data, symbol = NA_character_) {
  con <- pluck_at(data, "data", "gene", "gnomad_constraint")
  if (is.null(con)) {
    return(list(
      ok = FALSE,
      error = paste0("gnomAD has no constraint data for ", symbol, ".")
    ))
  }
  num <- function(k) as.numeric(pluck_at(con, k, default = NA))
  list(
    ok = TRUE,
    pli = num("pli"),
    loeuf = num("oe_lof_upper"),
    oe_lof = num("oe_lof"),
    oe_mis = num("oe_mis"),
    mis_z = num("mis_z"),
    syn_z = num("syn_z"),
    lof_z = num("lof_z")
  )
}

# All ClinVar variants catalogued for a gene, with their protein position and
# clinical significance. Drives the protein "lollipop" plot: each variant is
# placed at its residue and coloured by significance. Looked up by symbol.
# Returns:
#   list(ok = TRUE, variants = data.frame(residue, aa, category, significance,
#        consequence))
#   list(ok = FALSE, error = "...")
gnomad_clinvar_variants <- function(symbol, reference_genome = "GRCh38") {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene symbol for ClinVar lookup."))
  }
  query <- sprintf(
    paste(
      "query($sym: String!) {",
      "  gene(gene_symbol: $sym, reference_genome: %s) {",
      "    clinvar_variants {",
      "      pos hgvsp major_consequence clinical_significance",
      "    }",
      "  }",
      "}",
      sep = "\n"
    ),
    reference_genome
  )
  res <- vr_api_post_json(
    GNOMAD_URL,
    body = list(query = query, variables = list(sym = symbol)),
    source = "gnomAD"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = "gnomAD returned a query error."))
  }
  gnomad_parse_clinvar_variants(
    pluck_at(res$data, "data", "gene", "clinvar_variants"),
    symbol
  )
}

# Bucket a ClinVar clinical-significance string into a coarse category for
# colouring: pathogenic, benign, conflicting, or uncertain/other.
gnomad_sig_category <- function(sig) {
  s <- tolower(as.character(sig %||% ""))
  if (grepl("conflict", s)) {
    "Conflicting"
  } else if (grepl("pathogenic", s)) {
    # Catches "Pathogenic" and "Likely pathogenic" (but conflicting handled above)
    "Pathogenic / likely"
  } else if (grepl("benign", s)) {
    "Benign / likely"
  } else if (grepl("uncertain", s)) {
    "Uncertain"
  } else {
    "Other"
  }
}

# Protein residue number parsed from an HGVS protein change (3- or 1-letter,
# e.g. "p.Val600Glu" or "p.V600E"), or NA when there is no protein position.
gnomad_hgvsp_residue <- function(hgvsp) {
  if (is_blank(hgvsp)) {
    return(NA_integer_)
  }
  m <- regmatches(hgvsp, regexpr("[0-9]+", hgvsp))
  if (length(m) == 0 || !nzchar(m)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(m))
}

# Pure parser: ClinVar variant records -> a residue-level data frame (only the
# protein-coding variants that carry a residue position are kept).
gnomad_parse_clinvar_variants <- function(records, symbol = NA_character_) {
  empty <- list(
    ok = FALSE,
    error = paste0("gnomAD lists no ClinVar variants for ", symbol, ".")
  )
  if (is.null(records) || length(records) == 0) {
    return(empty)
  }
  rows <- lapply(records, function(r) {
    residue <- gnomad_hgvsp_residue(pluck_at(r, "hgvsp"))
    if (is.na(residue)) {
      return(NULL)
    }
    sig <- pluck_at(r, "clinical_significance", default = NA_character_)
    data.frame(
      residue = residue,
      aa = as.character(pluck_at(r, "hgvsp", default = NA_character_)),
      category = gnomad_sig_category(sig),
      significance = as.character(sig %||% NA_character_),
      consequence = as.character(
        pluck_at(r, "major_consequence", default = NA_character_)
      ),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || nrow(rows) == 0) {
    return(empty)
  }
  rownames(rows) <- NULL
  list(ok = TRUE, variants = rows)
}
