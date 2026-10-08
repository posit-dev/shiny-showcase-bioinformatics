# ClinVar client via NCBI E-utilities (JSON): clinical significance, review
# status, and associated conditions for a variant.
# Docs: https://www.ncbi.nlm.nih.gov/books/NBK25500/

EUTILS_BASE <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# Look up the ClinVar classification for a search term (an rsID works best).
#
# `allele` is the myvariant_annotate() result for the allele being reviewed, or
# NULL. ClinVar keeps a record per allele, and also records for haplotypes
# and compound changes that contain it, so an rsID can return several
# (rs113488022 returns V600G's and V600E's). With an allele, the record is its
# own: the ClinVar variation id MyVariant has for it, else the record whose
# title names its cDNA change (see clinvar_pick_uid()). Without one, a single
# record is used and several are an error, since picking one would be a guess.
# Returns:
#   list(ok = TRUE, uid, accession, title, significance, review_status,
#        last_evaluated, conditions)
#   list(ok = FALSE, error = "...")
clinvar_classification <- function(term, allele = NULL) {
  cdna <- allele$hgvsc_all
  protein <- allele$hgvsp_all
  checkable <- length(cdna) > 0 ||
    length(protein) > 0 ||
    !is_blank(allele$vcf_id)
  # An indel in a repeat has more than one cDNA name (GJB2 35delG is c.30del
  # to snpEff and c.35del to ClinVar), so its protein change can stand in.
  indel <- any(grepl("del|dup|ins", cdna)) ||
    .clinvar_vcf_is_indel(allele$vcf_id)

  # MyVariant's id can name a haplotype that contains the allele: for rs7412
  # it is APOE's c.[526C>T;725G>A]. So the record is used only when it is the
  # allele's (see clinvar_record_fits()).
  if (!is_blank(allele$clinvar_id)) {
    uid <- as.character(allele$clinvar_id)
    own <- clinvar_fetch_record(uid)
    if (
      !isTRUE(own$ok) ||
        is_blank(term) ||
        clinvar_record_fits(own, allele$vcf_id, cdna, protein, indel)
    ) {
      return(own)
    }
  }
  if (is_blank(term)) {
    return(list(
      ok = FALSE,
      error = "No variant identifier for ClinVar lookup."
    ))
  }

  search <- vr_api_get(
    EUTILS_BASE,
    path = "esearch.fcgi",
    query = list(db = "clinvar", term = term, retmode = "json"),
    source = "ClinVar"
  )
  if (!search$ok) {
    return(list(ok = FALSE, error = search$error))
  }
  ids <- as.character(unlist(
    pluck_at(search$data, "esearchresult", "idlist"),
    use.names = FALSE
  ))
  if (length(ids) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No ClinVar record for ", term, ".")
    ))
  }
  if (is.null(allele) && length(ids) == 1) {
    return(clinvar_fetch_record(ids))
  }
  if (is.null(allele)) {
    return(list(
      ok = FALSE,
      error = paste0(
        "ClinVar has ",
        length(ids),
        " records for ",
        term,
        ", one per allele. Pick one allele in the Variant box to see its classification."
      )
    ))
  }

  # One esummary call for every record, then keep the allele's own.
  summary <- clinvar_esummary(ids)
  if (!summary$ok) {
    return(list(ok = FALSE, error = summary$error))
  }
  records <- lapply(ids, function(id) pluck_at(summary$data, "result", id))
  uid <- clinvar_pick_uid(
    ids,
    records,
    cdna = cdna,
    protein = protein,
    protein_too = indel,
    vcf_id = allele$vcf_id
  )
  if (is.null(uid)) {
    # Say what was checked. Without a cDNA or protein change there is nothing
    # to match on, which is not the same as ClinVar having no record.
    return(list(
      ok = FALSE,
      error = if (checkable) {
        paste0(
          "None of the ClinVar records for ",
          term,
          " is for ",
          if (is_blank(allele$hgvsp)) allele$id else allele$hgvsp,
          "."
        )
      } else {
        paste0(
          "ClinVar has ",
          length(ids),
          " record(s) for ",
          term,
          ", but ",
          allele$id,
          " has no cDNA or protein change to match them on. Check them on ClinVar."
        )
      }
    ))
  }
  clinvar_parse_record(records[[match(uid, ids)]], uid)
}

# esummary for one or more ClinVar uids, in one request.
clinvar_esummary <- function(ids) {
  vr_api_get(
    EUTILS_BASE,
    path = "esummary.fcgi",
    query = list(
      db = "clinvar",
      id = paste(ids, collapse = ","),
      retmode = "json"
    ),
    source = "ClinVar"
  )
}

# Fetch and parse the record for one uid.
clinvar_fetch_record <- function(uid) {
  summary <- clinvar_esummary(uid)
  if (!summary$ok) {
    return(list(ok = FALSE, error = summary$error))
  }
  record <- pluck_at(summary$data, "result", uid)
  if (is.null(record)) {
    return(list(ok = FALSE, error = "ClinVar summary was unavailable."))
  }
  clinvar_parse_record(record, uid)
}

# Pure helper: TRUE when a fetched record is the allele's own, or when nothing
# known about the allele can tell. The exact change decides when both the
# record and the allele have one; else the title does (see clinvar_pick_uid()).
clinvar_record_fits <- function(
  record,
  vcf_id,
  cdna = character(),
  protein = character(),
  indel = FALSE
) {
  change <- clinvar_spdi_change(clinvar_spdi(record))
  allele <- .mv_parse_vcf_id(vcf_id)
  if (!is.null(change) && !is.null(allele)) {
    return(.mv_same_change(change, allele))
  }
  if (length(cdna) == 0 && length(protein) == 0) {
    return(TRUE)
  }
  !is.null(clinvar_pick_uid("own", list(record), cdna, protein, indel))
}

# Pure helper: the uid of the record for the allele, or NULL. A record is
# matched on its exact change when the allele has a position (see below), else
# on its title, which reads like "NM_004333.6(BRAF):c.1799T>A (p.Val600Glu)".
# In the title, cDNA changes are matched first: two alleles at one position can
# share a protein change (both TTA>TTT and TTA>TTC are Leu>Phe) but never a
# cDNA change. The protein change is used when no cDNA change is known, or,
# with `protein_too`, when none matched (for an indel, whose cDNA name depends
# on where in a repeat it is written).
clinvar_pick_uid <- function(
  ids,
  records,
  cdna = character(),
  protein = character(),
  protein_too = FALSE,
  vcf_id = NA_character_
) {
  # The exact change first: ClinVar's canonical SPDI against the allele's
  # chrom-pos-ref-alt. It needs no cDNA or protein name, and it sees one
  # indel written at two places in a repeat as one change.
  allele <- .mv_parse_vcf_id(vcf_id)
  if (!is.null(allele)) {
    exact <- vapply(
      records,
      function(r) .mv_same_change(clinvar_spdi_change(clinvar_spdi(r)), allele),
      logical(1)
    )
    if (any(exact)) {
      return(as.character(ids[[which(exact)[[1]]]]))
    }
  }
  titles <- vapply(
    records,
    function(r) as.character(pluck_at(r, "title", default = "")),
    character(1)
  )
  by_cdna <- if (length(cdna) > 0) {
    title_cdna <- ifelse(
      grepl(":c\\.[^ ]+", titles),
      sub("^.*?:(c\\.[^ ]+).*$", "\\1", titles, perl = TRUE),
      ""
    )
    sub("(del|dup)[ACGTN]+$", "\\1", title_cdna) %in% cdna
  } else {
    logical(length(ids))
  }
  named <- by_cdna
  if (
    !any(named) && length(protein) > 0 && (length(cdna) == 0 || protein_too)
  ) {
    needles <- paste0("(", protein, ")")
    named <- vapply(
      titles,
      function(t) any(vapply(needles, grepl, logical(1), x = t, fixed = TRUE)),
      logical(1)
    )
  }
  if (!any(named)) NULL else as.character(ids[[which(named)[[1]]]])
}

# TRUE when a chrom-pos-ref-alt id is an insertion or deletion.
.clinvar_vcf_is_indel <- function(vcf_id) {
  parts <- if (is_blank(vcf_id)) character() else strsplit(vcf_id, "-")[[1]]
  length(parts) == 4 && nchar(parts[[3]]) != nchar(parts[[4]])
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
    conditions = clinvar_conditions(germline),
    spdi = clinvar_spdi(record)
  )
}

# A record's canonical SPDI ("NC_000017.11:43124027:CTCT:CT"), from a raw
# esummary record or a parsed one, or NA.
clinvar_spdi <- function(record) {
  spdi <- pluck_at(record, "spdi")
  if (is_blank(spdi)) {
    sets <- pluck_at(record, "variation_set")
    spdi <- if (length(sets) == 1) pluck_at(sets[[1]], "canonical_spdi")
  }
  if (is_blank(spdi)) NA_character_ else as.character(spdi)
}

# Pure helper: an SPDI as the change it makes, list(chrom, pos, ref, alt) on
# GRCh38 1-based coordinates, comparable with .mv_same_change(). The position
# is 0-based and the deleted bases are the reference there, so no anchor base
# is needed. ClinVar's canonical SPDI is on GRCh38. NULL for a sequence that
# is not a chromosome (a patch or an alternate locus).
clinvar_spdi_change <- function(spdi) {
  if (is_blank(spdi)) {
    return(NULL)
  }
  # A regex, not strsplit(), which drops the empty last field of a deletion
  # ("NC_000013.11:32339012:C:").
  parts <- regmatches(
    spdi,
    regexec("^(NC_0+[0-9]+\\.[0-9]+):([0-9]+):([ACGTN]*):([ACGTN]*)$", spdi)
  )[[1]][-1]
  if (length(parts) != 4) {
    return(NULL)
  }
  chrom_number <- as.integer(sub("^NC_0+([0-9]+)\\..*$", "\\1", parts[[1]]))
  chrom <- if (chrom_number <= 22) {
    as.character(chrom_number)
  } else if (chrom_number == 23) {
    "X"
  } else if (chrom_number == 24) {
    "Y"
  } else if (chrom_number == 12920) {
    "MT"
  } else {
    return(NULL)
  }
  pos <- suppressWarnings(as.integer(parts[[2]]))
  if (is.na(pos)) {
    return(NULL)
  }
  list(chrom = chrom, pos = pos + 1L, ref = parts[[3]], alt = parts[[4]])
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
