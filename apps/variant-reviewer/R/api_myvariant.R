# MyVariant.info client: annotate a variant given an rsID or HGVS string.
# Docs: https://docs.myvariant.info/en/latest/

MYVARIANT_BASE <- "https://myvariant.info/v1"

# The dashboard annotates GRCh38, so variant lookups ask MyVariant for hg38
# records. Left out, MyVariant answers in hg19.
MYVARIANT_ASSEMBLY <- "hg38"

# One rsID can cover several alleles (rs113488022 is V600A, V600E and V600G),
# so a variant lookup asks for up to this many hits and then picks one.
MYVARIANT_MAX_ALLELES <- 10

# MyVariant resolves rsIDs and HGVS strings, but free-text matches a bare
# protein change (e.g. "R175H") to an arbitrary variant. Only query for inputs
# that look like an rsID or an HGVS string (which contains a ":").
myvariant_is_queryable <- function(variant) {
  if (is_blank(variant)) {
    return(FALSE)
  }
  term <- trimws(as.character(variant))
  grepl("^rs[0-9]+$", term, ignore.case = TRUE) ||
    grepl(":", term, fixed = TRUE)
}

# The `q` value for a variant. MyVariant reads an unquoted HGVS string such as
# chr7:g.140753336A>T as a field:value query and finds nothing, so HGVS is
# quoted, with any quote or backslash typed inside it escaped.
myvariant_query_term <- function(variant) {
  term <- trimws(as.character(variant))
  if (!grepl(":", term, fixed = TRUE)) {
    return(term)
  }
  paste0("\"", gsub("([\"\\\\])", "\\\\\\1", term), "\"")
}

# Fetch the hit for the one allele `variant` names. An HGVS string names one
# allele. An rsID can name several, and then no hit is picked: the error lists
# the alleles so the user can choose, since any choice made here would be a
# guess (taking the first hit is what showed V600A for BRAF V600E).
# `fields` are the fields the caller needs; the ones used to label alleles are
# added here.
# Returns:
#   list(ok = TRUE, hit)
#   list(ok = FALSE, error, ambiguous = TRUE, gene, alleles) for several alleles
#   list(ok = FALSE, error = "...") otherwise
myvariant_fetch_allele <- function(variant, fields, not_found) {
  term <- trimws(as.character(variant))
  allele_fields <- c(
    "chrom",
    "vcf",
    "dbsnp.rsid",
    "clinvar.variant_id",
    "clinvar.rsid",
    "clinvar.gene.symbol",
    "clinvar.chrom",
    "clinvar.hg38",
    "clinvar.ref",
    "clinvar.alt",
    "dbnsfp.genename",
    "dbnsfp.aa",
    "dbnsfp.uniprot",
    "dbnsfp.hgvsp",
    "dbnsfp.hgvsc",
    "snpeff.ann.genename",
    "snpeff.ann.effect",
    "snpeff.ann.feature_id",
    "snpeff.ann.hgvs_p",
    "snpeff.ann.hgvs_c"
  )
  res <- vr_api_get(
    MYVARIANT_BASE,
    path = "query",
    query = list(
      q = myvariant_query_term(term),
      size = MYVARIANT_MAX_ALLELES,
      assembly = MYVARIANT_ASSEMBLY,
      fields = paste(unique(c(fields, allele_fields)), collapse = ",")
    ),
    source = "MyVariant"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  myvariant_pick_allele(myvariant_place_hits(res$data$hits), term, not_found)
}

# Give hits their leftmost GRCh38 position, stored as `.place`, a
# chrom-pos-ref-alt id like gnomAD's.
#
# MyVariant leaves some records without VCF fields (BRCA1 185delAG is only
# chr17:g.43124028CT[1]), and writes an indel in a repeat at whatever
# position its source used. With the reference sequence, both become one
# exact form: the record's HGVS id is turned into a VCF record, then moved to
# its leftmost position. That lets gnomAD and VEP look up records with no VCF
# fields, and lets one indel written two ways count as one allele.
#
# One reference fetch covers all the hits, and it is only made when it can
# change something: a hit without VCF fields, or two or more indels. Without
# the reference, hits are returned as they came.
myvariant_place_hits <- function(hits, margin = 200L) {
  vcfs <- lapply(hits, myvariant_vcf)
  ids <- vapply(
    hits,
    function(h) as.character(pluck_at(h, "_id", default = NA_character_)),
    character(1)
  )
  is_indel <- vapply(
    vcfs,
    function(v) !is.null(v) && nchar(v$ref) != nchar(v$alt),
    logical(1)
  )
  unplaced <- vapply(vcfs, is.null, logical(1))
  if (length(hits) == 0 || !(any(unplaced) || sum(is_indel) >= 2)) {
    return(hits)
  }

  spans <- lapply(seq_along(hits), function(i) {
    if (!is.null(vcfs[[i]])) {
      return(c(vcfs[[i]]$pos, vcfs[[i]]$pos + nchar(vcfs[[i]]$ref)))
    }
    .mv_hgvs_span(ids[[i]])
  })
  chroms <- unique(stats::na.omit(c(
    vapply(vcfs, function(v) if (is.null(v)) NA_character_ else v$chrom, ""),
    vapply(ids, .mv_hgvs_chrom, "")
  )))
  spans <- Filter(Negate(is.null), spans)
  if (length(chroms) != 1 || length(spans) == 0) {
    return(hits)
  }
  from <- max(1L, min(unlist(spans)) - margin)
  to <- max(unlist(spans)) + margin
  if (to - from > 10000L) {
    return(hits)
  }
  seq <- vr_reference_sequence(chroms, from, to)
  if (is.null(seq)) {
    return(hits)
  }
  ref_at <- function(start, end) {
    if (start < from || end > to || end < start) {
      return(NULL)
    }
    substr(seq, start - from + 1L, end - from + 1L)
  }

  # A VCF record as its leftmost id, or NA when its ref is not the reference
  # (a record built from a wrong id) or it cannot be moved.
  place_of <- function(vcf) {
    if (is.null(vcf)) {
      return(NA_character_)
    }
    if (!identical(ref_at(vcf$pos, vcf$pos + nchar(vcf$ref) - 1L), vcf$ref)) {
      return(NA_character_)
    }
    moved <- if (nchar(vcf$ref) == nchar(vcf$alt)) {
      .mv_trim_same_length(vcf$pos, vcf$ref, vcf$alt)
    } else {
      vr_left_align(
        vcf$pos,
        vcf$ref,
        vcf$alt,
        ref_at(from, vcf$pos - 1L) %||% ""
      )
    }
    if (is.null(moved)) {
      return(NA_character_)
    }
    paste(vcf$chrom, moved$pos, moved$ref, moved$alt, sep = "-")
  }

  for (i in seq_along(hits)) {
    candidates <- c(
      myvariant_vcf_candidates(hits[[i]]),
      list(.mv_hgvs_to_vcf(ids[[i]], ref_at))
    )
    places <- vapply(candidates, place_of, character(1))
    places <- places[!is.na(places)]
    if (length(places) > 0) {
      hits[[i]]$.place <- places[[1]]
      # Whether the hit's own HGVS id names this same change. When one
      # allele has several records, the list shows one whose id does.
      hits[[i]]$.id_ok <- identical(
        place_of(.mv_hgvs_to_vcf(ids[[i]], ref_at)),
        places[[1]]
      )
    }
  }
  hits
}

# A change whose ref and alt are the same length, without the bases they
# share at either end (17-102-CTC-CGG becomes 17-103-TC-GG), the form gnomAD
# uses. One base is always kept.
.mv_trim_same_length <- function(pos, ref, alt) {
  while (nchar(ref) > 1 && substr(ref, 1, 1) == substr(alt, 1, 1)) {
    ref <- substring(ref, 2)
    alt <- substring(alt, 2)
    pos <- pos + 1L
  }
  while (
    nchar(ref) > 1 &&
      substr(ref, nchar(ref), nchar(ref)) == substr(alt, nchar(alt), nchar(alt))
  ) {
    ref <- substr(ref, 1, nchar(ref) - 1)
    alt <- substr(alt, 1, nchar(alt) - 1)
  }
  list(pos = pos, ref = ref, alt = alt)
}

# The chromosome of a genomic HGVS id ("chr7:g.140753336A>T" -> "7"), or NA.
# The mitochondrion is "MT", as MyVariant and Ensembl name it.
.mv_hgvs_chrom <- function(id) {
  if (!grepl("^chr(?:[0-9]+|X|Y|MT|M):g\\.", id, perl = TRUE)) {
    return(NA_character_)
  }
  chrom <- sub("^chr([0-9]+|X|Y|MT|M):.*$", "\\1", id, perl = TRUE)
  if (chrom == "M") "MT" else chrom
}

# The first and last position a genomic HGVS id names, or NULL.
.mv_hgvs_span <- function(id) {
  m <- regmatches(
    id,
    regexec(
      "^chr(?:[0-9]+|X|Y|MT|M):g\\.([0-9]+)(?:_([0-9]+))?",
      id,
      perl = TRUE
    )
  )[[1]]
  if (length(m) == 0) {
    return(NULL)
  }
  start <- as.integer(m[[2]])
  end <- if (nzchar(m[[3]])) as.integer(m[[3]]) else start
  c(start, end)
}

# Pure helper: a MyVariant genomic HGVS id as a VCF record, list(chrom, pos,
# ref, alt), or NULL for a form it does not read. `ref_at(start, end)` returns
# reference bases, or NULL. The forms MyVariant uses:
#   chr7:g.140753336A>T          substitution
#   chr13:g.20189552del          deletion (also S_Edel)
#   chr17:g.43057065dup          duplication (also S_Edup)
#   chr1:g.100_101insAT          insertion
#   chr1:g.100_102delinsAT       deletion-insertion (also Sdelins)
#   chr17:g.43124028CT[1]        repeat: the unit at that position, as many
#                                times as the number says
.mv_hgvs_to_vcf <- function(id, ref_at) {
  chrom <- .mv_hgvs_chrom(id)
  if (is.na(chrom)) {
    return(NULL)
  }
  body <- sub("^chr(?:[0-9]+|X|Y|MT|M):g\\.", "", id, perl = TRUE)
  num <- function(x) suppressWarnings(as.integer(x))
  vcf <- function(pos, ref, alt) {
    if (is.null(ref) || is.null(alt) || is.na(pos) || !nzchar(ref)) {
      return(NULL)
    }
    list(chrom = chrom, pos = pos, ref = ref, alt = alt)
  }

  if (grepl("^[0-9]+[ACGT]>[ACGT]$", body)) {
    pos <- num(sub("[ACGT]>[ACGT]$", "", body))
    return(vcf(
      pos,
      substr(body, nchar(body) - 2, nchar(body) - 2),
      substr(body, nchar(body), nchar(body))
    ))
  }
  m <- regmatches(
    body,
    regexec(
      "^([0-9]+)(?:_([0-9]+))?(delins|del|dup|ins)([ACGT]*)$",
      body,
      perl = TRUE
    )
  )[[1]]
  if (length(m) > 0) {
    start <- num(m[[2]])
    end <- if (nzchar(m[[3]])) num(m[[3]]) else start
    kind <- m[[4]]
    bases <- m[[5]]
    if (is.na(start) || is.na(end) || end < start) {
      return(NULL)
    }
    before <- ref_at(start - 1L, start - 1L)
    span <- ref_at(start, end)
    if (kind == "del") {
      return(vcf(start - 1L, paste0(before, span), before))
    }
    if (kind == "dup") {
      last <- ref_at(end, end)
      return(vcf(end, last, paste0(last, span)))
    }
    if (kind == "ins" && nzchar(bases)) {
      first <- ref_at(start, start)
      return(vcf(start, first, paste0(first, bases)))
    }
    if (kind == "delins" && nzchar(bases)) {
      return(vcf(start - 1L, paste0(before, span), paste0(before, bases)))
    }
    return(NULL)
  }
  m <- regmatches(body, regexec("^([0-9]+)([ACGT]+)\\[([0-9]+)\\]$", body))[[1]]
  if (length(m) > 0) {
    start <- num(m[[2]])
    unit <- m[[3]]
    copies <- num(m[[4]])
    size <- nchar(unit)
    # Count the copies the reference has from `start`.
    have <- 0L
    repeat {
      piece <- ref_at(start + have * size, start + (have + 1L) * size - 1L)
      # Past the end of the reference fetched: the count would be too low.
      if (is.null(piece) || have > 1000L) {
        return(NULL)
      }
      if (!identical(piece, unit)) {
        break
      }
      have <- have + 1L
    }
    if (have == 0L || is.na(copies) || copies == have) {
      return(NULL)
    }
    last <- start + have * size - 1L
    if (copies < have) {
      drop_from <- start + copies * size
      before <- ref_at(drop_from - 1L, drop_from - 1L)
      return(vcf(
        drop_from - 1L,
        paste0(before, ref_at(drop_from, last)),
        before
      ))
    }
    anchor <- ref_at(last, last)
    return(vcf(last, anchor, paste0(anchor, strrep(unit, copies - have))))
  }
  NULL
}

# Pure helper for myvariant_fetch_allele(): the one hit, or an error naming
# every allele when there are several.
myvariant_pick_allele <- function(hits, term, not_found) {
  if (is.null(hits) || length(hits) == 0) {
    hint <- if (grepl(":[cnp]\\.", term)) {
      paste(
        " MyVariant finds transcript HGVS only for some variants;",
        "try the GRCh38 genomic HGVS or the rsID."
      )
    } else {
      ""
    }
    return(list(ok = FALSE, error = paste0(not_found, " '", term, "'.", hint)))
  }
  # A search for an rsID also matches records that carry it in some other
  # field (an EVS rsID, say) but are another variant. Keep the records that
  # are this rsID, unless that leaves none (an rsID merged into another one).
  if (grepl("^rs[0-9]+$", term, ignore.case = TRUE)) {
    own <- vapply(
      hits,
      function(h) {
        # A ClinVar block that belongs to another allele does not make the
        # record this rsID (MyVariant files rs80357906's ClinVar block under
        # rs2051500205's TG duplication).
        ids <- tolower(c(
          unlist(pluck_at(h, "dbsnp", "rsid"), use.names = FALSE),
          if (.mv_clinvar_is_own(h)) {
            unlist(pluck_at(h, "clinvar", "rsid"), use.names = FALSE)
          }
        ))
        tolower(term) %in% ids
      },
      logical(1)
    )
    if (any(own)) {
      hits <- hits[own]
    }
  }
  hits <- myvariant_distinct_alleles(hits)
  if (length(hits) == 1) {
    return(list(ok = TRUE, hit = hits[[1]]))
  }
  ids <- vapply(
    hits,
    function(h) as.character(pluck_at(h, "_id", default = NA_character_)),
    character(1)
  )
  changes <- vapply(hits, myvariant_hgvsp, character(1))
  alleles <- data.frame(id = ids, hgvsp = changes, stringsAsFactors = FALSE)
  alleles <- alleles[order(alleles$id), , drop = FALSE]
  rownames(alleles) <- NULL
  listed <- ifelse(
    is.na(alleles$hgvsp),
    alleles$id,
    paste0(alleles$hgvsp, " (", alleles$id, ")")
  )
  genes <- unique(stats::na.omit(vapply(hits, myvariant_gene, character(1))))
  list(
    ok = FALSE,
    ambiguous = TRUE,
    # Every allele of an rsID sits in the same gene, so the gene-level cards
    # can still load while the user picks one.
    gene = if (length(genes) == 1) genes[[1]] else NULL,
    alleles = alleles,
    error = paste0(
      term,
      " covers ",
      nrow(alleles),
      " alleles: ",
      paste(listed, collapse = ", "),
      ". Pick one from the Variant list, or enter its HGVS."
    )
  )
}

# The real, distinct alleles among an rsID's hits.
#
# Two kinds of hit are not alleles and are dropped: a record whose ref equals
# its alt (ClinVar keeps some for the reference allele, such as APOE's
# c.388=), and a record whose ref disagrees with the ref most hits at that
# position share (rs6025 has a T>C where the reference base is C).
#
# MyVariant can also hold one indel twice, shifted a few bases inside a
# repeat: CFTR F508del is both 7-117559590-ATCT-A and 7-117559591-TCTT-T.
# Those are merged, keeping the hit with a ClinVar record (the one ClinVar and
# gnomAD also use). Only shifts that keep the two records touching are seen
# (see .mv_same_change()). Hits without VCF fields are kept as they are.
myvariant_distinct_alleles <- function(hits) {
  vcfs <- lapply(hits, myvariant_vcf)
  site <- vapply(
    vcfs,
    function(v) if (is.null(v)) NA_character_ else paste(v$chrom, v$pos),
    character(1)
  )
  real <- vapply(
    seq_along(hits),
    function(i) {
      v <- vcfs[[i]]
      if (is.null(v)) {
        return(TRUE)
      }
      if (identical(v$ref, v$alt)) {
        return(FALSE)
      }
      refs <- vapply(
        vcfs[site %in% site[[i]]],
        function(s) substr(s$ref, 1, 1),
        character(1)
      )
      counts <- table(refs)
      top <- names(counts)[counts == max(counts)]
      length(top) > 1 || identical(substr(v$ref, 1, 1), top)
    },
    logical(1)
  )
  kept <- list()
  for (hit in hits[real]) {
    vcf <- myvariant_vcf(hit)
    place <- hit$.place
    same <- which(vapply(
      kept,
      function(k) {
        (!is.null(place) && identical(k$.place, place)) ||
          .mv_same_change(myvariant_vcf(k), vcf)
      },
      logical(1)
    ))
    if (length(same) == 0) {
      kept[[length(kept) + 1]] <- hit
    } else if (.mv_better_record(hit, kept[[same[[1]]]])) {
      kept[[same[[1]]]] <- hit
    }
  }
  kept
}

# Which of two records of one allele the list shows: first one whose HGVS id
# names the allele (see myvariant_place_hits(); MyVariant misnames some), then
# one with a ClinVar record.
.mv_better_record <- function(hit, kept) {
  id_ok <- isTRUE(hit$.id_ok)
  kept_id_ok <- isTRUE(kept$.id_ok)
  if (id_ok != kept_id_ok) {
    return(id_ok)
  }
  is_blank(pluck_at(kept, "clinvar", "variant_id")) &&
    !is_blank(pluck_at(hit, "clinvar", "variant_id"))
}

# TRUE when two VCF records (see myvariant_vcf()) make the same edit to the
# reference. Their ref bases are laid over one stretch of the reference (they
# must agree where they overlap and leave no gap), each edit is applied, and
# the two results are compared.
.mv_same_change <- function(a, b) {
  if (is.null(a) || is.null(b) || !identical(a$chrom, b$chrom)) {
    return(FALSE)
  }
  start <- min(a$pos, b$pos)
  end <- max(a$pos + nchar(a$ref), b$pos + nchar(b$ref)) - 1
  if (end - start + 1 > nchar(a$ref) + nchar(b$ref)) {
    return(FALSE)
  }
  ref <- rep(NA_character_, end - start + 1)
  for (r in list(a, b)) {
    at <- r$pos - start + seq_len(nchar(r$ref))
    bases <- strsplit(r$ref, "", fixed = TRUE)[[1]]
    if (any(!is.na(ref[at]) & ref[at] != bases)) {
      return(FALSE)
    }
    ref[at] <- bases
  }
  if (anyNA(ref)) {
    return(FALSE)
  }
  edit <- function(r) {
    before <- seq_len(r$pos - start)
    after <- seq_along(ref) > r$pos - start + nchar(r$ref)
    paste0(
      paste(ref[before], collapse = ""),
      r$alt,
      paste(ref[after], collapse = "")
    )
  }
  identical(edit(a), edit(b))
}

# A chrom-pos-ref-alt id ("7-117559590-ATCT-A") as a VCF record (see
# myvariant_vcf()), or NULL when it is not one.
.mv_parse_vcf_id <- function(id) {
  if (is_blank(id)) {
    return(NULL)
  }
  parts <- strsplit(as.character(id), "-", fixed = TRUE)[[1]]
  pos <- suppressWarnings(as.integer(parts[2]))
  if (length(parts) != 4 || is.na(pos) || !all(nzchar(parts))) {
    return(NULL)
  }
  list(chrom = parts[[1]], pos = pos, ref = parts[[3]], alt = parts[[4]])
}

# TRUE when two chrom-pos-ref-alt ids name the same change, written either way
# round inside a repeat (see .mv_same_change()).
myvariant_same_vcf_id <- function(a, b) {
  .mv_same_change(.mv_parse_vcf_id(a), .mv_parse_vcf_id(b))
}

# Returns:
#   list(ok = TRUE, id, rsid, gene, hgvsp, hgvsp_all, hgvsc_all, cadd_phred,
#        clinvar_id, vcf_id)
#   list(ok = FALSE, error = "...") (plus ambiguous, gene and alleles when an
#   rsID covers several alleles; see myvariant_fetch_allele())
myvariant_annotate <- function(variant) {
  if (is_blank(variant)) {
    return(list(ok = FALSE, error = "No variant supplied."))
  }
  if (!myvariant_is_queryable(variant)) {
    return(list(
      ok = FALSE,
      error = "Enter an rsID (rs...) or HGVS (e.g. chr7:g.140753336A>T) for variant-level annotation."
    ))
  }
  term <- trimws(as.character(variant))

  found <- myvariant_fetch_allele(
    term,
    fields = c(
      "dbsnp.rsid",
      "cadd.phred",
      "dbnsfp.cadd.phred"
    ),
    not_found = "No annotation found for"
  )
  if (!found$ok) {
    return(found)
  }
  myvariant_parse_hit(found$hit, term)
}

# The allele the variant cards describe, from a myvariant_annotate() result: the
# result itself when it names one allele, list(ambiguous = TRUE) when the input
# is an rsID covering several, and NULL when there is nothing to go on.
vr_variant_allele <- function(annotation) {
  if (isTRUE(annotation$ok)) {
    return(annotation)
  }
  if (isTRUE(annotation$ambiguous)) {
    return(list(ambiguous = TRUE))
  }
  NULL
}

# What an allele-level card shows while an rsID's allele is not picked yet.
vr_allele_needed <- function(what) {
  list(
    ok = FALSE,
    error = paste0(
      "This rsID covers more than one allele. Pick one in the Variant box to see its ",
      what,
      "."
    )
  )
}

# What a card shows when the picked allele has no genomic position in
# MyVariant. Looking it up by rsID instead could return another allele.
vr_allele_unplaced <- function(source) {
  list(
    ok = FALSE,
    error = paste0(
      "MyVariant has no genomic position for this allele, so ",
      source,
      " cannot look it up."
    )
  )
}

# Pure parser: turn a single MyVariant hit into the normalized result. Besides
# what the Variant card shows, it carries what the other variant cards need to
# find this same allele: its ClinVar variation id, its VCF id (for gnomAD and
# VEP), and every spelling of its cDNA and protein change.
myvariant_parse_hit <- function(hit, term = NA_character_) {
  clinvar_ok <- .mv_clinvar_is_own(hit)
  list(
    ok = TRUE,
    id = pluck_at(hit, "_id", default = term),
    rsid = myvariant_rsid(hit),
    gene = myvariant_gene(hit),
    hgvsp = myvariant_hgvsp(hit),
    hgvsp_all = myvariant_hgvsp_all(hit),
    hgvsc_all = myvariant_hgvsc_all(hit),
    cadd_phred = .mv_cadd(hit),
    clinvar_id = if (clinvar_ok) {
      mygene_first(pluck_at(hit, "clinvar", "variant_id"))
    } else {
      NA_character_
    },
    # The leftmost position when myvariant_place_hits() found one.
    vcf_id = hit$.place %||% myvariant_vcf_id(hit)
  )
}

# FALSE when the hit's ClinVar block names a different rsID from its dbSNP
# block, which means it describes another allele. MyVariant files BRCA1
# 5382insC (rs80357906) under chr17:g.43057062_43057063dup, a TG duplication
# with rsID rs2051500205.
.mv_clinvar_is_own <- function(hit) {
  dbsnp <- mygene_first(pluck_at(hit, "dbsnp", "rsid"))
  clinvar <- mygene_first(pluck_at(hit, "clinvar", "rsid"))
  is_blank(dbsnp) ||
    is_blank(clinvar) ||
    identical(tolower(dbsnp), tolower(clinvar))
}

# The hit's dbSNP rsID. Some records carry it only in their ClinVar block (the
# ClinVar record for CFTR F508del has no dbsnp block).
myvariant_rsid <- function(hit) {
  rsid <- mygene_first(pluck_at(hit, "dbsnp", "rsid"))
  if (is_blank(rsid)) {
    rsid <- mygene_first(pluck_at(hit, "clinvar", "rsid"))
  }
  rsid
}

# The hit's gene. dbNSFP covers only single-base substitutions, so an indel or
# a non-coding change takes ClinVar's gene, then snpEff's. snpEff also names
# genes the variant is only near (MT-TL1 m.3243A>G is "downstream" of RNR1),
# so those annotations do not count.
myvariant_gene <- function(hit) {
  gene <- mygene_first(pluck_at(hit, "dbnsfp", "genename"))
  if (is_blank(gene) && .mv_clinvar_is_own(hit)) {
    gene <- mygene_first(pluck_at(hit, "clinvar", "gene", "symbol"))
  }
  if (is_blank(gene)) {
    ann <- pluck_at(hit, "snpeff", "ann")
    if (!is.null(names(ann))) {
      ann <- list(ann)
    }
    inside <- Filter(
      function(a) {
        !pluck_at(a, "effect", default = "") %in%
          c(
            "upstream_gene_variant",
            "downstream_gene_variant",
            "intergenic_region"
          )
      },
      ann
    )
    gene <- mygene_first(.mv_snpeff_field(
      list(snpeff = list(ann = inside)),
      "genename"
    ))
  }
  gene
}

# One snpEff field (e.g. "hgvs_p") across a hit's annotations. `snpeff.ann` is
# one object, or a list of them when the variant hits several transcripts.
.mv_snpeff_field <- function(hit, field) {
  ann <- pluck_at(hit, "snpeff", "ann")
  if (is.null(ann)) {
    return(character())
  }
  if (!is.null(names(ann))) {
    ann <- list(ann)
  }
  values <- unlist(
    lapply(ann, function(a) pluck_at(a, field)),
    use.names = FALSE
  )
  values[!is.na(values) & nzchar(values)]
}

.mv_snpeff_hgvsp <- function(hit) .mv_snpeff_field(hit, "hgvs_p")

# Every cDNA change for a hit, across transcripts (e.g. "c.1799T>A"). The bases
# some tools write after del/dup ("c.1521_1523delCTT") are dropped, as in
# ClinVar's titles. ClinVar records are matched on this, since two alleles at
# one position can share a protein change but never a cDNA change.
myvariant_hgvsc_all <- function(hit) {
  changes <- c(
    .mv_snpeff_field(hit, "hgvs_c"),
    unlist(pluck_at(hit, "dbnsfp", "hgvsc"), use.names = FALSE)
  )
  changes <- sub("(del|dup)[ACGTN]+$", "\\1", changes)
  unique(changes[grepl("^c\\.", changes)])
}

# The protein change to show for a hit (e.g. p.Val600Glu), numbered on the
# reviewed UniProt protein, which is what the protein and structure cards use.
# Neither snpEff nor dbNSFP lists that one first: snpEff lists APOE's long
# isoform (p.Arg202Cys) first, and dbNSFP's first TP53 entry is p.Arg136His,
# not p.Arg175His. A transcript vote does not work either, since TP53 has
# more transcripts for its short isoforms than for the usual one. Without a
# reviewed position, the change most snpEff transcripts give is used, then
# dbNSFP's first.
myvariant_hgvsp <- function(hit) {
  snpeff <- .mv_snpeff_hgvsp(hit)
  dbnsfp <- unlist(pluck_at(hit, "dbnsfp", "hgvsp"), use.names = FALSE)
  pos <- .mv_swissprot_pos(hit)
  if (!is.na(pos)) {
    named <- c(snpeff, dbnsfp)
    named <- named[
      grepl("^p\\.[A-Z][a-z]{2}", named) & .mv_protein_pos(named) %in% pos
    ]
    if (length(named) > 0) {
      return(named[[1]])
    }
  }
  # Without dbNSFP (indels, mostly), take snpEff's change on the gene's
  # lowest-numbered RefSeq transcript, usually its reference one: MSH6's
  # NM_000179 gives p.Phe1088fs where a shorter isoform gives p.Phe786fs.
  ann <- pluck_at(hit, "snpeff", "ann")
  if (!is.null(names(ann))) {
    ann <- list(ann)
  }
  tx <- vapply(
    ann,
    function(a) as.character(pluck_at(a, "feature_id", default = NA)),
    character(1)
  )
  change <- vapply(
    ann,
    function(a) as.character(pluck_at(a, "hgvs_p", default = NA)),
    character(1)
  )
  on_refseq <- !is.na(change) & nzchar(change) & grepl("^NM_[0-9]+", tx)
  if (any(on_refseq)) {
    number <- as.numeric(sub("^NM_0*([0-9]+).*$", "\\1", tx[on_refseq]))
    return(change[on_refseq][[which.min(number)]])
  }
  if (length(snpeff) > 0) {
    counts <- table(factor(snpeff, levels = unique(snpeff)))
    return(names(counts)[which.max(counts)])
  }
  mygene_first(dbnsfp)
}

# The residue number in a protein change ("p.Arg175His", "p.R175H" -> 175), or
# NA.
.mv_protein_pos <- function(x) {
  suppressWarnings(as.integer(sub("^p\\.[A-Za-z]+?([0-9]+).*$", "\\1", x)))
}

# The residue number on the reviewed UniProt (Swiss-Prot) canonical protein,
# from dbNSFP's per-transcript lists, or NA. dbNSFP pairs each transcript's
# position with a UniProt entry. The canonical one has a plain accession
# (P04637, not the isoform P04637-4) and a mnemonic entry name (P53_HUMAN);
# unreviewed TrEMBL entries repeat their accession (A0A2R8Y8E0_HUMAN).
.mv_swissprot_pos <- function(hit) {
  pos <- suppressWarnings(as.integer(unlist(
    pluck_at(hit, "dbnsfp", "aa", "pos"),
    use.names = FALSE
  )))
  uniprot <- pluck_at(hit, "dbnsfp", "uniprot")
  if (!is.null(names(uniprot))) {
    uniprot <- list(uniprot)
  }
  if (length(uniprot) == 0 || length(uniprot) != length(pos)) {
    return(NA_integer_)
  }
  canonical <- vapply(
    uniprot,
    function(u) {
      acc <- as.character(pluck_at(u, "acc", default = ""))
      entry <- as.character(pluck_at(u, "entry", default = ""))
      nzchar(acc) &&
        !grepl("-", acc) &&
        nzchar(entry) &&
        !startsWith(entry, acc)
    },
    logical(1)
  )
  found <- unique(pos[canonical & !is.na(pos)])
  if (length(found) == 1) found else NA_integer_
}

# Every three-letter protein change for a hit, across transcripts. ClinVar
# names a record by one of them, e.g. "NM_004333.6(BRAF):c.1799T>A
# (p.Val600Glu)", so this is what a ClinVar title is matched against.
myvariant_hgvsp_all <- function(hit) {
  changes <- c(
    .mv_snpeff_hgvsp(hit),
    unlist(pluck_at(hit, "dbnsfp", "hgvsp"), use.names = FALSE)
  )
  unique(changes[grepl("^p\\.[A-Z][a-z]{2}[0-9]", changes)])
}

# The hit's GRCh38 VCF record as list(chrom, pos, ref, alt), or NULL: the most
# trusted of myvariant_vcf_candidates().
myvariant_vcf <- function(hit) {
  candidates <- myvariant_vcf_candidates(hit)
  if (length(candidates) == 0) NULL else candidates[[1]]
}

# The hit's possible VCF records, most trusted first: its own ClinVar block
# (see .mv_clinvar_is_own()), then its VCF fields. MyVariant builds the VCF
# fields from its HGVS id, and the id can be wrong while the ClinVar block is
# right: GJB2 35dupG is named chr13:g.20189546_20189547dup, a two-base
# duplication, and NPM1's insertions carry the wrong inserted bases. With the
# reference at hand, myvariant_place_hits() keeps the first one whose ref
# matches it.
myvariant_vcf_candidates <- function(hit) {
  make <- function(chrom, pos, ref, alt) {
    chrom <- as.character(chrom %||% NA)
    pos <- suppressWarnings(as.integer(pos %||% NA))
    ref <- as.character(ref %||% NA)
    alt <- as.character(alt %||% NA)
    if (
      length(pos) != 1 ||
        is.na(pos) ||
        anyNA(c(chrom, ref, alt)) ||
        !all(grepl("^[ACGTN]+$", c(ref, alt))) ||
        !nzchar(chrom)
    ) {
      return(NULL)
    }
    list(chrom = chrom, pos = pos, ref = ref, alt = alt)
  }
  from_vcf <- make(
    pluck_at(hit, "chrom"),
    pluck_at(hit, "vcf", "position"),
    pluck_at(hit, "vcf", "ref"),
    pluck_at(hit, "vcf", "alt")
  )
  from_clinvar <- NULL
  if (.mv_clinvar_is_own(hit)) {
    # ClinVar's ref and alt are VCF style, with the base before an indel, but
    # for a deletion its start is the first deleted base, one past that base.
    ref <- as.character(pluck_at(hit, "clinvar", "ref", default = NA))
    alt <- as.character(pluck_at(hit, "clinvar", "alt", default = NA))
    start <- suppressWarnings(as.integer(
      pluck_at(hit, "clinvar", "hg38", "start", default = NA)
    ))
    is_deletion <- !anyNA(c(ref, alt)) &&
      nchar(ref) > nchar(alt) &&
      startsWith(ref, substr(alt, 1, 1))
    from_clinvar <- make(
      pluck_at(hit, "clinvar", "chrom") %||% pluck_at(hit, "chrom"),
      if (is_deletion) start - 1L else start,
      ref,
      alt
    )
  }
  candidates <- Filter(Negate(is.null), list(from_clinvar, from_vcf))
  candidates[!duplicated(candidates)]
}

# The allele as chrom-pos-ref-alt ("7-140753336-A-T"), the form gnomAD uses for
# its variant ids, or NA when the VCF fields are missing.
myvariant_vcf_id <- function(hit) {
  vcf <- myvariant_vcf(hit)
  if (is.null(vcf)) {
    return(NA_character_)
  }
  paste(vcf$chrom, vcf$pos, vcf$ref, vcf$alt, sep = "-")
}

# CADD phred for a hit. MyVariant's own CADD block exists only for hg19
# records, so hg38 records fall back to the CADD score dbNSFP carries.
.mv_cadd <- function(hit) {
  cadd <- .mv_max_num(pluck_at(hit, "cadd", "phred"))
  if (is.na(cadd)) {
    cadd <- .mv_max_num(pluck_at(hit, "dbnsfp", "cadd", "phred"))
  }
  cadd
}

# In-silico pathogenicity predictions for a variant, from dbNSFP (+ CADD) via
# MyVariant. Fetched through myvariant_fetch_allele() like
# myvariant_annotate(), so it resolves the same allele. Returns:
#   list(ok = TRUE, predictions = list(list(name, score, call), ...))
#   list(ok = FALSE, error = "...")
myvariant_predictions <- function(variant) {
  if (is_blank(variant)) {
    return(list(ok = FALSE, error = "No variant supplied."))
  }
  if (!myvariant_is_queryable(variant)) {
    return(list(
      ok = FALSE,
      error = "Enter an rsID (rs...) or HGVS for in-silico predictions."
    ))
  }
  found <- myvariant_fetch_allele(
    variant,
    fields = c(
      "cadd.phred",
      "dbnsfp.cadd.phred",
      "dbnsfp.revel",
      "dbnsfp.alphamissense",
      "dbnsfp.sift",
      "dbnsfp.polyphen2",
      "dbnsfp.metalr",
      "dbnsfp.metasvm"
    ),
    not_found = "No predictions found for"
  )
  if (!found$ok) {
    return(list(ok = FALSE, error = found$error))
  }
  myvariant_parse_predictions(found$hit)
}

# dbNSFP prediction-code dictionaries (per predictor). Codes come as a scalar or
# a per-transcript array; the parser collapses them to one representative call.
.mv_pred_maps <- list(
  alphamissense = c(
    P = "likely pathogenic",
    B = "likely benign",
    A = "ambiguous"
  ),
  polyphen2 = c(D = "probably damaging", P = "possibly damaging", B = "benign"),
  sift = c(D = "deleterious", T = "tolerated"),
  meta = c(D = "damaging", T = "tolerated")
)

# Max numeric across a scalar/array (most-damaging), NA if none.
.mv_max_num <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else max(v)
}

# First non-empty prediction code mapped through `map`, NA if none.
.mv_call <- function(x, map) {
  codes <- unlist(x, use.names = FALSE)
  codes <- codes[nzchar(codes)]
  if (length(codes) == 0) {
    return(NA_character_)
  }
  code <- codes[[1]]
  if (code %in% names(map)) unname(map[[code]]) else code
}

# Pure parser: turn a MyVariant hit into an ordered list of predictor readouts.
myvariant_parse_predictions <- function(hit) {
  d <- pluck_at(hit, "dbnsfp")
  revel <- .mv_max_num(pluck_at(d, "revel", "score"))
  cadd <- .mv_cadd(hit)
  entries <- list(
    list(
      name = "REVEL",
      score = revel,
      call = if (is.na(revel)) {
        NA_character_
      } else if (revel >= 0.5) {
        "damaging-leaning"
      } else {
        "benign-leaning"
      }
    ),
    list(
      name = "AlphaMissense",
      score = .mv_max_num(pluck_at(d, "alphamissense", "score")),
      call = .mv_call(
        pluck_at(d, "alphamissense", "pred"),
        .mv_pred_maps$alphamissense
      )
    ),
    list(
      name = "CADD (phred)",
      score = cadd,
      call = if (!is.na(cadd) && cadd >= 20) {
        "top ~1% deleterious"
      } else {
        NA_character_
      }
    ),
    list(
      name = "PolyPhen-2",
      score = .mv_max_num(pluck_at(d, "polyphen2", "hdiv", "score")),
      call = .mv_call(
        pluck_at(d, "polyphen2", "hdiv", "pred"),
        .mv_pred_maps$polyphen2
      )
    ),
    list(
      name = "SIFT",
      score = .mv_max_num(pluck_at(d, "sift", "score")),
      call = .mv_call(pluck_at(d, "sift", "pred"), .mv_pred_maps$sift)
    ),
    list(
      name = "MetaLR",
      score = .mv_max_num(pluck_at(d, "metalr", "score")),
      call = .mv_call(pluck_at(d, "metalr", "pred"), .mv_pred_maps$meta)
    ),
    list(
      name = "MetaSVM",
      score = .mv_max_num(pluck_at(d, "metasvm", "score")),
      call = .mv_call(pluck_at(d, "metasvm", "pred"), .mv_pred_maps$meta)
    )
  )
  entries <- Filter(function(e) !is.na(e$score) || !is.na(e$call), entries)
  if (length(entries) == 0) {
    return(list(
      ok = FALSE,
      error = "No in-silico predictions available for this variant."
    ))
  }
  list(ok = TRUE, predictions = entries)
}

# Evolutionary conservation scores for a variant's position, from dbNSFP via
# MyVariant. Higher scores/ranks mean a more conserved (less tolerant) position,
# a supporting line of evidence in variant interpretation. The whole dbnsfp
# block is fetched because the "gerp++" key cannot be requested through the
# field selector. Returns:
#   list(ok = TRUE, metrics = data.frame(metric, score, rankscore))
#   list(ok = FALSE, error = "...")
myvariant_conservation <- function(variant) {
  if (is_blank(variant)) {
    return(list(ok = FALSE, error = "No variant supplied."))
  }
  if (!myvariant_is_queryable(variant)) {
    return(list(
      ok = FALSE,
      error = "Enter an rsID (rs...) or HGVS for conservation scores."
    ))
  }
  term <- trimws(as.character(variant))
  res <- vr_api_get(
    MYVARIANT_BASE,
    path = "query",
    # Conservation is a property of the position, so whichever allele of an
    # rsID comes first gives the same scores.
    query = list(
      q = term,
      size = 1,
      assembly = MYVARIANT_ASSEMBLY,
      fields = "dbnsfp"
    ),
    source = "MyVariant"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  hits <- res$data$hits
  if (is.null(hits) || length(hits) == 0) {
    return(list(
      ok = FALSE,
      error = paste0("No conservation scores found for '", term, "'.")
    ))
  }
  myvariant_parse_conservation(hits[[1]])
}

# One conservation-metric row (numeric score + 0-1 rankscore, NA when absent).
.mv_cons_row <- function(metric, score, rankscore) {
  data.frame(
    metric = metric,
    score = suppressWarnings(as.numeric(score %||% NA)),
    rankscore = suppressWarnings(as.numeric(rankscore %||% NA)),
    stringsAsFactors = FALSE
  )
}

# Pure parser: pull the four common conservation metrics out of a dbNSFP hit.
myvariant_parse_conservation <- function(hit) {
  d <- pluck_at(hit, "dbnsfp")
  rows <- list(
    .mv_cons_row(
      "phyloP (100-way vertebrate)",
      pluck_at(d, "phylop", "100way_vertebrate", "score"),
      pluck_at(d, "phylop", "100way_vertebrate", "rankscore")
    ),
    .mv_cons_row(
      "phastCons (100-way vertebrate)",
      pluck_at(d, "phastcons", "100way_vertebrate", "score"),
      pluck_at(d, "phastcons", "100way_vertebrate", "rankscore")
    ),
    .mv_cons_row(
      "GERP++ RS",
      pluck_at(d, "gerp++", "rs"),
      pluck_at(d, "gerp++", "rs_rankscore")
    ),
    .mv_cons_row(
      "SiPhy (29-way)",
      pluck_at(d, "siphy_29way", "logodds_score"),
      pluck_at(d, "siphy_29way", "logodds_rankscore")
    )
  )
  df <- do.call(rbind, rows)
  df <- df[!(is.na(df$score) & is.na(df$rankscore)), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(
      ok = FALSE,
      error = "No conservation scores available for this variant."
    ))
  }
  rownames(df) <- NULL
  list(ok = TRUE, metrics = df)
}

# Variants for a gene's suggestion list: those with an rsID and at least one
# Pathogenic or Likely pathogenic ClinVar submission. That is a rule for
# choosing the list, not a classification: a variant here can have
# conflicting submissions (BRAF V600E does), so the list shows no
# classification, and the ClinVar card gives ClinVar's own. One row per allele,
# keyed by its hg38 HGVS id, so V600E and V600G of rs113488022 are two
# choices. `total` is how many MyVariant has; at most `size` are fetched.
# Returns:
#   list(ok = TRUE, variants = data.frame(id, rsid, label, position), total)
#   list(ok = FALSE, error = "...")
myvariant_gene_variants <- function(symbol, size = 200) {
  if (is_blank(symbol)) {
    return(list(ok = FALSE, error = "No gene supplied."))
  }
  sym <- trimws(as.character(symbol))
  res <- vr_api_get(
    MYVARIANT_BASE,
    path = "query",
    query = list(
      q = paste0(
        "clinvar.gene.symbol:",
        sym,
        " AND clinvar.rcv.clinical_significance:",
        "(\"Pathogenic\" OR \"Likely pathogenic\")",
        " AND _exists_:dbsnp.rsid"
      ),
      size = size,
      assembly = MYVARIANT_ASSEMBLY,
      fields = paste(
        "dbsnp.rsid",
        "dbnsfp.aa.ref",
        "dbnsfp.aa.alt",
        "dbnsfp.aa.pos",
        "dbnsfp.uniprot",
        sep = ","
      )
    ),
    source = "MyVariant"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  parsed <- myvariant_parse_gene_variants(res$data$hits)
  if (isTRUE(parsed$ok)) {
    parsed$total <- as.integer(res$data$total %||% nrow(parsed$variants))
  }
  parsed
}

# One-letter amino-acid change (e.g. "V600E") from a dbnsfp.aa block, or NA.
# `pos` arrives as a per-transcript array. `canonical` is the position on the
# reviewed UniProt protein (see .mv_swissprot_pos()); without one, the first
# position is used.
.mv_aa_label <- function(aa, canonical = NA_integer_) {
  ref <- mygene_first(pluck_at(aa, "ref"))
  alt <- mygene_first(pluck_at(aa, "alt"))
  pos <- suppressWarnings(as.integer(unlist(
    pluck_at(aa, "pos"),
    use.names = FALSE
  )))
  pos <- if (is.na(canonical)) pos[!is.na(pos)] else canonical
  if (is_blank(ref) || is_blank(alt) || length(pos) == 0) {
    return(NA_character_)
  }
  paste0(ref, pos[[1]], if (identical(alt, "X")) "*" else alt)
}

# Pure parser: turn gene-scoped hits into a variant table sorted by protein
# position, then rsID, with variants that have no protein change last. One row
# per allele.
myvariant_parse_gene_variants <- function(hits) {
  empty <- list(ok = FALSE, error = "No notable variants found for this gene.")
  if (is.null(hits) || length(hits) == 0) {
    return(empty)
  }
  rows <- lapply(hits, function(h) {
    id <- pluck_at(h, "_id")
    rsid <- mygene_first(pluck_at(h, "dbsnp", "rsid"))
    if (is_blank(id) || is_blank(rsid)) {
      return(NULL)
    }
    aa <- pluck_at(h, "dbnsfp", "aa")
    canonical <- .mv_swissprot_pos(h)
    label <- .mv_aa_label(aa, canonical)
    data.frame(
      id = as.character(id),
      rsid = tolower(rsid),
      label = label,
      position = if (is.na(label)) {
        NA_integer_
      } else {
        .mv_protein_pos(paste0("p.", label))
      },
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || nrow(rows) == 0) {
    return(empty)
  }
  rows <- rows[!duplicated(rows$id), ]
  rsid_number <- suppressWarnings(as.numeric(sub("^rs", "", rows$rsid)))
  rows <- rows[order(is.na(rows$position), rows$position, rsid_number), ]
  rownames(rows) <- NULL
  list(ok = TRUE, variants = rows)
}

# Named character vector for a selectizeInput: value = the allele's hg38 HGVS
# id, name = display label like "V600E, rs113488022". The value is the HGVS id,
# not the rsID, because one rsID can be several alleles and the choice has to
# say which one. The label is the rsID alone when there is no amino-acid
# change (e.g. splice/frameshift variants).
myvariant_variant_choices <- function(parsed) {
  if (is.null(parsed) || !isTRUE(parsed$ok)) {
    return(character())
  }
  v <- parsed$variants
  disp <- ifelse(is.na(v$label), v$rsid, paste0(v$label, ", ", v$rsid))
  # Two alleles can share a label (BRAF rs138333692 has two that give N236K);
  # those get their HGVS id too.
  repeated <- disp %in% disp[duplicated(disp)]
  disp[repeated] <- paste0(disp[repeated], " (", v$id[repeated], ")")
  stats::setNames(v$id, disp)
}
