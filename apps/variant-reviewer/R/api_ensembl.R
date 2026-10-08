# Ensembl VEP client: predicted consequences of a variant.
# REST API docs: https://rest.ensembl.org/documentation/info/vep_id_get

ENSEMBL_BASE <- "https://rest.ensembl.org"

# Run VEP for a variant. VEP by rsID runs every allele of the rsID at once (one
# record with the consequences of all of them mixed), so when `vcf_id` names
# the one allele being reviewed, VEP is run on that allele's region instead.
# Returns:
#   list(ok = TRUE, most_severe, assembly,
#        data = data.frame(gene, transcript, consequence, impact, sift, polyphen))
#   list(ok = FALSE, error = "...")
ensembl_vep <- function(rsid, vcf_id = NULL) {
  region <- ensembl_vep_region(vcf_id)
  if (is.null(region) && is_blank(rsid)) {
    return(list(ok = FALSE, error = "No rsID available for VEP lookup."))
  }
  label <- if (is.null(region)) rsid else vcf_id

  res <- vr_api_get(
    ENSEMBL_BASE,
    path = if (is.null(region)) {
      paste0("vep/human/id/", rsid)
    } else {
      paste0("vep/human/region/", region)
    },
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
      error = paste0("Ensembl VEP has no record for ", label, ".")
    ))
  }

  ensembl_parse_vep(records[[1]])
}

# VEP's region form of a chrom-pos-ref-alt allele (GRCh38), or NULL when it is
# not one. VCF adds a shared leading base to insertions and deletions, which
# VEP's form leaves out, and an insertion's region ends one base before it
# starts:
#   7-140753336-A-T    -> 7:140753336-140753336:1/T
#   7-117559590-ATCT-A -> 7:117559591-117559593:1/-
#   7-117559594-T-TCTT -> 7:117559595-117559594:1/CTT
ensembl_vep_region <- function(vcf_id) {
  if (is_blank(vcf_id)) {
    return(NULL)
  }
  parts <- strsplit(as.character(vcf_id), "-", fixed = TRUE)[[1]]
  if (length(parts) != 4 || !all(grepl("^[ACGTN]+$", parts[3:4]))) {
    return(NULL)
  }
  pos <- suppressWarnings(as.integer(parts[[2]]))
  ref <- parts[[3]]
  alt <- parts[[4]]
  if (is.na(pos)) {
    return(NULL)
  }
  if (nchar(ref) != nchar(alt) && substr(ref, 1, 1) == substr(alt, 1, 1)) {
    ref <- substring(ref, 2)
    alt <- substring(alt, 2)
    pos <- pos + 1L
  }
  paste0(
    parts[[1]],
    ":",
    pos,
    "-",
    pos + nchar(ref) - 1L,
    ":1/",
    if (nzchar(alt)) alt else "-"
  )
}

# Bases of the GRCh38 reference from `start` to `end` (1-based, inclusive), or
# NULL when neither source answers. UCSC's genome API comes first: it answers
# in well under a second, while Ensembl's sequence endpoint often takes 10 s
# or more. Ensembl is the fallback.
#
# The sequence only refines a lookup, and every session waits on it (the app
# fetches synchronously in one R process), so each source gets one short try.
# After a source fails it is not asked again for REFERENCE_PAUSE seconds:
# failures are not cached, and without the pause each allele lookup would
# wait out the timeout again.
REFERENCE_PAUSE <- 60
.reference_state <- new.env(parent = emptyenv())

UCSC_API <- "https://api.genome.ucsc.edu"

vr_reference_sequence <- function(chrom, start, end) {
  for (source in c("UCSC", "Ensembl")) {
    if (as.numeric(Sys.time()) < (.reference_state[[source]] %||% -Inf)) {
      next
    }
    res <- if (source == "UCSC") {
      # UCSC counts from 0 and leaves out the end; it names chromosomes chr7
      # and the mitochondrion chrM.
      vr_api_get(
        UCSC_API,
        path = "getData/sequence",
        query = list(
          genome = "hg38",
          chrom = paste0("chr", if (chrom == "MT") "M" else chrom),
          start = start - 1L,
          end = end
        ),
        source = "UCSC",
        timeout = 8,
        max_tries = 1
      )
    } else {
      vr_api_get(
        ENSEMBL_BASE,
        path = paste0(
          "sequence/region/human/",
          chrom,
          ":",
          start,
          "..",
          end,
          ":1"
        ),
        query = list(`content-type` = "application/json"),
        source = "Ensembl",
        timeout = 8,
        max_tries = 1
      )
    }
    seq <- if (isTRUE(res$ok)) {
      pluck_at(res$data, if (source == "UCSC") "dna" else "seq")
    }
    if (!is_blank(seq) && nchar(seq) == end - start + 1L) {
      return(toupper(as.character(seq)))
    }
    status <- res$status %||% NA_integer_
    if (is.na(status) || status >= 500 || status == 429) {
      .reference_state[[source]] <- as.numeric(Sys.time()) + REFERENCE_PAUSE
    }
  }
  NULL
}

# An indel's chrom-pos-ref-alt id moved to its leftmost position in a repeat,
# the form gnomAD stores: 13-32339427-AA-A (BRCA2 c.5073del) becomes
# 13-32339421-CA-C. Substitutions are returned unchanged. NULL when the
# reference cannot be fetched, or when the repeat runs past the `window`
# bases fetched to the left.
ensembl_left_align <- function(vcf_id, window = 200L) {
  v <- .mv_parse_vcf_id(vcf_id)
  if (is.null(v)) {
    return(NULL)
  }
  if (nchar(v$ref) == nchar(v$alt)) {
    return(vcf_id)
  }
  left <- if (v$pos > 1) {
    vr_reference_sequence(v$chrom, max(1L, v$pos - window), v$pos - 1L)
  } else {
    ""
  }
  if (is.null(left)) {
    return(NULL)
  }
  moved <- vr_left_align(v$pos, v$ref, v$alt, left)
  if (is.null(moved)) {
    return(NULL)
  }
  paste(v$chrom, moved$pos, moved$ref, moved$alt, sep = "-")
}

# Pure helper for ensembl_left_align(), the usual normalization: drop a last
# base ref and alt share; when either runs out, take the next reference base
# from the left; repeat; then drop shared first bases down to one. `left` is
# the reference just before `pos`. NULL when the shift runs past it.
vr_left_align <- function(pos, ref, alt, left) {
  repeat {
    nr <- nchar(ref)
    na <- nchar(alt)
    if (nr > 0 && na > 0 && substr(ref, nr, nr) == substr(alt, na, na)) {
      ref <- substr(ref, 1, nr - 1)
      alt <- substr(alt, 1, na - 1)
    } else if (nr == 0 || na == 0) {
      nl <- nchar(left)
      if (nl == 0) {
        return(NULL)
      }
      base <- substr(left, nl, nl)
      left <- substr(left, 1, nl - 1)
      ref <- paste0(base, ref)
      alt <- paste0(base, alt)
      pos <- pos - 1L
    } else {
      break
    }
  }
  while (
    nchar(ref) > 1 && nchar(alt) > 1 && substr(ref, 1, 1) == substr(alt, 1, 1)
  ) {
    ref <- substring(ref, 2)
    alt <- substring(alt, 2)
    pos <- pos + 1L
  }
  list(pos = pos, ref = ref, alt = alt)
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

# Exon/transcript model for a gene, from Ensembl's lookup endpoint (canonical
# transcript). Drives the gene-model card, which draws the exons and marks the
# exon the variant falls in. Looked up by Ensembl gene id.
# Returns:
#   list(ok = TRUE, transcript, strand, region, gene_start, gene_end,
#        exons = data.frame(start, end, number))
#   list(ok = FALSE, error = "...")
ensembl_gene_model <- function(gene_id) {
  if (is_blank(gene_id)) {
    return(list(ok = FALSE, error = "No Ensembl gene id for the gene model."))
  }
  res <- vr_api_get(
    ENSEMBL_BASE,
    path = paste0("lookup/id/", gene_id),
    query = list(expand = 1, `content-type` = "application/json"),
    source = "Ensembl"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  ensembl_parse_gene_model(res$data)
}

# Pure parser: pick the canonical transcript (else the first) and tidy its exons
# into a start-sorted, numbered data frame.
ensembl_parse_gene_model <- function(record) {
  transcripts <- pluck_at(record, "Transcript")
  if (is.null(transcripts) || length(transcripts) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no transcripts."))
  }
  canonical <- Filter(
    function(t) isTRUE(as.logical(pluck_at(t, "is_canonical"))),
    transcripts
  )
  tx <- if (length(canonical) > 0) canonical[[1]] else transcripts[[1]]
  exons <- pluck_at(tx, "Exon")
  if (is.null(exons) || length(exons) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no exons."))
  }
  rows <- lapply(exons, function(e) {
    data.frame(
      start = suppressWarnings(as.numeric(pluck_at(e, "start", default = NA))),
      end = suppressWarnings(as.numeric(pluck_at(e, "end", default = NA))),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$start) & !is.na(df$end), , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(ok = FALSE, error = "Ensembl returned no exon coordinates."))
  }
  df <- df[order(df$start), , drop = FALSE]
  strand <- suppressWarnings(as.numeric(pluck_at(tx, "strand", default = NA)))
  # Number exons in transcription (5'->3') order: on the minus strand that is
  # decreasing genomic coordinate, so the highest-coordinate exon is exon 1.
  df$number <- if (isTRUE(strand < 0)) {
    rev(seq_len(nrow(df)))
  } else {
    seq_len(nrow(df))
  }
  rownames(df) <- NULL
  list(
    ok = TRUE,
    transcript = as.character(pluck_at(tx, "id", default = NA_character_)),
    strand = strand,
    region = as.character(pluck_at(record, "seq_region_name", default = NA)),
    gene_start = suppressWarnings(as.numeric(
      pluck_at(record, "start", default = min(df$start))
    )),
    gene_end = suppressWarnings(as.numeric(
      pluck_at(record, "end", default = max(df$end))
    )),
    exons = df
  )
}

# Build a data.frame of protein-coding transcript consequences (the rows worth
# showing), or NULL when there are none.
ensembl_consequences_df <- function(consequences) {
  if (is.null(consequences) || length(consequences) == 0) {
    return(NULL)
  }
  is_coding <- vapply(
    consequences,
    function(x) {
      identical(pluck_at(x, "biotype"), "protein_coding")
    },
    logical(1)
  )
  consequences <- consequences[is_coding]
  if (length(consequences) == 0) {
    return(NULL)
  }

  chr_field <- function(key) {
    vapply(
      consequences,
      function(x) {
        as.character(pluck_at(x, key, default = NA_character_))
      },
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
