# Ensembl VEP client: predicted consequences of a variant.
# REST API docs: https://rest.ensembl.org/documentation/info/vep_id_get

ENSEMBL_BASE <- "https://rest.ensembl.org"

# Run VEP for a variant id (rsID).
# Returns:
#   list(ok = TRUE, most_severe, assembly,
#        data = data.frame(gene, transcript, consequence, impact, sift, polyphen))
#   list(ok = FALSE, error = "...")
ensembl_vep <- function(rsid) {
  if (is_blank(rsid)) {
    return(list(ok = FALSE, error = "No rsID available for VEP lookup."))
  }

  res <- vr_api_get(
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
