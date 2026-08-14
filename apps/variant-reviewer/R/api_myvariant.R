# MyVariant.info client: annotate a variant given an rsID or HGVS string.
# Docs: https://docs.myvariant.info/en/latest/

MYVARIANT_BASE <- "https://myvariant.info/v1"

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

# Returns:
#   list(ok = TRUE, id, rsid, gene, hgvsp, cadd_phred, clinvar_significance)
#   list(ok = FALSE, error = "...")
myvariant_annotate <- function(variant) {
  if (is_blank(variant)) {
    return(list(ok = FALSE, error = "No variant supplied."))
  }
  if (!myvariant_is_queryable(variant)) {
    return(list(
      ok = FALSE,
      error = "Enter an rsID (rs...) or HGVS (e.g. chr7:g.140453136A>G) for variant-level annotation."
    ))
  }
  term <- trimws(as.character(variant))

  res <- vr_api_get(
    MYVARIANT_BASE,
    path = "query",
    query = list(
      q = term,
      size = 1,
      fields = paste(
        "dbsnp.rsid",
        "dbnsfp.genename",
        "dbnsfp.hgvsp",
        "cadd.phred",
        "clinvar.rcv.clinical_significance",
        sep = ","
      )
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
      error = paste0("No annotation found for '", term, "'.")
    ))
  }
  myvariant_parse_hit(hits[[1]], term)
}

# Pure parser: turn a single MyVariant hit into the normalized result.
myvariant_parse_hit <- function(hit, term = NA_character_) {
  list(
    ok = TRUE,
    id = pluck_at(hit, "_id", default = term),
    rsid = mygene_first(pluck_at(hit, "dbsnp", "rsid")),
    gene = mygene_first(pluck_at(hit, "dbnsfp", "genename")),
    hgvsp = mygene_first(pluck_at(hit, "dbnsfp", "hgvsp")),
    cadd_phred = pluck_at(hit, "cadd", "phred", default = NA),
    clinvar_significance = myvariant_clinvar_sig(hit)
  )
}

# In-silico pathogenicity predictions for a variant, from dbNSFP (+ CADD) via
# MyVariant. Same query style as myvariant_annotate(), so it resolves the same
# hit. Returns:
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
  term <- trimws(as.character(variant))
  res <- vr_api_get(
    MYVARIANT_BASE,
    path = "query",
    query = list(
      q = term,
      size = 1,
      fields = paste(
        "cadd.phred",
        "dbnsfp.revel",
        "dbnsfp.alphamissense",
        "dbnsfp.sift",
        "dbnsfp.polyphen2",
        "dbnsfp.metalr",
        "dbnsfp.metasvm",
        sep = ","
      )
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
      error = paste0("No predictions found for '", term, "'.")
    ))
  }
  myvariant_parse_predictions(hits[[1]])
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
  cadd <- .mv_max_num(pluck_at(hit, "cadd", "phred"))
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
    query = list(q = term, size = 1, fields = "dbnsfp"),
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

# Notable variants for a gene: ClinVar pathogenic / likely-pathogenic variants
# that carry an rsID, used to populate the search box's variant suggestions.
# Returns:
#   list(ok = TRUE, variants = data.frame(rsid, label, significance, cadd))
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
      fields = paste(
        "dbsnp.rsid",
        "dbnsfp.aa.ref",
        "dbnsfp.aa.alt",
        "dbnsfp.aa.pos",
        "clinvar.rcv.clinical_significance",
        "cadd.phred",
        sep = ","
      )
    ),
    source = "MyVariant"
  )
  if (!res$ok) {
    return(list(ok = FALSE, error = res$error))
  }
  myvariant_parse_gene_variants(res$data$hits)
}

# One-letter amino-acid change (e.g. "V600E") from a dbnsfp.aa block, or NA.
# `pos` arrives as a per-transcript array; the first position is representative.
.mv_aa_label <- function(aa) {
  ref <- mygene_first(pluck_at(aa, "ref"))
  alt <- mygene_first(pluck_at(aa, "alt"))
  pos <- suppressWarnings(as.integer(unlist(
    pluck_at(aa, "pos"),
    use.names = FALSE
  )))
  pos <- pos[!is.na(pos)]
  if (is_blank(ref) || is_blank(alt) || length(pos) == 0) {
    return(NA_character_)
  }
  paste0(ref, pos[[1]], if (identical(alt, "X")) "*" else alt)
}

# Primary clinical significance (label + severity rank) from the "; "-joined
# significance string, so suggestions can lead with the most severe call.
.mv_sig_primary <- function(sig) {
  terms <- tolower(trimws(strsplit(sig %||% "", ";", fixed = TRUE)[[1]]))
  if ("pathogenic" %in% terms) {
    return(list(label = "Pathogenic", rank = 1L))
  }
  if ("likely pathogenic" %in% terms) {
    return(list(label = "Likely pathogenic", rank = 2L))
  }
  first <- terms[nzchar(terms)]
  list(
    label = if (length(first) == 0) {
      "ClinVar"
    } else {
      tools::toTitleCase(first[[1]])
    },
    rank = 3L
  )
}

# Pure parser: turn gene-scoped hits into a ranked, de-duplicated variant table
# (most severe first, then highest CADD). One row per rsID.
myvariant_parse_gene_variants <- function(hits) {
  empty <- list(ok = FALSE, error = "No notable variants found for this gene.")
  if (is.null(hits) || length(hits) == 0) {
    return(empty)
  }
  rows <- lapply(hits, function(h) {
    rsid <- mygene_first(pluck_at(h, "dbsnp", "rsid"))
    if (is_blank(rsid)) {
      return(NULL)
    }
    prim <- .mv_sig_primary(myvariant_clinvar_sig(h))
    data.frame(
      rsid = tolower(rsid),
      label = .mv_aa_label(pluck_at(h, "dbnsfp", "aa")),
      significance = prim$label,
      rank = prim$rank,
      cadd = .mv_max_num(pluck_at(h, "cadd", "phred")),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows) || nrow(rows) == 0) {
    return(empty)
  }
  rows <- rows[order(rows$rank, -ifelse(is.na(rows$cadd), -Inf, rows$cadd)), ]
  rows <- rows[!duplicated(rows$rsid), ]
  rows$rank <- NULL
  rownames(rows) <- NULL
  list(ok = TRUE, variants = rows)
}

# Named character vector for a selectizeInput: value = rsID, name = display label
# like "V600E, rs113488022 (Pathogenic)". Falls back to the rsID when there is
# no amino-acid change (e.g. splice/frameshift variants).
myvariant_variant_choices <- function(parsed, max_n = 100) {
  if (is.null(parsed) || !isTRUE(parsed$ok)) {
    return(character())
  }
  v <- parsed$variants
  if (nrow(v) > max_n) {
    v <- v[seq_len(max_n), ]
  }
  disp <- ifelse(
    is.na(v$label),
    sprintf("%s (%s)", v$rsid, v$significance),
    sprintf("%s, %s (%s)", v$label, v$rsid, v$significance)
  )
  stats::setNames(v$rsid, disp)
}

# clinvar.rcv may be a single object or a list of RCV records; collapse the
# distinct clinical significance values into one readable string.
myvariant_clinvar_sig <- function(hit) {
  rcv <- pluck_at(hit, "clinvar", "rcv")
  if (is.null(rcv)) {
    return(NA_character_)
  }
  sigs <- if (!is.null(rcv$clinical_significance)) {
    rcv$clinical_significance
  } else {
    lapply(rcv, function(r) r$clinical_significance)
  }
  sigs <- unique(unlist(sigs, use.names = FALSE))
  sigs <- sigs[!is.na(sigs) & nzchar(sigs)]
  if (length(sigs) == 0) NA_character_ else paste(sigs, collapse = "; ")
}
