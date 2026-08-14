# Connectivity scoring (CMap / LINCS-style): score each reference perturbation
# profile against a query signature (up / down gene sets). A positive score means
# the perturbation *mimics* the query (drives the same genes the same way); a
# negative score means it *reverses* it -- the basis for drug-repurposing hits.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns a plain data
# frame the classic Shiny modules turn into a ranked table / score distribution,
# and that a future shinyreact version can render client-side.

# Weighted-KS enrichment score (GSEA-style) of one gene set against one ranked
# profile. profile: named numeric (genes), higher = more up-regulated; weights
# the running sum by |profile| as in the CMap connectivity score.
connectivity_es <- function(profile, gene_set) {
  hits_all <- names(profile) %in% gene_set
  if (!any(hits_all)) {
    return(0)
  }
  ord <- order(profile, decreasing = TRUE)
  vals <- abs(profile[ord])
  hits <- hits_all[ord]

  hit_norm <- sum(vals[hits])
  if (hit_norm == 0) {
    return(0)
  }
  p_hit <- cumsum(ifelse(hits, vals, 0)) / hit_norm
  p_miss <- cumsum(ifelse(hits, 0, 1)) / max(sum(!hits), 1)
  dev <- p_hit - p_miss
  dev[which.max(abs(dev))]
}

# Score every perturbation in a profiles matrix against an up/down query.
# profiles: numeric matrix, genes in rows (rownames), perturbations in columns.
# up, down: character vectors of query genes.
# method:
#   "wtcs"   -- weighted connectivity score: combine the up- and down-set
#               enrichment, ES = (ES_up - ES_down)/2 when they have opposite
#               signs (a coherent signature), else 0. Range ~[-1, 1].
#   "cosine" -- cosine similarity to the query vector (+1 up, -1 down, 0 else).
# Returns data.frame(pert_id, score) ordered by score descending (mimics first).
connectivity_score <- function(
  profiles,
  up,
  down,
  method = c("wtcs", "cosine")
) {
  method <- match.arg(method)
  if (!is.matrix(profiles)) {
    profiles <- as.matrix(profiles)
  }
  if (is.null(rownames(profiles))) {
    stop("profiles must have rownames (genes)")
  }
  up <- intersect(as.character(up), rownames(profiles))
  down <- intersect(as.character(down), rownames(profiles))
  if (length(up) == 0 && length(down) == 0) {
    stop("none of the query genes are present in the profiles (id mismatch?)")
  }

  if (method == "wtcs") {
    score <- apply(profiles, 2, function(col) {
      names(col) <- rownames(profiles)
      es_up <- if (length(up) > 0) connectivity_es(col, up) else 0
      es_down <- if (length(down) > 0) connectivity_es(col, down) else 0
      if (sign(es_up) != sign(es_down)) (es_up - es_down) / 2 else 0
    })
  } else {
    qv <- rep(0, nrow(profiles))
    names(qv) <- rownames(profiles)
    qv[up] <- 1
    qv[down] <- -1
    qn <- sqrt(sum(qv^2))
    score <- apply(profiles, 2, function(col) {
      denom <- sqrt(sum(col^2)) * qn
      if (denom == 0) 0 else sum(col * qv) / denom
    })
  }

  res <- data.frame(
    pert_id = colnames(profiles),
    score = round(as.numeric(score), 4),
    stringsAsFactors = FALSE
  )
  res[order(res$score, decreasing = TRUE), , drop = FALSE]
}

# Derive an up / down query signature from a DE results table: the n most
# significant genes in each direction. Lets a DE contrast (e.g. TCGA-BRCA tumor
# vs normal) become the query for connectivity scoring.
# de_results: data frame with `gene`, a log-fold-change column, and a p column.
# Returns list(up = up-regulated genes, down = down-regulated genes).
connectivity_query <- function(
  de_results,
  n = 150,
  lfc_col = "logFC",
  p_col = "padj"
) {
  required <- c("gene", lfc_col, p_col)
  missing <- setdiff(required, names(de_results))
  if (length(missing) > 0) {
    stop("de_results is missing required column(s): ", toString(missing))
  }
  df <- de_results[
    !is.na(de_results[[p_col]]) & !is.na(de_results[[lfc_col]]),
    ,
    drop = FALSE
  ]
  df <- df[order(df[[p_col]]), , drop = FALSE]
  list(
    up = utils::head(as.character(df$gene[df[[lfc_col]] > 0]), n),
    down = utils::head(as.character(df$gene[df[[lfc_col]] < 0]), n)
  )
}
