# Shiny-free data layer: load the bundled CSVs and shape them into the exact
# contracts the plotomics factories expect (columns + meta). The SAME shaped
# data backs both the React feeds (via reactive_output) and the ggplot2
# renderings, so the two engines always show the same numbers.
#
# All paths are relative to the app working directory (shiny::runApp sets it).

.data_path <- function(f) file.path("data", f)

# ---- differential expression (volcano) -----------------------------------
biov_de <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("de_results.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# Tidy the DE table into volcano columns. Returns a list with x (log2FC),
# y (-log10 p), gene, status. `label` is left to the React factory (labelTopN)
# and to ggrepel on the ggplot side.
biov_volcano <- function(fc = 1, p = 0.05, p_col = "padj") {
  de <- biov_de()
  p_raw <- de[[p_col]]
  neg_log10_p <- -log10(pmax(p_raw, .Machine$double.xmin))
  status <- ifelse(de$logFC >= fc & p_raw < p, "Up",
             ifelse(de$logFC <= -fc & p_raw < p, "Down", "NS"))
  list(
    gene = as.character(de$gene),
    logFC = de$logFC,
    p = p_raw,
    neg_log10_p = neg_log10_p,
    status = factor(status, levels = c("Down", "NS", "Up"))
  )
}

# ---- expression matrix + sample metadata (heatmap) ------------------------
biov_expression <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      df <- utils::read.csv(.data_path("expression.csv"),
                            stringsAsFactors = FALSE, check.names = FALSE)
      genes <- df[[1]]
      m <- as.matrix(df[, -1, drop = FALSE])
      rownames(m) <- genes
      cache <<- m
    }
    cache
  }
})

biov_metadata <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("metadata.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# Select the `n_genes` most variable genes, order samples by group, and return
# both the raw matrix (genes x samples) and the plotomics heatmap contract
# (row-major values + meta). z-scoring is left to the consumer so the ggplot2
# and React sides apply it identically.
biov_heatmap <- function(n_genes = 40) {
  m <- biov_expression()
  meta <- biov_metadata()
  # order columns by group so the two blocks are visually contiguous
  grp <- meta$group[match(colnames(m), meta$sample)]
  col_ord <- order(grp)
  m <- m[, col_ord, drop = FALSE]
  grp <- grp[col_ord]
  v <- apply(m, 1, stats::var)
  top <- utils::head(order(v, decreasing = TRUE), n_genes)
  sub <- m[top, , drop = FALSE]
  list(
    matrix = sub,                       # genes x samples (for ggplot2)
    groups = grp,
    values = as.numeric(t(sub)),        # row-major: gene r, sample c -> r*ncols + c
    nrows = nrow(sub),
    ncols = ncol(sub),
    rowLabels = rownames(sub),
    colLabels = colnames(sub)
  )
}

# ---- mutations (treemap + igv) --------------------------------------------
biov_mutations <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("mutations.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# Build the treemap hierarchy: one root ("BRCA") -> gene -> variant leaf, leaf
# value = mutation count. stratify() requires exactly one root, so the root row
# carries an empty parent.
biov_treemap <- function() {
  mut <- biov_mutations()
  # collapse duplicate protein changes just in case
  agg <- stats::aggregate(count ~ gene + protein_change, data = mut, FUN = sum)
  gene_tot <- stats::aggregate(count ~ gene, data = agg, FUN = sum)
  gene_tot <- gene_tot[order(-gene_tot$count), ]

  ids <- c("BRCA", gene_tot$gene, paste(agg$gene, agg$protein_change, sep = ":"))
  parents <- c("", rep("BRCA", nrow(gene_tot)), agg$gene)
  values <- c(0, rep(0, nrow(gene_tot)), agg$count)
  labels <- c("BRCA", gene_tot$gene, agg$protein_change)
  list(
    id = ids, parent = parents, value = values, labels = labels,
    # for the ggplot2 renderer (flat gene->variant)
    df = data.frame(gene = agg$gene, variant = agg$protein_change,
                    count = agg$count, stringsAsFactors = FALSE),
    gene_order = gene_tot$gene
  )
}

# igv.js config: hg19 (mutations are hg19), centred on the selected gene, and a
# single inline 'annotation' track built from the mutation sites so both the
# classic (needle plot) and React (igv.js) genome views show the same variants.
biov_igv_config <- function(gene = "TP53") {
  mut <- biov_mutations()
  features <- lapply(seq_len(nrow(mut)), function(i) {
    list(
      chr = mut$chrom[i],
      start = mut$pos[i] - 1L,           # igv features are 0-based start
      end = mut$pos[i],
      name = paste0(mut$gene[i], " ", mut$protein_change[i]),
      score = mut$count[i]
    )
  })
  # locus spans the selected gene's variants (with padding)
  sub <- mut[mut$gene == gene, , drop = FALSE]
  if (!nrow(sub)) sub <- mut
  pad <- 2000
  locus <- sprintf("%s:%d-%d", sub$chrom[1], min(sub$pos) - pad, max(sub$pos) + pad)
  list(
    genome = "hg19",
    locus = locus,
    tracks = list(list(
      name = "BRCA mutations",
      type = "annotation",
      displayMode = "EXPANDED",
      color = "#ff6b6b",
      features = features
    ))
  )
}

# genes present in the mutation set, most-recurrent first (for the gene picker).
biov_mutation_genes <- function() {
  mut <- biov_mutations()
  tot <- stats::aggregate(count ~ gene, data = mut, FUN = sum)
  tot$gene[order(-tot$count)]
}

# ---- large gene network (treemap/igv share BRCA; this is its own graph) ----
# A large simulated gene-regulatory network with community structure: a
# stochastic block model of `k` modules, laid out ONCE with igraph so the React
# (sigma/WebGL) and classic (ggplot2) engines share identical coordinates. This
# is the network analogue of the UMAP showcase - many thousands of edges that
# WebGL keeps interactive while ggplot2 renders slowly.
biov_network <- local({
  cache <- NULL
  function(n_per = 130, k = 12) {
    if (!is.null(cache)) return(cache)
    stopifnot(requireNamespace("igraph", quietly = TRUE))
    set.seed(42)
    sizes <- rep(n_per, k)
    n <- sum(sizes)
    # dense within-module, sparse between-module connections
    p_in <- 0.06; p_out <- 0.0012
    pm <- matrix(p_out, k, k); diag(pm) <- p_in
    g <- igraph::sample_sbm(n, pref.matrix = pm, block.sizes = sizes)
    g <- igraph::simplify(g)
    # keep the giant component so the layout is coherent
    comp <- igraph::components(g)
    g <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
    module <- rep(seq_len(k), times = sizes)[as.integer(igraph::V(g))]
    deg <- igraph::degree(g)
    # fast large-graph layout
    lay <- igraph::layout_with_drl(g)
    lay <- scale(lay)  # center/normalize
    ids <- paste0("G", seq_len(igraph::vcount(g)))
    el <- igraph::as_edgelist(g, names = FALSE)
    cache <<- list(
      id = ids,
      x = as.numeric(lay[, 1]),
      y = as.numeric(lay[, 2]),
      size = as.numeric(2 + 6 * (deg - min(deg)) / (max(deg) - min(deg) + 1)),
      group = paste0("module ", module),
      source = ids[el[, 1]],
      target = ids[el[, 2]],
      n_nodes = igraph::vcount(g),
      n_edges = igraph::ecount(g),
      layout = lay,
      edge_index = el
    )
    cache
  }
})

# ---- Tahoe-100M perturbation coverage (real data via prep script) ---------
# Drug x cell-line matrix of cells profiled (log10), from Tahoe-100M's
# obs_cell_grid (see data/prep/prepare-tahoe.R). The Tahoe explorer app's
# flagship "coverage" view; here it drives both engines as a heatmap.
biov_tahoe <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      df <- utils::read.csv(.data_path("tahoe_perturbation.csv"),
                            check.names = FALSE, stringsAsFactors = FALSE)
      drugs <- df[[1]]
      m <- as.matrix(df[, -1, drop = FALSE])
      rownames(m) <- drugs
      lm <- log10(m + 1)
      cache <<- list(
        matrix = lm, drugs = drugs, cells = colnames(m),
        values = as.numeric(t(lm)),           # row-major
        nrows = nrow(lm), ncols = ncol(lm),
        rowLabels = drugs, colLabels = colnames(m)
      )
    }
    cache
  }
})

# ---- Hi-C contact matrix (simulated chromatin interactions) ---------------
# A synthetic but realistic Hi-C map: distance decay along the diagonal, nested
# topologically-associating domains (TADs), and a few off-diagonal loops. Returned
# row-major so both the plotomics `hic` (WebGL) and ggplot2 (geom_raster) engines
# read the identical matrix.
biov_hic <- local({
  cache <- NULL
  function(n = 180L, chrom = "chr8", bin_size = 100000L) {
    if (!is.null(cache)) return(cache)
    set.seed(7)
    idx <- seq_len(n)
    d <- abs(outer(idx, idx, "-"))
    M <- 1 / (d + 1)^0.9                     # polymer distance decay
    # nested TAD blocks: boost within-domain contacts
    bounds <- unique(c(1, sort(sample(6:(n - 6), 10)), n + 1))
    for (b in seq_len(length(bounds) - 1)) {
      lo <- bounds[b]; hi <- bounds[b + 1] - 1
      M[lo:hi, lo:hi] <- M[lo:hi, lo:hi] + 0.5
    }
    # a handful of long-range loops (corner peaks)
    for (k in seq_len(6)) {
      i <- sample(idx, 1); j <- min(n, i + sample(15:45, 1))
      M[i, j] <- M[i, j] + 0.8; M[j, i] <- M[i, j]
    }
    M <- M * matrix(stats::rgamma(n * n, shape = 8, rate = 8), n, n)  # noise
    M <- (M + t(M)) / 2                       # enforce symmetry
    cache <<- list(
      values = as.numeric(t(M)),              # row-major
      n = n, chrom = chrom, bin_size = bin_size,
      matrix = M
    )
    cache
  }
})

# ---- UMAP ggplot sample (the deliberately slower classic side) ------------
biov_umap_sample <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("umap_ggplot_sample.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# ---- Xenium single-molecule transcripts -----------------------------------
# The React side streams the binary blobs in www/data straight from HTTP, so
# the server never sees the full million detections. What it does read is the
# sidecar, because that is where the level order and the colours live: driving
# both engines from the one file is what stops them drifting apart.
biov_xenium_meta <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- jsonlite::fromJSON(
        .data_path(file.path("..", "www", "data", "xenium_meta.json")),
        simplifyVector = TRUE)
    }
    cache
  }
})

# The 40k subsample the ggplot2 side is limited to, mirroring biov_umap_sample.
biov_xenium_sample <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("xenium_ggplot_sample.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# ---- GWAS summary statistics (Manhattan + QQ) -----------------------------
# Simulated genome-wide association results: SNPs across 22 chromosomes with a
# handful of genuine association peaks. Seeded so both engines see identical
# data. Returns per-SNP arrays plus the cumulative x-position and per-chromosome
# tick centres a Manhattan plot needs.
biov_gwas <- local({
  cache <- NULL
  function(n = 50000L) {
    if (!is.null(cache)) return(cache)
    set.seed(11)
    # approximate hg38 chromosome lengths (Mb) for realistic spacing
    chr_len <- c(248,242,198,190,181,171,159,145,138,133,135,133,114,107,
                 101,90,83,80,58,64,46,50)
    chr <- sample(seq_len(22), n, replace = TRUE, prob = chr_len)
    pos <- floor(runif(n) * chr_len[chr] * 1e6) + 1
    # null p-values (uniform); then inject peaks on a few chromosomes
    p <- runif(n)
    peaks <- data.frame(chr = c(2, 6, 6, 9, 15, 19),
                        at = c(135, 32, 130, 22, 78, 45) * 1e6)
    for (i in seq_len(nrow(peaks))) {
      near <- which(chr == peaks$chr[i] & abs(pos - peaks$at[i]) < 3e6)
      if (length(near)) {
        k <- sample(near, min(length(near), 60))
        d <- abs(pos[k] - peaks$at[i]) / 3e6
        p[k] <- 10^(-(runif(length(k), 6, 14) * (1 - d)))
      }
    }
    p <- pmin(pmax(p, .Machine$double.xmin), 1)
    ord <- order(chr, pos)
    chr <- chr[ord]; pos <- pos[ord]; p <- p[ord]
    # cumulative x offset per chromosome
    offs <- cumsum(c(0, chr_len[-22])) * 1e6
    x <- pos + offs[chr]
    centres <- offs + chr_len * 1e6 / 2
    list(chr = chr, pos = pos, p = p, x = x,
         neglog10p = -log10(p), n = length(p),
         chr_centres = centres, chr_bounds = c(offs, sum(chr_len) * 1e6),
         genome_len = sum(chr_len) * 1e6, sig = -log10(5e-8))
  }
})

# Q-Q data: observed vs expected -log10 p (thinned for plotting).
biov_qq <- function() {
  g <- biov_gwas()
  o <- sort(g$p)
  m <- length(o)
  expected <- -log10((seq_len(m) - 0.5) / m)
  observed <- -log10(o)
  # thin the bulk (keep all of the informative tail)
  keep <- unique(c(seq(1, m, length.out = 4000L), (m - 2000L):m))
  keep <- keep[keep >= 1 & keep <= m]
  list(expected = expected[keep], observed = observed[keep],
       lambda = round(stats::median(stats::qchisq(1 - o, 1)) / stats::qchisq(0.5, 1), 3))
}

# ---- N-dimensional array cube (hyperspectral) -----------------------------
# Reads the Float32 cube written by data/prep/prepare-ndarray.R (channel-major,
# y-major within channel) and exposes a 2-D channel slice and a per-pixel
# spectrum. Same blob the React client fetches, so both engines agree.
biov_ndarray <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      meta <- jsonlite::fromJSON(.data_path(file.path("..", "www", "data", "ndarray_meta.json")))
      con <- file(.data_path(file.path("..", "www", "data", "ndarray.f32")), "rb")
      cube <- readBin(con, "numeric", n = meta$ny * meta$nx * meta$nch, size = 4, endian = "little")
      close(con)
      ny <- meta$ny; nx <- meta$nx; nch <- meta$nch
      cache <<- list(
        ny = ny, nx = nx, nch = nch,
        slice = function(ch) {
          ch <- max(1L, min(nch, as.integer(ch)))
          block <- cube[((ch - 1L) * ny * nx + 1L):(ch * ny * nx)]
          matrix(block, nrow = ny, ncol = nx, byrow = TRUE)   # m[y, x]
        },
        spectrum = function(px, py) {
          px <- max(1L, min(nx, as.integer(px))); py <- max(1L, min(ny, as.integer(py)))
          idx <- (seq_len(nch) - 1L) * ny * nx + (py - 1L) * nx + (px - 1L) + 1L
          cube[idx]
        }
      )
    }
    cache
  }
})

# ---- single-cell ATAC coverage-by-cluster ---------------------------------
# Simulated pseudobulk accessibility (Signac CoveragePlot style): per-cluster
# signal across a genomic window, with shared promoter peaks and a few
# cluster-specific enhancer peaks. Returns the coverage matrix (cluster x bin)
# plus bin positions and a simple gene model.
biov_atac <- local({
  cache <- NULL
  function(n_bins = 600L, n_clusters = 8L, chrom = "chr11", start = 65200000L) {
    if (!is.null(cache)) return(cache)
    set.seed(31)
    bp <- start + round(seq(0, 60000, length.out = n_bins))
    clusters <- paste0("cluster ", seq_len(n_clusters))
    peak <- function(centre, width, height) height * exp(-0.5 * ((bp - centre) / width)^2)
    m <- matrix(0, n_clusters, n_bins)
    shared <- peak(start + 12000, 800, 1)          # a promoter shared by all
    for (c in seq_len(n_clusters)) {
      row <- shared * runif(1, 0.6, 1) +
        peak(start + sample(c(20000, 30000, 42000, 52000), 1), 1200, runif(1, 0.4, 1.1)) +
        peak(start + 5000 + c * 6500, 700, runif(1, 0.5, 1.3))   # cluster-specific enhancer
      m[c, ] <- row + abs(stats::rnorm(n_bins, 0, 0.03))
    }
    m <- m / max(m)
    genes <- data.frame(name = "GENE", xstart = start + 11000, xend = start + 46000,
                        exons_s = c(11000, 24000, 45000) + start,
                        exons_e = c(13000, 26000, 46000) + start)
    list(matrix = m, positions = bp, clusters = clusters,
         signal = as.numeric(t(m)), n_clusters = n_clusters, n_bins = n_bins,
         chrom = chrom, start = min(bp), end = max(bp), genes = genes)
  }
})

# ---- eQTL / pQTL effect matrix --------------------------------------------
# Simulated cis-QTL effect sizes: variants (rows) x genes/proteins (cols),
# signed effect (beta). A block structure gives clusters of co-regulated
# variant-gene pairs. Returns the plotomics heatmap contract + the raw matrix.
biov_eqtl <- local({
  cache <- NULL
  function(n_var = 45L, n_gene = 30L) {
    if (!is.null(cache)) return(cache)
    set.seed(21)
    m <- matrix(stats::rnorm(n_var * n_gene, 0, 0.25), n_var, n_gene)
    # a few strong QTL blocks
    for (b in list(list(1:12, 1:8, 1.4), list(15:26, 10:18, -1.2),
                   list(30:40, 20:28, 1.1))) {
      m[b[[1]], b[[2]]] <- m[b[[1]], b[[2]]] +
        matrix(stats::rnorm(length(b[[1]]) * length(b[[2]]), b[[3]], 0.2),
               length(b[[1]]), length(b[[2]]))
    }
    rownames(m) <- sprintf("rs%d", sample(1e6:9e6, n_var))
    colnames(m) <- sprintf("GENE%02d", seq_len(n_gene))
    list(matrix = m, values = as.numeric(t(m)), nrows = n_var, ncols = n_gene,
         rowLabels = rownames(m), colLabels = colnames(m))
  }
})

# ---- oncoplot / OncoPrint (cohort alteration landscape) -------------------
# Real TCGA-BRCA per-sample alterations from cBioPortal (see
# data/prep/prepare-brca-cohort.R): somatic mutations plus GISTIC deep
# deletions and amplifications, collapsed to one class per gene x sample, with
# clinical annotation and overall survival alongside.
biov_brca_alterations <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("brca_oncoplot.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

biov_brca_clinical <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("brca_clinical.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# cBioPortal's memoSort: genes by descending alteration frequency, then samples
# ordered so the top gene's carriers come first, ties broken by the next gene
# down. That ordering is what makes mutual exclusivity between drivers read as
# a staircase. It is computed HERE, once, and shipped to both engines, because
# two implementations tie-breaking differently would silently disagree.
.memo_sort <- function(hit) {
  g <- order(rowSums(hit), decreasing = TRUE)
  hit <- hit[g, , drop = FALSE]
  keys <- lapply(seq_len(nrow(hit)), function(i) -as.integer(hit[i, ]))
  list(genes = g, samples = do.call(order, c(keys, list(method = "radix"))))
}

biov_oncoplot <- local({
  cache <- new.env(parent = emptyenv())
  function(n_genes = 25L) {
    key <- as.character(n_genes)
    hit0 <- cache[[key]]
    if (!is.null(hit0)) return(hit0)

    alt <- biov_brca_alterations()
    classes <- names(biov_variant_colours())
    classes <- classes[classes %in% unique(alt$class)]

    # Keep the n most recurrently altered genes.
    gene_freq <- sort(table(alt$gene), decreasing = TRUE)
    genes <- names(gene_freq)[seq_len(min(n_genes, length(gene_freq)))]
    alt <- alt[alt$gene %in% genes, , drop = FALSE]
    samples <- sort(unique(alt$sample))

    gi <- match(alt$gene, genes)
    si <- match(alt$sample, samples)
    ci <- match(alt$class, classes)
    m <- matrix(0L, length(genes), length(samples),
                dimnames = list(genes, samples))
    m[cbind(gi, si)] <- ci

    ord <- .memo_sort(m > 0L)
    m <- m[ord$genes, ord$samples, drop = FALSE]
    genes <- rownames(m)
    samples <- colnames(m)

    clin <- biov_brca_clinical()
    ci_row <- match(samples, clin$sample)
    ann <- function(name, values, palette) {
      f <- factor(values)
      list(name = name, levels = I(levels(f)),
           codes = ifelse(is.na(f), -1L, as.integer(f) - 1L),
           colors = I(unname(palette(nlevels(f)))))
    }

    out <- list(
      matrix = m,
      codes = as.integer(t(m)),          # row-major, 0 = no alteration
      genes = genes, samples = samples,
      nrows = nrow(m), ncols = ncol(m),
      classes = I(classes),
      classColors = I(unname(biov_variant_colours()[classes])),
      tmb = as.integer(colSums(m > 0L)),
      # unname() is load-bearing: rowSums() keeps the gene names, and Shiny
      # serializes a named vector as a JSON object rather than an array, which
      # the component would read as an empty column.
      freq = unname(round(100 * rowSums(m > 0L) / ncol(m), 1)),
      annotations = list(
        ann("Subtype", clin$subtype[ci_row], biov_categorical),
        ann("Stage", clin$stage[ci_row],
            function(k) biov_gradient()[round(seq(2, 6, length.out = k))])),
      altered = sum(colSums(m > 0L) > 0L),
      cohort = nrow(clin))
    cache[[key]] <- out
    out
  }
})

# ---- Visium spatial transcriptomics ---------------------------------------
# Real 10x Visium capture spots on a breast cancer section, with the low-res
# H&E the browser also fetches (see data/prep/prepare-visium.R). The server
# sends the selected gene's per-spot vector rather than shipping the whole
# panel and letting the client subset it: 3,798 rounded numbers is nothing, and
# it means the ggplot fill and the canvas fill come from one computation.
# Loaded once and shared by the spot map and the marker dot plot, so the two
# pages cannot end up with different cluster labels for the same tissue.
.visium_data <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      spots <- utils::read.csv(.data_path("visium_spots.csv"),
                               stringsAsFactors = FALSE)
      expr <- utils::read.csv(.data_path("visium_expr.csv"),
                              stringsAsFactors = FALSE, check.names = FALSE)
      meta <- jsonlite::fromJSON(.data_path("visium_meta.json"))
      genes <- expr$gene
      mat <- as.matrix(expr[, spots$barcode, drop = FALSE])
      rownames(mat) <- genes
      cl <- factor(spots$cluster,
                   levels = paste("Cluster", sort(unique(as.integer(
                     sub("^Cluster ", "", spots$cluster))))))
      cache <<- list(spots = spots, mat = mat, genes = sort(genes),
                     meta = meta, clusterLevels = levels(cl),
                     clusterColors = biov_categorical(nlevels(cl)))
    }
    cache
  }
})

biov_visium <- local({
  function(gene = NULL, colour_by = "cluster") {
    cache <- .visium_data()
    g <- if (is.null(gene) || !gene %in% rownames(cache$mat)) {
      "ERBB2"
    } else gene
    if (!g %in% rownames(cache$mat)) g <- rownames(cache$mat)[1]
    e <- unname(cache$mat[g, ])
    list(x = cache$spots$x, y = cache$spots$y,
         cluster = cache$spots$cluster, barcode = cache$spots$barcode,
         expr = e, gene = g, genes = I(cache$genes),
         colourBy = colour_by,
         clusterLevels = I(cache$clusterLevels),
         clusterColors = I(cache$clusterColors),
         image = cache$meta$image, imgWidth = cache$meta$imgWidth,
         imgHeight = cache$meta$imgHeight,
         spotDiameter = cache$meta$spotDiameter,
         nSpots = nrow(cache$spots), nGenes = length(cache$genes),
         exprMax = max(e), dataset = cache$meta$dataset)
  }
})

# ---- marker gene dot plot -------------------------------------------------
# The other half of the Visium story: the spot map says where the domains are,
# this says what defines them. Per gene and cluster, the share of spots with any
# detection (dot size) and the mean expression (dot colour), the two channels
# every scanpy/Seurat dot plot uses.
#
# Gene order is computed here, not in either renderer. Sorting genes by the
# cluster they best mark is what turns the grid into a readable diagonal, and
# two implementations breaking ties differently would produce two different
# figures from one dataset.
biov_dotplot <- local({
  cache <- new.env(parent = emptyenv())
  function(scale_by = "gene") {
    hit <- cache[[scale_by]]
    if (!is.null(hit)) return(hit)

    v <- .visium_data()
    mat <- v$mat
    cl <- factor(v$spots$cluster, levels = v$clusterLevels)
    clusters <- levels(cl)
    genes <- rownames(mat)

    idx <- lapply(clusters, function(k) which(cl == k))
    # Detection rate and mean expression, gene x cluster.
    pct <- vapply(idx, function(i) rowMeans(mat[, i, drop = FALSE] > 0) * 100,
                  numeric(length(genes)))
    avg <- vapply(idx, function(i) rowMeans(mat[, i, drop = FALSE]),
                  numeric(length(genes)))
    dimnames(pct) <- list(genes, clusters)
    dimnames(avg) <- list(genes, clusters)

    # Scaling across clusters within a gene is what makes a lowly expressed but
    # highly specific marker visible next to a ubiquitous one. Raw means keep
    # the absolute comparison instead; the page offers both.
    scaled <- if (identical(scale_by, "gene")) {
      rng <- t(apply(avg, 1, range))
      span <- rng[, 2] - rng[, 1]
      out <- (avg - rng[, 1]) / ifelse(span > 0, span, 1)
      out[span <= 0, ] <- 0
      out
    } else {
      avg
    }

    # Order genes by the cluster they mark most strongly, then within that by
    # how strongly. Ties resolve on the gene name so the order is total.
    best <- max.col(scaled, ties.method = "first")
    strength <- scaled[cbind(seq_along(genes), best)] -
      (rowSums(scaled) - scaled[cbind(seq_along(genes), best)]) /
        (ncol(scaled) - 1)
    ord <- order(best, -strength, genes)
    genes <- genes[ord]
    pct <- pct[ord, , drop = FALSE]
    avg <- avg[ord, , drop = FALSE]
    scaled <- scaled[ord, , drop = FALSE]

    # Long form, row-major over genes: one entry per dot.
    out <- list(
      gene = rep(genes, each = length(clusters)),
      cluster = rep(clusters, times = length(genes)),
      pct = as.numeric(t(pct)),
      value = as.numeric(t(scaled)),
      meanExpr = as.numeric(t(avg)),
      genes = genes, clusters = clusters,
      clusterColors = v$clusterColors,
      nGenes = length(genes), nClusters = length(clusters),
      nSpots = nrow(v$spots),
      spotsPerCluster = unname(as.integer(table(cl))),
      scaleBy = scale_by,
      valueLabel = if (identical(scale_by, "gene"))
        "scaled mean expression" else "mean log1p CP10K",
      dataset = v$meta$dataset
    )
    cache[[scale_by]] <- out
    out
  }
})

# ---- SBS96 mutational signatures ------------------------------------------
# The observed 96-context catalogue for a TCGA-BRCA cohort plus signatures
# extracted de novo from it (see data/prep/prepare-sbs96.R). These are NOT
# COSMIC reference signatures: COSMIC's terms forbid redistribution, so the
# page ships real spectra computed from open GDC data and names them
# accordingly. The resemblance to known processes is described in the copy, not
# asserted by the labels.
biov_sbs96 <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cat_df <- utils::read.csv(.data_path("sbs96_catalogue.csv"),
                                stringsAsFactors = FALSE)
      sig_df <- utils::read.csv(.data_path("sbs96_signatures.csv"),
                                stringsAsFactors = FALSE, check.names = FALSE)
      exp_df <- utils::read.csv(.data_path("sbs96_exposures.csv"),
                                stringsAsFactors = FALSE, check.names = FALSE)
      sig_names <- setdiff(names(sig_df), c("context", "trinuc", "sub"))
      cache <<- list(
        contexts = cat_df$context, trinuc = cat_df$trinuc, sub = cat_df$sub,
        counts = cat_df$count,
        signatures = sig_names,
        sig = sig_df[sig_names],
        exposures = exp_df,
        subLevels = names(biov_sbs_colours()),
        subColors = unname(biov_sbs_colours()),
        nTumours = nrow(exp_df),
        nSnv = sum(cat_df$count))
    }
    cache
  }
})

# One profile: either the observed cohort catalogue or a de novo signature.
# `which` is "catalogue" or a signature name.
biov_sbs96_profile <- function(which = "catalogue") {
  s <- biov_sbs96()
  if (identical(which, "catalogue")) {
    v <- as.numeric(s$counts)
    lab <- "SNVs"
  } else {
    if (!which %in% s$signatures) which <- s$signatures[1]
    v <- as.numeric(s$sig[[which]])
    lab <- "share of signature"
  }
  list(profile = which, value = v, contexts = s$contexts,
       trinuc = s$trinuc, sub = s$sub,
       subLevels = I(s$subLevels), subColors = I(s$subColors),
       choices = I(c("catalogue", s$signatures)),
       yLabel = lab, isCatalogue = identical(which, "catalogue"),
       total = sum(v), nTumours = s$nTumours, nSnv = s$nSnv,
       # Share of cohort mutations each signature accounts for.
       share = if (identical(which, "catalogue")) NA_real_ else
         round(100 * sum(s$exposures[[which]]) /
                 sum(as.matrix(s$exposures[s$signatures])), 1))
}

# ---- protein domain lollipop ----------------------------------------------
# Three real layers over one protein: mutation stems from the same cBioPortal
# TCGA-BRCA fetch the oncoplot uses, Pfam domain rectangles from InterPro, and
# PTM sites from UniProt (see data/prep/prepare-protein-tracks.R).
biov_protein_domains <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("protein_domains.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

biov_protein_ptm <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("protein_ptm.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

biov_lollipop_variants <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- utils::read.csv(.data_path("brca_lollipop.csv"),
                                stringsAsFactors = FALSE)
    }
    cache
  }
})

# Genes that have both a domain architecture and variants to show, most
# variant-rich first (drives the picker order).
biov_lollipop_genes <- function() {
  d <- biov_protein_domains()
  v <- biov_lollipop_variants()
  g <- intersect(unique(d$gene), unique(v$gene))
  tot <- stats::aggregate(count ~ gene, data = v[v$gene %in% g, ], FUN = sum)
  tot$gene[order(-tot$count)]
}

biov_lollipop <- local({
  cache <- new.env(parent = emptyenv())
  function(gene = "TP53", label_top_n = 12L) {
    hit <- cache[[gene]]
    if (!is.null(hit)) return(hit)

    dom <- biov_protein_domains()
    dom <- dom[dom$gene == gene, , drop = FALSE]
    if (!nrow(dom)) return(NULL)
    plen <- dom$length[1]
    uniprot <- dom$uniprot[1]

    v <- biov_lollipop_variants()
    v <- v[v$gene == gene & v$residue >= 1 & v$residue <= plen, , drop = FALSE]
    if (!nrow(v)) return(NULL)
    # Several distinct protein changes can hit one residue; keep them separate
    # so the classes stay honest, but order by position for the label stacking.
    v <- v[order(v$residue, -v$count), ]

    classes <- names(biov_variant_colours())
    classes <- classes[classes %in% unique(v$class)]

    ptm <- biov_protein_ptm()
    ptm <- ptm[ptm$gene == gene, , drop = FALSE]

    # Resolve the labelled stems ONCE so ggrepel and the canvas label the same
    # variants. 0-based for the client, 1-based for the ggplot side.
    top <- order(-v$count)[seq_len(min(label_top_n, nrow(v)))]
    top <- sort(top)

    out <- list(
      gene = gene, uniprot = uniprot, length = plen,
      position = v$residue, count = v$count,
      class = v$class, label = v$protein_change,
      labelRows = top,
      labelIndex = I(as.integer(top - 1L)),
      classes = I(classes),
      classColors = I(unname(biov_variant_colours()[classes])),
      domains = lapply(seq_len(nrow(dom)), function(i) {
        list(name = dom$name[i], start = dom$start[i], end = dom$end[i])
      }),
      domainNames = dom$name, domainStart = dom$start, domainEnd = dom$end,
      domainColors = I(biov_categorical(nrow(dom))),
      ptms = if (nrow(ptm)) lapply(seq_len(nrow(ptm)), function(i) {
        list(position = ptm$position[i], type = ptm$type[i])
      }) else list(),
      ptmPosition = ptm$position, ptmType = ptm$type,
      nVariants = nrow(v), nSamples = sum(v$count))
    cache[[gene]] <- out
    out
  }
})

# ---- AlphaFold predicted aligned error (PAE) ------------------------------
# The other half of an AlphaFold prediction. Entry (x, y) is the expected
# position error at residue x when the prediction is superposed on residue y.
# Low blocks along the diagonal are confidently-folded domains; a high
# off-diagonal block between two low blocks means both domains are individually
# confident but their relative orientation is not. That is the read the pLDDT
# profile on the protein page cannot give you.

# AlphaFold bumps its model version periodically (v4 is already retired and
# returns 404), so ask the API for the current file URLs instead of guessing.
# Falls back to the known v6 pattern when the API is unreachable, and caches
# per accession so an offline session pays the timeout once.
.af_urls <- local({
  cache <- new.env(parent = emptyenv())
  function(uniprot) {
    hit <- cache[[uniprot]]
    if (!is.null(hit)) return(hit)
    urls <- list(
      pdb = sprintf("https://alphafold.ebi.ac.uk/files/AF-%s-F1-model_v6.pdb", uniprot),
      pae = sprintf("https://alphafold.ebi.ac.uk/files/AF-%s-F1-predicted_aligned_error_v6.json",
                    uniprot))
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    old <- options(timeout = 15); on.exit(options(old), add = TRUE)
    # A miss here is expected and handled (we fall back to the v6 pattern), so
    # don't let download.file's warning reach the Shiny console.
    got <- suppressWarnings(try(utils::download.file(
      sprintf("https://alphafold.ebi.ac.uk/api/prediction/%s", uniprot),
      tmp, mode = "wb", quiet = TRUE), silent = TRUE))
    if (!inherits(got, "try-error") && file.exists(tmp) && file.info(tmp)$size > 0) {
      j <- try(jsonlite::fromJSON(tmp), silent = TRUE)
      if (!inherits(j, "try-error") && is.data.frame(j) && nrow(j) >= 1) {
        if (!is.null(j$pdbUrl)) urls$pdb <- j$pdbUrl[1]
        if (!is.null(j$paeDocUrl)) urls$pae <- j$paeDocUrl[1]
      }
    }
    cache[[uniprot]] <- urls
    urls
  }
})

.af_pae_path <- function(uniprot) {
  dir.create(.data_path("raw"), showWarnings = FALSE, recursive = TRUE)
  dest <- .data_path(file.path("raw", paste0("PAE-", uniprot, ".json")))
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    suppressWarnings(try(
      utils::download.file(.af_urls(uniprot)$pae, dest, mode = "wb", quiet = TRUE),
      silent = TRUE))
  }
  dest
}

# Block-mean a square matrix down to at most n_max per side. PIK3CA is 1,068
# residues, i.e. a 1.14M-cell matrix - far more than a screen can resolve or
# than is worth pushing over the websocket. Binning is reported in the stat bar
# rather than being applied silently, and both engines plot the binned matrix so
# they cannot disagree.
.bin_square <- function(m, n_max = 400L) {
  n <- nrow(m)
  if (n <= n_max) return(list(m = m, bin = 1L))
  f <- ceiling(n / n_max)
  idx <- rep(seq_len(ceiling(n / f)), each = f)[seq_len(n)]
  k <- as.numeric(table(idx))
  agg <- rowsum(m, idx) / k
  list(m = t(rowsum(t(agg), idx) / k), bin = as.integer(f))
}

biov_pae <- local({
  cache <- new.env(parent = emptyenv())
  function(uniprot = "P04637", n_max = 400L) {
    key <- paste0(uniprot, "_", n_max)
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)
    path <- .af_pae_path(uniprot)
    if (!file.exists(path) || file.info(path)$size == 0) return(NULL)
    j <- try(jsonlite::fromJSON(path), silent = TRUE)
    if (inherits(j, "try-error") || is.null(j$predicted_aligned_error)) return(NULL)
    full <- as.matrix(j$predicted_aligned_error[[1]])
    b <- .bin_square(full, n_max)
    # Round the matrix ITSELF, not just the feed copy, for two reasons: both
    # engines then plot bit-identical numbers, and the feed stays small. Shiny
    # serializes at 16 significant digits, so an unrounded binned mean costs ~15
    # bytes instead of ~4 (PIK3CA: 1.84 MB vs 0.47 MB). AlphaFold reports PAE as
    # integers anyway, so 0.1 A is already finer than the source.
    m <- round(b$m, 1)
    n <- nrow(m)
    # Label each binned row/col with the residue it centres on.
    labs <- as.character(round(seq(1, nrow(full), length.out = n)))
    out <- list(matrix = m, values = as.numeric(t(m)),
                nrows = n, ncols = n, rowLabels = labs, colLabels = labs,
                residues = nrow(full), bin = b$bin,
                maxPae = as.numeric(j$max_predicted_aligned_error[1]))
    cache[[key]] <- out
    out
  }
})

# ---- stacked violin -------------------------------------------------------
# The dot plot's two summary numbers per cell, replaced by the whole
# distribution. A gene detected in half a cluster at high level and silent in
# the other half has the same mean as one detected weakly everywhere; the dot
# plot cannot separate them and this can.
#
# Densities are estimated here, once, on a grid shared across every violin in a
# gene. Both engines then draw the same curves: kernel bandwidth is a claim
# about the data, and two renderers choosing it independently would disagree.
biov_violin <- local({
  cache <- new.env(parent = emptyenv())
  function(n_genes = 8L, grid_n = 64L) {
    key <- paste(n_genes, grid_n, sep = "|")
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)

    v <- .visium_data()
    d <- biov_dotplot("gene")
    m <- matrix(d$value, nrow = d$nGenes, byrow = TRUE,
                dimnames = list(d$genes, d$clusters))
    best <- max.col(m, ties.method = "first")

    # Genes are chosen by detection rate here, not by the fold change the dot
    # plot ranks on, and the difference is not cosmetic. A violin needs a
    # distribution: the panel's median gene is detected in 16% of spots, so its
    # density is 84% a spike at zero and the shape carries no information. The
    # dot plot is the right tool for those, because it encodes detection rate
    # as a separate channel. Here we keep the genes that are actually measured
    # across the section, which are also the recognisable ones.
    det <- rowMeans(v$mat > 0)
    ok <- names(det)[det >= 0.5]
    if (!length(ok)) ok <- names(sort(det, decreasing = TRUE))
    # Order the survivors by the cluster they mark, so the stack still reads
    # as a marker panel rather than an arbitrary list.
    ord <- order(best[match(ok, d$genes)], -det[ok])
    genes <- utils::head(ok[ord], n_genes)

    cl <- factor(v$spots$cluster, levels = v$clusterLevels)
    clusters <- levels(cl)
    idx <- lapply(clusters, function(k) which(cl == k))

    # One grid per gene, spanning that gene's own range across all clusters.
    # A single grid across genes would be "comparable" in principle but
    # unreadable in practice: one gene reaching 7.4 compresses every other row
    # into a flat line. Clusters stay comparable within a gene, which is the
    # comparison a marker panel is actually read for.
    grids <- list()
    rows <- list(); dens <- list(); meds <- numeric(0)
    for (g in genes) {
      rng <- range(v$mat[g, ])
      if (!is.finite(rng[1]) || rng[1] == rng[2]) rng <- c(rng[1], rng[1] + 1)
      grids[[length(grids) + 1]] <- seq(rng[1], rng[2], length.out = grid_n)
      for (k in seq_along(clusters)) {
        x <- v$mat[g, idx[[k]]]
        # A cluster whose spots all read the same value has no density to
        # estimate; a flat row is the honest rendering, not an error.
        y <- if (length(unique(x)) < 2L) rep(0, grid_n) else
          stats::density(x, from = rng[1], to = rng[2], n = grid_n)$y
        rows[[length(rows) + 1]] <- data.frame(
          feature = g, cluster = clusters[k], stringsAsFactors = FALSE)
        dens[[length(dens) + 1]] <- y
        meds <- c(meds, stats::median(x))
      }
    }
    key_df <- do.call(rbind, rows)
    grid_m <- do.call(rbind, grids)

    out <- list(
      feature = key_df$feature, cluster = key_df$cluster,
      # Row-major, violins x grid_n. The transpose is load-bearing: rbind()
      # stacks the densities as rows, and as.numeric() on a matrix flattens
      # column-major, which would interleave every violin with its neighbours.
      density = as.numeric(t(do.call(rbind, dens))),
      # The first gene's grid doubles as the shared fallback; grids carries one
      # row per gene, row-major, same transpose reasoning as density.
      grid = grid_m[1, ], grids = as.numeric(t(grid_m)), gridN = grid_n,
      median = unname(meds),
      genes = genes, clusters = clusters,
      clusterColors = v$clusterColors,
      nGenes = length(genes), nClusters = length(clusters),
      nSpots = nrow(v$spots),
      spotsPerCluster = unname(as.integer(table(cl))),
      dataset = v$meta$dataset
    )
    cache[[key]] <- out
    out
  }
})

# ---- driver co-occurrence (UpSet) -----------------------------------------
# The oncoplot shows every sample as its own column, which is where mutual
# exclusivity is easy to assert and hard to actually read. Collapsing the same
# alterations into exclusive intersections makes the claim checkable: if two
# drivers avoid each other, their shared bar is small next to their solo bars.
#
# "Exclusive" is load-bearing. A sample sits in exactly one column, the one
# naming precisely the genes it carries, so the columns sum to the number of
# altered samples instead of double-counting.
biov_upset <- local({
  cache <- new.env(parent = emptyenv())
  function(n_genes = 8L, max_n = 20L) {
    key <- paste(n_genes, max_n, sep = "|")
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)

    a <- biov_brca_alterations()
    freq <- sort(table(a$gene), decreasing = TRUE)
    genes <- names(freq)[seq_len(min(n_genes, length(freq)))]
    samples <- sort(unique(a$sample))

    m <- matrix(FALSE, nrow = length(samples), ncol = length(genes),
                dimnames = list(samples, genes))
    sub <- a[a$gene %in% genes, , drop = FALSE]
    m[cbind(sub$sample, sub$gene)] <- TRUE

    keys <- apply(m, 1L, function(r) paste0(as.integer(r), collapse = ""))
    none <- sum(keys == strrep("0", length(genes)))
    keys <- keys[keys != strrep("0", length(genes))]
    tab <- sort(table(keys), decreasing = TRUE)
    shown <- utils::head(tab, max_n)

    memb <- t(vapply(names(shown), function(k) {
      as.integer(strsplit(k, "", fixed = TRUE)[[1]]) == 1L
    }, logical(length(genes))))
    dimnames(memb) <- NULL

    # Pairwise co-occurrence against independence, for the page's stat bar.
    # This is the number the figure is really about.
    pair <- NULL
    if (length(genes) >= 2) {
      combos <- utils::combn(seq_along(genes), 2)
      obs <- apply(combos, 2, function(ij) sum(m[, ij[1]] & m[, ij[2]]))
      exp <- apply(combos, 2, function(ij)
        sum(m[, ij[1]]) * sum(m[, ij[2]]) / nrow(m))
      p <- apply(combos, 2, function(ij)
        stats::fisher.test(table(factor(m[, ij[1]], c(FALSE, TRUE)),
                                 factor(m[, ij[2]], c(FALSE, TRUE))))$p.value)
      ord <- order(p)
      pair <- list(
        a = genes[combos[1, ord]], b = genes[combos[2, ord]],
        observed = unname(obs[ord]), expected = round(unname(exp[ord]), 1),
        p = signif(unname(p[ord]), 3)
      )
    }

    out <- list(
      size = unname(as.integer(shown)),
      membership = as.integer(t(memb)),   # row-major, intersections x genes
      sets = genes,
      setSizes = unname(as.integer(colSums(m))),
      total = nrow(m),
      altered = nrow(m) - none,
      unaltered = none,
      nIntersections = length(tab),
      shown = length(shown),
      pairs = pair
    )
    cache[[key]] <- out
    out
  }
})

# ---- overall survival (Kaplan-Meier) --------------------------------------
# The same TCGA-BRCA cohort the oncoplot reads, now with its follow-up. All the
# estimation happens here, once: survival probabilities, Greenwood bands,
# censoring times, at-risk counts, medians and the log-rank test. Both engines
# get the same numbers, so the curves cannot step in different places.

# Strata for the gene view come from the alteration table the oncoplot uses, so
# "altered" means exactly what that page shows it to mean.
biov_survival_genes <- local({
  cache <- NULL
  function(n = 12L) {
    if (is.null(cache)) {
      a <- biov_brca_alterations()
      cache <<- names(sort(table(a$gene), decreasing = TRUE))
    }
    utils::head(cache, n)
  }
})

.survival_strata <- function(group_by, gene) {
  cl <- biov_brca_clinical()
  keep <- !is.na(cl$os_months) & !is.na(cl$os_event) & cl$os_months > 0
  cl <- cl[keep, , drop = FALSE]

  if (identical(group_by, "gene")) {
    altered <- unique(biov_brca_alterations()$sample[
      biov_brca_alterations()$gene == gene])
    lab <- ifelse(cl$sample %in% altered,
                  sprintf("%s altered", gene),
                  sprintf("%s wild-type", gene))
    levs <- c(sprintf("%s altered", gene), sprintf("%s wild-type", gene))
    # Altered in the warning red, wild-type in the calm teal.
    cols <- c("#C63F3E", "#0E7175")
  } else if (identical(group_by, "age")) {
    # Conventional TCGA cut points rather than a median split, which would move
    # with the cohort and make the strata mean something different each time.
    lab <- cut(cl$age, breaks = c(-Inf, 50, 65, Inf),
               labels = c("under 50", "50 to 64", "65 and over"))
    lab <- as.character(lab)
    levs <- c("under 50", "50 to 64", "65 and over")
    cols <- biov_categorical(3)
  } else if (identical(group_by, "stage")) {
    lab <- cl$stage
    levs <- c("I", "II", "III", "IV")
    cols <- c("#0E7175", "#708C69", "#E4A25B", "#C63F3E")
  } else {
    lab <- cl$subtype
    levs <- c("LumA", "LumB", "Her2", "Basal", "Normal")
    cols <- biov_categorical(5)
  }

  ok <- !is.na(lab) & lab %in% levs
  levs_present <- levs[levs %in% unique(lab[ok])]
  list(df = data.frame(time = cl$os_months[ok], event = cl$os_event[ok],
                       group = factor(lab[ok], levels = levs_present),
                       stringsAsFactors = FALSE),
       levels = levs_present,
       colors = cols[match(levs_present, levs)])
}

biov_survival <- local({
  cache <- new.env(parent = emptyenv())
  function(group_by = "subtype", gene = "TP53") {
    key <- paste(group_by, gene, sep = "|")
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)

    if (!requireNamespace("survival", quietly = TRUE)) {
      stop("the survival package is required for the Kaplan-Meier page")
    }
    s <- .survival_strata(group_by, gene)
    d <- s$df
    fit <- survival::survfit(survival::Surv(time, event) ~ group, data = d)

    n <- length(fit$time)
    grp <- if (is.null(fit$strata)) rep(s$levels[1], n) else
      rep(sub("^[^=]*=", "", names(fit$strata)), times = as.integer(fit$strata))

    # survfit does not store the origin, and without it every curve would begin
    # partway down its first drop.
    starts <- data.frame(time = 0, surv = 1, lower = 1, upper = 1,
                         group = s$levels, stringsAsFactors = FALSE)
    curves <- rbind(starts, data.frame(
      time = as.numeric(fit$time), surv = as.numeric(fit$surv),
      lower = as.numeric(fit$lower), upper = as.numeric(fit$upper),
      group = grp, stringsAsFactors = FALSE))
    curves$group <- factor(curves$group, levels = s$levels)
    curves <- curves[order(curves$group, curves$time), , drop = FALSE]
    # survfit's CI is NA until the first event; carry the origin's 1 forward so
    # the band has somewhere to start.
    curves$lower[is.na(curves$lower)] <- 1
    curves$upper[is.na(curves$upper)] <- 1

    cens <- fit$n.censor > 0
    censor <- data.frame(time = as.numeric(fit$time[cens]),
                         surv = as.numeric(fit$surv[cens]),
                         group = grp[cens], stringsAsFactors = FALSE)

    # A round grid the axis can also use as its ticks.
    tmax <- max(d$time)
    risk_times <- seq(0, floor(tmax / 60) * 60, by = 60)
    if (length(risk_times) < 2) risk_times <- c(0, round(tmax))
    risk <- t(vapply(s$levels, function(g) {
      i <- which(grp == g)
      tt <- fit$time[i]; nr <- fit$n.risk[i]
      vapply(risk_times, function(t) {
        j <- which(tt >= t)
        if (length(j) == 0L) 0L else as.integer(nr[j[1]])
      }, integer(1))
    }, integer(length(risk_times))))

    # Median survival per stratum: the first time the curve reaches 0.5. NA
    # when it never does, which is the honest answer, not "not reached yet".
    medians <- vapply(s$levels, function(g) {
      i <- which(curves$group == g)
      j <- which(curves$surv[i] <= 0.5)
      if (length(j) == 0L) NA_real_ else curves$time[i][j[1]]
    }, numeric(1))

    p <- NA_real_
    if (length(s$levels) > 1) {
      sd <- survival::survdiff(survival::Surv(time, event) ~ group, data = d)
      p <- stats::pchisq(sd$chisq, df = length(sd$n) - 1, lower.tail = FALSE)
    }

    out <- list(
      time = curves$time, surv = curves$surv,
      lower = curves$lower, upper = curves$upper,
      group = as.character(curves$group),
      censorTime = censor$time, censorSurv = censor$surv,
      censorGroup = censor$group,
      levels = s$levels, colors = s$colors,
      riskTimes = risk_times,
      riskCounts = as.integer(t(risk)),   # row-major, groups x times
      medians = unname(medians),
      counts = unname(as.integer(table(d$group))),
      events = unname(as.integer(tapply(d$event, d$group, sum))),
      n = nrow(d), nEvents = sum(d$event == 1),
      p = p, pLabel = .p_label(p),
      groupBy = group_by, gene = gene
    )
    cache[[key]] <- out
    out
  }
})

.p_label <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("log-rank p < 0.001")
  sprintf("log-rank p = %s", format(round(p, 3), nsmall = 3))
}

# ---- PCA explorer ---------------------------------------------------------
# One decomposition, three views. The scores, the scree bars and the loadings
# all read from this single fit, so the panels cannot disagree about which
# component is which or how much variance it carries.
#
# Genes are centred but not scaled. The values are VST-transformed counts,
# already variance-stabilised, and scaling to unit variance would hand a
# low-expression gene the same say as a strong marker.
biov_pca <- local({
  cache <- NULL
  function(n_genes = 2000L) {
    key <- as.integer(n_genes)
    if (!is.null(cache) && identical(cache$n_genes, key)) return(cache)
    m <- biov_expression()
    md <- biov_metadata()
    v <- apply(m, 1, stats::var)
    top <- utils::head(order(v, decreasing = TRUE), key)
    sub <- m[top, , drop = FALSE]
    # prcomp wants samples in rows, so the matrix is transposed here and the
    # loadings come back gene-wise in `rotation`.
    p <- stats::prcomp(t(sub), center = TRUE, scale. = FALSE)
    ve <- p$sdev^2 / sum(p$sdev^2)
    # Centring 40 samples leaves 39 real components; the rest are numerical
    # dust and would draw as spurious zero-height bars.
    keep <- seq_len(min(ncol(p$x), nrow(p$x) - 1L))
    cache <<- list(
      n_genes = key,
      scores = p$x[, keep, drop = FALSE],
      loadings = p$rotation[, keep, drop = FALSE],
      var_exp = ve[keep],
      samples = rownames(p$x),
      groups = md$group[match(rownames(p$x), md$sample)],
      genes = rownames(sub),
      npc = length(keep)
    )
    cache
  }
})

# Sample scores on a chosen pair of components, in the embedding contract.
biov_pca_scores <- function(pc_x = 1L, pc_y = 2L) {
  p <- biov_pca()
  i <- max(1L, min(as.integer(pc_x), p$npc))
  j <- max(1L, min(as.integer(pc_y), p$npc))
  list(
    x = unname(p$scores[, i]), y = unname(p$scores[, j]),
    color = p$groups, label = p$samples,
    pcX = i, pcY = j,
    xLabel = sprintf("PC%d (%.1f%%)", i, 100 * p$var_exp[i]),
    yLabel = sprintf("PC%d (%.1f%%)", j, 100 * p$var_exp[j])
  )
}

# Variance explained per component, in the profile contract. No grouping: a
# scree plot has one series and a header band would only add ink.
biov_pca_scree <- function(n = 10L) {
  p <- biov_pca()
  k <- max(1L, min(as.integer(n), p$npc))
  idx <- seq_len(k)
  list(
    value = 100 * unname(p$var_exp[idx]),
    label = paste0("PC", idx),
    cumulative = 100 * cumsum(unname(p$var_exp[idx])),
    npc = p$npc
  )
}

# The genes driving one component, in the profile contract. Ranked by absolute
# loading, then drawn in signed order so the two directions form contiguous
# runs the header band can label: a gene loading negatively is pulling samples
# the opposite way, which is the half of the story a bare magnitude hides.
biov_pca_loadings <- function(pc = 1L, n = 20L) {
  p <- biov_pca()
  i <- max(1L, min(as.integer(pc), p$npc))
  k <- max(2L, min(as.integer(n), nrow(p$loadings)))
  l <- p$loadings[, i]
  top <- utils::head(order(abs(l), decreasing = TRUE), k)
  ord <- top[order(l[top], decreasing = TRUE)]
  vals <- unname(l[ord])
  list(
    value = vals,
    label = p$genes[ord],
    group = ifelse(vals >= 0, "positive", "negative"),
    # Pinned so the header keeps both blocks even when one direction is empty.
    groups = c("positive", "negative"),
    pc = i,
    varExp = 100 * p$var_exp[i]
  )
}
