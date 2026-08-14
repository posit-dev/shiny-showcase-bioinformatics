# ggplot2 (classic) renderings. Each returns a base64 PNG data-URI string that
# the React client drops into an <img>. Rendering to a data-URI (rather than
# ImageOutput) keeps the whole UI React-owned and behaves identically in R.
#
# ragg::agg_png gives crisp anti-aliased text on a transparent background so the
# plot blends into the dark app panel.

# Render a ggplot to a PNG data-URI at the given pixel size.
gg_data_uri <- function(plot, width = 900, height = 620, res = 108) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ragg::agg_png(tmp, width = width, height = height, units = "px",
                res = res, background = "transparent")
  print(plot)
  grDevices::dev.off()
  base64enc::dataURI(file = tmp, mime = "image/png")
}

# ---- volcano --------------------------------------------------------------
plot_volcano_gg <- function(fc = 1, p = 0.05, label_top_n = 12) {
  v <- biov_volcano(fc = fc, p = p)
  df <- data.frame(gene = v$gene, logFC = v$logFC, y = v$neg_log10_p,
                   status = v$status, stringsAsFactors = FALSE)
  # label the most significant up/down genes
  df$label <- NA_character_
  cand <- which(df$status != "NS")
  if (length(cand) && label_top_n > 0) {
    top <- cand[order(df$y[cand], decreasing = TRUE)][seq_len(min(label_top_n, length(cand)))]
    df$label[top] <- df$gene[top]
  }
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = logFC, y = y, colour = status)) +
    ggplot2::geom_point(alpha = 0.75, size = 1.5) +
    ggplot2::geom_vline(xintercept = c(-fc, fc), linetype = "dashed", colour = "#5c6b85") +
    ggplot2::geom_hline(yintercept = -log10(p), linetype = "dashed", colour = "#5c6b85") +
    ggplot2::scale_colour_manual(values = biov_status_colours(), drop = FALSE) +
    ggplot2::labs(x = expression(log[2] ~ fold ~ change),
                  y = expression(-log[10] ~ p), colour = NULL) +
    biov_theme()
  lab <- df[!is.na(df$label), , drop = FALSE]
  if (nrow(lab)) {
    p1 <- p1 + ggrepel::geom_text_repel(
      data = lab, ggplot2::aes(label = label), size = 3.3,
      max.overlaps = Inf, colour = "#233038", show.legend = FALSE)
  }
  gg_data_uri(p1)
}

# ---- heatmap (geom_tile) --------------------------------------------------
plot_heatmap_gg <- function(n_genes = 40, z_score = TRUE) {
  h <- biov_heatmap(n_genes = n_genes)
  m <- h$matrix
  if (z_score) {
    m <- t(scale(t(m)))                 # per-gene z-score
    m[is.na(m)] <- 0
    fill_lab <- "z-score"
  } else {
    fill_lab <- "expr"
  }
  long <- data.frame(
    gene = factor(rep(rownames(m), times = ncol(m)), levels = rev(rownames(m))),
    sample = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)),
    value = as.numeric(m)
  )
  ramp <- if (z_score) biov_diverging() else biov_gradient()
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = sample, y = gene, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = ramp, name = fill_lab) +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
                   axis.text.y = ggplot2::element_text(size = 6),
                   legend.position = "right")
  gg_data_uri(p1, width = 900, height = 700)
}

# ---- treemap (hand-rolled slice-and-dice layout, no extra deps) -----------
# We avoid a treemapify dependency by laying out one row of gene blocks sized by
# total mutation count, then slicing each gene block vertically into its
# variants. Rendered with geom_rect + geom_text.
plot_treemap_gg <- function() {
  tm <- biov_treemap()
  df <- tm$df
  genes <- tm$gene_order
  gene_tot <- tapply(df$count, df$gene, sum)[genes]
  total <- sum(gene_tot)
  cols <- biov_categorical(length(genes)); names(cols) <- genes

  rects <- list(); labels <- list()
  x0 <- 0
  for (g in genes) {
    w <- gene_tot[[g]] / total
    x1 <- x0 + w
    vs <- df[df$gene == g, , drop = FALSE]
    vs <- vs[order(-vs$count), ]
    y0 <- 0; gsum <- sum(vs$count)
    for (i in seq_len(nrow(vs))) {
      hgt <- vs$count[i] / gsum
      y1 <- y0 + hgt
      rects[[length(rects) + 1]] <- data.frame(
        xmin = x0, xmax = x1, ymin = y0, ymax = y1, gene = g)
      # only label reasonably large tiles
      if (w > 0.06 && hgt > 0.12) {
        labels[[length(labels) + 1]] <- data.frame(
          x = (x0 + x1) / 2, y = (y0 + y1) / 2,
          lab = paste0(vs$variant[i], "\n(", vs$count[i], ")"))
      }
      y0 <- y1
    }
    # gene name across the top of its column
    labels[[length(labels) + 1]] <- data.frame(
      x = (x0 + x1) / 2, y = 1.03, lab = g)
    x0 <- x1
  }
  rects <- do.call(rbind, rects)
  labs <- do.call(rbind, labels)
  p1 <- ggplot2::ggplot(rects) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = gene),
                       colour = "#FBF7EF", linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::geom_text(data = labs, ggplot2::aes(x = x, y = y, label = lab),
                       size = 2.9, colour = "#FBF7EF", lineheight = 0.9) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0, 1.06)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
  gg_data_uri(p1, width = 900, height = 620)
}

# ---- clustermap (classic: base heatmap() with real dendrograms) ------------
# base::heatmap draws hierarchical row/col dendrograms - the honest classic
# counterpart to the plotomics clustermap. Captured to PNG via ragg.
plot_clustermap_gg <- function(n_genes = 40, z_score = TRUE) {
  h <- biov_heatmap(n_genes = n_genes)
  m <- h$matrix
  scale_arg <- if (z_score) "row" else "none"
  ramp <- grDevices::colorRampPalette(biov_diverging())(64)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ragg::agg_png(tmp, width = 900, height = 760, units = "px", res = 108,
                background = "transparent")
  op <- graphics::par(col.axis = "#233038", col.lab = "#233038", col.main = "#233038",
                      fg = "#6E7B72", cex.axis = 0.5)
  stats::heatmap(m, scale = scale_arg, col = ramp,
                 margins = c(7, 6), cexRow = 0.5, cexCol = 0.55,
                 col.axis = "#233038")
  graphics::par(op)
  grDevices::dev.off()
  base64enc::dataURI(file = tmp, mime = "image/png")
}

# ---- Hi-C (classic: geom_raster of the contact matrix, log scale) ----------
plot_hic_gg <- function() {
  hic <- biov_hic()
  n <- hic$n
  M <- hic$matrix
  long <- data.frame(
    i = rep(seq_len(n), times = n),
    j = rep(seq_len(n), each = n),
    v = log1p(as.numeric(M))
  )
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = i, y = j, fill = v)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colours = biov_gradient(), name = "log contacts") +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal() +
    ggplot2::labs(x = sprintf("%s bin (%dkb)", hic$chrom, hic$bin_size %/% 1000L),
                  y = NULL) +
    biov_theme(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
  gg_data_uri(p1, width = 760, height = 700)
}

# ---- Tahoe perturbation coverage (classic: geom_tile) ----------------------
plot_tahoe_gg <- function() {
  t <- biov_tahoe()
  m <- t$matrix
  long <- data.frame(
    drug = factor(rep(rownames(m), times = ncol(m)), levels = rev(rownames(m))),
    cell = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)),
    value = as.numeric(m)
  )
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = cell, y = drug, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = biov_gradient(), name = "log10 cells") +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5.5),
                   axis.text.y = ggplot2::element_text(size = 5.5),
                   legend.position = "right")
  gg_data_uri(p1, width = 940, height = 720)
}

# ---- network (classic: igraph layout drawn with ggplot geom_segment) -------
plot_network_gg <- function() {
  net <- biov_network()
  t0 <- Sys.time()
  nodes <- data.frame(id = net$id, x = net$x, y = net$y, size = net$size,
                      group = net$group, stringsAsFactors = FALSE)
  pos <- setNames(seq_along(net$id), net$id)
  edges <- data.frame(
    x = net$x[pos[net$source]], y = net$y[pos[net$source]],
    xend = net$x[pos[net$target]], yend = net$y[pos[net$target]]
  )
  cols <- biov_categorical(length(unique(nodes$group)))
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "#D8CFBE", linewidth = 0.12, alpha = 0.5) +
    ggplot2::geom_point(data = nodes,
      ggplot2::aes(x = x, y = y, colour = group, size = size), alpha = 0.9) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
  uri <- gg_data_uri(p1, width = 900, height = 720)
  attr(uri, "nodes") <- net$n_nodes
  attr(uri, "edges") <- net$n_edges
  attr(uri, "secs") <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  uri
}

# ---- IGV classic counterpart: a variant "needle / lollipop" plot -----------
# The same somatic variants shown as recurrence vs genomic position for one gene
# - the canonical static genomics view that pairs with the live igv.js browser.
plot_igv_needle_gg <- function(gene = "TP53") {
  mut <- biov_mutations()
  sub <- mut[mut$gene == gene, , drop = FALSE]
  if (!nrow(sub)) sub <- mut
  sub <- sub[order(sub$pos), ]
  p1 <- ggplot2::ggplot(sub, ggplot2::aes(x = pos, y = count)) +
    ggplot2::geom_segment(ggplot2::aes(xend = pos, yend = 0), colour = "#5c6b85", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(size = count), colour = "#ff6b6b", alpha = 0.9) +
    ggrepel::geom_text_repel(ggplot2::aes(label = protein_change), size = 3.2,
                             colour = "#233038", max.overlaps = Inf) +
    ggplot2::scale_size_area(max_size = 9, guide = "none") +
    ggplot2::labs(x = sprintf("%s genomic position (%s, hg19)", gene, sub$chrom[1]),
                  y = "recurrence (samples)") +
    biov_theme(base_size = 13)
  gg_data_uri(p1, width = 900, height = 560)
}

# ---- Protein classic counterpart: per-residue AlphaFold pLDDT profile ------
# Parse CA-atom B-factors (pLDDT) from the cached AlphaFold PDB and plot the
# confidence profile, marking the variant residue. Pairs with the 3Dmol view.
.af_pdb_path <- function(uniprot) {
  dir.create(.data_path("raw"), showWarnings = FALSE, recursive = TRUE)
  dest <- .data_path(file.path("raw", paste0("AF-", uniprot, ".pdb")))
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    # .af_urls() (R/data.R) resolves the current model version from the
    # AlphaFold API rather than hardcoding one, which has already drifted once.
    suppressWarnings(try(
      utils::download.file(.af_urls(uniprot)$pdb, dest, mode = "wb", quiet = TRUE),
      silent = TRUE))
  }
  dest
}

plot_protein_plddt_gg <- function(uniprot = "P04637", residue = NULL) {
  path <- .af_pdb_path(uniprot)
  if (!file.exists(path) || file.info(path)$size == 0) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = paste("Could not fetch", uniprot),
                        colour = "#233038") + ggplot2::theme_void()))
  }
  lines <- readLines(path, warn = FALSE)
  ca <- lines[substr(lines, 1, 4) == "ATOM" & substr(lines, 13, 16) == " CA "]
  resi <- as.integer(substr(ca, 23, 26))
  plddt <- as.numeric(substr(ca, 61, 66))
  df <- data.frame(resi = resi, plddt = plddt)
  band <- cut(df$plddt, c(-Inf, 50, 70, 90, Inf),
              labels = c("Very low", "Low", "Confident", "Very high"))
  df$band <- band
  band_cols <- c("Very low" = "#FF7D45", "Low" = "#FFDB13",
                 "Confident" = "#65CBF3", "Very high" = "#0053D6")
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = resi, y = plddt)) +
    ggplot2::geom_line(colour = "#93a1b8", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(colour = band), size = 0.9) +
    ggplot2::scale_colour_manual(values = band_cols, name = "pLDDT", drop = FALSE) +
    ggplot2::ylim(0, 100) +
    ggplot2::labs(x = "residue", y = "pLDDT confidence",
                  title = paste("AlphaFold", uniprot)) +
    biov_theme(base_size = 13)
  if (!is.null(residue)) {
    p1 <- p1 + ggplot2::geom_vline(xintercept = residue, colour = "#ff6b6b",
                                   linetype = "dashed", linewidth = 0.6)
  }
  gg_data_uri(p1, width = 900, height = 560)
}

# ---- oncoplot / OncoPrint -------------------------------------------------
# Five aligned panels: the alteration grid, a per-sample burden barplot above,
# a per-gene frequency barplot to the right, and two clinical strips below.
# patchwork is used for exactly one reason: it merges gtable widths per column
# and heights per row, so the burden bars land above their own sample columns
# without us needing to know how wide the gene-label axis happened to render.
# Nothing here re-derives the ordering; it plots biov_oncoplot()'s order, which
# is the same order the React engine gets.
plot_oncoplot_gg <- function(n_genes = 25L) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = "patchwork is required",
                        colour = "#233038") + ggplot2::theme_void()))
  }
  o <- biov_oncoplot(n_genes = n_genes)
  # Top gene at the top of the panel, so reverse for the discrete y scale.
  glev <- rev(o$genes)
  cls <- as.character(o$classes)
  long <- data.frame(
    gene = factor(rep(o$genes, times = o$ncols), levels = glev),
    sample = factor(rep(o$samples, each = o$nrows), levels = o$samples),
    code = as.integer(o$matrix))
  long$cls <- factor(ifelse(long$code == 0L, NA_character_,
                            cls[pmax(long$code, 1L)]), levels = cls)
  cls_cols <- stats::setNames(as.character(o$classColors), cls)

  bare_x <- ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank())

  p_main <- ggplot2::ggplot(long, ggplot2::aes(sample, gene)) +
    ggplot2::geom_tile(fill = "#EFE9DC", width = 0.94, height = 0.82) +
    ggplot2::geom_tile(data = long[!is.na(long$cls), ],
                       ggplot2::aes(fill = cls), width = 0.94, height = 0.82) +
    ggplot2::scale_fill_manual(values = cls_cols, drop = FALSE, name = NULL,
                               na.translate = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 10) + bare_x +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7,
                                                       face = "italic"))

  p_tmb <- ggplot2::ggplot(
      data.frame(sample = factor(o$samples, levels = o$samples), v = o$tmb),
      ggplot2::aes(sample, v)) +
    ggplot2::geom_col(fill = "#0E7175", width = 0.94) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "alterations") +
    biov_theme(base_size = 9) + bare_x

  p_freq <- ggplot2::ggplot(
      data.frame(gene = factor(o$genes, levels = glev), v = o$freq),
      ggplot2::aes(v, gene)) +
    ggplot2::geom_col(fill = "#ED773C", width = 0.82) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", v)),
                       hjust = -0.15, size = 2.4, colour = "#233038") +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.28))) +
    ggplot2::labs(x = "% altered", y = NULL) +
    biov_theme(base_size = 9) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())

  strip <- function(a) {
    d <- data.frame(
      sample = factor(o$samples, levels = o$samples),
      lv = factor(ifelse(a$codes < 0L, NA_character_,
                         as.character(a$levels)[a$codes + 1L]),
                  levels = as.character(a$levels)),
      row = a$name)
    ggplot2::ggplot(d, ggplot2::aes(sample, row, fill = lv)) +
      ggplot2::geom_tile(width = 0.94) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(as.character(a$colors),
                                 as.character(a$levels)),
        name = a$name, na.value = "#EFE9DC") +
      ggplot2::labs(x = NULL, y = NULL) +
      biov_theme(base_size = 9) + bare_x +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7))
  }
  strips <- lapply(o$annotations, strip)

  # A = burden, B = grid, C = frequency, D/E = clinical strips. The "#" cells
  # keep the right-hand column empty on every row except the grid's.
  pw <- p_tmb + p_main + p_freq + strips[[1]] + strips[[2]] +
    patchwork::plot_layout(
      design = "A#\nBC\nD#\nE#",
      widths = c(5.2, 1), heights = c(1.15, 6.4, 0.32, 0.32),
      guides = "collect") +
    # Without this patchwork paints an opaque background and the PNG stops
    # blending into the panel the way every other classic view does.
    patchwork::plot_annotation(theme = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent",
                                              colour = NA))) &
    ggplot2::theme(legend.position = "bottom",
                   legend.key.size = ggplot2::unit(9, "pt"),
                   legend.text = ggplot2::element_text(size = 7),
                   legend.title = ggplot2::element_text(size = 7))
  gg_data_uri(pw, width = 1020, height = 780)
}

# ---- Visium spatial transcriptomics ----------------------------------------
# annotation_raster reads the SAME PNG the browser fetches from www/, so there
# is one image with two readers rather than two copies that can diverge.
#
# Two details worth keeping: the raster must be added as the FIRST layer (it is
# an ordinary layer, not a below-everything annotation), and y is negated in the
# data rather than using scale_y_reverse(), because a reversed scale flips the
# raster's ymin/ymax mapping and silently draws the tissue upside down under
# correctly-placed spots.
plot_visium_gg <- function(gene = NULL, colour_by = "cluster") {
  v <- biov_visium(gene, colour_by)
  img_path <- file.path("www", v$image)
  if (!requireNamespace("png", quietly = TRUE) || !file.exists(img_path)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = "tissue image unavailable",
                        colour = "#233038") + ggplot2::theme_void(),
      width = 760, height = 740))
  }
  ras <- grDevices::as.raster(png::readPNG(img_path))
  W <- v$imgWidth; H <- v$imgHeight
  d <- data.frame(x = v$x, yy = -v$y,
                  cluster = factor(v$cluster, levels = as.character(v$clusterLevels)),
                  expr = v$expr)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = yy)) +
    ggplot2::annotation_raster(ras, xmin = 0, xmax = W, ymin = -H, ymax = 0,
                               interpolate = TRUE)
  if (identical(colour_by, "gene")) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(colour = expr), size = 1.15,
                          alpha = 0.85) +
      ggplot2::scale_colour_gradientn(colours = biov_gradient(),
                                      name = sprintf("%s\nlog1p CP10K", v$gene),
                                      limits = c(0, max(v$exprMax, 1e-6)))
  } else {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(colour = cluster), size = 1.15,
                          alpha = 0.85) +
      ggplot2::scale_colour_manual(
        values = stats::setNames(as.character(v$clusterColors),
                                 as.character(v$clusterLevels)),
        name = NULL) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(size = 3)))
  }
  p <- p +
    ggplot2::coord_fixed(xlim = c(0, W), ylim = c(-H, 0), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.position = "right",
      legend.text = ggplot2::element_text(colour = "#233038", size = 8),
      legend.title = ggplot2::element_text(colour = "#233038", size = 9),
      plot.margin = ggplot2::margin(6, 6, 6, 6))
  gg_data_uri(p, width = 780, height = 700)
}

# ---- SBS96 mutational signature profile -----------------------------------
# The six substitution blocks are drawn as a geom_rect banner over a continuous
# x rather than with facet_grid. Faceting would give six panels with their own
# strips and inter-panel gaps; the published layout is one continuous axis under
# one banner, and hand-rolling the banner is the same move plot_treemap_gg makes
# to avoid grob surgery.
plot_sbs96_gg <- function(which = "catalogue") {
  s <- biov_sbs96_profile(which)
  df <- data.frame(i = seq_along(s$value), v = s$value,
                   sub = factor(s$sub, levels = as.character(s$subLevels)),
                   tri = s$trinuc)
  top <- max(df$v)
  hdr <- do.call(rbind, lapply(as.character(s$subLevels), function(k) {
    ii <- range(which(s$sub == k))
    data.frame(xmin = ii[1] - 0.5, xmax = ii[2] + 0.5, x = mean(ii), sub = k)
  }))
  hdr$sub <- factor(hdr$sub, levels = as.character(s$subLevels))
  # Black and pale-grey blocks need opposite label colours to stay readable.
  hdr$ink <- c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#233038", "#233038", "#233038")
  cols <- stats::setNames(as.character(s$subColors), as.character(s$subLevels))

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = hdr,
                       ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = top * 1.08, ymax = top * 1.19,
                                    fill = sub)) +
    ggplot2::geom_text(data = hdr,
                       ggplot2::aes(x = x, y = top * 1.135, label = sub),
                       colour = hdr$ink, size = 3.1, fontface = "bold") +
    ggplot2::geom_col(data = df, ggplot2::aes(i, v, fill = sub), width = 0.62) +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(breaks = df$i, labels = df$tri,
                                expand = c(0.005, 0)) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      labels = if (s$isCatalogue) waiver() else scales::percent) +
    ggplot2::coord_cartesian(ylim = c(0, top * 1.21), clip = "off") +
    ggplot2::labs(x = NULL, y = s$yLabel,
                  title = if (s$isCatalogue) "Observed cohort catalogue"
                          else paste("De novo signature", s$profile)) +
    biov_theme(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = 4.2, family = "mono",
                                          colour = "#6E7B72"),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 14, 4, 8))
  gg_data_uri(p, width = 1020, height = 430)
}

# ---- protein domain lollipop ----------------------------------------------
# One panel, two independent discrete scales: `fill` carries the domains and
# `colour` the variant classes, which avoids needing ggnewscale. The domain and
# PTM tracks live below zero on the same y axis, with clipping off so they can
# sit outside the panel range. Labels come from biov_lollipop()'s labelRows, so
# ggrepel labels exactly the variants the canvas labels.
plot_lollipop_gg <- function(gene = "TP53") {
  l <- biov_lollipop(gene)
  if (is.null(l)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = paste("No variants for", gene),
                        colour = "#233038") + ggplot2::theme_void()))
  }
  d <- data.frame(pos = l$position, count = l$count,
                  cls = factor(l$class, levels = as.character(l$classes)),
                  label = l$label, stringsAsFactors = FALSE)
  top <- max(c(d$count, 1))
  base <- -0.14 * top
  hgt <- 0.11 * top
  dm <- data.frame(name = factor(l$domainNames, levels = unique(l$domainNames)),
                   start = l$domainStart, end = l$domainEnd)
  dm$mid <- (dm$start + dm$end) / 2

  p <- ggplot2::ggplot() +
    # backbone
    ggplot2::annotate("rect", xmin = 1, xmax = l$length,
                      ymin = base + hgt * 0.32, ymax = base + hgt * 0.68,
                      fill = "#E6DCC8") +
    ggplot2::geom_rect(data = dm,
                       ggplot2::aes(xmin = start, xmax = end,
                                    ymin = base, ymax = base + hgt,
                                    fill = name)) +
    ggplot2::geom_segment(data = d,
                          ggplot2::aes(x = pos, xend = pos, y = 0, yend = count),
                          colour = "#93a1b8", linewidth = 0.4) +
    ggplot2::geom_point(data = d,
                        ggplot2::aes(x = pos, y = count, colour = cls,
                                     size = count), alpha = 0.92) +
    ggrepel::geom_text_repel(
      data = d[l$labelRows, , drop = FALSE],
      ggplot2::aes(x = pos, y = count, label = label),
      size = 3, colour = "#233038", direction = "y", nudge_y = top * 0.07,
      segment.colour = "#C9C1B1", segment.size = 0.25,
      max.overlaps = Inf, seed = 3) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(as.character(l$domainColors),
                               levels(dm$name)), name = "Domain") +
    ggplot2::scale_colour_manual(
      values = stats::setNames(as.character(l$classColors),
                               as.character(l$classes)),
      drop = FALSE, name = "Variant") +
    ggplot2::scale_size_area(max_size = 8, guide = "none") +
    # Padded on both axes so a repelled label on the tallest stem, or on a
    # variant near residue 1 or the C-terminus, has somewhere to go instead of
    # riding the panel edge.
    ggplot2::scale_x_continuous(limits = c(0, l$length),
                                expand = ggplot2::expansion(mult = 0.045)) +
    ggplot2::labs(x = sprintf("%s (%s) - amino-acid position", l$gene, l$uniprot),
                  y = "samples") +
    biov_theme(base_size = 12) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(14, 16, 6, 8))

  if (length(l$ptmPosition)) {
    ptm <- data.frame(pos = l$ptmPosition, type = l$ptmType)
    p <- p +
      ggplot2::geom_segment(data = ptm,
                            ggplot2::aes(x = pos, xend = pos,
                                         y = base - 0.30 * hgt,
                                         yend = base - 0.05 * hgt),
                            colour = "#8A9384", linewidth = 0.3) +
      ggplot2::geom_point(data = ptm,
                          ggplot2::aes(x = pos, y = base - 0.40 * hgt),
                          shape = 17, size = 1.4, colour = "#6E7B72")
  }
  # clip = "off" lets the domain and PTM tracks sit below the panel range; the
  # generous upper bound is headroom for the repelled labels.
  p <- p + ggplot2::coord_cartesian(
    ylim = c(base - 0.55 * hgt, top * 1.26), clip = "off")
  gg_data_uri(p, width = 1000, height = 560)
}

# ---- AlphaFold predicted aligned error (PAE) ------------------------------
# Same binned matrix the React engine gets, on the same LTC ramp with the same
# limits, so the two renderings are the same picture in two engines.
plot_pae_gg <- function(uniprot = "P04637", residue = NULL) {
  pae <- biov_pae(uniprot)
  if (is.null(pae)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = paste("Could not fetch PAE for", uniprot),
                        colour = "#233038") + ggplot2::theme_void()))
  }
  m <- pae$matrix
  pos <- as.integer(pae$rowLabels)
  # as.numeric() unrolls column-major, so the row index cycles fastest.
  long <- data.frame(
    scored  = rep(pos, each = nrow(m)),
    aligned = rep(pos, times = ncol(m)),
    pae     = as.numeric(m))
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = scored, y = aligned, fill = pae)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colours = biov_gradient(), name = "PAE (Å)",
                                  limits = c(0, pae$maxPae)) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "scored residue", y = "aligned residue",
                  title = paste("AlphaFold PAE", uniprot)) +
    biov_theme(base_size = 12)
  if (!is.null(residue) && !is.na(residue)) {
    p1 <- p1 +
      ggplot2::geom_vline(xintercept = residue, colour = "#C63F3E",
                          linetype = "dashed", linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = residue, colour = "#C63F3E",
                          linetype = "dashed", linewidth = 0.4)
  }
  gg_data_uri(p1, width = 700, height = 680)
}

# One row of the PAE matrix: how well the rest of the chain is placed relative
# to the selected residue. The React twin of the N-d array's pixel spectrum.
plot_pae_profile_gg <- function(uniprot = "P04637", residue = NULL) {
  pae <- biov_pae(uniprot)
  if (is.null(pae)) {
    return(gg_data_uri(ggplot2::ggplot() + ggplot2::theme_void(),
                       width = 640, height = 300))
  }
  pos <- as.integer(pae$rowLabels)
  res <- if (is.null(residue) || is.na(residue)) pos[1] else residue
  i <- which.min(abs(pos - res))
  df <- data.frame(residue = pos, pae = pae$matrix[i, ])
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = residue, y = pae)) +
    ggplot2::geom_area(fill = "#8BC8CB", alpha = 0.55) +
    ggplot2::geom_line(colour = "#0E7175", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = pos[i], colour = "#C63F3E",
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::ylim(0, pae$maxPae) +
    ggplot2::labs(x = "residue", y = "PAE (Å)",
                  title = sprintf("aligned on residue %d", pos[i])) +
    biov_theme(base_size = 12)
  gg_data_uri(p1, width = 640, height = 300)
}

# ---- Manhattan plot (GWAS) ------------------------------------------------
plot_manhattan_gg <- function() {
  g <- biov_gwas()
  df <- data.frame(x = g$x, y = g$neglog10p, chr = factor(g$chr, levels = 1:22))
  # alternating two-tone colouring by chromosome
  cols <- rep(c("#0E7175", "#9BB29E"), length.out = 22); names(cols) <- levels(df$chr)
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = chr)) +
    ggplot2::geom_point(size = 0.5, alpha = 0.75) +
    ggplot2::geom_hline(yintercept = g$sig, linetype = "dashed", colour = "#C63F3E") +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(breaks = g$chr_centres, labels = 1:22, expand = c(0.01, 0)) +
    ggplot2::labs(x = "chromosome", y = expression(-log[10] ~ p)) +
    biov_theme(base_size = 12) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  gg_data_uri(p1, width = 1000, height = 480)
}

# ---- QQ plot (GWAS) -------------------------------------------------------
plot_qq_gg <- function() {
  q <- biov_qq()
  df <- data.frame(expected = q$expected, observed = q$observed)
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = expected, y = observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#C9C1B1") +
    ggplot2::geom_point(size = 0.7, colour = "#0E7175", alpha = 0.8) +
    ggplot2::labs(x = expression(expected ~ -log[10] ~ p),
                  y = expression(observed ~ -log[10] ~ p),
                  title = bquote(lambda[GC] == .(q$lambda))) +
    biov_theme(base_size = 12)
  gg_data_uri(p1, width = 560, height = 520)
}

# ---- eQTL / pQTL effect heatmap -------------------------------------------
plot_eqtl_gg <- function() {
  e <- biov_eqtl()
  m <- e$matrix
  long <- data.frame(
    variant = factor(rep(rownames(m), times = ncol(m)), levels = rev(rownames(m))),
    gene = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)),
    beta = as.numeric(m))
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = gene, y = variant, fill = beta)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = biov_diverging(), name = "effect (β)",
                                  limits = max(abs(m)) * c(-1, 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
                   axis.text.y = ggplot2::element_text(size = 5.5), legend.position = "right")
  gg_data_uri(p1, width = 820, height = 760)
}

# ---- scATAC coverage-by-cluster (Signac-style faceted area tracks) --------
plot_atac_gg <- function() {
  a <- biov_atac()
  m <- a$matrix
  long <- data.frame(
    cluster = factor(rep(a$clusters, times = a$n_bins), levels = a$clusters),
    pos = rep(a$positions, each = a$n_clusters),
    signal = as.numeric(m))
  cols <- biov_categorical(a$n_clusters); names(cols) <- a$clusters
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = pos, y = signal, fill = cluster)) +
    ggplot2::geom_area(alpha = 0.9) +
    ggplot2::facet_grid(cluster ~ ., switch = "y") +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(labels = function(v) paste0(round(v / 1e6, 2), " Mb")) +
    ggplot2::labs(x = a$chrom, y = NULL) +
    biov_theme(base_size = 11) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 7, colour = "#233038"),
      panel.spacing = ggplot2::unit(1, "pt"))
  gg_data_uri(p1, width = 940, height = 620)
}

# ---- N-D array: 2-D slice (geom_raster) + spectrum at a probe pixel -------
plot_ndslice_gg <- function(channel = 1L) {
  nd <- biov_ndarray()
  sl <- nd$slice(channel)                 # matrix ny x nx, sl[y, x]
  long <- expand.grid(x = seq_len(nd$nx), y = seq_len(nd$ny))
  long$v <- sl[cbind(long$y, long$x)]
  p1 <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = y, fill = v)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colours = biov_gradient(), name = "intensity") +
    ggplot2::scale_y_reverse() + ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, title = sprintf("channel %d / %d", channel, nd$nch)) +
    biov_theme(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
  gg_data_uri(p1, width = 640, height = 620)
}

plot_ndspectrum_gg <- function(px, py) {
  nd <- biov_ndarray()
  s <- nd$spectrum(px, py)
  df <- data.frame(channel = seq_along(s), intensity = s)
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = channel, y = intensity)) +
    ggplot2::geom_line(colour = "#0E7175", linewidth = 0.7) +
    ggplot2::geom_point(colour = "#ED773C", size = 1.3) +
    ggplot2::labs(x = "channel", y = "intensity",
                  title = sprintf("spectrum at pixel (%d, %d)", px, py)) +
    biov_theme(base_size = 12)
  gg_data_uri(p1, width = 640, height = 300)
}

# ---- UMAP (deliberately renders a 40k subsample; the point is that this is
# slow/limited while React renders all 584k instantly) ----------------------
plot_umap_gg <- function(colour_by = "cell_type") {
  df <- biov_umap_sample()
  df$grp <- df[[colour_by]]
  # cap legend: keep top 12 categories, lump the rest into "Other"
  keep <- names(sort(table(df$grp), decreasing = TRUE))[seq_len(min(12, length(unique(df$grp))))]
  df$grp <- ifelse(df$grp %in% keep, df$grp, "Other")
  n <- nrow(df)
  t0 <- Sys.time()
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = grp)) +
    ggplot2::geom_point(size = 0.35, alpha = 0.5) +
    ggplot2::scale_colour_manual(values = biov_categorical(length(unique(df$grp))), name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2, alpha = 1))) +
    ggplot2::labs(x = "UMAP-1", y = "UMAP-2") +
    biov_theme(base_size = 12) +
    ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 7))
  uri <- gg_data_uri(p1, width = 900, height = 640)
  attr(uri, "n") <- n
  attr(uri, "secs") <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  uri
}

# ---- stacked violin -------------------------------------------------------
# Rebuilds the polygons from the densities biov_violin() estimated, rather than
# calling geom_violin on the raw values. geom_violin would re-estimate with its
# own bandwidth, and the two engines would then draw different curves from one
# dataset.
plot_violin_gg <- function(n_genes = 8L) {
  vd <- biov_violin(n_genes)
  n <- vd$gridN
  dm <- matrix(vd$density, ncol = n, byrow = TRUE)
  cols <- stats::setNames(vd$clusterColors, vd$clusters)

  gm <- matrix(vd$grids, ncol = n, byrow = TRUE)
  poly <- do.call(rbind, lapply(seq_len(nrow(dm)), function(i) {
    row <- dm[i, ]
    grid_i <- gm[match(vd$feature[i], vd$genes), ]
    # Scale within the gene's row so clusters stay comparable inside a gene,
    # matching the component's default.
    gi <- match(vd$feature[i], vd$genes)
    rows_of_gene <- which(vd$feature == vd$feature[i])
    rmax <- max(dm[rows_of_gene, ], na.rm = TRUE)
    if (!is.finite(rmax) || rmax <= 0) rmax <- 1
    w <- row / rmax
    ci <- match(vd$cluster[i], vd$clusters)
    data.frame(
      # x is the cluster centre plus the mirrored density.
      x = c(ci + 0.42 * w, rev(ci - 0.42 * w)),
      y = c(grid_i, rev(grid_i)),
      id = i,
      feature = factor(vd$feature[i], levels = vd$genes),
      cluster = factor(vd$cluster[i], levels = vd$clusters),
      stringsAsFactors = FALSE
    )
  }))

  meds <- data.frame(
    feature = factor(vd$feature, levels = vd$genes),
    cluster = factor(vd$cluster, levels = vd$clusters),
    ci = match(vd$cluster, vd$clusters),
    med = vd$median
  )

  p <- ggplot2::ggplot(poly, ggplot2::aes(x, y, group = id, fill = cluster)) +
    ggplot2::geom_polygon(colour = NA, alpha = 0.85) +
    ggplot2::geom_segment(data = meds,
      ggplot2::aes(x = ci - 0.15, xend = ci + 0.15, y = med, yend = med),
      colour = "#FBF7EF", linewidth = 0.5, inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = cols, guide = "none", drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(vd$clusters), labels = vd$clusters,
      limits = c(0.4, length(vd$clusters) + 0.6), expand = ggplot2::expansion(0)) +
    # scales = "free_y" so each gene keeps its own range, matching the
    # component's per-row grids.
    ggplot2::facet_grid(rows = ggplot2::vars(feature), switch = "y",
                        scales = "free_y") +
    ggplot2::labs(x = NULL, y = "log1p CP10K") +
    biov_theme(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 7),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 9, hjust = 1),
      strip.background = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1, "pt"),
      panel.grid.major.x = ggplot2::element_blank()
    )
  gg_data_uri(p, width = 900, height = 700)
}

# ---- UpSet: driver co-occurrence -----------------------------------------
# Three panels sharing one discrete x (the intersections) and one discrete y
# (the genes), aligned with patchwork. The intersection order and membership
# come from biov_upset(), so both engines draw the same columns.
plot_upset_gg <- function(n_genes = 8L, max_n = 20L) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = "patchwork is required",
                        colour = "#233038") + ggplot2::theme_void()))
  }
  u <- biov_upset(n_genes, max_n)
  m <- matrix(u$membership, nrow = length(u$size), byrow = TRUE)
  idx <- factor(seq_along(u$size), levels = seq_along(u$size))
  # Genes top to bottom in frequency order, so the discrete y is reversed.
  glev <- rev(u$sets)

  bars <- data.frame(i = idx, size = u$size)
  p_bars <- ggplot2::ggplot(bars, ggplot2::aes(i, size)) +
    ggplot2::geom_col(fill = "#233038", width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = size), vjust = -0.35, size = 2.6,
                       colour = "#6E7B72") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::scale_x_discrete(limits = levels(idx), drop = FALSE) +
    ggplot2::labs(x = NULL, y = "samples") +
    biov_theme(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  dots <- expand.grid(i = idx, gene = factor(u$sets, levels = glev),
                      KEEP.OUT.ATTRS = FALSE)
  dots$on <- as.logical(as.integer(m))
  # Spines connect the first and last filled dot in each column, which is what
  # makes a three-gene intersection read as one thing.
  spine <- do.call(rbind, lapply(seq_len(nrow(m)), function(k) {
    on <- which(m[k, ] == 1L)
    if (length(on) < 2) return(NULL)
    data.frame(i = idx[k],
               lo = factor(u$sets[max(on)], levels = glev),
               hi = factor(u$sets[min(on)], levels = glev))
  }))

  # Pin both discrete scales. Otherwise whichever layer ggplot reaches first
  # seeds them, and adding the spine layer silently reorders the columns so the
  # matrix stops lining up with the bars above it.
  p_matrix <- ggplot2::ggplot(dots, ggplot2::aes(i, gene)) +
    ggplot2::geom_point(ggplot2::aes(colour = on), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#233038",
                                            `FALSE` = "#DED5C4"),
                                 guide = "none") +
    ggplot2::scale_x_discrete(limits = levels(idx), drop = FALSE) +
    ggplot2::scale_y_discrete(limits = glev, drop = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 9),
                   panel.grid.major.x = ggplot2::element_blank())
  if (!is.null(spine)) {
    p_matrix <- p_matrix +
      ggplot2::geom_segment(data = spine,
        ggplot2::aes(x = i, xend = i, y = lo, yend = hi),
        colour = "#233038", linewidth = 0.7, inherit.aes = FALSE) +
      ggplot2::geom_point(ggplot2::aes(colour = on), size = 2.6)
  }

  tot <- data.frame(gene = factor(u$sets, levels = glev), n = u$setSizes)
  p_sets <- ggplot2::ggplot(tot, ggplot2::aes(n, gene)) +
    ggplot2::geom_col(fill = "#233038", alpha = 0.55, width = 0.6) +
    ggplot2::scale_y_discrete(limits = glev, drop = FALSE) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(x = "set size", y = NULL) +
    biov_theme(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 8))

  # "#" is an empty area, not a slot, so the design takes exactly three plots:
  # A = intersection bars, B = set totals, C = the membership matrix.
  pw <- patchwork::wrap_plots(
    p_bars, p_sets, p_matrix,
    design = "#A\nBC",
    widths = c(1, 4.2), heights = c(2.4, 1.6)) +
    patchwork::plot_annotation(theme = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent",
                                              colour = NA)))
  gg_data_uri(pw, width = 960, height = 620)
}

# ---- marker gene dot plot -------------------------------------------------
# Gene order, the detection percentages and the scaling all come from
# biov_dotplot(), so this and the React component plot one computation. Size is
# mapped through scale_size_area, which makes AREA proportional to the
# percentage the way the component's sqrt radius does; scale_size would map
# radius instead and overstate the biggest dots.
plot_dotplot_gg <- function(scale_by = "gene") {
  d <- biov_dotplot(scale_by)
  df <- data.frame(
    gene = factor(d$gene, levels = rev(d$genes)),   # first gene at the top
    cluster = factor(d$cluster, levels = d$clusters),
    pct = d$pct, value = d$value
  )
  # Dots for genes never seen in a cluster are noise, not information.
  df <- df[df$pct > 0, , drop = FALSE]

  p <- ggplot2::ggplot(df, ggplot2::aes(cluster, gene)) +
    ggplot2::geom_point(ggplot2::aes(size = pct, colour = value)) +
    ggplot2::scale_size_area(max_size = 5.2, name = "% expressing",
                             limits = c(0, 100),
                             breaks = c(25, 50, 75, 100)) +
    ggplot2::scale_colour_gradientn(colours = biov_gradient(),
                                    name = d$valueLabel) +
    ggplot2::labs(x = NULL, y = NULL) +
    biov_theme(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 7),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.size = ggplot2::unit(10, "pt"),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7)
    )
  gg_data_uri(p, width = 820, height = 900)
}

# ---- Kaplan-Meier survival + number at risk ------------------------------
# Curves and risk table are two panels sharing one x scale, aligned with
# patchwork. Everything plotted is computed in biov_survival(), so this and the
# React component are drawing one set of numbers.

# Duplicate each point so a straight line through the result traces the step.
# geom_step would do it for the curve, but a ribbon has no step variant and
# would draw diagonal band edges the estimator never asserts. Expanding here
# instead is also the same operation the React component's stepPoints() does,
# so the two engines draw geometrically identical shapes.
.step_expand <- function(df) {
  n <- nrow(df)
  if (n < 2) return(df)
  out <- df[rep(seq_len(n), each = 2), , drop = FALSE]
  out <- out[-1, , drop = FALSE]
  out <- out[-nrow(out), , drop = FALSE]
  # Odd rows carry the previous y at the new x; even rows are the real point.
  keep_prev <- seq(1, nrow(out), by = 2)
  for (col in c("surv", "lower", "upper")) {
    if (!is.null(out[[col]])) out[[col]][keep_prev] <- df[[col]][-n]
  }
  out$time[keep_prev] <- df$time[-1]
  out
}

plot_km_gg <- function(group_by = "subtype", gene = "TP53") {
  # patchwork's `/` is an S3 method on ggplot objects, registered when its
  # namespace loads. Without this the operator is a plain error, and whether it
  # worked would depend on whether the oncoplot page had been visited first.
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(gg_data_uri(ggplot2::ggplot() +
      ggplot2::annotate("text", 0, 0, label = "patchwork is required",
                        colour = "#233038") + ggplot2::theme_void()))
  }
  s <- biov_survival(group_by, gene)
  cols <- stats::setNames(s$colors, s$levels)
  curves <- data.frame(time = s$time, surv = s$surv,
                       lower = s$lower, upper = s$upper,
                       group = factor(s$group, levels = s$levels))
  stepped <- do.call(rbind, lapply(split(curves, curves$group), .step_expand))
  cens <- data.frame(time = s$censorTime, surv = s$censorSurv,
                     group = factor(s$censorGroup, levels = s$levels))
  # Match the component, which frames to the last observation, not the last
  # tick. coord_cartesian rather than limits so nothing is silently dropped.
  xmax <- max(c(s$time, s$riskTimes))

  top <- ggplot2::ggplot(stepped, ggplot2::aes(time, surv, colour = group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = group),
                         alpha = 0.13, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(data = cens, shape = 3, size = 1.1,
                        show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cols, name = NULL, drop = FALSE) +
    ggplot2::scale_fill_manual(values = cols, guide = "none", drop = FALSE) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
    ggplot2::scale_x_continuous(breaks = s$riskTimes) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "overall survival") +
    ggplot2::annotate("text", x = 2, y = 0.06, label = s$pLabel,
                      hjust = 0, size = 3.6, colour = "#233038") +
    biov_theme(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   legend.text = ggplot2::element_text(size = 9),
                   # The axis belongs under the risk table, drawn once.
                   axis.text.x = ggplot2::element_blank())

  risk <- expand.grid(time = s$riskTimes, group = factor(s$levels, s$levels),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  # riskCounts is row-major groups x times, so column-major expand.grid needs
  # the transpose to line up.
  risk$n <- as.integer(t(matrix(s$riskCounts, nrow = length(s$levels),
                                byrow = TRUE)))
  # Reverse so the first stratum sits at the top, matching the legend.
  risk$group <- factor(risk$group, levels = rev(s$levels))

  bottom <- ggplot2::ggplot(risk, ggplot2::aes(time, group)) +
    ggplot2::geom_text(ggplot2::aes(label = n, colour = group), size = 3.1,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cols, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = s$riskTimes) +
    ggplot2::coord_cartesian(xlim = c(0, xmax)) +
    ggplot2::labs(x = "months", y = NULL, title = "number at risk") +
    biov_theme(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, colour = "#6E7B72"),
      axis.text.y = ggplot2::element_text(size = 9)
    )

  p <- top / bottom + patchwork::plot_layout(heights = c(3.4, 1))
  gg_data_uri(p, width = 900, height = 640)
}

# ---- Xenium single-molecule map (same subsample contrast as the UMAP page) --
# Levels and colours come from www/data/xenium_meta.json, the same sidecar the
# React client reads, so the legend order and the palette cannot drift between
# the two engines.
plot_xenium_gg <- function(colour_by = "class") {
  df <- biov_xenium_sample()
  meta <- biov_xenium_meta()
  fld <- meta$fields[[colour_by]]
  if (is.null(fld)) {
    colour_by <- "class"
    fld <- meta$fields[["class"]]
  }
  df$grp <- factor(df[[colour_by]], levels = fld$levels)
  n <- nrow(df)
  t0 <- Sys.time()
  # Tissue coordinates are in micrometres and the section is not square, so the
  # aspect ratio has to be fixed or the anatomy is a lie. y is flipped to match
  # the image convention the instrument writes.
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = grp)) +
    ggplot2::geom_point(size = 0.3, alpha = 0.55) +
    ggplot2::scale_colour_manual(values = stats::setNames(fld$colors, fld$levels),
                                 drop = FALSE, name = NULL) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(size = 2.4, alpha = 1))) +
    ggplot2::labs(x = expression(x ~ (mu * m)), y = expression(y ~ (mu * m))) +
    biov_theme(base_size = 12) +
    ggplot2::theme(legend.position = "right",
                   legend.text = ggplot2::element_text(size = 8))
  # coord_fixed on a 1.37:1 section, so the frame is sized to match rather than
  # leaving a band of empty panel above and below the tissue.
  uri <- gg_data_uri(p1, width = 960, height = 620)
  attr(uri, "n") <- n
  attr(uri, "secs") <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  uri
}

# ---- PCA explorer ---------------------------------------------------------
# Three views off one decomposition. Which one renders is the caller's choice
# and matches the React side exactly, so the engine toggle never swaps the
# figure out from under the reader.
plot_pca_gg <- function(view = "scores", pc_x = 1L, pc_y = 2L,
                        pc = 1L, n_load = 20L) {
  if (identical(view, "scree")) return(.pca_scree_gg())
  if (identical(view, "loadings")) return(.pca_loadings_gg(pc, n_load))
  .pca_scores_gg(pc_x, pc_y)
}

.pca_scores_gg <- function(pc_x = 1L, pc_y = 2L) {
  s <- biov_pca_scores(pc_x, pc_y)
  df <- data.frame(x = s$x, y = s$y, group = s$color, label = s$label,
                   stringsAsFactors = FALSE)
  lev <- sort(unique(df$group))
  cols <- stats::setNames(biov_categorical(length(lev)), lev)
  df$group <- factor(df$group, levels = lev)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#E6DCC8") +
    ggplot2::geom_vline(xintercept = 0, colour = "#E6DCC8") +
    ggplot2::geom_point(size = 3, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = cols, limits = lev, drop = FALSE) +
    ggplot2::labs(x = s$xLabel, y = s$yLabel, colour = NULL) +
    # Both axes are in the same units, so one unit must be one unit in either
    # direction. Without this a component explaining 4% is stretched to fill
    # the panel and reads as though it mattered as much as one explaining 34%.
    # It matches aspect = "equal" on the React side.
    ggplot2::coord_fixed(ratio = 1) +
    biov_theme()
  # An ellipse needs more points than parameters; with a tiny stratum it would
  # fail rather than degrade, so it is only added where every group can carry one.
  if (all(table(df$group) >= 4L)) {
    p <- p + ggplot2::stat_ellipse(ggplot2::aes(fill = group), geom = "polygon",
                                   alpha = 0.08, colour = NA, type = "norm",
                                   show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = cols, limits = lev, drop = FALSE)
  }
  # Squarer than the other views: coord_fixed leaves side margins in a wide
  # panel, so a wide canvas would just be drawing whitespace.
  gg_data_uri(p, width = 740, height = 640)
}

.pca_scree_gg <- function(n = 10L) {
  s <- biov_pca_scree(n)
  df <- data.frame(pc = factor(s$label, levels = s$label),
                   value = s$value, cum = s$cumulative)
  cols <- biov_categorical(2)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = pc)) +
    ggplot2::geom_col(ggplot2::aes(y = value), fill = cols[1], width = 0.62) +
    # Both series are percentages, so they share one axis honestly rather than
    # needing a second scale the reader has to reconcile.
    ggplot2::geom_line(ggplot2::aes(y = cum, group = 1), colour = cols[2],
                       linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = cum), colour = cols[2], size = 2.2) +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                labels = function(v) paste0(v, "%")) +
    ggplot2::labs(x = NULL, y = "variance explained") +
    biov_theme()
  gg_data_uri(p, width = 900, height = 560)
}

.pca_loadings_gg <- function(pc = 1L, n = 20L) {
  l <- biov_pca_loadings(pc, n)
  df <- data.frame(gene = factor(l$label, levels = rev(l$label)),
                   value = l$value,
                   dir = factor(l$group, levels = c("positive", "negative")),
                   stringsAsFactors = FALSE)
  cols <- stats::setNames(biov_categorical(4)[c(1, 4)],
                          c("positive", "negative"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = gene, y = value, fill = dir)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, colour = "#6E7B72", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = cols, limits = names(cols),
                               drop = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, fill = NULL,
                  y = sprintf("PC%d loading (%.1f%% of variance)",
                              l$pc, l$varExp)) +
    biov_theme()
  gg_data_uri(p, width = 900, height = 640)
}
