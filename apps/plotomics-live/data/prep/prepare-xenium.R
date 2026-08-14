#!/usr/bin/env Rscript
# Prepare the Xenium single-molecule transcript map for the React showcase page.
#
# Source: 10x Genomics Xenium In Situ, "Human Breast Cancer Rep 1"
# (Xenium Analyzer 1.0.1, 313-gene breast panel, FFPE section).
#   https://www.10xgenomics.com/datasets/
#     ffpe-human-breast-with-custom-add-on-panel-1-standard
#   License: CC BY 4.0
#
# This is the second WebGL flagship, and the counterpart to the UMAP page. There
# the points are cells in an abstract embedding; here they are individual mRNA
# molecules at their real position in a tissue section, which is the thing
# Xenium exists to measure. 42.6M detections is far past what any static plot
# can draw, so the ggplot2 side subsamples and says so.
#
# The source transcript table is a 1.4 GB .csv.gz. It is never held in memory:
# duckdb streams the gzip and does the filtering and reservoir sampling in one
# pass. The download is cached in the gitignored data/raw/ and is only needed to
# regenerate the blobs, not to run the app.
#
# Output blobs (little-endian):
#   xenium_x.i16, xenium_y.i16   quantized micrometre coordinates
#   xenium_class.i16             marker-class codes
#   xenium_gene.i16              top-gene codes
#   xenium_nucleus.i16           nuclear / cytoplasmic codes
#   xenium_meta.json             levels, colours, and the dequantization terms
#
# Coordinates are quantized to Int16 rather than shipped as Float32. Over a
# 7.5 mm section that is a 0.115 um step, which is finer than the instrument's
# own localization precision, so nothing measurable is lost and the two coord
# blobs cost 4 MB instead of 8 MB.

suppressWarnings(suppressMessages({
  library(DBI)
  library(duckdb)
  library(readr)
  library(jsonlite)
}))

# --- paths -----------------------------------------------------------------
# Resolve the app root from this script's own location (data/prep/ -> app root),
# robust under Rscript, source(), and interactive runs.
this_file <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) f[1] else if (!is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else NA
}
app_dir <- if (!is.na(this_file)) {
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE)
} else getwd()
if (!dir.exists(file.path(app_dir, "srcts"))) app_dir <- getwd()  # last-resort fallback
raw_dir <- file.path(app_dir, "data", "raw")
out_dir <- file.path(app_dir, "www", "data")
csv_dir <- file.path(app_dir, "data")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

URL <- paste0("https://cf.10xgenomics.com/samples/xenium/1.0.1/",
              "Xenium_FFPE_Human_Breast_Cancer_Rep1/",
              "Xenium_FFPE_Human_Breast_Cancer_Rep1_transcripts.csv.gz")
tx_f <- file.path(raw_dir, "xenium_transcripts.csv.gz")

N_SAMPLE <- 1000000L   # detections kept for the WebGL layer
N_GG     <- 40000L     # detections kept for the ggplot2 counterpart
N_TOPGENE <- 12L       # genes named individually in the per-gene view
QV_MIN   <- 20         # 10x's own recommended decoding quality floor
SEED     <- 42L

# --- download once ---------------------------------------------------------
# Downloaded to a .part file first: a truncated download that already carried
# its final name would look like a valid cache on the next run.
if (!file.exists(tx_f) || file.info(tx_f)$size == 0) {
  message("downloading (1.4 GB, one time): ", URL)
  part <- paste0(tx_f, ".part")
  utils::download.file(URL, part, mode = "wb", quiet = FALSE)
  file.rename(part, tx_f)
} else {
  message("cached: ", basename(tx_f))
}

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
src <- sprintf("read_csv_auto('%s')", gsub("\\\\", "/", tx_f))

# Control features are the panel's built-in false-positive monitors, not
# biology: negative-control probes, unused codewords, blanks, and antisense
# controls. They belong in a QC report, not on a tissue map.
not_control <- paste(
  "feature_name not like '%Codeword%'",
  "feature_name not like 'NegControl%'",
  "feature_name not like 'BLANK%'",
  "feature_name not like 'antisense%'",
  sep = " and "
)

# --- global counts (one streamed pass) -------------------------------------
message("scanning for cohort totals ...")
tot <- dbGetQuery(con, sprintf("
  select count(*) n_all,
         sum(case when qv >= %f and %s then 1 else 0 end) n_kept,
         count(distinct case when %s then feature_name end) n_genes
  from %s", QV_MIN, not_control, not_control, src))
message(sprintf("  %s detections, %s pass QV>=%d and are real genes (%d genes)",
                format(tot$n_all, big.mark = ","),
                format(tot$n_kept, big.mark = ","), QV_MIN, tot$n_genes))

# --- sample (second streamed pass) -----------------------------------------
# Bernoulli, not reservoir: duckdb runs the scan across threads and each thread
# keeps its own reservoir, so a "reservoir(1000000 rows)" request comes back
# well short. A per-row coin flip is thread-independent, so the fraction is
# honoured. Ask for 6% more than needed to absorb the binomial wobble, then
# trim to exactly N_SAMPLE.
pct <- 100 * (N_SAMPLE * 1.06) / tot$n_kept
message(sprintf("sampling %s detections (%.3f%% bernoulli) ...",
                format(N_SAMPLE, big.mark = ","), pct))
tx <- dbGetQuery(con, sprintf("
  select x_location as x, y_location as y, feature_name as gene,
         overlaps_nucleus as nuc
  from %s
  where qv >= %f and %s
  using sample %f%% (bernoulli, %d)",
  src, QV_MIN, not_control, pct, SEED))
message(sprintf("  drew %s rows", format(nrow(tx), big.mark = ",")))
if (nrow(tx) < N_SAMPLE) {
  stop(sprintf("bernoulli draw came back short (%d < %d); raise the margin",
               nrow(tx), N_SAMPLE))
}

# Trim to exactly N_SAMPLE, and shuffle while doing it. A scatter of a million
# overlapping points has no z-order, so whichever rows land last win the pixel.
# Randomizing the row order means no category is systematically drawn on top of
# another.
set.seed(SEED)
tx <- tx[sample.int(nrow(tx), N_SAMPLE), ]

# --- marker classes --------------------------------------------------------
# Hand-curated from the 313-gene breast panel. The point of this view is tissue
# architecture: where the tumour nests are, where the stroma walls them off,
# where the immune infiltrate sits. Genes that are broadly expressed or
# ambiguous are deliberately left in "Other" rather than forced into a class.
markers <- list(
  "Tumour epithelial" = c(
    "ERBB2", "EPCAM", "KRT7", "KRT8", "KRT23", "CDH1", "FOXA1", "GATA3",
    "TACSTD2", "CEACAM6", "ANKRD30A", "ELF3", "ELF5", "S100A14", "CLDN4",
    "LYPD3", "AGR3", "MLPH", "ESR1", "PGR", "AR", "SCGB2A1", "DSP", "JUP",
    "PIGR", "CLIC6", "DSC2", "MUC6", "OPRPN", "ABCC11", "SERHL2"),
  "Myoepithelial / muscle" = c(
    "ACTA2", "ACTG2", "MYH11", "MYLK", "KRT5", "KRT14", "KRT15", "KRT16",
    "KRT6B", "OXTR", "SVIL", "MYBPC1", "DAPK3"),
  "Fibroblast / stroma" = c(
    "POSTN", "LUM", "FBLN1", "DPT", "PDGFRA", "PDGFRB", "PCOLCE", "SFRP1",
    "SFRP4", "CCDC80", "MMP2", "CXCL12", "LRRC15", "ADH1B", "PTN", "IGF1",
    "CRISPLD2", "TIMP4", "MMP1", "MMP12", "EDNRB", "PTGDS"),
  "Immune" = c(
    "PTPRC", "CD3D", "CD3E", "CD3G", "CD247", "CD4", "CD8A", "CD8B", "TRAC",
    "IL7R", "CCL5", "GZMA", "GZMB", "GZMK", "NKG7", "PRF1", "GNLY", "KLRB1",
    "KLRC1", "KLRD1", "KLRF1", "FOXP3", "IL2RA", "CTLA4", "PDCD1", "LAG3",
    "TIGIT", "HAVCR2", "CD27", "CD69", "CCR7", "TCF7", "SELL", "LTB",
    "CD68", "CD14", "CD163", "C1QA", "C1QC", "AIF1", "ITGAM", "ITGAX", "LYZ",
    "MNDA", "TYROBP", "FCER1G", "FCGR3A", "MRC1", "MS4A1", "CD79A", "CD79B",
    "BANK1", "TCL1A", "MZB1", "DERL3", "TNFRSF17", "CPA3", "TPSAB1", "HDC",
    "MPO", "CTSG", "S100A8", "LILRA4", "PLD4", "SPIB", "IL3RA", "CLEC9A",
    "CD1C", "FCER1A", "IGSF6", "LY86", "CYTIP", "RHOH", "CD19", "CD80",
    "CD86", "CD83", "CXCR4", "GPR183", "CX3CR1", "SLAMF1", "SLAMF7",
    "CEACAM8", "AHSP", "SLC4A1", "CCL8", "CCL20", "FGL2", "IL2RG"),
  "Endothelial / vascular" = c(
    "PECAM1", "VWF", "CLDN5", "EGFL7", "KDR", "RAMP2", "MMRN2", "CLEC14A",
    "AQP1", "SOX17", "SOX18", "NOSTRIN", "ESM1", "ANGPT2", "CD93", "CRHBP"),
  "Adipocyte" = c("ADIPOQ", "LEP", "LPL", "PDK4", "UCP1", "MEDAG")
)
class_levels <- c(names(markers), "Other")
class_colors <- c("#C63F3E", "#ED773C", "#708C69", "#0E7175", "#808BC5",
                  "#E4A25B", "#C9C1B1")   # Other is a warm grey so it recedes

gene2class <- stats::setNames(
  rep(names(markers), lengths(markers)), unlist(markers, use.names = FALSE))
cls <- unname(gene2class[tx$gene])
cls[is.na(cls)] <- "Other"

# --- per-gene field (top N by abundance, rest lumped) ----------------------
gene_rank <- sort(table(tx$gene), decreasing = TRUE)
top_genes <- names(gene_rank)[seq_len(min(N_TOPGENE, length(gene_rank)))]
gene_levels <- c(top_genes, "Other")
gene_colors <- c(c("#0E7175", "#ED773C", "#708C69", "#C63F3E", "#808BC5",
                   "#E4A25B", "#245E55", "#9E3F71", "#56B4E9", "#EAC119",
                   "#5B5F8D", "#9BB29E")[seq_along(top_genes)], "#C9C1B1")
gene_lab <- ifelse(tx$gene %in% top_genes, tx$gene, "Other")

# --- subcellular field -----------------------------------------------------
nuc_levels <- c("Nuclear", "Cytoplasmic")
nuc_colors <- c("#0E7175", "#C9C1B1")
nuc_lab <- ifelse(tx$nuc == 1, "Nuclear", "Cytoplasmic")

# --- encode ----------------------------------------------------------------
code_of <- function(v, levels) as.integer(factor(v, levels = levels)) - 1L

# Map micrometres onto the full Int16 range. Both the scale and the offset go
# into the sidecar so the client can undo it exactly.
quantize <- function(v) {
  lo <- min(v); hi <- max(v)
  scale <- (hi - lo) / 65535
  list(codes = as.integer(round((v - lo) / scale) - 32768L),
       scale = scale, offset = lo, min = lo, max = hi)
}
qx <- quantize(tx$x)
qy <- quantize(tx$y)
message(sprintf("  quantization step: %.4f um in x, %.4f um in y",
                qx$scale, qy$scale))

wi16 <- function(x, f) writeBin(as.integer(x), f, size = 2L, endian = "little")
wi16(qx$codes, file.path(out_dir, "xenium_x.i16"))
wi16(qy$codes, file.path(out_dir, "xenium_y.i16"))
wi16(code_of(cls, class_levels), file.path(out_dir, "xenium_class.i16"))
wi16(code_of(gene_lab, gene_levels), file.path(out_dir, "xenium_gene.i16"))
wi16(code_of(nuc_lab, nuc_levels), file.path(out_dir, "xenium_nucleus.i16"))

meta_json <- list(
  dataset = "Xenium human breast cancer, Rep 1 (10x Genomics)",
  source = paste0("https://www.10xgenomics.com/datasets/",
                  "ffpe-human-breast-with-custom-add-on-panel-1-standard"),
  license = "CC BY 4.0",
  panel = "Xenium Human Breast v1, 313 genes",
  n = nrow(tx),
  nTotal = as.numeric(tot$n_all),
  nPassing = as.numeric(tot$n_kept),
  nGenes = as.integer(tot$n_genes),
  qvMin = QV_MIN,
  coords = list(
    x = list(file = "xenium_x.i16", scale = qx$scale, offset = qx$offset),
    y = list(file = "xenium_y.i16", scale = qy$scale, offset = qy$offset)
  ),
  bounds = list(minX = qx$min, maxX = qx$max, minY = qy$min, maxY = qy$max),
  fields = list(
    class = list(label = "Marker class", file = "xenium_class.i16",
                 levels = class_levels, colors = class_colors),
    gene = list(label = sprintf("Gene (top %d)", length(top_genes)),
                file = "xenium_gene.i16",
                levels = gene_levels, colors = gene_colors),
    nucleus = list(label = "Subcellular", file = "xenium_nucleus.i16",
                   levels = nuc_levels, colors = nuc_colors)
  )
)
write_json(meta_json, file.path(out_dir, "xenium_meta.json"),
           auto_unbox = TRUE, digits = 10, pretty = TRUE)

# --- a downsampled CSV for the (deliberately slower) ggplot2 side ----------
s <- sort(sample.int(nrow(tx), min(nrow(tx), N_GG)))
gg <- data.frame(x = round(tx$x[s], 2), y = round(tx$y[s], 2),
                 class = cls[s], gene = gene_lab[s], nucleus = nuc_lab[s])
write_csv(gg, file.path(csv_dir, "xenium_ggplot_sample.csv"))

message("\nclass composition of the sample:")
print(round(100 * sort(table(cls), decreasing = TRUE) / nrow(tx), 1))
message(sprintf("\ndone: %s detections in %s (+ %s-row ggplot sample)",
                format(nrow(tx), big.mark = ","), out_dir,
                format(length(s), big.mark = ",")))
