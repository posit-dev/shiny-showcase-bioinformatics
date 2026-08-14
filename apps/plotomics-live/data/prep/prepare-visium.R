#!/usr/bin/env Rscript
# Prepare the 10x Visium spatial transcriptomics data.
#
# Source: 10x Genomics public dataset "Human Breast Cancer (Block A Section 1)",
# Visium Spatial Gene Expression v1.1.0, 3,798 spots x 36,601 genes.
#   https://www.10xgenomics.com/datasets/human-breast-cancer-block-a-section-1-1-standard
#   License: CC BY 4.0. Redistributable with attribution.
#
# Human breast cancer on purpose: it keeps the whole gallery on one disease, so
# the spatial page sits alongside the TCGA-BRCA oncoplot, lollipop and volcano
# rather than introducing a fifth unrelated tissue.
#
# Three files are pulled and reduced to about 2 MB of committed output:
#   _spatial.tar.gz   (10 MB)  spot coordinates, the low-res H&E image, scale factors
#   _analysis.tar.gz  (33 MB)  graph-based clusters and per-cluster marker genes
#   _filtered_feature_bc_matrix.tar.gz (78 MB)  the expression matrix, MEX format
#
# MEX rather than HDF5 so this needs only Matrix, not rhdf5.
#
# Re-run to refresh; downloads are cached in data/raw/.

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(Matrix)
}))

# --- paths -----------------------------------------------------------------
this_file <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) f[1] else if (!is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else NA
}
app_dir <- if (!is.na(this_file)) {
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE)
} else getwd()
if (!dir.exists(file.path(app_dir, "srcts"))) app_dir <- getwd()
raw_dir <- file.path(app_dir, "data", "raw", "visium")
csv_dir <- file.path(app_dir, "data")
www_dir <- file.path(app_dir, "www", "spatial")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(www_dir, showWarnings = FALSE, recursive = TRUE)

BASE <- paste0("https://cf.10xgenomics.com/samples/spatial-exp/1.1.0/",
               "V1_Breast_Cancer_Block_A_Section_1/",
               "V1_Breast_Cancer_Block_A_Section_1_")
# Panel size. Must stay above canon + per-cluster picks or the truncation below
# silently drops the last clusters' markers, leaving them unrepresented in the
# dot plot.
N_GENES <- 80L
N_PER_CLUSTER <- 6L
# A gene has to actually be detected to be a marker. Without this floor the top
# of the fold-change ranking is genes seen in a handful of spots and nowhere
# else, which is how the panel filled up with lncRNAs and IG segments.
MIN_MEAN_COUNTS <- 0.25

download_once <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("cached: ", basename(dest)); return(invisible(dest))
  }
  message("downloading: ", basename(dest))
  utils::download.file(url, dest, mode = "wb", quiet = FALSE)
  invisible(dest)
}

spatial_tar <- file.path(raw_dir, "spatial.tar.gz")
analysis_tar <- file.path(raw_dir, "analysis.tar.gz")
matrix_tar <- file.path(raw_dir, "matrix.tar.gz")
download_once(paste0(BASE, "spatial.tar.gz"), spatial_tar)
download_once(paste0(BASE, "analysis.tar.gz"), analysis_tar)
download_once(paste0(BASE, "filtered_feature_bc_matrix.tar.gz"), matrix_tar)

for (f in c(spatial_tar, analysis_tar, matrix_tar)) {
  marker <- file.path(raw_dir, paste0(".extracted-", basename(f)))
  if (!file.exists(marker)) {
    utils::untar(f, exdir = raw_dir)
    file.create(marker)
  }
}

# --- 1. spot coordinates + scale factors -----------------------------------
# tissue_positions_list.csv has no header in v1.x:
#   barcode, in_tissue, array_row, array_col, pxl_row_in_fullres, pxl_col_in_fullres
pos <- utils::read.csv(file.path(raw_dir, "spatial", "tissue_positions_list.csv"),
                       header = FALSE, stringsAsFactors = FALSE)
names(pos) <- c("barcode", "in_tissue", "row", "col", "py", "px")
pos <- pos[pos$in_tissue == 1L, , drop = FALSE]
message("spots in tissue: ", nrow(pos))

sf <- fromJSON(file.path(raw_dir, "spatial", "scalefactors_json.json"))
# Full-resolution pixel coordinates scaled onto the low-res image we ship.
pos$x <- pos$px * sf$tissue_lowres_scalef
pos$y <- pos$py * sf$tissue_lowres_scalef
spot_d <- sf$spot_diameter_fullres * sf$tissue_lowres_scalef

invisible(file.copy(file.path(raw_dir, "spatial", "tissue_lowres_image.png"),
                    file.path(www_dir, "visium_he.png"), overwrite = TRUE))
img <- png::readPNG(file.path(www_dir, "visium_he.png"))
img_h <- dim(img)[1]; img_w <- dim(img)[2]
message("H&E low-res image: ", img_w, " x ", img_h)

# --- 2. graph-based clusters ----------------------------------------------
clus <- utils::read.csv(file.path(raw_dir, "analysis", "clustering",
                                  "graphclust", "clusters.csv"),
                        stringsAsFactors = FALSE)
pos$cluster <- clus$Cluster[match(pos$barcode, clus$Barcode)]
pos <- pos[!is.na(pos$cluster), , drop = FALSE]
message("clusters: ", length(unique(pos$cluster)))

# --- 3. marker gene panel --------------------------------------------------
# 10x ships its own per-cluster differential expression, so the panel is the
# dataset's own markers rather than a hand-picked list, plus a few canonical
# breast genes so the page can show something a reader recognises.
de <- utils::read.csv(file.path(raw_dir, "analysis", "diffexp", "graphclust",
                                "differential_expression.csv"),
                      stringsAsFactors = FALSE, check.names = FALSE)
lfc_cols <- grep("Log2 fold change", names(de), value = TRUE)
mc_cols <- grep("Mean Counts", names(de), value = TRUE)
ap_cols <- grep("Adjusted p value", names(de), value = TRUE)
# Rank by fold change within the genes that clear the detection floor, relaxing
# significance before the floor: a marker nobody can see is not a marker.
top_per_cluster <- unlist(lapply(seq_along(lfc_cols), function(k) {
  ok <- integer(0)
  for (p_cut in c(0.01, 0.05, 1)) {
    ok <- which(de[[ap_cols[k]]] < p_cut & de[[mc_cols[k]]] >= MIN_MEAN_COUNTS)
    if (length(ok) >= N_PER_CLUSTER) break
  }
  if (!length(ok)) return(character(0))
  ranked <- de[["Feature Name"]][ok][order(-de[[lfc_cols[k]]][ok])]
  ranked[seq_len(min(N_PER_CLUSTER, length(ranked)))]
}))
CANON <- c("ERBB2", "ESR1", "PGR", "MKI67", "KRT5", "KRT14", "KRT8", "KRT18",
           "CD3D", "PTPRC", "COL1A1", "ACTA2", "EPCAM", "VIM", "CD68", "MS4A1")
panel <- unique(c(CANON, top_per_cluster))

# --- 4. expression for the panel ------------------------------------------
mex <- file.path(raw_dir, "filtered_feature_bc_matrix")
feats <- utils::read.delim(gzfile(file.path(mex, "features.tsv.gz")),
                           header = FALSE, stringsAsFactors = FALSE)
bcs <- readLines(gzfile(file.path(mex, "barcodes.tsv.gz")))
message("reading MEX matrix ...")
m <- Matrix::readMM(gzfile(file.path(mex, "matrix.mtx.gz")))
rownames(m) <- feats$V2
colnames(m) <- bcs

panel <- panel[panel %in% rownames(m)]
panel <- panel[seq_len(min(N_GENES, length(panel)))]
message("marker panel: ", length(panel), " genes")

sub <- m[panel, pos$barcode, drop = FALSE]
# CP10K then log1p, the standard normalisation, using each spot's TOTAL counts
# rather than the panel's, so values are comparable to any other analysis.
totals <- Matrix::colSums(m[, pos$barcode, drop = FALSE])
expr <- log1p(t(t(as.matrix(sub)) / pmax(totals, 1) * 1e4))
expr <- round(expr, 3)

# --- write -----------------------------------------------------------------
spots <- data.frame(barcode = pos$barcode,
                    x = round(pos$x, 2), y = round(pos$y, 2),
                    cluster = paste("Cluster", pos$cluster),
                    stringsAsFactors = FALSE)
utils::write.csv(spots, file.path(csv_dir, "visium_spots.csv"),
                 row.names = FALSE)

expr_df <- data.frame(gene = rownames(expr), expr, check.names = FALSE,
                      stringsAsFactors = FALSE)
utils::write.csv(expr_df, file.path(csv_dir, "visium_expr.csv"),
                 row.names = FALSE)

meta <- list(dataset = "10x Visium, Human Breast Cancer (Block A Section 1)",
             source = "https://www.10xgenomics.com/datasets/human-breast-cancer-block-a-section-1-1-standard",
             license = "CC BY 4.0",
             nSpots = nrow(spots), nGenes = length(panel),
             image = "spatial/visium_he.png",
             imgWidth = img_w, imgHeight = img_h,
             spotDiameter = round(spot_d, 2))
write_json(meta, file.path(csv_dir, "visium_meta.json"),
           auto_unbox = TRUE, pretty = TRUE)

message("wrote data/visium_spots.csv  (", nrow(spots), " spots)")
message("wrote data/visium_expr.csv   (", length(panel), " genes)")
message("wrote www/spatial/visium_he.png")
message("wrote data/visium_meta.json")
