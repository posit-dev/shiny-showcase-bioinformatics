#!/usr/bin/env Rscript
# Prepare the large single-cell UMAP embedding for the React showcase page.
#
# Source: UCSC Cell Browser -- "Human Cell Landscape" (Han et al. 2020),
# ~584k cells across whole-body organs, colored by cell type.
#   https://cells.ucsc.edu/?ds=human-cellular-landscape
#
# Why binary, not JSON over the websocket: ~584k points is far past what is
# comfortable to ship as JSON on Shiny's socket. Instead we precompute compact
# little-endian binary column blobs (Float32 coords + Int16 category codes) that
# the React client fetches over plain HTTP and feeds straight into the WebGL
# scatter as typed arrays -- the fast path plotomics/regl-scatterplot want. This
# is exactly why the React view loads instantly while ggplot2 must subsample.
#
# Pure base R + readr + jsonlite (no python / anndata / arrow needed). Re-run to
# refresh; downloads are cached in data/raw/.

suppressWarnings(suppressMessages({
  library(readr)
  library(jsonlite)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
raw_dir  <- file.path(app_dir, "data", "raw")
out_dir  <- file.path(app_dir, "www", "data")
csv_dir  <- file.path(app_dir, "data")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

BASE     <- "https://cells.ucsc.edu/human-cellular-landscape"
coords_f <- file.path(raw_dir, "UMAP.coords.tsv.gz")
meta_f   <- file.path(raw_dir, "meta.tsv")

download_once <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("cached: ", basename(dest)); return(invisible())
  }
  message("downloading: ", url)
  utils::download.file(url, dest, mode = "wb", quiet = FALSE)
}

download_once(file.path(BASE, "UMAP.coords.tsv.gz"), coords_f)
download_once(file.path(BASE, "meta.tsv"), meta_f)

# --- read ------------------------------------------------------------------
message("reading coords ...")
coords <- read_tsv(coords_f, col_names = c("cellId", "x", "y"),
                   col_types = "cdd", progress = FALSE)

message("reading meta ...")
meta <- read_tsv(meta_f, col_types = cols_only(
  cellId = col_character(),
  cell_type = col_character(),
  organ = col_character()
), progress = FALSE)

# Align meta to coord order by cellId.
idx <- match(coords$cellId, meta$cellId)
cell_type <- meta$cell_type[idx]
organ     <- meta$organ[idx]
cell_type[is.na(cell_type)] <- "Unknown"
organ[is.na(organ)]         <- "Unknown"

n <- nrow(coords)
message(sprintf("aligned %d cells", n))

# --- categorical encoding --------------------------------------------------
# Okabe-Ito + extended colorblind-safe palette (matches plotomics OKABE_ITO).
palette <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9",
  "#F0E442", "#000000", "#8C564B", "#17BECF", "#BCBD22", "#7F7F7F",
  "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94",
  "#F7B6D2", "#DBDB8D", "#9EDAE5", "#393B79", "#637939", "#8C6D31"
)
encode <- function(v) {
  f <- factor(v)
  levs <- levels(f)
  # order levels by frequency (largest first) so the legend & palette lead with
  # the biggest populations.
  ord <- order(-table(f))
  levs <- levs[ord]
  f <- factor(v, levels = levs)
  list(codes = as.integer(f) - 1L, levels = levs,
       colors = palette[((seq_along(levs) - 1L) %% length(palette)) + 1L])
}
ct <- encode(cell_type)
og <- encode(organ)

# --- write binary column blobs (little-endian) -----------------------------
wf32 <- function(x, f) writeBin(as.numeric(x), f, size = 4L, endian = "little")
wi16 <- function(x, f) writeBin(as.integer(x), f, size = 2L, endian = "little")

wf32(coords$x, file.path(out_dir, "umap_x.f32"))
wf32(coords$y, file.path(out_dir, "umap_y.f32"))
wi16(ct$codes, file.path(out_dir, "umap_celltype.i16"))
wi16(og$codes, file.path(out_dir, "umap_organ.i16"))

meta_json <- list(
  dataset = "Human Cell Landscape (Han et al. 2020)",
  source  = "https://cells.ucsc.edu/?ds=human-cellular-landscape",
  n = n,
  bounds = list(minX = min(coords$x), maxX = max(coords$x),
                minY = min(coords$y), maxY = max(coords$y)),
  fields = list(
    cell_type = list(label = "Cell type", file = "umap_celltype.i16",
                     levels = ct$levels, colors = ct$colors),
    organ     = list(label = "Organ", file = "umap_organ.i16",
                     levels = og$levels, colors = og$colors)
  )
)
write_json(meta_json, file.path(out_dir, "umap_meta.json"),
           auto_unbox = TRUE, digits = 6, pretty = TRUE)

# --- a downsampled CSV for the (deliberately slower) ggplot2 side ----------
set.seed(1)
cap <- min(n, 40000L)
s <- sort(sample.int(n, cap))
gg <- data.frame(x = coords$x[s], y = coords$y[s],
                 cell_type = cell_type[s], organ = organ[s])
write_csv(gg, file.path(csv_dir, "umap_ggplot_sample.csv"))

message(sprintf("done: wrote %d binary cells to %s (+ %d-cell ggplot sample)",
                n, out_dir, cap))
