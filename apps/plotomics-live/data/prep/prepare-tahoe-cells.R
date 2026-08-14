#!/usr/bin/env Rscript
# Prepare a large per-cell sample from Tahoe-100M for the React scatter showcase.
#
# The Tahoe-100M obs_metadata table has ~100 million cells (2.3 GB parquet). We
# use duckdb to draw a uniform SAMPLE of 400,000 cells WITHOUT loading the file,
# keeping per-cell QC + cell-cycle values, then write compact binary column
# blobs (like the UMAP page) that the React engine streams to the GPU. This is
# the "power of Shiny React on large data" highlight: 400k individual cells,
# interactive, from a 100M-cell atlas.
#
# Pure duckdb + base R. Re-run to refresh. NEVER reads obs_metadata into R.

suppressWarnings(suppressMessages({ library(duckdb); library(DBI); library(jsonlite) }))

this_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
app_dir <- if (length(this_file) && !is.na(this_file))
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE) else getwd()
out_dir <- file.path(app_dir, "www", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Same as prepare-tahoe.R: the 2.3 GB parquet lives outside the repo, so the
# path comes from an argument, then TAHOE_OBS_METADATA, then data/raw/.
obs <- local({
  a <- commandArgs(TRUE)
  if (length(a) && nzchar(a[1])) return(a[1])
  env <- Sys.getenv("TAHOE_OBS_METADATA")
  if (nzchar(env)) return(env)
  file.path(app_dir, "data", "raw", "obs_metadata.parquet")
})
if (!file.exists(obs)) {
  stop("Tahoe obs_metadata.parquet not found at ", obs, "\n",
       "Pass the path as an argument, set TAHOE_OBS_METADATA, or drop the ",
       "file in data/raw/.")
}

N <- 400000L
con <- dbConnect(duckdb()); on.exit(dbDisconnect(con, shutdown = TRUE))
message("sampling ", N, " cells from Tahoe-100M (duckdb)…")
df <- dbGetQuery(con, sprintf(
  "SELECT tscp_count, gene_count, pcnt_mito, S_score, G2M_score, phase, cell_name
   FROM read_parquet('%s')
   WHERE pass_filter = 'full'
   USING SAMPLE %d ROWS (reservoir, 42)", obs, N))
message("got ", nrow(df), " cells")

palette <- c(
  "#0E7175","#ED773C","#708C69","#C63F3E","#808BC5","#E4A25B","#245E55","#9E3F71",
  "#56B4E9","#EAC119","#5B5F8D","#9BB29E","#013D5A","#DA6B51","#66A182","#EAA7C7",
  "#89973D","#9298BA","#B96A8D","#5DB7C4","#E88170","#82A0C2","#C8C8A9","#98C1D9")
encode <- function(v) {
  v[is.na(v)] <- "NA"
  f <- factor(v); levs <- levels(f)
  ord <- order(-table(f)); levs <- levs[ord]; f <- factor(v, levels = levs)
  list(codes = as.integer(f) - 1L, levels = levs,
       colors = palette[((seq_along(levs) - 1L) %% length(palette)) + 1L])
}

wf32 <- function(x, f) writeBin(as.numeric(x), file.path(out_dir, f), size = 4L, endian = "little")
wi16 <- function(x, f) writeBin(as.integer(x), file.path(out_dir, f), size = 2L, endian = "little")

ltscp <- log10(pmax(df$tscp_count, 0) + 1)
lgene <- log10(pmax(df$gene_count, 0) + 1)
wf32(ltscp, "tahoe_ltscp.f32")
wf32(lgene, "tahoe_lgene.f32")
wf32(df$S_score, "tahoe_s.f32")
wf32(df$G2M_score, "tahoe_g2m.f32")

ph <- encode(df$phase); cl <- encode(df$cell_name)
wi16(ph$codes, "tahoe_phase.i16")
wi16(cl$codes, "tahoe_cellline.i16")

meta <- list(
  dataset = "Tahoe-100M (Arc Institute)", n = nrow(df),
  axes = list(
    qc = list(label = "QC: UMIs × genes", x = "tahoe_ltscp.f32", y = "tahoe_lgene.f32",
              xLabel = "log10 UMIs / cell", yLabel = "log10 genes / cell"),
    cellcycle = list(label = "Cell cycle: S × G2M", x = "tahoe_s.f32", y = "tahoe_g2m.f32",
                     xLabel = "S phase score", yLabel = "G2M phase score")
  ),
  fields = list(
    phase = list(label = "Cell-cycle phase", file = "tahoe_phase.i16",
                 levels = ph$levels, colors = ph$colors),
    cell_line = list(label = "Cell line", file = "tahoe_cellline.i16",
                     levels = cl$levels, colors = cl$colors)
  )
)
write_json(meta, file.path(out_dir, "tahoe_cells_meta.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE)
message("done: wrote ", nrow(df), " cells to ", out_dir)
