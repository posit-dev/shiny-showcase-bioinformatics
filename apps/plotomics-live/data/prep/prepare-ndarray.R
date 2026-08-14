#!/usr/bin/env Rscript
# Generate a synthetic hyperspectral image cube for the N-dimensional array
# viewer: shape (ny, nx, nchannel). Several spatial "features" (Gaussian blobs)
# each light up in a different channel, so a 2-D slice looks different per
# channel and each pixel has a meaningful spectrum.
#
# Written as one Float32 blob in CHANNEL-MAJOR order, and within each channel in
# Y-MAJOR (x fastest) order, so a channel slice is contiguous and already in the
# row-major layout the plotomics heatmap expects (index = ch*ny*nx + y*nx + x).
# Both the React client and the R (ggplot) side read this same blob.

suppressWarnings(suppressMessages(library(jsonlite)))
this_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
app_dir <- if (length(this_file) && !is.na(this_file))
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE) else getwd()
out_dir <- file.path(app_dir, "www", "data"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ny <- 100L; nx <- 100L; nch <- 24L
set.seed(41)
# features: cx, cy (pixel), sigma (px), peak channel, channel width, amplitude
feats <- data.frame(
  cx = c(28, 70, 50, 78, 22),
  cy = c(30, 32, 62, 74, 76),
  sg = c(10, 8, 14, 7, 9),
  pch = c(4, 10, 15, 19, 22),
  cw = c(2.5, 2, 3, 2, 2.5),
  amp = c(1, 0.9, 1.1, 0.8, 0.95))

xs <- matrix(rep(seq_len(nx), each = ny), ny, nx)   # xs[y,x] = x
ys <- matrix(rep(seq_len(ny), times = nx), ny, nx)   # ys[y,x] = y

vals <- numeric(nch * ny * nx)
i <- 1L
for (ch in seq_len(nch)) {
  img <- matrix(0.03, ny, nx)                        # faint background
  for (f in seq_len(nrow(feats))) {
    spatial <- feats$amp[f] * exp(-((xs - feats$cx[f])^2 + (ys - feats$cy[f])^2) / (2 * feats$sg[f]^2))
    spectral <- exp(-((ch - feats$pch[f])^2) / (2 * feats$cw[f]^2))
    img <- img + spatial * spectral
  }
  img <- img + matrix(abs(rnorm(ny * nx, 0, 0.01)), ny, nx)
  # write Y-MAJOR (x fastest): for y, for x
  block <- as.numeric(t(img))                        # t() -> row-major of img (x fastest)
  vals[i:(i + ny * nx - 1L)] <- block
  i <- i + ny * nx
}

writeBin(vals, file.path(out_dir, "ndarray.f32"), size = 4L, endian = "little")
write_json(list(dataset = "Synthetic hyperspectral cube", ny = ny, nx = nx, nch = nch),
           file.path(out_dir, "ndarray_meta.json"), auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("wrote cube %dx%dx%d (%d floats) to %s\n", ny, nx, nch, length(vals), out_dir))
