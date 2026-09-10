# Renders each artboard in thumbnails/src/ to thumbnails/<app>.png at 2400x1600,
# the size a card expects. Run build_thumbs.py first; it writes the artboards.
#
#   python thumbnails/src/build_thumbs.py
#   Rscript --no-init-file thumbnails/src/render_boards.R
#
# --no-init-file skips the renv profile. The script needs chromote alone, and
# Chrome loads the fonts from Google Fonts, so it needs the network.
#
# For a slide, pass a larger scale: SCALE=2 gives 4800x3200.
library(chromote)

src <- normalizePath("thumbnails/src", winslash = "/")
out <- normalizePath("thumbnails", winslash = "/")
scale <- as.numeric(Sys.getenv("SCALE", "1"))

boards <- c(
  "Main.dc.html" = "tahoe-explorer",
  "GeneScout.dc.html" = "genescout",
  "PlotomicsLive.dc.html" = "plotomics-live",
  "VariantReviewer.dc.html" = "variant-reviewer",
  "GeneListBuilder.dc.html" = "gene-list-builder",
  "DEExplorer.dc.html" = "de-explorer",
  "SignatureScoring.dc.html" = "signature-scoring",
  "DrugPerturbation.dc.html" = "drug-perturbation",
  "GenomeExplorer.dc.html" = "genome-explorer",
  "RecountExplorer.dc.html" = "recount-explorer"
)

b <- ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(width = 1200, height = 800, deviceScaleFactor = 2, mobile = FALSE)
b$Emulation$setScrollbarsHidden(hidden = TRUE)
for (f in names(boards)) {
  b$Page$navigate(paste0("file:///", src, "/", f))
  b$Page$loadEventFired()
  Sys.sleep(2.5)
  res <- b$Page$captureScreenshot(
    format = "png",
    clip = list(x = 0, y = 0, width = 1200, height = 800, scale = scale)
  )
  suffix <- if (scale == 1) "" else paste0("-", 2400 * scale)
  path <- file.path(out, paste0(boards[[f]], suffix, ".png"))
  writeBin(jsonlite::base64_dec(res$data), path)
  message("Saved: ", path)
}
b$close()
