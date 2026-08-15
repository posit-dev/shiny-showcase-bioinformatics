# Plotomics Live - biological visualizations, two ways

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.21936926-1682D4)](https://doi.org/10.5281/zenodo.21936926)

By [Samuel Bharti](https://www.samuelbharti.com)

Plotomics Live is a Shiny **React** (TSX) app. It shows twenty-six
biological-data visualizations, each rendered **two ways**. With the engine
toggle, you can compare the two renderings side by side:

- **Shiny React** - an interactive, GPU-accelerated TSX component (plotomics /
  WebGL, sigma, igv.js, 3Dmol.js), and
- **ggplot2 (classic)** - a server-rendered image (the traditional R path).

The whole UI is React. It has no bslib or Bootstrap component. The server
renders ggplot2 plots as base64 PNGs and shows them in an `<img>` element. As
a result, both engines always show the same server-side computation.

## The visualizations

The 26 visualizations are grouped into five analysis areas (the same grouping
drives the category dropdown nav and the home page).

### Single-cell & spatial

| Page | Shiny React (TSX) | ggplot2 / classic |
|------|-------------------|-------------------|
| Single-cell UMAP | **584,207 real cells** streamed as binary typed arrays → WebGL | ggplot2 (40k subsample) |
| Tahoe-100M perturbation | drug×cell-line coverage clustermap **+ a 380k-cell cell-cycle scatter** (real Tahoe data) | ggplot2 `geom_tile` |
| Visium spatial transcriptomics | plotomics `spatial` (H&E underlay + canvas spots, one shared transform) | ggplot2 `annotation_raster` + `geom_point` |
| Xenium single-molecule transcripts | plotomics `embedding` (WebGL, 1M mRNA detections streamed as binary blobs) | ggplot2 `geom_point` on a 40k subsample |
| Marker gene dot plot | plotomics `dotplot` (canvas dots, area-proportional size + colour) | ggplot2 `geom_point` with `scale_size_area` |
| Stacked violin | plotomics `violin` (canvas densities, per-feature y ranges) | ggplot2 `geom_polygon` from the same densities, faceted |

### Gene expression

| Page | Shiny React (TSX) | ggplot2 / classic |
|------|-------------------|-------------------|
| Volcano | plotomics `volcano` (WebGL) | ggplot2 + ggrepel |
| Expression heatmap | plotomics `heatmap` | ggplot2 `geom_tile` |
| Clustered heatmap | plotomics `clustermap` (in-browser clustering + dendrograms) | base-R `heatmap()` |
| PCA explorer | plotomics `embedding` for scores, `profile` for scree and loadings | ggplot2 `geom_point` + `stat_ellipse`, `geom_col` |

### Cancer genomics

| Page | Shiny React (TSX) | ggplot2 / classic |
|------|-------------------|-------------------|
| Oncoplot (OncoPrint) | plotomics `oncoplot` (canvas grid + marginal bars + clinical strips) | five ggplot2 panels aligned with `patchwork` |
| Domain lollipop | plotomics `lollipop` (canvas stems + Pfam domains + PTM track) | ggplot2 `geom_segment` + `geom_point` + `ggrepel` |
| Mutation treemap | plotomics `treemap` | hand-rolled ggplot2 treemap |
| Mutational signatures (SBS96) | plotomics `profile` (96 canvas bars + six-block banner) | ggplot2 `geom_col` + hand-rolled banner |
| Driver co-occurrence (UpSet) | plotomics `upset` (canvas bars + membership matrix) | three ggplot2 panels aligned with `patchwork` |
| Kaplan-Meier survival | plotomics `km` (canvas step curves + Greenwood band + risk table) | ggplot2 `geom_line` on expanded steps + `patchwork` risk table |

### Genome & epigenome

| Page | Shiny React (TSX) | ggplot2 / classic |
|------|-------------------|-------------------|
| Manhattan + QQ (GWAS) | canvas-2D scatter with a chromosome axis | ggplot2 by chromosome + QQ |
| eQTL / pQTL effect map | plotomics `heatmap` (diverging β) | ggplot2 `geom_tile` |
| Genome browser (IGV) | igv.js (hg19 + variant track) | ggplot2 variant needle plot |
| Gosling genome view | Gosling.js declarative JSON spec (loaded from CDN) | (spec-driven; no classic equivalent) |
| Hi-C contact matrix | plotomics `heatmap` (log contacts) | ggplot2 `geom_raster` |
| Single-cell ATAC coverage | canvas-2D per-cluster coverage tracks | ggplot2 faceted `geom_area` |

### Structure & networks

| Page | Shiny React (TSX) | ggplot2 / classic |
|------|-------------------|-------------------|
| Protein structure | 3Dmol.js (AlphaFold, coloured by pLDDT) | ggplot2 per-residue pLDDT profile |
| AlphaFold PAE matrix | plotomics `heatmap` (residue × residue error) + canvas row profile | ggplot2 `geom_raster` + `geom_area` profile |
| Gene network | plotomics `network` (sigma/WebGL, ~1.5k nodes / 7.4k edges) | igraph layout + ggplot2 |
| N-dimensional array viewer | plotomics `heatmap` slice + channel slider + per-pixel spectrum (WebGL) | ggplot2 `geom_raster` + spectrum |

The **UMAP** view is the headline feature. About 584,000 real cells arrive as
about 7 MB of binary column blobs over plain HTTP, and the GPU renders them
instantly. ggplot2 can show only a static subsample. This contrast is the
point.

Every visualization panel has a **full-screen** button. A floating **advisory
chat assistant** sits in the bottom-right corner. It answers questions about
the app, about each visualization, and about which visualization fits a given
dataset. It also gives a rough sense of how much data each visualization can
render.

The chat assistant is **bring-your-own-key**. Paste a Gemini, OpenAI, or
Anthropic key into the assistant's key panel to chat with a live model,
through [`ellmer`](https://ellmer.tidyverse.org). With no key, the assistant
falls back to a built-in guide, so the app still works offline. The key stays
in server memory for the life of the session. The app never stores it on
disk. The assistant is advice-only: it has no tools, so it cannot control the
app.

## Architecture

- **Backend:** R + [`shinyreact`](https://github.com/posit-dev/shinyreact),
  `ui.tsx` pattern. `app.R` is compute-only: one `reactive_output()` feed per
  visualization (both the plotomics data contract *and* a ggplot2 PNG). The
  React client reads them via `useShinyOutputValue` off `window.shinyreact`.
- **Frontend:** TSX built with Vite (IIFE, React externalized to
  `window.shinyreact`). React Router uses a `HashRouter`, with one route per
  visualization. The visualizations are grouped into five analysis-area
  categories: single-cell and spatial, gene expression, cancer genomics,
  genome and epigenome, and structure and networks. The top nav shows one
  dropdown menu per category, and the home page lays out the cards under the
  same headings. The theme is a light, organic palette derived from the
  [LTC color palettes](https://github.com/loukesio/ltc-color-palettes).
- **Reuse:** the [plotomics](https://github.com/samuelbharti/plotomics) headless component
  factories (wrapped with one thin `PlotomicsView` lifecycle component),
  ggplot2 renderers adapted from `lifescience-shiny-gallery`, and a Tahoe
  coverage matrix prepared from `tahoe-explorer` via duckdb.

```text
app.R                server: reactive_output feeds + ggplot2->PNG
R/{data,plots,palettes}.R   shiny-free data + rendering layer
data/                bundled CSVs + data/prep/ refresh scripts (+ PROVENANCE.md)
www/                 index.html, built app.js, app.css, data/ (UMAP binary blobs)
srcts/               main.tsx, App.tsx, lib/, components/, pages/ (TSX sources)
```

## Run

```bash
# 1. build the frontend (Node 18+)
npm install
npm run build            # -> www/app.js

# 2. run the Shiny app (R 4.x)
R -e "shiny::runApp('.', port = 8000)"
# open http://127.0.0.1:8000
```

To refresh or regenerate the data, run the scripts in `data/prep/` (see
`data/prep/PROVENANCE.md`). The repository already includes the UMAP blobs
and the Tahoe matrix, so the app runs offline by default.

## Deployment

The app deploys to Posit Connect or Connect Cloud as a plain Shiny app.
`.rscignore` (plus `data/.rscignore`) keeps the frontend build sources,
repo-health files, and any local cache out of the deployment bundle; only
`app.R`, `R/`, `data/` (without `data/raw/`), and the built `www/` ship.

`manifest.json` is a snapshot of the R package versions the app needs, and it
goes out of date as dependencies change. Rebuild it with `npm run build`
first, so `www/app.js` is current, then run:

```bash
R -e "rsconnect::writeManifest(appDir = '.')"
```

## Notes / known limits

- WebGL views need a browser with hardware acceleration. The software
  renderer in some headless setups draws these views incorrectly. An error
  boundary keeps a failed view from blanking the whole app.
- The Hi-C React view uses the heatmap factory, because a contact map is a
  heatmap. The dedicated `hic` factory needs `OES_texture_float`, and current
  Chrome no longer exposes that extension.
- The IGV browser streams its genome reference from the igv.js data servers.
  It needs open network access to those hosts.

## Large / high-dimensional data

The N-dimensional array viewer demonstrates the microscopy and geoscience use
case (zarr, xarray, HyperSpy) with a WebGL slice and spectrum, and needs no
Python. For datasets beyond the client-side limit of WebGL (tens of millions
to billions of points), [datashader](https://datashader.org) is the right
tool. However, datashader is Python-only and rasterizes on the server. The
clean way to use it here is with offline, pre-rasterized image tiles served
as static files, not a live in-app renderer.

## Future visualizations (easy to add)

A GO / pathway enrichment plot and an MDS ordination.

## Contributing

Issues and pull requests are welcome. Read [CONTRIBUTING.md](CONTRIBUTING.md)
first, and please follow the [Code of Conduct](CODE_OF_CONDUCT.md).

For a security problem, do not open a public issue: [SECURITY.md](SECURITY.md)
explains how to report it privately.

## Author

Samuel Bharti

- Email: <samuelbharti.io@gmail.com>
- Web: [samuelbharti.com](https://www.samuelbharti.com)
- ORCID: [0000-0003-4190-7058](https://orcid.org/0000-0003-4190-7058)
- GitHub: [@samuelbharti](https://github.com/samuelbharti)

## Citation

Zenodo archives each release. The badge at the top of this file resolves to the
latest version; to cite one specific version, use that version's DOI from the
[Zenodo record](https://doi.org/10.5281/zenodo.21936926).
[CITATION.cff](CITATION.cff) holds the full metadata, and
[CITATION.md](CITATION.md) gives a ready-made text and BibTeX entry.

## License

The code is [MIT](LICENSE). Copyright (c) 2026 Samuel Bharti.

The bundled datasets are **not** MIT-licensed. Each one keeps the license of
its own source, and those terms travel with the data if you redistribute this
repository. [DATA-LICENSES.md](DATA-LICENSES.md) summarizes them, and
`data/prep/PROVENANCE.md` lists the license per dataset.
