# Recount Explorer

[![lint](https://github.com/samuelbharti/recount-explorer/actions/workflows/lint.yml/badge.svg)](https://github.com/samuelbharti/recount-explorer/actions/workflows/lint.yml)
[![test](https://github.com/samuelbharti/recount-explorer/actions/workflows/test.yml/badge.svg)](https://github.com/samuelbharti/recount-explorer/actions/workflows/test.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Recount Explorer is a Shiny app that shows RNA-seq studies from the
[recount3](https://bioconductor.org/packages/recount3/) project. recount3 holds
human and mouse RNA-seq samples. One pipeline processed all of these samples in
the same way. The samples come from SRA, GTEx, TCGA, and four smaller sources.

You select a study from the catalog. The app loads that study through the
recount3 API and draws the plots. You write no code.

## Views

- **Browse studies**: All 18,998 studies, with no waiting and no network call.
  One search box covers the accession, the title, and the abstract text.
  Filters sit on the left and the study you select opens on the right. The app
  loads a study on a background process, so it stays responsive, and reports
  each step of the load rather than showing a spinner.
- **Study overview**: The headline numbers for the study, a quality plot of
  library size against detected genes, and the spread of log2 CPM in each
  sample. The sample metadata sits below in its own table. It shows a short
  set of columns and never scrolls sideways; pick more columns from the list
  above it, or select a sample to read every field recorded for it.
- **Quality**: The checks to run before you trust a study, all built from data
  the study already carries. The metric panel plots what recount3 measured
  during alignment: how much of each sample mapped, how much landed in genes,
  and how much is mitochondrial or intronic. Library composition shows the
  share of reads by gene biotype, which is how you spot rRNA or globin. The
  donor sex plot separates samples on chromosome X and Y reads. The
  correlation heatmap clusters the samples and shows batch structure that PCA
  can hide.
- **Gene explorer**: Search for one gene. The app plots the expression of that
  gene as log2 CPM. You can split the plot by a metadata column.
- **PCA**: Principal component analysis of the samples. The app uses the genes
  with the highest variance. You can color the points by metadata. The
  loadings plot names the genes that push a component hardest, which is why
  the samples separate.
- **Export**: Download the RangedSummarizedExperiment (`.rds`), the log2 CPM
  matrix (`.csv.gz`), and the sample metadata (`.csv`). The app also writes the
  session as an R script, a Quarto notebook, or an R Markdown notebook. The
  overview, quality, gene and PCA views download each of their plots as a PDF.
- **About**: What the app is, where the data comes from, how to cite it.

The interface follows your system light or dark setting, and there is a toggle
in the navigation bar. The plots follow it too. A downloaded PDF stays light. A
figure going into a document belongs on a white background.

## Theme

Colours live in [`_brand.yml`](_brand.yml), in the
[brand.yml](https://posit-dev.github.io/brand-yml/) format. bslib reads it to
build the Bootstrap theme. `R/logic_brand.R` reads the same file for the
ggplot2 figures. The buttons, the tables and the plots always agree.

The palette is coffee: roasted browns on cream, with caramel in place of the
usual Bootstrap blue. To try another palette, edit that file, or point
`RECOUNT_EXPLORER_BRAND` at a different one.

## Run it

```r
install.packages("renv")
renv::restore()

shiny::runApp()
```

`renv::restore()` installs the package versions that `renv.lock` records. That
includes the Bioconductor packages.

There is no build step and no JavaScript toolchain. The interface is bslib, so
every part of it is R code you can edit and reload.

The catalog comes from a snapshot in `data/`, so the study list appears at once
and needs no network. Loading a study downloads it from the recount3 servers.
recount3 keeps a local copy of each download through BiocFileCache. Later
loads of the same study are fast.

Every study shows its download size before you load it, and you can filter by
size. Most studies are small: 18,323 of the 18,998 are under 10 MB, and only
four are over 200 MB.

CAUTION: the app refuses any study over 500 samples, which is about 1 GB of
memory. That allows 98.7 percent of the catalog and stops the 238 studies that
would exhaust a server. Set `RECOUNT_EXPLORER_MAX_SAMPLES` to raise the limit
on a machine with memory to spare.

To warm the cache before a demo or a workshop, so nothing waits on a download:

```sh
Rscript data-raw/prefetch_studies.R --max-samples 50 --limit 20 --dry-run
Rscript data-raw/prefetch_studies.R SRP107565 DRP000425
```

## Structure

```
app.R                    App layer: page layout, mirai daemons, module wiring
R/
  logic_catalog.R        Catalog snapshot, study titles and abstracts (Shiny-free)
  logic_recount.R        recount3 access: study load, log2 CPM (Shiny-free)
  logic_analysis.R       QC, PCA, quality metrics, per-gene frames (Shiny-free)
  logic_plots.R          Plot builders shared by views and PDF downloads (Shiny-free)
  logic_export.R         Reproduction script builder, CSV export frames (Shiny-free)
  mod_study_browser.R    Catalog browsing, background load, returns the study reactive
  mod_study_overview.R   Study header, QC and distribution plots, metadata table
  mod_quality.R          Quality metrics, biotypes, donor sex, sample correlation
  mod_gene_explorer.R    Per-gene expression by group, plot PDF download
  mod_pca_explorer.R     Sample-level PCA and gene loadings, plot PDF downloads
  mod_export.R           Data downloads and reproduction script
  utils.R                Small helpers
tests/testthat/          Logic and module tests. They use a fixture, never the network
www/app.css              Navbar, card and footer theming, the tour, and the
                         stat tiles and spacing bslib has no opinion on
docs/design.md           Architecture and design notes
```

The computation sits in a Shiny-free logic layer. You can test that layer
without a running app. Each view is a Shiny module. [docs/design.md](docs/design.md)
explains why.

## Development

```sh
prek install        # install the git hooks: air, lintr, secret scanning
air format .        # format the R code
```

```r
install.packages(c("testthat", "lintr"))   # dev tools, not in renv.lock
lintr::lint_dir(".")
testthat::test_dir("tests/testthat")
```

[CONTRIBUTING.md](CONTRIBUTING.md) has the details. It covers the branch
convention and the rebuild of the catalog snapshot.

## Citation

If you use this app in your work, cite both this app and the recount3 paper.
[CITATION.cff](CITATION.cff) holds the same information in machine-readable
form.

> Wilks C, Zheng SC, Chen FY, et al. recount3: summaries and queries for
> large-scale RNA-seq expression and splicing. *Genome Biology* 22, 323 (2021).
> https://doi.org/10.1186/s13059-021-02533-6

## Author

**Samuel Bharti**

[www.samuelbharti.com](https://www.samuelbharti.com) ·
[samuelbharti.io@gmail.com](mailto:samuelbharti.io@gmail.com) ·
[ORCID 0000-0003-4190-7058](https://orcid.org/0000-0003-4190-7058) ·
[GitHub](https://github.com/samuelbharti)

## License

MIT. See [LICENSE](LICENSE). The recount3 project sets the terms for the
recount3 data itself. Read [rna.recount.bio](https://rna.recount.bio/) for
those terms.
