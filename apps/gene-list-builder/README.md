# Gene List Builder

A lightweight Shiny app for building a curated gene list for a disease: "GDA
(gene–disease association) for gene lists". It replaces the manual,
API-by-hand curation workflow with a reproducible pipeline:

1. **Resolve** a disease name to an ontology term (EFO/MONDO) via Open Targets.
2. **Query** multiple gene–disease-association sources.
3. **Aggregate + dedupe** the results into a common schema (one row per gene).
4. **Rank** the genes with transparent, tunable, source-weighted scoring.
5. **Curate** the final list with an AI agent (Google Gemini), on demand.
6. **Export** the gene list (CSV) and a run report (Markdown).

Current app version: v0.2.0

## Data sources

**Evidence sources** add candidate genes and count toward the multi-source
coverage bonus:

| Source | Driven by | Key | Notes |
| --- | --- | --- | --- |
| [Open Targets](https://platform.opentargets.org/) | disease | no | Overall association score (0–1); also resolves the disease name to an ontology id. |
| [PanelApp](https://panelapp.genomicsengland.co.uk/) | disease | no | Genomics England curated diagnostic panels; green/amber confidence → score. |
| [DISEASES](https://diseases.jensenlab.org/) | disease | no | Text-mined + curated disease–gene associations (keyed by DOID via OT xrefs). |
| [ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/) | disease | no¹ | Genes with pathogenic/likely-pathogenic variants (NCBI E-utilities). |
| [DGIdb](https://dgidb.org/) | gene | no | Drug–gene interaction count (druggability). |

**Annotation sources** (prioritizers) score genes to nudge ranking but do *not*
inflate coverage:

| Source | Driven by | Key | Notes |
| --- | --- | --- | --- |
| [gnomAD](https://gnomad.broadinstitute.org/) | gene | no | Loss-of-function constraint (LOEUF); more constrained → higher score. |
| [Pharos](https://pharos.nih.gov/) | gene | no | Target Development Level (Tclin → Tdark). |

¹ ClinVar needs no key; an optional `ENTREZ_KEY` raises the E-utilities rate limit.

Sources are registered through an extensible registry (`R/source_registry.R`).
Each entry declares `needs` (`disease` or `genes`) and `role` (`evidence` or
`annotation`). Adding DisGeNET, OMIM, GWAS Catalog, or others is a matter of
writing one adapter that returns the canonical schema and calling
`register_source()`. No pipeline changes are required.

## Requirements

- R (>= 4.3)
- Packages: `shiny`, `bslib`, `brand.yml`, `dplyr`, `tibble`, `tidyr`, `rlang`,
  `DT`, `shinycssloaders`, `ellmer`, `shinyvalidate`, `cicerone`, `shinyjs`, plus
  `biohttp`, `bioclients`, and `biobouncer` (HTTP transport, database clients,
  and identifier validation).

## Installation

### Option 1: renv (reproducible)

`renv.lock` pins every dependency, including the exact `biobouncer` version.
That matters for reproducibility: `biobouncer` bundles the HGNC snapshot used
for the `symbol_status` column, so the package version determines the result.

```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()
```

### Option 2: install directly

`biohttp`, `bioclients`, and `biobouncer` are not on CRAN; they are served from
an r-universe repo, so a single `install.packages()` call covers everything:

```r
install.packages(
  c(
    "shiny", "bslib", "brand.yml", "dplyr", "tibble", "tidyr",
    "rlang", "DT", "shinycssloaders", "ellmer", "shinyvalidate",
    "cicerone", "shinyjs",
    "biohttp", "bioclients", "biobouncer"
  ),
  repos = c(
    "https://samuelbharti.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Configuration (API keys)

The app runs fully without any keys (AI curation degrades to a top-N-by-rank
fallback). To enable Gemini curation, copy `.Renviron.example` to `.Renviron`
(git-ignored) and set:

```sh
GEMINI_API_KEY=your-key-here
# optional, defaults to gemini-flash-lite-latest
GLB_GEMINI_MODEL=gemini-flash-lite-latest
```

Open Targets and DGIdb are public and need no credentials.

Caching happens in two tiers. Finished per-source gene tables are stored under
`data/cache/` (git-ignored, override with `GLB_CACHE_DIR`), and the sidebar
"Rebuild from source" toggle bypasses that tier. Underneath, `biohttp` also
caches the HTTP responses themselves in memory for a short TTL, so that toggle
does not guarantee a fresh network call; set `BIOHTTP_CACHE_TTL` to shorten it.

## How to run

```r
shiny::runApp()
```

Or open the project in RStudio and click **Run App**. Enter a disease (e.g.
"lung cancer"), resolve and select the matching term, choose sources, click
**Build gene list**, tune the ranking weights, then optionally **Curate with
AI** and export.

## Build and run with Docker

```bash
docker build -t gene-list-builder .
docker run --rm -p 3838:3838 -e GEMINI_API_KEY=your-key gene-list-builder
```

Then open [http://localhost:3838](http://localhost:3838).

## Scoring

For each gene:

```txt
weighted_evidence = sum over present sources of weight * normalized_score
coverage_factor   = 1 + bonus * (n_sources - 1) / (n_total - 1)
combined_score    = weighted_evidence * coverage_factor
```

Open Targets/PanelApp scores pass through (already 0–1); count-based scores
(DISEASES, ClinVar, DGIdb) are rank-normalized within the source; gnomAD is
rank-normalized so lower LOEUF scores higher. Annotation sources (gnomAD,
Pharos) contribute to the weighted score but `n_sources`/coverage count evidence
sources only, so a constraint metric doesn't inflate the multi-source bonus.
Genes present in only some sources are not zero-imputed; breadth of evidence is
rewarded explicitly via the coverage factor. Weights and the multi-source bonus
are tunable live in the sidebar and re-rank instantly without re-querying.

## Symbol quality and alias merging

The aggregated table carries a `symbol_status` column from `biobouncer`'s
bundled (offline) HGNC snapshot: `valid`, `alias of <X>`, `microRNA (not HGNC)`,
or `unknown`. It is diagnostic and never affects scoring.

Sources sometimes name the same gene differently (`KMT2A` vs the legacy `MLL`),
which splits one gene into two rows, halves its evidence and understates the
multi-source coverage bonus. Setting `GLB_REPAIR_SYMBOLS=1` merges an alias
onto its modern symbol, but **only when that modern symbol was already reported
by another source in the same run**. That corroboration gate matters:
`biobouncer::repair_id()` also suggests via a fuzzy match, and would otherwise
turn `ORF1AB` into the unrelated `OR11A1`.

It is off by default. On a real lung-cancer run (449 genes) the corpus was 427
`valid`, 21 `microRNA`, and 1 alias with no corroborating source, so enabling it
merged nothing. Open Targets and PanelApp already return HGNC-approved symbols.
Check `symbol_status` on your own diseases before turning it on.

## Project structure

```txt
.
├── _brand.yml              # Brand colors, fonts, logo (theming)
├── global.R                # Libraries + component loader
├── ui.R / server.R         # App entry points
├── R/                      # Pure logic: schema, sources, ranking, curator
├── modules/                # Shiny modules (disease search, pipeline, etc.)
├── userInterface/          # Page layouts (builder, about)
├── data/cache/             # Cached API responses (git-ignored)
├── tests/testthat/         # Unit, testServer, and shinytest2 smoke tests
└── docs/                   # Project documentation
```

## Testing

```r
shiny::runTests(".")          # or: testthat::test_dir("tests/testthat")
```

Pure logic and source adapters are tested without network access (the GraphQL
client and the AI chat are injected as stubs). The shinytest2 smoke test runs
the pipeline against offline demo data via the `GLB_TEST_MODE` environment
variable and skips when Chrome is unavailable.

## Deployment

- Posit Publisher or Posit Connect for direct app publishing.
- Docker image deployment for a containerized release.

## Theming

Branding lives in [`_brand.yml`](_brand.yml): colors, fonts, and logo in one
place, applied by bslib via `bs_theme(brand = TRUE)` in [ui.R](ui.R). See
[docs/theming.md](docs/theming.md).

## Contributing

See CONTRIBUTING.md for contribution guidelines.
