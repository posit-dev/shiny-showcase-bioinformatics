# Variant Reviewer

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.21934011-1682D4)](https://doi.org/10.5281/zenodo.21934011)

By [Samuel Bharti](https://www.samuelbharti.com)

Variant Reviewer is a Shiny app that helps you interpret a gene or a variant.
Enter a gene symbol, a variant, or both. The dashboard then shows what the gene
does, where the body expresses it, what it interacts with, and protein-level
context for the variant. If you enter only a variant, the app finds its gene.

The app is a reactive front end over several public bioinformatics APIs:

| Card / section          | Source                                      |
| ----------------------- | ------------------------------------------- |
| Gene summary            | MyGene                                      |
| Variant annotation      | MyVariant                                   |
| In-silico predictions   | dbNSFP (MyVariant)                          |
| Protein context         | ProtVar (EBI)                               |
| Variant landscape       | ClinVar variants (gnomAD) + UniProt domains |
| Conservation            | dbNSFP (MyVariant)                          |
| Ancestry frequency      | gnomAD                                      |
| Protein domains         | UniProt (EBI Proteins)                      |
| 3D structure            | AlphaFold DB                                |
| Clinical significance   | ClinVar (NCBI E-utilities)                  |
| Population frequency    | gnomAD                                      |
| Gene constraint         | gnomAD                                      |
| Gene model              | Ensembl                                     |
| Variant consequences    | Ensembl VEP                                 |
| Tissue expression       | GTEx                                        |
| Protein interactions    | STRING                                      |
| Disease associations    | Open Targets                                |
| Phenotypes (HPO)        | Monarch Initiative                          |
| Known drugs             | Open Targets                                |
| Pharmacogenomics        | Open Targets                                |
| Literature              | Europe PMC                                  |
| External resource links | derived                                     |
| AI assistant (chat)     | BYOK: Gemini / OpenAI / Anthropic           |

All data sources are public. None of them need an API key. The optional AI
assistant is bring your own key (BYOK). See [AI assistant](#ai-assistant)
below.

## Scope

**The app** reviews one human gene at a time, and optionally one variant. It
gathers public, read-only annotations onto a single dashboard, and links out to
the original sources.

The app is **not** for:

- large-scale or VCF-scale analysis, or variant calling
- non-human species
- clinical diagnosis, treatment, or genetic-counseling advice

**The assistant** helps you interpret the gene or variant on screen, and
answers general genomics questions within that scope. It declines requests
outside that scope. It is not a source of medical, diagnostic, or treatment
advice.

## Requirements

- R (>= 4.3)
- Packages managed with `renv` (see `renv.lock`)

## Installation

```r
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::restore()
```

Or install the core packages by hand:

```r
install.packages(c(
  "shiny", "bslib", "brand.yml", "ggplot2",
  "httr2", "reactable", "jsonlite", "shinycssloaders",
  "r3dmol", # 3D structure card (AlphaFold viewer)
  "cicerone", # guided Demo tour
  # AI assistant (optional; the app degrades gracefully without them)
  "ellmer", "shinychat"
))

# biobouncer (input validation) is published on r-universe, not CRAN:
install.packages(
  "biobouncer",
  repos = c("https://samuelbharti.r-universe.dev", getOption("repos"))
)
```

## How to run

```r
shiny::runApp()
```

Or open the project in RStudio, then click **Run App**.

## Build and run with Docker

```bash
docker build -t variant-reviewer .
docker run --rm -p 3838:3838 variant-reviewer
```

Then open [http://localhost:3838](http://localhost:3838).

## Deploy to Posit Connect Cloud

Connect Cloud deploys from a public GitHub repo. It needs two files: a primary
file ([app.R](app.R)) and a [manifest.json](manifest.json). Connect Cloud does
**not** read `renv.lock`. The manifest is what pins the R version and every
package.

Whenever a dependency changes, regenerate the manifest, then commit it:

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

Before you run this command, make sure that `renv::status()` reports no
issues. If it does not, `writeManifest()` stops with the message "library and
lockfile are out of sync".

In the Connect Cloud dashboard, pick `app.R` as the primary file. The AI
assistant needs no key to deploy: it is bring your own key (BYOK), so each
visitor supplies their own. If you want to fund a shared assistant instead,
set `GEMINI_API_KEY`, `OPENAI_API_KEY`, or `ANTHROPIC_API_KEY` as a variable
there. In that case, the account behind that key pays for every visitor's
use.

[.rscignore](.rscignore) keeps the bundle small, and keeps `.Renviron` out of
it. `rsconnect` does not read `.gitignore`, so you must exclude secrets there
too.

## Project structure

```txt
.
├── _brand.yml              # Brand colors, fonts, logo (theming)
├── global.R                # Libraries and component loading
├── app.R                   # Entry point: sources global.R, app_ui.R, app_server.R
├── app_ui.R                # Navbar layout: Home dashboard + About page
├── app_server.R            # Wires search -> resolved gene -> result modules
├── R/                      # Pure-R API clients + helpers (no Shiny)
│   ├── api_http.R          # Shared httr2 GET wrapper (timeouts, retries, errors)
│   ├── api_mygene.R        # Gene resolution
│   ├── api_myvariant.R     # Variant annotation
│   ├── api_protvar.R       # Protein-level context
│   ├── api_gtex.R          # Tissue expression
│   ├── api_string.R        # Interaction partners
│   └── ui_helpers.R        # Small presentation helpers
├── modules/                # Shiny modules (one card each)
├── userInterface/          # Page-level layout (dashboard_ui.R, about_ui.R)
├── tests/                  # testthat unit/reactive tests + shinytest2 e2e
└── docs/                   # Project documentation, including docs/plans/
```

### Architecture

The search module creates a reactive query with a gene and a variant. As you
type a gene, the variant box suggests that gene's known pathogenic and likely
pathogenic variants (from ClinVar, through MyVariant). You can still type any
rsID or HGVS value by hand.

Before the app queries an API, it checks the gene and the variant. It uses
[biobouncer](https://github.com/samuelbharti/biobouncer)'s offline `pattern`
mode: the HGNC grammar for genes, and the dbSNP format for rsIDs. This check
rejects bad input right away, with a message in the app, instead of running a
search that will fail. The same check applies to the assistant's
`set_selection` tool.

`app_server.R` resolves the gene one time, through MyGene. This step maps the
gene symbol to its Ensembl, Entrez, and UniProt IDs. Every gene-level module
shares this one result, so the app queries each API only when needed. API
clients are pure R code, in `R/`, and you can test each one on its own.
Modules only manage reactivity and rendering.

The app caches successful HTTP responses in memory for 30 minutes, at the
`vr_api_get()` and `vr_api_post_json()` functions. As a result, a repeated
search returns instantly. The app never caches a failure.

To add a new data source, for example ClinVar, gnomAD, or Open Targets, add
one `R/api_*.R` file and one `modules/*_mod.R` file to the dashboard grid.

## Testing

```r
shiny::runTests(".")
```

- **Parser tests** (`tests/testthat/test-api-parsers.R`) run offline, against
  recorded JSON fixtures in `tests/testthat/fixtures/`.
- **Reactive tests** (`test-modules.R`) use `shiny::testServer()`.
- **End-to-end tests** (`test-shinytest2.R`) launch the app in a headless
  browser. CI skips the live-API search test, to keep results repeatable. Run
  it locally with `NOT_CRAN=true`.

## AI assistant

The dashboard includes an optional chat assistant
([modules/byok_chat_mod.R](modules/byok_chat_mod.R)) for discussing the gene or
variant you are reviewing. The assistant uses a bring-your-own-key (BYOK)
model. To connect it:

1. Open the chat's **Model & key** drawer. Use the gear button in the chat
   header.
2. Pick a provider: Google Gemini, OpenAI, or Anthropic.
3. Paste your own API key.

The app holds your key only in the server memory for your session. It never
writes the key to disk. When you paste the key, the app loads the models that
the key can reach, and the model picker fills in on its own. You can still
type any model ID that the key supports. By default, Gemini uses
`gemini-flash-lite-latest`.

You can also set a key on the server, in the matching environment variable:
`GEMINI_API_KEY` or `GOOGLE_API_KEY`, `OPENAI_API_KEY`, or `ANTHROPIC_API_KEY`.
Then connect with the key field blank.

The assistant reads the dashboard's data through a set of tools scoped to the
app. [app_server.R](app_server.R) wires the tools. [R/chat_tools.R](R/chat_tools.R)
formats their output.

- `get_current_selection`: returns the gene and variant that are loaded now.
- `read_card`: returns the data in one card. Valid cards: gene, variant,
  predictions, protein, landscape, conservation, domains, structure, clinvar,
  gnomad, constraint, genemodel, consequences, expression, interactions,
  diseases, phenotypes, drugs, pharmacogenomics, and literature.
- `set_selection`: enters a gene, a variant, or both into the search box, then
  clicks Review. This is the same as typing the search by hand.

The assistant can read all of this data. It can change only one thing: the
search. `set_selection` enters the search terms, and the app fills the cards on
its own. The assistant cannot write to a card, and cannot change what a card
shows. A request from the assistant goes through the same check as a typed
search. The gene and variant that it used stay visible in the search box.

The assistant uses only the app's own data and lookups. It does not search the
web. The assistant needs the `ellmer` and `shinychat` packages. If either
package is missing, the card shows a short setup panel instead, and the rest
of the app loads as normal.

## Theming

All branding (colors, fonts, and the logo) lives in one file:
[`_brand.yml`](_brand.yml). [app_ui.R](app_ui.R) applies it through bslib's
`bs_theme(brand = TRUE)`.

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
[Zenodo record](https://doi.org/10.5281/zenodo.21934011).

[CITATION.cff](CITATION.cff) holds the full metadata, and
[CITATION.md](CITATION.md) gives a ready-made text and BibTeX entry.

## License

[MIT](LICENSE). Copyright (c) 2026 Samuel Bharti.
