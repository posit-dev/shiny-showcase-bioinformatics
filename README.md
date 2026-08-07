# Shiny Showcase: Bioinformatics

A Quarto website that showcases Shiny apps built by the bioinformatics and
computational biology community. Renders as a gallery of cards grouped by
category: Genomics & Variant Analysis, Transcriptomics & Single-Cell,
Proteomics & Metabolomics, Structural Biology, Microbiome & Metagenomics,
Clinical & Translational.

Structure and tooling follow [posit-dev/shiny-gallery](https://github.com/posit-dev/shiny-gallery).

## How it works

`apps.yml` **is** the app data — hand-edited, the single source of truth, no
generation step. Quarto renders it into the gallery through the `showcase.ejs`
template.

Quarto renders categories and tiles in the order they appear in `apps.yml`, so
position is the only ordering mechanism — move a block to move it on the page.
There is no `order` field.

The repo ships with one placeholder tile under Genomics & Variant Analysis so a
fresh clone renders and checks cleanly. Delete it once the first real app lands.
The remaining categories are empty (`tiles: []`); drop any you don't need.

## Updating apps

### 1. Edit `apps.yml`

Add a tile under the relevant category, or add a new top-level `category:`
block. Each tile has `org`, `title`, `url`, `description`, `thumbnail`, and
optionally `tags` (curated, not currently rendered):

```yaml
- category: Transcriptomics & Single-Cell
  tiles:
    - org: Broad Institute
      title: Single Cell Portal
      url: https://singlecell.broadinstitute.org/
      description: Explore and visualize single-cell RNA-seq datasets.
      thumbnail: singlecell_broadinstitute_org_.png
      tags: single-cell, rna-seq
```

### 2. Add a thumbnail

The `thumbnail` filename follows a convention derived from the app URL.
`thumbnail_name()` in `R/thumbnail-name.R` is the single source of truth: it
drops the protocol and any `#fragment`, strips `&` and percent-escapes, turns
`=` and `.` into `_`, then hands the rest to `fs::path_sanitize()` — which
converts `/`, `?` and anything else unsafe in a filename — before appending
`.png`. The query string is kept, because some apps differ only by it, and the
only `.` in the result is the one before the extension. For example:

```
https://singlecell.broadinstitute.org/
  ->  singlecell_broadinstitute_org_.png

https://lsp.connect.hms.harvard.edu/smallmoleculesuite/?_inputs_&tab=%22library%22
  ->  lsp_connect_hms_harvard_edu_smallmoleculesuite___inputs_tab_library.png
```

To capture screenshots, run the **`/update-thumbnails` skill** (works in Claude
Code and Posit Assistant), or do it by hand in R:

```r
source("R/capture.R")
url <- "https://singlecell.broadinstitute.org/"
b <- open_app(url)        # opens a viewable browser window at 2400x1600
# interact in the window (dismiss dialogs, click tabs, scroll),
# or drive it: b$Runtime$evaluate('document.querySelector("...").click()')
capture_app(b, url)       # saves thumbnails/<derived name>.png
b$close()
```

A "new app" is any tile whose `thumbnail` is missing from `thumbnails/`.

### 3. Check `apps.yml`

```r
source("R/check.R")
```

Errors if a tile is missing a required field or its thumbnail isn't on disk;
warns if a filename drifts from the convention or a URL is duplicated. Nothing
is generated and nothing is fetched over the network.

R dependencies are managed with renv. To restore them:

```r
source("R/renv/activate.R")
renv::restore()
```

### 4. Preview locally

```bash
quarto preview
```

### 5. Publish

Not yet configured. The upstream gallery deploys to Posit Connect via the Posit
Publisher extension in Positron, which writes its config to `.posit/publish/`.
Set that up when there's a target to deploy to.

## Adding a new category

Add a top-level block to `apps.yml` where you want it to appear on the page.
Nothing else needs to change:

```yaml
- category: New Category
  tiles:
    - org: ...
```

## Project structure

```
apps.yml              # App data — source of truth, hand-edited, order = display order
index.qmd             # Main page
showcase.ejs          # Card layout template
thumbnails/           # App screenshot images (2400x1600)
R/check.R             # check_apps(): validates apps.yml against thumbnails/
R/capture.R           # open_app()/capture_app() helpers for screenshotting apps
R/thumbnail-name.R    # thumbnail_name(): canonical URL -> filename convention
R/renv.lock           # renv lockfile (renv paths are set in .Rprofile)
.agents/skills/       # update-thumbnails skill (symlinked into .claude/skills/)
_brand.yml            # Posit brand colors and typography
_variables.yml        # Site-level variables (vertical name, URLs, description)
_quarto.yml           # Quarto project config
styles.scss           # Custom styles
title-block.html      # Custom title block partial
```
