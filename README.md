# Shiny Showcase: Bioinformatics

A Quarto website showcasing the computational-biology tools, apps and packages
built by Samuel Bharti — a family of research-genomics Shiny applications on one
shared template, plus the cross-language packages beneath them.

Two tabs:

| Tab | Page | Data |
|---|---|---|
| **Apps** | `index.qmd` | `apps.yml` — 10 tiles in 2 categories |
| **Packages** | `packages.qmd` | `packages.yml` — biobouncer, plotomics |

Structure and tooling follow [posit-dev/shiny-gallery](https://github.com/posit-dev/shiny-gallery),
with the card template extended for status chips, optional links and a `fit`
control. Content is drawn from the private `comp-bio-apps` repository.

> [!WARNING]
> **Not ready to publish.** The site is built to go public eventually, but two
> things must be settled first:
>
> 1. **It contains screenshots of private applications** — `variant-reviewer` is
>    marked Private, and the four `lifescience-shiny-gallery` apps are
>    Posit-internal (that repo is not public, despite being MIT-licensed).
>    `comp-bio-apps/README.md` is explicit that this material needs a sanitisation
>    pass before anything derived from it goes public.
> 2. **No remote is configured**, and `_variables.yml` still carries a guessed
>    `issue-url`.
>
> Publishing is not wired up, so nothing leaves this machine by accident.

## How it works

`apps.yml` and `packages.yml` **are** the data — hand-edited, the single source
of truth, no generation step. Quarto renders each into a card grid through the
shared `showcase.ejs` template.

Quarto renders categories and tiles in the order they appear in the file, so
position is the only ordering mechanism — move a block to move it on the page.
There is no `order` field.

### Tile fields

| Field | | Notes |
|---|---|---|
| `title` | required | |
| `org` | required | The line under the title — owner, or parent project |
| `description` | required | Markdown is allowed |
| `thumbnail` | required | A file in `thumbnails/`, named after the app |
| `status` | required | `public` · `internal` · `private` · `wip` — renders as a chip |
| `links` | optional | List of `{text, url}`; each renders a button |
| `tech` | optional | The stack line under the description |
| `fit` | optional | `cover` (default, crops a screenshot) or `contain` (shows a diagram whole) |
| `alt` | optional | Image alt text; defaults sensibly from the title |
| `tags` | optional | Curated, not currently rendered |

### Links, and why most tiles have none

None of these applications is publicly deployed, so a "View app" button would be
a dead link on almost every card. Instead each tile carries a **status chip**,
and `links` is omitted entirely — a tile with nowhere to point renders no footer
at all. Add `links` as apps get somewhere to point:

```yaml
links:
  - text: GitHub
    url: https://github.com/samuelbharti/genescout
  - text: DOI
    url: https://doi.org/10.5281/zenodo.21352389
```

## Updating

### 1. Edit `apps.yml` or `packages.yml`

Add a tile under the relevant category, or add a new top-level `category:` block.

### 2. Add a thumbnail

Thumbnails are named **after the app** (`genescout.png`), not derived from a URL —
these apps are first-party and mostly run at a localhost URL that identifies
nothing. Tiles without a screenshot yet point at `thumbnails/placeholder.svg` and
are listed by `check.R` as "awaiting a real screenshot".

To capture one, run the **`/update-thumbnails` skill** (works in Claude Code and
Posit Assistant), or do it by hand — start the app locally, then in R:

```r
source("R/capture.R")
url <- "http://127.0.0.1:3838"
b <- open_app(url)                        # viewable window, 2400x1600 output
# interact in the window, or drive it: b$Runtime$evaluate('...')
capture_app(b, url, file = "genescout.png")
b$close()
```

Cards crop to 3:2 from the top-left, so put the app header and its most legible
content in the upper-left of the capture.

`R/thumbnail-name.R` still holds the upstream URL-derived convention. It is no
longer enforced, and applies only if a third-party app is ever added.

### 3. Check

```r
source("R/check.R")
```

Checks both YAML files. **Errors** on a tile missing a required field, an unknown
`status` or `fit`, a thumbnail that isn't on disk, or a `links` entry missing
half of itself. **Warns** on duplicate titles and on images in `thumbnails/` that
no tile references. Nothing is generated and nothing is fetched over the network.

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

Not configured — see the warning above.

## Project structure

```
apps.yml              # Apps tab data — source of truth, order = display order
packages.yml          # Packages tab data, same schema
index.qmd             # Apps tab
packages.qmd          # Packages tab
showcase.ejs          # Card layout template, shared by both tabs
thumbnails/           # Screenshots, diagrams, and placeholder.svg
R/check.R             # check_apps(): validates both YAML files
R/capture.R           # open_app()/capture_app() screenshot helpers
R/thumbnail-name.R    # Upstream URL -> filename convention; no longer enforced
R/renv.lock           # renv lockfile (renv paths are set in .Rprofile)
.agents/skills/       # update-thumbnails skill (symlinked into .claude/skills/)
_brand.yml            # Posit brand colors and typography
_variables.yml        # Site-level variables (name, author, descriptions, URLs)
_quarto.yml           # Quarto project config, including the two-tab navbar
styles.scss           # Custom styles — chips, card image fitting, tech line
title-block.html      # Custom title block partial
```
