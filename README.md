# Shiny Showcase: Bioinformatics

A Quarto website. It shows the computational-biology applications and packages
that Samuel Bharti built. The repository also holds the source code of the
applications, ready to deploy to Posit Connect Cloud.

The website is one page, `index.qmd`. It shows two sections:

| Section | Data file |
|---|---|
| Applications | `apps.yml`, 8 tiles in 1 category |
| Supporting Packages | `packages.yml`, 4 packages |

The site has no navbar. The "Supporting Packages" button links to
`#supporting-packages`. Quarto does not add an anchor to a heading that a
listing template writes, so `showcase.ejs` makes the anchor from the category
name.

The structure follows
[posit-dev/shiny-gallery](https://github.com/posit-dev/shiny-gallery). This
repository adds three things to the card template: optional links, a `fit`
control, and `intro` text for each category.

> [!CAUTION]
> Do not make this repository public yet. Correct these two problems first.
>
> 1. The site shows screenshots of private applications. `variant-reviewer` is
>    private. The four `lifescience-shiny-gallery` applications are
>    Posit-internal, because that repository is not public, although its license
>    is MIT. The README of `comp-bio-apps` says that this material needs a
>    sanitization pass first.
> 2. `app/` holds a copy of the source code of those applications, and not only
>    the screenshots.
>
> The repository is private, and nothing publishes the site automatically.

## Two kinds of content

The repository holds two different things. Do not confuse them.

| Directory | Content | Purpose |
|---|---|---|
| `apps.yml`, `packages.yml`, `thumbnails/` | Text and images | The gallery website |
| `app/` | Application source code | Deployment to Connect Cloud |

A tile in `apps.yml` is a description of an application. A directory in `app/`
is the application. The two are independent: a tile can exist with no code, and
code can exist with no tile.

## The gallery

`apps.yml` and `packages.yml` are the data. A person edits them by hand. No
script writes them. Quarto reads each file and makes a grid of cards with the
`showcase.ejs` template.

Quarto shows the categories and the tiles in the same order as the file. Position
in the file is the only control of order. There is no `order` field. To move a
card on the page, move its block in the file.

### Category fields

| Field | | Notes |
|---|---|---|
| `category` | required | The section heading. Its lowercase-hyphenated form is the anchor |
| `intro` | optional | A paragraph below the heading |
| `tiles` | required | The cards, in the sequence they appear |

### Tile fields

| Field | | Notes |
|---|---|---|
| `title` | required | |
| `org` | required | The line below the title. The owner, or the parent project |
| `description` | required | Markdown is permitted |
| `thumbnail` | required | A file in `thumbnails/`, named after the application |
| `links` | optional | A list of `{text, url}`. Each one makes a button |
| `tech` | optional | The stack line below the description. Only `packages.yml` uses it |
| `fit` | optional | `cover` (default, crops a screenshot), `contain` (shows a diagram complete), or `hex` (puts a package logo in the center of a tint) |
| `alt` | optional | Alternative text for the image. It defaults from the title |
| `tags` | optional | Kept for reference. The template does not show them |

### Why the application tiles have no links

The packages are public. Their tiles link to GitHub, R-universe, PyPI and DOIs.

No application is deployed in public. A "View app" button on an application card
would be a dead link. Therefore `links` is absent when there is nothing to link
to, and a tile with no links shows no footer.

When an application gets a public address, add its links:

```yaml
links:
  - text: GitHub
    url: https://github.com/samuelbharti/genescout
  - text: DOI
    url: https://doi.org/10.5281/zenodo.21352389
```

## How to update the gallery

### 1. Edit `apps.yml` or `packages.yml`

Add a tile below the correct category. To make a new section, add a top-level
`category:` block.

### 2. Add a thumbnail

Each thumbnail has the name of its application, for example `genescout.png`. The
name does not come from a URL. These applications are first-party, and most of
them operate at a localhost address that identifies nothing.

A tile with no screenshot points to `thumbnails/placeholder.svg`. `check.R`
lists these tiles as "awaiting a real screenshot".

A package tile shows the hex logo of the package instead of a screenshot, with
`fit: hex`. Each logo is a copy of `man/figures/logo.svg` (or `.png`) from that
package.

To capture a screenshot, use the `/update-thumbnails` skill. It operates in
Claude Code and in Posit Assistant. To do it by hand, start the application
locally. Then, in R:

```r
source("R/capture.R")
url <- "http://127.0.0.1:3838"
b <- open_app(url)                        # viewable window, 2400x1600 output
# interact in the window, or drive it: b$Runtime$evaluate('...')
capture_app(b, url, file = "genescout.png")
b$close()
```

A card crops the image to 3:2 from the top left corner. Put the header of the
application, and its most legible content, in the top left of the capture.

`R/thumbnail-name.R` holds the name convention of the upstream gallery, which
makes the file name from a URL. Nothing applies this convention now. It becomes
necessary only if you add a third-party application.

### 3. Make sure the data is correct

```r
source("R/check.R")
```

`check.R` reads both YAML files. It gives an error for these four problems:

- A tile has no value for a required field.
- The value of `fit` is unknown.
- A thumbnail is absent from disk.
- A `links` entry has only one of its two halves.

It gives a warning for a duplicate title, and for an image in `thumbnails/` that
no tile uses. It writes no files and it makes no network requests.

renv controls the R dependencies. To install them:

```r
source("R/renv/activate.R")
renv::restore()
```

### 4. See the site locally

```bash
quarto preview
```

## Application source code in `app/`

Connect Cloud deploys an application from its directory in `app/`. Each
directory is complete and independent: its own modules, its own data, its own
`_brand.yml`, and its own `manifest.json`.

Connect Cloud needs `manifest.json` for R content. An application with no
manifest cannot deploy. To write one:

```r
rsconnect::writeManifest(appDir = "app/<name>")
```

If an application uses a Bioconductor package, put the Bioconductor
repositories in `options(repos)` before you write the manifest. Use
`BiocManager::repositories()`. A manifest that names a Bioconductor package,
but records only CRAN, installs correctly on your machine and then fails on
Connect.

The `include` list of an application must contain every file that its
`manifest.json` names. Connect Cloud reads the manifest and then reads those
files. A file that the manifest names, but that `include` does not copy, is
absent from `app/<name>/`.

Therefore the source repository must write a manifest of the application alone,
and not of the complete repository. Put the tests, the notes and the development
tools in a `.rscignore` file before you write the manifest.

`app/` is not part of the website. `_quarto.yml` excludes it, because some
applications hold their own `.qmd` files.

### Two ways an application arrives in `app/`

| Source | Method | Applications |
|---|---|---|
| A repository with GitHub Releases | The workflow copies it | `genescout`, `tahoe-explorer` |
| `lifescience-shiny-gallery` | A person copies it by hand | the other four |

`app/sources.yml` lists the applications that the workflow controls. It records
the source repository, the release tag, and the files to copy. Read the comments
in that file before you add an entry.

The four `lifescience-shiny-gallery` applications are absent from that list.
That repository publishes no releases. Its `R/_shared` and `R/_modules`
directories are build output, and git ignores them, so a release archive would
give applications that fail at startup. Copy those four by hand from a
synchronized working tree.

### How to update a vendored application

Publish a release in the source repository first. The workflow reads releases,
not branches, so it cannot see a file that you add after the tag.

Then run two commands:

```bash
gh workflow run vendor-apps.yml -R posit-dev/shiny-showcase-bioinformatics
gh pr create --head vendor-apps-bot --fill
```

The first command starts the workflow. It examines every application in
`sources.yml`, copies the ones with a new release, and pushes the
`vendor-apps-bot` branch. One run covers all the applications.

The second command opens the pull request. The `posit-dev` organization does not
permit GitHub Actions to open pull requests, so a person must do this step. The
summary of the workflow shows the command.

## Project structure

```
apps.yml              # Applications section. Sequence in the file = sequence on the page
packages.yml          # Supporting Packages section. Same fields
index.qmd             # The page. It shows both listings
showcase.ejs          # Card template. Both listings use it
thumbnails/           # Screenshots, package hex logos, and placeholder.svg
app/                  # Application source code for Connect Cloud
app/sources.yml       # Which applications the vendor workflow controls
R/check.R             # check_apps(): reads both YAML files and reports problems
R/capture.R           # open_app() and capture_app(): screenshot helpers
R/thumbnail-name.R    # The URL name convention of the upstream gallery. Unused
R/renv.lock           # renv lockfile. .Rprofile sets the renv paths
.github/workflows/    # vendor-apps.yml: copies applications from their releases
.github/scripts/      # vendor_apps.py: the code that the workflow operates
.agents/skills/       # update-thumbnails skill, symlinked into .claude/skills/
_brand.yml            # Posit brand colors and typography
_variables.yml        # Site variables: name, author, description, URLs
_quarto.yml           # Quarto project configuration
styles.scss           # Custom styles: card images, tech line, callout
title-block.html      # Custom title block partial
```
