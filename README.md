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
> 1. The site shows screenshots of private applications. `gene-list-builder` is
>    private. The four `lifescience-shiny-gallery` applications are
>    Posit-internal, because that repository is not public, although its license
>    is MIT. The README of `comp-bio-apps` says that this material needs a
>    sanitization pass first.
> 2. `apps/` holds a copy of the source code of those applications, and not only
>    the screenshots.
>
> The repository is private, and nothing publishes the site automatically.

## Two kinds of content

The repository holds two different things. Do not confuse them.

| Directory | Content | Purpose |
|---|---|---|
| `apps.yml`, `packages.yml`, `thumbnails/` | Text and images | The gallery website |
| `apps/` | Application source code | Deployment to Connect Cloud |

A tile in `apps.yml` is a description of an application. A directory in `apps/`
is the application. The two are independent: a tile can exist with no code, and
code can exist with no tile.

The two names are similar, so read them with care. `apps.yml` is one file at
the root, and it holds the text of the gallery. `apps/` is a directory, and it
holds R code.

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

### Which tiles have links

A tile gets a link when the link goes to a public address. A tile with no
links shows no footer.

Five applications have a public repository and a DOI, so their tiles link to
both:

```yaml
links:
  - text: GitHub
    url: https://github.com/samuelbharti/genescout
  - text: DOI
    url: https://doi.org/10.5281/zenodo.21352389
```

Three applications have no links: DE Explorer, Signature Scoring and Drug
Perturbation. They came from `posit-dev/lifescience-shiny-gallery`, which is
not public.

No application has a "View app" link, because no application is deployed in
public yet. Add that link to a tile after its deployment gets an address.

Make sure that each link gives status 200 before you add it. A dead link on a
card is worse than no link.

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

## Application source code in `apps/`

Connect Cloud deploys an application from its directory in `apps/`. Each
directory is complete and independent: its own modules, its own data, its own
`_brand.yml`, and its own `manifest.json`.

Connect Cloud needs `manifest.json` for R content. An application with no
manifest cannot deploy. To write one:

```r
rsconnect::writeManifest(appDir = "apps/<name>")
```

If an application uses a Bioconductor package, put the Bioconductor
repositories in `options(repos)` before you write the manifest. Use
`BiocManager::repositories()`. A manifest that names a Bioconductor package,
but records only CRAN, installs correctly on your machine and then fails on
Connect.

The `include` list of an application must contain every file that its
`manifest.json` names. Connect Cloud reads the manifest and then reads those
files. A file that the manifest names, but that `include` does not copy, is
absent from `apps/<name>/`.

Therefore the source repository must write a manifest of the application alone,
and not of the complete repository. Put the tests, the notes and the development
tools in a `.rscignore` file before you write the manifest.

`apps/` is not part of the website. `_quarto.yml` excludes it, because some
applications hold their own `.qmd` files.

### Two kinds of application live in `apps/`

| Kind | Applications | Where a change starts |
|---|---|---|
| Vendored | `genescout`, `tahoe-explorer`, `variant-reviewer` | The source repository. The workflow copies the release into this repository |
| Local | `de-explorer`, `signature-scoring`, `drug-perturbation`, `genome-explorer` | Here. This repository is their home |

`apps/sources.yml` lists the vendored applications. It records the source
repository, the release tag, and the files to copy. Read the comments in that
file before you add an entry.

**Do not change a vendored application here.** The next run of the workflow
copies the release again, and the copy removes your change. Change the source
repository, publish a release, then start the workflow.

**Change a local application here.** These four are different, and they must
stay absent from `sources.yml`. No other repository sends a change to them, and
the workflow must never write to them. They came from
`posit-dev/lifescience-shiny-gallery` one time, and a person copied them by
hand. That repository publishes no release, and its `R/_shared` and `R/_modules`
directories are build output that git ignores, so a release archive would give
applications that fail at startup.

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

## Tests on each pull request

`.github/workflows/checks.yml` operates three jobs on every pull request, and
also on `main` after a merge. The workflow has no path filter, because a test
that does not operate gives a false result.

| Job | Program | What it finds |
|---|---|---|
| Manifests | `check_manifests.py` | A file that `manifest.json` names, but that `apps/<name>/` does not contain. Also a package that the code loads and the manifest does not name |
| sources.yml | `check_sources.py` | An incorrect entry, and an `include` list that does not copy every file of the manifest. This test reads the release, so it finds the problem before the vendor workflow operates |
| Secrets | `check_secrets.py` and gitleaks | A file with the name of a secret file, such as `.Renviron`. gitleaks then reads the content of each file, and also the history of git |

The three programs also operate on your machine:

```bash
python .github/scripts/check_manifests.py
python .github/scripts/check_secrets.py
GH_TOKEN=$(gh auth token) python .github/scripts/check_sources.py
```

`check_manifests.py` and `check_sources.py` need `ruamel.yaml`.
`check_sources.py` also needs `requests`.

### The two manifest tests are not the same

`check_sources.py` reads the release of the source repository, and it answers
this question: does `include` name every file of the manifest?
`check_manifests.py` reads the directory in `apps/`, and it answers a different
question: is every file of the manifest on disk?

The first test finds the problem in the pull request that writes the entry. The
second test finds a file that a person removed after the copy.

## Project structure

```
apps.yml              # Applications section. Sequence in the file = sequence on the page
packages.yml          # Supporting Packages section. Same fields
index.qmd             # The page. It shows both listings
showcase.ejs          # Card template. Both listings use it
thumbnails/           # Screenshots, package hex logos, and placeholder.svg
apps/                 # Application source code for Connect Cloud
apps/sources.yml      # Which applications the vendor workflow copies
R/check.R             # check_apps(): reads both YAML files and reports problems
R/capture.R           # open_app() and capture_app(): screenshot helpers
R/thumbnail-name.R    # The URL name convention of the upstream gallery. Unused
R/renv.lock           # renv lockfile. .Rprofile sets the renv paths
.github/workflows/    # vendor-apps.yml: copies applications from their releases
                      # checks.yml: the tests that operate on each pull request
.github/scripts/      # vendor_apps.py: the code that the vendor workflow operates
                      # check_manifests.py, check_sources.py, check_secrets.py
.agents/skills/       # update-thumbnails skill, symlinked into .claude/skills/
_brand.yml            # Posit brand colors and typography
_variables.yml        # Site variables: name, author, description, URLs
_quarto.yml           # Quarto project configuration
styles.scss           # Custom styles: card images, tech line, callout
title-block.html      # Custom title block partial
```
