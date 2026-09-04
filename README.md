# Shiny Showcase: Bioinformatics

A Quarto website. It shows the computational-biology applications and packages
that Samuel Bharti built. The repository also holds the source code of the
applications, ready to deploy to Posit Connect Cloud.

The website is one page, `index.qmd`. It shows two sections:

| Section | Data file |
|---|---|
| Applications | `apps.yml`, 9 tiles in 1 category |
| Supporting Packages | `packages.yml`, 4 packages |

The site has no navbar. The "Supporting Packages" button links to
`#supporting-packages`. Quarto does not add an anchor to a heading that a
listing template writes, so `showcase.ejs` makes the anchor from the category
name.

The structure follows
[posit-dev/shiny-gallery](https://github.com/posit-dev/shiny-gallery). This
repository adds three things to the card template: optional links, a `fit`
control, and `intro` text for each category.

> [!IMPORTANT]
> The four `lifescience-shiny-gallery` applications — DE Explorer, Signature
> Scoring, Drug Perturbation and Genome Explorer — carry `deploy: false` in
> `apps.yml`, and they keep it until a sanitization pass. Their source
> repository is Posit-internal, so their code and screenshots are here but
> their tiles carry no links, and nothing publishes them to a public address.
> Do not remove `deploy: false` from one of them without that pass.

## Source and citation

Five applications have a public repository of their own, and each release is
archived on Zenodo. Their tiles link to both.

| Application | Source | DOI of the release on the tile |
|---|---|---|
| tahoe-explorer | [samuelbharti/tahoe-explorer](https://github.com/samuelbharti/tahoe-explorer) | [10.5281/zenodo.21950641](https://doi.org/10.5281/zenodo.21950641) |
| genescout | [samuelbharti/genescout](https://github.com/samuelbharti/genescout) | [10.5281/zenodo.21950644](https://doi.org/10.5281/zenodo.21950644) |
| plotomics-live | [samuelbharti/plotomics-live](https://github.com/samuelbharti/plotomics-live) | [10.5281/zenodo.21950647](https://doi.org/10.5281/zenodo.21950647) |
| variant-reviewer | [samuelbharti/variant-reviewer](https://github.com/samuelbharti/variant-reviewer) | [10.5281/zenodo.21950635](https://doi.org/10.5281/zenodo.21950635) |
| gene-list-builder | [samuelbharti/gene-list-builder](https://github.com/samuelbharti/gene-list-builder) | [10.5281/zenodo.21950640](https://doi.org/10.5281/zenodo.21950640) |

Cite the application, and not the address of the deployment. An address moves;
a DOI does not. The form Zenodo gives, for the release above:

```
Bharti, S. (2026). Tahoe Explorer: a Shiny app for exploring Tahoe-100M
metadata (0.1.2). Zenodo. https://doi.org/10.5281/zenodo.21950641
```

Each DOI in `apps.yml` names **one version**. Zenodo also mints a concept DOI
for the record, which always resolves to the newest version; the Zenodo page
shows it under "Cite all versions". Cite the version DOI to name the software a
piece of work used, and the concept DOI to name the application itself.

The four `lifescience-shiny-gallery` applications have no public source and no
DOI. Cite this gallery instead, until the sanitization pass gives them one.

The packages in `packages.yml` follow the same convention, and their tiles
carry the same buttons. Only `biobouncer` has a DOI so far.

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

`apps.yml` is also the source of truth for the deployments. Its `pcc-account`,
`app`, `content_id` and `deploy` fields decide which applications
`.github/workflows/deploy-apps.yml` publishes, and where. Nothing else records
that, so nothing else can disagree with it. `CLAUDE.md` states the rule, and
"Deployment to Connect Cloud" below describes the workflow.

Quarto shows the categories and the tiles in the same order as the file. Position
in the file is the only control of order. There is no `order` field. To move a
card on the page, move its block in the file.

### Category fields

| Field | | Notes |
|---|---|---|
| `category` | required | The section heading. Its lowercase-hyphenated form is the anchor |
| `intro` | optional | A paragraph below the heading |
| `tiles` | required | The cards, in the sequence they appear |
| `pcc-account` | required, when a tile below it has a `content_id` | The Connect Cloud account that publishes. Every address below the category is built from it |

### Tile fields

| Field | | Notes |
|---|---|---|
| `title` | required | |
| `org` | required | The line below the title. The owner, or the parent project |
| `description` | required | Markdown is permitted |
| `thumbnail` | required | A file in `thumbnails/`, named after the application |
| `app` | optional | The directory in `apps/` that holds the source code. The title is a display name, and the two are not always the same |
| `content_id` | optional | The Connect Cloud content that `app` deploys to. Absent means the application is not published yet |
| `deploy` | optional | Default `true`. `false` holds the workflow back from an application that has both `app` and `content_id` |
| `links` | optional | A list of `{text, url}`. Each one makes a button. Not for the "View app" button, which the template derives |
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
    url: https://doi.org/10.5281/zenodo.21950644
```

Four applications have no links: DE Explorer, Signature Scoring, Drug
Perturbation and Genome Explorer. They came from
`posit-dev/lifescience-shiny-gallery`, which is not public.

**Do not write a "View app" link.** `showcase.ejs` makes that button itself,
from `pcc-account`, `app` and `content_id`, and it puts the button first. The
address of a deployment is
`https://<pcc-account>-<app>.share.connect.posit.cloud/`, a function of values
the file already holds, so a URL written by hand is a second copy of them.
`R/check.R` rejects one.

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
| Vendored | `genescout`, `tahoe-explorer`, `variant-reviewer`, `gene-list-builder`, `plotomics-live` | The source repository. The workflow copies the release into this repository |
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

## Deployment to Connect Cloud

`.github/workflows/deploy-apps.yml` publishes the applications to
[Posit Connect Cloud](https://connect.posit.cloud/posit/), under the `posit`
account. It operates after a merge into main, and it deploys only the
applications that the merge touched. `workflow_dispatch` deploys every
configured application.

The workflow holds no list of applications and no content id. `apps.yml` holds
both, and `.github/scripts/deploy_matrix.py` turns its tiles into the matrix of
the deploy job:

```yaml
- category: Applications
  pcc-account: posit
  tiles:
    - title: genescout
      app: genescout
      content_id: "01a068f2-...."
```

A tile deploys when it has `app` and `content_id`, and `deploy` is not false. So
one edit of `apps.yml` starts a deployment, stops it, or moves it to different
content, in the same block that gives the application its card:

| Fields | Result |
|---|---|
| `app` and `content_id` | Deploys |
| `app`, no `content_id` | Reported as `WAIT`. The application is not published yet |
| `app`, `deploy: false` | Reported as `HOLD` |
| No `app` | A description with no code in this repository |

To see what the workflow will do:

```bash
python .github/scripts/deploy_matrix.py
```

`R/check.R` and that script test the same fields, so a mistake fails on the pull
request, through the `apps.yml` job of `checks.yml`.

### The address of an application

Every deployment serves at
`https://<pcc-account>-<app>.share.connect.posit.cloud/`, where `<app>` is the
directory in `apps/` and `<pcc-account>` is the account on the category.

`deploy_app.R` sets that address, with the `vanity_name` field of the content.
Connect Cloud puts the account name in front of the value, so `vanity_name` of
`genescout` under the `posit` account becomes `posit-genescout`. The original
address of the content, the one with the content id in it, redirects to the new
one, so a change breaks no link.

The account is written one time, in `apps.yml`. `showcase.ejs` builds the
button from it, `deploy_matrix.py` puts it in the matrix, and `deploy_app.R`
takes it from `PCC_ACCOUNT` with no default. Nothing hard-codes it.

The script also sets `default_robots_policy` to `allow_all`. Connect Cloud
creates content with `disallow_all`, and this is a gallery, so the applications
must be findable.

Both settings belong to the content and not to a deployment, but the
application serves each one as it stood at its last publish. So `deploy_app.R`
applies them *before* it deploys, and a change lands in the same run. It reads
the content first and writes only when a value differs, so an ordinary run
makes one request and changes nothing.

Do not call `POST /contents/{id}/republish` to land a change instead. It can
leave the content with no current revision, and then every later deployment
fails with `Invalid token`. `apps/variant-reviewer` is in that state, and
`CLAUDE.md` explains both the cause and the repair that `deploy_app.R` applies
by itself.

### Why the first deployment is manual

rsconnect cannot find existing content by name on Connect Cloud, and it cannot
create content with a known id. A runner holds no deployment record, so it
needs the content id, and the id exists only after the first deployment.

Therefore a person publishes each application one time, from their machine, and
writes the id into `apps.yml`. After that the workflow updates the same content
on every merge, and it never creates content. A missing id is an application
that the matrix reports as `WAIT`, and never a second copy of the content.

The `posit` account publishes content with public access, so a new deployment
is readable by anybody who has the address. Confirm that in the content
settings after the first deployment: rsconnect cannot set the visibility of
Connect Cloud content, because its `appVisibility` argument has no effect on
that server.

### Publish an application the first time

Authenticate one time. This opens a browser:

```r
rsconnect::connectCloudUser()
```

Then operate `deploy_app.R` with no `CONTENT_ID`. It creates the content and
prints the new id:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=false \
  APP=genescout PCC_ACCOUNT=posit Rscript .github/scripts/deploy_app.R
```

The environment variable disables renv. The project library holds the packages
of the gallery tools, and this script needs rsconnect alone: it deploys from the
committed manifest, so it installs no dependency of the application.

Then do two things:

1. Open the content settings in Connect Cloud, and confirm that Access is
   public.
2. Write the id that the script printed into the tile in `apps.yml`, as
   `content_id`, beside its `app`.

That is the whole configuration. The tile gets its "View app" button from those
two fields, and the workflow gets its matrix entry from them.

New content serves at its content id until the *second* deployment. The first
run creates the content, so it can only set `vanity_name` afterwards, and the
address follows that field at publish time. The first run of the workflow after
step 2 gives the application its real address.

### The two rsconnect problems that `deploy_app.R` works around

The script needs rsconnect 1.11.0 or later. Two comments in it explain the
detail; this is the summary.

**The primary file.** rsconnect 1.10.1 through 1.11.0 send
`primary_file: null` to Connect Cloud when they deploy from a manifest, and the
API rejects the request. rsconnect infers that value while it infers the
application mode, and a manifest supplies the mode, so the inference never
happens. The script patches one internal function to supply it, from the file
list of the manifest. Remove the patch when rsconnect reads the primary file
from the manifest itself.

**The deployment record.** `deployApp()` identifies existing Connect Cloud
content only through a local `rsconnect/*.dcf` record. Its `appId` argument
does not work there, because the Connect Cloud client implements no
`getApplication()`, and content has no name to search for: the name that
rsconnect reports is the title. A runner holds no record, and git ignores the
directory that holds one, so the script writes the record from the content id
first, with `migrateToConnectCloud()`. That function arrived in 1.11.0, and it
is the reason for the version requirement.

### The two secrets

The workflow authenticates with an OAuth service account, from
<https://login.posit.cloud/identity/credentials>. Store them as repository
secrets:

| Secret | Value |
|---|---|
| `PCC_CLIENT_ID` | The client id |
| `PCC_CLIENT_SECRET` | The client secret |

`deploy_app.R` makes the call that the
[Connect Cloud documentation](https://docs.posit.co/connect-cloud/user/publish/console-or-terminal.html)
gives:

```r
rsconnect::connectCloudClientCredentials(
  clientId = ..., clientSecret = ..., account = "posit"
)
```

**The credentials must grant publish permission on that account.** The
function registers it only when `GET /v1/accounts` advertises `content:create`
on it, and the failure names the account rather than the permission:

```
Account "posit" is visible to these credentials but does not grant
publish permission.
```

The account name is `posit`, and not the `Posit PBC` that the interface shows
beside it: those are the `name` and the `display_name` of the same account, and
the lookup compares `name`. The name comes from `pcc-account` in `apps.yml`,
which is the only copy of it.

### Git-backed publishing is the other option

Connect Cloud can watch a branch by itself, with no workflow: it redeploys when
the branch changes. This repository uses GitHub Actions instead, because a
deployment then happens after the tests of `checks.yml`, and because one file
records which applications publish and where.

Do not enable both for one piece of content. Connect Cloud would redeploy on
the push, and the workflow would redeploy again.

## Tests on each pull request

`.github/workflows/checks.yml` operates three jobs on every pull request, and
also on `main` after a merge. The workflow has no path filter, because a test
that does not operate gives a false result.

| Job | Program | What it finds |
|---|---|---|
| Manifests | `check_manifests.py` | A file that `manifest.json` names, but that `apps/<name>/` does not contain. Also a package that the code loads and the manifest does not name |
| sources.yml | `check_sources.py` | An incorrect entry, and an `include` list that does not copy every file of the manifest. This test reads the release, so it finds the problem before the vendor workflow operates |
| Secrets | `check_secrets.py` and gitleaks | A file with the name of a secret file, such as `.Renviron`, and a file inside a forbidden directory, such as `.posit`. gitleaks then reads the content of each file, and also the history of git |

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
CLAUDE.md             # Notes for an agent. The rules, and the hard-won knowledge
apps.yml              # Applications section, and the source of truth for the deployments
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
                      # deploy-apps.yml: publishes apps/ to Connect Cloud on merge
.github/scripts/      # vendor_apps.py: the code that the vendor workflow operates
                      # check_manifests.py, check_sources.py, check_secrets.py
                      # deploy_matrix.py: reads apps.yml, writes the deploy matrix
                      # deploy_app.R: deploys one app, in CI or on your machine
.agents/skills/       # update-thumbnails skill, symlinked into .claude/skills/
_brand.yml            # Posit brand colors and typography
_variables.yml        # Site variables: name, author, description, URLs
_quarto.yml           # Quarto project configuration
styles.scss           # Custom styles: card images, tech line, callout
title-block.html      # Custom title block partial
```
