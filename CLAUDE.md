# CLAUDE.md

Notes for an agent working in this repository. `README.md` is the document for a
person, and it is longer. This file holds the rules that are easy to break and
the knowledge that cost something to acquire.

## The one rule

**`apps.yml` is the source of truth for everything about an application.**

Do not add a second place to record something that `apps.yml` can hold. Do not
copy a value out of it into a workflow, a script or a document. Derive the value
instead, at the moment it is needed.

Three things read `apps.yml`, and none of them keeps its own copy:

| Reader | What it takes |
|---|---|
| `showcase.ejs`, through `index.qmd` | The cards, and the derived "View app" address |
| `R/check.R` | Everything, to test it |
| `.github/scripts/deploy_matrix.py` | `pcc-account`, `app`, `content_id` and `deploy`, as the matrix of `deploy-apps.yml` |

Consequences that are easy to get wrong:

- The address of a deployed application is
  `https://<pcc-account>-<app>.share.connect.posit.cloud/`, a function of the
  account on the category and the `app` field. `showcase.ejs` builds the "View
  app" button from those. **Never write a "View app" link into `apps.yml`**, and
  **never hard-code the account name**; `R/check.R` rejects the first and
  `deploy_app.R` has no default for the second.
- `deploy-apps.yml` holds no list of applications, no content id and no account
  name.
- To start, stop or move a deployment, edit `apps.yml`. Nothing else.

## Two kinds of content, which are easy to confuse

| Path | What it is |
|---|---|
| `apps.yml`, `packages.yml`, `thumbnails/` | The gallery website: text and images |
| `apps/` | Application source code, for deployment |

`apps.yml` is one file at the root. `apps/` is a directory. A tile can exist
with no code, and code can exist with no tile.

Vendored applications (`genescout`, `tahoe-explorer`, `variant-reviewer`) come
from their own repositories through `.github/workflows/vendor-apps.yml`. **Never
edit those directories here**; the next vendor run discards the change. The four
`lifescience-shiny-gallery` applications are local, and this repository is their
home.

One exception exists, and it is written beside its entry in `apps/sources.yml`:
`apps/variant-reviewer/.Rprofile` is patched here, because at v2.3.1 the file
stops R at startup on Connect Cloud and the application served nothing. A patch
of a vendored file is temporary by construction, so it must carry a comment in
`sources.yml` saying what to check in the next release. Prefer a fix in the
source repository; patch here only when the application is dead without it.

## Commands

```bash
# The gallery data. Needs the renv library.
Rscript -e 'source("R/check.R")'

# Which applications deploy, and where.
python .github/scripts/deploy_matrix.py

# The tests that run on a pull request.
python .github/scripts/test_deploy_matrix.py
python .github/scripts/check_manifests.py
python .github/scripts/check_secrets.py
GH_TOKEN=$(gh auth token) python .github/scripts/check_sources.py

# Deploy one application by hand. RENV_CONFIG_AUTOLOADER_ENABLED matters:
# without it renv hides the rsconnect that this needs. PCC_ACCOUNT has no
# default, and its value is the `pcc-account` of the category in apps.yml.
RENV_CONFIG_AUTOLOADER_ENABLED=false APP=genescout PCC_ACCOUNT=posit \
  CONTENT_ID=<id from apps.yml> Rscript .github/scripts/deploy_app.R
```

`R/check.R` and `deploy_matrix.py` test the same deployment fields, and they are
strict about the *type* of each one. `deploy: "false"` is a string, and a string
is true in Python, so an untyped test would deploy an application that a person
held back. Keep the two programs in agreement, and keep
`test_deploy_matrix.py` in agreement with both.

## Connect Cloud and rsconnect

This is the expensive knowledge. Verify before you contradict any of it.

The API has a public OpenAPI document at
<https://api.connect.posit.cloud/openapi.json>. It is the best reference; the
user-facing documentation does not cover these fields. rsconnect exposes almost
none of them, so `deploy_app.R` calls the API directly through
`rsconnect:::PATCH_JSON` and the client's own `withTokenRefreshRetry`.

### Broken in rsconnect 1.11.0 and on main

1. **`deployApp(manifestPath=)` sends `primary_file: null`** and Connect Cloud
   rejects the request. rsconnect infers the primary file only while it infers
   the application mode, and a manifest supplies the mode, so the inference
   never runs. `deploy_app.R` patches `rsconnect:::appMetadata` to supply it.
   Deploying from the manifest is not optional here: three applications name
   packages that are not on CRAN, so a runner cannot resolve dependencies.
2. **`appId` does not work for Connect Cloud.** The client implements no
   `getApplication()`, so `deployApp(appId=)` fails. Content is identified only
   through a local `rsconnect/*.dcf` record, and git ignores that directory, so
   `deploy_app.R` writes the record from the content id with
   `migrateToConnectCloud()` first. That function arrived in 1.11.0, which is
   the floor for the version.
3. **`applications()` aborts** for Connect Cloud accounts, so content cannot be
   looked up by name. On main, the name-based lookup inside `deployApp()` was
   removed for Connect Cloud as well: a future release will create *duplicate
   content* instead of updating, for any code that relies on `appName` alone.
   The content id is the only stable identifier.
4. **`upload = FALSE`** fails with `object 'bundle' not found`.

Filed upstream: rstudio/rsconnect#1366, #1367, #1368, #1369, and #1370 for the
`current_revision` fault below.

### The publish state machine

- `default_robots_policy` and `vanity_name` belong to the content, but the
  application serves each one **as it stood at its last publish**. So
  `deploy_app.R` applies them *before* it deploys, and a change lands in the
  same run. Verified: PATCH `disallow_all` on healthy content, deploy, and the
  served `robots.txt` follows immediately.
- `vanity_name` sets the address. Connect Cloud prefixes the account name, so
  `vanity_name: genescout` under `posit` serves at `posit-genescout.share...`.
  The payload **must** carry `domain_id`, and `null` selects the shared domain.
  Omitting the key gives HTTP 422. A custom domain needs DNS, and
  `GET /v1/domains` is empty for this account.
- The old content-id address redirects to the vanity address, so changing a
  vanity name breaks no link.
- **A PATCH of the content creates no revision, and is safe**, before a
  deployment or after one.
- **`POST /contents/{id}/republish` is not safe.** It can leave the content with
  no `current_revision` and a *published* `next_revision`, permanently. Do not
  call it. Filed as rstudio/rsconnect#1370.

### `Invalid token`, and the content with no current revision

This one cost the most, and the cause is not where it appears to be.

rsconnect asks Connect Cloud for a new bundle only when the content has a
current revision:

```r
# current revision will be null only when creating new content
if (!is.null(application$current_revision)) { ... updateContent(...) }
```

**That comment is wrong.** A republished content also has no current revision.
rsconnect then skips the request that mints a bundle, and uploads against the
`source_bundle_upload_url` that the content already carries. That token expires
one hour after it was minted, so **every** deployment of such content fails with
`Invalid token`, forever, and the failure looks like an authentication problem.

`PATCH /contents/{id}?new_bundle=true` is the request rsconnect skipped, and it
is the repair: it creates a pending revision with a fresh token, which the
deployment then finds. `ensureFreshBundle()` in `deploy_app.R` does exactly
that, and only for content in this state. Deleting and recreating the content is
*not* necessary, and deleting is not free: the delete is soft, `getContent()`
then aborts with "Content is pending deletion", and only the vanity name is
released at once.

Dead ends, for the record: `POST /publish`, a second `republish`,
`deployApp(upload = FALSE)`, and
`POST /revisions/{id}/refresh_upload_url` (HTTP 409, "Revision has already been
published").

### A green job is not a working application

**`deployApp()` does not signal a failed publish.** It prints `Deployment
failed with error: ...` and returns `FALSE` *invisibly*, so a script that
ignores the value exits 0 for an application that uploaded and then failed to
start. `deploy_app.R` takes the value and stops. Do not remove that check.

**A 200 from the address is not proof either.** Connect Cloud answers for
content whose application failed to start, so `curl -o /dev/null -w
'%{http_code}'` says 200 for a broken application. Read the body: a working
application here returns tens of kilobytes of Shiny and bslib assets, and a
broken one returned 61 bytes. `apps/variant-reviewer` was broken from its first
deployment and nobody noticed, because both signals said it was fine.

### Facts about this account

- Account `posit`, at <https://connect.posit.cloud/posit/>.
- New content is created with `access: "public"` here.
- New content is created with `default_robots_policy: "disallow_all"`, and this
  is a gallery, so `deploy_app.R` sets `allow_all`.
- The workflow authenticates with `PCC_CLIENT_ID` and `PCC_CLIENT_SECRET`,
  repository secrets from <https://login.posit.cloud/identity/credentials>.

## Keeping this file current

This file is only worth its length while it is true. When you learn something
here that the next agent would otherwise pay for again, write it down in the
same commit as the work.

Add a fact when:

- An API or a package behaves differently from its documentation, or from this
  file. Record what you observed, and how you observed it.
- You work around a bug. Name the version, and the condition for removing the
  work-around.
- A repository rule turns out to have a reason that is not obvious. Record the
  reason, not only the rule.
- A step needs an environment variable, a version floor or an order of
  operations that nothing else states.

Remove a fact when:

- A package release fixes a bug named here. Delete the entry and the
  work-around together, and search for a `ponytail:` comment about it.
- You verify that a statement here is wrong. Correct it; do not add a second
  statement beside it.

Keep the shape: a claim, then the evidence or the consequence. Prefer deleting a
stale paragraph to qualifying it. If this file and the code disagree, the code
is right and this file is a bug.
