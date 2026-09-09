---
name: publish-showcase
description: Publish the gallery website itself — the site that index.qmd renders — to Posit Connect Cloud under the `posit` account, at https://posit-shiny-showcase-bioinformatics.share.connect.posit.cloud/, and point the GitHub repository's homepage at it. Use after a change to index.qmd, apps.yml, packages.yml, thumbnails/ or the theme, when the published site should show it. Not for deploying an application in apps/; deploy-apps.yml and deploy_app.R do that.
---

# Publish the showcase

The site is one page, `index.qmd`, rendered by Quarto into `_site/`. Connect
Cloud serves it as static content, so this deployment carries no packages, no
manifest and no R process. That is the whole difference from an application in
`apps/`, and it is why `.github/scripts/deploy_showcase.R` is a separate script
from `deploy_app.R` rather than an argument to it.

**Read the "Connect Cloud and rsconnect" section of `CLAUDE.md` before you
debug anything here.** Every failure mode of an application deployment applies
to this one, and the expensive ones look like authentication problems and are
not.

The address is fixed by two values in the script, `account` and `name`:

```
https://posit-shiny-showcase-bioinformatics.share.connect.posit.cloud/
```

Nothing else records them, so change the address by editing the script. There
is no apps.yml field for the site: apps.yml describes the tiles, and the page
that shows the tiles is not one of them.

## Steps

1. **Sign in through SSO, then register the account.** The `posit` account has
   `sso_enabled: true`, and Connect Cloud rejects every write from a token that
   did not come through the organization's SSO. A read succeeds either way, so
   a working `getContent()` proves nothing.

   Open <https://connect.posit.cloud/posit/> in a browser and sign in through
   SSO. Then, in R:

   ```r
   rsconnect::connectCloudUser()
   ```

   **This step is why `.github/workflows/deploy-showcase.yml` is held back with
   `if: false`.** `PCC_CLIENT_ID` and `PCC_CLIENT_SECRET` are client
   credentials, they never pass through SSO, and Connect Cloud answers 401
   `sso_required`, so the only publisher of this content is a person at their
   own console after that browser login. The comment on that job lists what
   must change before it can run.

   Signing in to the browser is not enough on its own: the stored token is the
   one the last device flow minted, so `connectCloudUser()` must run *after*
   the SSO login. `rsconnect::accounts()` listing `posit` does not mean the
   token behind it came through SSO, and a 401 `sso_required` on a write is
   what says it did not. Test it with the no-op PATCH in `CLAUDE.md` before
   blaming anything else.

   An agent cannot run `connectCloudUser()` for you. It prompts to choose
   between the accounts the login can see, and this login sees more than one.
   Ask the person to run it.

2. **Check the version of rsconnect.** 1.11.0 is the floor, but only on a
   machine with no local deployment record: that is the one path that needs
   `migrateToConnectCloud()`. A machine that has deployed the site before
   already holds `rsconnect/connect.posit.cloud/posit/*.dcf`, and 1.10.1
   deploys from it.

   ```r
   packageVersion("rsconnect")
   ```

3. **Render, and look at what rendered.** `quarto render` empties `_site/`
   first, so a failed render leaves nothing to deploy and the script stops.

   ```bash
   quarto render
   ```

   Read `R/check.R` first if the change touched `apps.yml` or `packages.yml`;
   Quarto renders a tile with a missing thumbnail without complaining.

   ```bash
   Rscript -e 'source("R/check.R")'
   ```

4. **Deploy.**

   ```bash
   Rscript .github/scripts/deploy_showcase.R
   ```

   Under renv, and not with `RENV_CONFIG_AUTOLOADER_ENABLED=false` the way
   `deploy_app.R` runs. `R/renv.lock` records rsconnect for this script, so the
   renv library holds the version that is known to work; the system library
   holds whatever is installed there. `renv::restore()` if the library is cold.

5. **On the first run only: record the content id, then deploy a second
   time.** With no id the script creates content, prints the id, and asks for
   it. Write it into the `contentId` of `.github/scripts/deploy_showcase.R` and
   commit that. rsconnect cannot find Connect Cloud content by name, so a lost
   id means the next run creates a *second* site.

   Then run step 4 again. The settings of new content can only be applied
   *after* it exists, and the site serves each one as it stood at its last
   publish, so the first run leaves the vanity address answering "Page Not
   Found" and `robots.txt` saying `Disallow: /`. The second run publishes with
   them in place and both are correct. Only the first run has this problem;
   afterwards the script applies the settings before it deploys.

   Confirm that the content's access is public. Content in this account is
   created public, and a `curl` with no credentials is the check.

6. **Read the served page.** A 200 is not proof: Connect Cloud answers for
   content that failed to publish.

   ```bash
   curl -s https://posit-shiny-showcase-bioinformatics.share.connect.posit.cloud/ | head -20
   ```

   The gallery answers with its own `<title>` and the card markup. Connect
   Cloud's spinner page answers with `<title>Posit Connect Cloud</title>`, and
   that is a failure whatever the status code said.

7. **Point the repository at it.** Done already; repeat only if the address
   changes:

   ```bash
   gh repo edit posit-dev/shiny-showcase-bioinformatics \
     --homepage https://posit-shiny-showcase-bioinformatics.share.connect.posit.cloud/
   ```

   `gh repo view --json homepageUrl` reads back what took.

## Notes

The script is idempotent. It applies the robots policy and the vanity name
before it deploys, because the site serves each one as it stood at its last
publish, and it writes nothing when both already match.

It has no repair for content with no `current_revision`, the state that
`deploy_app.R` fixes in `ensureFreshBundle()`. Only `POST .../republish` causes
that state and nothing here calls it. If `Invalid token` ever appears, copy
that function rather than deleting and recreating the content.
