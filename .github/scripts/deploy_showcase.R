# Deploys the gallery website itself to Posit Connect Cloud.
#
# This is the site that `index.qmd` renders, and not an application in `apps/`.
# `.github/scripts/deploy_app.R` is the other script, it deploys one
# application from its manifest, and the two have no code in common because a
# static site has no manifest, no packages and no primary R file.
#
# Nothing in CI operates this script, and nothing can: see the comment on
# authentication below. Run it by hand, through the `/publish-showcase` skill,
# which has the login that Connect Cloud needs and the steps around it.
#
#   quarto render
#   Rscript .github/scripts/deploy_showcase.R
#
# Run it under renv, unlike deploy_app.R. R/renv.lock records rsconnect 1.11.0
# for this script, so the renv library is where the right version is; a system
# rsconnect is whatever happens to be installed, and 1.10.1 cannot write the
# deployment record from a content id.

# The account and the name are here because nothing else records them. This is
# not a second copy of an apps.yml value: apps.yml describes tiles and their
# applications, and the site that shows those tiles is not one of them.
#
# Connect Cloud prefixes the account name to the vanity name, so these two
# together are the address:
#
#   https://posit-shiny-showcase-bioinformatics.share.connect.posit.cloud/
#
# `name` is both the vanity name and the name of the local rsconnect record.
# One value, because two would be one address and a different filename for no
# reason.
account <- "posit"
name <- "shiny-showcase-bioinformatics"

# The content that receives the deployment. Empty until the first run creates
# it; that run prints the id and asks for it to be written here. rsconnect
# cannot find Connect Cloud content by name, so this id is the only stable
# identifier of the site.
contentId <- "01a08804-0b57-662e-2bd8-cd3b64a6012e"

siteDir <- "_site"
stopifnot(
  "no _site/index.html: run `quarto render` first" =
    file.exists(file.path(siteDir, "index.html"))
)

# No client-credentials branch, unlike deploy_app.R. The `posit` account has
# `sso_enabled: true`, and Connect Cloud rejects a write from any token that did
# not come through the organization's SSO, so `PCC_CLIENT_ID` cannot publish
# this content. The account must be registered by
# `rsconnect::connectCloudUser()`, interactively, after an SSO login in the
# browser. That is why no workflow operates this script, and why it cannot be
# made to.
stopifnot(
  "the account is not registered: see the /publish-showcase skill" =
    account %in% rsconnect::accounts(server = "connect.posit.cloud")$name
)

# `recordDir` keeps rsconnect's deployment record out of `_site`, which
# `quarto render` empties on every run. The record lands in `./rsconnect/`,
# which git ignores, beside the records that deploy_app.R writes.
recordDir <- "."

if (nzchar(contentId)) {
  record <- rsconnect::deployments(
    appPath = recordDir,
    nameFilter = name,
    serverFilter = "connect.posit.cloud"
  )
  if (nrow(record) == 0) {
    # Only this branch needs the version, so the check belongs here and not at
    # the top: a machine that already holds the record deploys on 1.10.1.
    stopifnot(
      "rsconnect 1.11.0 or later is necessary for migrateToConnectCloud()" =
        utils::packageVersion("rsconnect") >= "1.11.0"
    )
    rsconnect::migrateToConnectCloud(
      appPath = recordDir,
      contentId = contentId,
      cloudAccount = account,
      appName = name
    )
  } else if (!identical(record$appId[[1]], contentId)) {
    stop(sprintf(
      "the deployment record names content %s, and this script names %s",
      record$appId[[1]],
      contentId
    ))
  }
}

# The robots policy and the vanity name belong to the content, but the site
# serves each one as it stood at its last publish, so they are applied before
# the deployment and land in the same run. `domain_id` must be in the payload,
# and null selects the shared share.connect.posit.cloud domain; without the key
# Connect Cloud answers 422. rsconnect has no argument for either setting, so
# these go to the API directly, through the client's own token refresh.
applyContentSettings <- function(id, policy = "allow_all") {
  info <- rsconnect:::accountInfo(account, "connect.posit.cloud")
  client <- rsconnect:::clientForAccount(info)
  content <- client$getContent(id)

  if (
    identical(content$default_robots_policy, policy) &&
      identical(content$vanity_name, name)
  ) {
    cat(sprintf(
      "Settings already correct: robots %s, address https://%s.share.connect.posit.cloud/\n",
      policy,
      content$vanity_domain
    ))
    return(invisible())
  }

  content <- client$withTokenRefreshRetry(
    rsconnect:::PATCH_JSON,
    paste0("/contents/", id),
    list(
      default_robots_policy = policy,
      vanity_name = name,
      domain_id = NULL
    )
  )
  stopifnot(
    "Connect Cloud did not accept the robots policy" =
      identical(content$default_robots_policy, policy),
    "Connect Cloud did not accept the vanity name" =
      identical(content$vanity_name, name)
  )

  cat(sprintf(
    "Settings changed: robots %s, address https://%s.share.connect.posit.cloud/\n",
    content$default_robots_policy,
    content$vanity_domain
  ))
}

if (nzchar(contentId)) {
  applyContentSettings(contentId)
}

# rsconnect infers the mode from the directory: `_site` holds HTML and no R, so
# the mode is static and the primary file is index.html. That inference is why
# this script needs none of the shims that deploy_app.R installs; those exist
# because a manifest suppresses it. `contentCategory = "site"` is what
# deploySite() passes for a multi-page site.
#
# deployApp() returns FALSE invisibly for a failed publish, so take the value
# and stop. Without that this script exits 0 for a site that never published.
deployed <- rsconnect::deployApp(
  appDir = siteDir,
  recordDir = recordDir,
  appName = name,
  appTitle = "Shiny in Bioinformatics",
  contentCategory = "site",
  account = account,
  server = "connect.posit.cloud",
  forceUpdate = TRUE,
  logLevel = "verbose"
)
if (!isTRUE(deployed)) {
  stop("the showcase did not deploy; the log above holds the reason")
}

if (!nzchar(contentId)) {
  deployedId <- rsconnect::deployments(
    appPath = recordDir,
    nameFilter = name
  )$appId[[1]]
  applyContentSettings(deployedId)
  cat(sprintf(
    paste0(
      "\nNew content. Confirm that its access is public, then write the id ",
      "into the `contentId` of this script:\n  contentId <- \"%s\"\n",
      "Address: https://%s-%s.share.connect.posit.cloud/\n"
    ),
    deployedId,
    account,
    name
  ))
}
