# Deploys one application to Posit Connect Cloud.
#
# .github/workflows/deploy-apps.yml operates this script, once per application,
# and passes every value in an environment variable. It also runs on your
# machine; see the "Deployment to Connect Cloud" section of README.md.
#
# The script deploys from the committed manifest.json, with the manifestPath
# argument of deployApp(). Therefore it needs no dependency of the application
# itself: rsconnect reads the file list and the package list from the manifest,
# and Connect Cloud installs the packages. A deployment that regenerated the
# manifest would need every package of every application on the runner, and
# three of the applications name packages that are not on CRAN.
#
# CONTENT_ID is the content that receives the deployment. An empty CONTENT_ID
# creates new content and prints its id; the workflow never does this, because
# it deploys only the applications that already have an id.
#
# rsconnect 1.11.2 or later. Earlier versions could not deploy from a manifest
# to Connect Cloud, nor deploy to content by id, nor redeploy content whose
# last publish failed; this script once carried a work-around for each.
stopifnot(
  "rsconnect 1.11.2 or later is necessary" =
    utils::packageVersion("rsconnect") >= "1.11.2"
)

app <- Sys.getenv("APP")
contentId <- Sys.getenv("CONTENT_ID")
account <- Sys.getenv("PCC_ACCOUNT")
clientId <- Sys.getenv("PCC_CLIENT_ID")
clientSecret <- Sys.getenv("PCC_CLIENT_SECRET")

stopifnot(
  "APP is empty" = nzchar(app),
  # No default. The account belongs to apps.yml, which the workflow reads
  # through deploy_matrix.py, so a hard-coded name here would be a second copy.
  "PCC_ACCOUNT is empty" = nzchar(account)
)

appDir <- file.path("apps", app)
manifestPath <- file.path(appDir, "manifest.json")
stopifnot("no manifest.json" = file.exists(manifestPath))

# An empty client id means an interactive session, where the account is already
# registered with connectCloudUser().
#
# `account` is the spelling that the Connect Cloud documentation uses, and it
# partial-matches the `accountName` formal. The credentials must grant publish
# permission on this account; CLAUDE.md, fault 5, has what the failure looks
# like when they do not.
#
# With PCC_ACCOUNT_ID, the script registers the account itself and skips that
# check: connectCloudClientCredentials() trusts only the permissions that
# `GET /v1/accounts` advertises, and the API that receives the deployment is
# the better judge. The id comes from a secret, and must never be printed;
# CLAUDE.md, "This repository is public".
#
# ponytail: a private rsconnect function, for as long as the advertised
# permissions disagree with the API.
accountId <- Sys.getenv("PCC_ACCOUNT_ID")
if (nzchar(clientId) && nzchar(accountId)) {
  token <- rsconnect:::cloudAuthClient()$exchangeClientCredentials(
    clientId,
    clientSecret
  )
  rsconnect:::registerAccount(
    serverName = "connect.posit.cloud",
    accountName = account,
    accountId = accountId,
    accessToken = token$access_token,
    refreshToken = token$refresh_token,
    clientId = clientId,
    clientSecret = clientSecret
  )
} else if (nzchar(clientId)) {
  rsconnect::connectCloudClientCredentials(
    clientId = clientId,
    clientSecret = clientSecret,
    account = account
  )
}

# Two settings of the content that this repository controls, and that a
# deployment does not carry:
#
# `default_robots_policy`. Connect Cloud creates content with `disallow_all`,
# and this is a gallery, so the applications must be findable.
#
# `vanity_name`. It replaces the content id in the address with the name of the
# application. Connect Cloud prefixes the account name, so `vanity_name` of
# `genescout` under an account named `posit` serves at
# https://posit-genescout.share.connect.posit.cloud/. The old address of the
# content redirects to the new one, so no link breaks. This makes the address of
# every application a function of PCC_ACCOUNT and the directory name, which is
# what lets `showcase.ejs` build the same address with no URL written by hand.
#
# Both belong to the content and not to a revision, but the application serves
# each one as it stood at its last publish. So this runs *before* the
# deployment, and the change lands in the same run. Observed on healthy
# content: a PATCH of `default_robots_policy`, then a deployment, and the
# robots.txt of the application follows immediately.
#
# Do not republish to land a change instead. `POST /contents/{id}/republish`
# leaves the content with no `current_revision` and a published
# `next_revision`.
#
# The function reads the content first, and writes only when a value differs,
# so the usual run makes one request and changes nothing. rsconnect has no
# argument for either setting, so the requests go to the Connect Cloud API
# directly. `withTokenRefreshRetry` is the client's own wrapper, and it mints a
# fresh token when the current one has expired.
#
# `domain_id` must be in the payload, and null. Connect Cloud rejects a
# `vanity_name` with no `domain_id` key at all, and a null value is what selects
# the shared `share.connect.posit.cloud` domain instead of a custom domain of
# the account.
applyContentSettings <- function(id, policy = "allow_all") {
  info <- rsconnect:::accountInfo(account, "connect.posit.cloud")
  client <- rsconnect:::clientForAccount(info)
  content <- client$getContent(id)

  if (
    identical(content$default_robots_policy, policy) &&
      identical(content$vanity_name, app)
  ) {
    cat(sprintf(
      "Settings of %s already correct: robots %s, address https://%s.share.connect.posit.cloud/\n",
      app,
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
      vanity_name = app,
      domain_id = NULL
    )
  )
  stopifnot(
    "Connect Cloud did not accept the robots policy" =
      identical(content$default_robots_policy, policy),
    "Connect Cloud did not accept the vanity name" =
      identical(content$vanity_name, app)
  )

  cat(sprintf(
    "Settings of %s changed: robots %s, address https://%s.share.connect.posit.cloud/\n",
    app,
    content$default_robots_policy,
    content$vanity_domain
  ))
}

if (nzchar(contentId)) {
  applyContentSettings(contentId)
}

# deployApp() does not signal a failed publish. It prints "Deployment failed
# with error: ..." and returns FALSE, invisibly, so a script that ignores the
# value exits 0 for an application that uploaded correctly and then failed to
# start. This job would have been green for a deployment that never ran.
#
# So take the value and stop. Connect Cloud installs the packages and starts
# the application after the upload, and that is where an application with a
# broken .Rprofile or an unresolvable package fails.
deployed <- rsconnect::deployApp(
  appDir = appDir,
  manifestPath = manifestPath,
  # The content id is the only stable identifier on Connect Cloud. NULL, for
  # an empty CONTENT_ID, creates new content.
  appId = if (nzchar(contentId)) contentId,
  appName = app,
  appTitle = app,
  account = account,
  server = "connect.posit.cloud",
  # No prompt on a runner.
  forceUpdate = TRUE,
  logLevel = "verbose"
)
if (!isTRUE(deployed)) {
  stop(sprintf(
    "%s did not deploy. The log above holds the reason; a failure after the upload is usually the application failing to start on Connect Cloud.",
    app
  ))
}

if (!nzchar(contentId)) {
  # deployApp() returns whether it succeeded, not the content, so the id of new
  # content comes from the deployment record that it just wrote. The content did
  # not exist before the deployment, so its settings could not be applied
  # before it, and the next deployment is what makes the application serve
  # them.
  deployedId <- rsconnect::deployments(appPath = appDir)$appId[[1]]
  applyContentSettings(deployedId)
  cat(sprintf(
    "\nNew content for %s. Confirm that its access is public, then write this into the tile in apps.yml:\n      app: %s\n      content_id: \"%s\"\nAddress: https://%s-%s.share.connect.posit.cloud/\n",
    app,
    app,
    deployedId,
    account,
    app
  ))
}
