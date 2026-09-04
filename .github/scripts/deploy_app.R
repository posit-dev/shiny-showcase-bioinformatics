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
# Updating existing content needs rsconnect 1.11.0 or later. The script works
# around several problems in that version, and each one has a comment below.

app <- Sys.getenv("APP")
contentId <- Sys.getenv("CONTENT_ID")
account <- Sys.getenv("PCC_ACCOUNT")
accountId <- Sys.getenv("PCC_ACCOUNT_ID")
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

manifest <- jsonlite::read_json(manifestPath)

# The primary file of the application, by the rule that
# rsconnect:::inferAppMode() applies to a Shiny application: app.R at the root
# if it exists, and server.R otherwise. The applications here use both styles.
primaryFileFromManifest <- function(manifest) {
  root <- names(manifest$files)[dirname(names(manifest$files)) == "."]
  for (candidate in c("app.R", "server.R")) {
    hit <- root[tolower(root) == tolower(candidate)]
    if (length(hit)) {
      return(hit[[1]])
    }
  }
  stop("manifest.json holds neither app.R nor server.R at its root")
}

# rsconnect 1.10.1 through 1.11.0 send `primary_file: null` to Connect Cloud
# when deployApp() gets manifestPath, and the API rejects the request with
# "Field `body.next_revision.primary_file` error". The value comes from
# appMetadata(), which infers it while it infers the application mode; a
# manifest supplies the mode, so rsconnect skips the inference and the value
# stays empty. This shim supplies it. It affects the creation of content and
# every later deployment, so both paths need it.
#
# ponytail: remove this when rsconnect reads the primary file from the
# manifest. `get()` fails loudly if that function changes name, and the
# condition below leaves a value that rsconnect does infer untouched.
installPrimaryFileShim <- function(primaryFile) {
  ns <- asNamespace("rsconnect")
  original <- get("appMetadata", envir = ns)
  patched <- function(...) {
    metadata <- original(...)
    if (
      is.null(metadata$appPrimaryDoc) && is.null(metadata$inferredPrimaryFile)
    ) {
      metadata$inferredPrimaryFile <- primaryFile
    }
    metadata
  }
  utils::assignInNamespace("appMetadata", patched, ns = "rsconnect")
}
installPrimaryFileShim(primaryFileFromManifest(manifest))

# Register the account that the deployment publishes to. connectCloudUser()
# has already done it in an interactive session, which is what an empty client
# id means. CLAUDE.md, fault 5, explains the account id and the fallback.
if (nzchar(clientId)) {
  registered <- FALSE
  for (candidate in Filter(nzchar, unique(c(accountId, account)))) {
    registered <- tryCatch(
      {
        rsconnect::connectCloudClientCredentials(
          clientId = clientId,
          clientSecret = clientSecret,
          account = candidate,
          # The local name of the record. It defaults to the name that the
          # lookup matched, and the rest of this script, deployApp() included,
          # reads the record by the name in PCC_ACCOUNT. So pin it, or a
          # registration that matched the id would be filed under the id.
          name = account
        )
        TRUE
      },
      error = function(e) {
        cat(sprintf(
          "connectCloudClientCredentials(account = \"%s\") did not register: %s\n",
          candidate,
          conditionMessage(e)
        ))
        FALSE
      }
    )
    if (registered) {
      cat(sprintf("Registered account %s, matched as \"%s\".\n", account, candidate))
      break
    }
  }

  if (!registered) {
    cat("Falling back to the account id in PCC_ACCOUNT_ID.\n")
    stopifnot(
      "PCC_ACCOUNT_ID is empty, so the account cannot be registered by id either" =
        nzchar(accountId)
    )
    tokens <- rsconnect:::cloudAuthClient()$exchangeClientCredentials(
      clientId,
      clientSecret
    )
    rsconnect:::registerAccount(
      serverName = "connect.posit.cloud",
      accountName = account,
      accountId = accountId,
      accessToken = tokens$access_token,
      refreshToken = tokens$refresh_token,
      clientId = clientId,
      clientSecret = clientSecret
    )
    cat(sprintf("Registered account %s by id.\n", account))
  }

  # What Connect Cloud says about the accounts these credentials can see, so
  # that a permission failure names its cause instead of appearing as a
  # deployment fault. This is the response that the lookup above judged. It is a
  # diagnostic and nothing depends on it, so it must not be what fails the job.
  #
  # **Print nothing beyond PCC_ACCOUNT itself.** This repository is public, so
  # its Actions logs are public. An account id is not ours to publish, and
  # neither is the name of another account these credentials happen to see. The
  # name of this one is already public, as `pcc-account` in apps.yml.
  try({
    info <- rsconnect:::accountInfo(account, "connect.posit.cloud")
    visible <- rsconnect:::clientForAccount(info)$getAccounts()$data
    mine <- Filter(function(a) identical(a$name, account), visible)
    for (a in mine) {
      cat(sprintf(
        "%s: role %s, content:create %s\n",
        a$name,
        a$role,
        "content:create" %in% unlist(a$permissions)
      ))
    }
    cat(sprintf(
      "%d account(s) visible to these credentials, %d named %s.\n",
      length(visible),
      length(mine),
      account
    ))
  })
}

# Point the local deployment record at the content that CONTENT_ID names.
# deployApp() needs that record: on Connect Cloud it cannot find content by
# name, and its appId argument does not work either, because the Connect Cloud
# client implements no getApplication(). A fresh runner holds no record, and
# git ignores the directory that holds one, so this step runs on every job.
if (nzchar(contentId)) {
  stopifnot(
    "rsconnect 1.11.0 or later is necessary for migrateToConnectCloud()" =
      utils::packageVersion("rsconnect") >= "1.11.0"
  )
  record <- rsconnect::deployments(
    appPath = appDir,
    serverFilter = "connect.posit.cloud"
  )
  if (nrow(record) == 0) {
    rsconnect::migrateToConnectCloud(
      appPath = appDir,
      contentId = contentId,
      cloudAccount = account,
      appName = app
    )
  } else if (!identical(record$appId[[1]], contentId)) {
    stop(sprintf(
      "the deployment record of %s names content %s, and CONTENT_ID names %s",
      app,
      record$appId[[1]],
      contentId
    ))
  }
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
# `next_revision`, and `ensureFreshBundle()` below then has to repair it on
# every later deployment.
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

# Repair content that has no `current_revision`, which `POST .../republish`
# leaves behind.
#
# rsconnect asks Connect Cloud for a new bundle only when the content has a
# current revision:
#
#   # current revision will be null only when creating new content
#   if (!is.null(application$current_revision)) { ... updateContent(...) }
#
# That comment is wrong. A republished content also has none, and then
# rsconnect skips the request that mints a bundle and uploads against the
# `source_bundle_upload_url` the content already carries. That token expires one
# hour after it was minted, so every deployment of such content fails with
# `Invalid token`, forever.
#
# `PATCH /contents/{id}?new_bundle=true` is the request rsconnect skipped. It
# creates a pending revision with a fresh token, which the deployment then
# finds and uploads to. This runs last, so nothing comes between minting the
# token and using it.
#
# ponytail: remove this when rsconnect asks for a bundle whatever the state of
# the content. `apps/variant-reviewer` is the content in this state.
ensureFreshBundle <- function(id) {
  info <- rsconnect:::accountInfo(account, "connect.posit.cloud")
  client <- rsconnect:::clientForAccount(info)
  if (!is.null(client$getContent(id)$current_revision$id)) {
    return(invisible())
  }
  content <- client$updateContent(
    id,
    envVars = character(),
    newBundle = TRUE,
    primaryFile = primaryFileFromManifest(manifest),
    appMode = manifest$metadata$appmode
  )
  cat(sprintf(
    "%s has no current revision, so this run minted revision %s to upload to.\n",
    app,
    content$next_revision$id
  ))
}

if (nzchar(contentId)) {
  applyContentSettings(contentId)
  ensureFreshBundle(contentId)
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
  appName = app,
  appTitle = app,
  account = account,
  server = "connect.posit.cloud",
  # No prompt on a runner. The content is the one that the record names.
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
