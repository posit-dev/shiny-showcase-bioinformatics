# Shared HTTP transport ---------------------------------------------------------
#
# Every source adapter used to carry its own near-identical 15-line httr2
# pipeline (seven copies), each of which collapsed *every* failure to NULL via
# `tryCatch(..., error = function(e) NULL)`. That made a 500, a DNS failure, a
# timeout and a genuinely empty result indistinguishable, so a source that was
# completely down surfaced in the UI as "ok, 0 genes".
#
# These helpers delegate to {biohttp}, which returns a normalised *envelope*
# rather than raising, and distinguishes transport failure from non-success
# status from an unreadable body. That also brings per-host circuit breaking,
# a transient-only retry predicate, throttling and a success-only response
# cache, none of which the hand-rolled version had.
#
# Envelope shape: list(ok, status, http, data, source, error, detail, ts) where
# `status` is one of biohttp::STATUS_LEVELS.
#
# ALWAYS call these as biohttp::/bioclients:: qualified. Never attach either
# package: this app sources into the global environment, so a global would
# shadow the namespace.

# --- Last-transport-status recorder -------------------------------------------
#
# Adapters return a canonical tibble and degrade to `empty_gene_table()` on any
# failure, so by the time the registry sees the result the *reason* is gone.
# Rather than rewrite all seven adapters (and every injected test stub) to pass
# envelopes around, the transport helpers record the worst status they saw, and
# `.run_one()` reads it straight after calling the adapter.
#
# This is deliberately process-global state. It is safe here because Shiny is
# single-threaded and `run_sources()` runs strictly sequentially, one source at
# a time, resetting before each. It would NOT be safe under ExtendedTask or any
# concurrent runner: revisit this if the pipeline ever becomes async.
.glb_http_status <- new.env(parent = emptyenv())

# Clear the recorder. Called once per source, before its adapter runs.
glb_status_reset <- function() {
  .glb_http_status$worst <- NULL
  .glb_http_status$message <- NULL
  invisible(NULL)
}

# Record one envelope, keeping only the most severe status seen. biohttp orders
# STATUS_LEVELS from benign to severe, so a later index wins. A source that
# makes many calls (PanelApp's page walk, DGIdb's chunks) reports the worst.
glb_status_record <- function(res) {
  status <- res$status %||% "ok"
  cur <- .glb_http_status$worst
  rank <- function(s) match(s, biohttp::STATUS_LEVELS, nomatch = 1L)
  if (is.null(cur) || rank(status) > rank(cur)) {
    .glb_http_status$worst <- status
    .glb_http_status$message <- res$error
  }
  invisible(res)
}

# Worst status recorded since the last reset, or NULL if nothing was recorded
# (which happens when a test injects a stub instead of hitting the transport).
glb_status_worst <- function() {
  if (is.null(.glb_http_status$worst)) {
    return(NULL)
  }
  list(status = .glb_http_status$worst, message = .glb_http_status$message)
}

# POST a GraphQL query. Returns an envelope. A GraphQL-level `errors` payload
# (HTTP 200 with an error body) is converted into an error envelope, so callers
# do not have to remember to check for it. Four of the seven original wrappers
# forgot to.
glb_graphql <- function(
  url,
  query,
  variables = list(),
  source = "API",
  timeout = 20,
  max_tries = 3
) {
  res <- biohttp::post_json(
    url,
    body = list(query = query, variables = variables),
    source = source,
    timeout = timeout,
    max_tries = max_tries
  )
  gql_err <- biohttp::graphql_error(res, source)
  if (!is.null(gql_err)) {
    return(glb_status_record(gql_err))
  }
  glb_status_record(res)
}

# GET JSON. Returns an envelope. `secret_query` is applied at dispatch only, so
# API keys stay out of cache keys and out of error text.
glb_get <- function(
  base_url,
  path = NULL,
  query = list(),
  source = "API",
  timeout = 20,
  max_tries = 3,
  throttle = NULL,
  secret_query = NULL
) {
  glb_status_record(biohttp::get_json(
    base_url,
    path = path,
    query = query,
    source = source,
    timeout = timeout,
    max_tries = max_tries,
    throttle = throttle,
    secret_query = secret_query
  ))
}

# Unwrap a bioclients envelope AND record its status.
#
# bioclients uses biohttp's transport directly, so its calls never pass through
# glb_graphql()/glb_get() and would otherwise be invisible to the recorder.
# Without this, a migrated adapter whose API is down reports "no_data" (the
# source answered, nothing found) instead of "error" -- exactly the bug the
# status work set out to fix. Every bioclients call must go through here.
glb_client_body <- function(res) {
  biohttp::body_or_null(glb_status_record(res))
}

# The `data` field of a GraphQL envelope, or NULL. Preserves the exact contract
# the old hand-rolled wrappers had, so adapters and their injected test stubs
# keep working unchanged while the status travels separately.
glb_graphql_data <- function(res) {
  body <- biohttp::body_or_null(res)
  if (is.null(body)) {
    return(NULL)
  }
  body$data
}
