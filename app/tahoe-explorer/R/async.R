# Optional non-blocking (async) execution for heavy obs work.
#
# The app's obs aggregates (the per-condition cell grid, and the interactive
# Cells-tab summaries) are cheap on the fixtures and on a downloaded local file,
# but in *remote* mode a single query scans the 2.29 GB HuggingFace parquet. With
# one process-global duckdb connection that scan blocks the whole R process, so on
# a shared/hosted deployment one user's query freezes every other session.
#
# This module lets those heavy calls run in a background worker via Shiny's
# ExtendedTask, so the main process stays responsive. It is deliberately:
#
#   * Opt-in. Nothing here runs unless TAHOE_ASYNC is truthy AND the `promises` +
#     `future` packages are installed AND a worker plan initialises. When it is
#     off (the default), .tahoe_async_enabled() returns FALSE and every module
#     takes exactly its original synchronous path -- behaviour is unchanged.
#   * Gracefully degrading. Any failure to set up the runtime is caught and
#     reported as "not enabled", so a misconfigured host silently falls back to
#     synchronous rather than erroring.
#   * Serialization-safe. duckdb connections are external pointers and cannot be
#     shipped to another process. The worker therefore closes over NOTHING from
#     the app: it receives only plain strings/lists as `globals`, re-sources
#     R/data.R inside the fresh worker process (which opens that worker's own
#     duckdb connection), runs the query, and returns a plain tibble. The
#     connection lives and dies inside the worker.
#
# Enable on a host with, e.g.:
#   TAHOE_ASYNC=1 TAHOE_ASYNC_WORKERS=2 TAHOE_OBS_REMOTE=1
# and verify end-to-end with dev/test-async.R.

# TRUE for a set, non-"false"/"0" environment value.
.tahoe_env_truthy <- function(v) {
  nzchar(v) && !identical(tolower(v), "false") && v != "0"
}

# Number of background workers (TAHOE_ASYNC_WORKERS, default 2, min 1).
.tahoe_async_workers <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("TAHOE_ASYNC_WORKERS", "2")))
  if (is.na(n) || n < 1L) 2L else n
}

# Establish the multisession worker plan once per process. Returns TRUE on
# success. Best-effort: a failure to plan (e.g. no permission to fork sessions)
# degrades to "async unavailable" rather than propagating. Cached so we set the
# plan at most once.
.tahoe_async_init <- function() {
  if (!is.null(.tahoe_cache$async_planned)) {
    return(.tahoe_cache$async_planned)
  }
  ok <- tryCatch(
    {
      future::plan(future::multisession, workers = .tahoe_async_workers())
      TRUE
    },
    error = function(e) FALSE
  )
  .tahoe_cache$async_planned <- isTRUE(ok)
  .tahoe_cache$async_planned
}

#' Whether heavy obs work should run asynchronously. TRUE only when opted in via
#' TAHOE_ASYNC, the async packages are installed, and a worker plan initialised.
#' Cached for the process lifetime, so the decision is stable within a session.
.tahoe_async_enabled <- function() {
  if (!is.null(.tahoe_cache$async_enabled)) {
    return(.tahoe_cache$async_enabled)
  }
  enabled <- .tahoe_env_truthy(Sys.getenv("TAHOE_ASYNC", "")) &&
    requireNamespace("promises", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE) &&
    .tahoe_async_init()
  .tahoe_cache$async_enabled <- isTRUE(enabled)
  .tahoe_cache$async_enabled
}

# Run a nullary/parameterised data-layer function `fn` in a fresh worker process
# and return a promise of its (plain-tibble) result. The worker closes over only
# `root`, `fn`, and `args` -- all plain, serializable values -- so no app state
# (and crucially no duckdb connection) is shipped. It re-sources R/data.R with
# the working directory at the app root, giving the worker its own connection and
# cache, then calls `fn` there. Attributes on the result (tahoe_source /
# tahoe_error) survive serialization, so callers see the same shape as the
# synchronous path.
.tahoe_worker_promise <- function(root, fn, args = list()) {
  promises::future_promise(
    {
      owd <- setwd(root)
      on.exit(setwd(owd), add = TRUE)
      source("R/data.R", local = TRUE)
      do.call(fn, args)
    },
    globals = list(root = root, fn = fn, args = args),
    packages = c("DBI", "duckdb", "dplyr", "stringr"),
    seed = TRUE
  )
}

#' An ExtendedTask that builds the per-condition cell grid in a background
#' worker. Invoke with `$invoke()`; read `$status()` / `$result()` reactively.
tahoe_make_grid_task <- function(root = .tahoe_root) {
  shiny::ExtendedTask$new(function() {
    .tahoe_worker_promise(root, "tahoe_cell_grid")
  })
}

#' An ExtendedTask that runs a Cells-tab obs summary in a background worker.
#' Invoke with `$invoke(group_by, filters, metric, limit)`.
tahoe_make_obs_task <- function(root = .tahoe_root) {
  shiny::ExtendedTask$new(function(group_by, filters, metric, limit) {
    .tahoe_worker_promise(
      root,
      "tahoe_obs_summary",
      list(
        group_by = group_by,
        filters = filters,
        metric = metric,
        limit = limit
      )
    )
  })
}
