# Crash-safe LLM stage execution.
#
# The AI stages (input curator, final curator, specialists) make blocking libcurl
# calls through ellmer. When one of those runs directly in the Shiny R session and the
# user hits Stop (an R interrupt) or refreshes mid-call, interrupting libcurl in-flight
# can SEGFAULT the whole session - a hard crash, not a caught error. This module runs
# each LLM stage in a short-lived BACKGROUND R process (a mirai daemon) instead, so the
# main session only ever blocks on an interrupt-safe nanonext receive, never inside
# curl: stopping or refreshing then cleanly cancels the wait and, at worst, orphans the
# background worker - it cannot crash the user's session.
#
# This is a SAFETY wrapper, not an optimization or a behavior change:
#  - The offloaded function (curate_gene_list / run_specialists / curate_input) is the
#    same one, run with the same args; it returns the same value.
#  - Off under testthat (offline, deterministic) and opt-out via an option; when mirai
#    is absent the call simply runs in-process, exactly as before.
#  - Any daemon/serialization failure falls back to an in-process call, so a run never
#    breaks - it only loses the crash-isolation. (ellmer's own errors are already caught
#    INSIDE the stages, which return a graceful value, so a worker error here means the
#    infrastructure failed, not the model.)
#
# ellmer's parallel_chat is in-process (httr2 curl-multi), NOT mirai, so an entire
# stage - including the three parallel specialists - offloads as ONE task with no
# nested daemons. The daemon inherits the API credentials already in the session
# environment (loaded from .Renviron before any daemon launches) and sources the engine
# so the stage function and ellmer resolve there.

GENESCOUT_LLM_COMPUTE <- "genescout_llm"

# Is background LLM execution allowed right now? Never under testthat, only when enabled
# (default TRUE), and only when mirai is installed.
genescout_llm_offload_available <- function() {
  !identical(Sys.getenv("TESTTHAT"), "true") &&
    isTRUE(getOption("genescout.llm.offload", TRUE)) &&
    requireNamespace("mirai", quietly = TRUE)
}

# Source the engine (R/ + R/tools/, not the Shiny modules/UI) into a daemon's global
# environment so an offloaded stage function and its dependencies resolve there. Mirrors
# the app's load_components.R and the enrichment worker bootstrap in R/parallel.R; it
# must be inlined in the everywhere() expression (the daemon has no engine until it
# runs). Returns TRUE on success.
genescout_llm_bootstrap <- function(root, libs) {
  tryCatch(
    {
      mirai::everywhere(
        {
          .libPaths(genescout_libs)
          Sys.setenv(GENESCOUT_APP_ROOT = genescout_root)
          eng <- list.files(
            file.path(genescout_root, "R"),
            pattern = "[.][Rr]$",
            full.names = TRUE
          )
          eng <- eng[basename(eng) != "load_components.R"]
          for (f in sort(eng)) {
            sys.source(f, envir = globalenv())
          }
          tools <- list.files(
            file.path(genescout_root, "R", "tools"),
            pattern = "[.][Rr]$",
            full.names = TRUE
          )
          for (f in sort(tools)) {
            sys.source(f, envir = globalenv())
          }
        },
        genescout_libs = libs,
        genescout_root = root,
        .compute = GENESCOUT_LLM_COMPUTE
      )
      TRUE
    },
    error = function(e) FALSE
  )
}

# Run `fn(...)` crash-safely, returning its value. When offloading is available the call
# executes in a background process and the caller blocks (interrupt-safe) for the
# result; otherwise, or on ANY worker failure, it runs in-process so behavior is
# unchanged. `fn` is a stage function (a global closure - it and ellmer resolve in the
# daemon's sourced engine); `...` are its plain-data arguments (the ranked result,
# config, sizes), which serialize cleanly.
genescout_llm_run <- function(fn, ...) {
  if (!genescout_llm_offload_available()) {
    return(fn(...))
  }
  args <- list(...)
  root <- genescout_engine_root()
  libs <- .libPaths()

  started <- tryCatch(
    {
      mirai::daemons(1, .compute = GENESCOUT_LLM_COMPUTE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!started) {
    return(fn(...))
  }
  on.exit(
    try(mirai::daemons(0, .compute = GENESCOUT_LLM_COMPUTE), silent = TRUE),
    add = TRUE
  )
  if (!genescout_llm_bootstrap(root, libs)) {
    return(fn(...))
  }

  m <- mirai::mirai(
    do.call(genescout_fn, genescout_args),
    genescout_fn = fn,
    genescout_args = args,
    .compute = GENESCOUT_LLM_COMPUTE
  )
  res <- m[] # blocks on an interrupt-safe nanonext receive, not inside libcurl
  if (mirai::is_error_value(res)) {
    # The worker infrastructure failed (the stage itself catches model errors and
    # returns a graceful value). Fall back in-process so the run still produces a
    # result rather than surfacing a daemon error.
    return(fn(...))
  }
  res
}
