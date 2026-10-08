# Background execution of the AI steps (input curator, final curator, specialists).
#
# The AI steps make long, blocking libcurl calls through ellmer. They run in a
# pool of background R processes (mirai daemons) and never in the Shiny process,
# for two reasons:
#  - One R process serves every visitor of the app. A step that ran there, or
#    that the process waited on, would make every other visitor wait for it.
#  - An R interrupt that lands inside a libcurl call can crash R. In a worker
#    that costs at most the worker, and the pool starts a new one.
#
# Each Shiny session runs a step through an ExtendedTask (genescout_llm_task()),
# so only that session waits for the result. A run stops after a time limit, and
# when its session ends.
#
# The pool starts on the first AI step, not at app start, and lives until the app
# stops (global.R registers genescout_llm_pool_close() with onStop). Its size and
# the time limit are options, with environment variables for hosts such as Posit
# Connect Cloud, where only variables can be set:
#   genescout.llm.workers      GENESCOUT_LLM_WORKERS      default 2
#   genescout.llm.timeout_ms   GENESCOUT_LLM_TIMEOUT_MS   default 10 minutes
#
# Under testthat, with options(genescout.llm.offload = FALSE), or without mirai,
# a step runs in the Shiny process and comes back as a resolved promise.
#
# ellmer's parallel_chat is in-process (httr2 curl-multi), NOT mirai, so a whole
# step - including the three parallel specialists - runs as ONE task on one
# worker. A worker inherits the API keys already in the environment (loaded from
# .Renviron before the pool starts); a key pasted in the app travels inside the
# step's `config` argument.

GENESCOUT_LLM_COMPUTE <- "genescout_llm"
GENESCOUT_LLM_WORKERS <- 2L
GENESCOUT_LLM_TIMEOUT_MS <- 10 * 60 * 1000
# How long a new pool may take for its workers to connect before it counts as
# failed and is started again.
GENESCOUT_LLM_START_GRACE_S <- 60

# The pool of this R process: its size and when it started.
.genescout_llm_pool <- new.env(parent = emptyenv())

# Is background LLM execution allowed right now? Never under testthat, only when enabled
# (default TRUE), and only when mirai is installed.
genescout_llm_offload_available <- function() {
  !identical(Sys.getenv("TESTTHAT"), "true") &&
    isTRUE(getOption("genescout.llm.offload", TRUE)) &&
    requireNamespace("mirai", quietly = TRUE)
}

# Source the engine (R/ + R/tools/, not the Shiny modules/UI) into the global
# environment of every worker, so a step function and its dependencies resolve
# there. Mirrors the app's load_components.R and the enrichment worker bootstrap
# in R/parallel.R; it must be inlined in the everywhere() expression (a worker has
# no engine until it runs). `.min = n` makes the first steps wait until all `n`
# workers have it. Returns TRUE when the bootstrap was sent.
genescout_llm_bootstrap <- function(root, libs, n = 1L) {
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
        .min = n,
        .compute = GENESCOUT_LLM_COMPUTE
      )
      TRUE
    },
    error = function(e) FALSE
  )
}

# --- The pool -------------------------------------------------------------------

# Pool size and time limit: the option, else the environment variable, else the
# default.
genescout_llm_workers <- function() {
  n <- suppressWarnings(as.integer(getOption(
    "genescout.llm.workers",
    Sys.getenv("GENESCOUT_LLM_WORKERS", "")
  )))
  if (length(n) != 1 || is.na(n) || n < 1) GENESCOUT_LLM_WORKERS else n
}

genescout_llm_timeout_ms <- function() {
  ms <- suppressWarnings(as.numeric(getOption(
    "genescout.llm.timeout_ms",
    Sys.getenv("GENESCOUT_LLM_TIMEOUT_MS", "")
  )))
  if (length(ms) != 1 || is.na(ms) || ms <= 0) GENESCOUT_LLM_TIMEOUT_MS else ms
}

# Start the pool, or start it again. daemons(n) would wait, with no deadline, for
# n workers to connect, and freeze the app if one never starts. A pool that
# listens on a local url and launches its workers separately returns at once; a
# worker that never connects then makes a step time out instead. Calling
# daemons() again also closes an earlier pool on this profile.
genescout_llm_pool_start <- function(n = genescout_llm_workers()) {
  mirai::daemons(url = mirai::local_url(), .compute = GENESCOUT_LLM_COMPUTE)
  invisible(mirai::launch_local(n, .compute = GENESCOUT_LLM_COMPUTE))
  if (isTRUE(getOption("genescout.llm.bootstrap", TRUE))) {
    sent <- genescout_llm_bootstrap(genescout_engine_root(), .libPaths(), n)
    if (!sent) {
      message("GeneScout: could not send the engine to the AI workers.")
    }
  }
  .genescout_llm_pool$n <- n
  .genescout_llm_pool$started <- Sys.time()
  message(sprintf(
    "GeneScout: started %d background worker(s) for AI steps.",
    n
  ))
  invisible(n)
}

# Is the pool fit for the next step? FALSE when there is no pool, when its
# workers did not connect within the grace period, or when a worker stopped and
# the pool is idle (or has no worker left). A pool that lost a worker keeps
# serving on the others while it is busy, so a restart never cuts a running step.
genescout_llm_pool_ok <- function() {
  if (!mirai::daemons_set(.compute = GENESCOUT_LLM_COMPUTE)) {
    return(FALSE)
  }
  inf <- mirai::info(.compute = GENESCOUT_LLM_COMPUTE)
  n <- .genescout_llm_pool$n %||% 1L
  if (inf[["connections"]] >= n) {
    return(TRUE)
  }
  if (inf[["cumulative"]] < n) {
    waited <- difftime(Sys.time(), .genescout_llm_pool$started, units = "secs")
    return(as.numeric(waited) < GENESCOUT_LLM_START_GRACE_S)
  }
  inf[["connections"]] > 0 && inf[["executing"]] + inf[["awaiting"]] > 0
}

# Close the pool. Registered with onStop in global.R.
genescout_llm_pool_close <- function() {
  if (
    requireNamespace("mirai", quietly = TRUE) &&
      mirai::daemons_set(.compute = GENESCOUT_LLM_COMPUTE)
  ) {
    mirai::daemons(0, .compute = GENESCOUT_LLM_COMPUTE)
  }
  .genescout_llm_pool$n <- NULL
  invisible(NULL)
}

# --- One step -------------------------------------------------------------------

# Start `fn(...)` and return at once: a mirai when it runs on the pool, else a
# promise of the in-process value (an error becomes a rejected promise). `fn` is a
# step function defined in the global environment, so it resolves in a worker's
# sourced engine; `...` are plain data (the ranked result, config, sizes). A failed
# step is never run again in this process: that would make everyone wait again.
genescout_llm_submit <- function(fn, ...) {
  if (!genescout_llm_offload_available()) {
    return(promises::promise_resolve(fn(...)))
  }
  if (!genescout_llm_pool_ok()) {
    genescout_llm_pool_start()
  }
  mirai::mirai(
    do.call(fn, args),
    .args = list(fn = fn, args = list(...)),
    .timeout = genescout_llm_timeout_ms(),
    .compute = GENESCOUT_LLM_COMPUTE
  )
}

# Stop a running step. TRUE when a step was stopped.
genescout_llm_cancel <- function(x) {
  if (inherits(x, "mirai")) mirai::stop_mirai(x) else FALSE
}

# --- Shiny glue (the workers source this file but never call these) -------------

# An ExtendedTask for one kind of step in one session. `$task$invoke(fn, ...)`
# starts a step, `$cancel()` stops the running one, and `$outcome()` reads the
# finished one (see genescout_llm_outcome()). The step stops when the session ends.
genescout_llm_task <- function(session = shiny::getDefaultReactiveDomain()) {
  inflight <- NULL
  ended <- FALSE
  task <- shiny::ExtendedTask$new(function(fn, ...) {
    inflight <<- NULL
    step <- genescout_llm_submit(fn, ...)
    inflight <<- step
    # Settle the task only while the session is open: a task that settles after
    # its session ended updates reactive state that no longer exists, which
    # Shiny logs as an error. A background failure (timed out, cancelled, worker
    # stopped) settles as its value, which genescout_llm_outcome() reads; only an
    # in-process error stays an error.
    promises::promise(function(resolve, reject) {
      promises::then(
        step,
        onFulfilled = function(value) {
          if (!ended) resolve(value)
        },
        onRejected = function(error) {
          if (ended) {
            return(invisible())
          }
          if (!inherits(step, "mirai")) {
            return(reject(error))
          }
          genescout_llm_log_failure(step$data)
          resolve(step$data)
        }
      )
    })
  })
  cancel <- function() genescout_llm_cancel(inflight)
  session$onSessionEnded(function() {
    ended <<- TRUE
    if (isTRUE(cancel())) {
      message("GeneScout: stopped an AI step because its session ended.")
    }
  })
  list(
    task = task,
    cancel = cancel,
    outcome = function() genescout_llm_outcome(task, inflight)
  )
}

# NULL while a step has not finished. Then list(ok = TRUE, value) or
# list(ok = FALSE, message), where `message` is NULL for a step that was stopped
# on purpose. A background failure settles the task as a "success" whose value
# is an errorValue (see genescout_llm_task()), so the mirai is read first.
genescout_llm_outcome <- function(task, m) {
  status <- task$status()
  if (!status %in% c("success", "error")) {
    return(NULL)
  }
  value <- if (inherits(m, "mirai")) m$data else NULL
  if (inherits(value, "errorValue")) {
    return(list(ok = FALSE, message = genescout_llm_failure(value)))
  }
  if (identical(status, "error")) {
    msg <- tryCatch(
      {
        task$result()
        "the AI step failed."
      },
      error = function(e) conditionMessage(e)
    )
    return(list(ok = FALSE, message = msg))
  }
  list(ok = TRUE, value = task$result())
}

# One line in the server log for a failed background step: the kind of failure,
# never its message, so no API key can reach the log. A step cancelled on
# purpose is not logged here (the session-end stop logs its own line).
genescout_llm_log_failure <- function(x) {
  kind <- if (inherits(x, "miraiError")) {
    "an R error in the step"
  } else {
    switch(
      as.character(suppressWarnings(as.integer(unclass(x)))),
      "5" = "it timed out",
      "19" = "its worker stopped",
      "20" = NULL,
      "an unknown failure"
    )
  }
  if (!is.null(kind)) {
    message("GeneScout: an AI step failed: ", kind, ".")
  }
  invisible(kind)
}

# A plain message for a failed background step, to follow "Curation failed: " and
# the like, or NULL for a step that was cancelled on purpose. mirai reports a
# step's R error as a miraiError, an interrupt as a miraiInterrupt, and otherwise a
# code: 5 timed out, 19 the worker stopped, 20 cancelled.
genescout_llm_failure <- function(x) {
  if (inherits(x, "miraiError")) {
    return(conditionMessage(x))
  }
  if (inherits(x, "miraiInterrupt")) {
    return("the AI step was interrupted. Please try again.")
  }
  code <- suppressWarnings(as.integer(unclass(x)))
  if (identical(code, 5L)) {
    ms <- genescout_llm_timeout_ms()
    limit <- if (ms < 60000) {
      seconds <- as.integer(max(1, ceiling(ms / 1000)))
      sprintf("%d second%s", seconds, if (seconds == 1L) "" else "s")
    } else {
      sprintf("%s minutes", format(round(ms / 60000, 1)))
    }
    return(sprintf("it ran longer than %s and was stopped.", limit))
  }
  if (identical(code, 19L)) {
    return("the background worker stopped. Please try again.")
  }
  if (identical(code, 20L)) {
    return(NULL)
  }
  sprintf(
    "the background worker failed (code %s). Please try again.",
    code
  )
}
