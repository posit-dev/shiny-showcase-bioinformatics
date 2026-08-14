# Optional parallel evidence retrieval.
#
# enrich_genes() (R/enrich.R) is a serial per-gene loop of BLOCKING HTTP calls: for
# N genes and ~8 active gene-signals it makes up to N x 8 requests one at a time. For
# a small list that is fine, but for a large list (discovery seeds dozens; a pasted
# panel can be hundreds) it is the dominant cost and the app looks frozen. This module
# fans the per-gene enrichment across a small pool of background R processes (mirai
# daemons), each of which sources the same engine and runs the SAME enrich_genes() on
# one gene, then binds the per-gene results back into the identical tables the serial
# path returns.
#
# Design rules that keep this safe:
#  - Parallelism is a PURE OPTIMIZATION. It is gated to large lists, disabled under
#    testthat, and any setup/serialization/daemon failure falls back to the serial
#    enrich_genes() - correctness never depends on it, and the output is byte-for-byte
#    the same shape (a single gene through enrich_genes() is just the serial loop of 1).
#  - The workers are isolated: a named mirai compute profile ("genescout_enrich") so we
#    never collide with the LLM path (ellmer's own parallel pool uses the default one),
#    the run's registry + context are shipped ONCE per daemon via everywhere(), and the
#    pool is torn down when the run ends.
#  - No new hard dependency at load time: mirai is probed with requireNamespace(); if it
#    is absent the app runs exactly as before (serial).

# Below this many genes the daemon startup + per-gene serialization overhead is not
# worth it, so we stay serial. Overridable for tuning/tests.
GENESCOUT_PARALLEL_MIN_GENES <- 12L
# Bounded concurrency: a politeness cap so we never open more simultaneous connections
# to a single bio-database host than is neighbourly (pairs with the per-host throttle
# and circuit-breaker in R/http.R). Overridable via the option below.
GENESCOUT_PARALLEL_MAX_WORKERS <- 6L
# The isolated mirai compute profile for enrichment (kept separate from ellmer's pool).
GENESCOUT_PARALLEL_COMPUTE <- "genescout_enrich"

# Is the parallel path allowed right now? Never under testthat (offline tests must stay
# serial and deterministic), only when explicitly enabled (default TRUE), and only when
# mirai is actually installed.
genescout_parallel_available <- function() {
  !identical(Sys.getenv("TESTTHAT"), "true") &&
    isTRUE(getOption("genescout.parallel", TRUE)) &&
    requireNamespace("mirai", quietly = TRUE)
}

# Worker count for a run of `n_genes`: the configured cap, never more than one per gene,
# and 1 (i.e. serial) when the parallel path is unavailable.
genescout_parallel_workers <- function(n_genes) {
  if (!genescout_parallel_available()) {
    return(1L)
  }
  cap <- as.integer(getOption(
    "genescout.parallel.workers",
    GENESCOUT_PARALLEL_MAX_WORKERS
  ))
  max(1L, min(cap, as.integer(n_genes)))
}

# The directory a worker must source the engine from. The Shiny app / CLI run with the
# app root as the working directory, and a hosted service sets GENESCOUT_APP_ROOT; use that
# when present, else the current working directory.
genescout_engine_root <- function() {
  root <- Sys.getenv("GENESCOUT_APP_ROOT", "")
  if (!nzchar(root)) {
    root <- getwd()
  }
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

# Choose serial vs parallel enrichment and return the SAME list(signals_long,
# evidence_long) shape either way. This is the single call site run_enrich() uses in
# place of enrich_genes(), so the rest of the pipeline is unchanged.
enrich_genes_dispatch <- function(
  resolved,
  registry,
  context = list(),
  progress = NULL
) {
  n <- nrow(resolved)
  workers <- if (n >= GENESCOUT_PARALLEL_MIN_GENES) {
    genescout_parallel_workers(n)
  } else {
    1L
  }
  if (workers <= 1L) {
    return(enrich_genes(
      resolved,
      registry,
      context = context,
      progress = progress
    ))
  }
  tryCatch(
    enrich_genes_parallel(resolved, registry, context, workers, progress),
    error = function(e) {
      # Parallel is only ever a speedup; a broken pool must never break a run. Warn
      # (so it is visible in logs) and fall back to the proven serial path.
      warning(
        "Parallel enrichment unavailable (",
        conditionMessage(e),
        "); running serially.",
        call. = FALSE
      )
      enrich_genes(
        resolved,
        registry,
        context = context,
        progress = progress
      )
    }
  )
}

# Fan the per-gene enrichment across `workers` mirai daemons. Each daemon sources the
# engine once and holds this run's registry + context; each gene is one task that runs
# the ordinary enrich_genes() on a single row. Results are collected in input order,
# ticking `progress` as each gene lands, and bound into the serial-path tables. A task
# that errors on a daemon is recomputed serially so no gene is silently dropped.
enrich_genes_parallel <- function(
  resolved,
  registry,
  context,
  workers,
  progress = NULL
) {
  root <- genescout_engine_root()
  libs <- .libPaths()
  gene_timeout <- as.numeric(getOption(
    "genescout.parallel.gene_timeout_ms",
    120000
  ))

  mirai::daemons(workers, .compute = GENESCOUT_PARALLEL_COMPUTE)
  on.exit(
    mirai::daemons(0, .compute = GENESCOUT_PARALLEL_COMPUTE),
    add = TRUE
  )

  # Bootstrap every daemon ONCE: use the same renv library, resolve app-relative files
  # regardless of the daemon's working directory, source the engine + tool clients (not
  # the Shiny modules/UI - a worker only enriches), and stash the run's registry/context
  # as daemon globals the per-gene tasks reference.
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
      assign(".genescout_reg", genescout_reg, envir = globalenv())
      assign(".genescout_ctx", genescout_ctx, envir = globalenv())
    },
    genescout_libs = libs,
    genescout_root = root,
    genescout_reg = registry,
    genescout_ctx = context,
    .compute = GENESCOUT_PARALLEL_COMPUTE
  )

  n <- nrow(resolved)
  jobs <- lapply(seq_len(n), function(i) {
    mirai::mirai(
      enrich_genes(genescout_row, .genescout_reg, context = .genescout_ctx),
      genescout_row = resolved[i, , drop = FALSE],
      .timeout = gene_timeout,
      .compute = GENESCOUT_PARALLEL_COMPUTE
    )
  })

  parts <- vector("list", n)
  for (i in seq_len(n)) {
    val <- jobs[[i]][] # blocks until this gene's task resolves
    if (mirai::is_error_value(val)) {
      # A daemon-side error or timeout for this gene: recompute it in-process so the
      # gene still contributes its rows (identical to the serial per-gene guarantee).
      val <- enrich_genes(
        resolved[i, , drop = FALSE],
        registry,
        context = context
      )
    }
    parts[[i]] <- val
    if (is.function(progress)) {
      progress(i, n, resolved$symbol[i])
    }
  }

  list(
    signals_long = dplyr::bind_rows(lapply(parts, `[[`, "signals_long")),
    evidence_long = dplyr::bind_rows(lapply(parts, `[[`, "evidence_long"))
  )
}
