# Source registry + orchestration ---------------------------------------------
#
# Adding a new source = write one fetch_*() returning the canonical schema, then
# register_source() it here. No pipeline changes required.
#
# Sources declare how they are driven:
#   needs = "disease" - independent; takes the resolved disease, returns genes
#   needs = "genes"   - dependent; annotates the union of genes found by the
#                       disease-driven sources (e.g. DGIdb drug interactions)

SOURCE_REGISTRY <- new.env(parent = emptyenv())

register_source <- function(
  id,
  label,
  fetch_fn,
  needs = c("disease", "genes"),
  role = c("evidence", "annotation"),
  norm_method = "rank",
  enabled = TRUE
) {
  needs <- match.arg(needs)
  role <- match.arg(role)
  assign(
    id,
    list(
      id = id,
      label = label,
      fetch_fn = fetch_fn,
      needs = needs,
      role = role,
      norm_method = norm_method,
      enabled = enabled
    ),
    envir = SOURCE_REGISTRY
  )
  invisible(id)
}

list_sources <- function() {
  lapply(sort(ls(SOURCE_REGISTRY)), get, envir = SOURCE_REGISTRY)
}

get_source <- function(id) {
  if (exists(id, envir = SOURCE_REGISTRY, inherits = FALSE)) {
    get(id, envir = SOURCE_REGISTRY)
  } else {
    NULL
  }
}

# Named vector source_id -> norm_method, optionally restricted to `ids`.
source_norm_methods <- function(ids = NULL) {
  srcs <- list_sources()
  m <- vapply(srcs, function(s) s$norm_method, character(1))
  names(m) <- vapply(srcs, function(s) s$id, character(1))
  if (!is.null(ids)) {
    m <- m[intersect(names(m), ids)]
  }
  m
}

# Ids of "evidence" sources (those that drive coverage), optionally restricted
# to `ids`. Annotation sources (gnomAD, Pharos) are excluded.
evidence_source_ids <- function(ids = NULL) {
  srcs <- list_sources()
  ev <- vapply(srcs, function(s) identical(s$role, "evidence"), logical(1))
  out <- vapply(srcs[ev], function(s) s$id, character(1))
  if (!is.null(ids)) {
    out <- intersect(out, ids)
  }
  out
}

# `status` is one of biohttp::STATUS_LEVELS. `ok` is DERIVED, never passed in,
# so the two can never disagree. Keeping `ok` means existing consumers
# (source_status_mod, report.R) keep working while callers migrate to `status`.
.status_row <- function(src, status, n_genes, latency_ms, message) {
  tibble::tibble(
    source_id = src$id,
    label = src$label,
    status = status,
    ok = identical(status, "ok"),
    n_genes = as.integer(n_genes),
    latency_ms = as.integer(latency_ms),
    message = message
  )
}

# Run a single source's fetch_fn with timing + error capture.
.run_one <- function(src, disease, gene_symbols, force) {
  key <- glb_cache_key(
    src$id,
    disease$id %||% "",
    if (identical(src$needs, "genes")) rlang::hash(sort(gene_symbols)) else ""
  )
  t0 <- Sys.time()
  result <- if (!force) glb_cache_get(key) else NULL
  from_cache <- !is.null(result)
  transport <- NULL
  if (is.null(result)) {
    # Clear the transport recorder so whatever this adapter reports is its own.
    glb_status_reset()
    result <- tryCatch(
      src$fetch_fn(disease = disease, gene_symbols = gene_symbols),
      error = function(e) e
    )
    transport <- glb_status_worst()
    # Cache only successful, non-empty results. Adapters return an empty table
    # on network/API error, so caching empties would pin a transient failure;
    # leaving them uncached lets the next run retry.
    if (!inherits(result, "error") && nrow(result) > 0) {
      glb_cache_set(key, result)
    }
  }
  latency <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  if (inherits(result, "error")) {
    # The adapter itself raised: that is a code fault, not a network one.
    return(list(
      genes = empty_gene_table(),
      status = .status_row(src, "error", 0L, latency, conditionMessage(result))
    ))
  }

  n <- nrow(result)
  # Precedence: a transport failure explains an empty table, so it wins over
  # "no_data". This is the whole point of the change: a source that timed out
  # must never again be reported as a source that simply found nothing.
  if (!is.null(transport) && !identical(transport$status, "ok")) {
    status <- transport$status
    message <- transport$message %||% status
  } else if (n == 0) {
    status <- "no_data"
    message <- "no genes returned"
  } else {
    status <- "ok"
    message <- if (from_cache) "ok (cached)" else "ok"
  }

  list(
    genes = result,
    status = .status_row(src, status, n, latency, message)
  )
}

# Orchestrate the selected sources. `on_progress(detail)` is called once per
# source (the UI maps it to incProgress). Returns:
#   list(results = named list of canonical tibbles, status = tibble)
run_sources <- function(
  disease,
  source_ids,
  force = FALSE,
  on_progress = NULL
) {
  if (truthy(Sys.getenv("GLB_TEST_MODE"))) {
    return(demo_run_sources(disease, source_ids))
  }

  selected <- Filter(
    function(s) !is.null(s) && s$enabled,
    lapply(source_ids, get_source)
  )
  disease_sources <- Filter(function(s) s$needs == "disease", selected)
  gene_sources <- Filter(function(s) s$needs == "genes", selected)

  results <- list()
  status <- list()

  for (src in disease_sources) {
    if (is.function(on_progress)) {
      on_progress(paste("Querying", src$label))
    }
    out <- .run_one(src, disease, NULL, force)
    results[[src$id]] <- out$genes
    status[[src$id]] <- out$status
  }

  seed_symbols <- unique(unlist(lapply(results, function(g) g$gene_symbol)))

  for (src in gene_sources) {
    if (is.function(on_progress)) {
      on_progress(paste("Querying", src$label))
    }
    out <- .run_one(src, disease, seed_symbols, force)
    results[[src$id]] <- out$genes
    status[[src$id]] <- out$status
  }

  list(
    results = results,
    status = dplyr::bind_rows(status)
  )
}

# --- Offline demo data (used when GLB_TEST_MODE is set) -----------------------
demo_run_sources <- function(disease, source_ids) {
  ot <- as_gene_table(
    data.frame(
      gene_symbol = c("TP53", "EGFR", "KRAS", "ALK", "BRAF"),
      ensembl_id = paste0("ENSG", 1:5),
      source_score_raw = c(0.93, 0.88, 0.80, 0.72, 0.65),
      evidence_type = "overall_association",
      url = "https://platform.opentargets.org/",
      stringsAsFactors = FALSE
    ),
    "opentargets"
  )
  dg <- as_gene_table(
    data.frame(
      gene_symbol = c("EGFR", "BRAF", "ALK"),
      source_score_raw = c(42, 30, 18),
      n_evidence = c(42L, 30L, 18L),
      evidence_type = "drug_interaction",
      url = "https://dgidb.org/",
      stringsAsFactors = FALSE
    ),
    "dgidb"
  )
  gn <- as_gene_table(
    data.frame(
      gene_symbol = c("TP53", "EGFR", "KRAS", "ALK", "BRAF"),
      source_score_raw = c(0.05, 1.10, 0.30, 0.20, 0.90),
      evidence_type = "gnomad_constraint",
      url = "https://gnomad.broadinstitute.org/",
      stringsAsFactors = FALSE
    ),
    "gnomad"
  )
  results <- list(opentargets = ot, dgidb = dg, gnomad = gn)
  results <- results[intersect(source_ids, names(results))]
  status <- dplyr::bind_rows(lapply(names(results), function(id) {
    tibble::tibble(
      source_id = id,
      label = id,
      status = "ok",
      ok = TRUE,
      n_genes = nrow(results[[id]]),
      latency_ms = 0L,
      message = "demo"
    )
  }))
  list(results = results, status = status)
}

# --- Register sources ---------------------------------------------------------
# Disease-driven evidence sources (add candidate genes):
register_source(
  id = "opentargets",
  label = "Open Targets",
  fetch_fn = fetch_opentargets,
  needs = "disease",
  role = "evidence",
  norm_method = "passthrough"
)

register_source(
  id = "panelapp",
  label = "PanelApp",
  fetch_fn = fetch_panelapp,
  needs = "disease",
  role = "evidence",
  norm_method = "passthrough"
)

register_source(
  id = "diseases",
  label = "DISEASES (Jensen Lab)",
  fetch_fn = fetch_diseases,
  needs = "disease",
  role = "evidence",
  norm_method = "rank"
)

register_source(
  id = "clinvar",
  label = "ClinVar",
  fetch_fn = fetch_clinvar,
  needs = "disease",
  role = "evidence",
  norm_method = "rank"
)

# Gene-driven evidence source (annotates candidate genes, counts as a source):
register_source(
  id = "dgidb",
  label = "DGIdb (drug-gene)",
  fetch_fn = fetch_dgidb,
  needs = "genes",
  role = "evidence",
  norm_method = "rank"
)

# Gene-driven annotation sources (prioritizers; do not inflate coverage):
register_source(
  id = "gnomad",
  label = "gnomAD constraint",
  fetch_fn = fetch_gnomad,
  needs = "genes",
  role = "annotation",
  norm_method = "rank_desc"
)

register_source(
  id = "pharos",
  label = "Pharos (druggability)",
  fetch_fn = fetch_pharos,
  needs = "genes",
  role = "annotation",
  norm_method = "passthrough"
)
