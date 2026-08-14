# Orchestration: the top-level gene-list prioritizer.
#
# Deterministic, no LLM (the subjective-ranking agent is a later stage). Split in
# two so the UI can re-rank on a weight change without re-querying:
#   run_enrich(): flatten -> resolve to canonical ids (dedupe) -> enrich
#     (per-source signals) -> citation gate -> assemble the gene x signal matrix.
#   rank_result(): compute composite (with optional weight override) -> rank ->
#     grade. Pure and cheap.
# run_review() runs both for the CLI / evals / tests. The free-text `description`
# (what the user is studying) is carried through for the later agent stage.
#
# Return shape:
#   list(
#     description        = <free text, for the agent stage>,
#     genes              = <ranked wide matrix: rank, symbol, per-signal raw+_n,
#                           composite, grade, resolved, input_lists, ...>,
#     signals            = <tidy long signals>,
#     evidence           = <gated evidence for drill-down>,
#     rejected           = <ungrounded evidence dropped by the gate>,
#     registry           = <signal registry summary for the UI legend>,
#     provenance         = <sources queried>,
#     generated_with_llm = FALSE
#   )

# The network half: flatten -> resolve -> enrich -> citation gate -> assemble the
# (unranked) gene x signal matrix. Runs once per "Rank genes" click. `context`
# carries run-wide state (e.g. a resolved disease) for disease-aware extractors.
# The returned list keeps the full registry object (`registry_obj`) so the cheap
# ranking half can be recomputed with new weights without re-querying.
run_enrich <- function(
  gene_lists,
  description = "",
  config = NULL,
  registry = genescout_signal_registry(),
  resolver = resolve_symbol,
  context = list(),
  seeder = seed_disease_genes,
  fetch_network = string_network,
  enabled = NULL,
  progress = NULL,
  max_genes = Inf
) {
  cs <- as_candidate_set(gene_lists)
  # Optional disease-context priors (context/*.yaml: FLAGS genes, tissues, drivers)
  # for the caveats stage. Accept an already-loaded `priors` list or a `priors_id`
  # to load; a bad id degrades to no priors rather than crashing the run.
  # Use `[[` (exact), NOT `$`: `context$priors` PARTIAL-MATCHES `priors_id`, so a
  # request that carries only priors_id would look like it already had priors and
  # skip the load entirely.
  if (is.null(context[["priors"]]) && !is_blank(context[["priors_id"]])) {
    context[["priors"]] <- tryCatch(
      load_context(context[["priors_id"]]),
      error = function(e) NULL
    )
  }
  # Discovery: with a disease context, seed candidate genes from the disease-keyed
  # sources (union with the user's set) and stash their per-gene tables for the
  # disease-mode extractors. The seeded genes go in as their own genescout_source
  # marked `seeded = TRUE` (id prefixed `seed:`), so a later stage can tell the
  # engine's own universe from the user's real sources. `seeder` is injectable.
  disease <- pluck_at(context, "disease")
  if (!is.null(disease)) {
    seeded <- seeder(disease)
    context$seed_data <- seeded$data
    if (length(seeded$symbols) > 0) {
      label <- paste0("disease: ", disease$name %||% disease$id)
      cs <- candidate_set_add(
        cs,
        genescout_source(
          seeded$symbols,
          label = label,
          id = paste0("seed:disease:", disease$id %||% slugify(label)),
          type = "disease_seed",
          seeded = TRUE
        )
      )
    }
    # Record when the seeded universe was capped, so the truncation is audited in
    # the provenance rather than being a silent limit.
    n_seeded <- seeded$n_seeded %||% length(seeded$symbols)
    if (n_seeded > length(seeded$symbols)) {
      context$seed_capped <- list(
        kept = length(seeded$symbols),
        total = n_seeded
      )
    }
  }
  # Cross-source corroboration: the labels of the user's OWN (non-seeded) sources.
  # When the user gave >= 2, append the cross-source signal so a gene appearing in
  # more of their sources ranks higher. Captured from the `seeded` flag, so the
  # disease-discovery universe is never counted as user corroboration.
  user_sources <- vapply(
    Filter(function(s) !isTRUE(s$seeded), unclass(cs)),
    function(s) s$label,
    character(1)
  )
  context$user_sources <- user_sources
  flat <- flatten_candidate_set(cs)
  if (nrow(flat) == 0) {
    stop("No genes to review.", call. = FALSE)
  }
  # Input guardrail: a very large candidate list fans out to (#genes x #sources)
  # serial calls and would appear to hang. Cap the enriched set to `max_genes`
  # (Inf = unlimited, the CLI/eval default) and audit the truncation so the
  # provenance and a UI notice can report it, mirroring the seed cap above.
  if (is.finite(max_genes) && nrow(flat) > max_genes) {
    context$input_capped <- list(
      kept = as.integer(max_genes),
      total = nrow(flat)
    )
    flat <- flat[seq_len(max_genes), , drop = FALSE]
  }
  # Source selection: which connectors this run may query. Precedence
  # (source_selection): explicit `enabled` > config `sources:` > each source's
  # default_on. Filtering the registry BEFORE enrichment gates at QUERY time - a
  # deselected source is never called (a weight of 0 would still pay its network
  # cost). A key-gated source whose key is absent is dropped by signal_available().
  sel <- source_selection(enabled, config)
  registry <- Filter(
    function(s) {
      source_is_active(s$key, sel, s$default_on %||% TRUE) &&
        signal_available(s)
    },
    registry
  )
  # An EXPLICIT selection (sel not NULL) that keeps no gene source is an empty
  # review - error now, BEFORE the derived auto-signals are appended, so a UI
  # "deselect all" (which arrives as character(0)) queries nothing as the picker
  # promises. A default run (sel = NULL) never trips this and is unchanged.
  if (!is.null(sel) && length(registry) == 0) {
    stop("No data sources selected for this review.", call. = FALSE)
  }
  registry_keys <- vapply(registry, function(s) s$key, character(1))
  has_gene_source <- length(registry) > 0
  # The runtime-data auto-signals (cross_source, STRING) are appended fresh from run
  # data. They are NOT offered in the source picker (needs != "gene"), so a positive
  # gene-only selection can never list them - gating them on selection membership
  # would silently drop them on every UI run (diverging from the CLI/eval default).
  # Instead they run on their data-condition whenever the run has an active gene
  # source (sel = NULL default, or an explicit selection that kept >= 1 source), so
  # a UI run stays byte-for-byte identical to the default (non-negotiables #3, #5).
  # gtex_tissue IS in the picker (needs = "gene"), so it stays selection-gated below.
  auto_active <- function(key) {
    is.null(sel) || key %in% sel || has_gene_source
  }
  if (
    length(user_sources) >= 2 &&
      auto_active("cross_source") &&
      !("cross_source" %in% registry_keys)
  ) {
    registry <- c(registry, list(cross_source_signal()))
  }
  # Tissue-expression: append the GTEx signal only when the study named tissue(s)
  # of interest, so runs without a tissue context are byte-for-byte unchanged.
  # Unlike the auto-signals above, GTEx is selectable in the picker, so it stays
  # gated on the selection (unchecking it suppresses it even with tissues set).
  if (
    length(context_tissues(context)) > 0 &&
      source_is_active("gtex_tissue", sel) &&
      !("gtex_tissue" %in% registry_keys)
  ) {
    registry <- c(registry, list(gtex_tissue_signal()))
  }
  # STRING within-list connectivity: append only for a multi-gene list (a network
  # needs several nodes to be informative), so a tiny list makes no STRING call and
  # every small offline test is byte-for-byte unchanged. Recorded in context so the
  # provenance can report the source.
  if (
    nrow(flat) >= GENESCOUT_STRING_MIN_GENES &&
      auto_active("string") &&
      !("string" %in% registry_keys)
  ) {
    registry <- c(registry, list(string_signal()))
    context$network_signal <- TRUE
  }
  if (length(registry) == 0) {
    stop("No data sources selected for this review.", call. = FALSE)
  }
  # Record the active source set (post-filter, post-append) for the audit.
  context$active_sources <- vapply(registry, function(s) s$key, character(1))

  resolved <- resolve_genes(flat, resolver = resolver)
  # Per-gene evidence retrieval. enrich_genes_dispatch() runs the serial enrich_genes()
  # for a small list (or when the parallel path is unavailable) and fans the per-gene
  # loop across a bounded mirai worker pool for a large one; either way it returns the
  # same signals_long / evidence_long tables, so the rest of the pipeline is unchanged.
  enriched <- enrich_genes_dispatch(
    resolved,
    registry,
    context = context,
    progress = progress
  )
  # Fill the input-derived (cross_source) and network (STRING) signals - each sees
  # more than one gene at a time, so they run outside the per-gene enrich_genes loop
  # - and fold their tidy rows + grounded evidence into the same tables.
  input_enr <- enrich_input_signals(resolved, registry, context = context)
  network_enr <- enrich_network_signals(
    resolved,
    registry,
    context = context,
    fetch_network = fetch_network
  )
  # Record a truncated STRING query so the dropped genes are an audited limit
  # (surfaced by genescout_provenance below), mirroring the discovery seed cap.
  if (!is.null(network_enr$capped)) {
    context$string_capped <- network_enr$capped
  }
  signals_long <- dplyr::bind_rows(
    enriched$signals_long,
    input_enr$signals_long,
    network_enr$signals_long
  )
  evidence_long <- dplyr::bind_rows(
    enriched$evidence_long,
    input_enr$evidence_long,
    network_enr$evidence_long
  )
  gated <- validate_evidence(evidence_long)
  genes <- assemble_matrix(signals_long, resolved, registry)

  list(
    description = description %||% "",
    genes = genes,
    signals = signals_long,
    evidence = gated$kept,
    rejected = gated$rejected,
    registry = registry_summary(registry),
    registry_obj = registry,
    provenance = genescout_provenance(context),
    context = context
  )
}

# The ranking half (pure, no network): score -> rank -> grade. `weights` (a named
# key -> weight vector) overrides the registry weights, so the UI weight sliders
# re-rank instantly on the cached `enriched` result. Drops the internal
# `registry_obj` from the public result.
rank_result <- function(
  enriched,
  weights = NULL,
  coverage_bonus = FALSE,
  caveats = TRUE,
  generated_with_llm = FALSE
) {
  registry <- enriched$registry_obj
  genes <- compute_composite(
    enriched$genes,
    registry,
    weights = weights,
    coverage_bonus = coverage_bonus
  )
  # Caveats/veto: deterministic anti-bias override. Runs on the cheap ranking half
  # so a slider change re-applies it with no re-query; reads context$priors when a
  # disease context supplied them.
  genes <- apply_caveats(
    genes,
    registry,
    context = enriched$context %||% list(),
    enabled = isTRUE(caveats)
  )
  genes <- rank_genes(genes)
  genes$grade <- grade_for_score(genes$composite)
  if ("vetoed" %in% names(genes)) {
    vetoed <- as.logical(genes$vetoed)
    vetoed[is.na(vetoed)] <- FALSE
    genes$grade[vetoed] <- "Vetoed"
  }

  out <- enriched
  out$genes <- genes
  out$registry_obj <- NULL
  out$generated_with_llm <- generated_with_llm
  out
}

# One-shot pipeline (enrich + rank) for the CLI, evals, and tests. The Shiny app
# calls run_enrich() once and rank_result() reactively so sliders re-rank without
# re-querying.
run_review <- function(
  gene_lists,
  description = "",
  config = NULL,
  registry = genescout_signal_registry(),
  resolver = resolve_symbol,
  coverage_bonus = rubric_coverage_bonus(),
  caveats = TRUE,
  context = list(),
  fetch_network = string_network,
  enabled = NULL
) {
  enriched <- run_enrich(
    gene_lists,
    description = description,
    config = config,
    registry = registry,
    resolver = resolver,
    context = context,
    fetch_network = fetch_network,
    enabled = enabled
  )
  rank_result(
    enriched,
    weights = NULL,
    coverage_bonus = coverage_bonus,
    caveats = caveats
  )
}

# Coerce whatever a request carries as `sources` into a candidate_set: a
# candidate_set passes through; a plain list-of-source-objects (the JSON form a
# non-R frontend posts, each element carrying `genes`) is rebuilt via
# candidate_set_from_list(); anything else (named list, vector, data frame) goes
# through as_candidate_set(). The JSON form must be detected FIRST - as_candidate_
# set() would otherwise read each source OBJECT as a gene vector.
coerce_request_sources <- function(sources) {
  if (inherits(sources, "genescout_candidate_set")) {
    return(sources)
  }
  looks_json <- is.list(sources) &&
    length(sources) > 0 &&
    is.list(sources[[1]]) &&
    !is.null(sources[[1]]$genes)
  if (looks_json) {
    return(candidate_set_from_list(sources))
  }
  as_candidate_set(sources)
}

# One review from a single, serializable request envelope - the ONE call a
# plumber route or the CLI wraps, so every frontend (Shiny, CLI, React/Python via
# the API) shares one contract. `req` is
#   list(sources = <candidate_set | JSON source list | named list | vector | df>,
#        description = <chr>, disease = <resolved list(id, name) | NULL>,
#        tissues = <chr>, priors_id = <context/<id>.yaml id | NULL>,
#        options = list(weights, coverage_bonus, caveats, sources))
# `options$sources` is the source SELECTION - a character vector of connector keys
# (from genescout_source_catalog); omitted -> the catalog's default_on subset. The
# disease is assumed ALREADY RESOLVED (the confirm step grounds it), so this stays
# deterministic. Returns the ranked run_review() result.
run_review_request <- function(
  req,
  config = NULL,
  registry = genescout_signal_registry(),
  resolver = resolve_symbol,
  fetch_network = string_network
) {
  cs <- coerce_request_sources(req$sources)
  context <- list()
  disease <- req$disease
  if (!is.null(disease) && !is_blank(disease$id %||% disease$name)) {
    context$disease <- disease
  }
  tissues <- as.character(req$tissues %||% character())
  tissues <- tissues[!is.na(tissues) & nzchar(trimws(tissues))]
  if (length(tissues) > 0) {
    context$tissues_of_interest <- tissues
  }
  # Study-context priors (context/<id>.yaml): run_enrich loads them from priors_id
  # and degrades to no priors on a bad id. Distinct from `disease` (ontology).
  if (!is_blank(pluck_at(req, "priors_id"))) {
    context$priors_id <- req$priors_id
  }
  opts <- req$options %||% list()
  enriched <- run_enrich(
    cs,
    description = req$description %||% "",
    config = config,
    registry = registry,
    resolver = resolver,
    context = context,
    fetch_network = fetch_network,
    enabled = opts$sources
  )
  rank_result(
    enriched,
    weights = opts$weights,
    coverage_bonus = isTRUE(opts$coverage_bonus),
    caveats = opts$caveats %||% TRUE
  )
}

# Coerce assorted inputs into the named list of character vectors that
# flatten_gene_lists() expects. The canonical model is now a `candidate_set`
# (R/parse_input.R); this is a thin back-compat shim over it. Dispatch runs
# through as_candidate_set(), which keys on the candidate_set / genescout_source S3
# classes FIRST - a candidate_set is itself a list, so the old bare
# `if (is.list(x)) return(x)` passthrough would have handed a list of
# genescout_source records straight to flatten and corrupted the run.
as_gene_lists <- function(x) {
  candidate_set_to_named_lists(as_candidate_set(x))
}

# Provenance for the sources the deterministic pipeline queries. `context` may
# carry a resolved disease (PR2 discovery mode), recorded here for the audit.
genescout_provenance <- function(context = list()) {
  # Only the sources this run actually queried. `active_sources` (set by run_enrich)
  # is the selected key set; when absent (older callers), every source is listed.
  active <- pluck_at(context, "active_sources")
  disease <- pluck_at(context, "disease")
  disease_on <- !is.null(disease) && !is_blank(disease$id %||% "")
  # In discovery mode the seeder ALWAYS queries Open Targets, PanelApp and DISEASES
  # to build the candidate universe, independent of each one's per-gene signal
  # selection - so the audit must count them as queried even when their per-gene
  # signal is deselected (otherwise provenance UNDER-reports what actually ran).
  seeded_sources <- if (disease_on) {
    c("ot_assoc", "panelapp", "diseases")
  } else {
    character()
  }
  queried <- union(active %||% character(), seeded_sources)
  is_on <- function(key) is.null(active) || key %in% queried
  base <- list(
    ot_assoc = list(
      source = "Open Targets Platform",
      endpoint = OPENTARGETS_URL
    ),
    pmc_hits = list(source = "Europe PMC", endpoint = EUROPEPMC_BASE),
    pubtator = list(source = "PubTator3", endpoint = PUBTATOR_BASE),
    clinvar_path = list(
      source = "ClinVar (NCBI E-utilities)",
      endpoint = CLINVAR_EUTILS_BASE
    ),
    dgidb = list(source = "DGIdb", endpoint = DGIDB_URL),
    gnomad_loeuf = list(source = "gnomAD", endpoint = GNOMAD_URL),
    pharos_tdl = list(source = "Pharos", endpoint = PHAROS_URL),
    reactome = list(source = "Reactome", endpoint = REACTOME_BASE),
    hpo = list(source = "Human Phenotype Ontology", endpoint = HPO_BASE),
    hpa = list(source = "Human Protein Atlas", endpoint = HPA_BASE),
    cbioportal = list(
      source = "cBioPortal (MSK-IMPACT)",
      endpoint = CBIOPORTAL_BASE
    ),
    civic = list(source = "CIViC", endpoint = CIVIC_GRAPHQL),
    clingen = list(
      source = "ClinGen gene-disease validity",
      endpoint = CLINGEN_GV_URL
    ),
    uniprot_disease = list(
      source = "UniProt (Swiss-Prot)",
      endpoint = UNIPROT_REST
    ),
    go = list(source = "QuickGO (EBI)", endpoint = QUICKGO_BASE),
    pdbe = list(source = "PDBe (EBI)", endpoint = PDBE_BASE),
    impc = list(source = "IMPC", endpoint = IMPC_SOLR)
  )
  # MyGene is always used (symbol resolution, not a rankable signal).
  sources <- c(
    list(list(source = "MyGene.info", endpoint = MYGENE_BASE)),
    unname(base[vapply(names(base), is_on, logical(1))])
  )
  if (disease_on) {
    if (is_on("panelapp")) {
      sources <- c(
        sources,
        list(list(
          source = "Genomics England PanelApp",
          endpoint = PANELAPP_BASE
        ))
      )
    }
    if (is_on("diseases")) {
      sources <- c(
        sources,
        list(list(
          source = "DISEASES (Jensen Lab)",
          endpoint = DISEASES_BASE
        ))
      )
    }
    sources <- c(
      sources,
      list(list(
        source = paste0("Disease context: ", disease$name %||% disease$id),
        endpoint = disease$id
      ))
    )
  }
  tissues <- context_tissues(context)
  # Gate on is_on("gtex_tissue") like every other selectable source: with tissues
  # set but GTEx deselected, the signal never appended and never queried, so it must
  # not appear in the audit (mirrors how STRING is gated on context$network_signal).
  if (length(tissues) > 0 && is_on("gtex_tissue")) {
    sources <- c(
      sources,
      list(list(
        source = paste0(
          "GTEx tissue expression: ",
          paste(tissues, collapse = ", ")
        ),
        endpoint = GTEX_BASE
      ))
    )
  }
  if (isTRUE(pluck_at(context, "network_signal"))) {
    sources <- c(
      sources,
      list(list(
        source = "STRING within-list interaction network",
        endpoint = STRING_BASE
      ))
    )
  }
  priors <- pluck_at(context, "priors")
  if (!is.null(priors) && !is_blank(priors$label %||% priors$id)) {
    sources <- c(
      sources,
      list(list(
        source = paste0("Context priors: ", priors$label %||% priors$id),
        endpoint = ""
      ))
    )
  }
  cap <- pluck_at(context, "seed_capped")
  if (!is.null(cap)) {
    sources <- c(
      sources,
      list(list(
        source = sprintf(
          "Discovery seeding: kept top %d of %d seeded candidate genes",
          cap$kept,
          cap$total
        ),
        endpoint = ""
      ))
    )
  }
  icap <- pluck_at(context, "input_capped")
  if (!is.null(icap)) {
    sources <- c(
      sources,
      list(list(
        source = sprintf(
          "Input cap: ranked the first %d of %d submitted candidate genes",
          icap$kept,
          icap$total
        ),
        endpoint = ""
      ))
    )
  }
  scap <- pluck_at(context, "string_capped")
  if (!is.null(scap)) {
    sources <- c(
      sources,
      list(list(
        source = sprintf(
          "STRING connectivity: queried %d of %d genes (list exceeds the %d-gene cap)",
          scap$kept,
          scap$total,
          STRING_MAX_NODES
        ),
        endpoint = ""
      ))
    )
  }
  sources
}

# TRUE when an LLM step can run: ellmer installed AND a credential exists for the
# configured provider - either a session-scoped BYOK key (`config$api_key`, set by
# the Review-tab key card via R/byok.R) or the environment.
genescout_llm_available <- function(config = load_config()) {
  requireNamespace("ellmer", quietly = TRUE) &&
    provider_credentials_ready(config$provider %||% "anthropic", config$api_key)
}
