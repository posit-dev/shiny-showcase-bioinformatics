# Gene-list enrichment: pull a ranking signal from each source for every gene,
# merge and dedupe by canonical gene id, and shape the tidy + wide tables the
# ranking and UI consume. Deterministic, no LLM. In discovery mode a resolved
# disease context (context$disease) seeds extra candidate genes and makes the
# OT / ClinVar / PMC signals disease-specific; in plain enrichment the pull is
# gene-only. The free-text description is reserved for the later agent stage.
#
# A SIGNAL is one source's contribution, defined once in the registry:
#   genescout_signal(key, label, source, extractor, normalize, weight, direction,
#                 role, needs)
# `role` is "evidence" (counts toward breadth) or "annotation" (nudges but does
# not gate); `needs` is "gene" (per-gene extractor), "disease" (a seeder, run
# before this step), "input" (derived from the input structure, no network -
# cross_source, filled by enrich_input_signals), or "network" (one call sees the
# WHOLE resolved set - STRING, filled by enrich_network_signals). Adding a source =
# one genescout_signal() here plus its R/tools/ client. Each per-gene extractor
# returns a raw number + grounded evidence rows.

# --- Signal registry --------------------------------------------------------

genescout_signal <- function(
  key,
  label,
  source,
  extractor,
  normalize,
  weight = 1,
  direction = "higher_better",
  role = "evidence",
  needs = "gene",
  seed_key = NULL,
  domain = NULL,
  default_on = TRUE,
  auth = NULL,
  key_env = NULL,
  stub = FALSE
) {
  list(
    key = key,
    label = label,
    source = source,
    extractor = extractor,
    normalize = normalize,
    weight = weight,
    direction = direction,
    role = role,
    needs = needs,
    # When set, this gene-keyed signal can be satisfied OFFLINE from the named
    # context$seed_data table (a disease seeder already fetched it). Such signals
    # still run for an unresolved gene, so a seeded gene MyGene could not resolve
    # keeps its already-in-hand grounded evidence instead of being blanked.
    seed_key = seed_key,
    # Catalog metadata (see genescout_source_catalog / resolve_active_sources). All
    # optional and back-compatible: a signal with defaults behaves exactly as before.
    #   domain     - grouping label for the UI picker / report (e.g. "cancer").
    #   default_on - TRUE if this source runs when the caller selects nothing.
    #   auth       - NULL (keyless), "query" (NCBI-style key), or "header"/"bearer".
    #   key_env    - Sys.getenv name of the API key for a key-gated source.
    domain = domain,
    default_on = default_on,
    auth = auth,
    key_env = key_env,
    # A stub is catalog/introspection-only: it advertises a source GeneScout knows
    # about but has no live client for yet, so it is never a runnable/selectable
    # source (the picker lists it separately, never as a working checkbox).
    stub = stub
  )
}

# TRUE if a signal can actually run: keyless signals are always available; a
# key-gated one is available only when its key is present in the environment (so a
# keyless deploy silently skips it instead of erroring - mirrors genescout_llm_available).
signal_available <- function(sig) {
  if (is.null(sig$auth) || is.null(sig$key_env)) {
    return(TRUE)
  }
  nzchar(Sys.getenv(sig$key_env))
}

# The (redacted, non-cached) auth headers for a key-gated source, built from its
# key_env - or NULL when the source is keyless or its key is absent. Never logs the
# key; http_get_json/http_post_json accept these via their `headers` argument. The
# exact header shape per source is finalized when that source's client is wired.
source_auth_headers <- function(sig) {
  if (is.null(sig$auth) || is.null(sig$key_env)) {
    return(NULL)
  }
  key <- Sys.getenv(sig$key_env)
  if (!nzchar(key)) {
    return(NULL)
  }
  switch(
    sig$auth,
    bearer = list(Authorization = paste("Bearer", key)),
    header = list(Authorization = key),
    NULL # "query" keys are injected into the query by the client (NCBI pattern)
  )
}

# The cross-source corroboration signal: a gene's value is how many of the USER'S
# OWN input sources it appears in - a breadth measure derived from the input
# structure, not a network call. Role = evidence, so breadth is rewarded through
# the weighted-mean denominator and can out-rank a single loud external source;
# but the saturating normalizer caps a breadth-only gene below the High grade, so
# a top grade still requires some external evidence (the "Balanced" calibration).
# needs = "input": enrich_genes() runs only needs = "gene" extractors and skips
# this; enrich_input_signals() fills it from resolved$input_lists.
cross_source_signal <- function(rubric = NULL) {
  # Tolerate a missing rubric (fall back to the same defaults) so run_enrich's
  # internal call never hard-fails when rubric.yml is off the current path.
  rubric <- rubric %||% tryCatch(load_rubric(), error = function(e) list())
  genescout_signal(
    "cross_source",
    "Cross-source corroboration",
    "Your input sources",
    extractor = NULL,
    normalize = normalize_corroboration(rubric$midpoints$cross_source %||% 1),
    weight = rubric$weights$cross_source %||% 2,
    role = "evidence",
    needs = "input",
    domain = "input-provenance"
  )
}

# The GTEx tissue-expression signal (annotation). Appended by run_enrich only
# when the study supplies tissue(s) of interest, so runs without a tissue context
# are unchanged. Rubric-tolerant defaults so it never hard-fails off-path.
gtex_tissue_signal <- function(rubric = NULL) {
  rubric <- rubric %||% tryCatch(load_rubric(), error = function(e) list())
  genescout_signal(
    "gtex_tissue",
    "GTEx tissue expression",
    "GTEx",
    extractor = extract_gtex_tissue,
    normalize = normalize_identity,
    weight = rubric$weights$gtex_tissue %||% 0.5,
    role = "annotation",
    needs = "gene",
    domain = "expression"
  )
}

# A network needs several nodes before within-list connectivity means anything, so
# run_enrich appends the STRING signal only for lists of at least this many input
# tokens. Below it, connectivity is uninformative and no STRING call is made - and
# every small offline test stays byte-for-byte unchanged (no network column added).
GENESCOUT_STRING_MIN_GENES <- 5L

# The STRING within-list connectivity signal (annotation). needs = "network": one
# STRING call sees the WHOLE resolved set at once (enrich_network_signals), unlike
# per-gene extractors. Role = annotation because the signal is COHORT-RELATIVE (a
# gene's value depends on which other genes are in the list) - so it may only nudge
# a connected gene up, never penalize an isolated one and never gate. Appended by
# run_enrich only for a multi-gene list. Rubric-tolerant defaults, off-path safe.
string_signal <- function(rubric = NULL) {
  rubric <- rubric %||% tryCatch(load_rubric(), error = function(e) list())
  genescout_signal(
    "string",
    "STRING within-list connectivity",
    "STRING",
    extractor = NULL,
    normalize = normalize_saturating(rubric$midpoints$string %||% 3),
    weight = rubric$weights$string %||% 0.5,
    role = "annotation",
    needs = "network",
    domain = "interaction"
  )
}

# The active signal registry, wired from the rubric (weights + normalization
# midpoints). Registry order is the column order in the results table. When
# `disease_mode` is TRUE (a disease context is set), the disease-keyed PanelApp
# and DISEASES signals are appended; when `multi_source` is TRUE (the user gave
# >= 2 sources) the cross-source signal is appended. Both are omitted otherwise
# so their absence never penalizes a gene (single-source runs are unchanged).
genescout_signal_registry <- function(
  rubric = load_rubric(),
  disease_mode = FALSE,
  multi_source = FALSE
) {
  w <- rubric$weights %||% list()
  m <- rubric$midpoints %||% list()
  base <- list(
    genescout_signal(
      "ot_assoc",
      "Open Targets association",
      "Open Targets Platform",
      extractor = extract_ot_assoc,
      normalize = normalize_identity,
      weight = w$ot_assoc %||% 1,
      role = "evidence",
      domain = "gene-disease",
      # In discovery mode this reads the seeded associated-targets table offline;
      # in plain enrichment it queries live by gene id (handled by the extractor).
      seed_key = "ot_targets"
    ),
    genescout_signal(
      "pmc_hits",
      "Europe PMC mentions",
      "Europe PMC",
      extractor = extract_pmc_hits,
      normalize = normalize_log_saturating(m$pmc_hits %||% 50),
      weight = w$pmc_hits %||% 0.5,
      role = "evidence",
      domain = "literature"
    ),
    genescout_signal(
      "pubtator",
      "PubTator3 gene mentions",
      "PubTator3",
      extractor = extract_pubtator,
      normalize = normalize_log_saturating(m$pubtator %||% 300),
      weight = w$pubtator %||% 0.5,
      role = "evidence",
      domain = "literature"
    ),
    genescout_signal(
      "clinvar_path",
      "ClinVar pathogenic variants",
      "ClinVar",
      extractor = extract_clinvar_path,
      normalize = normalize_saturating(m$clinvar_path %||% 5),
      weight = w$clinvar_path %||% 1,
      role = "evidence",
      domain = "variant-effect"
    ),
    genescout_signal(
      "dgidb",
      "DGIdb drug interactions",
      "DGIdb",
      extractor = extract_dgidb,
      normalize = normalize_log_saturating(m$dgidb %||% 5),
      weight = w$dgidb %||% 0.5,
      role = "evidence",
      domain = "druggability"
    ),
    genescout_signal(
      "gnomad_loeuf",
      "gnomAD LOEUF constraint",
      "gnomAD",
      extractor = extract_gnomad_loeuf,
      normalize = normalize_saturating_desc(m$gnomad_loeuf %||% 0.35),
      weight = w$gnomad_loeuf %||% 0.75,
      direction = "lower_better",
      role = "annotation",
      domain = "constraint"
    ),
    # Opt-in (default_on = FALSE): scans a gene's whole variant set, so it runs only
    # when the user selects it. Weight 0 + role annotation keeps it OUT of the
    # composite entirely (excluded from the evidence mean and from the up-nudge,
    # which needs weight > 0); it exists purely to drive the "common variant" caveat
    # and as an informative column. normalize_saturating_desc makes its _n rarity
    # (rare -> high), so if a user ever raises its slider it nudges in the correct
    # direction rather than rewarding a common gene.
    genescout_signal(
      "gnomad_af",
      "gnomAD common-LoF frequency",
      "gnomAD",
      extractor = extract_gnomad_af,
      normalize = normalize_saturating_desc(m$gnomad_af %||% 0.001),
      weight = w$gnomad_af %||% 0,
      direction = "lower_better",
      role = "annotation",
      domain = "population-frequency",
      default_on = FALSE
    ),
    genescout_signal(
      "pharos_tdl",
      "Pharos target dev. level",
      "Pharos",
      extractor = extract_pharos_tdl,
      normalize = normalize_identity,
      weight = w$pharos_tdl %||% 0.5,
      role = "annotation",
      domain = "druggability"
    ),
    genescout_signal(
      "reactome",
      "Reactome disease pathways",
      "Reactome",
      extractor = extract_reactome,
      normalize = normalize_saturating(m$reactome %||% 2),
      weight = w$reactome %||% 0.5,
      role = "annotation",
      domain = "pathway-disease"
    ),
    # Opt-in keyless connectors (default_on = FALSE): in the catalog + selectable,
    # but off unless a review selects them, so default runs stay lean + unchanged.
    genescout_signal(
      "hpo",
      "HPO gene-disease",
      "Human Phenotype Ontology",
      extractor = extract_hpo,
      normalize = normalize_saturating(m$hpo %||% 3),
      weight = w$hpo %||% 0.75,
      role = "evidence",
      domain = "gene-disease",
      default_on = FALSE
    ),
    genescout_signal(
      "hpa",
      "HPA disease/cancer class",
      "Human Protein Atlas",
      extractor = extract_hpa,
      normalize = normalize_saturating(m$hpa %||% 2),
      weight = w$hpa %||% 0.5,
      role = "annotation",
      domain = "cancer",
      default_on = FALSE
    ),
    genescout_signal(
      "cbioportal",
      "cBioPortal mutation frequency",
      "cBioPortal (MSK-IMPACT)",
      extractor = extract_cbioportal,
      normalize = normalize_saturating(m$cbioportal %||% 0.05),
      weight = w$cbioportal %||% 0.5,
      role = "evidence",
      domain = "cancer",
      default_on = FALSE
    ),
    genescout_signal(
      "civic",
      "CIViC clinical evidence",
      "CIViC",
      extractor = extract_civic,
      normalize = normalize_log_saturating(m$civic %||% 15),
      weight = w$civic %||% 0.5,
      role = "evidence",
      domain = "cancer",
      default_on = FALSE
    ),
    genescout_signal(
      "clingen",
      "ClinGen gene-disease validity",
      "ClinGen",
      extractor = extract_clingen,
      normalize = normalize_saturating(m$clingen %||% 2),
      weight = w$clingen %||% 0.75,
      role = "evidence",
      domain = "gene-disease",
      default_on = FALSE
    ),
    genescout_signal(
      "uniprot_disease",
      "UniProt disease involvement",
      "UniProt (Swiss-Prot)",
      extractor = extract_uniprot_disease,
      normalize = normalize_saturating(m$uniprot_disease %||% 2),
      weight = w$uniprot_disease %||% 0.75,
      role = "evidence",
      domain = "gene-disease",
      default_on = FALSE
    ),
    genescout_signal(
      "go",
      "Gene Ontology function",
      "QuickGO (EBI)",
      extractor = extract_go,
      normalize = normalize_saturating(m$go %||% 3),
      weight = w$go %||% 0.5,
      role = "annotation",
      domain = "function",
      default_on = FALSE
    ),
    genescout_signal(
      "pdbe",
      "PDBe 3D structures",
      "PDBe (EBI)",
      extractor = extract_pdbe,
      normalize = normalize_saturating(m$pdbe %||% 2),
      weight = w$pdbe %||% 0.4,
      role = "annotation",
      domain = "structure",
      default_on = FALSE
    ),
    genescout_signal(
      "impc",
      "IMPC mouse knockout phenotype",
      "IMPC",
      extractor = extract_impc,
      normalize = normalize_log_saturating(m$impc %||% 4),
      weight = w$impc %||% 0.4,
      role = "annotation",
      domain = "model-organism",
      default_on = FALSE
    )
  )
  if (isTRUE(disease_mode)) {
    base <- c(
      base,
      list(
        genescout_signal(
          "panelapp",
          "PanelApp confidence",
          "Genomics England PanelApp",
          extractor = extract_panelapp,
          normalize = normalize_identity,
          weight = w$panelapp %||% 1,
          role = "evidence",
          domain = "gene-disease",
          seed_key = "panelapp"
        ),
        genescout_signal(
          "diseases",
          "DISEASES (Jensen) score",
          "DISEASES (Jensen Lab)",
          extractor = extract_diseases,
          normalize = normalize_saturating(m$diseases %||% 2),
          weight = w$diseases %||% 0.75,
          role = "evidence",
          domain = "gene-disease",
          seed_key = "diseases"
        )
      )
    )
  }
  if (isTRUE(multi_source)) {
    base <- c(base, list(cross_source_signal(rubric)))
  }
  base
}

# --- Source catalog + selection --------------------------------------------
# The catalog is the full set of connectors GeneScout knows about; a run activates a
# SELECTED subset. Selection gates which extractors are QUERIED (unlike a weight of
# 0, which mutes ranking but still pays the network cost). This is the extensibility
# surface a non-R front end introspects (genescout_source_catalog) and chooses from.

# Key-gated source STUBS: sources GeneScout knows about but whose live clients need an
# API key / license (deferred). They appear in the catalog (so a front end can show
# "needs a key") but are default_on = FALSE and unavailable without their key, so a
# keyless deploy never selects or queries them. Their extractor is a safe miss, so
# even an explicit selection with the key present cannot crash (no client yet). They
# live ONLY in the catalog (introspection), never in the run registry.
genescout_source_stubs <- function() {
  stub <- function(key, label, source, domain, auth, key_env) {
    genescout_signal(
      key,
      label,
      source,
      extractor = function(resolved, context = list()) signal_miss(),
      normalize = normalize_identity,
      role = "evidence",
      domain = domain,
      default_on = FALSE,
      auth = auth,
      key_env = key_env,
      stub = TRUE
    )
  }
  list(
    stub(
      "oncokb",
      "OncoKB oncogenicity",
      "OncoKB",
      "cancer",
      "bearer",
      "ONCOKB_API_KEY"
    ),
    stub(
      "cosmic_cgc",
      "COSMIC Cancer Gene Census",
      "COSMIC",
      "cancer",
      "bearer",
      "COSMIC_API_KEY"
    ),
    stub(
      "disgenet",
      "DisGeNET gene-disease",
      "DisGeNET",
      "gene-disease",
      "bearer",
      "DISGENET_API_KEY"
    ),
    stub(
      "omim",
      "OMIM Mendelian gene-disease",
      "OMIM",
      "gene-disease",
      "query",
      "OMIM_API_KEY"
    ),
    stub(
      "drugbank",
      "DrugBank drug targets",
      "DrugBank",
      "druggability",
      "header",
      "DRUGBANK_API_KEY"
    )
  )
}

# The full ordered source catalog: every registry signal (base + disease-keyed +
# cross-source) plus the tissue (GTEx) and network (STRING) signals, plus the
# key-gated stubs. New connectors are added to genescout_signal_registry() as
# default_on = FALSE (opt-in) and flow in here automatically. This is the
# introspectable universe of selectable sources (a front end renders a picker from it).
genescout_source_catalog <- function(rubric = load_rubric()) {
  c(
    genescout_signal_registry(rubric, disease_mode = TRUE, multi_source = TRUE),
    list(
      gtex_tissue_signal(rubric),
      string_signal(rubric)
    ),
    genescout_source_stubs()
  )
}

# The catalog as plain, serializable data for introspection - the /catalog API and
# the UI source picker. One record per source with its selection metadata + whether
# it is currently `available` (a key-gated source needs its key). No closures, so it
# is safe to serialize to JSON for a non-R front end to render a grouped picker.
genescout_catalog_json <- function(catalog = genescout_source_catalog()) {
  lapply(catalog, function(s) {
    list(
      key = s$key,
      label = s$label,
      source = s$source,
      domain = s$domain %||% "other",
      role = s$role %||% "evidence",
      default_on = isTRUE(s$default_on %||% TRUE),
      auth = s$auth %||% "none",
      available = signal_available(s),
      # TRUE for a catalog-only stub (no live client) so a front end renders it as
      # "planned / needs a key", not as a working, selectable source.
      stub = isTRUE(s$stub)
    )
  })
}

# The effective source selection, NULL meaning "use each source's default_on".
# Precedence: an explicit `selection` (envelope / CLI / UI) > a deploy default
# (config `sources:`) > NULL. Returned as a character vector of keys, or NULL.
source_selection <- function(selection = NULL, config = NULL) {
  if (!is.null(selection)) {
    return(as.character(selection))
  }
  cs <- if (!is.null(config)) config$sources else NULL
  if (!is.null(cs)) as.character(cs) else NULL
}

# Whether a source key is active for a run, given the effective selection `sel`
# (NULL -> fall back to the source's default_on) and its default_on flag.
source_is_active <- function(key, sel, default_on = TRUE) {
  if (is.null(sel)) isTRUE(default_on) else key %in% sel
}

# The concrete active key set over a catalog (for provenance and the /catalog API):
# selected (or default_on) AND available (a key-gated source with no key is dropped,
# never errors). Order follows the catalog.
resolve_active_sources <- function(catalog, selection = NULL, config = NULL) {
  sel <- source_selection(selection, config)
  keep <- vapply(
    catalog,
    function(s) {
      source_is_active(s$key, sel, s$default_on %||% TRUE) &&
        signal_available(s)
    },
    logical(1)
  )
  vapply(catalog[keep], function(s) s$key, character(1), USE.NAMES = FALSE)
}

# A compact registry description for the UI legend / result object.
registry_summary <- function(registry) {
  tibble::tibble(
    key = vapply(registry, function(s) s$key, character(1)),
    label = vapply(registry, function(s) s$label, character(1)),
    source = vapply(registry, function(s) s$source, character(1)),
    weight = vapply(registry, function(s) s$weight, numeric(1)),
    role = vapply(registry, function(s) s$role %||% "evidence", character(1)),
    direction = vapply(
      registry,
      function(s) s$direction %||% "higher_better",
      character(1)
    )
  )
}

# --- Extractors -------------------------------------------------------------
# extractor(resolved, context) -> list(ok, raw, source_id, source_url, evidence)
# `resolved` is a list with $gene_id (canonical Ensembl id), $symbol, $entrez.
# `context` is a list carrying run-wide state; `context$disease` (a resolved
# disease, or NULL) lets a gene-keyed extractor run a disease-aware query. All
# PR1 extractors are gene-only and ignore `context`; PR2 wires the disease path.

# No signal for this gene from this source (absent or lookup failed).
signal_miss <- function() {
  list(
    ok = FALSE,
    raw = NA_real_,
    source_id = "",
    source_url = "",
    evidence = empty_evidence_long()
  )
}

# A one-row lookup in a stashed seed_data table (symbol match, case-insensitive).
# `symbols` may be several aliases for one gene: the seed table is keyed by the
# SOURCE's symbol, which MyGene may canonicalize to a different HGNC symbol, so we
# match on the canonical symbol OR any original input symbol. Returns the 1-row
# tibble or NULL. Used by the disease-mode extractors so they read the
# once-fetched disease tables instead of re-querying per gene.
seed_row <- function(context, key, symbols) {
  tbl <- pluck_at(context, "seed_data", key)
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(NULL)
  }
  want <- toupper(symbols[!is.na(symbols) & nzchar(symbols)])
  hit <- tbl[toupper(tbl$symbol) %in% want, , drop = FALSE]
  if (nrow(hit) == 0) {
    return(NULL)
  }
  hit[1, , drop = FALSE]
}

# The symbols to look a resolved gene up by in the seed tables: its canonical
# (MyGene) symbol plus every original input token it collapsed from, so a seed
# keyed under an alias still matches after canonicalization.
seed_lookup_symbols <- function(resolved) {
  unique(c(resolved$symbol, resolved$input_symbols))
}

# Open Targets association. In discovery mode (context$disease present) this is
# the gene's association to THAT disease, read from the seeded associated-targets
# table; otherwise the gene's max association across all diseases.
extract_ot_assoc <- function(resolved, context = list()) {
  disease <- pluck_at(context, "disease")
  if (!is.null(disease)) {
    row <- seed_row(context, "ot_targets", seed_lookup_symbols(resolved))
    if (is.null(row)) {
      return(signal_miss())
    }
    return(list(
      ok = TRUE,
      raw = row$raw,
      source_id = row$source_id,
      source_url = row$source_url,
      evidence = evidence_long_rows(
        resolved$gene_id,
        "ot_assoc",
        domain = "pathway-disease",
        title = sprintf("%s association", disease$name %||% disease$id),
        detail = sprintf("association score %.2f", row$raw),
        score = row$raw,
        source_id = row$source_id,
        source_url = row$source_url
      )
    ))
  }
  a <- gene_disease_assoc(resolved$gene_id)
  if (!isTRUE(a$ok) || nrow(a$evidence) == 0) {
    return(signal_miss())
  }
  ev <- a$evidence
  top <- ev[which.max(ev$score), ]
  list(
    ok = TRUE,
    raw = max(ev$score, na.rm = TRUE),
    source_id = top$source_id,
    source_url = top$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "ot_assoc",
      domain = "pathway-disease",
      title = ev$disease,
      detail = sprintf("association score %.2f", ev$score),
      score = ev$score,
      source_id = ev$source_id,
      source_url = ev$source_url
    )
  )
}

# How many top articles the pmc_hits signal surfaces as citable PMID evidence, on
# top of the query-count summary row - enough real papers to ground a curation /
# specialist rationale without bloating a prompt.
GENESCOUT_PMC_EVIDENCE_MAX <- 5L

# Europe PMC gene-mention count. In discovery mode, the count of papers
# co-mentioning the gene AND the disease (a context-specific literature signal);
# otherwise the gene's total mention count. The SCORE is the hit count
# (europepmc_count, which keeps a genuine 0 as 0); when there IS literature, we
# additionally pull the top few articles (europepmc_search) so the drill-down, the
# AI curator, and the literature specialist can cite real PMIDs rather than only the
# search-query link. The paper pull is best-effort: a failed/empty search just leaves
# the count row, and the score is unchanged either way.
extract_pmc_hits <- function(resolved, context = list()) {
  disease <- pluck_at(context, "disease")
  dname <- if (!is.null(disease)) disease$name %||% disease$id else NULL
  q <- if (!is.null(dname)) {
    sprintf('"%s" AND "%s"', resolved$symbol, dname)
  } else {
    sprintf('"%s"', resolved$symbol)
  }
  r <- europepmc_count(q)
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  title <- if (!is.null(dname)) {
    sprintf(
      "%d Europe PMC co-mentions of %s and %s",
      r$count,
      resolved$symbol,
      dname
    )
  } else {
    sprintf("%d Europe PMC mentions of %s", r$count, resolved$symbol)
  }
  ev <- evidence_long_rows(
    resolved$gene_id,
    "pmc_hits",
    domain = "literature",
    title = title,
    detail = sprintf("query %s", q),
    score = NA_real_,
    source_id = r$source_id,
    source_url = r$source_url
  )
  if (isTRUE(r$count > 0)) {
    papers <- tryCatch(
      europepmc_search(q, limit = GENESCOUT_PMC_EVIDENCE_MAX),
      error = function(e) NULL
    )
    if (!is.null(papers) && isTRUE(papers$ok)) {
      ev <- dplyr::bind_rows(
        ev,
        pmc_paper_evidence(resolved$gene_id, papers$data)
      )
    }
  }
  list(
    ok = TRUE,
    raw = r$count,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = ev
  )
}

# Map Europe PMC search results (europepmc_search()$data - the parsed citation
# tibble) to citable literature evidence rows, one per article, each grounded on its
# PMID (or Europe PMC source id) so a rationale can reference the actual paper. Pure
# and fixture-testable; `papers` is truncated to `max_rows`.
pmc_paper_evidence <- function(
  gene_id,
  papers,
  max_rows = GENESCOUT_PMC_EVIDENCE_MAX
) {
  if (is.null(papers) || nrow(papers) == 0) {
    return(empty_evidence_long())
  }
  papers <- utils::head(papers, max_rows)
  jr <- ifelse(is.na(papers$journal), "", as.character(papers$journal))
  yr <- ifelse(is.na(papers$year), "", as.character(papers$year))
  evidence_long_rows(
    gene_id,
    "pmc_hits",
    domain = "literature",
    title = papers$title,
    detail = trimws(paste(jr, yr)),
    score = NA_real_,
    source_id = papers$source_id,
    source_url = papers$source_url
  )
}

# PubTator3: count of PubMed/PMC articles in which the gene ENTITY is tagged - the
# entity-precise literature signal alongside the Europe PMC symbol count. Uses the
# resolved NCBI Gene id when available (`@GENE_<entrez>`) so aliases disambiguate
# exactly, else the symbol. Gene-level (not disease-scoped): the documented role is
# the precise successor to the gene-level Europe PMC count. A real 0 is a genuine
# zero (kept), only a failed fetch is a miss.
extract_pubtator <- function(resolved, context = list()) {
  r <- pubtator_gene_literature(resolved$symbol, entrez = resolved$entrez)
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = r$count,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "pubtator",
      domain = "literature",
      title = sprintf(
        "%d PubTator3-tagged articles for %s",
        r$count,
        resolved$symbol
      ),
      detail = sprintf(
        "entity %s (entity-tagged, not a string match)",
        r$entity
      ),
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# ClinVar pathogenic / likely-pathogenic variant count. In discovery mode the
# count is scoped to the disease context; otherwise it is the gene's total.
extract_clinvar_path <- function(resolved, context = list()) {
  disease <- pluck_at(context, "disease")
  dname <- if (!is.null(disease)) disease$name %||% disease$id else NULL
  r <- if (!is.null(dname)) {
    clinvar_gene_disease_pathogenic_count(resolved$symbol, dname)
  } else {
    clinvar_gene_pathogenic_count(resolved$symbol)
  }
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  title <- if (!is.null(dname)) {
    sprintf(
      "%d pathogenic / likely-pathogenic ClinVar variants for %s",
      r$count,
      dname
    )
  } else {
    sprintf("%d pathogenic / likely-pathogenic ClinVar variants", r$count)
  }
  list(
    ok = TRUE,
    raw = r$count,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "clinvar_path",
      domain = "variant-effect",
      title = title,
      detail = "germline classification",
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# DGIdb: number of curated drug-gene interactions (a druggability / actionability
# signal). A gene present in DGIdb with 0 interactions is a real 0; a gene absent
# from DGIdb is a miss.
extract_dgidb <- function(resolved, context = list()) {
  r <- dgidb_gene_interactions(resolved$symbol)
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = r$count,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "dgidb",
      domain = "druggability",
      title = sprintf("%d drug-gene interactions (DGIdb)", r$count),
      detail = "curated across DGIdb interaction sources",
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# gnomAD: LOEUF loss-of-function constraint (lower = more constrained = more
# likely essential). Annotation signal, normalized descending so low LOEUF -> high
# score. Absent constraint record is a miss.
extract_gnomad_loeuf <- function(resolved, context = list()) {
  r <- gnomad_loeuf(resolved$symbol)
  if (!isTRUE(r$ok) || is_blank(r$loeuf)) {
    return(signal_miss())
  }
  detail <- if (!is_blank(r$pli)) {
    sprintf("pLI %.2f", r$pli)
  } else {
    "loss-of-function constraint"
  }
  list(
    ok = TRUE,
    raw = r$loeuf,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "gnomad_loeuf",
      domain = "constraint",
      title = sprintf(
        "gnomAD LOEUF %.2f (lower = more LoF-constrained)",
        r$loeuf
      ),
      detail = detail,
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# gnomAD common loss-of-function frequency: the max allele frequency among the
# gene's pLoF variants. A COMMON pLoF variant means the general population tolerates
# losing the gene, so it is a weaker driver candidate - the caveats stage reads this
# raw value and down-weights a gene above the rubric threshold. Raw 0 (a gene in
# gnomAD with no common pLoF) is a REAL value, not a miss. Annotation, weight 0:
# purely a caveat driver + an informative column, never a scoring contribution.
extract_gnomad_af <- function(resolved, context = list()) {
  r <- gnomad_gene_max_lof_af(resolved$symbol)
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = r$max_lof_af,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "gnomad_af",
      domain = "population-frequency",
      title = sprintf(
        "gnomAD max pLoF allele frequency %.2g (%d pLoF variant%s)",
        r$max_lof_af,
        r$n_lof,
        if (r$n_lof == 1) "" else "s"
      ),
      detail = "higher = a more common loss-of-function variant in the population",
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# Pharos: target development level (Tclin/Tchem/Tbio/Tdark) mapped to a 0-1
# druggability/actionability score. Annotation signal.
extract_pharos_tdl <- function(resolved, context = list()) {
  r <- pharos_tdl(resolved$symbol)
  if (!isTRUE(r$ok) || is_blank(r$score)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = r$score,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "pharos_tdl",
      domain = "druggability",
      title = sprintf("Pharos target development level: %s", r$tdl),
      detail = sprintf("TDL druggability score %.2f", r$score),
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# Reactome: mechanistic pathway membership. Counts the gene's disease-associated
# Reactome pathways (isInDisease) plus any that match a context pathway prior
# (context$priors$pathways), with each relevant pathway as grounded evidence.
# Annotation (nudges up, never penalizes): a gene with no relevant pathway is a
# miss, not a demotion - so a gene in only generic pathways (e.g. TTN in muscle
# pathways) gets no spurious pathway boost.
extract_reactome <- function(resolved, context = list()) {
  r <- reactome_pathways(resolved$symbol)
  if (!isTRUE(r$ok) || nrow(r$pathways) == 0) {
    return(signal_miss())
  }
  hits <- reactome_relevant(
    r$pathways,
    pluck_at(context, "priors", "pathways")
  )
  if (nrow(hits) == 0) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = nrow(hits),
    source_id = hits$source_id[1],
    source_url = hits$source_url[1],
    evidence = evidence_long_rows(
      resolved$gene_id,
      "reactome",
      domain = "pathway-disease",
      title = hits$name,
      detail = ifelse(
        hits$in_disease,
        "Reactome disease-associated pathway",
        "Reactome pathway matching the study context"
      ),
      score = NA_real_,
      source_id = hits$source_id,
      source_url = hits$source_url
    )
  )
}

# The study's tissue(s) of interest: a UI/CLI-supplied list (context$
# tissues_of_interest) or, failing that, a disease-context prior
# (context$priors$tissues_of_interest). Empty when none was given.
context_tissues <- function(context = list()) {
  t <- context$tissues_of_interest %||%
    pluck_at(context, "priors", "tissues_of_interest")
  t <- as.character(t %||% character())
  t[!is.na(t) & nzchar(trimws(t))]
}

# GTEx: tissue-expression relevance. Rewards a gene expressed in the study's
# tissue(s) of interest (peak expression there vs the gene's peak across all
# tissues). Annotation (nudges up, never penalizes): a gene expressed only in
# unrelated tissues scores low but is not demoted here - that is the caveats
# stage's "unrelated-tissue-only" trigger. Only active when tissues of interest
# were supplied AND at least one maps to a GTEx tissue (else a cheap miss, no
# wasted network call).
extract_gtex_tissue <- function(resolved, context = list()) {
  terms <- context_tissues(context)
  if (length(terms) == 0) {
    return(signal_miss())
  }
  r <- gtex_tissue_expression(resolved$gene_id)
  if (!isTRUE(r$ok) || nrow(r$expression) == 0) {
    return(signal_miss())
  }
  rel <- gtex_relevance(r$expression, terms)
  if (!isTRUE(rel$present)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = rel$relevance,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "gtex_tissue",
      domain = "expression",
      title = sprintf(
        "GTEx: %s median %.1f TPM",
        rel$matched$tissue,
        rel$matched$median
      ),
      detail = "median gene expression in a tissue of interest",
      score = NA_real_,
      source_id = paste0(r$source_id, ":", rel$matched$tissue),
      source_url = r$source_url
    )
  )
}

# PanelApp: Genomics England panel-membership confidence for the disease, read
# from the once-fetched panel table (context$seed_data). Discovery mode only.
extract_panelapp <- function(resolved, context = list()) {
  row <- seed_row(context, "panelapp", seed_lookup_symbols(resolved))
  if (is.null(row)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = row$raw,
    source_id = row$source_id,
    source_url = row$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "panelapp",
      domain = "pathway-disease",
      title = sprintf("Genomics England PanelApp confidence %.2f", row$raw),
      detail = "diagnostic-panel gene rating (green = 1.0, amber = 0.5)",
      score = row$raw,
      source_id = row$source_id,
      source_url = row$source_url
    )
  )
}

# DISEASES (Jensen Lab): knowledge + text-mining association of the gene to the
# disease, read from context$seed_data. Discovery mode only.
extract_diseases <- function(resolved, context = list()) {
  row <- seed_row(context, "diseases", seed_lookup_symbols(resolved))
  if (is.null(row)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = row$raw,
    source_id = row$source_id,
    source_url = row$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "diseases",
      domain = "pathway-disease",
      title = sprintf("DISEASES (Jensen Lab) association score %.2f", row$raw),
      detail = "knowledge + text-mining channels",
      score = row$raw,
      source_id = row$source_id,
      source_url = row$source_url
    )
  )
}

# HPO: the Mendelian / phenotype diseases the Human Phenotype Ontology associates
# with the gene (each grounded by an OMIM / ORPHA id). In a disease-scoped review
# the raw value is the count of associated diseases MATCHING the study context
# (strong, context-specific gene-disease evidence); otherwise the count of all
# associated diseases. Uses the resolved NCBI Gene id. Opt-in (default_on FALSE).
extract_hpo <- function(resolved, context = list()) {
  r <- hpo_gene_diseases(resolved$entrez)
  if (!isTRUE(r$ok) || nrow(r$diseases) == 0) {
    return(signal_miss())
  }
  rel <- hpo_relevance(r$diseases, pluck_at(context, "disease"))
  if (!isTRUE(rel$present)) {
    return(signal_miss())
  }
  ev <- rel$matched
  list(
    ok = TRUE,
    raw = rel$n,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "hpo",
      domain = "gene-disease",
      title = paste0("HPO gene-disease association: ", ev$name),
      detail = "Human Phenotype Ontology gene-disease annotation",
      score = NA_real_,
      # Fall back to the gene-level source_id when a disease has no per-disease id.
      # nzchar(NA) is TRUE and ifelse() with an NA condition returns NA, so the
      # is.na() guard is required - without it a null-id row carries source_id = NA
      # and the citation gate drops it (still counted in raw): a grounding mismatch.
      source_id = ifelse(!is.na(ev$id) & nzchar(ev$id), ev$id, r$source_id),
      source_url = r$source_url
    )
  )
}

# HPA: how many disease/cancer classifications the Human Protein Atlas assigns the
# gene (e.g. "Cancer-related genes", "Tumor suppressor", "Disease related genes").
# A disease/cancer-relevance ANNOTATION (nudges up, never gates), grounded by the
# HPA gene page. Uses the resolved Ensembl id. Opt-in (default_on FALSE).
extract_hpa <- function(resolved, context = list()) {
  r <- hpa_gene(resolved$gene_id)
  if (!isTRUE(r$ok)) {
    return(signal_miss())
  }
  rel <- hpa_relevance(r)
  if (!isTRUE(rel$present)) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = rel$n,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "hpa",
      domain = "cancer",
      title = paste0("HPA classification: ", rel$tags),
      detail = "Human Protein Atlas curated disease/cancer classification",
      score = NA_real_,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# cBioPortal: the gene's somatic mutation frequency across a large pan-cancer cohort
# (MSK-IMPACT). raw is the mutated fraction (0..1); higher = more recurrently mutated
# in cancer. Research evidence (recurrence), never a clinical call. Opt-in (off by
# default). Uses the HUGO symbol.
extract_cbioportal <- function(resolved, context = list()) {
  r <- cbioportal_gene_frequency(resolved$symbol)
  if (!isTRUE(r$ok) || r$mutated <= 0) {
    return(signal_miss())
  }
  pct <- format(round(100 * r$frequency, 1), nsmall = 1)
  list(
    ok = TRUE,
    raw = r$frequency,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "cbioportal",
      domain = "cancer",
      title = paste0(
        "Somatic mutation frequency (MSK-IMPACT): ",
        pct,
        "% of ",
        r$total,
        " tumors"
      ),
      detail = "cBioPortal cross-cancer somatic mutation frequency (research evidence)",
      score = r$frequency,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# CIViC: the count of expert-curated clinical evidence items for the gene - a grounded
# measure of curated cancer variant-interpretation weight. raw = evidence-item count.
# Research evidence (curated literature), never a clinical call. Opt-in. HUGO symbol.
extract_civic <- function(resolved, context = list()) {
  r <- civic_gene_evidence(resolved$symbol)
  if (!isTRUE(r$ok) || r$evidence_items <= 0) {
    return(signal_miss())
  }
  list(
    ok = TRUE,
    raw = r$evidence_items,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "civic",
      domain = "cancer",
      title = paste0(
        "CIViC curated clinical evidence: ",
        r$evidence_items,
        " item(s)"
      ),
      detail = "CIViC expert-curated cancer variant clinical evidence (CC0)",
      score = r$evidence_items,
      source_id = r$source_id,
      source_url = r$source_url
    )
  )
}

# ClinGen: the strength of expert-curated gene-disease validity (Definitive..Limited),
# scoped to the study disease when one is given. raw = the strongest matching
# classification (ordinal 0..4). Research evidence (curated validity), never a
# clinical call. Opt-in. Uses the HUGO symbol against the bulk CSV.
extract_clingen <- function(resolved, context = list()) {
  r <- clingen_gene_validity(resolved$symbol)
  if (!isTRUE(r$ok) || r$n == 0) {
    return(signal_miss())
  }
  rel <- clingen_relevance(r$curations, pluck_at(context, "disease"))
  if (!isTRUE(rel$present)) {
    return(signal_miss())
  }
  m <- rel$matched
  top <- m[which.max(clingen_strength(m$classification)), , drop = FALSE]
  sig_source_id <- if (!is.na(top$report_url) && nzchar(top$report_url)) {
    top$report_url
  } else {
    paste0("ClinGen:", top$hgnc)
  }
  list(
    ok = TRUE,
    raw = rel$strength,
    source_id = sig_source_id,
    source_url = if (!is.na(top$report_url) && nzchar(top$report_url)) {
      top$report_url
    } else {
      CLINGEN_GV_URL
    },
    evidence = evidence_long_rows(
      resolved$gene_id,
      "clingen",
      domain = "gene-disease",
      title = paste0("ClinGen validity: ", m$classification, " - ", m$disease),
      detail = "ClinGen expert-curated gene-disease validity classification",
      score = clingen_strength(m$classification),
      # Ground each row on its assertion URL; fall back to the HGNC id (with the
      # is.na guard - nzchar(NA) is TRUE, so ifelse alone would keep an NA id).
      source_id = ifelse(
        !is.na(m$report_url) & nzchar(m$report_url),
        m$report_url,
        paste0("ClinGen:", m$hgnc)
      ),
      source_url = ifelse(
        !is.na(m$report_url) & nzchar(m$report_url),
        m$report_url,
        CLINGEN_GV_URL
      )
    )
  )
}

# QuickGO (Gene Ontology): the gene's biological-process functional annotation.
# With a study pathway prior (context$priors$pathways) the raw value is the count
# of BP terms whose name matches that biology (function-in-context, e.g. RAS/MAPK
# for NF1); without one, the count of distinct BP terms (general functional depth).
# Annotation (nudges up, never gates). Uses the resolved UniProt accession; each GO
# term is grounded evidence (drill-down capped at 12; raw is the full count).
extract_go <- function(resolved, context = list()) {
  r <- quickgo_annotations(resolved$uniprot)
  if (!isTRUE(r$ok) || nrow(r$terms) == 0) {
    return(signal_miss())
  }
  ctx_pathways <- pluck_at(context, "priors", "pathways")
  terms <- r$terms
  if (!is.null(ctx_pathways) && length(ctx_pathways) > 0) {
    matched <- pathway_matches_context(terms$go_name, ctx_pathways)
    terms <- terms[matched, , drop = FALSE]
    if (nrow(terms) == 0) {
      return(signal_miss())
    }
  }
  ev <- utils::head(terms, 12)
  list(
    ok = TRUE,
    raw = nrow(terms),
    source_id = terms$source_id[1],
    source_url = terms$source_url[1],
    evidence = evidence_long_rows(
      resolved$gene_id,
      "go",
      domain = "function",
      title = paste0("GO biological process: ", ev$go_name),
      detail = "Gene Ontology functional annotation (QuickGO)",
      score = NA_real_,
      source_id = ev$source_id,
      source_url = ev$source_url
    )
  )
}

# UniProt (Swiss-Prot) disease involvement: curated gene-disease links. In a
# disease-scoped review the raw value is the count of curated diseases MATCHING the
# study context; otherwise the count of all curated diseases. Independent expert
# curation, so it corroborates HPO / ClinGen / DISEASES from a different channel.
# Evidence (opt-in). Uses the resolved UniProt accession; each disease is grounded
# by its UniProt disease id (DI-…).
extract_uniprot_disease <- function(resolved, context = list()) {
  r <- uniprot_gene_diseases(resolved$uniprot)
  if (!isTRUE(r$ok) || nrow(r$diseases) == 0) {
    return(signal_miss())
  }
  rel <- uniprot_disease_relevance(r$diseases, pluck_at(context, "disease"))
  if (!isTRUE(rel$present)) {
    return(signal_miss())
  }
  m <- rel$matched
  list(
    ok = TRUE,
    raw = rel$n,
    source_id = m$source_id[1],
    source_url = m$source_url[1],
    evidence = evidence_long_rows(
      resolved$gene_id,
      "uniprot_disease",
      domain = "gene-disease",
      title = paste0("UniProt disease involvement: ", m$name),
      detail = ifelse(
        m$causal,
        "Swiss-Prot: disease caused by variants in this gene",
        "Swiss-Prot: gene may be involved in disease pathogenesis"
      ),
      score = NA_real_,
      source_id = m$source_id,
      source_url = m$source_url
    )
  )
}

# PDBe: experimentally solved 3D structures covering the gene's protein. raw = the
# count of distinct PDB entries; a structurally characterized target is more
# tractable for mechanistic and structure-guided follow-up (complements the Pharos
# ligand-tractability level). Annotation (nudges up, never gates). Uses the resolved
# UniProt accession; each structure is grounded by its PDB id (drill-down capped at
# 12; raw is the full count).
extract_pdbe <- function(resolved, context = list()) {
  r <- pdbe_structures(resolved$uniprot)
  if (!isTRUE(r$ok) || r$n <= 0) {
    return(signal_miss())
  }
  ev <- utils::head(r$structures, 12)
  list(
    ok = TRUE,
    raw = r$n,
    source_id = r$structures$source_id[1],
    source_url = r$structures$source_url[1],
    evidence = evidence_long_rows(
      resolved$gene_id,
      "pdbe",
      domain = "structure",
      title = paste0("PDB structure ", toupper(ev$pdb_id), ": ", ev$method),
      detail = ifelse(
        is.na(ev$resolution),
        "Experimental 3D structure (PDBe)",
        sprintf("Experimental 3D structure, %.2g Å (PDBe)", ev$resolution)
      ),
      score = NA_real_,
      source_id = ev$source_id,
      source_url = ev$source_url
    )
  )
}

# IMPC: the significant phenotypes a mouse KNOCKOUT of the gene's ortholog produces -
# in-vivo functional-genetics evidence. raw = count of distinct significant phenotype
# (MP/MPATH) terms; a gene IMPC never phenotyped, or phenotyped with none significant,
# is a miss (never a 0). A hypothesis-free screen, so it is not a study-popularity
# proxy. Annotation (nudges, never gates), opt-in. Keyed by the human symbol, mapped
# to the mouse ortholog internally.
extract_impc <- function(resolved, context = list()) {
  r <- impc_gene_phenotypes(resolved$symbol)
  if (!isTRUE(r$ok) || r$n <= 0) {
    return(signal_miss())
  }
  ph <- r$phenotypes
  list(
    ok = TRUE,
    raw = r$n,
    source_id = r$source_id,
    source_url = r$source_url,
    evidence = evidence_long_rows(
      resolved$gene_id,
      "impc",
      domain = "model-organism",
      title = paste0("Mouse knockout phenotype: ", ph$mp_name),
      detail = paste0(
        "IMPC significant ",
        ph$zygosity,
        " knockout phenotype (",
        r$marker_symbol,
        ")"
      ),
      score = NA_real_,
      source_id = ph$source_id,
      source_url = ph$source_url
    )
  )
}

# --- Discovery seeding ------------------------------------------------------

# Prioritize + cap the seeded candidate union so a broad disease does not fan
# out into thousands of live per-gene queries (each seeded gene later fires the
# gene-keyed extractors). Priority is the sum of each source's normalized
# contribution, so a multi-source gene ranks ahead of a single-source one; ties
# break on symbol for determinism. Pure, so it is testable offline. Returns the
# (deduped, NA-stripped) symbols unchanged when already within `max_seed`.
cap_seed_symbols <- function(symbols, data, max_seed = 200) {
  symbols <- unique(symbols[!is.na(symbols) & nzchar(symbols)])
  if (length(symbols) <= max_seed) {
    return(symbols)
  }
  prio <- stats::setNames(numeric(length(symbols)), symbols)
  contrib <- function(tbl, tf) {
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(NULL)
    }
    val <- tf(as.numeric(tbl$raw))
    val[is.na(val)] <- 0
    tapply(val, tbl$symbol, max) # best contribution per symbol within a source
  }
  pieces <- list(
    contrib(data$ot_targets, function(x) pmin(pmax(x, 0), 1)),
    contrib(data$panelapp, function(x) pmin(pmax(x, 0), 1)),
    contrib(data$diseases, function(x) x / (x + 2))
  )
  for (p in pieces) {
    if (is.null(p)) {
      next
    }
    idx <- match(names(p), names(prio))
    ok <- !is.na(idx)
    prio[idx[ok]] <- prio[idx[ok]] + as.numeric(p)[ok]
  }
  names(prio)[order(-prio, names(prio))][seq_len(max_seed)]
}

# Gather candidate genes for a disease from the disease-keyed sources (Open
# Targets associated targets, PanelApp, DISEASES). Returns the union of gene
# symbols (deduped, NA-stripped, and capped to `max_seed` by seed strength) plus
# a normalized per-source table (symbol, raw, source_id, source_url) that the
# disease-mode extractors read via seed_row(). `n_seeded` is the pre-cap count so
# a caller can record when the universe was truncated. Each source is wrapped so
# one failing source never sinks the others.
# The interactive (Shiny) disease-seed cap. A disease can associate with hundreds
# of genes (NF1 -> ~520); enriching all of them live is minutes of blocking work,
# which reads as a hung app. The batch default (max_seed = 200 below) is kept for
# the CLI/evals, where a long run is fine; the interactive app passes this smaller
# cap so a discovery click returns in a bounded time. The user's OWN pasted genes
# are enriched up to GENESCOUT_INTERACTIVE_INPUT_MAX (a generous safety bound); the
# disease-seeded universe is capped separately and more tightly here.
GENESCOUT_INTERACTIVE_SEED_MAX <- 75L

# The interactive (Shiny) cap on the USER's own candidate list (run_enrich's
# `max_genes`). A pasted list of thousands would fan out to (#genes x #sources)
# serial calls and read as a hung app; this bounds an interactive run while staying
# far above any realistic candidate set. The CLI/eval path leaves max_genes = Inf.
GENESCOUT_INTERACTIVE_INPUT_MAX <- 300L

seed_disease_genes <- function(disease, ot_size = 250, max_seed = 200) {
  data <- list()
  syms <- character()
  # Uppercase + drop NA/blank symbols: a source row with a null approvedSymbol
  # yields NA, which must never enter the candidate universe.
  norm_tbl <- function(symbol, raw, source_id, source_url) {
    tbl <- tibble::tibble(
      symbol = toupper(as.character(symbol)),
      raw = as.numeric(raw),
      source_id = as.character(source_id),
      source_url = as.character(source_url)
    )
    tbl[!is.na(tbl$symbol) & nzchar(tbl$symbol), , drop = FALSE]
  }

  ot <- tryCatch(
    ot_disease_targets(disease$id, size = ot_size),
    error = function(e) list(ok = FALSE)
  )
  if (isTRUE(ot$ok) && nrow(ot$genes) > 0) {
    data$ot_targets <- norm_tbl(
      ot$genes$symbol,
      ot$genes$score,
      ot$genes$source_id,
      ot$genes$source_url
    )
    syms <- c(syms, data$ot_targets$symbol)
  }

  pa <- tryCatch(
    panelapp_disease_genes(disease$name),
    error = function(e) list(ok = FALSE)
  )
  if (isTRUE(pa$ok) && nrow(pa$genes) > 0) {
    data$panelapp <- norm_tbl(
      pa$genes$symbol,
      pa$genes$confidence,
      pa$genes$source_id,
      pa$genes$source_url
    )
    syms <- c(syms, data$panelapp$symbol)
  }

  doid <- tryCatch(
    ot_disease_doid(disease$id),
    error = function(e) list(ok = FALSE)
  )
  if (isTRUE(doid$ok) && !is_blank(doid$doid)) {
    dis <- tryCatch(
      diseases_gene_associations(doid$doid),
      error = function(e) list(ok = FALSE)
    )
    if (isTRUE(dis$ok) && nrow(dis$genes) > 0) {
      data$diseases <- norm_tbl(
        dis$genes$symbol,
        dis$genes$score,
        dis$genes$source_id,
        dis$genes$source_url
      )
      syms <- c(syms, data$diseases$symbol)
    }
  }

  syms <- unique(syms[!is.na(syms) & nzchar(syms)])
  list(
    symbols = cap_seed_symbols(syms, data, max_seed = max_seed),
    data = data,
    n_seeded = length(syms),
    max_seed = max_seed
  )
}

# --- Flatten, resolve, dedupe -----------------------------------------------

# Union the sources of a candidate_set into unique tokens, remembering which
# source label(s) / id(s) / type(s) each token came from. Dedup by uppercased
# token, first-seen order preserved. Blank/NA tokens and '#' comments are
# dropped. Returns tibble(token, input_lists, input_source_ids,
# input_source_types) with sorted list-cols. `input_lists` (source labels) is
# byte-identical to the old flatten_gene_lists output, so existing provenance is
# unchanged; the id/type columns are additive (report/provenance only).
flatten_candidate_set <- function(cs) {
  cs <- as_candidate_set(cs)
  membership <- list()
  order <- character()
  for (src in cs) {
    toks <- trimws(as.character(src$genes))
    # Drop NA first: nzchar(NA) is TRUE and startsWith(NA, "#") is NA, so an NA
    # token would survive the filter and later crash the membership lookup. NAs
    # reach here from a seeded gene with a null source symbol or an empty CSV cell.
    toks <- toks[!is.na(toks) & nzchar(toks) & !startsWith(toks, "#")]
    for (t in toks) {
      key <- toupper(t)
      if (is.null(membership[[key]])) {
        membership[[key]] <- list(
          token = t,
          labels = character(),
          ids = character(),
          types = character()
        )
        order <- c(order, key)
      }
      membership[[key]]$labels <- union(membership[[key]]$labels, src$label)
      membership[[key]]$ids <- union(membership[[key]]$ids, src$id)
      membership[[key]]$types <- union(membership[[key]]$types, src$type)
    }
  }
  if (length(order) == 0) {
    return(tibble::tibble(
      token = character(),
      input_lists = list(),
      input_source_ids = list(),
      input_source_types = list()
    ))
  }
  pull_sorted <- function(field) {
    unname(lapply(order, function(k) sort(membership[[k]][[field]])))
  }
  tibble::tibble(
    token = vapply(
      order,
      function(k) membership[[k]]$token,
      character(1),
      USE.NAMES = FALSE
    ),
    input_lists = pull_sorted("labels"),
    input_source_ids = pull_sorted("ids"),
    input_source_types = pull_sorted("types")
  )
}

# Back-compat shim: a named list of character vectors is one source per name.
# Delegates to flatten_candidate_set so the two share one implementation and the
# `token` + sorted `input_lists` columns stay byte-identical to before.
flatten_gene_lists <- function(lists) {
  flatten_candidate_set(lists)
}

# Resolve every token to a canonical gene id (MyGene ensembl_gene) and dedupe by
# it, so aliases collapse to one gene. `resolver` is injectable for offline tests.
# Unresolved tokens keep a synthetic id and resolved = FALSE (shown, not dropped).
#
# The whole list is resolved in ONE batched request (`batch_resolver`) instead of a
# GET per token - the biggest first-run latency lever. Batching is used only on the
# default (network) resolver: a caller that injects a stub `resolver` (every offline
# test) stays serial with no network, exactly as before. A batch transport failure
# falls back to serial resolution via `resolver`, so the proven path is never lost.
resolve_genes <- function(flat, resolver = resolve_symbol) {
  if (nrow(flat) == 0) {
    return(empty_resolved())
  }
  batched <- NULL
  if (identical(resolver, resolve_symbol)) {
    batched <- tryCatch(resolve_symbols_batch(flat$token), error = function(e) {
      NULL
    })
    # A batch that comes back the wrong shape/length is a miss -> serial fallback.
    if (!is.list(batched) || length(batched) != nrow(flat)) {
      batched <- NULL
    }
  }
  has_ids <- "input_source_ids" %in% names(flat)
  has_types <- "input_source_types" %in% names(flat)
  rows <- lapply(seq_len(nrow(flat)), function(i) {
    token <- flat$token[i]
    r <- if (!is.null(batched)) batched[[i]] else resolver(token)
    prov <- list(
      token = token,
      input_lists = flat$input_lists[[i]],
      input_source_ids = if (has_ids) {
        flat$input_source_ids[[i]]
      } else {
        character()
      },
      input_source_types = if (has_types) {
        flat$input_source_types[[i]]
      } else {
        character()
      }
    )
    resolved <- if (isTRUE(r$ok) && !is_blank(r$ensembl_gene)) {
      list(
        gene_id = r$ensembl_gene,
        symbol = r$symbol %||% token,
        entrez = as.character(r$entrez %||% NA),
        uniprot = as.character(r$uniprot %||% NA),
        resolved = TRUE
      )
    } else {
      list(
        gene_id = paste0("UNRESOLVED:", toupper(token)),
        symbol = token,
        entrez = NA_character_,
        uniprot = NA_character_,
        resolved = FALSE
      )
    }
    c(resolved, prov)
  })
  merge_resolved(rows)
}

# Collapse resolved rows by gene_id, aggregating input symbols and list membership.
merge_resolved <- function(rows) {
  by_id <- list()
  order <- character()
  for (r in rows) {
    id <- r$gene_id
    if (is.null(by_id[[id]])) {
      by_id[[id]] <- list(
        gene_id = id,
        symbol = r$symbol,
        entrez = r$entrez,
        uniprot = r$uniprot,
        resolved = r$resolved,
        input_symbols = character(),
        input_lists = character(),
        input_source_ids = character(),
        input_source_types = character()
      )
      order <- c(order, id)
    }
    by_id[[id]]$input_symbols <- union(by_id[[id]]$input_symbols, r$token)
    by_id[[id]]$input_lists <- union(by_id[[id]]$input_lists, r$input_lists)
    by_id[[id]]$input_source_ids <- union(
      by_id[[id]]$input_source_ids,
      r$input_source_ids %||% character()
    )
    by_id[[id]]$input_source_types <- union(
      by_id[[id]]$input_source_types,
      r$input_source_types %||% character()
    )
  }
  pull <- function(f) vapply(order, f, character(1), USE.NAMES = FALSE)
  pull_list <- function(field) {
    unname(lapply(order, function(k) sort(by_id[[k]][[field]])))
  }
  tibble::tibble(
    gene_id = pull(function(k) by_id[[k]]$gene_id),
    symbol = pull(function(k) by_id[[k]]$symbol),
    entrez = pull(function(k) as.character(by_id[[k]]$entrez)),
    uniprot = pull(function(k) as.character(by_id[[k]]$uniprot %||% NA)),
    resolved = vapply(
      order,
      function(k) by_id[[k]]$resolved,
      logical(1),
      USE.NAMES = FALSE
    ),
    input_symbols = pull_list("input_symbols"),
    input_lists = pull_list("input_lists"),
    input_source_ids = pull_list("input_source_ids"),
    input_source_types = pull_list("input_source_types")
  )
}

empty_resolved <- function() {
  tibble::tibble(
    gene_id = character(),
    symbol = character(),
    entrez = character(),
    uniprot = character(),
    resolved = logical(),
    input_symbols = list(),
    input_lists = list(),
    input_source_ids = list(),
    input_source_types = list()
  )
}

# Summarize how many candidate genes resolved to a canonical id, for a UI notice
# ("N of M could not be resolved"). Unresolved tokens (typos/aliases MyGene could
# not map) are ranked at the bottom with no signals, so a heads-up prevents silent
# confusion. Reads the `resolved` flag on the gene matrix; degrades to zeros when it
# is absent (a hand-built matrix in tests).
resolution_summary <- function(genes) {
  if (is.null(genes) || nrow(genes) == 0 || !("resolved" %in% names(genes))) {
    return(list(
      total = 0L,
      resolved = 0L,
      unresolved = 0L,
      unresolved_symbols = character()
    ))
  }
  ok <- as.logical(genes$resolved)
  ok[is.na(ok)] <- FALSE
  list(
    total = nrow(genes),
    resolved = sum(ok),
    unresolved = sum(!ok),
    unresolved_symbols = as.character(genes$symbol[!ok])
  )
}

# --- Enrich + assemble ------------------------------------------------------

empty_signals_long <- function() {
  tibble::tibble(
    gene_id = character(),
    symbol = character(),
    signal_key = character(),
    label = character(),
    raw = numeric(),
    normalized = numeric(),
    present = logical(),
    source_id = character(),
    source_url = character()
  )
}

# Run every gene-keyed registry signal for every resolved gene. Returns the tidy
# signals table (one row per gene x signal) and grounded evidence for drill-down.
# Disease-keyed (`needs = "disease"`) signals are seeders handled before this
# step; they are skipped here. `context` is passed to each extractor.
enrich_genes <- function(
  resolved,
  registry = genescout_signal_registry(),
  context = list(),
  progress = NULL
) {
  gene_signals <- Filter(
    function(s) identical(s$needs %||% "gene", "gene"),
    registry
  )
  has_input_symbols <- "input_symbols" %in% names(resolved)
  signals <- list()
  evidence <- list()
  n_genes <- nrow(resolved)
  for (i in seq_len(n_genes)) {
    gene <- list(
      gene_id = resolved$gene_id[i],
      symbol = resolved$symbol[i],
      entrez = resolved$entrez[i],
      uniprot = if ("uniprot" %in% names(resolved)) {
        resolved$uniprot[i]
      } else {
        NA_character_
      },
      resolved = resolved$resolved[i],
      input_symbols = if (has_input_symbols) {
        resolved$input_symbols[[i]]
      } else {
        character()
      }
    )
    for (sig in gene_signals) {
      # Run the extractor when the gene resolved, OR when the signal is
      # seed-backed and this gene's grounded evidence is already in seed_data
      # (an offline read that needs only the symbol) - so a seeded gene MyGene
      # could not resolve is not silently blanked. Live signals still skip
      # unresolved genes (no wasted network calls on junk tokens).
      seed_backed <- !is.null(sig$seed_key) &&
        !is.null(seed_row(context, sig$seed_key, seed_lookup_symbols(gene)))
      res <- if (isTRUE(gene$resolved) || seed_backed) {
        tryCatch(sig$extractor(gene, context), error = function(e) {
          signal_miss()
        })
      } else {
        signal_miss()
      }
      raw <- as.numeric(res$raw)
      signals[[length(signals) + 1]] <- tibble::tibble(
        gene_id = gene$gene_id,
        symbol = gene$symbol,
        signal_key = sig$key,
        label = sig$label,
        raw = raw,
        normalized = as.numeric(sig$normalize(raw)),
        present = isTRUE(res$ok) && !is.na(raw),
        source_id = res$source_id %||% "",
        source_url = res$source_url %||% ""
      )
      if (!is.null(res$evidence) && nrow(res$evidence) > 0) {
        evidence[[length(evidence) + 1]] <- res$evidence
      }
    }
    # Report progress after each gene finishes so a UI can show a determinate bar
    # (a disease-seeded run enriches dozens of genes and would otherwise look hung).
    # `progress` is optional and defaults to NULL, so the engine stays UI-agnostic
    # and every offline test is unchanged.
    if (is.function(progress)) {
      progress(i, n_genes, gene$symbol)
    }
  }
  list(
    signals_long = if (length(signals)) {
      dplyr::bind_rows(signals)
    } else {
      empty_signals_long()
    },
    evidence_long = if (length(evidence)) {
      dplyr::bind_rows(evidence)
    } else {
      empty_evidence_long()
    }
  )
}

# Fill the `needs = "input"` signals (currently just cross_source) from the input
# structure - no network. For each such signal and every resolved gene, the raw
# value is the count of the user's OWN sources the gene appears in
# (resolved$input_lists intersected with context$user_sources, which EXCLUDES any
# engine-seeded disease source). A gene corroborated by >= 2 user sources is
# "present" and emits one grounded provenance evidence row per corroborating
# source (domain "input-provenance", source_id "user-list:<label>"), so the
# citation gate keeps it and the drill-down can show it. Returns the same
# signals_long / evidence_long shapes enrich_genes() does, ready to bind.
enrich_input_signals <- function(resolved, registry, context = list()) {
  input_signals <- Filter(
    function(s) identical(s$needs %||% "gene", "input"),
    registry
  )
  if (length(input_signals) == 0 || nrow(resolved) == 0) {
    return(list(
      signals_long = empty_signals_long(),
      evidence_long = empty_evidence_long()
    ))
  }
  user_sources <- context$user_sources %||% character()
  # Optional per-source trust weights (label -> weight, default 1) the user set
  # next to each candidate list. as.list() so a missing label yields NULL (not an
  # out-of-bounds error) whether a named list or vector was passed.
  source_weights <- as.list(context$source_weights %||% list())
  src_weight <- function(l) {
    v <- source_weights[[l]]
    if (is.null(v) || length(v) == 0 || is.na(v[1])) 1 else as.numeric(v[1])
  }
  has_input_lists <- "input_lists" %in% names(resolved)
  signals <- list()
  evidence <- list()
  for (i in seq_len(nrow(resolved))) {
    lists_i <- if (has_input_lists) resolved$input_lists[[i]] else character()
    corroborating <- sort(intersect(lists_i, user_sources))
    n <- length(corroborating)
    present <- n >= 2
    # Weighted corroboration: sum the trust weights of the corroborating sources.
    # A lone source stays neutral (raw = n < 2 normalizes to 0), so a high weight
    # can never manufacture breadth from a single source. With every weight at the
    # default 1, w_sum == n, so the signal is byte-identical to the unweighted case.
    raw_corr <- if (present) {
      sum(vapply(corroborating, src_weight, numeric(1)))
    } else {
      as.numeric(n)
    }
    for (sig in input_signals) {
      raw <- raw_corr
      signals[[length(signals) + 1]] <- tibble::tibble(
        gene_id = resolved$gene_id[i],
        symbol = resolved$symbol[i],
        signal_key = sig$key,
        label = sig$label,
        raw = raw,
        normalized = as.numeric(sig$normalize(raw)),
        present = present,
        source_id = if (present) {
          paste0("user-list:", paste(corroborating, collapse = "|"))
        } else {
          ""
        },
        source_url = ""
      )
      if (present) {
        for (src in corroborating) {
          evidence[[length(evidence) + 1]] <- evidence_long_rows(
            resolved$gene_id[i],
            sig$key,
            "input-provenance",
            paste0("Listed in your source: ", src),
            "One of your own candidate sources",
            NA_real_,
            paste0("user-list:", src),
            ""
          )
        }
      }
    }
  }
  list(
    signals_long = if (length(signals)) {
      dplyr::bind_rows(signals)
    } else {
      empty_signals_long()
    },
    evidence_long = if (length(evidence)) {
      dplyr::bind_rows(evidence)
    } else {
      empty_evidence_long()
    }
  )
}

# Fill the `needs = "network"` signals (currently just STRING) from ONE call that
# sees the whole resolved set - unlike the per-gene extractors. For each network
# signal and every resolved gene, the raw value is the gene's within-list
# connectivity (how many OTHER resolved genes it shares a high-confidence STRING
# edge with); a connected gene is "present" and emits one grounded evidence row per
# interacting partner (domain "interaction", source_id "STRING:<a>-<b>"). Only
# genes that RESOLVED are sent to STRING (junk tokens are skipped). `fetch_network`
# is injectable so the fill is testable offline. A failed/absent network (or a gene
# past the STRING_MAX_NODES cap) leaves that gene's raw value NA - so it reads "no
# data" in the report, never a measured "0 interactions" - and absent (annotation
# neutral, composites unchanged); only a gene STRING was actually asked about and
# returned isolated gets a grounded degree 0. Returns signals_long / evidence_long
# (as enrich_genes() does) plus `capped` (non-NULL when the query was truncated).
enrich_network_signals <- function(
  resolved,
  registry,
  context = list(),
  fetch_network = string_network
) {
  net_signals <- Filter(
    function(s) identical(s$needs %||% "gene", "network"),
    registry
  )
  if (length(net_signals) == 0 || nrow(resolved) == 0) {
    return(list(
      signals_long = empty_signals_long(),
      evidence_long = empty_evidence_long()
    ))
  }
  all_syms <- toupper(resolved$symbol)
  resolved_ok <- if ("resolved" %in% names(resolved)) {
    as.logical(resolved$resolved)
  } else {
    rep(TRUE, nrow(resolved))
  }
  resolved_ok[is.na(resolved_ok)] <- FALSE
  query_syms <- unique(all_syms[resolved_ok & nzchar(all_syms)])
  net <- if (length(query_syms) >= 2) {
    tryCatch(fetch_network(query_syms), error = function(e) list(ok = FALSE))
  } else {
    list(ok = FALSE)
  }
  conn <- if (isTRUE(net$ok)) {
    string_connectivity(net$edges, all_syms)
  } else {
    NULL
  }
  net_url <- if (isTRUE(net$ok)) net$source_url %||% "" else ""
  # The exact symbol set STRING was asked about, so a gene queried and found
  # isolated (grounded degree 0) is distinguishable from one never queried (fetch
  # failed, or dropped past STRING_MAX_NODES), which must read NA rather than 0.
  queried <- if (isTRUE(net$ok)) {
    toupper(as.character(net$queried %||% query_syms))
  } else {
    character()
  }

  signals <- list()
  evidence <- list()
  for (i in seq_len(nrow(resolved))) {
    sym <- all_syms[i]
    row <- if (!is.null(conn)) {
      conn[conn$symbol == sym, , drop = FALSE]
    } else {
      NULL
    }
    degree <- if (!is.null(row) && nrow(row) > 0) {
      as.numeric(row$degree[1])
    } else {
      0
    }
    partners <- if (!is.null(row) && nrow(row) > 0) {
      row$partners[[1]]
    } else {
      character()
    }
    measured <- isTRUE(net$ok) && (sym %in% queried)
    present <- measured && degree >= 1
    for (sig in net_signals) {
      raw <- if (measured) as.numeric(degree) else NA_real_
      signals[[length(signals) + 1]] <- tibble::tibble(
        gene_id = resolved$gene_id[i],
        symbol = resolved$symbol[i],
        signal_key = sig$key,
        label = sig$label,
        raw = raw,
        normalized = as.numeric(sig$normalize(raw)),
        present = present,
        source_id = if (present) {
          paste0("STRING:", sym, ":degree:", degree)
        } else {
          ""
        },
        source_url = if (present) net_url else ""
      )
      if (present) {
        for (p in partners) {
          evidence[[length(evidence) + 1]] <- evidence_long_rows(
            resolved$gene_id[i],
            sig$key,
            "interaction",
            paste0("STRING interaction: ", sym, " - ", p),
            "High-confidence within-list STRING interaction (combined score >= 0.7)",
            NA_real_,
            paste0("STRING:", sym, "-", p),
            paste0(
              STRING_WEB,
              "?identifiers=",
              sym,
              "%0d",
              p,
              "&species=",
              STRING_HUMAN
            )
          )
        }
      }
    }
  }
  # Audit a truncated query (more resolved genes than STRING_MAX_NODES) the same way
  # the seed cap is audited, so the dropped genes are a recorded limit, not silent.
  capped <- if (isTRUE(net$truncated)) {
    list(kept = net$n_query, total = net$n_query + (net$n_dropped %||% 0L))
  } else {
    NULL
  }
  list(
    signals_long = if (length(signals)) {
      dplyr::bind_rows(signals)
    } else {
      empty_signals_long()
    },
    evidence_long = if (length(evidence)) {
      dplyr::bind_rows(evidence)
    } else {
      empty_evidence_long()
    },
    capped = capped
  )
}

# Pivot the tidy signals to one row per gene (raw <key> + normalized <key>_n
# columns), joined with resolved-gene metadata and a coverage count.
assemble_matrix <- function(
  signals_long,
  resolved,
  registry = genescout_signal_registry()
) {
  keys <- vapply(registry, function(s) s$key, character(1))
  roles <- vapply(registry, function(s) s$role %||% "evidence", character(1))
  genes <- resolved$gene_id
  # Pre-extract: tibble() data-masks columns as it builds them, so a column named
  # `resolved` would shadow the `resolved` argument for later columns.
  symbols <- resolved$symbol
  resolved_flag <- resolved$resolved
  input_lists <- resolved$input_lists
  mat <- tibble::tibble(
    gene_id = genes,
    symbol = symbols,
    resolved = resolved_flag,
    input_lists = input_lists
  )
  # Carry per-source provenance when present (report / drill-down only). Guarded
  # so a hand-built resolved tibble without these columns still assembles.
  if ("input_source_ids" %in% names(resolved)) {
    mat$input_source_ids <- resolved$input_source_ids
  }
  if ("input_source_types" %in% names(resolved)) {
    mat$input_source_types <- resolved$input_source_types
  }
  for (k in keys) {
    sub <- signals_long[signals_long$signal_key == k, , drop = FALSE]
    raw_by <- stats::setNames(sub$raw, sub$gene_id)
    norm_by <- stats::setNames(sub$normalized, sub$gene_id)
    pres_by <- stats::setNames(sub$present, sub$gene_id)
    mat[[k]] <- as.numeric(raw_by[genes])
    mat[[paste0(k, "_n")]] <- as.numeric(norm_by[genes])
    p <- pres_by[genes]
    p[is.na(p)] <- FALSE
    mat[[paste0(k, "_present")]] <- as.logical(p)
  }
  # Coverage counts: all present signals (display) and evidence-only present
  # signals (the breadth measure the composite's coverage bonus uses).
  ev_keys <- keys[roles == "evidence"]
  count_present <- function(g, which_keys) {
    sum(signals_long$present[
      signals_long$gene_id == g & signals_long$signal_key %in% which_keys
    ])
  }
  mat$n_sources_present <- vapply(
    genes,
    function(g) count_present(g, keys),
    integer(1),
    USE.NAMES = FALSE
  )
  mat$n_evidence_present <- vapply(
    genes,
    function(g) count_present(g, ev_keys),
    integer(1),
    USE.NAMES = FALSE
  )
  mat
}
