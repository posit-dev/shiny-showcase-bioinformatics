# Agentic specialists - the OPTIONAL LLM synthesis layer over the deterministic,
# already-grounded evidence. This runs AFTER ranking (like the AI curator) and
# never replaces it: the deterministic ranking is the spine, and same inputs still
# give the same ranking. For the top candidates, three specialists
# (variant-effect, pathway-disease, literature) each read that gene's grounded
# evidence for their domains and return a structured synthesis + a suggested next
# experiment.
#
# Grounding (a GeneScout non-negotiable): every specialist FINDING must cite evidence
# ids that actually appear in the gene's gathered evidence - a fabricated citation
# is dropped, and a finding left with no grounded id is dropped entirely (it would
# be an ungrounded claim). The narrative `assessment` is a model summary of the
# same shown evidence, presented as such. The specialists never fetch new data;
# they interpret what the deterministic pipeline already grounded, so the citation
# guarantee is preserved by construction.
#
# Provider/model come from config.yml (the `specialist` role) via build_chat(),
# never hardcoded. `runner` is injectable so tests run offline with no network.

# The three specialists: each owns a set of evidence domains (produced by the
# signal extractors, R/enrich.R) and a short focus used in its prompt. Domains not
# claimed by any specialist (interaction, input-provenance) are cohort-relative
# annotations, not per-gene biological evidence, so no specialist synthesizes them.
genescout_specialists <- function() {
  list(
    list(
      key = "variant-effect",
      label = "Variant & genomic alteration",
      domains = c("variant-effect", "constraint", "cancer"),
      focus = paste(
        "functional/clinical variant significance, population constraint, and",
        "somatic alteration frequency"
      )
    ),
    list(
      key = "pathway-disease",
      label = "Pathway & disease",
      domains = c("pathway-disease", "gene-disease", "expression"),
      focus = paste(
        "gene-disease association, pathway membership, phenotype overlap, and",
        "tissue expression"
      )
    ),
    list(
      key = "literature",
      label = "Literature & translation",
      domains = c("literature", "druggability"),
      focus = "published support and translational / druggability context"
    )
  )
}

# The structured output each specialist returns for one gene.
specialist_schema <- function() {
  ellmer::type_object(
    assessment = ellmer::type_string(
      paste(
        "2-3 sentence synthesis of what THIS domain's evidence says about the",
        "gene for the study, using ONLY the evidence shown"
      )
    ),
    findings = ellmer::type_array(
      items = ellmer::type_object(
        point = ellmer::type_string(
          "one specific point grounded in the evidence shown for this gene"
        ),
        source_ids = ellmer::type_array(
          items = ellmer::type_string(
            "an evidence id copied EXACTLY from the [brackets] supporting this point"
          )
        )
      )
    ),
    strength = ellmer::type_string(
      "domain support for this gene: one of strong, moderate, weak, none"
    ),
    next_experiment = ellmer::type_string(
      paste(
        "one concrete next experiment or analysis that would resolve the biggest",
        "remaining uncertainty in THIS domain; empty string if none is warranted"
      )
    )
  )
}

# The top `n` candidate genes (by rank) that specialists analyze. When
# `restrict_to` (a vector of symbols, e.g. the AI-curated set) is given, the pool is
# first narrowed to those genes so specialists run on the curated shortlist's top n,
# not the raw rank's - an empty/absent restrict_to keeps the whole ranked set.
specialist_candidates <- function(result, n, restrict_to = NULL) {
  g <- result$genes
  if (!is.null(restrict_to) && length(restrict_to) > 0) {
    g <- g[toupper(g$symbol) %in% toupper(restrict_to), , drop = FALSE]
  }
  g <- g[order(g$rank), , drop = FALSE]
  utils::head(g, n)
}

# The grounded evidence ids available for one gene within a specialist's domains -
# the only ids a finding may cite.
specialist_evidence_ids <- function(evidence, gene_id, domains) {
  if (is.null(evidence) || nrow(evidence) == 0) {
    return(character())
  }
  keep <- evidence$gene_id == gene_id & evidence$domain %in% domains
  ids <- unique(evidence$source_id[keep])
  ids[!is.na(ids) & nzchar(ids)]
}

# Render a gene's domain evidence as grounded, citable lines (each ends with its
# [source_id]), capped so the prompt stays compact.
specialist_evidence_text <- function(
  evidence,
  gene_id,
  domains,
  max_rows = 12
) {
  ev <- evidence[
    evidence$gene_id == gene_id & evidence$domain %in% domains,
    ,
    drop = FALSE
  ]
  if (nrow(ev) == 0) {
    return("")
  }
  ev <- utils::head(ev, max_rows)
  detail <- ifelse(
    is.na(ev$detail) | !nzchar(ev$detail),
    "",
    paste0(" - ", ev$detail)
  )
  paste0(
    "- [",
    ev$domain,
    "] ",
    ev$title,
    detail,
    " [",
    ev$source_id,
    "]",
    collapse = "\n"
  )
}

# System prompt for a specialist (the synthesis contract - it interprets shown
# evidence, never fetches or recalls).
specialist_system_prompt <- function(specialist) {
  paste(
    "You are the",
    specialist$label,
    "specialist in a research-genomics evidence review. You are given a candidate",
    "gene and the grounded evidence already retrieved for it in your domain:",
    paste0(specialist$focus, "."),
    "Synthesize ONLY that evidence.",
    "Hard rules: use no fact that is not in the evidence shown; for every finding,",
    "cite in source_ids the exact evidence ids shown in [brackets] for that point,",
    "copied verbatim; if the evidence is thin or absent, say so and set strength",
    "accordingly (do not inflate). Treat ClinVar/gnomAD as research evidence, never",
    "a clinical or pathogenicity call. Keep it concise and specific to THIS study."
  )
}

# User prompt for one gene.
specialist_user_prompt <- function(specialist, gene_row, evidence, context) {
  disease <- pluck_at(context, "disease")
  disease_lbl <- if (is.null(disease)) {
    "(none)"
  } else {
    disease$name %||% disease$id %||% "(unspecified)"
  }
  paste0(
    "Disease context: ",
    disease_lbl,
    "\nGene: ",
    gene_row$symbol,
    sprintf(
      " (composite %.3f, grade %s)",
      gene_row$composite,
      gene_row$grade
    ),
    "\n\nGrounded ",
    specialist$label,
    " evidence (the ONLY evidence you may use):\n",
    specialist_evidence_text(evidence, gene_row$gene_id, specialist$domains),
    "\n\nReturn your structured domain assessment."
  )
}

# Normalize a model-returned source_ids field to a clean character vector.
specialist_norm_ids <- function(x) {
  ids <- trimws(as.character(unlist(x, use.names = FALSE)))
  ids[!is.na(ids) & nzchar(ids)]
}

# Clean one specialist result for one gene: coerce fields, GROUND each finding's
# cited ids to `valid` (the gene's real domain evidence ids), and drop any finding
# left with no grounded id. Returns a tidy list or NULL if nothing survives.
clean_specialist_result <- function(raw, valid) {
  if (is.null(raw)) {
    return(NULL)
  }
  strength <- tolower(trimws(as.character(raw$strength %||% "")))
  if (!strength %in% c("strong", "moderate", "weak", "none")) {
    strength <- "moderate"
  }
  findings_raw <- raw$findings %||% list()
  if (is.data.frame(findings_raw)) {
    findings_raw <- lapply(
      seq_len(nrow(findings_raw)),
      function(i) as.list(findings_raw[i, , drop = FALSE])
    )
  }
  findings <- list()
  for (f in findings_raw) {
    cited <- unique(specialist_norm_ids(f$source_ids))
    grounded <- cited[cited %in% valid]
    point <- trimws(as.character(f$point %||% ""))
    # A finding must be grounded: no grounded id -> drop it (never an ungrounded claim).
    if (length(grounded) == 0 || !nzchar(point)) {
      next
    }
    findings[[length(findings) + 1L]] <- list(
      point = point,
      source_ids = grounded
    )
  }
  assessment <- trimws(as.character(raw$assessment %||% ""))
  next_exp <- trimws(as.character(raw$next_experiment %||% ""))
  if (length(findings) == 0 && !nzchar(assessment)) {
    return(NULL)
  }
  list(
    assessment = assessment,
    findings = findings,
    strength = strength,
    next_experiment = next_exp
  )
}

# Fan per-item prompts out concurrently with ellmer::parallel_chat_structured on a
# chat built for the given config `role` (specialist / orchestrator), returning a
# list of raw result objects (one per prompt). Injected/overridden in tests so no
# network is touched.
run_parallel_structured <- function(
  system_prompt,
  user_prompts,
  schema,
  config,
  role
) {
  chat <- build_chat(
    config$provider %||% "anthropic",
    model_for(role, config),
    system_prompt,
    api_key = config$api_key
  )
  out <- ellmer::parallel_chat_structured(
    chat,
    as.list(user_prompts),
    type = schema
  )
  coerce_parallel_rows(out)
}

# The default specialist runner (the `specialist` model role).
default_specialist_runner <- function(
  system_prompt,
  user_prompts,
  schema,
  config
) {
  run_parallel_structured(
    system_prompt,
    user_prompts,
    schema,
    config,
    "specialist"
  )
}

# parallel_chat_structured returns a data frame (one row per prompt) for a
# type_object, with nested arrays (findings, source_ids) as LIST-columns. Coerce
# each row to a named list, taking a list-column's OWN element (not a length-1
# wrapper) so `findings` is the row's findings, not a list containing them. A
# non-data-frame result (already a list-of-rows) passes through unchanged.
coerce_parallel_rows <- function(out) {
  if (!is.data.frame(out)) {
    return(out)
  }
  cols <- names(out)
  lapply(seq_len(nrow(out)), function(i) {
    stats::setNames(
      lapply(cols, function(col) {
        v <- out[[col]]
        if (is.list(v)) v[[i]] else v[i]
      }),
      cols
    )
  })
}

# Main entry point. Run the specialists over the top candidates of a ranked
# result. Returns
#   list(ai_used = <lgl>, message = <chr|NULL>,
#        by_gene = <named-by-UPPER-symbol list of
#                     specialist_key -> {label, assessment, findings, strength,
#                                        next_experiment}>)
# `runner` is injectable; when NULL and no LLM is configured, returns ai_used =
# FALSE with a message (specialists are optional, so the app still works).
run_specialists <- function(
  result,
  config = load_config(),
  top_n = 10,
  runner = NULL,
  specialists = genescout_specialists(),
  synthesize = TRUE,
  synth_runner = NULL,
  restrict_to = NULL
) {
  genes <- result$genes
  if (is.null(genes) || nrow(genes) == 0) {
    return(list(
      ai_used = FALSE,
      message = "No ranked genes to analyze.",
      by_gene = list()
    ))
  }
  if (is.null(runner) && !genescout_llm_available(config)) {
    return(list(
      ai_used = FALSE,
      message = "No LLM credentials set - specialist analysis is unavailable.",
      by_gene = list()
    ))
  }
  # Remember whether the caller injected a specialist runner: if so, we do NOT
  # invent a live synthesis call (tests inject a specialist runner without a
  # synth_runner and must stay offline). The live app injects neither and gets both.
  injected_runner <- !is.null(runner)
  if (is.null(runner)) {
    runner <- function(system_prompt, user_prompts, schema) {
      default_specialist_runner(system_prompt, user_prompts, schema, config)
    }
  }
  candidates <- specialist_candidates(result, top_n, restrict_to = restrict_to)
  evidence <- result$evidence
  context <- result$context %||% list()
  # The schema is only needed by the live ellmer runner; tolerate ellmer being
  # absent so an injected runner (and the offline tests) never require it.
  schema <- tryCatch(specialist_schema(), error = function(e) NULL)

  by_gene <- list()
  err <- NULL
  for (spec in specialists) {
    # Only genes with at least one evidence row in this specialist's domains get a
    # prompt (no point asking a specialist to synthesize nothing).
    idx <- which(vapply(
      seq_len(nrow(candidates)),
      function(i) {
        length(specialist_evidence_ids(
          evidence,
          candidates$gene_id[i],
          spec$domains
        )) >
          0
      },
      logical(1)
    ))
    if (length(idx) == 0) {
      next
    }
    prompts <- vapply(
      idx,
      function(i) {
        specialist_user_prompt(
          spec,
          candidates[i, , drop = FALSE],
          evidence,
          context
        )
      },
      character(1)
    )
    raw_list <- tryCatch(
      runner(specialist_system_prompt(spec), prompts, schema),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(raw_list)) {
      next
    }
    for (j in seq_along(idx)) {
      i <- idx[j]
      valid <- specialist_evidence_ids(
        evidence,
        candidates$gene_id[i],
        spec$domains
      )
      cleaned <- clean_specialist_result(raw_list[[j]], valid)
      if (is.null(cleaned)) {
        next
      }
      key <- toupper(candidates$symbol[i])
      cleaned$label <- spec$label
      if (is.null(by_gene[[key]])) {
        by_gene[[key]] <- list()
      }
      by_gene[[key]][[spec$key]] <- cleaned
    }
  }

  ai_used <- length(by_gene) > 0

  # Synthesis stage: roll each gene's specialists into one grounded verdict. Skipped
  # when a specialist runner was injected without a synth_runner (offline tests), so
  # only the live app (neither injected) or an explicit synth_runner triggers it.
  do_synth <- isTRUE(synthesize) &&
    ai_used &&
    (!is.null(synth_runner) || !injected_runner)
  if (do_synth) {
    sr <- synth_runner %||%
      function(system_prompt, user_prompts, schema) {
        run_parallel_structured(
          system_prompt,
          user_prompts,
          schema,
          config,
          "orchestrator"
        )
      }
    by_gene <- tryCatch(
      run_synthesis(by_gene, context, sr),
      error = function(e) by_gene
    )
  }

  msg <- if (!ai_used && !is.null(err)) {
    paste("Specialist analysis failed:", err)
  } else if (!ai_used) {
    "The specialists returned no grounded findings for the top candidates."
  } else {
    NULL
  }
  list(ai_used = ai_used, message = msg, by_gene = by_gene)
}

# --- Synthesis: roll the three domain specialists into one per-gene verdict -----
# An orchestrator stage that integrates a gene's specialist assessments into ONE
# grounded verdict: an overall read, a research plausibility level, the key
# cross-domain caveats, and the single highest-priority next experiment. It cites
# only ids the specialists already grounded (grounding chains through), and it is a
# research plausibility read for prioritization - never a clinical/pathogenicity call.

synthesis_schema <- function() {
  ellmer::type_object(
    verdict = ellmer::type_string(
      paste(
        "2-3 sentence integrated read of how plausibly this gene is a real",
        "candidate for THIS study, from the domain assessments only"
      )
    ),
    plausibility = ellmer::type_string(
      "overall research plausibility for the study: one of compelling, plausible, uncertain, weak"
    ),
    caveats = ellmer::type_array(
      items = ellmer::type_string(
        "a key caveat, uncertainty, or cross-domain conflict"
      )
    ),
    next_experiment = ellmer::type_string(
      "the single highest-priority next experiment or analysis across all domains"
    ),
    source_ids = ellmer::type_array(
      items = ellmer::type_string(
        "an evidence id copied EXACTLY from the [brackets] that the verdict leans on"
      )
    )
  )
}

synthesis_system_prompt <- function() {
  paste(
    "You are the orchestrator of a research-genomics evidence review. You are given",
    "one candidate gene and the assessments of three domain specialists (variant,",
    "pathway/disease, literature), each already grounded in retrieved evidence.",
    "Integrate them into ONE verdict for the study.",
    "Hard rules: use only what the specialists reported; cite in source_ids the exact",
    "ids shown in [brackets]; weigh breadth and specificity, and be explicit about",
    "weak, conflicting, or missing support (do not smooth over gaps). This is a",
    "research plausibility read for prioritization - never a diagnosis, clinical, or",
    "pathogenicity call."
  )
}

# Render a gene's specialist cards as the orchestrator's grounded input.
synthesis_user_prompt <- function(symbol, gene_specialists, context) {
  disease <- pluck_at(context, "disease")
  disease_lbl <- if (is.null(disease)) {
    "(none)"
  } else {
    disease$name %||% disease$id %||% "(unspecified)"
  }
  ordered <- intersect(GENESCOUT_SPECIALIST_ORDER, names(gene_specialists))
  blocks <- vapply(
    ordered,
    function(k) {
      sp <- gene_specialists[[k]]
      finding_lines <- if (length(sp$findings) > 0) {
        paste0(
          "    - ",
          vapply(
            sp$findings,
            function(f) {
              paste0(f$point, " [", paste(f$source_ids, collapse = ", "), "]")
            },
            character(1)
          ),
          collapse = "\n"
        )
      } else {
        "    - (no grounded findings)"
      }
      nxt <- if (nzchar(sp$next_experiment %||% "")) {
        paste0("\n    next: ", sp$next_experiment)
      } else {
        ""
      }
      paste0(
        sp$label,
        " (strength ",
        sp$strength,
        ")\n  ",
        sp$assessment %||% "",
        "\n",
        finding_lines,
        nxt
      )
    },
    character(1)
  )
  paste0(
    "Disease context: ",
    disease_lbl,
    "\nGene: ",
    symbol,
    "\n\nSpecialist assessments:\n\n",
    paste(blocks, collapse = "\n\n"),
    "\n\nReturn your integrated verdict."
  )
}

# The union of grounded ids a gene's specialists cited - the only ids the verdict
# may cite (grounding chains through the specialists to the verdict).
gene_grounded_ids <- function(gene_specialists) {
  ordered <- intersect(GENESCOUT_SPECIALIST_ORDER, names(gene_specialists))
  ids <- unlist(
    lapply(ordered, function(k) {
      unlist(
        lapply(gene_specialists[[k]]$findings, function(f) f$source_ids),
        use.names = FALSE
      )
    }),
    use.names = FALSE
  )
  unique(ids[!is.na(ids) & nzchar(ids)])
}

# Clean one synthesis result: normalize plausibility, clean caveats, and GROUND the
# cited ids to `valid` (the gene's specialist-grounded ids). NULL if no verdict text.
clean_synthesis_result <- function(raw, valid) {
  if (is.null(raw)) {
    return(NULL)
  }
  plaus <- tolower(trimws(as.character(raw$plausibility %||% "")))
  if (!plaus %in% c("compelling", "plausible", "uncertain", "weak")) {
    plaus <- "uncertain"
  }
  verdict <- trimws(as.character(raw$verdict %||% ""))
  if (!nzchar(verdict)) {
    return(NULL)
  }
  cited <- unique(specialist_norm_ids(raw$source_ids))
  list(
    verdict = verdict,
    plausibility = plaus,
    # specialist_norm_ids is a generic char-vector cleaner (trims, drops blanks).
    caveats = specialist_norm_ids(raw$caveats),
    next_experiment = trimws(as.character(raw$next_experiment %||% "")),
    source_ids = cited[cited %in% valid]
  )
}

# Add a grounded verdict to each analyzed gene. `by_gene` is run_specialists()'s
# structure; `runner(system_prompt, user_prompts, schema)` is injectable. Genes are
# synthesized in parallel; a gene whose verdict is empty is left without one.
run_synthesis <- function(by_gene, context = list(), runner) {
  keys <- names(by_gene)
  if (length(keys) == 0) {
    return(by_gene)
  }
  schema <- tryCatch(synthesis_schema(), error = function(e) NULL)
  prompts <- vapply(
    keys,
    function(k) synthesis_user_prompt(k, by_gene[[k]], context),
    character(1)
  )
  raw_list <- runner(synthesis_system_prompt(), prompts, schema)
  for (j in seq_along(keys)) {
    valid <- gene_grounded_ids(by_gene[[keys[j]]])
    cleaned <- clean_synthesis_result(raw_list[[j]], valid)
    if (!is.null(cleaned)) {
      by_gene[[keys[j]]]$verdict <- cleaned
    }
  }
  by_gene
}
