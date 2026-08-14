# AI curation - the final subjective compaction step (the LAST stage).
#
# The deterministic pipeline ranks every candidate; this optional step asks the
# configured LLM to filter/curate the ranked list down to a usable, defensible
# set for the specific study, with a grounded one-line rationale per gene. It is
# a final polish, never a hard dependency: with no credentials (or on any error)
# it falls back to the top genes by composite rank, so the app always works.
#
# Grounding (a GeneScout non-negotiable): the gene SELECTION is structurally gated -
# validate_curation() drops any symbol not in the ranked candidate set, so the
# model cannot introduce a gene. The one-line rationale is model-written and only
# prompt-instructed to stay on the shown evidence, so it is presented as an AI
# summary, not as a separately citation-gated evidence item; the grounded,
# source-linked evidence remains in the ranked table and per-gene drill-down.
# Provider/model come from config.yml via build_chat(), never hardcoded, so
# switching providers is a config change. `chat_factory` is injectable so tests
# run without the network.

# Default target size for the curated list when the caller (UI/CLI) names none.
GENESCOUT_CURATE_TARGET_DEFAULT <- 30L

empty_curated_table <- function() {
  tibble::tibble(
    gene_symbol = character(),
    include = logical(),
    confidence = numeric(),
    rationale = character(),
    # The grounded evidence ids (accessions / PMIDs) the model cited for this
    # decision, filtered to ids that actually appear in the gene's evidence. A
    # list-column: one character vector per row (possibly empty).
    source_ids = list()
  )
}

# Attach the standard reporting attributes to a curated tibble.
curated_with_attrs <- function(df, ai_used, message = NULL, error = NULL) {
  attr(df, "ai_used") <- isTRUE(ai_used)
  if (!is.null(message)) {
    attr(df, "message") <- message
  }
  if (!is.null(error)) {
    attr(df, "error") <- error
  }
  df
}

# Structured-output schema requested from the model.
curation_schema <- function() {
  ellmer::type_object(
    selections = ellmer::type_array(
      items = ellmer::type_object(
        gene_symbol = ellmer::type_string(
          "gene symbol, copied EXACTLY from the candidate list"
        ),
        include = ellmer::type_boolean(
          "TRUE if this gene belongs in the final curated list for the study"
        ),
        confidence = ellmer::type_number(
          "confidence from 0 to 1 in this include/exclude decision"
        ),
        rationale = ellmer::type_string(
          "one short sentence justifying the decision FROM THE EVIDENCE SHOWN"
        ),
        source_ids = ellmer::type_array(
          items = ellmer::type_string(
            paste(
              "one evidence id (accession / PMID / database id) copied EXACTLY",
              "from inside the square brackets on an evidence line shown for THIS",
              "gene, that supports the rationale. Cite only ids shown for this gene."
            )
          )
        )
      )
    ),
    overall_notes = ellmer::type_string(
      "brief summary of the curation strategy and any caveats"
    )
  )
}

# The top `n` candidate genes (by rank) as the curation shortlist.
curation_candidates <- function(result, n) {
  g <- result$genes
  g <- g[order(g$rank), , drop = FALSE]
  utils::head(g, n)
}

# A short disease label for the prompt, or "(none)".
curation_disease_label <- function(result) {
  d <- pluck_at(result, "context", "disease")
  if (is.null(d)) {
    return("(none)")
  }
  d$name %||% d$id %||% "(unspecified)"
}

# Render the candidate genes as a compact, grounded text block: per gene, its
# composite/grade, its per-signal values, and up to a few grounded evidence lines
# (each with its source id) so the model reasons only from what is shown.
curation_candidates_text <- function(result, candidates, max_evidence = 6) {
  reg <- result$registry
  ev <- result$evidence
  blocks <- vapply(
    seq_len(nrow(candidates)),
    function(i) {
      row <- candidates[i, , drop = FALSE]
      sig_bits <- vapply(
        seq_len(nrow(reg)),
        function(j) {
          v <- row[[reg$key[j]]]
          if (is.null(v) || is.na(v)) {
            return(NA_character_)
          }
          sprintf("%s=%s", reg$label[j], format_signal_value(v))
        },
        character(1)
      )
      sig_bits <- sig_bits[!is.na(sig_bits)]
      ev_g <- ev[ev$gene_id == row$gene_id, , drop = FALSE]
      ev_g <- utils::head(ev_g, max_evidence)
      ev_lines <- if (nrow(ev_g) > 0) {
        paste0(
          "    - ",
          ev_g$title,
          " [",
          ev_g$source_id,
          "]",
          collapse = "\n"
        )
      } else {
        "    - (no grounded evidence)"
      }
      paste0(
        sprintf(
          "%d. %s (composite %.3f, %s)\n",
          row$rank,
          row$symbol,
          row$composite,
          row$grade
        ),
        "    signals: ",
        paste(sig_bits, collapse = "; "),
        "\n",
        ev_lines
      )
    },
    character(1)
  )
  paste(blocks, collapse = "\n\n")
}

# Build the system + user prompts.
build_curation_prompt <- function(result, candidates, top_n) {
  system_prompt <- paste(
    "You are an expert biomedical curator compiling a compact, defensible gene",
    "list for a specific research study. You are given candidate genes already",
    "ranked by a transparent multi-source composite, each with the grounded",
    "evidence behind its signals. Select the genes that genuinely belong in the",
    "final list for THIS study.",
    "Hard rules: choose ONLY from the provided candidate list; never invent,",
    "rename, or merge symbols; base every rationale ONLY on the evidence shown",
    "for that gene and introduce no fact that is not listed; be conservative and",
    "prefer genes with broad, specific, well-grounded support. For EVERY gene,",
    "populate source_ids with the exact evidence ids that back your rationale -",
    "each is shown inside the square brackets at the end of an evidence line for",
    "that gene (e.g. a PMID, a ClinVar or Open Targets accession). Copy them",
    "verbatim and cite only ids listed for that gene; an ungrounded rationale is",
    "unacceptable. Aim for roughly",
    top_n,
    "included genes unless the evidence clearly supports fewer or more."
  )
  user_prompt <- paste0(
    "Study context: ",
    if (is_blank(result$description)) "(none given)" else result$description,
    "\nDisease context: ",
    curation_disease_label(result),
    "\n\nCandidate genes (best first), with grounded evidence:\n\n",
    curation_candidates_text(result, candidates),
    "\n\nReturn your curated selection."
  )
  list(system = system_prompt, user = user_prompt)
}

# Normalize a model-returned source_ids field (a vector, a list, or NULL) to a
# clean character vector of non-blank ids.
normalize_source_ids <- function(x) {
  ids <- trimws(as.character(unlist(x, use.names = FALSE)))
  ids[!is.na(ids) & nzchar(ids)]
}

# Coerce ellmer structured output (a data frame or list-of-lists) to a tibble,
# always carrying a `source_ids` list-column (empty when the model cited none).
selections_to_df <- function(sel) {
  if (is.null(sel)) {
    return(NULL)
  }
  if (is.data.frame(sel)) {
    df <- tibble::as_tibble(sel)
    df$source_ids <- if ("source_ids" %in% names(df)) {
      lapply(df$source_ids, normalize_source_ids)
    } else {
      replicate(nrow(df), character(), simplify = FALSE)
    }
    return(df)
  }
  dplyr::bind_rows(lapply(sel, function(x) {
    tibble::tibble(
      gene_symbol = as.character(x$gene_symbol %||% NA),
      include = as.logical(x$include %||% NA),
      confidence = as.numeric(x$confidence %||% NA),
      rationale = as.character(x$rationale %||% NA),
      source_ids = list(normalize_source_ids(x$source_ids))
    )
  }))
}

# Map each candidate gene (by uppercased symbol) to the set of grounded evidence
# ids actually gathered for it - the ONLY ids a curation rationale may cite. Built
# from result$evidence (source_id per gene_id), keyed by the candidate's symbol.
curation_evidence_ids <- function(result, candidates) {
  ev <- result$evidence
  out <- list()
  if (is.null(ev) || nrow(ev) == 0 || is.null(candidates)) {
    return(out)
  }
  for (i in seq_len(nrow(candidates))) {
    ids <- unique(ev$source_id[ev$gene_id == candidates$gene_id[i]])
    ids <- ids[!is.na(ids) & nzchar(ids)]
    out[[toupper(candidates$symbol[i])]] <- ids
  }
  out
}

# Validate/clean the model's selections against the candidate set: drop
# hallucinated symbols (the grounding gate), clamp confidence, default missing
# fields, de-duplicate, and GROUND the cited source_ids - keeping only ids that
# actually appear in that gene's evidence (`evidence_ids`, a symbol -> id-vector
# map from curation_evidence_ids). So a rationale can never cite a paper/accession
# that was not in the evidence shown. `candidate_symbols` is the shortlist shown.
validate_curation <- function(df, candidate_symbols, evidence_ids = list()) {
  if (is.null(df) || nrow(df) == 0 || !"gene_symbol" %in% names(df)) {
    return(empty_curated_table())
  }
  df <- tibble::as_tibble(df)
  sym <- toupper(trimws(as.character(df$gene_symbol)))
  keep <- sym %in% toupper(candidate_symbols)

  include <- if ("include" %in% names(df)) as.logical(df$include) else TRUE
  include[is.na(include)] <- FALSE
  conf <- if ("confidence" %in% names(df)) {
    pmin(pmax(as.numeric(df$confidence), 0), 1)
  } else {
    NA_real_
  }
  rat <- if ("rationale" %in% names(df)) as.character(df$rationale) else ""
  rat[is.na(rat)] <- ""

  raw_ids <- if ("source_ids" %in% names(df)) {
    df$source_ids
  } else {
    vector("list", nrow(df))
  }
  grounded <- lapply(seq_len(nrow(df)), function(i) {
    cited <- unique(normalize_source_ids(raw_ids[[i]]))
    valid <- evidence_ids[[sym[i]]]
    if (is.null(valid)) character() else cited[cited %in% valid]
  })

  out <- tibble::tibble(
    gene_symbol = sym,
    include = include,
    confidence = conf,
    rationale = rat,
    source_ids = grounded
  )[keep, , drop = FALSE]
  out[!duplicated(out$gene_symbol), , drop = FALSE]
}

# Fallback when the LLM is unavailable or fails: top genes by composite rank. Even
# here the source_ids are grounded - each gene's own gathered evidence ids (a few)
# - so the deterministic path carries the same citation trail as the AI path.
fallback_curation <- function(result, top_n) {
  genes <- result$genes
  if (is.null(genes) || nrow(genes) == 0) {
    return(empty_curated_table())
  }
  picked <- utils::head(genes[order(genes$rank), , drop = FALSE], top_n)
  ev_ids <- curation_evidence_ids(result, picked)
  tibble::tibble(
    gene_symbol = picked$symbol,
    include = TRUE,
    confidence = NA_real_,
    rationale = "Selected by composite rank (AI unavailable).",
    source_ids = lapply(
      toupper(picked$symbol),
      function(s) utils::head(ev_ids[[s]] %||% character(), 6L)
    )
  )
}

# Cap the curated selection to the requested target size. The deterministic rank
# already shortlists the pool the model sees; if the model still includes MORE than
# `n` genes, keep the `n` most-confident and mark the rest excluded, so the final
# list honors the user's ceiling (the evidence can still justify fewer, never more).
cap_curation <- function(df, n) {
  if (is.null(df) || nrow(df) == 0 || !"include" %in% names(df)) {
    return(df)
  }
  inc <- which(df$include)
  if (length(inc) <= n) {
    return(df)
  }
  conf <- if ("confidence" %in% names(df)) {
    df$confidence[inc]
  } else {
    rep(NA_real_, length(inc))
  }
  keep <- inc[order(conf, decreasing = TRUE, na.last = TRUE)][seq_len(n)]
  df$include[setdiff(inc, keep)] <- FALSE
  df
}

# Main entry point. Curate a run_review() result down to a usable set of ~`top_n`
# genes: the deterministic rank shortlists a pool (`pool_n`, default a multiple of the
# target), the model filters that pool, and the included set is capped to `top_n`.
# Returns a curated tibble(gene_symbol, include, confidence, rationale, source_ids)
# with attributes `ai_used` (logical) and, optionally, `message` / `error`.
curate_gene_list <- function(
  result,
  config = load_config(),
  top_n = GENESCOUT_CURATE_TARGET_DEFAULT,
  pool_n = NULL,
  chat_factory = NULL
) {
  top_n <- max(1L, as.integer(top_n))
  # The rank pre-shortlist the model chooses from - wide enough to see real
  # alternatives, bounded so the prompt (and cost) stay sane.
  pool_n <- if (is.null(pool_n)) max(top_n * 3L, 60L) else as.integer(pool_n)
  genes <- result$genes
  if (is.null(genes) || nrow(genes) == 0) {
    return(curated_with_attrs(
      empty_curated_table(),
      ai_used = FALSE,
      message = "No ranked genes to curate."
    ))
  }
  candidates <- curation_candidates(result, max(pool_n, top_n))
  cand_symbols <- toupper(candidates$symbol)
  # The per-gene grounded evidence ids the model is allowed to cite (its rationale
  # source_ids are filtered to this set - the citation grounding gate).
  evidence_ids <- curation_evidence_ids(result, candidates)

  if (is.null(chat_factory) && !genescout_llm_available(config)) {
    return(curated_with_attrs(
      fallback_curation(result, top_n),
      ai_used = FALSE,
      message = "No LLM credentials set - selected top genes by composite rank."
    ))
  }
  if (is.null(chat_factory)) {
    chat_factory <- function(system_prompt) {
      build_chat(
        config$provider %||% "anthropic",
        model_for("orchestrator", config),
        system_prompt,
        api_key = config$api_key
      )
    }
  }

  prompt <- build_curation_prompt(result, candidates, top_n)
  out <- tryCatch(
    {
      chat <- chat_factory(prompt$system)
      raw <- chat$chat_structured(prompt$user, type = curation_schema())
      curated_with_attrs(
        cap_curation(
          validate_curation(
            selections_to_df(raw$selections),
            cand_symbols,
            evidence_ids
          ),
          top_n
        ),
        ai_used = TRUE
      )
    },
    error = function(e) {
      curated_with_attrs(
        fallback_curation(result, top_n),
        ai_used = FALSE,
        error = conditionMessage(e)
      )
    }
  )

  # The AI succeeded but returned nothing usable: fall back to rank.
  if (
    isTRUE(attr(out, "ai_used")) &&
      (nrow(out) == 0 || !any(out$include, na.rm = TRUE))
  ) {
    return(curated_with_attrs(
      fallback_curation(result, top_n),
      ai_used = FALSE,
      message = "The model returned no usable selection - fell back to rank."
    ))
  }
  out
}
