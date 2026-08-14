# Interpretive input agent - the optional FIRST stage (QA + derive context).
#
# Given the user's raw, multi-source candidate tokens plus their free-text study
# description, this proposes a cleaned input: per-token keep / correct / flag /
# drop decisions (fix typos and aliases, flag ambiguity, drop non-genes) and a
# proposed disease SEARCH TERM derived from the description. It is the front-of-
# pipeline mirror of R/curate.R and obeys the same contract:
#
#   * Grounding (a GeneScout non-negotiable): the agent may NEVER introduce a gene
#     the user did not provide. validate_input_curation() gates on the ORIGINAL
#     token (not the symbol), so a typo correction like KRSA -> KRAS survives
#     while a fabricated token is dropped.
#   * Human-in-the-loop: curate_input() returns a pure-data PROPOSAL; the user
#     reviews/edits it; confirm_input() applies the confirmed decisions and
#     returns a candidate_set. Only that confirmed set reaches run_enrich(), so
#     the deterministic pipeline downstream stays reproducible.
#   * Graceful fallback: with no LLM credentials (or on any error) the proposal
#     is an identity pass-through (ai_used = FALSE), so the app runs keyless.
#   * Network-free: curate_input() never resolves the disease over the network -
#     it only proposes a search term; resolve_proposed_disease() (called later,
#     at confirm time, by the UI/CLI/API) grounds it. This keeps the confirmed
#     set replayable.
#
# Provider/model come from config.yml via build_chat() (role "input_curator"),
# never hardcoded; `chat_factory` and `validator` are injectable for offline tests.

# --- Small helpers ----------------------------------------------------------

# Clean one source's raw tokens the way flatten does (trim; drop NA/blank/'#'
# comments) and de-duplicate case-insensitively, keeping the first spelling.
clean_input_tokens <- function(x) {
  toks <- trimws(as.character(x))
  toks <- toks[!is.na(toks) & nzchar(toks) & !startsWith(toks, "#")]
  toks[!duplicated(toupper(toks))]
}

# The unique cleaned tokens across every source - the grounding whitelist.
unique_input_tokens <- function(cs) {
  cs <- as_candidate_set(cs)
  all <- unlist(
    lapply(cs, function(s) clean_input_tokens(s$genes)),
    use.names = FALSE
  )
  all[!duplicated(toupper(all))]
}

# Source metadata (id, label, type) for rebuilding the candidate_set at confirm.
input_sources_meta <- function(cs) {
  cs <- as_candidate_set(cs)
  tibble::tibble(
    id = vapply(cs, function(s) s$id, character(1)),
    label = vapply(cs, function(s) s$label, character(1)),
    type = vapply(cs, function(s) s$type, character(1))
  )
}

empty_disease <- function() list(name = "", search_term = "")

# Sanitize the model's proposed disease into {name, search_term}. It is only ever
# a search TERM (never an ontology id, which the model would have to invent).
proposed_disease_from <- function(x) {
  if (is.null(x)) {
    return(empty_disease())
  }
  list(
    name = trimws(as.character(x$name %||% "")),
    search_term = trimws(as.character(x$search_term %||% ""))
  )
}

# Attach the standard reporting attributes to a proposal (mirrors
# curated_with_attrs). The proposal is a plain, classed list.
input_proposal_with_attrs <- function(
  p,
  ai_used,
  message = NULL,
  error = NULL
) {
  attr(p, "ai_used") <- isTRUE(ai_used)
  if (!is.null(message)) {
    attr(p, "message") <- message
  }
  if (!is.null(error)) {
    attr(p, "error") <- error
  }
  structure(p, class = "genescout_input_proposal")
}

empty_tokens_table <- function() {
  tibble::tibble(
    source_id = character(),
    source_label = character(),
    original = character(),
    symbol = character(),
    action = character(),
    reason = character(),
    confidence = numeric()
  )
}

# --- Schema + prompt --------------------------------------------------------

# Structured-output schema requested from the model.
input_curation_schema <- function() {
  ellmer::type_object(
    tokens = ellmer::type_array(
      items = ellmer::type_object(
        original = ellmer::type_string(
          "the user's input token, copied EXACTLY"
        ),
        symbol = ellmer::type_string(
          paste(
            "the official HGNC gene symbol (corrected if the original is a typo",
            "or alias); empty string if this token is not a gene"
          )
        ),
        action = ellmer::type_string(
          "exactly one of: keep, correct, flag, drop"
        ),
        reason = ellmer::type_string("a short reason for the decision"),
        confidence = ellmer::type_number(
          "confidence from 0 to 1 in this decision"
        )
      )
    ),
    proposed_disease = ellmer::type_object(
      name = ellmer::type_string(
        "human-readable disease/condition implied by the description, or empty"
      ),
      search_term = ellmer::type_string(
        paste(
          "a concise disease search term for the resolver (e.g.",
          "'neurofibromatosis type 1'), or empty if none is clearly implied"
        )
      )
    ),
    notes = ellmer::type_string("a brief overall interpretation summary")
  )
}

# Build the system + user prompts. The user block lists each named/typed source
# with its tokens, so the model has provenance and can reason per source.
build_input_prompt <- function(cs, description) {
  cs <- as_candidate_set(cs)
  src_blocks <- vapply(
    seq_along(cs),
    function(i) {
      s <- cs[[i]]
      toks <- clean_input_tokens(s$genes)
      paste0(
        sprintf("Source \"%s\" (type: %s):\n  ", s$label, s$type),
        if (length(toks)) paste(toks, collapse = ", ") else "(empty)"
      )
    },
    character(1)
  )
  user_prompt <- paste0(
    "Study description: ",
    if (is_blank(description)) "(none given)" else description,
    "\n\nThe user provided these candidate gene tokens, grouped by source:\n\n",
    paste(src_blocks, collapse = "\n"),
    "\n\nReview EVERY token exactly once. For each, choose keep / correct / flag",
    " / drop and give the official symbol (empty when dropping). Then, if the",
    " description clearly implies a disease, propose a concise search term for it."
  )
  list(system = read_prompt("input-curator"), user = user_prompt)
}

# Coerce ellmer structured tokens (a data frame or list-of-lists) to a tibble.
input_tokens_to_df <- function(sel) {
  if (is.null(sel)) {
    return(NULL)
  }
  if (is.data.frame(sel)) {
    return(tibble::as_tibble(sel))
  }
  dplyr::bind_rows(lapply(sel, function(x) {
    tibble::tibble(
      original = as.character(x$original %||% NA),
      symbol = as.character(x$symbol %||% NA),
      action = as.character(x$action %||% NA),
      reason = as.character(x$reason %||% NA),
      confidence = as.numeric(x$confidence %||% NA)
    )
  }))
}

# --- Grounding gate ---------------------------------------------------------

# Validate/clean the model's token decisions against the PROVIDED tokens. The
# gate keys on the ORIGINAL token, not the symbol, so a typo correction (whose
# symbol is legitimately off-list) survives while a fabricated token is dropped -
# the agent can never introduce a gene. Actions are coerced to the allowed set;
# a drop clears its symbol; a keep/correct with a blank symbol is downgraded to
# flag; and any provided token the model omitted is RECONCILED back in as a
# pass-through keep, so nothing the user gave is silently lost.
validate_input_curation <- function(df, provided) {
  allowed <- c("keep", "correct", "flag", "drop")
  provided <- as.character(provided)
  provided_up <- toupper(trimws(provided))

  clean <- empty_decisions()
  if (!is.null(df) && nrow(df) > 0 && "original" %in% names(df)) {
    df <- tibble::as_tibble(df)
    orig <- trimws(as.character(df$original))
    grounded <- toupper(orig) %in% provided_up # drop invented tokens
    df <- df[grounded, , drop = FALSE]
    orig <- orig[grounded]
    if (nrow(df) > 0) {
      action <- tolower(trimws(as.character(df$action %||% "")))
      action[!(action %in% allowed) | is.na(action)] <- "flag"
      symbol <- toupper(trimws(as.character(df$symbol %||% "")))
      # keep = the token is already a valid symbol; use the token itself.
      symbol[action == "keep"] <- toupper(orig[action == "keep"])
      symbol[action == "drop"] <- NA_character_
      blank <- is.na(symbol) | !nzchar(symbol)
      action[action == "correct" & blank] <- "flag"
      reason <- as.character(df$reason %||% "")
      reason[is.na(reason)] <- ""
      conf <- if ("confidence" %in% names(df)) {
        pmin(pmax(as.numeric(df$confidence), 0), 1)
      } else {
        NA_real_
      }
      clean <- tibble::tibble(
        original = orig,
        symbol = symbol,
        action = action,
        reason = reason,
        confidence = conf
      )
      clean <- clean[!duplicated(toupper(clean$original)), , drop = FALSE]
    }
  }

  # Reconcile: re-add any provided token the model omitted, as a pass-through.
  missing <- provided[!(provided_up %in% toupper(clean$original))]
  if (length(missing) > 0) {
    clean <- dplyr::bind_rows(
      clean,
      tibble::tibble(
        original = missing,
        symbol = toupper(missing),
        action = "keep",
        reason = "pass-through (not reviewed)",
        confidence = NA_real_
      )
    )
  }
  clean
}

empty_decisions <- function() {
  tibble::tibble(
    original = character(),
    symbol = character(),
    action = character(),
    reason = character(),
    confidence = numeric()
  )
}

# --- biogate seam (deferred) ------------------------------------------------

# The deterministic ID-validation seam. Returns NULL unless the sibling `biogate`
# package is installed, exactly like the ellmer guard - so biogate drops in by
# merely being present, with no code change and without being in DESCRIPTION.
# When present it validates/canonicalizes symbols (e.g. retired MLL -> KMT2A);
# it only ANNOTATES/NORMALIZES, never introduces a token.
default_input_validator <- function() {
  if (!requireNamespace("biogate", quietly = TRUE)) {
    return(NULL)
  }
  function(symbols, species = "human") {
    biogate::check_id(symbols, source_db = "hgnc", how = "cache")
  }
}

# Apply a validator to the proposed symbols at PROPOSE time: adopt a canonical /
# retired->current mapping as a `correct` the user then confirms. Applied here
# (not at confirm) so the user sees the change and confirm_input stays pure.
apply_input_validator <- function(decisions, validator) {
  if (is.null(validator) || nrow(decisions) == 0) {
    return(decisions)
  }
  idx <- which(!is.na(decisions$symbol) & nzchar(decisions$symbol))
  if (length(idx) == 0) {
    return(decisions)
  }
  checked <- tryCatch(validator(decisions$symbol[idx]), error = function(e) {
    NULL
  })
  if (is.null(checked) || !is.data.frame(checked)) {
    return(decisions)
  }
  for (k in seq_along(idx)) {
    if (k > nrow(checked)) {
      break
    }
    row <- checked[k, , drop = FALSE]
    canon <- toupper(trimws(as.character(
      row$normalized %||% row$suggestion %||% ""
    )))
    cur <- toupper(decisions$symbol[idx[k]])
    if (nzchar(canon) && canon != cur) {
      decisions$symbol[idx[k]] <- canon
      decisions$action[idx[k]] <- "correct"
      decisions$reason[idx[k]] <- paste0("normalized ", cur, " -> ", canon)
    }
  }
  decisions
}

# --- Proposal assembly ------------------------------------------------------

# Join the per-token decisions back onto every (source, token) pair, so the
# proposal preserves per-source membership AND is self-contained for confirm.
build_input_proposal <- function(cs, decisions, disease, notes) {
  cs <- as_candidate_set(cs)
  dec_key <- toupper(decisions$original)
  rows <- list()
  for (s in cs) {
    for (tok in clean_input_tokens(s$genes)) {
      j <- which(dec_key == toupper(tok))[1]
      d <- if (!is.na(j)) decisions[j, , drop = FALSE] else NULL
      rows[[length(rows) + 1]] <- tibble::tibble(
        source_id = s$id,
        source_label = s$label,
        original = tok,
        symbol = if (is.null(d)) toupper(tok) else d$symbol,
        action = if (is.null(d)) "keep" else d$action,
        reason = if (is.null(d)) "pass-through" else d$reason,
        confidence = if (is.null(d)) NA_real_ else d$confidence
      )
    }
  }
  tokens <- if (length(rows)) dplyr::bind_rows(rows) else empty_tokens_table()
  list(
    tokens = tokens,
    sources = input_sources_meta(cs),
    proposed_disease = disease,
    notes = as.character(notes %||% "")
  )
}

# Fallback proposal: identity pass-through (every token kept). No disease is
# derived without the LLM (disease derivation is inherently interpretive).
fallback_input <- function(cs) {
  cs <- as_candidate_set(cs)
  provided <- unique_input_tokens(cs)
  decisions <- tibble::tibble(
    original = provided,
    symbol = toupper(provided),
    action = rep("keep", length(provided)),
    reason = rep("pass-through (AI unavailable)", length(provided)),
    confidence = rep(NA_real_, length(provided))
  )
  build_input_proposal(
    cs,
    decisions,
    empty_disease(),
    "AI was unavailable; input passed through unchanged."
  )
}

# --- Main entry point -------------------------------------------------------

# Interpret + clean the user's multi-source input into a proposal. Returns a
# genescout_input_proposal: list(tokens, sources, proposed_disease, notes) with
# attributes ai_used (logical) and optionally message / error. NEVER resolves the
# disease over the network.
curate_input <- function(
  raw_sources,
  description = "",
  config = load_config(),
  validator = NULL,
  chat_factory = NULL
) {
  cs <- as_candidate_set(raw_sources)
  provided <- unique_input_tokens(cs)
  if (length(provided) == 0) {
    return(input_proposal_with_attrs(
      build_input_proposal(cs, empty_decisions(), empty_disease(), ""),
      ai_used = FALSE,
      message = "No input tokens to review."
    ))
  }
  validator <- validator %||% default_input_validator()

  if (is.null(chat_factory) && !genescout_llm_available(config)) {
    return(input_proposal_with_attrs(
      fallback_input(cs),
      ai_used = FALSE,
      message = "No LLM credentials set - input passed through unchanged."
    ))
  }
  if (is.null(chat_factory)) {
    chat_factory <- function(system_prompt) {
      build_chat(
        config$provider %||% "anthropic",
        model_for("input_curator", config),
        system_prompt,
        api_key = config$api_key
      )
    }
  }

  prompt <- build_input_prompt(cs, description)
  tryCatch(
    {
      chat <- chat_factory(prompt$system)
      raw <- chat$chat_structured(prompt$user, type = input_curation_schema())
      decisions <- validate_input_curation(
        input_tokens_to_df(raw$tokens),
        provided
      )
      decisions <- apply_input_validator(decisions, validator)
      input_proposal_with_attrs(
        build_input_proposal(
          cs,
          decisions,
          proposed_disease_from(raw$proposed_disease),
          raw$notes %||% ""
        ),
        ai_used = TRUE
      )
    },
    error = function(e) {
      input_proposal_with_attrs(
        fallback_input(cs),
        ai_used = FALSE,
        error = conditionMessage(e)
      )
    }
  )
}

# --- Confirm (pure) ---------------------------------------------------------

# Apply per-row user overrides to the token decisions. `edits` is a tibble/list
# of rows matched by source_id and/or original, with optional `action` / `symbol`
# to override. A user-set symbol is tagged (a user's own edit is user-provided
# input, allowed - only the AGENT is barred from introducing an off-token gene).
apply_input_edits <- function(tokens, edits) {
  if (is.null(edits) || NROW(edits) == 0) {
    return(tokens)
  }
  edits <- tibble::as_tibble(edits)
  for (i in seq_len(nrow(edits))) {
    e <- edits[i, , drop = FALSE]
    match <- rep(TRUE, nrow(tokens))
    if ("source_id" %in% names(edits) && !is.na(e$source_id)) {
      match <- match & tokens$source_id == e$source_id
    }
    if ("original" %in% names(edits) && !is.na(e$original)) {
      match <- match & toupper(tokens$original) == toupper(e$original)
    }
    if (!any(match)) {
      next
    }
    if ("action" %in% names(edits) && !is.na(e$action)) {
      tokens$action[match] <- tolower(trimws(as.character(e$action)))
    }
    if ("symbol" %in% names(edits) && !is.na(e$symbol)) {
      tokens$symbol[match] <- toupper(trimws(as.character(e$symbol)))
      tokens$reason[match] <- "user-edit"
    }
  }
  tokens
}

# Apply the (possibly user-edited) proposal and return the confirmed
# candidate_set that run_enrich() consumes. PURE - no network. Only keep/correct
# tokens are included (flag/drop are excluded); the applied decision table is
# recorded on the result as attr "decisions" for the audit trail. This is the
# reproducibility anchor: run_review(confirm_input(...)) is deterministic.
confirm_input <- function(proposal, edits = NULL) {
  tokens <- proposal$tokens %||% empty_tokens_table()
  meta <- proposal$sources %||% input_sources_meta(new_candidate_set(list()))
  if (nrow(tokens) == 0) {
    cs <- new_candidate_set(list())
    attr(cs, "decisions") <- tokens
    attr(cs, "confirmed") <- TRUE
    return(cs)
  }
  tokens <- apply_input_edits(tokens, edits)
  included <- tokens[tokens$action %in% c("keep", "correct"), , drop = FALSE]
  out_sources <- list()
  for (i in seq_len(nrow(meta))) {
    sid <- meta$id[i]
    genes <- included$symbol[included$source_id == sid]
    genes <- genes[!is.na(genes) & nzchar(genes)]
    if (length(genes) == 0) {
      next
    }
    out_sources[[length(out_sources) + 1L]] <- genescout_source(
      genes,
      label = meta$label[i],
      type = meta$type[i],
      id = sid
    )
  }
  cs <- new_candidate_set(out_sources)
  attr(cs, "decisions") <- tokens
  attr(cs, "confirmed") <- TRUE
  cs
}

# Ground a proposed disease search term into candidate ontology matches for the
# user to confirm. A NETWORK step, deliberately OUTSIDE curate_input, so the
# CLI/API/Shiny all share one propose -> confirm -> run disease path and the
# offline propose step stays deterministic.
resolve_proposed_disease <- function(search_term, limit = 5) {
  if (is_blank(search_term)) {
    return(list(ok = FALSE, error = "No disease search term to resolve."))
  }
  resolve_disease(search_term, limit = limit)
}
