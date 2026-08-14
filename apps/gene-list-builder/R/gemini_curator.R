# AI curation of the ranked gene list (Google Gemini via ellmer) ---------------
#
# The AI step is a final-polish layer, not a hard dependency: without a
# GEMINI_API_KEY (or on any failure) it falls back to selecting the top genes by
# combined rank, so the app stays fully functional.

gemini_available <- function() {
  nzchar(Sys.getenv("GEMINI_API_KEY"))
}

empty_curated_table <- function() {
  tibble::tibble(
    gene_symbol = character(),
    include = logical(),
    confidence = numeric(),
    rationale = character()
  )
}

# Structured-output schema requested from the model.
curation_schema <- function() {
  ellmer::type_object(
    selections = ellmer::type_array(
      items = ellmer::type_object(
        gene_symbol = ellmer::type_string(
          "HGNC gene symbol, copied exactly from the candidate list"
        ),
        include = ellmer::type_boolean(
          "TRUE if this gene belongs in the final curated disease panel"
        ),
        confidence = ellmer::type_number(
          "confidence from 0 to 1 in this include/exclude decision"
        ),
        rationale = ellmer::type_string(
          "one short sentence justifying the decision from the evidence"
        )
      )
    ),
    overall_notes = ellmer::type_string(
      "brief summary of the curation strategy and any caveats"
    )
  )
}

# Build the system + user prompts. The candidate table is rendered compactly to
# keep token use bounded.
build_curation_prompt <- function(candidates, disease_name, top_n) {
  system_prompt <- paste(
    "You are an expert biomedical curator assembling a gene panel for a disease.",
    "You will be given candidate genes ranked by aggregated evidence from",
    "multiple databases. Select the genes that genuinely belong in the panel.",
    "Hard rules: only choose genes from the provided candidate list; never",
    "invent or rename symbols; justify each decision from the evidence shown;",
    "be conservative and prefer well-supported genes. Aim for roughly",
    top_n,
    "included genes unless the evidence clearly supports fewer or more."
  )

  cols <- intersect(
    c(
      "rank",
      "gene_symbol",
      "n_sources",
      "sources",
      "combined_score",
      "evidence"
    ),
    names(candidates)
  )
  tbl <- candidates[, cols, drop = FALSE]
  num <- vapply(tbl, is.numeric, logical(1))
  tbl[num] <- lapply(tbl[num], function(x) round(x, 3))
  tbl_txt <- paste(
    utils::capture.output(utils::write.csv(tbl, row.names = FALSE)),
    collapse = "\n"
  )

  user_prompt <- paste0(
    "Disease: ",
    disease_name,
    "\n\n",
    "Candidate genes (CSV, best evidence first):\n",
    tbl_txt,
    "\n\n",
    "Return your curated selection."
  )

  list(system = system_prompt, user = user_prompt)
}

# Coerce ellmer structured output (a data frame or list-of-lists) to a tibble.
selections_to_df <- function(sel) {
  if (is.null(sel)) {
    return(NULL)
  }
  if (is.data.frame(sel)) {
    return(tibble::as_tibble(sel))
  }
  dplyr::bind_rows(lapply(sel, function(x) {
    tibble::tibble(
      gene_symbol = as.character(x$gene_symbol %||% NA),
      include = as.logical(x$include %||% NA),
      confidence = as.numeric(x$confidence %||% NA),
      rationale = as.character(x$rationale %||% NA)
    )
  }))
}

# Validate/clean the model's selections against the candidate set:
# drop hallucinated symbols, clamp confidence, default missing fields.
validate_curation <- function(df, candidate_symbols) {
  if (is.null(df) || nrow(df) == 0) {
    return(empty_curated_table())
  }
  df <- tibble::as_tibble(df)
  if (!"gene_symbol" %in% names(df)) {
    return(empty_curated_table())
  }

  sym <- toupper(trimws(as.character(df$gene_symbol)))
  keep <- sym %in% toupper(candidate_symbols)

  include <- if ("include" %in% names(df)) as.logical(df$include) else TRUE
  include[is.na(include)] <- FALSE
  conf <- if ("confidence" %in% names(df)) clamp01(df$confidence) else NA_real_
  rat <- if ("rationale" %in% names(df)) as.character(df$rationale) else ""
  rat[is.na(rat)] <- ""

  out <- tibble::tibble(
    gene_symbol = sym,
    include = include,
    confidence = conf,
    rationale = rat
  )[keep, , drop = FALSE]

  out[!duplicated(out$gene_symbol), , drop = FALSE]
}

# Fallback selection when AI is unavailable or fails: top genes by combined rank.
fallback_curation <- function(ranked, top_n) {
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(empty_curated_table())
  }
  picked <- utils::head(ranked, top_n)
  tibble::tibble(
    gene_symbol = picked$gene_symbol,
    include = TRUE,
    confidence = NA_real_,
    rationale = "Selected by combined rank (AI unavailable)."
  )
}

# Main entry point. `chat_factory(system_prompt)` returns an ellmer chat object;
# inject a stub in tests. Returns a curated tibble with attributes:
#   ai_used (logical), message (chr, optional), error (chr, optional).
curate_gene_list <- function(
  ranked,
  disease_name,
  top_n = 30,
  model = Sys.getenv(
    "GLB_GEMINI_MODEL",
    unset = "gemini-flash-lite-latest"
  ),
  chat_factory = NULL
) {
  if (is.null(ranked) || nrow(ranked) == 0) {
    out <- empty_curated_table()
    attr(out, "ai_used") <- FALSE
    attr(out, "message") <- "No ranked genes to curate."
    return(out)
  }

  candidates <- utils::head(ranked, max(top_n * 2L, 50L))
  cand_symbols <- toupper(candidates$gene_symbol)

  if (is.null(chat_factory) && !gemini_available()) {
    out <- fallback_curation(ranked, top_n)
    attr(out, "ai_used") <- FALSE
    attr(out, "message") <-
      "GEMINI_API_KEY not set - selected top genes by combined rank."
    return(out)
  }

  if (is.null(chat_factory)) {
    chat_factory <- function(system_prompt) {
      ellmer::chat_google_gemini(system_prompt = system_prompt, model = model)
    }
  }

  prompt <- build_curation_prompt(candidates, disease_name, top_n)
  out <- tryCatch(
    {
      chat <- chat_factory(prompt$system)
      raw <- chat$chat_structured(prompt$user, type = curation_schema())
      v <- validate_curation(selections_to_df(raw$selections), cand_symbols)
      attr(v, "ai_used") <- TRUE
      v
    },
    error = function(e) {
      res <- fallback_curation(ranked, top_n)
      attr(res, "ai_used") <- FALSE
      attr(res, "error") <- conditionMessage(e)
      res
    }
  )

  # The AI succeeded but returned nothing usable: fall back to rank.
  if (
    isTRUE(attr(out, "ai_used")) &&
      (nrow(out) == 0 || !any(out$include, na.rm = TRUE))
  ) {
    fb <- fallback_curation(ranked, top_n)
    attr(fb, "ai_used") <- FALSE
    attr(fb, "message") <-
      "AI returned no usable selection - fell back to combined rank."
    return(fb)
  }

  out
}
