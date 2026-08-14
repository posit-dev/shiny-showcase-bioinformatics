# GeneScout HTTP API - DESIGN-ONLY REFERENCE (not deployed, not in the test suite).
#
# This file exists to PROVE the engine surface is UI-agnostic: a React / Shiny-
# Python / any front end drives the exact same core functions the Shiny app and
# the CLI use, over plain JSON, with zero Shiny. It is intentionally NOT wired
# into the app and `plumber` is deliberately NOT in DESCRIPTION / renv.lock.
#
# To try it (after `install.packages("plumber")`), from the app root:
#   Rscript -e 'plumber::pr("api/plumber.R") |> plumber::pr_run(port = 8000)'
#
# Design notes that shaped the endpoints:
#   * ONE call per phase, each a 1-2 line wrapper over an existing core function.
#   * NEVER serialize the enriched object's `registry_obj` - it holds live R
#     closures (each signal's extractor/normalize), so a stateless /enrich ->
#     /rank split is impossible. /review runs enrich + rank server-side in one go.
#   * List-column provenance (input_lists, input_source_ids) serializes to nested
#     JSON arrays - a client must expect arrays-of-arrays, not scalars.
#   * The propose -> confirm -> run flow is a pure DATA contract (curate_input /
#     confirm_input / run_review_request), identical to the CLI and the Shiny UI.

# plumber evaluates this file with the working directory set to its own folder
# (api/), but the engine's global.R - and every path it reads (R/, rubric.yml,
# context/, data/) - is relative to the app ROOT. Hop up to the root so a launch
# via plumber::pr("api/plumber.R") from the app root resolves correctly.
if (!file.exists("global.R") && file.exists(file.path("..", "global.R"))) {
  setwd("..")
}
# Anchor the file-based loaders (rubric.yml, context/) to the app root: plumber
# serves each request with a working directory of its own choosing, so a plain
# relative path would fail at request time. genescout_app_path() reads this.
Sys.setenv(GENESCOUT_APP_ROOT = normalizePath(getwd()))
source("global.R")

# --- (de)serialization helpers ---------------------------------------------

# Rebuild an input proposal (tibbles) from the plain JSON a client re-posts.
proposal_from_body <- function(p) {
  as_rows <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    dplyr::bind_rows(lapply(x, function(r) tibble::as_tibble(r)))
  }
  list(
    tokens = as_rows(p$tokens),
    sources = as_rows(p$sources),
    proposed_disease = p$proposed_disease %||%
      list(name = "", search_term = ""),
    notes = p$notes %||% ""
  )
}

# The public shape of a proposal (tibbles serialize as arrays of row-objects).
proposal_response <- function(p) {
  list(
    tokens = p$tokens,
    sources = p$sources,
    proposed_disease = p$proposed_disease,
    notes = p$notes,
    ai_used = isTRUE(attr(p, "ai_used")),
    message = attr(p, "message"),
    error = attr(p, "error")
  )
}

#* @apiTitle GeneScout
#* @apiDescription Research-use-only gene-list prioritization. Not clinical.

#* The source catalog: every connector + its selection metadata, so a front end can
#* render a grouped picker and post the chosen keys back as options.sources on
#* /review. Plain data (no closures) - the introspection surface of the core engine.
#* @get /catalog
#* @serializer unboxedJSON
function() {
  list(sources = genescout_catalog_json())
}

#* Interpret + clean multi-source input into a proposal (front-of-pipeline agent).
#* @post /propose
#* @serializer unboxedJSON
function(req) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  cs <- candidate_set_from_list(body$sources)
  proposal <- curate_input(cs, body$description %||% "", genescout_config)
  proposal_response(proposal)
}

#* Apply the (edited) proposal -> the confirmed, serializable candidate_set.
#* @post /confirm
#* @serializer unboxedJSON
function(req) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  edits <- if (is.null(body$edits)) {
    NULL
  } else {
    dplyr::bind_rows(lapply(body$edits, tibble::as_tibble))
  }
  confirmed <- confirm_input(proposal_from_body(body$proposal), edits)
  list(sources = candidate_set_to_list(confirmed))
}

#* Ground a proposed disease SEARCH TERM into candidate ontology matches.
#* @post /resolve-disease
#* @serializer unboxedJSON
function(req) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  r <- resolve_proposed_disease(body$search_term %||% "")
  if (!isTRUE(r$ok)) {
    return(list(ok = FALSE, error = r$error))
  }
  list(ok = TRUE, matches = r$matches)
}

#* Enrich + rank a review request in one call (never splits enrich/rank).
#* @post /review
#* @serializer unboxedJSON
function(req) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  disease <- body$disease
  registry <- if (!is.null(disease) && !is_blank(disease$id %||% "")) {
    genescout_registry_disease
  } else {
    genescout_registry
  }
  result <- run_review_request(
    body,
    config = genescout_config,
    registry = registry
  )
  list(
    genes = result$genes,
    evidence = result$evidence,
    registry = result$registry,
    provenance = result$provenance
  )
}
