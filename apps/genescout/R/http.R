# Shared HTTP layer for all bio-database clients.
#
# Every client funnels through http_get_json() (REST) or http_post_json()
# (GraphQL / JSON POST) so timeouts, retries, caching, and error handling live in
# one place. The return value is always a normalized list, so callers never write
# their own tryCatch():
#   list(ok = TRUE,  status = 200L, data = <parsed JSON>, error = NULL)
#   list(ok = FALSE, status = <int|NA>, data = NULL, error = "<message>")
#
# Successful responses are cached in-process for GENESCOUT_CACHE_TTL seconds so
# repeated lookups are instant; failures are never cached, so a transient outage
# does not get stuck. Adapted from the sibling variant-reviewer app.
#
# No {ellmer} imports here - clients must be callable and testable in plain R.

GENESCOUT_CACHE_TTL <- 1800 # 30 minutes
genescout_cache <- cachem::cache_mem(max_age = GENESCOUT_CACHE_TTL)

# Stable cache key for a request.
genescout_cache_key <- function(...) {
  rlang::hash(list(...))
}

# A descriptive, contact-bearing User-Agent. NCBI E-utilities and Europe PMC ask
# callers to identify themselves with a contact; the public repo URL is the stable
# identifier, and a hosted operator can add a mailto via GENESCOUT_CONTACT_EMAIL.
GENESCOUT_REPO_URL <- "https://github.com/samuelbharti/genescout"
# Read once from DESCRIPTION (sourced from the app root at startup) so the outbound
# User-Agent and the in-app footer share one version and never drift. This is the
# single source of truth for both; falls back to a literal if DESCRIPTION is absent.
GENESCOUT_VERSION <- local({
  v <- tryCatch(
    as.character(read.dcf(
      genescout_app_path("DESCRIPTION"),
      fields = "Version"
    )[1, 1]),
    error = function(e) NA_character_
  )
  if (is.na(v) || !nzchar(v)) "0.1.0" else v
})

genescout_user_agent <- function() {
  email <- Sys.getenv("GENESCOUT_CONTACT_EMAIL", "")
  contact <- if (nzchar(email)) {
    paste0("+", GENESCOUT_REPO_URL, "; mailto:", email)
  } else {
    paste0("+", GENESCOUT_REPO_URL)
  }
  sprintf("GeneScout/%s (%s)", GENESCOUT_VERSION, contact)
}

# --- Per-host circuit breaker ------------------------------------------------
# The enrichment loop makes one call per (gene x source), so a single unreachable
# host would otherwise cost `timeout x retries` on EVERY gene (failures are not
# cached). The breaker trips a host after GENESCOUT_BREAKER_THRESHOLD consecutive
# TRANSPORT failures (unreachable / timeout - an HTTP response, even 404/5xx, means
# the host is up and clears the count) and short-circuits further calls to it for
# GENESCOUT_BREAKER_COOLDOWN seconds, so one dead source degrades to fast misses instead
# of stalling the whole run. Process-global and self-healing via the cooldown.
GENESCOUT_BREAKER_THRESHOLD <- 3L
GENESCOUT_BREAKER_COOLDOWN <- 120 # seconds a tripped host is skipped before a retry
genescout_breaker <- new.env(parent = emptyenv())

# Hostname of a request URL, for keying the breaker (falls back to a constant so a
# malformed URL never errors the breaker).
genescout_url_host <- function(url) {
  h <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) NULL)
  if (is.null(h) || !nzchar(h)) "unknown-host" else h
}

genescout_breaker_state <- function(host) {
  genescout_breaker[[host]] %||% list(fails = 0L, tripped_until = 0)
}

# TRUE when `host` is currently tripped (skip the network, return a fast miss).
genescout_breaker_open <- function(host, now = as.numeric(Sys.time())) {
  isTRUE(genescout_breaker_state(host)$tripped_until > now)
}

# Record a call outcome: a success (host reachable) clears the count; the Kth
# consecutive transport failure trips the breaker for the cooldown window.
genescout_breaker_record <- function(host, ok, now = as.numeric(Sys.time())) {
  if (isTRUE(ok)) {
    genescout_breaker[[host]] <- list(fails = 0L, tripped_until = 0)
  } else {
    st <- genescout_breaker_state(host)
    fails <- st$fails + 1L
    tripped_until <- if (fails >= GENESCOUT_BREAKER_THRESHOLD) {
      now + GENESCOUT_BREAKER_COOLDOWN
    } else {
      st$tripped_until
    }
    genescout_breaker[[host]] <- list(
      fails = fails,
      tripped_until = tripped_until
    )
  }
  invisible(genescout_breaker[[host]])
}

# Clear all breaker state (used by tests to isolate; a fresh process starts clean).
genescout_breaker_reset <- function() {
  rm(list = ls(genescout_breaker), envir = genescout_breaker)
  invisible(NULL)
}

# GET a REST endpoint. `source` is a friendly label used in error messages.
# `headers` is an optional NAMED list of request headers for a key-gated source
# (e.g. list(Authorization = paste("Bearer", token))); they are marked sensitive
# (redacted from printed requests / errors) and are DELIBERATELY excluded from the
# cache key - the same gene returns the same data regardless of the caller's token,
# and the secret must never enter the hashed key.
http_get_json <- function(
  base_url,
  path = NULL,
  query = list(),
  source = "API",
  timeout = 15,
  max_tries = 3,
  headers = NULL
) {
  if (length(query) > 0) {
    query <- query[!vapply(query, is_blank, logical(1))]
  }
  key <- genescout_cache_key("GET", base_url, path, query)

  genescout_cached(key, function() {
    req <- httr2::request(base_url)
    if (!is.null(path)) {
      req <- httr2::req_url_path_append(req, path)
    }
    if (length(query) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), query))
    }
    req <- genescout_req_defaults(req, timeout, max_tries, headers)
    genescout_perform(req, source)
  })
}

# POST a JSON body (e.g. a GraphQL query) and parse the JSON response. `headers` is
# an optional named list of (redacted, non-cached) request headers - see http_get_json.
http_post_json <- function(
  url,
  body,
  source = "API",
  timeout = 20,
  max_tries = 3,
  headers = NULL
) {
  key <- genescout_cache_key("POST", url, body)

  genescout_cached(key, function() {
    req <- httr2::request(url)
    req <- httr2::req_body_json(req, body)
    req <- genescout_req_defaults(req, timeout, max_tries, headers)
    genescout_perform(req, source)
  })
}

# Normalize a GraphQL response into the clients' error contract. A GraphQL server
# reports query errors inside an HTTP 200 body (a top-level `errors` array), so a
# 200 is not enough. Returns a standard `list(ok = FALSE, error = ...)` on a
# transport failure OR a query error, else NULL - letting each GraphQL client
# collapse its two-step check into one guarded call.
graphql_error <- function(res, source = "API") {
  if (!isTRUE(res$ok)) {
    return(list(
      ok = FALSE,
      error = res$error %||% paste0(source, " request failed.")
    ))
  }
  if (!is.null(res$data$errors)) {
    return(list(ok = FALSE, error = paste0(source, " returned a query error.")))
  }
  NULL
}

# GET a text endpoint (some sources ship a bulk flat file - CSV/TSV - rather than a
# per-record JSON API; e.g. ClinGen's gene-validity download). Same timeout / retry /
# cache / header-redaction as http_get_json, but the body is returned verbatim as a
# string in `text` (list(ok, status, text, error)) instead of parsed JSON in `data`.
# The success-only cache means a bulk file is fetched once per URL and every later
# per-gene lookup hits the in-process cache.
http_get_text <- function(
  base_url,
  path = NULL,
  query = list(),
  source = "API",
  timeout = 30,
  max_tries = 3,
  headers = NULL
) {
  if (length(query) > 0) {
    query <- query[!vapply(query, is_blank, logical(1))]
  }
  key <- genescout_cache_key("GET_TEXT", base_url, path, query)

  genescout_cached(key, function() {
    req <- httr2::request(base_url)
    if (!is.null(path)) {
      req <- httr2::req_url_path_append(req, path)
    }
    if (length(query) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), query))
    }
    req <- genescout_req_defaults(req, timeout, max_tries, headers)
    genescout_perform_text(req, source)
  })
}

# Perform a request and normalize into an ok/status/text/error list (text variant
# of genescout_perform, for non-JSON bodies). Same per-host breaker as genescout_perform.
genescout_perform_text <- function(req, source) {
  host <- genescout_url_host(req$url)
  if (genescout_breaker_open(host)) {
    return(list(
      ok = FALSE,
      status = NA_integer_,
      text = NULL,
      error = paste0(source, " skipped (host temporarily unreachable).")
    ))
  }
  res <- tryCatch(
    {
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      if (status >= 200 && status < 300) {
        list(
          ok = TRUE,
          status = status,
          text = httr2::resp_body_string(resp),
          error = NULL
        )
      } else {
        list(
          ok = FALSE,
          status = status,
          text = NULL,
          error = paste0(source, " returned HTTP ", status, ".")
        )
      }
    },
    error = function(e) {
      list(
        ok = FALSE,
        status = NA_integer_,
        text = NULL,
        error = paste0("Could not reach ", source, ": ", conditionMessage(e))
      )
    }
  )
  genescout_breaker_record(host, ok = !is.na(res$status))
  res
}

# Return a cached successful result for `key`, otherwise run `fetch()` and cache
# it only when it succeeded.
genescout_cached <- function(key, fetch) {
  hit <- genescout_cache$get(key, missing = NULL)
  if (!is.null(hit)) {
    return(hit)
  }
  res <- fetch()
  if (isTRUE(res$ok)) {
    genescout_cache$set(key, res)
  }
  res
}

# Apply the shared request options (timeout, retries, no-raise, user agent). When
# `headers` (a named list) is supplied for a key-gated source, they are attached and
# marked sensitive via `.redact`, so httr2 never prints or logs the token (e.g. in a
# transport-error message). Header auth keeps the secret out of the URL entirely.
genescout_req_defaults <- function(req, timeout, max_tries, headers = NULL) {
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = max_tries)
  # Don't let httr2 raise on HTTP errors; we normalize them ourselves.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_user_agent(req, genescout_user_agent())
  if (!is.null(headers) && length(headers) > 0) {
    req <- do.call(
      httr2::req_headers,
      c(list(req), headers, list(.redact = names(headers)))
    )
  }
  req
}

# Perform a request and normalize into the standard ok/status/data/error list.
# Short-circuits to a fast miss when the host's breaker is tripped, and records the
# outcome (only a transport failure - status NA - counts against the host).
genescout_perform <- function(req, source) {
  host <- genescout_url_host(req$url)
  if (genescout_breaker_open(host)) {
    return(list(
      ok = FALSE,
      status = NA_integer_,
      data = NULL,
      error = paste0(source, " skipped (host temporarily unreachable).")
    ))
  }
  res <- tryCatch(
    {
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      if (status >= 200 && status < 300) {
        list(
          ok = TRUE,
          status = status,
          data = httr2::resp_body_json(
            resp,
            check_type = FALSE,
            simplifyVector = FALSE
          ),
          error = NULL
        )
      } else {
        list(
          ok = FALSE,
          status = status,
          data = NULL,
          error = paste0(source, " returned HTTP ", status, ".")
        )
      }
    },
    error = function(e) {
      list(
        ok = FALSE,
        status = NA_integer_,
        data = NULL,
        error = paste0("Could not reach ", source, ": ", conditionMessage(e))
      )
    }
  )
  genescout_breaker_record(host, ok = !is.na(res$status))
  res
}

# TRUE for NULL, NA, empty string, or zero-length values.
is_blank <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (length(x) == 1 && (is.na(x) || identical(trimws(as.character(x)), "")))
}

# Pull a value from a nested list by key path, returning `default` if any level
# is missing or NULL. e.g. pluck_at(x, "data", "target").
pluck_at <- function(x, ..., default = NULL) {
  keys <- c(...)
  for (key in keys) {
    if (is.null(x) || !is.list(x) || is.null(x[[key]])) {
      return(default)
    }
    x <- x[[key]]
  }
  if (is.null(x)) default else x
}
