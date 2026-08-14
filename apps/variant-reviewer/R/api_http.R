# Shared HTTP helpers for all external API clients.
#
# Every client funnels through vr_api_get() (REST) or vr_api_post_json()
# (GraphQL/JSON POST) so timeouts, retries, caching, and error handling live in
# one place. The return value is always a normalized list so callers never need
# their own tryCatch():
#   list(ok = TRUE,  status = 200L, data = <parsed JSON>)
#   list(ok = FALSE, status = <int|NA>, error = "<message>", data = NULL)
#
# Successful responses are cached in-process for VR_CACHE_TTL seconds so
# repeated searches for the same gene/variant are instant; failures are never
# cached, so a transient outage doesn't get stuck. TTL and the size ceilings are
# overridable with the VR_CACHE_TTL / VR_CACHE_MAX_SIZE / VR_CACHE_MAX_N
# environment variables.

# A positive number from an environment variable, else the default. Keeps a
# blank or malformed setting from silently disabling the cache.
.vr_env_num <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) {
    return(default)
  }
  val <- suppressWarnings(as.numeric(raw))
  if (length(val) != 1L || is.na(val) || val <= 0) default else val
}

VR_CACHE_TTL <- .vr_env_num("VR_CACHE_TTL", 1800) # 30 minutes
# Ceilings so a long-running server cannot grow without bound. cachem evicts
# least-recently-used first when either is reached, so the genes people are
# actually looking at stay warm and the long tail falls out.
VR_CACHE_MAX_SIZE <- .vr_env_num("VR_CACHE_MAX_SIZE", 256 * 1024^2) # 256 MB
VR_CACHE_MAX_N <- .vr_env_num("VR_CACHE_MAX_N", 5000)

# One cache for the whole process, deliberately shared across sessions: a gene
# one user looks at is warm for the next. It holds only public API responses,
# never anything user-specific, so there is nothing to leak between sessions.
vr_cache <- cachem::cache_mem(
  max_age = VR_CACHE_TTL,
  max_size = VR_CACHE_MAX_SIZE,
  max_n = VR_CACHE_MAX_N,
  evict = "lru"
)

# What the cache is holding right now, for the console or a health check.
vr_cache_stats <- function() {
  list(
    entries = vr_cache$size(),
    max_entries = VR_CACHE_MAX_N,
    max_size_bytes = VR_CACHE_MAX_SIZE,
    ttl_seconds = VR_CACHE_TTL
  )
}

# Drop everything (e.g. to force a refetch after an upstream fixes bad data).
vr_cache_clear <- function() {
  vr_cache$reset()
  invisible(TRUE)
}

# Stable cache key for a request (lowercase hex hash -> valid cachem key).
vr_cache_key <- function(...) {
  rlang::hash(list(...))
}

# GET a REST endpoint. `source` is a friendly label used in error messages.
vr_api_get <- function(
  base_url,
  path = NULL,
  query = list(),
  source = "API",
  timeout = 15,
  max_tries = 3
) {
  if (length(query) > 0) {
    # Drop NULL/empty query values so we never send "param=".
    query <- query[!vapply(query, is_blank, logical(1))]
  }
  key <- vr_cache_key("GET", base_url, path, query)

  vr_cached(key, function() {
    req <- httr2::request(base_url)
    if (!is.null(path)) {
      req <- httr2::req_url_path_append(req, path)
    }
    if (length(query) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), query))
    }
    req <- vr_req_defaults(req, timeout, max_tries)
    vr_perform(req, source)
  })
}

# POST a JSON body (e.g. a GraphQL query) and parse the JSON response.
vr_api_post_json <- function(
  url,
  body,
  source = "API",
  timeout = 20,
  max_tries = 3
) {
  key <- vr_cache_key("POST", url, body)

  vr_cached(key, function() {
    req <- httr2::request(url)
    req <- httr2::req_body_json(req, body)
    req <- vr_req_defaults(req, timeout, max_tries)
    vr_perform(req, source)
  })
}

# Return a cached successful result for `key`, otherwise run `fetch()` and cache
# it only when it succeeded.
vr_cached <- function(key, fetch) {
  hit <- vr_cache$get(key, missing = NULL)
  if (!is.null(hit)) {
    return(hit)
  }
  res <- fetch()
  if (isTRUE(res$ok)) {
    vr_cache$set(key, res)
  }
  res
}

# Apply the shared request options (timeout, retries, no-raise, user agent).
vr_req_defaults <- function(req, timeout, max_tries) {
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = max_tries)
  # Don't let httr2 raise on HTTP errors; we normalize them ourselves.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  httr2::req_user_agent(req, "variant-reviewer (Shiny app)")
}

# Turn a low-level failure -- a non-2xx HTTP status, or a curl/transport error
# (timeout, DNS, connection refused) -- into a short, plain-language message that
# is safe to show a user. It never leaks curl/httr2 internals; the raw cause is
# kept separately (the result's `detail` field) for server-side logs only.
# `source` is the friendly API label passed through from the client.
vr_http_error_message <- function(
  source,
  status = NA_integer_,
  condition = NULL
) {
  if (!is.na(status)) {
    if (status == 404) {
      return(paste0("No ", source, " data was found for this query."))
    }
    if (status == 429) {
      return(paste0(
        source,
        " is busy right now (too many requests). Please try again in a moment."
      ))
    }
    if (status >= 500) {
      return(paste0(
        source,
        " is temporarily unavailable. Please try again shortly."
      ))
    }
    return(paste0("Couldn't retrieve data from ", source, " right now."))
  }
  raw <- tolower(paste(
    if (is.null(condition)) "" else conditionMessage(condition),
    collapse = " "
  ))
  if (grepl("timeout|timed out", raw)) {
    return(paste0(
      source,
      " took too long to respond. Please try again shortly."
    ))
  }
  if (grepl("resolve|name or service|dns|offline|could not connect", raw)) {
    return(paste0(
      "Couldn't reach ",
      source,
      ". Please check your internet connection and try again."
    ))
  }
  paste0(source, " is temporarily unavailable. Please try again shortly.")
}

# Perform a request and normalize the result into the standard list. On failure
# `error` carries the user-facing message and `detail` the raw cause (logged to
# the server console via message(), never shown to the user).
vr_perform <- function(req, source) {
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      if (status >= 200 && status < 300) {
        list(
          ok = TRUE,
          status = status,
          # check_type = FALSE: some sources (e.g. STRING) send "text/json".
          data = httr2::resp_body_json(
            resp,
            check_type = FALSE,
            simplifyVector = FALSE
          ),
          error = NULL,
          detail = NULL
        )
      } else {
        detail <- paste0(source, " returned HTTP ", status)
        message("[variant-reviewer] ", detail)
        list(
          ok = FALSE,
          status = status,
          data = NULL,
          error = vr_http_error_message(source, status = status),
          detail = detail
        )
      }
    },
    error = function(e) {
      detail <- paste0("Could not reach ", source, ": ", conditionMessage(e))
      message("[variant-reviewer] ", detail)
      list(
        ok = FALSE,
        status = NA_integer_,
        data = NULL,
        error = vr_http_error_message(source, condition = e),
        detail = detail
      )
    }
  )
}

# TRUE for NULL, NA, empty string, or zero-length values.
is_blank <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (length(x) == 1 && (is.na(x) || identical(trimws(as.character(x)), "")))
}

# Pull a value from a nested list by key path, returning `default` if any level
# is missing or NULL. e.g. pluck_at(x, "ensembl", "gene").
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
