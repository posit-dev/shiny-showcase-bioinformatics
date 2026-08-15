safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }

  readRDS(path)
}

app_version <- function() {
  "0.2.1"
}

# Coerce to numeric without warnings (returns NA for unparseable values).
safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Clamp a numeric vector to the unit interval, preserving NA.
clamp01 <- function(x) {
  x <- safe_num(x)
  pmin(pmax(x, 0), 1)
}

# Rank-normalize a numeric vector to (0, 1]. Uses the maximum rank for ties, so
# all-equal inputs map to 1 and the smallest value still gets a nonzero 1/n
# (presence always contributes something). Robust to the heavy-tailed,
# count-based scores some sources return. NA values stay NA and are ignored.
normalize_rank <- function(x) {
  x <- safe_num(x)
  ok <- !is.na(x)
  n <- sum(ok)
  out <- rep(NA_real_, length(x))
  if (n == 0) {
    return(out)
  }
  out[ok] <- rank(x[ok], ties.method = "max") / n
  out
}

# TRUE for the usual "on" strings (and TRUE itself); used for env-var flags.
truthy <- function(x) {
  isTRUE(x) ||
    (length(x) == 1 &&
      tolower(as.character(x)) %in%
        c(
          "1",
          "true",
          "yes",
          "on"
        ))
}

# First non-missing value of a vector, or NA of the same flavour.
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0) x[[1]] else NA
}

# Null-coalescing operator. Defined here rather than in modules/ because
# R/load_components.R sources R/ FIRST, and nearly every file in R/ uses it.
# Base R only exports `%||%` from 4.4.0, so on the R >= 4.3 this project claims
# to support, R/ would fail to load without this definition.
`%||%` <- function(a, b) if (is.null(a)) b else a
