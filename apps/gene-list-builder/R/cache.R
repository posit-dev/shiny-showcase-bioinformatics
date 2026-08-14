# Lightweight on-disk cache for source responses -------------------------------
#
# Keyed on a string built from (disease id, source id, options) so repeated runs
# and slider tuning don't re-hit the APIs. The "force refresh" toggle in the UI
# bypasses reads. Cache lives under data/cache/ (git-ignored).

glb_cache_dir <- function() {
  dir <- Sys.getenv("GLB_CACHE_DIR", unset = file.path("data", "cache"))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# Build a stable string key from arbitrary parts.
glb_cache_key <- function(...) {
  parts <- lapply(list(...), function(x) paste(as.character(x), collapse = "|"))
  paste(unlist(parts), collapse = "::")
}

.cache_file <- function(key) {
  file.path(glb_cache_dir(), paste0(rlang::hash(key), ".rds"))
}

glb_cache_get <- function(key) {
  f <- .cache_file(key)
  if (file.exists(f)) readRDS(f) else NULL
}

glb_cache_set <- function(key, value) {
  saveRDS(value, .cache_file(key))
  invisible(value)
}
