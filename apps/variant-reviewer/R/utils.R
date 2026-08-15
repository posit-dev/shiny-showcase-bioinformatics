safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }

  readRDS(path)
}

# Keep in sync with `version` in CITATION.cff/CITATION.md, which can't read
# this value (they are static metadata, not evaluated).
app_version <- function() {
  "2.3.1"
}
