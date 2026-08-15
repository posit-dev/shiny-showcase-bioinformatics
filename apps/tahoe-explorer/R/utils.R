safe_read_rds <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }

  readRDS(path)
}

app_version <- function() {
  "0.1.2"
}
