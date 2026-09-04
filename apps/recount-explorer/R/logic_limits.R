# What a study costs, and where the app draws the line.
#
# Shiny-free, so every number here is testable without an app.
#
# recount3 stores one gzipped genes-by-samples file for each study. There is no
# way to ask for fewer genes or fewer samples: the `sample` argument to
# locate_url() is ignored for gene counts, and gzip has no row addressing, so
# the byte ranges the server offers cannot select anything useful. The download
# unit is the whole study. The only honest response is to say what it costs and
# to refuse what would exhaust the server.

# Genes in each annotation. Human is the value a loaded RSE reports; mouse is
# the row count of the shipped annotation. Both feed the memory estimate only,
# so being a few rows out does not matter.
GENES_PER_ORGANISM <- c(human = 63856, mouse = 55421)

# Measured, not guessed. A 100-sample human study (63,856 genes) occupies
# 194 MB in R: 137.5 MB for the RangedSummarizedExperiment plus 56.7 MB for the
# log2 CPM matrix. That is 30.4 bytes for each gene in each sample.
BYTES_PER_GENE_SAMPLE <- 30.4

# Measured across eight studies from 4 to 28,706 samples, and consistent:
# roughly 100 KB of gzipped counts for each sample. Used only when the catalog
# has no recorded size for a study.
KB_PER_SAMPLE <- 100

# Peak memory a study needs once loaded, in MB.
estimated_memory_mb <- function(n_samples, organism) {
  genes <- unname(GENES_PER_ORGANISM[as.character(organism)])
  genes[is.na(genes)] <- max(GENES_PER_ORGANISM)
  as.numeric(n_samples) * genes * BYTES_PER_GENE_SAMPLE / 1e6
}

# Download size in MB: the recorded value when the catalog has one, otherwise
# the per-sample estimate.
study_download_estimate_mb <- function(row) {
  recorded <- suppressWarnings(as.numeric(row$download_mb))
  if (length(recorded) == 1L && !is.na(recorded) && recorded > 0) {
    return(recorded)
  }
  as.numeric(row$n_samples) * KB_PER_SAMPLE / 1e3
}

# The ceiling, in samples.
#
# Default 500, which is about 1 GB of peak memory and allows 18,760 of the
# 18,998 studies in the catalog. The median study has 11 samples and the 95th
# percentile has 105, so the cap is generous for ordinary use and only stops
# the 238 studies that would put a server under real pressure.
#
# RECOUNT_EXPLORER_MAX_SAMPLES raises or lowers it. Set it to Inf on a
# workstation with memory to spare.
study_limits <- function() {
  raw <- Sys.getenv("RECOUNT_EXPLORER_MAX_SAMPLES", "")
  max_samples <- if (nzchar(raw)) suppressWarnings(as.numeric(raw)) else 500
  if (length(max_samples) != 1L || is.na(max_samples) || max_samples <= 0) {
    max_samples <- 500
  }
  list(max_samples = max_samples)
}

# NULL when the study is loadable, otherwise the reason, with the numbers in
# it. A message that says "too large" and stops there tells the reader nothing
# about what to do next.
study_load_block <- function(row, limits = study_limits()) {
  stopifnot(is.data.frame(row), nrow(row) == 1L)
  n <- as.numeric(row$n_samples)
  if (n <= limits$max_samples) {
    return(NULL)
  }
  list(
    reason = sprintf(
      "This study has %s samples. The limit is %s.",
      format(n, big.mark = ","),
      format(limits$max_samples, big.mark = ",")
    ),
    detail = sprintf(
      paste(
        "Loading it needs about %s of download and about %s of memory.",
        "Raise RECOUNT_EXPLORER_MAX_SAMPLES to load it anyway."
      ),
      format_size_mb(study_download_estimate_mb(row)),
      format_size_mb(estimated_memory_mb(n, row$organism))
    )
  )
}

# "296 MB" or "1.0 GB", whichever reads better.
format_size_mb <- function(mb) {
  if (length(mb) != 1L || is.na(mb)) {
    return("an unknown amount")
  }
  if (mb >= 1024) {
    return(sprintf("%.1f GB", mb / 1024))
  }
  sprintf("%.0f MB", mb)
}

# Roughly how long a download takes, for the one line a user reads before
# deciding. Deliberately vague, because it depends on their connection.
#
# The default rate is measured against the recount3 servers, not guessed from a
# link speed: a 10 MB study took 7.8 seconds of download, which is about
# 1.3 MB/s. Using a nominal 50 Mbit/s here would promise "under a minute" for a
# GTEx tissue that actually takes four.
MB_PER_SECOND <- 1.3

format_download_time <- function(mb, mb_per_second = MB_PER_SECOND) {
  secs <- mb / mb_per_second
  if (secs < 20) {
    return("a few seconds")
  }
  if (secs < 90) {
    return("under a minute")
  }
  sprintf("about %.0f minutes", secs / 60)
}
