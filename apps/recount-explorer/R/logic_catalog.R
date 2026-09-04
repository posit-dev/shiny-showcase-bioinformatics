# Study catalog layer. Shiny-free: it reads the shipped catalog snapshot and
# fetches the study title and abstract for one project at a time.
#
# available_projects() gives accessions and sample counts but no title and no
# abstract. Those live in a separate metadata file for each project. Collecting
# all of them costs about 19,000 HTTP requests, so a build script does it once
# and writes data/recount3_catalog.rds. See data-raw/build_catalog.R.

CATALOG_SCHEMA_VERSION <- 2L

CATALOG_COLUMNS <- c(
  "uid",
  "project",
  "organism",
  "file_source",
  "project_home",
  "n_samples",
  "download_mb",
  "study_title",
  "study_abstract"
)

# Stable key for one study. available_projects() reuses accessions across
# organisms, so the organism has to be part of the key.
catalog_uid <- function(df) {
  paste(df$organism, df$file_source, df$project, sep = "/")
}

# Metadata URL for one project. locate_url(type = "metadata") returns several
# URLs. The project metadata file is <src>.<src>.<PROJ>.MD.gz. Select it by
# name, not by position, so a change to the file order upstream cannot quietly
# hand us the wrong file.
recount3_metadata_url <- function(
  project,
  project_home,
  organism,
  recount3_url = getOption("recount3_url", "http://duffel.rail.bio/recount3")
) {
  urls <- recount3::locate_url(
    project = project,
    project_home = project_home,
    type = "metadata",
    organism = organism,
    recount3_url = recount3_url
  )
  src <- basename(project_home)
  want <- paste0(src, ".", src, ".", project, ".MD.gz")
  unname(if (want %in% names(urls)) urls[[want]] else urls[[1L]])
}

# Read the header and the first data row of a gzipped metadata file.
#
# curl::curl(), not base::url(). The recount3 host answers the documented
# http:// address with a 301 to https://, and that answers with a 302 to S3.
# base::url() follows neither, so every request returns 404. curl follows both.
#
# This also deliberately does not call recount3::file_retrieve(). That function
# probes the URL with http_error() before it downloads, which doubles the
# request count, and it writes into the shared BiocFileCache database. Parallel
# workers would serialize on that database and leave several GB behind.
# read.delim(nrows = 1) stops after the first row, so the transfer ends early.
read_first_metadata_row <- function(url, timeout = 60) {
  handle <- curl::new_handle(
    followlocation = TRUE,
    timeout = timeout,
    connecttimeout = 20L,
    useragent = "recount-explorer catalog build (R)"
  )
  con <- gzcon(curl::curl(url, open = "rb", handle = handle))
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  # readLines() rather than read.delim(con). read.delim() pushes back on the
  # connection to sniff the header, and a gzcon over a binary curl connection
  # refuses that. Two lines are the header and the first sample, which is all
  # the title and the abstract need, and the transfer stops there.
  lines <- readLines(con, n = 2L, warn = FALSE)
  if (length(lines) < 2L) {
    return(data.frame())
  }
  utils::read.delim(
    text = paste(lines, collapse = "\n"),
    sep = "\t",
    check.names = FALSE,
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE
  )
}

# Undo the markup that some submitters leave in study text.
#
# Only the HTML line break and paragraph tags are removed. Other angle bracket
# text in this data is not markup: study titles contain "IFN-<gamma>" and the
# mouse allele "Jundm2<tm2>", and a general tag strip would destroy both.
#
# The entity list is short on purpose. It covers what the recount3 metadata
# actually contains. &amp; is decoded last so that "&amp;lt;" cannot turn into
# a "<" on the second pass.
strip_markup <- function(x) {
  x <- gsub("</?(br|p)[[:space:]]*/?>", " ", x, ignore.case = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Collapse whitespace and turn placeholder values into NA.
clean_text <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("[\r\n\t]+", " ", as.character(x))
  x <- strip_markup(x)
  x <- trimws(gsub("[[:space:]]{2,}", " ", x))
  if (!nzchar(x) || tolower(x) %in% c("na", "n/a", "null", "none")) {
    return(NA_character_)
  }
  x
}

# Study text carries characters like the Greek beta and the typographic
# apostrophe. R reads them as "unknown" encoding, which means the native
# locale, so a string written on Windows can render as mojibake on a Linux
# server. Declare UTF-8 so the meaning travels with the data.
mark_utf8 <- function(x) {
  x <- as.character(x)
  present <- !is.na(x)
  if (any(present) && all(validUTF8(x[present]))) {
    Encoding(x) <- "UTF-8"
  }
  x
}

# Vectorized clean_text(), for cleaning a whole column at once.
clean_text_vec <- function(x) {
  vapply(x, clean_text, character(1), USE.NAMES = FALSE)
}

# Title and abstract for one project. Never throws: it always returns one row,
# so the caller can tell "not attempted" apart from "attempted and failed".
fetch_study_metadata <- function(
  project,
  project_home,
  organism,
  retries = 3L,
  backoff = 1.5,
  recount3_url = getOption("recount3_url", "http://duffel.rail.bio/recount3")
) {
  out <- data.frame(
    project = project,
    organism = organism,
    file_source = basename(project_home),
    study_title = NA_character_,
    study_abstract = NA_character_,
    fetch_status = "ok",
    fetch_message = NA_character_,
    stringsAsFactors = FALSE
  )

  url <- tryCatch(
    recount3_metadata_url(project, project_home, organism, recount3_url),
    error = function(e) NULL
  )
  if (is.null(url)) {
    out$fetch_status <- "error"
    out$fetch_message <- "could not build the metadata url"
    return(out)
  }

  last <- NULL
  for (attempt in seq_len(retries)) {
    row <- tryCatch(read_first_metadata_row(url), error = function(e) e)
    if (!inherits(row, "error")) {
      if (nrow(row) == 0L) {
        out$fetch_status <- "empty"
        return(out)
      }
      nm <- tolower(names(row))
      has_title <- "study_title" %in% nm
      has_abstract <- "study_abstract" %in% nm
      if (has_title) {
        out$study_title <- clean_text(row[[which(nm == "study_title")[1L]]])
      }
      if (has_abstract) {
        out$study_abstract <- clean_text(
          row[[which(nm == "study_abstract")[1L]]]
        )
      }
      if (!has_title && !has_abstract) {
        out$fetch_status <- "no_fields"
      }
      return(out)
    }
    last <- row
    # Some sources have no <src>.<src> file at all. A 404 is permanent, so do
    # not spend three attempts and twenty seconds of backoff on it.
    if (grepl("404|[Nn]ot [Ff]ound", conditionMessage(last))) {
      break
    }
    Sys.sleep(backoff^attempt)
  }

  out$fetch_status <- "error"
  out$fetch_message <- conditionMessage(last)
  out
}

# Provenance travels with the data as an attribute. Row subsetting keeps that
# attribute, but column subsetting drops it, and renderDT() selects columns.
# So filtered rows keep their provenance and a column subset does not. Read
# provenance from the master object to be safe.
catalog_meta <- function(df) {
  meta <- attr(df, "catalog_meta", exact = TRUE)
  if (is.null(meta)) {
    return(list(
      schema_version = NA_integer_,
      built_at = as.POSIXct(NA),
      recount3_version = NA_character_,
      n_projects = if (is.data.frame(df)) nrow(df) else NA_integer_,
      origin = "unknown"
    ))
  }
  meta
}

valid_catalog <- function(df) {
  is.data.frame(df) &&
    nrow(df) > 0L &&
    all(CATALOG_COLUMNS %in% names(df)) &&
    identical(catalog_meta(df)$schema_version, CATALOG_SCHEMA_VERSION)
}

# Coerce to the storage schema and attach provenance. n_samples arrives from
# available_projects() as a subsetted table, not an integer vector, so the
# unname() and the as.integer() are both load bearing.
finalize_catalog <- function(df, origin = "build", built_at = Sys.time()) {
  df$n_samples <- as.integer(unname(df$n_samples))
  df$organism <- as.character(df$organism)
  df$file_source <- as.character(df$file_source)
  df$project <- as.character(df$project)
  df$project_home <- as.character(df$project_home)
  # Normalize the study text here rather than at each source. Titles arrive
  # from a CSV export, from curated text, and from the metadata files, and all
  # three have to end up in the same shape.
  if (is.null(df$download_mb)) {
    df$download_mb <- NA_real_
  }
  df$download_mb <- as.numeric(df$download_mb)
  df$study_title <- mark_utf8(strip_markup(as.character(df$study_title)))
  df$study_abstract <- mark_utf8(strip_markup(as.character(df$study_abstract)))
  df$study_title <- trimws(gsub("[[:space:]]{2,}", " ", df$study_title))
  df$study_abstract <- trimws(gsub("[[:space:]]{2,}", " ", df$study_abstract))
  df$uid <- catalog_uid(df)

  df <- df[, CATALOG_COLUMNS, drop = FALSE]
  df <- df[order(df$organism, df$file_source, df$project), , drop = FALSE]
  rownames(df) <- NULL

  attr(df, "catalog_meta") <- list(
    schema_version = CATALOG_SCHEMA_VERSION,
    built_at = as.POSIXct(built_at, tz = "UTC"),
    recount3_version = as.character(utils::packageVersion("recount3")),
    r_version = as.character(getRversion()),
    n_projects = nrow(df),
    n_with_abstract = sum(nzchar(df$study_abstract)),
    origin = origin
  )
  df
}

# create_rse() reaches match.arg() through annotation_options(). match.arg()
# rejects a factor, so the row handed to load_study() must be character only.
catalog_proj_info <- function(row) {
  stopifnot(is.data.frame(row), nrow(row) == 1L)
  data.frame(
    project = as.character(row$project),
    organism = as.character(row$organism),
    file_source = as.character(row$file_source),
    project_home = as.character(row$project_home),
    project_type = "data_sources",
    n_samples = as.integer(row$n_samples),
    # Carried so the progress label can name the size. create_rse() reads the
    # columns it wants by name, so an extra one is ignored.
    download_mb = as.numeric(row$download_mb %||% NA_real_),
    # Carried so the loaded study can show its title without a second lookup.
    study_title = as.character(row$study_title %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

# ---- runtime -----------------------------------------------------------------

# Where the shipped snapshot lives. The environment variable is an escape
# hatch for a deployment that keeps the catalog outside the app bundle.
catalog_snapshot_path <- function() {
  Sys.getenv("RECOUNT_EXPLORER_CATALOG", "data/recount3_catalog.rds")
}

read_catalog_file <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  df <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(df) || !valid_catalog(df)) {
    return(NULL)
  }
  df
}

# The catalog the app runs on. Read once for each process, not once for each
# session, so every session shares one copy of the 19,000 rows.
read_catalog <- local({
  cached <- NULL
  function(path = catalog_snapshot_path(), refresh = FALSE) {
    if (refresh || is.null(cached)) {
      cached <<- read_catalog_file(path)
    }
    cached
  }
})

# One line of provenance for under the table.
catalog_summary_line <- function(df) {
  if (is.null(df)) {
    return("No catalog snapshot found.")
  }
  meta <- catalog_meta(df)
  built <- if (is.na(meta$built_at)) {
    "unknown date"
  } else {
    format(meta$built_at, "%Y-%m-%d")
  }
  sprintf(
    "%s studies, built %s with recount3 %s.",
    format(nrow(df), big.mark = ","),
    built,
    meta$recount3_version
  )
}

# Columns the results table shows, in order. The abstract is deliberately not
# among them: at roughly 900 characters a row it would swamp the table, and the
# right pane shows it for the one selected study instead.
CATALOG_TABLE_COLUMNS <- c(
  "project",
  "organism",
  "file_source",
  "n_samples",
  "study_title"
)

# Download size in MB for every row, falling back to the per-sample estimate
# where the catalog has no recorded value.
catalog_download_mb <- function(df) {
  mb <- suppressWarnings(as.numeric(df$download_mb))
  missing <- is.na(mb) | mb <= 0
  mb[missing] <- as.numeric(df$n_samples[missing]) * KB_PER_SAMPLE / 1e3
  mb
}

# The frame the results table renders: the stored columns plus a readable
# size, in the order the table shows them.
catalog_display <- function(df) {
  out <- df[, CATALOG_TABLE_COLUMNS, drop = FALSE]
  out$download <- vapply(catalog_download_mb(df), format_size_mb, character(1))
  out[, c(
    "project",
    "organism",
    "file_source",
    "n_samples",
    "download",
    "study_title"
  )]
}

# Sorted source names for the filter control, so a new recount3 source appears
# on its own rather than after someone edits a hardcoded list.
catalog_sources <- function(df) {
  sort(unique(as.character(df$file_source)))
}

# study_external_links() wants a catalog row. A loaded study carries the same
# three fields under different names, so this adapts one to the other rather
# than duplicating the link table.
study_link_row <- function(study) {
  data.frame(
    project = as.character(study$project),
    file_source = as.character(study$source),
    organism = as.character(study$organism),
    stringsAsFactors = FALSE
  )
}

# Links out to the archive that published the study.
study_external_links <- function(row) {
  project <- as.character(row$project)
  source <- as.character(row$file_source)
  if (source == "sra") {
    return(c(
      "SRA" = paste0(
        "https://trace.ncbi.nlm.nih.gov/Traces/study/?acc=",
        project
      ),
      "ENA" = paste0("https://www.ebi.ac.uk/ena/browser/view/", project)
    ))
  }
  if (source == "gtex") {
    return(c("GTEx Portal" = "https://gtexportal.org/home/"))
  }
  if (source == "tcga") {
    return(c(
      "GDC Portal" = paste0(
        "https://portal.gdc.cancer.gov/projects/TCGA-",
        project
      )
    ))
  }
  c("recount3" = "https://rna.recount.bio/")
}

# A data frame as a list of row objects.
#
# Shiny serializes a data frame column wise, so a client that expects an array
# of rows gets one object of parallel arrays and every .map() on it fails.
# Going row wise here keeps the JSON shape the React client declares.
df_to_rows <- function(df) {
  if (nrow(df) == 0L) {
    return(list())
  }
  unname(lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE])))
}

# ---- search ------------------------------------------------------------------

# The lowercase text that a search runs against: accession, title, abstract.
#
# Pasting 19,000 rows together costs about half a second, which is far too slow
# to do on every keystroke. Build it once for each catalog and keep it. The
# identity check is on the catalog build time and row count rather than the
# whole data frame, because comparing 20 MB of text would cost more than the
# work it saves.
catalog_haystack <- local({
  key <- NULL
  value <- NULL
  function(df) {
    meta <- catalog_meta(df)
    this <- paste(nrow(df), format(meta$built_at), meta$origin)
    if (!identical(this, key)) {
      value <<- tolower(paste(df$project, df$study_title, df$study_abstract))
      key <<- this
    }
    value
  }
})

# Filter and sort the catalog. Shiny-free so it can be tested without an app.
#
# `query` is matched against the accession, the title and the abstract. Every
# whitespace separated word has to appear somewhere in the row, which is what
# makes "glioma mouse" narrow rather than widen the result.
catalog_search <- function(
  catalog,
  query = "",
  organisms = NULL,
  sources = NULL,
  min_samples = NULL,
  max_samples = NULL,
  sort_by = "n_samples",
  sort_dir = "desc"
) {
  df <- catalog
  keep <- rep(TRUE, nrow(df))

  if (length(organisms)) {
    keep <- keep & df$organism %in% organisms
  }
  if (length(sources)) {
    keep <- keep & df$file_source %in% sources
  }
  if (length(min_samples) == 1L && !is.na(min_samples)) {
    keep <- keep & df$n_samples >= min_samples
  }
  if (length(max_samples) == 1L && !is.na(max_samples)) {
    keep <- keep & df$n_samples <= max_samples
  }

  query <- if (length(query) == 1L && !is.na(query)) trimws(query) else ""
  if (nzchar(query)) {
    haystack <- catalog_haystack(df)
    for (word in strsplit(tolower(query), "[[:space:]]+")[[1L]]) {
      if (!nzchar(word)) {
        next
      }
      keep <- keep & grepl(word, haystack, fixed = TRUE)
    }
  }

  out <- df[keep, , drop = FALSE]
  if (!sort_by %in% names(out)) {
    sort_by <- "n_samples"
  }
  ord <- order(out[[sort_by]], decreasing = identical(sort_dir, "desc"))
  out[ord, , drop = FALSE]
}

# One page of results, plus the counts the client needs to draw the pager.
catalog_page <- function(results, page = 1L, page_size = 25L) {
  page_size <- max(1L, as.integer(page_size))
  n <- nrow(results)
  pages <- max(1L, ceiling(n / page_size))
  page <- min(max(1L, as.integer(page)), pages)
  from <- (page - 1L) * page_size + 1L
  to <- min(n, page * page_size)
  rows <- if (n == 0L) {
    results[0, , drop = FALSE]
  } else {
    results[from:to, , drop = FALSE]
  }
  list(
    rows = rows,
    matched = n,
    page = page,
    pages = pages,
    from = if (n == 0L) 0L else from,
    to = to
  )
}

# ---- study files -------------------------------------------------------------

# URL of the gene counts file for one study. This is the big one: everything
# else recount3 fetches for a study is metadata measured in kilobytes.
recount3_counts_url <- function(
  project,
  project_home,
  organism,
  recount3_url = getOption("recount3_url", "http://duffel.rail.bio/recount3")
) {
  urls <- recount3::locate_url(
    project = project,
    project_home = project_home,
    type = "gene",
    organism = organism,
    recount3_url = recount3_url
  )
  unname(urls[[1L]])
}

# Size of the gene counts file, in MB, without downloading it.
#
# nobody = TRUE makes this a HEAD request, so the server answers with headers
# and no body. Returns NA when the server gives no Content-Length rather than
# guessing, so the caller can tell a real size from a fallback.
study_download_mb <- function(
  project,
  project_home,
  organism,
  timeout = 30
) {
  url <- tryCatch(
    recount3_counts_url(project, project_home, organism),
    error = function(e) NULL
  )
  if (is.null(url)) {
    return(NA_real_)
  }
  handle <- curl::new_handle(
    nobody = TRUE,
    followlocation = TRUE,
    timeout = timeout,
    connecttimeout = 15L,
    useragent = "recount-explorer catalog build (R)"
  )
  res <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(NA_real_)
  }
  headers <- rawToChar(res$headers)
  match <- regmatches(
    headers,
    regexpr("(?i)content-length:[[:space:]]*[0-9]+", headers, perl = TRUE)
  )
  if (!length(match)) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", match[[1L]])) / 1e6
}

# Warm the BiocFileCache entries that create_rse() needs, so the load itself
# finds everything on disk.
#
# create_rse() fetches exactly three things for a gene-level study: the
# metadata files, the annotation shared by every study of that organism, and
# the study counts file. Fetching them here rather than letting create_rse() do
# it is what allows the progress reporting: the caller sees three named steps
# instead of one opaque call.
#
# `on_stage(step, total, label)` is called before each step when supplied.
# "Downloading the counts, 33 MB" reads better than "Downloading the counts",
# because size is what the wait is made of.
counts_label <- function(proj_info) {
  mb <- suppressWarnings(as.numeric(proj_info$download_mb))
  if (length(mb) == 1L && !is.na(mb) && mb > 0) {
    return(paste0("Downloading the counts, ", format_size_mb(mb)))
  }
  "Downloading the counts"
}

prefetch_study_files <- function(proj_info, on_stage = NULL) {
  stopifnot(is.data.frame(proj_info), nrow(proj_info) == 1L)
  organism <- as.character(proj_info$organism)
  bfc <- recount3::recount3_cache()

  step <- function(i, label) {
    if (is.function(on_stage)) {
      on_stage(i, 3L, label)
    }
  }

  step(1L, "Fetching study metadata")
  meta_urls <- recount3::locate_url(
    project = proj_info$project,
    project_home = proj_info$project_home,
    type = "metadata",
    organism = organism
  )
  for (url in meta_urls) {
    recount3::file_retrieve(url = url, bfc = bfc, verbose = FALSE)
  }

  step(2L, "Fetching the gene annotation")
  recount3::file_retrieve(
    url = recount3::locate_url_ann(type = "gene", organism = organism),
    bfc = bfc,
    verbose = FALSE
  )

  step(3L, counts_label(proj_info))
  recount3::file_retrieve(
    url = recount3_counts_url(
      proj_info$project,
      proj_info$project_home,
      organism
    ),
    bfc = bfc,
    verbose = FALSE
  )

  invisible(TRUE)
}

# The annotation is shared by every study of one organism and is only 1.8 MB.
# Fetching it once at startup takes a step off every first study load.
prefetch_annotations <- function(organisms = c("human", "mouse")) {
  bfc <- recount3::recount3_cache()
  for (organism in organisms) {
    tryCatch(
      recount3::file_retrieve(
        url = recount3::locate_url_ann(type = "gene", organism = organism),
        bfc = bfc,
        verbose = FALSE
      ),
      error = function(e) NULL
    )
  }
  invisible(TRUE)
}

# ---- fast size lookup --------------------------------------------------------

# Everything below exists to make the build's HEAD pass affordable. Measured:
# locate_url() costs 0.39 s a study, and the documented http:// address answers
# with a 301 to https:// which answers with a 302 to S3, so each HEAD costs
# about 0.9 s. Together that is four hours for the whole catalog. Resolving the
# redirect once and building the URL by hand takes it to about six minutes.
#
# Neither shortcut is hardcoded. The storage prefix is learned by following one
# redirect, and the annotation code by making one locate_url() call for each
# organism, so both follow recount3 if it moves.

# The prefix that the recount3 address redirects to, learned once.
recount3_storage_base <- local({
  cached <- NULL
  function(sample_url) {
    if (!is.null(cached)) {
      return(cached)
    }
    handle <- curl::new_handle(nobody = TRUE, followlocation = TRUE)
    res <- tryCatch(
      curl::curl_fetch_memory(sample_url, handle = handle),
      error = function(e) NULL
    )
    if (is.null(res) || !nzchar(res$url)) {
      return(NULL)
    }
    marker <- "/recount3/"
    at <- regexpr(marker, sample_url, fixed = TRUE)
    if (at < 0) {
      return(NULL)
    }
    tail_path <- substring(sample_url, at + nchar(marker))
    if (!endsWith(res$url, tail_path)) {
      return(NULL)
    }
    cached <<- substring(res$url, 1, nchar(res$url) - nchar(tail_path))
    cached
  }
})

# The annotation code in a counts filename, G026 for human and M023 for mouse.
# Read from a real URL rather than written down, once for each organism.
recount3_annotation_code <- local({
  cached <- list()
  function(organism, project_home) {
    key <- as.character(organism)
    if (!is.null(cached[[key]])) {
      return(cached[[key]])
    }
    url <- recount3_counts_url("PLACEHOLDER", project_home, organism)
    # The filename is <source>.gene_sums.<project>.<annotation>.gz, so the
    # annotation is the second field from the end. Split rather than match, to
    # keep this readable.
    parts <- rev(strsplit(basename(url), ".", fixed = TRUE)[[1L]])
    code <- parts[[2L]]
    cached[[key]] <<- code
    code
  }
})

# Build the counts URL without calling locate_url().
#
# The layout is <base><organism>/<project_home>/gene_sums/<shard>/<project>/
# <source>.gene_sums.<project>.<annotation>.gz, where the shard is the last two
# characters of the accession. Verified against locate_url() for every
# organism and source combination in the catalog.
recount3_counts_url_fast <- function(project, project_home, organism, base) {
  code <- recount3_annotation_code(organism, project_home)
  source <- basename(project_home)
  shard <- substring(project, nchar(project) - 1L)
  paste0(
    base,
    organism,
    "/",
    project_home,
    "/gene_sums/",
    shard,
    "/",
    project,
    "/",
    source,
    ".gene_sums.",
    project,
    ".",
    code,
    ".gz"
  )
}

# Size in MB from a URL that needs no redirect. Returns NA when the server
# sends no Content-Length, so a fallback stays distinguishable from a real
# measurement.
content_length_mb <- function(url, handle) {
  res <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) NULL
  )
  if (is.null(res) || res$status_code >= 300) {
    return(NA_real_)
  }
  headers <- rawToChar(res$headers)
  match <- regmatches(
    headers,
    regexpr("(?i)content-length:[[:space:]]*[0-9]+", headers, perl = TRUE)
  )
  if (!length(match)) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", match[[1L]])) / 1e6
}

# Prove the shortcut agrees with locate_url() before trusting it on 19,000
# studies. Returns TRUE only when every sampled URL matches.
verify_fast_urls <- function(catalog, base, n = 2L) {
  combos <- unique(catalog[, c("organism", "project_home")])
  for (i in seq_len(nrow(combos))) {
    rows <- catalog[
      catalog$organism == combos$organism[i] &
        catalog$project_home == combos$project_home[i],
      ,
      drop = FALSE
    ]
    for (j in seq_len(min(n, nrow(rows)))) {
      slow <- recount3_counts_url(
        rows$project[j],
        rows$project_home[j],
        rows$organism[j]
      )
      fast <- recount3_counts_url_fast(
        rows$project[j],
        rows$project_home[j],
        rows$organism[j],
        base = ""
      )
      marker <- "/recount3/"
      at <- regexpr(marker, slow, fixed = TRUE)
      if (!identical(substring(slow, at + nchar(marker)), fast)) {
        return(FALSE)
      }
    }
  }
  TRUE
}
