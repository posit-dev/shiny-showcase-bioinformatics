# recount3 access layer. Shiny-free: plain arguments in, plain data structures
# out, so everything here can be tested without a running app.

# Is recount3 installed? Answered without loading it.
#
# system.file() looks on disk and returns "" when the package is absent. Use
# this on the main process. requireNamespace() would answer the same question
# but loads the package to do it, which costs 5.3 seconds and pulls in 98
# namespaces. Browsing the catalog never needs recount3: the catalog is a local
# file and create_rse() runs on a mirai daemon in its own process.
recount3_installed <- function() {
  nzchar(system.file(package = "recount3"))
}

# Is recount3 loadable? Call this only where loading it is wanted anyway,
# which means the daemon, never the main process during a browse.
recount3_available <- function() {
  requireNamespace("recount3", quietly = TRUE)
}

# Catalog of available studies for one organism. Returns a data.frame with
# project, organism, file_source, project_home, project_type, n_samples.
# recount3 caches the catalog via BiocFileCache, so only the first call
# per machine hits the network.
fetch_project_catalog <- function(organism = c("human", "mouse")) {
  organism <- match.arg(organism)
  projects <- recount3::available_projects(organism = organism)
  projects[projects$project_type == "data_sources", , drop = FALSE]
}

# Load one study (a single row of the catalog) as a RangedSummarizedExperiment
# with a "counts" assay of scaled read counts.
load_study <- function(proj_info) {
  stopifnot(is.data.frame(proj_info), nrow(proj_info) == 1)
  rse <- recount3::create_rse(proj_info)
  SummarizedExperiment::assay(rse, "counts") <- recount3::transform_counts(rse)
  rse
}

# log2 CPM from the scaled counts, computed once at load time and shared by
# the gene explorer and PCA modules.
log_cpm <- function(rse) {
  counts <- SummarizedExperiment::assay(rse, "counts")
  lib <- pmax(colSums(counts), 1)
  log2(sweep(counts, 2, lib, "/") * 1e6 + 1)
}

# Named vector for the gene selectize: values are gene ids, labels are
# "symbol (id)" so both are searchable.
gene_choices <- function(rse) {
  ids <- rownames(rse)
  symbols <- gene_symbols(rse)
  stats::setNames(ids, paste0(unname(symbols[ids]), " (", ids, ")"))
}

# Gene id to display symbol, falling back to the id where no symbol is
# recorded. Named by gene id so a lookup is a subset, not a match.
gene_symbols <- function(rse) {
  rd <- SummarizedExperiment::rowData(rse)
  ids <- rownames(rse)
  symbols <- if ("gene_name" %in% names(rd)) as.character(rd$gene_name) else ids
  blank <- is.na(symbols) | !nzchar(symbols)
  symbols[blank] <- ids[blank]
  stats::setNames(symbols, ids)
}

# Metadata columns usable for grouping: categorical, with a sane number of
# levels for a boxplot or a color scale.
#
# Returns a named vector, so the control says how many groups a column will
# make before you pick it. Sorted by that count, because a two-level column is
# almost always the one worth plotting and a twenty-level one almost never is.
metadata_group_choices <- function(rse, max_levels = 30) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  n_samples <- ncol(rse)
  counts <- vapply(
    cd,
    function(x) {
      if (is.list(x) || !(is.character(x) || is.factor(x) || is.logical(x))) {
        return(NA_integer_)
      }
      x <- x[!is.na(x)]
      length(unique(x))
    },
    integer(1)
  )
  # A column with one distinct value per sample is an identifier, not a
  # grouping. Grouping by it makes one box per sample, which is noise.
  ok <- !is.na(counts) &
    counts >= 2 &
    counts <= max_levels &
    !(counts == n_samples & n_samples > 2)

  keep <- names(cd)[ok]
  keep <- keep[order(counts[ok], keep)]
  stats::setNames(
    keep,
    sprintf("%s (%d groups)", keep, counts[keep])
  )
}

# Columns that carry something a reader can act on.
metadata_informative <- function(cd) {
  vapply(
    cd,
    function(x) {
      if (is.list(x)) {
        return(FALSE)
      }
      x <- x[!is.na(x)]
      length(x) > 0 && length(unique(x)) > 1
    },
    logical(1)
  )
}

# Which columns to show first, and which exist at all.
#
# recount3 gives every sample 175 columns. Showing them all means a horizontal
# scrollbar with no end, and showing the first forty means forty arbitrary
# ones. So the default is chosen: the sample id, the study's own annotation,
# then the quality metrics people judge a sample on. Everything else stays
# available through the column picker and the per-sample detail view.
metadata_columns <- function(rse, max_default = 8L, max_quality = 4L) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  informative <- names(cd)[metadata_informative(cd)]

  # Short values first: a column of free-text abstracts is informative and
  # still ruins the table it lands in.
  width_of <- function(name) {
    stats::median(nchar(as.character(cd[[name]])), na.rm = TRUE)
  }
  study_cols <- grep("^(sra|study)[.]", informative, value = TRUE)
  study_cols <- Filter(function(n) isTRUE(width_of(n) <= 40), study_cols)
  quality_cols <- intersect(unname(QC_METRICS), informative)

  # The quality metrics get their own budget rather than queueing behind the
  # study annotation. An SRA study can carry thirty sra.* columns, and without
  # a reserved share they would fill every slot and push out the numbers the
  # view exists to show.
  quality_take <- utils::head(quality_cols, max_quality)
  id_cols <- c("sample", intersect("external_id", informative))
  study_take <- utils::head(
    setdiff(study_cols, id_cols),
    max(max_default - length(id_cols) - length(quality_take), 0L)
  )

  default <- utils::head(
    unique(c(id_cols, study_take, quality_take, informative)),
    max_default
  )

  list(default = default, all = unique(c("sample", informative)))
}

# Sample metadata for display, in the requested columns.
metadata_table <- function(rse, columns = NULL) {
  cd <- as.data.frame(SummarizedExperiment::colData(rse))
  cd <- cbind(sample = colnames(rse), cd, stringsAsFactors = FALSE)
  if (is.null(columns)) {
    columns <- metadata_columns(rse)$default
  }
  columns <- unique(c("sample", intersect(columns, names(cd))))
  flat <- lapply(cd[columns], function(x) {
    if (is.list(x)) {
      return(vapply(
        x,
        function(v) paste(as.character(v), collapse = "; "),
        character(1)
      ))
    }
    x
  })
  as.data.frame(flat, stringsAsFactors = FALSE, check.names = FALSE)
}

# Every field recount3 records for one sample, as field and value rows.
#
# This is what makes the wide table unnecessary: the answer to "what else does
# this sample have" is one click down, not 170 columns to the right.
sample_detail <- function(rse, sample) {
  idx <- match(sample, colnames(rse))
  if (is.na(idx)) {
    return(NULL)
  }
  cd <- SummarizedExperiment::colData(rse)
  value <- vapply(
    names(cd),
    function(name) {
      v <- cd[[name]][[idx]]
      # Drop the missing parts before pasting: as.character(NA) is the string
      # "NA", which would fill the detail view with rows that say nothing.
      v <- v[!is.na(v)]
      if (is.null(v) || !length(v)) {
        return(NA_character_)
      }
      paste(as.character(v), collapse = "; ")
    },
    character(1)
  )
  out <- data.frame(
    field = names(cd),
    value = unname(value),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$value) & nzchar(out$value), , drop = FALSE]
}

# ---- staged loading ----------------------------------------------------------

# Loading happens on a mirai daemon, which is a separate process with no way to
# push a value back before it finishes. So the daemon writes its current step
# to a small file and the main process reads it. Returning intermediate values
# instead would mean serializing the whole RangedSummarizedExperiment twice.
#
# The write is atomic: a temporary file and a rename, so a reader never catches
# a half-written line.
write_load_progress <- function(path, step, total, label) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  tmp <- paste0(path, ".tmp")
  writeLines(paste(step, total, label, sep = "\t"), tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(NULL)
}

read_load_progress <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  line <- tryCatch(
    readLines(path, n = 1L, warn = FALSE),
    error = function(e) character()
  )
  if (!length(line) || !nzchar(line[[1L]])) {
    return(NULL)
  }
  parts <- strsplit(line[[1L]], "\t", fixed = TRUE)[[1L]]
  if (length(parts) < 3L) {
    return(NULL)
  }
  list(
    step = as.integer(parts[[1L]]),
    total = as.integer(parts[[2L]]),
    label = parts[[3L]]
  )
}

LOAD_STAGES <- 6L

# Load one study, reporting each step as it goes.
#
# The first three steps are the files create_rse() needs, fetched here rather
# than inside it, which is what turns one opaque call into named progress. The
# last three are the work that happens once the files are on disk.
load_study_staged <- function(proj_info, progress_file = NULL) {
  note <- function(step, label) {
    write_load_progress(progress_file, step, LOAD_STAGES, label)
  }

  prefetch_study_files(
    proj_info,
    on_stage = function(step, total, label) note(step, label)
  )

  note(4L, "Building the study object")
  rse <- recount3::create_rse(proj_info)

  note(5L, "Scaling the counts")
  SummarizedExperiment::assay(rse, "counts") <- recount3::transform_counts(rse)

  note(6L, "Computing log2 CPM")
  list(rse = rse, log_expr = log_cpm(rse))
}
