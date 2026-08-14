# Input parsing. Turns an uploaded table or pasted text into a normalized
# candidate tibble with columns: candidate, type, gene, variant, note.
# Pure and offline (no network, no LLM) so it is safe to run on every input.

# A candidate row is one of: a gene symbol, or a variant (HGVS / rsID / g.).
# `type` is a coarse guess used only to route retrieval, never as a claim.
guess_type <- function(x) {
  ifelse(
    grepl("^rs[0-9]+$", x) |
      grepl("[:.]|>|del|ins|dup", x, ignore.case = TRUE) |
      grepl("^[0-9XYM]+-[0-9]+-[ACGT]+-[ACGT]+$", x, ignore.case = TRUE),
    "variant",
    "gene"
  )
}

# Parse pasted text: one candidate per line, blank lines and comments dropped.
parse_candidate_lines <- function(text) {
  lines <- trimws(strsplit(text, "\r?\n")[[1]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0) {
    stop("No candidates found in pasted text.", call. = FALSE)
  }
  type <- guess_type(lines)
  tibble::tibble(
    candidate = lines,
    type = type,
    gene = ifelse(type == "gene", lines, NA_character_),
    variant = ifelse(type == "variant", lines, NA_character_),
    note = NA_character_
  )
}

# Parse an uploaded table. Accepts TSV/CSV; a `gene`/`candidate`/`symbol` column
# is required. Extra columns are preserved as-is on the returned tibble.
read_candidate_table <- function(path) {
  sep <- if (grepl("\\.csv$", path, ignore.case = TRUE)) "," else "\t"
  df <- utils::read.delim(
    path,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(df) <- tolower(names(df))
  key <- intersect(c("candidate", "gene", "symbol"), names(df))[1]
  if (is.na(key)) {
    stop(
      "Table needs a 'candidate', 'gene', or 'symbol' column.",
      call. = FALSE
    )
  }
  candidate <- as.character(df[[key]])
  type <- if ("type" %in% names(df)) {
    as.character(df$type)
  } else {
    guess_type(candidate)
  }
  tibble::tibble(
    candidate = candidate,
    type = type,
    gene = if ("gene" %in% names(df)) as.character(df$gene) else candidate,
    variant = if ("variant" %in% names(df)) {
      as.character(df$variant)
    } else {
      NA_character_
    },
    note = if ("note" %in% names(df)) as.character(df$note) else NA_character_
  )
}

# --- Bundled examples -------------------------------------------------------
# Public/synthetic example candidate lists in data/examples/, used by the UI's
# "load example" button. `dir` is injectable so the helper is testable offline.

# Path to a bundled example table (e.g. "nf1_candidates" -> the NF1 gene list).
example_path <- function(name, dir = file.path("data", "examples")) {
  file.path(dir, paste0(name, ".tsv"))
}

# Read a bundled example and return its candidates as newline-joined text,
# ready to pre-fill the paste box. The parse path then handles it like any paste.
example_text <- function(name, dir = file.path("data", "examples")) {
  df <- read_candidate_table(example_path(name, dir))
  paste(df$candidate, collapse = "\n")
}

# Read a plain gene-list file: a single column of symbols, one per line, with NO
# header row - the format every UI upload accepts. Handles .txt / .tsv / .csv by
# taking the first field of each line (so an extra column is ignored); a lone
# header-like first line ("gene", "symbol", ...) is dropped so a headered file
# still works. Comment/blank lines are skipped.
read_gene_list_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  # First field of each line (so an extra column is ignored), unquoted and trimmed,
  # so a quoted CSV (write.csv default) yields clean symbols rather than `"NF1"`.
  first <- trimws(sub("[\t,;].*$", "", lines))
  first <- sub('^"(.*)"$', "\\1", first)
  first <- trimws(sub("^'(.*)'$", "\\1", first))
  first <- first[nzchar(first) & !startsWith(first, "#")]
  # Drop a leading header cell. Deliberately NOT "id"/"ids": IDS (iduronate
  # 2-sulfatase) is a real HGNC symbol, so treating "ids" as a header would silently
  # drop a candidate; the words kept here collide with no approved gene symbol.
  header_words <- c(
    "gene",
    "genes",
    "symbol",
    "symbols",
    "candidate",
    "candidates"
  )
  if (length(first) > 0 && tolower(first[1]) %in% header_words) {
    first <- first[-1]
  }
  if (length(first) == 0) {
    stop("No gene symbols found in the file.", call. = FALSE)
  }
  first
}

# Lenient reader for a user upload: use a structured table's gene/candidate/symbol
# column when the file has that header, otherwise fall back to a headerless
# single-column gene list. Returns a character vector of candidate tokens, so both
# a proper table and a bare one-per-line list "just work". A file that parses but
# yields no symbols (e.g. header row only) errors, so collect_candidate_set records
# it rather than silently dropping the source.
read_candidate_file <- function(path) {
  tbl <- tryCatch(read_candidate_table(path), error = function(e) NULL)
  if (!is.null(tbl)) {
    genes <- as.character(tbl$candidate)
    genes <- genes[!is.na(genes) & nzchar(trimws(genes))]
    if (length(genes) == 0) {
      stop("No gene symbols found in the file.", call. = FALSE)
    }
    return(genes)
  }
  read_gene_list_file(path)
}

# A multi-source example: four candidate lists from different assays for the same
# NF1-associated MPNST study, tagged by assay type. It demonstrates cross-source
# corroboration (a gene found in more of your own lists ranks higher - e.g. CDK4,
# AURKA, EGFR, SOX9 recur across lists) and the caveats/veto stage (the sequencing-
# artifact genes TTN, MUC16 in the WES list sink despite passing raw signal). Only
# public gene symbols; illustrative candidate inputs, not biological claims. The
# UI's "Load 4-source example" button pre-fills these as tagged sources.
genescout_multisource_example <- function() {
  list(
    list(
      label = "WES somatic calls",
      type = "wes",
      genes = c(
        "NF1",
        "SUZ12",
        "EED",
        "CDKN2A",
        "TP53",
        "PTEN",
        "RB1",
        "NF2",
        "EGFR",
        "TTN",
        "MUC16"
      )
    ),
    list(
      label = "Bulk RNA-seq DEGs",
      type = "rnaseq_deg",
      genes = c(
        "SOX9",
        "TWIST1",
        "CDK4",
        "MDM2",
        "AURKA",
        "BIRC5",
        "TOP2A",
        "MKI67",
        "CENPF",
        "EGFR"
      )
    ),
    list(
      label = "ATAC-seq enriched",
      type = "atacseq",
      genes = c(
        "SOX10",
        "FOXD3",
        "PAX3",
        "EGR2",
        "POU3F1",
        "TFAP2A",
        "SOX9"
      )
    ),
    list(
      label = "CRISPR dependency screen",
      type = "crispr",
      genes = c(
        "PLK1",
        "WEE1",
        "CHEK1",
        "KIF11",
        "EZH2",
        "AURKA",
        "CDK4",
        "BRD4"
      )
    )
  )
}

# Assemble the named gene lists the pipeline consumes from the paste box and an
# optional uploaded table. Each source becomes one named list, so a gene's origin
# is retained through de-duplication. Returns list() when nothing was provided.
collect_gene_lists <- function(pasted, file_path = NULL) {
  lists <- list()
  if (!is.null(pasted) && nzchar(trimws(pasted %||% ""))) {
    lists[["pasted"]] <- strsplit(pasted, "\r?\n")[[1]]
  }
  if (!is.null(file_path) && nzchar(file_path)) {
    tbl <- tryCatch(read_candidate_table(file_path), error = function(e) NULL)
    if (!is.null(tbl)) {
      lists[["uploaded"]] <- as.character(tbl$candidate)
    }
  }
  lists
}

# --- Multi-source candidate model -------------------------------------------
# A `candidate_set` is GeneScout's canonical, UI-agnostic input: an ordered list of
# `genescout_source` records, each a named/typed bag of raw gene tokens. The bare
# named list of character vectors the pipeline used before is a strict subset
# (one source per name), so every existing caller keeps working through
# as_candidate_set() / as_gene_lists(). Everything is a plain list, so a set
# round-trips through jsonlite for a non-R frontend (Shiny-Python, React, CLI).

# Slug a human label into an id-safe token (lowercase, non-alnum -> '-'). Colons
# are stripped, so a user source id can never collide with a reserved 'seed:' id.
slugify <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("(^-+)|(-+$)", "", s)
  if (nzchar(s)) s else "source"
}

# Suggested (free-text) source types for the UI datalist / CLI hint. `type` stays
# free text and is never treated as a claim - this is only a vocabulary.
genescout_source_types <- function() {
  c(
    "unspecified",
    "wes",
    "wgs",
    "rnaseq_deg",
    "atacseq",
    "chipseq",
    "crispr",
    "proteomics",
    "gwas",
    "panel",
    "curated",
    "literature",
    "disease_seed"
  )
}

# One tagged source of candidate genes. `genes` are raw tokens (symbols or free
# text; cleaned later by the flatten step). `seeded = TRUE` marks a source the
# engine injected (e.g. a disease-discovery universe) so it is excluded from
# cross-source corroboration. Classed for robust dispatch; still a plain list.
genescout_source <- function(
  genes,
  label = NULL,
  type = "unspecified",
  note = NULL,
  id = NULL,
  seeded = FALSE
) {
  label <- label %||% "input"
  structure(
    list(
      id = as.character(id %||% slugify(label)),
      label = as.character(label),
      type = as.character(type %||% "unspecified"),
      note = if (is.null(note)) NA_character_ else as.character(note),
      seeded = isTRUE(seeded),
      genes = as.character(genes)
    ),
    class = "genescout_source"
  )
}

# Make source ids unique within a set (a collision gets a '-2', '-3' suffix) so
# per-source provenance never merges two distinct sources downstream.
dedupe_source_ids <- function(sources) {
  seen <- character()
  for (i in seq_along(sources)) {
    id <- sources[[i]]$id %||% slugify(sources[[i]]$label %||% "source")
    base <- id
    k <- 2L
    while (id %in% seen) {
      id <- paste0(base, "-", k)
      k <- k + 1L
    }
    seen <- c(seen, id)
    sources[[i]]$id <- id
  }
  sources
}

# The low-level constructor: an ordered, classed collection of genescout_source
# records (ids made unique). Order is the display / tie-break order.
new_candidate_set <- function(sources = list()) {
  structure(dedupe_source_ids(sources), class = "genescout_candidate_set")
}

# Convenience constructor: candidate_set(src1, src2, ...) or candidate_set(<list
# of sources>). Every frontend builds its input through this.
candidate_set <- function(...) {
  sources <- list(...)
  if (
    length(sources) == 1L &&
      is.list(sources[[1]]) &&
      !inherits(sources[[1]], "genescout_source")
  ) {
    sources <- sources[[1]]
  }
  new_candidate_set(sources)
}

# Append a genescout_source to a candidate_set, returning a new set (ids re-deduped).
# Used to fold an engine-injected source (e.g. the disease-discovery universe)
# into the user's set without mutating it.
candidate_set_add <- function(cs, source) {
  new_candidate_set(c(unclass(as_candidate_set(cs)), list(source)))
}

# Coerce assorted inputs into a candidate_set. Supersedes as_gene_lists(): passes
# a candidate_set through; wraps a single genescout_source; a bare character vector
# -> one source "input"; a gene/candidate/symbol data frame -> one source; and a
# NAMED list of character vectors (the pipeline's old shape) -> one source per
# name with id = label = name, so downstream provenance (input_lists) stays
# byte-identical. Dispatch on the classes FIRST (a candidate_set is itself a
# list, so a bare is.list() branch would mis-handle it).
as_candidate_set <- function(x) {
  if (inherits(x, "genescout_candidate_set")) {
    return(x)
  }
  if (inherits(x, "genescout_source")) {
    return(new_candidate_set(list(x)))
  }
  if (is.data.frame(x)) {
    col <- intersect(c("gene", "candidate", "symbol"), names(x))[1]
    genes <- if (!is.na(col)) as.character(x[[col]]) else character()
    return(new_candidate_set(list(genescout_source(genes, label = "input"))))
  }
  if (is.character(x)) {
    return(new_candidate_set(list(genescout_source(x, label = "input"))))
  }
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) {
      nms <- rep("", length(x))
    }
    srcs <- lapply(seq_along(x), function(i) {
      nm <- nms[i]
      if (!nzchar(nm %||% "")) {
        nm <- if (length(x) == 1L) "input" else paste0("source_", i)
      }
      genescout_source(x[[i]], label = nm, id = nm)
    })
    return(new_candidate_set(srcs))
  }
  new_candidate_set(list())
}

# Down-convert a candidate_set to the named-list-of-character-vectors shape older
# callers expect (one entry per source label). The back-compat inverse of the
# named-list branch of as_candidate_set().
candidate_set_to_named_lists <- function(cs) {
  cs <- as_candidate_set(cs)
  out <- list()
  for (src in cs) {
    out[[src$label]] <- src$genes
  }
  out
}

# Convert a candidate_set to a plain, unclassed list-of-lists ready for jsonlite
# (a non-R frontend / the plumber API). `note` is omitted when absent and `genes`
# is kept an array even at length 1. The inverse is candidate_set_from_list().
candidate_set_to_list <- function(cs) {
  cs <- as_candidate_set(cs)
  lapply(cs, function(s) {
    out <- list(
      id = s$id,
      label = s$label,
      type = s$type,
      seeded = s$seeded,
      genes = I(as.character(s$genes))
    )
    if (!is.na(s$note)) {
      out$note <- s$note
    }
    out
  })
}

# Rebuild a candidate_set from the plain list a frontend/API posts (the parsed
# JSON, with simplifyVector = FALSE). The inverse of candidate_set_to_list().
candidate_set_from_list <- function(x) {
  srcs <- lapply(x, function(s) {
    genescout_source(
      genes = unlist(s$genes, use.names = FALSE) %||% character(),
      label = s$label,
      type = s$type %||% "unspecified",
      note = s$note,
      id = s$id,
      seeded = isTRUE(s$seeded)
    )
  })
  new_candidate_set(srcs)
}

# Build a candidate_set from UI/CLI source specs. Each spec is a list with a
# `name` (label), optional `type`, and one of: `genes` (a vector), `text`
# (pasted, newline-separated), or `file` (a table path). Empty sources are
# dropped so a blank "+ add a source" row never becomes a phantom source. This is
# the single constructor Shiny, the CLI, and the API all call.
collect_candidate_set <- function(specs) {
  srcs <- list()
  errors <- list()
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    label <- spec$name %||% spec$label %||% paste0("source_", i)
    genes <- character()
    if (!is.null(spec$genes)) {
      genes <- as.character(spec$genes)
    } else {
      # A source can be filled by paste AND/OR upload; combine both. The upload
      # accepts a headerless single-column list or a structured table.
      if (!is.null(spec$text) && nzchar(trimws(spec$text %||% ""))) {
        genes <- c(genes, strsplit(spec$text, "\r?\n")[[1]])
      }
      if (!is.null(spec$file) && nzchar(spec$file)) {
        # A malformed upload is recorded, not silently dropped, so a caller can tell
        # the user why their file contributed nothing.
        fg <- tryCatch(
          read_candidate_file(spec$file),
          error = function(e) {
            errors[[length(errors) + 1L]] <<- list(
              source = label,
              message = conditionMessage(e)
            )
            character()
          }
        )
        genes <- c(genes, fg)
      }
    }
    genes <- genes[!is.na(genes) & nzchar(trimws(genes))]
    if (length(genes) == 0L) {
      next
    }
    srcs[[length(srcs) + 1L]] <- genescout_source(
      genes,
      label = label,
      type = spec$type %||% "unspecified"
    )
  }
  cs <- new_candidate_set(srcs)
  if (length(errors) > 0) {
    attr(cs, "parse_errors") <- errors
  }
  cs
}

# The parse errors collect_candidate_set attached (a list of list(source, message)),
# or NULL when every source parsed. Lets a UI surface an unreadable upload instead
# of silently dropping it.
candidate_parse_errors <- function(cs) {
  attr(cs, "parse_errors", exact = TRUE)
}

# Dispatch on an input source list(file = <path|NULL>, text = <chr|NULL>).
parse_candidates <- function(source) {
  if (!is.null(source$file) && nzchar(source$file)) {
    return(read_candidate_table(source$file))
  }
  if (!is.null(source$text) && nzchar(trimws(source$text %||% ""))) {
    return(parse_candidate_lines(source$text))
  }
  stop(
    "No candidates provided - upload a table or paste gene symbols.",
    call. = FALSE
  )
}

# Null/empty-coalescing helper used above.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x # nolint: object_name_linter.
