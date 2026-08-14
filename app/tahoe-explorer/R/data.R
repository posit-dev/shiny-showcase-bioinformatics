# Data access layer for Tahoe-100M metadata.
#
# Every module reads through these helpers rather than touching files directly.
# Resolution order for each table is: real file in the data dir -> bundled
# synthetic fixture. The app therefore always runs, even with no network and no
# downloaded data.

# Small in-process cache so repeated reactive reads are cheap.
.tahoe_cache <- new.env(parent = emptyenv())

# App root, captured when this file is sourced. global.R is always loaded with
# the working directory at the app root (by runApp, and by the tests via
# source(chdir = TRUE)), so anchoring data paths here keeps them correct even
# when tests later run from tests/testthat/.
.tahoe_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

# Columns we allow callers to group/filter on in the cell-level obs queries.
# Whitelisting keeps the generated SQL safe and predictable.
.tahoe_obs_group_cols <- c(
  "drug",
  "cell_line",
  "cell_name",
  "plate",
  "sample",
  "phase",
  "pass_filter",
  "sublibrary"
)

# Supported obs aggregation metrics mapped to their SQL expressions.
.tahoe_obs_metrics <- c(
  n_cells = "count(*)",
  mean_pcnt_mito = "avg(pcnt_mito)",
  mean_tscp_count = "avg(tscp_count)",
  mean_gene_count = "avg(gene_count)",
  mean_mread_count = "avg(mread_count)"
)

# Pinned Tahoe-100M dataset revision (HuggingFace commit SHA) for reproducibility.
# The default branch is mutable: an upstream re-upload would otherwise silently
# change every obs-derived number and any recipe handed off downstream. Bump this
# deliberately when adopting a newer dataset release (also update the copies in
# dev/download_metadata.R, which is a standalone script and cannot see this).
.tahoe_dataset_repo <- "tahoebio/Tahoe-100M"
.tahoe_dataset_revision <- "2dc57900b7981cfcf5e211527169a0b006546a95"

#' The pinned dataset repo + revision, for display (About) and recipe headers.
tahoe_dataset_pin <- function() {
  list(repo = .tahoe_dataset_repo, revision = .tahoe_dataset_revision)
}

# Remote location of the cell-level obs file (used only when opted in), pinned to
# the revision above. Uses duckdb's hf:// protocol, which works anonymously for
# this public dataset and picks up a HuggingFace token (see .tahoe_hf_token) for
# authenticated access.
.tahoe_obs_remote_url <- sprintf(
  "hf://datasets/%s@%s/metadata/obs_metadata.parquet",
  .tahoe_dataset_repo,
  .tahoe_dataset_revision
)

#' HuggingFace access token from the environment, or "" if unset. Checked in
#' order; enables authenticated (higher rate limit / gated) remote access.
tahoe_hf_token <- function() {
  for (var in c("HF_TOKEN", "HUGGING_FACE_HUB_TOKEN", "HUGGINGFACE_TOKEN")) {
    tok <- Sys.getenv(var, unset = "")
    if (nzchar(tok)) {
      return(tok)
    }
  }
  ""
}

#' Directory holding downloaded metadata (TAHOE_METADATA_DIR or "data").
tahoe_data_dir <- function() {
  env <- Sys.getenv("TAHOE_METADATA_DIR", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(env)
  }
  file.path(.tahoe_root, "data")
}

#' Directory holding the committed synthetic fixtures.
tahoe_fixture_dir <- function() {
  file.path(.tahoe_root, "data", "fixtures")
}

#' Resolve a small table to a real file if present, else its fixture.
#' Returns a list(path, source) where source is "real" or "fixture".
tahoe_table_path <- function(name) {
  real <- file.path(tahoe_data_dir(), paste0(name, ".parquet"))
  if (file.exists(real)) {
    return(list(path = real, source = "real"))
  }
  list(
    path = file.path(tahoe_fixture_dir(), paste0(name, ".parquet")),
    source = "fixture"
  )
}

# Apply optional duckdb resource caps for shared/hosted deployments, so one heavy
# query cannot exhaust the host. No-ops unless the env vars are set, so local and
# CI behaviour is unchanged. TAHOE_DUCKDB_MEMORY_LIMIT (e.g. "4GB"),
# TAHOE_DUCKDB_THREADS (e.g. "2"). Best-effort (wrapped in try()).
.tahoe_apply_limits <- function(con) {
  mem <- Sys.getenv("TAHOE_DUCKDB_MEMORY_LIMIT", unset = "")
  if (nzchar(mem)) {
    try(
      DBI::dbExecute(con, sprintf("SET memory_limit=%s", .tahoe_quote(mem))),
      silent = TRUE
    )
  }
  threads <- suppressWarnings(as.integer(
    Sys.getenv("TAHOE_DUCKDB_THREADS", unset = "")
  ))
  if (!is.na(threads) && threads > 0) {
    try(DBI::dbExecute(con, sprintf("SET threads=%d", threads)), silent = TRUE)
  }
  invisible(con)
}

#' Cached duckdb connection for the app's lifetime.
tahoe_con <- function() {
  if (is.null(.tahoe_cache$con) || !DBI::dbIsValid(.tahoe_cache$con)) {
    .tahoe_cache$con <- DBI::dbConnect(duckdb::duckdb())
    # A fresh connection has no extensions loaded; clear the httpfs flag so a
    # later remote query re-loads it instead of assuming the old connection's
    # state.
    .tahoe_cache$httpfs <- FALSE
    .tahoe_apply_limits(.tahoe_cache$con)
  }
  .tahoe_cache$con
}

# Single-quote a string for safe inlining into duckdb SQL.
.tahoe_quote <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

#' Read a parquet (or csv) file into a tibble via duckdb.
tahoe_read_file <- function(path) {
  reader <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    "read_csv_auto"
  } else {
    "read_parquet"
  }
  sql <- sprintf("SELECT * FROM %s(%s)", reader, .tahoe_quote(path))
  dplyr::as_tibble(DBI::dbGetQuery(tahoe_con(), sql))
}

#' Load a small metadata table by name, cached. Returns a tibble plus a
#' "tahoe_source" attribute ("real" or "fixture").
tahoe_table <- function(name) {
  key <- paste0("table_", name)
  if (is.null(.tahoe_cache[[key]])) {
    resolved <- tahoe_table_path(name)
    df <- tahoe_read_file(resolved$path)
    attr(df, "tahoe_source") <- resolved$source
    .tahoe_cache[[key]] <- df
  }
  .tahoe_cache[[key]]
}

#' Typed wrappers for the four small tables.
tahoe_drug <- function() tahoe_table("drug_metadata")
tahoe_cell_line <- function() tahoe_table("cell_line_metadata")
tahoe_sample <- function() tahoe_table("sample_metadata")
tahoe_gene <- function() tahoe_table("gene_metadata")

#' Per cell-line somatic variant calls (one row per variant) from external
#' sources -- DepMap full somatic profiles plus a Cellosaurus curated-driver
#' fallback -- carrying a `source` column and keyed on `cell_name`. Resolves
#' data/cell_line_variants.parquet if present, else the synthetic fixture, so it
#' stays offline-safe and metadata-only. Absent until dev/download_variants.R runs.
tahoe_cell_variants <- function() tahoe_table("cell_line_variants")

#' Target gene symbols for a single drug name, parsed from the drug table's
#' comma/semicolon-separated `targets` column. character(0) if unknown.
tahoe_drug_targets <- function(drug_name) {
  if (length(drug_name) != 1 || is.na(drug_name) || !nzchar(drug_name)) {
    return(character(0))
  }
  d <- tahoe_drug()
  if (!all(c("drug", "targets") %in% names(d))) {
    return(character(0))
  }
  row <- d[d$drug == drug_name, , drop = FALSE]
  if (nrow(row) == 0) {
    return(character(0))
  }
  parts <- trimws(unlist(strsplit(as.character(row$targets[[1]]), "[,;]")))
  unique(parts[!is.na(parts) & nzchar(parts)])
}

#' Assayed cell lines carrying a somatic variant in any of `genes`. Joins the
#' variant table to the lines actually present in the obs grid, so only
#' experimentally-assayed lines are returned -- the set over which a
#' mutant-vs-wildtype contrast could be designed. One row per matching (cell
#' line, variant). Empty tibble when there is no variant data or no match.
tahoe_target_mutations <- function(genes) {
  genes <- unique(genes[!is.na(genes) & nzchar(genes)])
  empty <- dplyr::tibble(
    cell_name = character(),
    gene = character(),
    protein_change = character(),
    source = character()
  )
  if (length(genes) == 0) {
    return(empty)
  }
  v <- tryCatch(tahoe_cell_variants(), error = function(e) NULL)
  if (
    is.null(v) || nrow(v) == 0 || !all(c("cell_name", "gene") %in% names(v))
  ) {
    return(empty)
  }
  assayed <- unique(tahoe_cell_grid()$cell_name)
  dplyr::as_tibble(
    v[v$gene %in% genes & v$cell_name %in% assayed, , drop = FALSE]
  )
}

#' Parse the `drugname_drugconc` string, e.g. "[('Drug', 5.0, 'uM')]", into a
#' tibble of (conc, unit). Returns NA for values it cannot parse.
tahoe_parse_dose <- function(x) {
  x <- as.character(x)
  conc <- suppressWarnings(as.numeric(
    stringr::str_match(x, ",\\s*([0-9.eE+-]+)\\s*,")[, 2]
  ))
  unit <- stringr::str_match(x, ",\\s*'([^']*)'\\s*\\)")[, 2]
  dplyr::tibble(conc = conc, unit = unit)
}

# TAHOE_OBS_REMOTE truthy? (set, and not "false"/"0").
.tahoe_remote_opted <- function() {
  v <- Sys.getenv("TAHOE_OBS_REMOTE", unset = "")
  nzchar(v) && !identical(tolower(v), "false") && v != "0"
}

#' Resolve where cell-level obs data comes from -- a single policy, so Overview
#' and the Cells tab can never disagree about the source within a session.
#' Returns list(type = "local"|"remote"|"fixture", src). A local download always
#' wins; otherwise `purpose` decides when the 2.29 GB remote obs is worth it:
#'  - "counts": cheap one-shot headline aggregates -- go remote when the small
#'    tables are real (the user downloaded data and wants real numbers) or
#'    TAHOE_OBS_REMOTE is set.
#'  - "summary": interactive per-cell aggregation -- go remote ONLY when
#'    explicitly opted in, so browsing never silently fires repeated 2.29 GB
#'    scans; otherwise stay on the fast fixture (with a clear provenance badge).
resolve_obs_source <- function(purpose = c("summary", "counts")) {
  purpose <- match.arg(purpose)
  local <- file.path(tahoe_data_dir(), "obs_metadata.parquet")
  if (file.exists(local)) {
    return(list(type = "local", src = local))
  }
  real_tables <- identical(attr(tahoe_drug(), "tahoe_source"), "real")
  go_remote <- .tahoe_remote_opted() ||
    (identical(purpose, "counts") && real_tables)
  if (go_remote) {
    return(list(type = "remote", src = .tahoe_obs_remote_url))
  }
  list(
    type = "fixture",
    src = file.path(tahoe_fixture_dir(), "obs_metadata.parquet")
  )
}

# Backward-compatible wrappers around the single policy above: per-cell summary
# queries (Cells tab) vs. the headline count aggregates (Overview).
tahoe_obs_source <- function() resolve_obs_source("summary")
.tahoe_obs_count_source <- function() resolve_obs_source("counts")

# Ensure httpfs is available before querying a remote parquet, and register a
# HuggingFace token as a duckdb secret when one is configured.
.tahoe_load_httpfs <- function() {
  if (isTRUE(.tahoe_cache$httpfs)) {
    return(invisible(TRUE))
  }
  con <- tahoe_con()
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  # Bounded retries/timeout so a flaky remote read fails cleanly instead of
  # hanging a session (best-effort tuning; ignore if a knob is unavailable).
  try(DBI::dbExecute(con, "SET http_retries=3"), silent = TRUE)
  try(DBI::dbExecute(con, "SET http_timeout=60000"), silent = TRUE)
  token <- tahoe_hf_token()
  if (nzchar(token)) {
    DBI::dbExecute(
      con,
      sprintf(
        "CREATE OR REPLACE SECRET tahoe_hf (TYPE HUGGINGFACE, TOKEN %s)",
        .tahoe_quote(token)
      )
    )
  }
  .tahoe_cache$httpfs <- TRUE
  invisible(TRUE)
}

# Build the WHERE clause from a whitelisted, value-escaped filter list.
.tahoe_obs_where <- function(filters) {
  filters <- filters[
    names(filters) %in%
      .tahoe_obs_group_cols &
      vapply(filters, function(v) length(v) > 0, logical(1))
  ]
  if (length(filters) == 0) {
    return("")
  }
  clauses <- vapply(
    names(filters),
    function(col) {
      vals <- paste(
        vapply(filters[[col]], .tahoe_quote, character(1)),
        collapse = ", "
      )
      sprintf("\"%s\" IN (%s)", col, vals)
    },
    character(1)
  )
  paste0(" WHERE ", paste(clauses, collapse = " AND "))
}

#' Lazily aggregate the cell-level obs data with duckdb and return a small
#' tibble. Never pulls raw cells into R. `group_by` and `metric` are
#' whitelisted; `filters` is a named list (column -> allowed values).
#'
#' Returns a tibble with columns `<group_by>` and `value`, carrying a
#' "tahoe_source" attribute. On any failure it returns a 0-row tibble with a
#' "tahoe_error" attribute rather than throwing, so the UI can degrade.
tahoe_obs_summary <- function(
  group_by,
  filters = list(),
  metric = "n_cells",
  limit = 100
) {
  stopifnot(length(group_by) == 1, group_by %in% .tahoe_obs_group_cols)
  if (!metric %in% names(.tahoe_obs_metrics)) {
    metric <- "n_cells"
  }
  source <- tahoe_obs_source()
  agg <- .tahoe_obs_metrics[[metric]]
  where <- .tahoe_obs_where(filters)
  limit_sql <- if (is.null(limit)) {
    ""
  } else {
    sprintf(" LIMIT %d", as.integer(limit))
  }
  sql <- sprintf(
    paste0(
      'SELECT "%s" AS "%s", %s AS value ',
      "FROM read_parquet(%s)%s GROUP BY 1 ORDER BY value DESC%s"
    ),
    group_by,
    group_by,
    agg,
    .tahoe_quote(source$src),
    where,
    limit_sql
  )
  out <- tryCatch(
    {
      if (identical(source$type, "remote")) {
        .tahoe_load_httpfs()
      }
      dplyr::as_tibble(DBI::dbGetQuery(tahoe_con(), sql))
    },
    error = function(e) {
      res <- dplyr::tibble(!!group_by := character(), value = numeric())
      attr(res, "tahoe_error") <- conditionMessage(e)
      res
    }
  )
  attr(out, "tahoe_source") <- source$type
  out
}

# Distinct values of a column, or nrow() if the column is missing. Counts of
# distinct entities, not raw rows: the real cell_line_metadata table is
# driver-level (many rows per cell line), so nrow() would overcount.
.tahoe_n_distinct <- function(df, column) {
  if (column %in% names(df)) {
    dplyr::n_distinct(df[[column]])
  } else {
    nrow(df)
  }
}

#' Cell-line metadata collapsed to one row per cell line. The source table has
#' one row per driver mutation, so this keeps the first row per `cell_name` and
#' summarizes the distinct driver genes into a `drivers` column (with
#' `n_drivers`). Returns the table unchanged if `cell_name` is absent.
tahoe_cell_line_unique <- function() {
  df <- tahoe_cell_line()
  if (!"cell_name" %in% names(df)) {
    return(df)
  }
  drivers <- if ("Driver_Gene_Symbol" %in% names(df)) {
    df |>
      dplyr::group_by(.data$cell_name) |>
      dplyr::summarise(
        drivers = paste(
          sort(unique(stats::na.omit(.data$Driver_Gene_Symbol))),
          collapse = ", "
        ),
        n_drivers = dplyr::n_distinct(
          stats::na.omit(.data$Driver_Gene_Symbol)
        ),
        .groups = "drop"
      )
  } else {
    NULL
  }
  out <- df[!duplicated(df$cell_name), , drop = FALSE]
  if (!is.null(drivers)) {
    out <- dplyr::left_join(out, drivers, by = "cell_name")
  }
  dplyr::as_tibble(out)
}

# Cells and distinct assayed cell lines from the obs data (the actual dataset),
# cached. cell_line_metadata over-annotates (~102 lines) while only ~50 were
# assayed, and the true cell count only exists in obs, so these must come from
# obs rather than the annotation tables. Degrades to NA if obs is unreachable.
.tahoe_obs_counts <- function() {
  if (!is.null(.tahoe_cache$obs_counts)) {
    return(.tahoe_cache$obs_counts)
  }
  # Prefer the prebuilt cell grid: it is derived from the same obs data and
  # yields the identical headline totals (sum of cells, distinct cell lines)
  # from a tiny local file, avoiding a slow remote DISTINCT scan of 100M rows.
  grid_file <- file.path(tahoe_data_dir(), "obs_cell_grid.parquet")
  if (file.exists(grid_file)) {
    grid_res <- tryCatch(
      {
        q <- DBI::dbGetQuery(
          tahoe_con(),
          sprintf(
            paste0(
              "SELECT sum(n_cells) AS cells, ",
              "count(DISTINCT cell_name) AS cell_lines ",
              "FROM read_parquet(%s)"
            ),
            .tahoe_quote(grid_file)
          )
        )
        list(cells = q$cells, cell_lines = q$cell_lines, type = "grid")
      },
      error = function(e) NULL
    )
    if (!is.null(grid_res) && !is.na(grid_res$cells)) {
      .tahoe_cache$obs_counts <- grid_res
      return(grid_res)
    }
  }
  source <- .tahoe_obs_count_source()
  res <- tryCatch(
    {
      if (identical(source$type, "remote")) {
        .tahoe_load_httpfs()
      }
      q <- DBI::dbGetQuery(
        tahoe_con(),
        sprintf(
          paste0(
            "SELECT count(*) AS cells, ",
            "count(DISTINCT cell_name) AS cell_lines ",
            "FROM read_parquet(%s)"
          ),
          .tahoe_quote(source$src)
        )
      )
      list(cells = q$cells, cell_lines = q$cell_lines, type = source$type)
    },
    error = function(e) {
      list(cells = NA_real_, cell_lines = NA_real_, type = source$type)
    }
  )
  # Only cache a real result. Caching the NA error fallback would poison the
  # cache for the whole process after one transient obs failure (the grid path
  # above is guarded the same way).
  if (!is.na(res$cells)) {
    .tahoe_cache$obs_counts <- res
  }
  res
}

#' Headline dataset counts for the Overview tab. Drugs, samples, plates, and
#' genes come from the (accurate) small tables; the count of assayed cell lines
#' and total cells come from the obs data, since the annotation tables would
#' otherwise overcount cell lines and cannot report the cell total.
tahoe_summary_counts <- function() {
  drug <- tahoe_drug()
  sample <- tahoe_sample()
  gene <- tahoe_gene()
  obs <- .tahoe_obs_counts()
  list(
    drugs = .tahoe_n_distinct(drug, "drug"),
    cell_lines = obs$cell_lines,
    samples = .tahoe_n_distinct(sample, "sample"),
    plates = .tahoe_n_distinct(sample, "plate"),
    genes = .tahoe_n_distinct(gene, "gene_symbol"),
    cells = obs$cells,
    obs_source = obs$type,
    data_source = attr(drug, "tahoe_source")
  )
}

# SQL for the per (drug x cell line x plate x dose) grid. `extended` adds the
# QC-tier (pass_filter = 'full'), cell-cycle-phase, and QC-metric-sum aggregates
# the QC/Coverage views use; the minimal form (cell counts only) is the fallback
# for an obs table that lacks those columns. Kept identical to the query in
# dev/build_grid.R so a prebuilt grid and an on-the-fly grid match exactly.
.tahoe_grid_query <- function(src_quoted, extended = TRUE) {
  base_sel <- paste0(
    "SELECT drug, cell_name, plate, TRY_CAST(regexp_extract(",
    "drugname_drugconc, ',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) AS conc, ",
    "count(*) AS n_cells"
  )
  ext_sel <- if (extended) {
    paste0(
      ", count(*) FILTER (WHERE pass_filter = 'full') AS n_full",
      ", count(*) FILTER (WHERE phase = 'G1') AS n_g1",
      ", count(*) FILTER (WHERE phase = 'S') AS n_s",
      ", count(*) FILTER (WHERE phase = 'G2M') AS n_g2m",
      ", sum(pcnt_mito) AS sum_pcnt_mito",
      ", sum(gene_count) AS sum_gene_count",
      ", sum(tscp_count) AS sum_tscp_count"
    )
  } else {
    ""
  }
  sprintf(
    "%s%s FROM read_parquet(%s) GROUP BY 1, 2, 3, 4",
    base_sel,
    ext_sel,
    src_quoted
  )
}

#' Per (drug x cell line x plate x dose) cell-count grid, cached. This small
#' aggregate (~66k rows for the real data) is derived once from the obs data so
#' the subset builder can compute live, accurate cell counts locally without
#' re-querying 100M cells. Reads a prebuilt <data_dir>/obs_cell_grid.parquet if
#' present (written by dev/download_metadata.R), else computes it from the obs
#' source. Joined to each cell line's organ and driver-gene summary.
#'
#' Columns: drug, cell_name, plate, conc, n_cells, organ, drivers.
tahoe_cell_grid <- function() {
  if (!is.null(.tahoe_cache$cell_grid)) {
    return(.tahoe_cache$cell_grid)
  }
  grid_file <- file.path(tahoe_data_dir(), "obs_cell_grid.parquet")
  base <- tryCatch(
    {
      if (file.exists(grid_file)) {
        tahoe_read_file(grid_file)
      } else {
        source <- .tahoe_obs_count_source()
        if (identical(source$type, "remote")) {
          .tahoe_load_httpfs()
        }
        src_q <- .tahoe_quote(source$src)
        # Prefer the extended grid; fall back to counts-only if the obs table
        # lacks the QC/phase columns, so the grid is never empty on schema drift.
        ext <- tryCatch(
          dplyr::as_tibble(DBI::dbGetQuery(
            tahoe_con(),
            .tahoe_grid_query(src_q, extended = TRUE)
          )),
          error = function(e) NULL
        )
        if (is.null(ext)) {
          dplyr::as_tibble(DBI::dbGetQuery(
            tahoe_con(),
            .tahoe_grid_query(src_q, extended = FALSE)
          ))
        } else {
          ext
        }
      }
    },
    error = function(e) NULL
  )
  failed <- is.null(base)
  if (failed) {
    base <- dplyr::tibble(
      drug = character(),
      cell_name = character(),
      plate = character(),
      conc = numeric(),
      n_cells = numeric()
    )
  }
  lines <- tahoe_cell_line_unique()
  cols <- intersect(c("cell_name", "Organ", "drivers"), names(lines))
  if ("cell_name" %in% cols && "cell_name" %in% names(base)) {
    ann <- lines[, cols, drop = FALSE]
    names(ann)[names(ann) == "Organ"] <- "organ"
    base <- dplyr::left_join(base, ann, by = "cell_name")
  }
  # Never cache a failed read: caching the empty error tibble would blank
  # Subset/Coverage/QC for the whole process after one transient obs failure.
  # A genuinely-empty successful read is fine to cache; only the failure path
  # self-heals on the next reactive read.
  if (!failed) {
    .tahoe_cache$cell_grid <- base
  }
  base
}

# The extended per-condition columns a full grid carries beyond the cell counts.
.tahoe_grid_extra_cols <- c(
  "n_full",
  "n_g1",
  "n_s",
  "n_g2m",
  "sum_pcnt_mito",
  "sum_gene_count",
  "sum_tscp_count"
)

# If the grid carries the extended QC/phase columns, summarise them to the given
# grain -- QC-passing cell count, cell-cycle phase counts, and cell-weighted mean
# QC metrics -- and left-join onto `summary_df`. Returns `summary_df` unchanged
# when the columns are absent (a counts-only grid), so callers that expect only
# the base columns keep working. `by_cols` is the grouping key of `summary_df`.
.tahoe_join_grid_extras <- function(summary_df, g, by_cols) {
  if (!all(.tahoe_grid_extra_cols %in% names(g))) {
    return(summary_df)
  }
  extra <- dplyr::summarise(
    dplyr::group_by(g, dplyr::across(dplyr::all_of(by_cols))),
    n_full = sum(n_full),
    n_g1 = sum(n_g1),
    n_s = sum(n_s),
    n_g2m = sum(n_g2m),
    mean_pcnt_mito = sum(sum_pcnt_mito) / sum(n_cells),
    mean_gene_count = sum(sum_gene_count) / sum(n_cells),
    mean_tscp_count = sum(sum_tscp_count) / sum(n_cells),
    .groups = "drop"
  )
  dplyr::left_join(summary_df, extra, by = by_cols)
}

#' Coverage summary derived from the cell grid: total cells per (drug x cell
#' line), the set of non-zero doses tested, how many of the 3 doses are present,
#' and the number of plates. Powers the coverage matrix and QC views. `DMSO_TF`
#' (the vehicle control, dose 0) is kept -- its coverage matters for QC.
tahoe_coverage <- function() {
  g <- tahoe_cell_grid()
  if (nrow(g) == 0) {
    return(dplyr::tibble(
      drug = character(),
      cell_name = character(),
      organ = character(),
      n_cells = numeric(),
      n_doses = integer(),
      doses = character(),
      n_plates = integer()
    ))
  }
  cov <- dplyr::summarise(
    dplyr::group_by(g, drug, cell_name, organ),
    n_cells = sum(n_cells),
    n_doses = dplyr::n_distinct(conc[conc > 0]),
    doses = paste(sort(unique(conc[conc > 0])), collapse = ", "),
    n_plates = dplyr::n_distinct(plate),
    .groups = "drop"
  )
  .tahoe_join_grid_extras(cov, g, c("drug", "cell_name"))
}

#' Analysis conditions from the cell grid: cells per (drug x cell line x dose),
#' summed over plates. This is the unit most differential analyses compare, so
#' it is the natural grain for power / low-n QC. `conc == 0` rows are the DMSO
#' vehicle control.
tahoe_conditions <- function() {
  g <- tahoe_cell_grid()
  if (nrow(g) == 0) {
    return(dplyr::tibble(
      drug = character(),
      cell_name = character(),
      organ = character(),
      conc = numeric(),
      n_cells = numeric(),
      n_plates = integer()
    ))
  }
  cond <- dplyr::summarise(
    dplyr::group_by(g, drug, cell_name, organ, conc),
    n_cells = sum(n_cells),
    n_plates = dplyr::n_distinct(plate),
    .groups = "drop"
  )
  .tahoe_join_grid_extras(cond, g, c("drug", "cell_name", "conc"))
}
