# Shared subset-recipe logic.
#
# The Subset Builder tab and the Chat assistant's `build_subset_recipe` tool must
# produce the SAME reproducible pull recipe for the same selection, so the pure
# logic lives here and both call it. Extracted verbatim from the subset builder's
# recipe() reactive; the string output is unchanged (guarded by
# tests/testthat/test-subset-builder.R).

# Public obs parquet on HuggingFace -- the source the generated code reads from,
# so the snippets run without pre-downloading anything. Pinned to the dataset
# revision (defined in R/data.R, sourced before this file).
.subset_obs_hf <- sprintf(
  "hf://datasets/%s@%s/metadata/obs_metadata.parquet",
  .tahoe_dataset_repo,
  .tahoe_dataset_revision
)

# Approximate compressed bytes per cell in the obs parquet (~2.29 GB / 100.6M
# cells), used for the subset size estimate.
.subset_obs_bytes_per_cell <- 23

# Format an integer for display, with an em dash for NA / unknown.
.subset_fmt <- function(x) {
  if (length(x) != 1 || is.na(x)) "-" else format(x, big.mark = ",")
}

# Render a character vector as a single-quoted SQL IN list, e.g. ('a', 'b').
.subset_sql_vec <- function(x) {
  vals <- paste(
    vapply(
      x,
      function(v) paste0("'", gsub("'", "''", v, fixed = TRUE), "'"),
      character(1)
    ),
    collapse = ", "
  )
  paste0("(", vals, ")")
}

# Coerce a loose selection list to clean, typed vectors. The module passes
# already-clean reactive values (a no-op here); the agent tool may pass NULLs or
# stray types, so normalise defensively without reordering.
.subset_normalize_sel <- function(sel) {
  chr <- function(x) {
    x <- as.character(x %||% character())
    x[!is.na(x) & nzchar(x)]
  }
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x %||% numeric()))
    x[!is.na(x)]
  }
  list(
    organs = chr(sel$organs),
    drivers = chr(sel$drivers),
    cell_lines = chr(sel$cell_lines),
    drugs = chr(sel$drugs),
    doses = num(sel$doses),
    plates = chr(sel$plates)
  )
}

#' Assayed cell lines implied by the organ + driver + explicit-cell-line
#' selection, intersected with the lines actually present in `grid`. Pure; used
#' by both the Subset Builder's matched_cell_names() reactive and
#' tahoe_subset_recipe(). `sel` is a list(organs, drivers, cell_lines, ...),
#' `grid` the cell grid (needs cell_name, organ), `lines_tbl` the driver-level
#' cell-line table (needs cell_name, Driver_Gene_Symbol).
tahoe_subset_matched_lines <- function(sel, grid, lines_tbl) {
  out <- unique(grid$cell_name)
  if (length(sel$organs) > 0 && "organ" %in% names(grid)) {
    out <- intersect(out, unique(grid$cell_name[grid$organ %in% sel$organs]))
  }
  if (
    length(sel$drivers) > 0 &&
      all(c("cell_name", "Driver_Gene_Symbol") %in% names(lines_tbl))
  ) {
    hit <- unique(
      lines_tbl$cell_name[lines_tbl$Driver_Gene_Symbol %in% sel$drivers]
    )
    out <- intersect(out, hit)
  }
  if (length(sel$cell_lines) > 0) {
    out <- intersect(out, sel$cell_lines)
  }
  out
}

# Cells in the grid after the matched-line + drug/dose/plate filters (mirrors the
# module's grid_filtered() reactive).
.subset_filtered_cells <- function(sel, grid, lines) {
  if (nrow(grid) == 0) {
    return(0L)
  }
  g <- grid[grid$cell_name %in% lines, , drop = FALSE]
  if (length(sel$drugs) > 0) {
    g <- g[g$drug %in% sel$drugs, , drop = FALSE]
  }
  if (length(sel$doses) > 0) {
    g <- g[!is.na(g$conc) & g$conc %in% sel$doses, , drop = FALSE]
  }
  if (length(sel$plates) > 0) {
    g <- g[g$plate %in% sel$plates, , drop = FALSE]
  }
  as.integer(sum(g$n_cells, na.rm = TRUE))
}

# Samples matching the drug/dose/plate dimensions (cell lines are pooled across
# all samples), as a count. Mirrors the module's matched_samples() reactive.
.subset_matched_sample_count <- function(sel, samples) {
  df <- samples
  keep <- rep(TRUE, nrow(df))
  if (length(sel$drugs) > 0 && "drug" %in% names(df)) {
    keep <- keep & df$drug %in% sel$drugs
  }
  if (length(sel$plates) > 0 && "plate" %in% names(df)) {
    keep <- keep & df$plate %in% sel$plates
  }
  if (length(sel$doses) > 0 && "drugname_drugconc" %in% names(df)) {
    conc <- tahoe_parse_dose(df$drugname_drugconc)$conc
    keep <- keep & !is.na(conc) & conc %in% sel$doses
  }
  sum(keep)
}

# Commented setup notes prepended to each snippet: which packages to install
# (uncomment to run), a nudge toward an isolated/reproducible environment, and
# how to supply a Hugging Face token for gated or rate-limited access.
.subset_r_preamble <- function() {
  paste(
    "# -- Setup (uncomment to install) -----------------------------",
    '# install.packages(c("duckdb", "DBI"))',
    "# Reproducible, project-local library (recommended):",
    '# install.packages("renv"); renv::init()',
    "# Hugging Face token -- the obs parquet is public, but a token avoids",
    "# rate limits and unlocks gated files. Create one at",
    "# https://huggingface.co/settings/tokens, then add HF_TOKEN to a project",
    '# .Renviron file (or run Sys.setenv(HF_TOKEN = "hf_...")) and register it',
    "# as a duckdb secret after connecting:",
    "# dbExecute(con, \"CREATE SECRET (TYPE huggingface, TOKEN getenv('HF_TOKEN'))\")",
    sep = "\n"
  )
}

.subset_py_preamble <- function() {
  paste(
    "# -- Setup (uncomment to install) -----------------------------",
    "# pip install duckdb scanpy huggingface_hub",
    "# Isolated environment (recommended):",
    "# python -m venv .venv && source .venv/bin/activate   # or: uv venv",
    "# Hugging Face token -- the obs parquet is public, but a token avoids",
    "# rate limits and unlocks gated files. Create one at",
    "# https://huggingface.co/settings/tokens, then add HF_TOKEN to a .env /",
    "# environment file, or authenticate in Python:",
    "# from huggingface_hub import login; login()",
    sep = "\n"
  )
}

#' Build the reproducible subset recipe (R duckdb + Python scanpy) and size
#' estimate for a selection across the six subset dimensions. Returns
#' list(recipe, header, r_code, py_code, cells, samples, obs_mb). `sel` is a
#' list(organs, drivers,
#' cell_lines, drugs, doses, plates); the data tables default to the cached data
#' layer so the agent tool can call it with just a selection, while the Subset
#' Builder passes its (possibly async) reactive grid + cell-line table.
tahoe_subset_recipe <- function(
  sel,
  grid = tahoe_cell_grid(),
  lines_tbl = tahoe_cell_line(),
  samples = tahoe_sample()
) {
  sel <- .subset_normalize_sel(sel)
  drugs <- sel$drugs
  doses <- sel$doses
  plates <- sel$plates
  # Only constrain cell lines when the resolved set is a strict subset.
  all_lines <- unique(grid$cell_name)
  lines <- tahoe_subset_matched_lines(sel, grid, lines_tbl)
  constrain_lines <- length(lines) > 0 &&
    length(lines) < length(all_lines)

  cells <- .subset_filtered_cells(sel, grid, lines)
  n_samp <- .subset_matched_sample_count(sel, samples)
  obs_mb <- round(cells * .subset_obs_bytes_per_cell / 1e6)
  est <- list(cells = cells, samples = n_samp, obs_mb = obs_mb)

  if (
    length(drugs) == 0 &&
      length(doses) == 0 &&
      length(plates) == 0 &&
      !constrain_lines
  ) {
    return(list(
      recipe = paste(
        "No filters selected: this recipe would return the full dataset.",
        "Pick a tissue, driver, cell line, drug, dose, or plate to build a",
        "reproducible subset predicate."
      ),
      header = NULL,
      r_code = NULL,
      py_code = NULL,
      cells = cells,
      samples = n_samp,
      obs_mb = obs_mb
    ))
  }

  # Both snippets read via duckdb with predicate pushdown (filter at scan time,
  # never pull ~100M rows into memory), so they share one SQL WHERE clause.
  r_where <- character()
  if (length(drugs) > 0) {
    # Include the DMSO_TF vehicle control so the pulled subset has a control
    # arm -- perturbation DE is treated-vs-control.
    drugs_ctrl <- unique(c(drugs, "DMSO_TF"))
    r_where <- c(r_where, sprintf("drug IN %s", .subset_sql_vec(drugs_ctrl)))
  }
  if (constrain_lines) {
    r_where <- c(
      r_where,
      sprintf("cell_name IN %s", .subset_sql_vec(lines))
    )
  }
  if (length(plates) > 0) {
    r_where <- c(r_where, sprintf("plate IN %s", .subset_sql_vec(plates)))
  }
  if (length(doses) > 0) {
    # Keep the DMSO vehicle (dose 0) alongside the chosen doses.
    doses_ctrl <- sort(unique(c(doses, 0)))
    r_where <- c(
      r_where,
      sprintf(
        paste0(
          "TRY_CAST(regexp_extract(drugname_drugconc, ",
          "',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) IN (%s)"
        ),
        paste(format(doses_ctrl, trim = TRUE), collapse = ", ")
      )
    )
  }

  r_predicate <- paste(r_where, collapse = "\n    AND ")

  header <- paste(
    "# -- Estimated subset -----------------------------------------",
    sprintf(
      "# ~%s cells across %s samples · ~%s MB of obs metadata to scan.",
      .subset_fmt(est$cells),
      .subset_fmt(est$samples),
      .subset_fmt(est$obs_mb)
    ),
    "# The expression matrix is downloaded separately and is larger.",
    "# DMSO_TF vehicle controls (dose 0) are included automatically.",
    sprintf("# Source: %s", .subset_obs_hf),
    sep = "\n"
  )
  r_snippet <- paste(
    "## R (duckdb) -- pull the subset's cell-level metadata ---------",
    .subset_r_preamble(),
    "",
    "library(duckdb); library(DBI)",
    "con <- dbConnect(duckdb())",
    'dbExecute(con, "INSTALL httpfs; LOAD httpfs;")',
    "obs <- dbGetQuery(con, \"",
    "  SELECT *",
    sprintf("  FROM read_parquet('%s')", .subset_obs_hf),
    sprintf("  WHERE %s", r_predicate),
    "\")",
    "dbDisconnect(con, shutdown = TRUE)",
    sep = "\n"
  )
  py_snippet <- paste(
    "## Python (duckdb) -- pull the subset's cell-level metadata -----",
    .subset_py_preamble(),
    "",
    "import duckdb",
    "con = duckdb.connect()",
    'con.execute("INSTALL httpfs; LOAD httpfs;")',
    'obs = con.execute("""',
    "  SELECT *",
    sprintf("  FROM read_parquet('%s')", .subset_obs_hf),
    sprintf("  WHERE %s", r_predicate),
    '""").df()',
    'cells = obs["BARCODE_SUB_LIB_ID"]',
    "",
    "# Point `adata` at the Tahoe-100M expression AnnData, then keep",
    "# only these cells (obs_names are the BARCODE_SUB_LIB_ID values):",
    "# import scanpy as sc",
    '# adata = sc.read_h5ad("<tahoe_expression.h5ad>", backed="r")',
    "# adata = adata[adata.obs_names.isin(cells)].to_memory()",
    sep = "\n"
  )
  list(
    recipe = paste(header, "", r_snippet, "", py_snippet, sep = "\n"),
    header = header,
    r_code = r_snippet,
    py_code = py_snippet,
    cells = cells,
    samples = n_samp,
    obs_mb = obs_mb
  )
}

# --- Downloadable recipe documents (R Markdown / Quarto / Jupyter) ------------
#
# The same subset recipe, packaged as a ready-to-open notebook so a user can
# download a scaffold rather than copy-pasting. Each document is single-language
# -- R (duckdb) or Python (scanpy) -- chosen by the caller, so an R and a Python
# step are never mixed in one file; the Jupyter export uses the matching kernel
# (IRkernel for R, python3 for Python). The code chunks are marked
# non-evaluating (they hit the network / reference a placeholder h5ad path), so
# the document renders safely and the user fills in the last step.

# Drop a leading decorative "## ..." comment line from a code snippet (the doc
# adds its own markdown section header instead).
.subset_code_body <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  if (length(lines) > 0 && grepl("^## ", lines[1])) {
    lines <- lines[-1]
  }
  # Trim a single leading blank line left by the stripped header.
  if (length(lines) > 0 && !nzchar(lines[1])) {
    lines <- lines[-1]
  }
  paste(lines, collapse = "\n")
}

# Prose intro shared by every format (a markdown block), tailored to the chosen
# language.
.subset_intro <- function(parts, language) {
  lang_label <- if (identical(language, "r")) {
    "R (duckdb)"
  } else {
    "Python (scanpy)"
  }
  env_tip <- if (identical(language, "r")) {
    "using renv keeps the analysis reproducible."
  } else {
    "using a virtual environment keeps the analysis reproducible."
  }
  paste(
    c(
      "This document reproduces a Tahoe-100M subset selected in Tahoe Explorer.",
      "",
      sprintf(
        paste0(
          "- **Estimated size:** ~%s cells across %s samples ",
          "(~%s MB of obs metadata to scan)."
        ),
        .subset_fmt(parts$cells),
        .subset_fmt(parts$samples),
        .subset_fmt(parts$obs_mb)
      ),
      sprintf("- **Source:** `%s`", .subset_obs_hf),
      sprintf("- **Language:** %s", lang_label),
      "- The expression matrix is downloaded separately and is larger.",
      "",
      paste(
        "The setup lines list the packages to install (uncomment to run) and",
        "how to supply a Hugging Face token;",
        env_tip
      )
    ),
    collapse = "\n"
  )
}

# Jupyter kernelspec for the chosen language.
.subset_kernel <- function(language) {
  if (identical(language, "r")) {
    list(name = "ir", display_name = "R", language = "R")
  } else {
    list(name = "python3", display_name = "Python 3", language = "python")
  }
}

# nbformat source array: one element per line, each ending in "\n" except the
# last, so Jupyter reconstructs the text exactly.
.subset_nb_source <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    return(list(""))
  }
  out <- paste0(lines, "\n")
  out[length(out)] <- lines[[length(lines)]]
  as.list(out)
}

.subset_nb_md <- function(text) {
  list(
    cell_type = "markdown",
    metadata = stats::setNames(list(), character()),
    source = .subset_nb_source(text)
  )
}

.subset_nb_code <- function(text) {
  list(
    cell_type = "code",
    metadata = stats::setNames(list(), character()),
    execution_count = NA,
    outputs = list(),
    source = .subset_nb_source(text)
  )
}

# A single-language Jupyter (.ipynb) notebook: an intro markdown cell followed
# by one executable code cell, using the kernel that matches `language`.
.subset_ipynb <- function(title, intro, body, language) {
  cells <- list(.subset_nb_md(paste0("# ", title, "\n\n", intro)))
  if (!is.null(body)) {
    cells <- c(cells, list(.subset_nb_code(body)))
  }
  nb <- list(
    cells = cells,
    metadata = list(
      kernelspec = .subset_kernel(language),
      language_info = list(
        name = if (identical(language, "r")) "R" else "python"
      )
    ),
    nbformat = 4L,
    nbformat_minor = 5L
  )
  as.character(jsonlite::toJSON(
    nb,
    auto_unbox = TRUE,
    na = "null",
    pretty = TRUE
  ))
}

#' Render a subset recipe (from tahoe_subset_recipe()) as a downloadable,
#' single-language notebook. `format` is "rmd" (R Markdown), "qmd" (Quarto), or
#' "ipynb" (Jupyter); `language` is "r" (duckdb) or "python" (scanpy). Returns
#' the file content as a single string.
tahoe_subset_document <- function(
  parts,
  format = c("rmd", "qmd", "ipynb"),
  language = c("r", "python"),
  title = "Tahoe-100M subset"
) {
  format <- match.arg(format)
  language <- match.arg(language)
  code <- if (identical(language, "r")) parts$r_code else parts$py_code
  have_code <- !is.null(code)

  # No filters selected: emit a minimal doc carrying the explanatory message.
  if (!have_code) {
    msg <- parts$recipe %||% "No filters selected."
    if (identical(format, "ipynb")) {
      return(.subset_ipynb(title, msg, NULL, language))
    }
    yaml <- if (identical(format, "qmd")) {
      c("---", sprintf('title: "%s"', title), "format: html", "---")
    } else {
      c("---", sprintf('title: "%s"', title), "output: html_document", "---")
    }
    return(paste(c(yaml, "", msg, ""), collapse = "\n"))
  }

  body <- .subset_code_body(code)
  intro <- .subset_intro(parts, language)
  section <- if (identical(language, "r")) {
    "## R (duckdb)"
  } else {
    "## Python (scanpy)"
  }

  if (identical(format, "ipynb")) {
    return(.subset_ipynb(title, intro, body, language))
  }

  if (identical(format, "qmd")) {
    yaml <- c("---", sprintf('title: "%s"', title), "format: html", "---")
    engine <- if (identical(language, "r")) "r" else "python"
    chunk <- c(sprintf("```{%s}", engine), "#| eval: false", body, "```")
  } else {
    yaml <- c(
      "---",
      sprintf('title: "%s"', title),
      "output: html_document",
      "---"
    )
    engine <- if (identical(language, "r")) "r" else "python"
    chunk <- c(sprintf("```{%s, eval=FALSE}", engine), body, "```")
  }
  paste(
    c(yaml, "", intro, "", section, "", chunk, ""),
    collapse = "\n"
  )
}
