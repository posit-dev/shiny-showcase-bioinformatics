# Hand-written tools the LLM assistant can call, over the app's data layer.
#
# Each tool is a PLAIN SPEC -- list(name, description, arguments, fun) -- so the
# backing functions are pure and testable without ellmer (the tests call
# spec$fun(...) directly against fixtures). tahoe_agent_client() converts each
# spec to an ellmer::tool() at registration time (enabled path only), via
# .tahoe_agent_ellmer_tool().
#
# Every tool returns a compact, row-capped structure so a single call can never
# flood the context window. Tools expose ONLY curated data-layer values -- there
# is no file/env/eval tool, so the agent cannot reach secrets.

# Coerce a model-supplied limit to a sane positive integer, else `default`.
.agent_limit <- function(x, default) {
  n <- suppressWarnings(as.integer(x))
  if (length(n) != 1 || is.na(n) || n < 1) default else n
}

# Shape a data frame for the model: keep `cols`, cap rows, report totals. Returns
# list(total_matches, returned, rows) so the model knows when results were cut.
.agent_table <- function(
  df,
  cols = names(df),
  max_rows = .tahoe_agent_row_cap()
) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  keep <- intersect(cols, names(df))
  if (length(keep) > 0) {
    df <- df[, keep, drop = FALSE]
  }
  total <- nrow(df)
  if (total > max_rows) {
    df <- df[seq_len(max_rows), , drop = FALSE]
  }
  list(total_matches = total, returned = nrow(df), rows = df)
}

# --- plain spec -> ellmer::tool() conversion (enabled path only) --------------

.tahoe_agent_ellmer_item <- function(item) {
  switch(
    item,
    string = ellmer::type_string(),
    number = ellmer::type_number(),
    integer = ellmer::type_integer(),
    boolean = ellmer::type_boolean(),
    ellmer::type_string()
  )
}

.tahoe_agent_ellmer_type <- function(a) {
  req <- isTRUE(a$required)
  switch(
    a$type,
    string = ellmer::type_string(a$desc, required = req),
    integer = ellmer::type_integer(a$desc, required = req),
    number = ellmer::type_number(a$desc, required = req),
    boolean = ellmer::type_boolean(a$desc, required = req),
    enum = ellmer::type_enum(a$values, a$desc, required = req),
    array = ellmer::type_array(
      .tahoe_agent_ellmer_item(a$items),
      a$desc,
      required = req
    ),
    stop("unknown tool arg type: ", a$type)
  )
}

.tahoe_agent_ellmer_tool <- function(spec) {
  args <- lapply(spec$arguments, .tahoe_agent_ellmer_type)
  ellmer::tool(
    spec$fun,
    spec$description,
    arguments = args,
    name = spec$name
  )
}

# --- the tool suite ----------------------------------------------------------

#' The assistant's tool specs. A list of list(name, description, arguments, fun).
#' `arguments` is a named list of plain type specs; `fun`'s args match the names.
tahoe_agent_tools <- function() {
  list(
    list(
      name = "dataset_overview",
      description = paste(
        "Headline Tahoe-100M counts (drugs, cell lines, samples, plates, genes,",
        "cells), the pinned dataset revision, and whether the numbers come from",
        "real data or synthetic fixtures. Use for any 'how many / how big'",
        "question."
      ),
      arguments = list(),
      fun = function() {
        cc <- tahoe_summary_counts()
        list(
          drugs = cc$drugs,
          assayed_cell_lines = cc$cell_lines,
          samples = cc$samples,
          plates = cc$plates,
          genes = cc$genes,
          cells = cc$cells,
          data_provenance = list(
            small_tables = cc$data_source,
            obs = cc$obs_source
          ),
          dataset = tahoe_dataset_pin()
        )
      }
    ),
    list(
      name = "list_drugs",
      description = paste(
        "Search/filter the drug metadata table by name substring, mechanism of",
        "action, and approval status. Returns drug, targets, MOA and approval."
      ),
      arguments = list(
        query = list(
          type = "string",
          desc = "Case-insensitive substring to match drug names.",
          required = FALSE
        ),
        moa = list(
          type = "string",
          desc = "Case-insensitive substring to match mechanism of action.",
          required = FALSE
        ),
        approved_only = list(
          type = "boolean",
          desc = "If true, only human-approved drugs.",
          required = FALSE
        ),
        limit = list(
          type = "integer",
          desc = "Max rows (default 20, capped at 40).",
          required = FALSE
        )
      ),
      fun = function(
        query = NULL,
        moa = NULL,
        approved_only = FALSE,
        limit = 20
      ) {
        d <- tahoe_drug()
        if (!is.null(query) && nzchar(query)) {
          d <- d[grepl(query, d$drug, ignore.case = TRUE), , drop = FALSE]
        }
        if (!is.null(moa) && nzchar(moa)) {
          hay <- paste(d[["moa-broad"]], d[["moa-fine"]])
          d <- d[grepl(moa, hay, ignore.case = TRUE), , drop = FALSE]
        }
        if (isTRUE(approved_only) && "human-approved" %in% names(d)) {
          keep <- tolower(as.character(d[["human-approved"]])) %in%
            c("yes", "true", "1", "approved")
          d <- d[keep, , drop = FALSE]
        }
        cap <- min(.agent_limit(limit, 20), 40L)
        .agent_table(
          d,
          c("drug", "targets", "moa-broad", "human-approved"),
          max_rows = cap
        )
      }
    ),
    list(
      name = "describe_drug",
      description = paste(
        "Full metadata for one drug by exact name: targets, MOA, approval /",
        "clinical status, PubChem CID, SMILES. Says so if the drug is not found."
      ),
      arguments = list(
        drug = list(
          type = "string",
          desc = "Exact drug name (use list_drugs to find it).",
          required = TRUE
        )
      ),
      fun = function(drug) {
        d <- tahoe_drug()
        row <- d[tolower(as.character(d$drug)) == tolower(drug), , drop = FALSE]
        if (nrow(row) == 0) {
          return(list(
            found = FALSE,
            message = sprintf("No drug named '%s' in the dataset.", drug)
          ))
        }
        row <- row[1, , drop = FALSE]
        list(
          found = TRUE,
          drug = as.list(row),
          targets = tahoe_drug_targets(as.character(row$drug[[1]]))
        )
      }
    ),
    list(
      name = "list_cell_lines",
      description = paste(
        "List assayed cell lines, filterable by organ/tissue and driver gene.",
        "Returns cell_name, Organ, driver genes and driver count."
      ),
      arguments = list(
        organ = list(
          type = "string",
          desc = "Case-insensitive organ/tissue substring.",
          required = FALSE
        ),
        driver = list(
          type = "string",
          desc = "Case-insensitive driver-gene substring.",
          required = FALSE
        ),
        limit = list(
          type = "integer",
          desc = "Max rows (default 30).",
          required = FALSE
        )
      ),
      fun = function(organ = NULL, driver = NULL, limit = 30) {
        u <- tahoe_cell_line_unique()
        assayed <- tryCatch(
          unique(tahoe_cell_grid()$cell_name),
          error = function(e) NULL
        )
        if (!is.null(assayed)) {
          u <- u[u$cell_name %in% assayed, , drop = FALSE]
        }
        if (!is.null(organ) && nzchar(organ) && "Organ" %in% names(u)) {
          u <- u[grepl(organ, u$Organ, ignore.case = TRUE), , drop = FALSE]
        }
        if (!is.null(driver) && nzchar(driver) && "drivers" %in% names(u)) {
          u <- u[grepl(driver, u$drivers, ignore.case = TRUE), , drop = FALSE]
        }
        .agent_table(
          u,
          c("cell_name", "Organ", "drivers", "n_drivers"),
          max_rows = .agent_limit(limit, 30)
        )
      }
    ),
    list(
      name = "describe_cell_line",
      description = paste(
        "Details for one cell line by exact name: organ, driver genes, and its",
        "somatic variants (DepMap / Cellosaurus). Says so if not found."
      ),
      arguments = list(
        cell_name = list(
          type = "string",
          desc = "Exact cell-line name (use list_cell_lines to find it).",
          required = TRUE
        )
      ),
      fun = function(cell_name) {
        u <- tahoe_cell_line_unique()
        row <- u[
          tolower(as.character(u$cell_name)) == tolower(cell_name),
          ,
          drop = FALSE
        ]
        if (nrow(row) == 0) {
          return(list(
            found = FALSE,
            message = sprintf("No cell line named '%s'.", cell_name)
          ))
        }
        v <- tryCatch(tahoe_cell_variants(), error = function(e) NULL)
        variants <- if (!is.null(v) && "cell_name" %in% names(v)) {
          vv <- v[
            tolower(as.character(v$cell_name)) == tolower(cell_name),
            ,
            drop = FALSE
          ]
          .agent_table(
            vv,
            c("gene", "protein_change", "variant_type", "source"),
            max_rows = .tahoe_agent_row_cap()
          )
        } else {
          NULL
        }
        cols <- intersect(
          c("cell_name", "Organ", "drivers", "n_drivers"),
          names(row)
        )
        list(
          found = TRUE,
          cell_line = as.list(row[1, cols, drop = FALSE]),
          variants = variants
        )
      }
    ),
    list(
      name = "coverage_lookup",
      description = paste(
        "Cells assayed per drug x cell-line: total cells, the non-zero doses",
        "tested, and plate count. Filter by exact drug and/or cell line."
      ),
      arguments = list(
        drug = list(
          type = "string",
          desc = "Exact drug name.",
          required = FALSE
        ),
        cell_line = list(
          type = "string",
          desc = "Exact cell-line name.",
          required = FALSE
        ),
        limit = list(
          type = "integer",
          desc = "Max rows (default 25).",
          required = FALSE
        )
      ),
      fun = function(drug = NULL, cell_line = NULL, limit = 25) {
        cov <- tahoe_coverage()
        if (!is.null(drug) && nzchar(drug)) {
          cov <- cov[tolower(cov$drug) == tolower(drug), , drop = FALSE]
        }
        if (!is.null(cell_line) && nzchar(cell_line)) {
          cov <- cov[
            tolower(cov$cell_name) == tolower(cell_line),
            ,
            drop = FALSE
          ]
        }
        cov <- cov[order(-cov$n_cells), , drop = FALSE]
        .agent_table(
          cov,
          c(
            "drug",
            "cell_name",
            "organ",
            "n_cells",
            "n_doses",
            "doses",
            "n_plates"
          ),
          max_rows = .agent_limit(limit, 25)
        )
      }
    ),
    list(
      name = "conditions_lookup",
      description = paste(
        "Cells per drug x cell-line x dose -- the unit most differential",
        "analyses compare, for judging statistical power. Filter by exact drug",
        "and/or cell line."
      ),
      arguments = list(
        drug = list(
          type = "string",
          desc = "Exact drug name.",
          required = FALSE
        ),
        cell_line = list(
          type = "string",
          desc = "Exact cell-line name.",
          required = FALSE
        ),
        limit = list(
          type = "integer",
          desc = "Max rows (default 25).",
          required = FALSE
        )
      ),
      fun = function(drug = NULL, cell_line = NULL, limit = 25) {
        cond <- tahoe_conditions()
        if (!is.null(drug) && nzchar(drug)) {
          cond <- cond[tolower(cond$drug) == tolower(drug), , drop = FALSE]
        }
        if (!is.null(cell_line) && nzchar(cell_line)) {
          cond <- cond[
            tolower(cond$cell_name) == tolower(cell_line),
            ,
            drop = FALSE
          ]
        }
        cond <- cond[order(-cond$n_cells), , drop = FALSE]
        .agent_table(
          cond,
          c("drug", "cell_name", "organ", "conc", "n_cells", "n_plates"),
          max_rows = .agent_limit(limit, 25)
        )
      }
    ),
    list(
      name = "drug_target_mutants",
      description = paste(
        "For a drug, list its target genes and the assayed cell lines carrying a",
        "somatic variant in those targets -- the set over which a mutant-vs-",
        "wildtype contrast can be designed. Key for mutant/WT subset arms."
      ),
      arguments = list(
        drug = list(
          type = "string",
          desc = "Exact drug name.",
          required = TRUE
        )
      ),
      fun = function(drug) {
        targets <- tahoe_drug_targets(drug)
        if (length(targets) == 0) {
          return(list(
            drug = drug,
            targets = character(0),
            message = "No known target genes for this drug (or drug not found)."
          ))
        }
        hits <- tryCatch(
          tahoe_target_mutations(targets),
          error = function(e) NULL
        )
        mut <- if (!is.null(hits)) {
          .agent_table(
            hits,
            c("cell_name", "gene", "protein_change", "source"),
            max_rows = .tahoe_agent_row_cap()
          )
        } else {
          NULL
        }
        list(drug = drug, targets = targets, mutant_lines = mut)
      }
    ),
    list(
      name = "gene_lookup",
      description = paste(
        "Search the measured genes (62,710) to check whether a gene is in the",
        "dataset and get its Ensembl id. Returns matching gene rows."
      ),
      arguments = list(
        query = list(
          type = "string",
          desc = "Case-insensitive gene-symbol substring, e.g. 'EGFR'.",
          required = TRUE
        ),
        limit = list(
          type = "integer",
          desc = "Max rows (default 20).",
          required = FALSE
        )
      ),
      fun = function(query, limit = 20) {
        g <- tahoe_gene()
        col <- if ("gene_symbol" %in% names(g)) "gene_symbol" else names(g)[1]
        hit <- g[grepl(query, g[[col]], ignore.case = TRUE), , drop = FALSE]
        .agent_table(
          hit,
          intersect(c("gene_symbol", "ensembl_id", "token_id"), names(g)),
          max_rows = .agent_limit(limit, 20)
        )
      }
    ),
    list(
      name = "obs_summary",
      description = paste(
        "Aggregate the cell-level obs table by one whitelisted column and",
        "metric (e.g. cells per phase, or mean %mito per drug). Numbers are from",
        "synthetic fixtures unless the full obs data is loaded."
      ),
      arguments = list(
        group_by = list(
          type = "enum",
          values = .tahoe_obs_group_cols,
          desc = "Column to group by.",
          required = TRUE
        ),
        metric = list(
          type = "enum",
          values = names(.tahoe_obs_metrics),
          desc = "Aggregate metric (default n_cells).",
          required = FALSE
        ),
        filter_column = list(
          type = "enum",
          values = .tahoe_obs_group_cols,
          desc = "Optional single column to filter on.",
          required = FALSE
        ),
        filter_values = list(
          type = "array",
          items = "string",
          desc = "Values to keep for filter_column.",
          required = FALSE
        ),
        limit = list(
          type = "integer",
          desc = "Max groups (default 15, capped at 25).",
          required = FALSE
        )
      ),
      fun = function(
        group_by,
        metric = "n_cells",
        filter_column = NULL,
        filter_values = NULL,
        limit = 15
      ) {
        if (!isTRUE(group_by %in% .tahoe_obs_group_cols)) {
          return(list(
            error = sprintf(
              "Unknown group_by '%s'. Valid columns: %s.",
              group_by,
              paste(.tahoe_obs_group_cols, collapse = ", ")
            )
          ))
        }
        if (!isTRUE(metric %in% names(.tahoe_obs_metrics))) {
          metric <- "n_cells"
        }
        filters <- list()
        if (
          !is.null(filter_column) &&
            filter_column %in% .tahoe_obs_group_cols &&
            length(filter_values) > 0
        ) {
          filters[[filter_column]] <- as.character(filter_values)
        }
        cap <- min(.agent_limit(limit, 15), 25L)
        res <- tahoe_obs_summary(
          group_by = group_by,
          filters = filters,
          metric = metric,
          limit = cap
        )
        out <- .agent_table(res, names(res), max_rows = 25L)
        out$metric <- metric
        out$provenance <- attr(res, "tahoe_source")
        if (!is.null(attr(res, "tahoe_error"))) {
          out$note <- paste(
            "The obs query failed; returning no rows.",
            "The cell-level obs data may be unavailable."
          )
        }
        out
      }
    ),
    list(
      name = "build_subset_recipe",
      description = paste(
        "Given a selection across the six subset dimensions (organs, drivers,",
        "cell_lines, drugs, doses, plates), return the copy-paste R (duckdb) +",
        "Python (scanpy) analysis recipe and the estimated cells/samples. This",
        "is how you hand the user a reproducible pull. Verify drug / cell-line /",
        "gene names with the list_* tools first."
      ),
      arguments = list(
        organs = list(
          type = "array",
          items = "string",
          desc = "Tissues/organs to keep.",
          required = FALSE
        ),
        drivers = list(
          type = "array",
          items = "string",
          desc = "Driver genes to keep.",
          required = FALSE
        ),
        cell_lines = list(
          type = "array",
          items = "string",
          desc = "Exact cell-line names to keep.",
          required = FALSE
        ),
        drugs = list(
          type = "array",
          items = "string",
          desc = "Exact drug names to keep.",
          required = FALSE
        ),
        doses = list(
          type = "array",
          items = "number",
          desc = "Doses in uM (0.05 / 0.5 / 5).",
          required = FALSE
        ),
        plates = list(
          type = "array",
          items = "string",
          desc = "Plate ids to keep.",
          required = FALSE
        )
      ),
      fun = function(
        organs = NULL,
        drivers = NULL,
        cell_lines = NULL,
        drugs = NULL,
        doses = NULL,
        plates = NULL
      ) {
        sel <- list(
          organs = organs,
          drivers = drivers,
          cell_lines = cell_lines,
          drugs = drugs,
          doses = doses,
          plates = plates
        )
        r <- tahoe_subset_recipe(sel)
        list(
          recipe = r$recipe,
          estimated_cells = r$cells,
          estimated_samples = r$samples,
          estimated_obs_mb = r$obs_mb
        )
      }
    )
  )
}
