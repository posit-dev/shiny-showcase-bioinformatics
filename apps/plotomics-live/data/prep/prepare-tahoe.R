#!/usr/bin/env Rscript
# Prepare the Tahoe-100M perturbation coverage matrix (drug x cell line).
#
# Source: tahoe-explorer's pre-aggregated obs_cell_grid.parquet (Tahoe-100M,
# Arc Institute). Each row is a (drug, cell_name, plate, conc) combination with
# n_cells profiled. We aggregate to a drug x cell-line matrix of total cells
# (log10), the same coverage view the Tahoe explorer app features.
#
# Uses duckdb to read the parquet WITHOUT loading it into R wholesale, then
# writes a compact CSV into data/ so the app runs offline with no duckdb / no
# external path dependency. Re-run to refresh.

suppressWarnings(suppressMessages({ library(duckdb); library(DBI) }))

this_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
app_dir <- if (length(this_file) && !is.na(this_file))
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE) else getwd()

# The parquet lives outside this repo (it is far too big to vendor), so the
# path has to come from the caller. Order: an explicit argument, then
# TAHOE_CELL_GRID, then data/raw/ where the other prep scripts cache.
grid <- local({
  a <- commandArgs(TRUE)
  if (length(a) && nzchar(a[1])) return(a[1])
  env <- Sys.getenv("TAHOE_CELL_GRID")
  if (nzchar(env)) return(env)
  file.path(app_dir, "data", "raw", "obs_cell_grid.parquet")
})
if (!file.exists(grid)) {
  stop("Tahoe obs_cell_grid.parquet not found at ", grid, "\n",
       "Pass the path as an argument, set TAHOE_CELL_GRID, or drop the file ",
       "in data/raw/.")
}

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE))

# top drugs and cell lines by total cells profiled, then the full matrix.
top_drugs <- dbGetQuery(con, sprintf(
  "SELECT drug, sum(n_cells) t FROM read_parquet('%s') GROUP BY drug ORDER BY t DESC LIMIT 40", grid))$drug
cells <- dbGetQuery(con, sprintf(
  "SELECT cell_name, sum(n_cells) t FROM read_parquet('%s') GROUP BY cell_name ORDER BY t DESC", grid))$cell_name

df <- dbGetQuery(con, sprintf(
  "SELECT drug, cell_name, sum(n_cells) AS n FROM read_parquet('%s')
   WHERE drug IN (%s) GROUP BY drug, cell_name", grid,
  paste(sprintf("'%s'", gsub("'", "''", top_drugs)), collapse = ",")))

# dense matrix: drugs (rows) x cell lines (cols)
m <- matrix(0, nrow = length(top_drugs), ncol = length(cells),
            dimnames = list(top_drugs, cells))
m[cbind(df$drug, df$cell_name)] <- df$n

out <- data.frame(drug = rownames(m), m, check.names = FALSE)
dest <- file.path(app_dir, "data", "tahoe_perturbation.csv")
utils::write.csv(out, dest, row.names = FALSE)
cat(sprintf("wrote %d drugs x %d cell lines -> %s\n",
            nrow(m), ncol(m), dest))
