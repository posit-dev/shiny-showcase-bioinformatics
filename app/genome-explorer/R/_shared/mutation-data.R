# Somatic-mutation track contract for the genome browser: a tidy table of
# recurrent mutations at genomic coordinates (one row per distinct variant).
# This is the single source of truth for the shape of the data the genome-browser
# module turns into an IGV track.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Shared by the classic
# Shiny and shinyreact versions.

mutation_required <- c(
  "chrom",
  "pos",
  "ref",
  "alt",
  "gene",
  "protein_change",
  "count"
)

# Validate the contract. Errors loudly at load time.
validate_mutations <- function(x) {
  miss <- setdiff(mutation_required, names(x))
  if (length(miss) > 0) {
    stop("mutations is missing required column(s): ", toString(miss))
  }
  if (!is.numeric(x$pos) || any(x$pos <= 0, na.rm = TRUE)) {
    stop("mutations$pos must be positive genomic positions")
  }
  if (!is.numeric(x$count) || any(x$count < 1, na.rm = TRUE)) {
    stop("mutations$count must be >= 1")
  }
  x
}

# Read the mutations table from a directory holding mutations.csv.
read_mutations <- function(dir) {
  path <- file.path(dir, "mutations.csv")
  if (!file.exists(path)) {
    stop("expected file not found: ", path)
  }
  validate_mutations(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  )
}

# Convert the mutations table to a BED-style data frame for an IGV features
# track (igvShiny::loadBedTrack): 0-based start, 1-based end, a readable name,
# and the recurrence count as the score.
mutations_to_bed <- function(muts) {
  data.frame(
    chrom = as.character(muts$chrom),
    start = as.integer(muts$pos) - 1L,
    end = as.integer(muts$pos),
    name = paste0(muts$gene, " ", muts$protein_change, " (n=", muts$count, ")"),
    score = as.integer(muts$count),
    stringsAsFactors = FALSE
  )
}
