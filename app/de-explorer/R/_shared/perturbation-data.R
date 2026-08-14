# Drug Perturbation Explorer data contract: construct, validate, and load a
# reference perturbation panel. This is the single source of truth for the shape
# of the reference data every connectivity view relies on (CMap / LINCS-style).
#
# The *query* signature (up/down genes) is runtime input, not part of this
# reference contract -- it lives with the connectivity scoring logic.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Shared by the classic
# Shiny and shinyreact versions of the app.

# Required columns. profiles is a gene column + one numeric column per
# perturbation; pert_meta describes each perturbation, keyed by pert_id.
perturbation_meta_required <- c(
  "pert_id",
  "drug",
  "dose",
  "time",
  "cell_line",
  "moa"
)

# Construct a perturbation_dataset from two data frames:
#   profiles:  a "gene" column + one numeric column per perturbation (pert_id),
#              holding reference signatures (per-gene z-scores).
#   pert_meta: one row per perturbation (pert_id, drug, dose, time, cell_line, moa).
new_perturbation_dataset <- function(profiles, pert_meta) {
  structure(
    list(profiles = profiles, pert_meta = pert_meta),
    class = "perturbation_dataset"
  )
}

# Validate the contract. Errors loudly on the first problem it finds so data
# issues surface at load time rather than deep inside a score.
validate_perturbation_dataset <- function(x) {
  stopifnot(inherits(x, "perturbation_dataset"))

  if (!"gene" %in% names(x$profiles)) {
    stop("profiles must have a 'gene' column")
  }
  if (anyDuplicated(x$profiles$gene)) {
    stop("profiles$gene has duplicate values")
  }
  miss_meta <- setdiff(perturbation_meta_required, names(x$pert_meta))
  if (length(miss_meta) > 0) {
    stop("pert_meta is missing required column(s): ", toString(miss_meta))
  }

  # Profile columns (the perturbations) must match pert_meta$pert_id exactly,
  # and every one must be numeric -- a stray text column would otherwise coerce
  # the whole profile matrix to character and break scoring far from here.
  profile_perts <- setdiff(names(x$profiles), "gene")
  non_numeric <- profile_perts[
    !vapply(x$profiles[profile_perts], is.numeric, logical(1))
  ]
  if (length(non_numeric) > 0) {
    stop(
      "profile column(s) are not numeric: ",
      toString(utils::head(non_numeric, 5))
    )
  }
  meta_perts <- as.character(x$pert_meta$pert_id)
  if (!setequal(profile_perts, meta_perts)) {
    stop(
      "profile columns do not match pert_meta$pert_id (",
      length(profile_perts),
      " vs ",
      length(meta_perts),
      ")"
    )
  }
  if (anyDuplicated(meta_perts)) {
    stop("pert_meta$pert_id has duplicate values")
  }

  x
}

# Public constructor: build and validate in one step.
perturbation_dataset <- function(profiles, pert_meta) {
  validate_perturbation_dataset(new_perturbation_dataset(profiles, pert_meta))
}

# Read a perturbation_dataset from a directory holding the two contract CSVs.
read_perturbation_dataset <- function(dir) {
  read_one <- function(name) {
    path <- file.path(dir, name)
    if (!file.exists(path)) {
      stop("expected file not found: ", path)
    }
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }
  perturbation_dataset(
    profiles = read_one("profiles.csv"),
    pert_meta = read_one("pert_meta.csv")
  )
}

# Convenience: the profiles as a numeric matrix with gene rownames
# (perturbations as columns), the shape connectivity scoring wants.
perturbation_profile_matrix <- function(x) {
  stopifnot(inherits(x, "perturbation_dataset"))
  m <- as.matrix(x$profiles[,
    setdiff(names(x$profiles), "gene"),
    drop = FALSE
  ])
  rownames(m) <- x$profiles$gene
  m
}
