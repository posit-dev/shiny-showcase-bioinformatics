# Data-provenance badge for the navbar.
#
# A persistent chip, visible on every tab, that tells the user whether the
# numbers on screen are real Tahoe-100M data or the bundled synthetic fixtures --
# so the offline demo is never mistaken for the real dataset. Uses only cheap
# checks (the small-table source attribute + obs source resolution); it never
# triggers a data scan, so it is safe to build at app startup.

#' Navbar badge (with tooltip) describing the current data provenance.
#' Returns a bslib tooltip wrapping a coloured pill: green "Real data" when the
#' small metadata tables are real, grey "Demo fixtures" otherwise. The tooltip
#' notes where the cell-level obs numbers come from (local / remote / fixture).
tahoe_provenance_badge <- function() {
  tbl <- tryCatch(
    attr(tahoe_drug(), "tahoe_source"),
    error = function(e) "fixture"
  )
  obs <- tryCatch(tahoe_obs_source()$type, error = function(e) "fixture")
  real <- identical(tbl, "real")

  obs_note <- switch(
    obs,
    local = "cell-level obs from a local file",
    remote = "cell-level obs read remotely from HuggingFace",
    fixture = "cell-level obs from the synthetic fixture",
    obs
  )

  if (real) {
    label <- "Real data"
    cls <- "text-bg-success"
    tip <- paste0("Showing real Tahoe-100M metadata (", obs_note, ").")
  } else {
    label <- "Demo fixtures"
    cls <- "text-bg-secondary"
    tip <- paste(
      "Showing bundled synthetic fixtures -- download the metadata for real",
      "Tahoe-100M numbers (see the README)."
    )
  }

  bslib::tooltip(
    tags$span(
      class = paste("badge rounded-pill", cls),
      style = "cursor: default;",
      label
    ),
    tip,
    placement = "bottom"
  )
}
