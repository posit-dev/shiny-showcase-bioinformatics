library(stringr)
library(fs)

#' Canonical thumbnail filename for an app URL.
#'
#' The single source of truth for the naming convention, sourced by both
#' `R/check.R` (to validate each tile's `thumbnail`) and `R/capture.R` (to save
#' screenshots).
#'
#' The query string is part of the name, because some apps are distinguished
#' only by it: the three Small Molecule Suite tiles share a path and differ
#' only in `tab=`. So:
#'
#' * the protocol is dropped;
#' * a `#fragment` is dropped, being client-side view state rather than app
#'   identity;
#' * `&` and percent-escapes (`%22` and friends) are stripped, and `=` and `.`
#'   become `_`, all of them legal in a filename, so `path_sanitize()` leaves
#'   them be, but all of them noise in a name;
#' * `fs::path_sanitize()` handles everything that is genuinely unsafe: `/` and
#'   `?` become `_`, as do `"`, `:`, `|`, `*`, `<`, `>` and control characters,
#'   and a name that is *entirely* a Windows reserved word (`CON`, `NUL`, ...)
#'   is escaped;
#' * the result is trimmed to 251 characters and `.png` appended, keeping the
#'   whole name inside the 255-byte limit most filesystems impose. Sanitizing
#'   before appending means the extension is never mangled.
#'
#' The upshot is that `_` is the only separator and the sole `.` is the one
#' before the extension.
#'
#' @param url App URL.
#'
#' @examples
#' thumbnail_name("https://rconnect.usgs.gov/PA_radon_map/")
#' #> "rconnect_usgs_gov_PA_radon_map_.png"
#' thumbnail_name("https://skylab.cdph.ca.gov/communityBurden/#tab-2190-1")
#' #> "skylab_cdph_ca_gov_communityBurden_.png"
#' thumbnail_name("https://lsp.connect.hms.harvard.edu/smallmoleculesuite/?_inputs_&tab=%22library%22")
#' #> "lsp_connect_hms_harvard_edu_smallmoleculesuite___inputs_tab_library.png"
thumbnail_name <- function(url) {
  url |>
    str_remove("^https?://") |>
    str_remove("#.*$") |>
    str_remove_all("%[0-9A-Fa-f]{2}") |>
    str_remove_all("&") |>
    str_replace_all("[=.]", "_") |>
    path_sanitize(replacement = "_") |>
    str_trunc(251, ellipsis = "") |>
    paste0(".png")
}
