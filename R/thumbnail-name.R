library(stringr)
library(fs)

#' Make the thumbnail file name from the URL of an application.
#'
#' NOTE: this convention applies to a third-party application, which a public
#' URL identifies. The applications in this gallery are first-party, and their
#' thumbnails have the name of the application. Nothing in this repository uses
#' this function now. Keep it for a third-party application in the future.
#'
#' The query string is part of the name, because it is the only difference
#' between some applications. For example, the three Small Molecule Suite tiles
#' have the same path, and only their `tab=` value is different.
#'
#' The function does these operations, in this sequence:
#'
#' * It removes the protocol.
#' * It removes a `#fragment`. A fragment is a view state in the browser, and it
#'   does not identify the application.
#' * It removes `&` and percent-escapes such as `%22`. It changes `=` and `.`
#'   to `_`. A file name can contain these characters, so `path_sanitize()`
#'   keeps them, but they make the name difficult to read.
#' * `fs::path_sanitize()` then changes the characters that are unsafe. It
#'   changes `/` and `?` to `_`, and also `"`, `:`, `|`, `*`, `<`, `>` and the
#'   control characters. It also escapes a name that is only a reserved word of
#'   Windows, such as `CON` or `NUL`.
#' * It cuts the result to 251 characters and adds `.png`. The complete name is
#'   then less than the limit of 255 bytes that most file systems have. The
#'   function makes the name safe before it adds the extension, so the extension
#'   is always correct.
#'
#' As a result, `_` is the only separator, and the name contains one `.`, before
#' the extension.
#'
#' @param url The URL of the application.
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
