# Validates apps.yml and packages.yml. Run before previewing or publishing:
#
#   source("R/check.R")
#
# Both files are hand-edited and are the source of truth, so nothing regenerates
# them, this only checks them. Errors on problems that would break a page (a
# tile missing a field, an unknown fit, a thumbnail that isn't on disk) and
# warns on things that are merely suspect (a duplicate title, an image in
# thumbnails/ that no tile references).
#
# Note on thumbnail names: these apps are first-party and mostly have no public
# URL, so thumbnails are named after the app (`<slug>.png`) rather than derived
# from a URL. `R/thumbnail-name.R` still holds the URL-based convention, which
# applies only if a third-party app is ever added.

library(yaml)
library(here)

`%||%` <- function(x, y) if (is.null(x)) y else x

FITS <- c("cover", "contain", "hex")
REQUIRED <- c("title", "org", "description", "thumbnail")
PLACEHOLDER <- "placeholder.svg"

check_file <- function(path) {
  name <- basename(path)
  gallery <- read_yaml(path)

  tiles <- unlist(lapply(gallery, `[[`, "tiles"), recursive = FALSE)
  if (length(tiles) == 0) {
    warning(name, " has no tiles.", call. = FALSE)
    return(character(0))
  }
  labels <- vapply(tiles, function(t) t$title %||% "<untitled>", "")

  # Errors: these render a broken page.
  incomplete <- vapply(tiles, function(t) !all(REQUIRED %in% names(t)), logical(1))
  if (any(incomplete)) {
    missing <- vapply(
      tiles[incomplete],
      function(t) paste(setdiff(REQUIRED, names(t)), collapse = ", "), ""
    )
    stop(
      name, ": tiles missing required fields:\n",
      paste0("  - ", labels[incomplete], " (missing: ", missing, ")", collapse = "\n"),
      call. = FALSE
    )
  }

  fits <- vapply(tiles, function(t) t$fit %||% "cover", "")
  bad_fit <- !fits %in% FITS
  if (any(bad_fit)) {
    stop(
      name, ": unknown `fit` (expected one of ", paste(FITS, collapse = ", "), "):\n",
      paste0("  - ", labels[bad_fit], " (", fits[bad_fit], ")", collapse = "\n"),
      call. = FALSE
    )
  }

  thumbs <- vapply(tiles, `[[`, "", "thumbnail")
  absent <- !file.exists(here("thumbnails", thumbs))
  if (any(absent)) {
    stop(
      name, ": thumbnails not found in thumbnails/, capture them with ",
      "R/capture.R (or the update-thumbnails skill), or use ", PLACEHOLDER, ":\n",
      paste0("  - ", labels[absent], " (", thumbs[absent], ")", collapse = "\n"),
      call. = FALSE
    )
  }

  # Every link needs both halves, or the button renders pointing at nothing.
  for (i in seq_along(tiles)) {
    for (link in (tiles[[i]]$links %||% list())) {
      if (!all(c("text", "url") %in% names(link))) {
        stop(
          name, ": every entry under `links` needs both `text` and `url`, see ",
          labels[i],
          call. = FALSE
        )
      }
    }
  }

  # Warnings: the page still renders, but something is off.
  dupes <- unique(labels[duplicated(labels)])
  if (length(dupes) > 0) {
    warning(
      name, ": duplicate titles:\n", paste0("  - ", dupes, collapse = "\n"),
      call. = FALSE
    )
  }

  pending <- thumbs == PLACEHOLDER
  message(
    name, " ok: ", length(tiles), " tiles in ", length(gallery),
    if (length(gallery) == 1) " category (" else " categories (",
    paste(vapply(gallery, `[[`, "", "category"), collapse = ", "), ")",
    if (any(pending)) {
      paste0(
        "\n  awaiting a real screenshot: ",
        paste(labels[pending], collapse = ", ")
      )
    } else {
      ""
    }
  )
  thumbs
}

check_apps <- function(paths = c(here("apps.yml"), here("packages.yml"))) {
  used <- unique(unlist(lapply(paths, check_file)))

  on_disk <- basename(list.files(here("thumbnails")))
  orphans <- setdiff(on_disk, c(used, PLACEHOLDER))
  if (length(orphans) > 0) {
    warning(
      "Images in thumbnails/ that no tile references:\n",
      paste0("  - ", orphans, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

check_apps()
