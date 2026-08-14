# This script examines apps.yml and packages.yml. Operate it before you look at
# the site, and before you publish:
#
#   source("R/check.R")
#
# A person edits both files by hand, and no script writes them. This script only
# reads them. It gives an error for a problem that breaks a page. Examples are a
# tile with a missing field, an unknown `fit` value, and a thumbnail that is
# absent from disk. It gives a warning for a problem that does not break a page.
# Examples are a duplicate title, and an image in thumbnails/ that no tile uses.
#
# Each thumbnail has the name of its application, for example `<slug>.png`. The
# name does not come from a URL, because these applications are first-party and
# most of them have no public address. `R/thumbnail-name.R` holds the URL
# convention, which applies only if you add a third-party application.

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
    warning(name, " contains no tiles.", call. = FALSE)
    return(character(0))
  }
  labels <- vapply(tiles, function(t) t$title %||% "<untitled>", "")

  # These conditions give an error, because they break the page.
  incomplete <- vapply(tiles, function(t) !all(REQUIRED %in% names(t)), logical(1))
  if (any(incomplete)) {
    missing <- vapply(
      tiles[incomplete],
      function(t) paste(setdiff(REQUIRED, names(t)), collapse = ", "), ""
    )
    stop(
      name, ": these tiles have no value for a required field:\n",
      paste0("  - ", labels[incomplete], " (missing: ", missing, ")", collapse = "\n"),
      call. = FALSE
    )
  }

  fits <- vapply(tiles, function(t) t$fit %||% "cover", "")
  bad_fit <- !fits %in% FITS
  if (any(bad_fit)) {
    stop(
      name, ": unknown `fit` value. Use one of ", paste(FITS, collapse = ", "), ":\n",
      paste0("  - ", labels[bad_fit], " (", fits[bad_fit], ")", collapse = "\n"),
      call. = FALSE
    )
  }

  thumbs <- vapply(tiles, `[[`, "", "thumbnail")
  absent <- !file.exists(here("thumbnails", thumbs))
  if (any(absent)) {
    stop(
      name, ": these thumbnails are absent from thumbnails/. Capture them ",
      "with R/capture.R or the update-thumbnails skill, or use ", PLACEHOLDER, ":\n",
      paste0("  - ", labels[absent], " (", thumbs[absent], ")", collapse = "\n"),
      call. = FALSE
    )
  }

  # A link needs both halves. If one is absent, the button points to nothing.
  for (i in seq_along(tiles)) {
    for (link in (tiles[[i]]$links %||% list())) {
      if (!all(c("text", "url") %in% names(link))) {
        stop(
          name, ": each entry in `links` needs both `text` and `url`. See ",
          labels[i],
          call. = FALSE
        )
      }
    }
  }

  # These conditions give a warning. The page is correct, but something is
  # probably an error.
  dupes <- unique(labels[duplicated(labels)])
  if (length(dupes) > 0) {
    warning(
      name, ": these titles are used more than one time:\n", paste0("  - ", dupes, collapse = "\n"),
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
      "No tile uses these images in thumbnails/:\n",
      paste0("  - ", orphans, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

check_apps()
