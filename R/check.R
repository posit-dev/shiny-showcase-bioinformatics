# Validates apps.yml. Run before previewing or publishing:
#
#   source("R/check.R")
#
# apps.yml is hand-edited and is the source of truth, so nothing regenerates it
# — this only checks it. Errors on problems that would break the page (a tile
# missing a field, a thumbnail that isn't on disk) and warns on things that are
# merely suspect (a filename that drifts from the convention, a duplicate URL).

library(yaml)
library(here)
source(here("R", "thumbnail-name.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

check_apps <- function(path = here("apps.yml")) {
  gallery <- read_yaml(path)
  required <- c("org", "title", "url", "description", "thumbnail")

  tiles <- unlist(lapply(gallery, `[[`, "tiles"), recursive = FALSE)
  labels <- vapply(tiles, function(t) t$title %||% "<untitled>", "")

  # Errors: these render a broken page.
  incomplete <- vapply(tiles, function(t) !all(required %in% names(t)), logical(1))
  if (any(incomplete)) {
    stop(
      "Tiles missing required fields (", paste(required, collapse = ", "), "):\n",
      paste0("  - ", labels[incomplete], collapse = "\n"),
      call. = FALSE
    )
  }

  thumbs <- vapply(tiles, `[[`, "", "thumbnail")
  absent <- !file.exists(here("thumbnails", thumbs))
  if (any(absent)) {
    stop(
      "Thumbnails not found in thumbnails/ — capture them with R/capture.R ",
      "(or the update-thumbnails skill):\n",
      paste0("  - ", labels[absent], " (", thumbs[absent], ")", collapse = "\n"),
      call. = FALSE
    )
  }

  # Warnings: the page still renders, but something is off.
  urls <- vapply(tiles, `[[`, "", "url")
  drifted <- thumbs != vapply(urls, thumbnail_name, "")
  if (any(drifted)) {
    warning(
      "Thumbnails not named per R/thumbnail-name.R:\n",
      paste0(
        "  - ", labels[drifted], "\n      is:       ", thumbs[drifted],
        "\n      expected: ", vapply(urls[drifted], thumbnail_name, ""),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  dupes <- unique(urls[duplicated(urls)])
  if (length(dupes) > 0) {
    warning(
      "Duplicate URLs:\n", paste0("  - ", dupes, collapse = "\n"),
      call. = FALSE
    )
  }

  message(
    "apps.yml ok: ", length(tiles), " apps in ", length(gallery), " categories (",
    paste(vapply(gallery, `[[`, "", "category"), collapse = ", "), ")"
  )
  invisible(TRUE)
}

check_apps()
