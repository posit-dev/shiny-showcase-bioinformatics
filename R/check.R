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
    # showcase.ejs derives the "View app" button from `app` and `content_id`,
    # so a hand-written one is a second address for the same application, and
    # the two can disagree.
    texts <- vapply(tiles[[i]]$links %||% list(), function(l) l$text %||% "", "")
    if ("View app" %in% texts) {
      stop(
        name, ": remove the \"View app\" link from ", labels[i],
        ". showcase.ejs makes that button from `app` and `content_id`.",
        call. = FALSE
      )
    }
  }

  # The deployment fields. apps.yml is the source of truth for them, and
  # .github/scripts/deploy_matrix.py turns them into the matrix of
  # .github/workflows/deploy-apps.yml. These tests are the same as the ones in
  # that script, so a problem appears here first, with no network and no runner.
  apps <- vapply(tiles, function(t) t$app %||% NA_character_, "")
  for (i in seq_along(tiles)) {
    tile <- tiles[[i]]
    if (!is.null(tile$content_id) && is.na(apps[i])) {
      stop(
        name, ": ", labels[i], " has `content_id` and no `app`. The content id ",
        "names the Connect Cloud content that the directory in apps/ deploys to.",
        call. = FALSE
      )
    }
    # `deploy` must be a real boolean. A quoted "false" reads as a string, and
    # a string does not hold a deployment back.
    if (!is.null(tile$deploy) && !is.logical(tile$deploy)) {
      stop(
        name, ": `deploy` of ", labels[i], " is \"", tile$deploy,
        "\", and it must be true or false. A quoted \"false\" is a string.",
        call. = FALSE
      )
    }
    # `app` becomes a label in a hostname, so it holds letters, digits and
    # hyphens, and it neither begins nor ends with a hyphen.
    if (!is.na(apps[i]) && !grepl("^[a-z0-9]([a-z0-9-]*[a-z0-9])?$", apps[i])) {
      stop(
        name, ": `app` of ", labels[i], " is \"", apps[i],
        "\". It must be letters, digits and hyphens, because the address of ",
        "the deployment is built from it.",
        call. = FALSE
      )
    }
    if (
      !is.null(tile$content_id) &&
        !grepl(
          "^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$",
          as.character(tile$content_id)
        )
    ) {
      stop(
        name, ": `content_id` of ", labels[i], " is \"", tile$content_id,
        "\", and it does not have the shape of a Connect Cloud content id.",
        call. = FALSE
      )
    }
    if (!is.na(apps[i]) && !dir.exists(here("apps", apps[i]))) {
      stop(
        name, ": `app` of ", labels[i], " is \"", apps[i],
        "\", and apps/", apps[i], "/ does not exist.",
        call. = FALSE
      )
    }
  }
  named <- apps[!is.na(apps)]
  dupe_apps <- unique(named[duplicated(named)])
  if (length(dupe_apps) > 0) {
    stop(
      name, ": more than one tile names the same `app`:\n",
      paste0("  - ", dupe_apps, collapse = "\n"),
      call. = FALSE
    )
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
