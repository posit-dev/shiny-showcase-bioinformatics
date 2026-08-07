---
name: update-thumbnails
description: Capture or refresh Shiny app thumbnails for the gallery. Use when new apps have been added to apps.yml and need screenshots, or when an existing thumbnail needs re-taking. Opens each app in a browser the user can drive, screenshots at 2400x1600, and saves under the canonical name.
---

# Update showcase thumbnails

Thumbnails live in `thumbnails/` and are named canonically from each app's URL.
`R/thumbnail-name.R` is the single source of truth for the convention
(`thumbnail_name()` drops the protocol and any `#fragment`, strips `&` and
percent-escapes, turns `=` and `.` into `_`, then hands the rest to
`fs::path_sanitize()` for `/`, `?` and anything else unsafe, and appends `.png`;
the query string is kept, so apps differing only by it get distinct names). `apps.yml` records the resulting filename in each
tile's `thumbnail` field, and `R/check.R` verifies it exists and matches.

A "new app" is any tile in `apps.yml` whose thumbnail is **absent** from
`thumbnails/`.

Run R **interactively** (in the Positron/RStudio console). Do **not** use `Rscript`.

## Steps

1. **List apps missing a thumbnail.** In R:
   ```r
   source("R/check.R")
   ```
   `apps.yml` is the source of truth and is edited by hand — nothing regenerates
   it. `check.R` errors with the title and filename of every tile whose
   thumbnail isn't on disk, and warns about names that drift from the
   convention. Needs a restored renv library (`renv::restore()` if packages are
   missing).

   For a tile you're adding, derive the filename first so you know what to
   capture:
   ```r
   source(here::here("R", "thumbnail-name.R"))
   thumbnail_name("https://kdph.shinyapps.io/atlas/")
   ```

3. **Capture each missing app.** For one URL at a time:
   ```r
   source("R/capture.R")
   url <- "https://kdph.shinyapps.io/atlas/"
   b <- open_app(url)          # opens a viewable window, 2400x1600 output
   ```
   If the app opens looking small/zoomed-out, re-open with a `zoom` factor —
   the output stays 2400x1600 but content renders larger and sharper:
   ```r
   b$close(); b <- open_app(url, zoom = 2)
   ```
   Now get the app into the view you want before capturing:
   - Interact directly in the window (dismiss dialogs, click tabs, scroll), **or**
   - Drive it from R, e.g.
     `b$Runtime$evaluate('document.querySelector("a[data-value=\\"Map\\"]").click()')`
   - Give slow Shiny apps time to finish loading (`Sys.sleep(6)` if needed).

   When it looks right:
   ```r
   capture_app(b, url)   # saves thumbnails/<derived name>.png
   b$close()
   ```
   Ask the user to confirm the saved PNG looks good (interesting view, no spinners
   or modals) before moving on.

4. **Re-check and preview.**
   ```r
   source("R/check.R")
   ```
   ```bash
   quarto preview
   ```
   Check every card renders an image — no broken thumbnails.

5. **Publish** via the Posit Publisher extension in Positron (see README).

## Notes

- Record the filename in the tile's `thumbnail` field in `apps.yml`, and keep it
  equal to `thumbnail_name(url)` — `check.R` warns when the two drift apart.
- If an app needs a specific tab/scroll state, that's exactly why capture is manual:
  drive the live session, then `capture_app()`.
