---
name: update-thumbnails
description: Capture or refresh Shiny app thumbnails for the gallery. Use when an app in apps.yml or packages.yml is still on the placeholder and needs a real screenshot, or when an existing thumbnail needs re-taking. Opens each app in a browser the user can drive, screenshots at 2400x1600, and saves it under the app's name.
---

# Update showcase thumbnails

Thumbnails live in `thumbnails/` and are named after the app: `genescout.png`,
`variant-reviewer.png`. Each tile in `apps.yml` / `packages.yml` records the
filename in its `thumbnail` field, and `R/check.R` verifies the file exists.

These apps are **first-party and mostly not deployed** — they run locally, at a
URL like `http://127.0.0.1:3838` that says nothing about which app it is. So
unlike an upstream gallery of third-party apps, the filename is *not* derived
from the URL. (`R/thumbnail-name.R` still holds that URL-based convention; it
applies only if a third-party app is ever added.)

An app that **needs** a screenshot is any tile whose `thumbnail` is
`placeholder.svg`. `R/check.R` lists them under "awaiting a real screenshot".

Run R **interactively** (in the Positron/RStudio console). Do **not** use `Rscript`.

## Steps

1. **List the tiles still on the placeholder.** In R:
   ```r
   source("R/check.R")
   ```
   `apps.yml` and `packages.yml` are the source of truth and are edited by hand —
   nothing regenerates them. `check.R` errors on a tile whose thumbnail is missing
   from disk entirely, and reports which tiles are still on `placeholder.svg`.
   Needs a restored renv library (`renv::restore()` if packages are missing).

2. **Start the app you're capturing.** These are not deployed, so run it locally
   first — from its own repository, in a separate R session — and note the port.

3. **Capture it.** One app at a time:
   ```r
   source("R/capture.R")
   url <- "http://127.0.0.1:3838"
   b <- open_app(url)          # opens a viewable window, 2400x1600 output
   ```
   If the app opens looking small/zoomed-out, re-open with a `zoom` factor — the
   output stays 2400x1600 but content renders larger and sharper:
   ```r
   b$close(); b <- open_app(url, zoom = 2)
   ```
   Now get the app into the view you want before capturing:
   - Interact directly in the window (dismiss dialogs, click tabs, scroll), **or**
   - Drive it from R, e.g.
     `b$Runtime$evaluate('document.querySelector("a[data-value=\\"Map\\"]").click()')`
   - Give slow Shiny apps time to finish loading (`Sys.sleep(6)` if needed).

   When it looks right — **pass `file` explicitly**, named after the app:
   ```r
   capture_app(b, url, file = "genescout.png")
   b$close()
   ```
   Ask the user to confirm the saved PNG looks good (interesting view, no spinners
   or modals) before moving on.

4. **Point the tile at it.** Change that tile's `thumbnail` from `placeholder.svg`
   to the new filename. Leave `fit` alone unless you captured a diagram rather
   than a screenshot, in which case set `fit: contain`.

5. **Re-check and preview.**
   ```r
   source("R/check.R")
   ```
   ```bash
   quarto preview
   ```
   Check every card on both tabs renders an image — no broken thumbnails.

## Notes

- Cards crop to 3:2 from the top-left (`fit: cover`), so put the app's header and
  its most legible content in the upper-left of the capture. Nothing needs to be
  exactly 2400x1600 — CSS handles the box — but capturing at that size keeps the
  crop sharp.
- `check.R` warns about images in `thumbnails/` that no tile references, which is
  how you catch a rename that only got applied on one side.
- If an app needs a specific tab/scroll state, that's exactly why capture is manual:
  drive the live session, then `capture_app()`.
