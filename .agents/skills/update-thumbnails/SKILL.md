---
name: update-thumbnails
description: Capture or replace the thumbnail of a Shiny app for the gallery. Use when a tile in apps.yml or packages.yml still shows the placeholder image, or when an existing thumbnail is out of date. Opens each app in a browser that the user controls. Captures the screen at 2400x1600. Saves the file with the name of the app.
---

# Update the gallery thumbnails

The thumbnails are in `thumbnails/`. Each file has the name of its application,
for example `genescout.png` or `variant-reviewer.png`. Each tile in `apps.yml`
and `packages.yml` records the file name in its `thumbnail` field. `R/check.R`
makes sure that the file is on disk.

These applications are first-party, and most of them are not deployed. They
operate at a local address such as `http://127.0.0.1:3838`, which identifies
nothing. Therefore the file name does not come from the URL. `R/thumbnail-name.R`
holds the URL convention of the upstream gallery, which applies only if a
third-party application is added.

A tile needs a screenshot if its `thumbnail` field is `placeholder.svg`.
`R/check.R` lists these tiles after the words "awaiting a real screenshot".

Operate R in the console of Positron or RStudio. Do not use `Rscript`. The
capture step opens a browser window that you control by hand.

## Steps

1. **Find the tiles that still show the placeholder.** In R:

   ```r
   source("R/check.R")
   ```

   A person edits `apps.yml` and `packages.yml` by hand, and no script writes
   them. `check.R` gives an error if a thumbnail is absent from disk. It also
   reports the tiles that still use `placeholder.svg`. If the packages are
   absent, restore the renv library with `renv::restore()`.

2. **Start the application.** These applications are not deployed. Operate the
   application locally, from its own repository, in a different R session. Write
   down the port number.

3. **Capture the screen.** Do one application at a time:

   ```r
   source("R/capture.R")
   url <- "http://127.0.0.1:3838"
   b <- open_app(url)          # opens a window you can see, 2400x1600 output
   ```

   If the content looks too small, open the application again with a `zoom`
   value. The output stays at 2400x1600, but the content becomes larger and
   sharper:

   ```r
   b$close(); b <- open_app(url, zoom = 2)
   ```

   Put the application into the view that you want. There are two methods:
   - Use the window directly. Close dialogs, select tabs, and scroll.
   - Control the window from R, for example
     `b$Runtime$evaluate('document.querySelector("a[data-value=\\"Map\\"]").click()')`

   If the application is slow, wait for it to finish. `Sys.sleep(6)` is
   sufficient for most applications.

   When the view is correct, give the `file` argument. Use the name of the
   application:

   ```r
   capture_app(b, url, file = "genescout.png")
   b$close()
   ```

   Ask the user to look at the PNG file before you continue. The image must show
   an interesting view, with no spinner and no dialog.

4. **Point the tile to the new file.** In `apps.yml` or `packages.yml`, change
   the `thumbnail` field from `placeholder.svg` to the new file name. Do not
   change `fit`, unless you captured a diagram instead of a screenshot. For a
   diagram, set `fit: contain`.

5. **Check the data and see the site.**

   ```r
   source("R/check.R")
   ```

   ```bash
   quarto preview
   ```

   Make sure that every card in every section shows an image.

## Notes

A card crops the image to 3:2 from the top left corner, because the default
`fit` is `cover`. Put the header of the application, and its most legible
content, in the top left of the capture. The image does not need to be exactly
2400x1600, because CSS controls the size of the box. A capture at that size
keeps the crop sharp.

`check.R` gives a warning for an image in `thumbnails/` that no tile uses. This
warning finds a rename that you applied to only one of the two places.

Capture is a manual step because an application often needs a specific tab or
scroll position. Control the live session, then call `capture_app()`.
