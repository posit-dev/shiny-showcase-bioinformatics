# install.packages(c("webshot2", "chromote"))
library(chromote)
library(here)
source(here("R", "thumbnail-name.R"))

#' Open a Shiny app in a viewable browser session, sized for thumbnail capture.
#'
#' Returns a live ChromoteSession. Call `b$view()` to open an interactive window
#' where you can dismiss dialogs, click tabs, and scroll. You can also drive the
#' same session programmatically, e.g.
#'   b$Runtime$evaluate("document.querySelector('a[data-value=\"Map\"]').click()")
#' When the app shows the view you want, pass the session to `capture_app()`.
#'
#' @param url App URL.
#' @param width,height Output screenshot size in pixels (default 2400x1600).
#'   This is fixed regardless of window size.
#' @param zoom How large the app content renders. The logical viewport is
#'   shrunk to `width/zoom` x `height/zoom` and the device scale factor set to
#'   `zoom`, so the output stays `width` x `height` pixels but content appears
#'   `zoom` times larger (and sharper). Defaults to 1.5; bump it higher when an
#'   app still opens looking small/zoomed-out, or drop toward 1 if content clips.
#' @param view Open the interactive window immediately (default TRUE).
#'
#' @examples
#' b <- open_app("https://kdph.shinyapps.io/atlas/", zoom = 2)
#' # ...interact in the window or via b$Runtime$evaluate(...)...
#' capture_app(b, "https://kdph.shinyapps.io/atlas/")
#' b$close()
open_app <- function(url, width = 2400, height = 1600, zoom = 1.5, view = TRUE) {
  b <- ChromoteSession$new()
  b$Emulation$setDeviceMetricsOverride(
    width = round(width / zoom), height = round(height / zoom),
    deviceScaleFactor = zoom, mobile = FALSE
  )
  # Hide the scrollbar so tall apps don't leave a gray gutter on the right edge.
  b$Emulation$setScrollbarsHidden(hidden = TRUE)
  b$Page$navigate(url)
  b$Page$loadEventFired()
  if (view) b$view()
  b
}

#' Screenshot the current state of an open app session.
#'
#' Saves a PNG to thumbnails/ using the canonical name from `thumbnail_name()`.
#'
#' @param b A live ChromoteSession from `open_app()`.
#' @param url The app URL (used to derive the filename).
#' @param file Override the output filename. Defaults to `thumbnail_name(url)`.
capture_app <- function(b, url, file = NULL) {
  if (is.null(file)) file <- thumbnail_name(url)
  out_path <- here("thumbnails", file)
  # Capture exactly the emulated viewport, giving a fixed `width` x `height`
  # crop (at the device scale factor). Note: b$screenshot() defaults to
  # selector = "html" and captures the *whole page* element, which produces
  # variable, often portrait, dimensions — not what we want for a thumbnail.
  res <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(res$data), out_path)
  message("Saved: ", out_path)
  invisible(out_path)
}
