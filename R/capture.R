# install.packages(c("webshot2", "chromote"))
library(chromote)
library(here)
source(here("R", "thumbnail-name.R"))

#' Open a Shiny application in a browser window, at the size for a thumbnail.
#'
#' The function gives a live ChromoteSession. Call `b$view()` to open a window
#' that you can use. In that window you can close dialogs, select tabs, and
#' scroll. You can also control the same session from R, for example:
#'   b$Runtime$evaluate("document.querySelector('a[data-value=\"Map\"]').click()")
#' When the application shows the correct view, give the session to
#' `capture_app()`.
#'
#' @param url The URL of the application.
#' @param width,height The size of the output image in pixels. The default is
#'   2400x1600. This size does not change with the size of the window.
#' @param zoom The size of the application content. The function makes the
#'   viewport `width/zoom` x `height/zoom`, and sets the device scale factor to
#'   `zoom`. The output stays `width` x `height` pixels, but the content becomes
#'   `zoom` times larger and sharper. The default is 1.5. If the content is too
#'   small, use a larger value. If the content is cut, use a value near 1.
#' @param view If TRUE, the function opens the window immediately. TRUE is the
#'   default.
#'
#' @examples
#' url <- "http://127.0.0.1:3838"
#' b <- open_app(url, zoom = 2)
#' # ...interact in the window or via b$Runtime$evaluate(...)...
#' capture_app(b, url, file = "genescout.png")
#' b$close()
open_app <- function(url, width = 2400, height = 1600, zoom = 1.5, view = TRUE) {
  b <- ChromoteSession$new()
  b$Emulation$setDeviceMetricsOverride(
    width = round(width / zoom), height = round(height / zoom),
    deviceScaleFactor = zoom, mobile = FALSE
  )
  # Hide the scrollbar. A tall application leaves a gray band at the right edge
  # of the image if the scrollbar is visible.
  b$Emulation$setScrollbarsHidden(hidden = TRUE)
  b$Page$navigate(url)
  b$Page$loadEventFired()
  if (view) b$view()
  b
}

#' Capture the screen of an open application session.
#'
#' The function writes a PNG file into thumbnails/.
#'
#' @param b A live ChromoteSession from `open_app()`.
#' @param url The URL of the application. The function uses it to make a file
#'   name only when `file` is absent.
#' @param file The name of the output file. Always give this argument. These
#'   applications are first-party, and most of them operate at a local address
#'   that identifies nothing. Therefore each thumbnail has the name of its
#'   application, for example `"genescout.png"`, which is also the value of the
#'   `thumbnail` field of the tile. The `thumbnail_name(url)` default is correct
#'   only for a third-party application with a public URL.
capture_app <- function(b, url, file = NULL) {
  if (is.null(file)) file <- thumbnail_name(url)
  out_path <- here("thumbnails", file)
  # Capture the viewport only. This gives an image of `width` x `height`
  # pixels, at the device scale factor. Do not use b$screenshot() here. Its
  # default selector is "html", so it captures the complete page element. The
  # size of that image changes with the page, and it is often taller than it is
  # wide.
  res <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(res$data), out_path)
  message("Saved: ", out_path)
  invisible(out_path)
}
