# Small presentation helpers shared by the result modules, so every card
# renders fields, empty states, and errors the same way.

# Null-coalescing operator (base R has this from 4.4.0; defined for older R).
`%||%` <- function(x, y) if (is.null(x)) y else x # nolint: object_name_linter.

# App palette (organic, mirrors the color palette in _brand.yml). Used where a
# color must be set in R (e.g. ggplot geoms) rather than via the CSS theme.
vr_colors <- list(
  primary = "#4c7a5b", # sage/forest green
  accent = "#c07a52", # warm clay
  stone = "#7a7468" # muted stone/taupe
)

# A "Label: value" row. Hides the row entirely when the value is blank.
vr_field <- function(label, value) {
  if (is_blank(value)) {
    return(NULL)
  }
  tags$p(
    class = "mb-2",
    tags$strong(paste0(label, ": ")),
    tags$span(as.character(value))
  )
}

# Neutral placeholder shown before a search or when a section has no data.
vr_empty <- function(message) {
  tags$p(class = "text-muted fst-italic mb-0", message)
}

# Error/warning state for failed API calls.
vr_error <- function(message) {
  tags$div(
    class = "alert alert-warning mb-0 py-2 px-3",
    role = "alert",
    message
  )
}

# Format a number for display, returning a dash for missing values.
vr_num <- function(x, digits = 2) {
  if (is_blank(x) || is.na(suppressWarnings(as.numeric(x)))) {
    return("—")
  }
  formatC(as.numeric(x), format = "f", digits = digits, drop0trailing = TRUE)
}
