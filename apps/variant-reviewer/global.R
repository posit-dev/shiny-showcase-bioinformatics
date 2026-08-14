# Load libraries and source files
library(shiny)
library(bslib)

# bs_theme(brand = TRUE) in ui.R reads _brand.yml via the {brand.yml} package.
# This reference keeps it visible to renv (it is otherwise a phantom dependency)
# and fails fast with a clear message if it is missing.
if (!requireNamespace("brand.yml", quietly = TRUE)) {
  stop("The 'brand.yml' package is required (used by bs_theme(brand = TRUE)).")
}

# Optionally theme base/ggplot/lattice output to match the app theme. Activates
# only if the {thematic} package is installed, so it adds no hard dependency.
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny(font = "auto")
}

# Theme reactable tables to match the bslib theme. reactable renders its own
# (light) theme by default, which clashes with the app palette; binding its
# colors to Bootstrap CSS variables makes every table adopt the theme (borders,
# text, header, hover) and track it if the palette changes.
options(
  reactable.theme = reactable::reactableTheme(
    color = "var(--bs-body-color)",
    backgroundColor = "transparent",
    borderColor = "var(--bs-border-color)",
    stripedColor = "var(--bs-tertiary-bg)",
    highlightColor = "var(--bs-secondary-bg)",
    cellPadding = "6px 8px",
    headerStyle = list(
      backgroundColor = "var(--bs-tertiary-bg)",
      borderColor = "var(--bs-border-color)"
    )
  )
)

# Hide the assistant's tool-call cards in chat responses so the conversation
# reads cleanly (the assistant still uses tools; only their UI cards are hidden).
# Choices: "none", "basic", "rich" (shinychat default).
options(shinychat.tool_display = "none")

source("R/load_components.R")

# Warm the demo example (BRAF V600E) from its recorded snapshot so the guided
# demo loads from memory rather than waiting on ~16 live API calls. No-op when
# the snapshot is absent (a fresh checkout before dev/record_demo_cache.R has
# run), so the app still works, the first demo is just cold.
vr_seed_demo_cache()

# Load data/connections
# Example: app_data <- readRDS("data/app_data.rds")

# Preprocess small data
