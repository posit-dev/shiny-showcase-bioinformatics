# Load libraries and source files
library(shiny)
library(bslib)

# The loading spinner defaults to a saturated Bootstrap blue that clashes with
# the theme. Pull it to the bronze accent. Read from the compiled theme rather
# than hard-coded so _brand.yml stays the single source of truth for colour.
options(
  spinner.color = bslib::bs_get_variables(
    bslib::bs_theme(brand = TRUE),
    "primary"
  )[["primary"]]
)

# Identify this app to the APIs it queries. biohttp builds its User-Agent from
# these. Guard with nzchar rather than Sys.getenv(unset=): an empty (not absent)
# value would otherwise produce a bare "User-Agent: /0.1.x".
if (!nzchar(Sys.getenv("BIOHTTP_CALLER_IDENTITY"))) {
  Sys.setenv(BIOHTTP_CALLER_IDENTITY = "gene-list-builder")
}
if (!nzchar(Sys.getenv("BIOHTTP_CONTACT_URL"))) {
  Sys.setenv(
    BIOHTTP_CONTACT_URL = "https://github.com/samuelbharti/gene-list-builder"
  )
}
source("R/load_components.R")

# Salt the response cache with the app version so a change to a parser cannot
# read back an entry produced by the previous one. Set after sourcing, since
# app_version() lives in R/utils.R.
if (!nzchar(Sys.getenv("BIOHTTP_CACHE_SALT"))) {
  Sys.setenv(BIOHTTP_CACHE_SALT = app_version())
}

# Load data/connections
# Example: app_data <- readRDS("data/app_data.rds")

# Preprocess small data
