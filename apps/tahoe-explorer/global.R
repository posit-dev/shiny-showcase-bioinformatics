# Load libraries and source files.
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(janitor)
library(reactable)
library(plotly)
library(echarts4r) # modern bar/histogram charts (Samples & plates tab)
library(DBI)
library(duckdb)
library(cicerone) # click-through guided tours (see R/tour.R)

# ui.R calls bslib::bs_theme(brand = TRUE), which reads _brand.yml through the
# brand.yml package. bslib only suggests that package, so a scan of this code
# does not find it and manifest.json does not name it. Connect Cloud then
# installs everything except brand.yml, and bs_theme() stops with
# "The package `brand.yml` is required". This call makes the dependency visible
# to rsconnect::writeManifest().
requireNamespace("brand.yml", quietly = TRUE)

# Plots are themed explicitly via R/theme.R (tahoe_theme / tahoe_plotly) and
# rendered as interactive plotly widgets, so no thematic auto-styling is needed.

# Source utilities (incl. the data layer, plot theme, and page registry),
# modules, then the page-level UI files, which self-register into the navbar.
source("R/load_components.R")
