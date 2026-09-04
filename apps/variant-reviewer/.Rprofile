source("renv/activate.R")

# biobouncer (input validation) is published on r-universe, not CRAN. Keep its
# repo alongside CRAN so install/restore/snapshot can find it.
local({
  repos <- getOption("repos")
  # `is.na(repos["biobouncer"])` is logical(0) when `repos` is empty, and
  # `if (logical(0))` is an error. Connect Cloud starts R with no repos set.
  if (!("biobouncer" %in% names(repos))) {
    repos["biobouncer"] <- "https://samuelbharti.r-universe.dev"
    options(repos = repos)
  }
})
