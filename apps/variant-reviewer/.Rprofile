source("renv/activate.R")

# biobouncer (input validation) is published on r-universe, not CRAN. Keep its
# repo alongside CRAN so install/restore/snapshot can find it.
local({
  repos <- getOption("repos")
  if (is.na(repos["biobouncer"])) {
    repos["biobouncer"] <- "https://samuelbharti.r-universe.dev"
    options(repos = repos)
  }
})
