# Contributing

Bug reports, feature ideas, and pull requests are all welcome.

## Set up

You need R 4.2 or a later version. Install the exact package versions with
renv:

```r
install.packages("renv")
renv::restore()
```

recount3 comes from Bioconductor. The first restore takes a long time.

`renv.lock` records the packages that the app needs at run time. It does not
record the development tools. A recursive read of every `Suggests` field pulls
in several hundred packages that nobody needs. Install the two tools once:

```r
install.packages(c("testthat", "lintr"))
```

There is no build step and no JavaScript toolchain. The interface is bslib, so
changing it means editing R and reloading the app.

Two command line tools handle the formatting and the git hooks:

- [air](https://posit-dev.github.io/air/) formats R code
- [prek](https://github.com/j178/prek) runs the pre-commit hooks

Install the hooks one time after you clone the repository:

```sh
prek install
```

## Branches and pull requests

`main` holds released work. `dev` is the integration branch. Every other branch
starts from `dev` and merges back into `dev`:

```sh
git checkout dev
git pull
git checkout -b feat/short-description
```

Start the pull request title with `feat:`, `fix:`, `docs:`, `chore:`, or
`refactor:`. Keep each pull request to one subject. Small pull requests get a
review faster than large ones.

## Versioning

`DESCRIPTION` carries the version as three numbers: `MAJOR.MINOR.PATCH`, for
example `0.1.0`. Bump it on the pull request from `dev` to `main`, not on
every feature branch:

- **PATCH** for a bug fix that changes no behaviour a user asked for.
- **MINOR** for a new view, plot, or option.
- **MAJOR** once the app is stable enough that a breaking change is worth
  calling out. Below `1.0.0`, treat this the same as MINOR.

A merge to `main` gets a matching git tag, `v0.1.0` for `DESCRIPTION`'s
`0.1.0`. Tag after the merge, from `main`:

```sh
git checkout main && git pull
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```

## Before you push

```sh
air format .
```

```r
lintr::lint_dir(".")
testthat::test_dir("tests/testthat")
```

`prek run --all-files` runs the same checks that the git hooks run. CI runs the
formatter, the linter, and the tests on every pull request.

The tests must not use the network. If a test needs data from the recount3
servers, put a fixture in `tests/testthat/fixtures/` instead.

## Code layout

The app keeps the computation apart from the interface. Keep it that way:

- `R/logic_*.R` is Shiny-free. Plain arguments go in and plain data comes out.
  You can test these functions without a running app. Put new computation here.
- `R/mod_*.R` holds the Shiny modules, one module for each view. The modules
  talk to each other through the shared `study` reactive and nothing else.
- `app.R` does the wiring only.

If you write `req()` or read `input$` inside a `logic_` file, move that code to
a module. If you write computation inside a module, move it to a `logic_` file.

## Changing colours

Every colour comes from [`_brand.yml`](_brand.yml). bslib reads it for the
Bootstrap theme and `R/logic_brand.R` reads it for the plots. Edit that file
rather than hunting for hex codes in the R or the CSS.

To try a palette without touching the repository, point
`RECOUNT_EXPLORER_BRAND` at another file.

## The catalog snapshot

`data/recount3_catalog.rds` holds every recount3 study with its title and its
abstract. A script generates this file. Never edit it by hand.

```sh
Rscript data-raw/build_catalog.R
```

With a study explorer export in `data-raw/` the rebuild takes about two
minutes. Without one it makes about 19,000 requests and takes 20 to 40
minutes. The script can resume, so an interrupted run continues from that
point. [data/README.md](data/README.md) explains the schema and the flags.

Most of the time you do not need a rebuild at all.

## Report a bug

Open an issue. Include the R version, the operating system, and the output of
`sessionInfo()`. If a study is involved, give the accession. Study size is a
common cause of a failed load.
