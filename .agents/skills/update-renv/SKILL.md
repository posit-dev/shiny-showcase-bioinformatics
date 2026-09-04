---
name: update-renv
description: Update the packages of the gallery's own renv library, in R/renv.lock. Use when R/check.R or R/capture.R fails on a package version, when a package the tooling needs is absent from the lockfile, or when the lockfile is old enough to be worth refreshing. Not for apps/*/manifest.json, which rsconnect writes in each application's own repository.
---

# Update the renv library

`R/renv.lock` records the packages that this repository's own tooling needs:
`R/check.R`, which tests `apps.yml` and `packages.yml`, and `R/capture.R`,
which takes the thumbnails. Nothing else reads it. No workflow restores it, so
a mistake here stops a person at their console and never a pull request.

**This is not `apps/*/manifest.json`.** Those files record what Connect Cloud
installs for one application, `rsconnect::writeManifest()` writes them in the
application's own repository, and renv has no part in them. If the task is a
package version for a deployed application, this is the wrong skill.

## The paths are not the default ones

`.Rprofile` sets `RENV_PATHS_RENV` to `R/renv` and `RENV_PATHS_LOCKFILE` to
`R/`, so the lockfile is `R/renv.lock` and the library is `R/renv/library`.
Operate R from the root of the repository and let `.Rprofile` do this. Never
pass a lockfile path by hand: a command that names `renv.lock` writes a second
lockfile at the root, and nothing reads that one.

## The trap: `renv::snapshot()` with no arguments

`R/renv/settings.json` sets `snapshot.type` to `implicit`, so renv derives the
lockfile from the code it finds in the project. It finds `apps/` as well.
`renv::dependencies()` discovers 69 packages across this repository, and 62 of
them are reached only through `apps/`: `duckdb`, `SummarizedExperiment`,
`igvShiny`, `shinyreact`, and several that exist only in a Posit-internal
repository. A bare `renv::snapshot()` tries to record all 62 in the gallery's
lockfile.

Snapshot the packages that `R/` actually uses:

```r
renv::snapshot(packages = unique(renv::dependencies("R", quiet = TRUE)$Package))
```

That call gives renv the seven direct dependencies of `R/` — `chromote`, `fs`,
`here`, `jsonlite`, `renv`, `stringr`, `yaml` — and renv adds their recursive
dependencies itself.

`renv::status()` reports the same confusion, as a long list of packages that
are "used" and not "recorded". Those lines are `apps/`, and they are not a
problem to fix.

## Steps

1. **Restore before anything else.** In R, from the root:

   ```r
   renv::restore()
   ```

   Until the library exists, `renv::status()` says only that nothing is
   installed, and `renv::update()` has nothing to compare. A cold renv cache
   makes this step a few minutes; a warm one makes it seconds, because renv
   links from the cache instead of installing.

2. **Move the date, then update.** The lockfile names a dated Package Manager
   snapshot, `https://packagemanager.posit.co/cran/2026-09-04` or later. That
   repository is frozen, so `renv::update()` against it reports that every
   package is up to date, whatever CRAN has released since. Moving the date is
   the update.

   ```r
   l <- renv::lockfile_read("R/renv.lock")
   l <- renv::lockfile_modify(l, repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-11-20"))
   renv::lockfile_write(l, "R/renv.lock")
   renv::update()
   ```

   Use today's date, or an older one deliberately. `lockfile_modify()` is the
   way to write it: `options(repos =)` followed by a snapshot looks equivalent
   and is not, because renv resolves packages against the lockfile's
   repositories rather than `options(repos)`, and the snapshot then rewrites
   each package's `Repository` field from the name `CRAN` to the literal old
   URL. Restore survives that, but the records are false, and a URL there
   becomes binding under `package_repository_resolution: "strict"`.

   renv is itself in the lockfile, so `renv::update()` moves renv too, and
   moving renv rewrites `R/renv/activate.R`. Expect that file in the diff, and
   do not try to keep it out: a lockfile that records renv 1.2.4 beside a
   bootstrap script for 1.1.5 is the inconsistent state, not the clean one.
   `renv::upgrade()` is for moving renv on its own, without touching another
   package.

   Every R session between this step and step 4 prints that the loaded renv is
   not the recorded one. Step 4 ends it. The message is correct and there is
   nothing to fix.

3. **Test both consumers.** `R/check.R` is the easy one:

   ```r
   source("R/check.R")
   ```

   Then test the capture path, which `check.R` never touches:

   ```r
   source("R/capture.R")
   b <- open_app("https://posit-genescout.share.connect.posit.cloud/")
   b$close()
   ```

   A deployed application serves as the target, so this needs no local server.
   A window opens, which is the point: `chromote` and `websocket` drive a
   headless Chrome, and an update can break that stack while every other check
   still passes. The failure would otherwise appear the next time somebody
   captured a thumbnail, a long way from the commit that caused it.

4. **Snapshot, scoped.** Use the call at the top of this file, not a bare
   `renv::snapshot()`.

   That one call changes versions and drops packages at the same time, and the
   dropped ones make a diff of a thousand lines that hides the versions
   completely. Two calls separate them, and each one is a real snapshot rather
   than an edited file:

   ```r
   # First: the versions alone, keeping every package the lockfile already has.
   old <- names(jsonlite::fromJSON("R/renv.lock", simplifyVector = FALSE)$Packages)
   renv::snapshot(packages = old)
   # Commit. Then the scoped call, whose diff is now removals alone.
   renv::snapshot(packages = unique(renv::dependencies("R", quiet = TRUE)$Package))
   ```

5. **Read the diff before you commit.**

   ```bash
   git diff --stat R/renv.lock
   ```

   Version numbers are expected. A package appearing or disappearing is not,
   unless step 4 or a code change explains it. Name the reason in the commit
   message.

   If the scoped call removed packages, test the removal instead of reasoning
   about it. Restore the new lockfile into an empty library outside the
   project, and run both consumers against it:

   ```bash
   RENV_PATHS_LIBRARY=/tmp/renv-fresh-lib R -q -e 'renv::restore(prompt = FALSE); source("R/check.R")'
   ```

   That is what a person cloning the repository gets. A library that still has
   the removed packages installed cannot fail this way, so testing in place
   proves nothing.

## Notes

A record's `Repository` field is the *name* that `options("repos")` carried when
that package was installed, which `install.packages()` writes into the installed
DESCRIPTION and renv copies out. It is not in the source tarball. So a record
saying `RSPM` is a fossil of another machine's configuration, and three of them
are in this lockfile. renv passes the field as `prefer` to
`renv_available_packages_entry()`, a tie-break between repositories that both
carry the package, so a name matching nothing costs nothing. Leave those
records alone; reinstalling a working package to correct a field that decides
nothing is not worth the diff.

The lockfile's `R.Version` field records whoever last ran `snapshot()`. Let
`snapshot()` write it and mention the change in the commit message if it moves.
Editing that field by hand records a claim that no library on any machine
supports.

`R/renv/library` is gitignored, along with the rest of `R/renv` except
`activate.R` and `settings.json`. Only the lockfile is a reviewable artifact.

The seven direct dependencies of `R/` need 29 packages with their recursive
dependencies. A lockfile materially larger than that has a tail from an earlier
version of the tooling, and the scoped call in step 4 removes it. That happened
once already: a lockfile of 57 packages carried `googledrive`, `readr`, `dplyr`
and `tidyr`, which nothing in the repository names, and those four brought 24
more with them.
