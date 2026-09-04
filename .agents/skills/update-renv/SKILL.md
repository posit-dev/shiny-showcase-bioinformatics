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
   installed, and `renv::update()` has nothing to compare. This step installs
   about 60 packages and takes a few minutes.

2. **Update.**

   ```r
   renv::update()
   ```

   To move renv itself, use `renv::upgrade()`, and make it a separate commit.
   It rewrites `R/renv/activate.R`, which is a large diff with nothing to
   review in it, so it does not belong beside a package change.

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

4. **Snapshot, scoped.** Use the call above, not a bare `renv::snapshot()`.

5. **Read the diff before you commit.**

   ```bash
   git diff --stat R/renv.lock
   ```

   Version numbers are expected. A package appearing or disappearing is not,
   unless step 4 or a code change explains it. Name the reason in the commit
   message.

## Notes

The lockfile points at `https://packagemanager.posit.co/cran/latest`, with no
snapshot date. Two people restoring months apart do not get the same library,
and that is the reason to update deliberately and record the result, rather
than trust that a restore gives what the lockfile describes.

The lockfile's `R.Version` field records whoever last ran `snapshot()`. Let
`snapshot()` write it and mention the change in the commit message if it moves.
Editing that field by hand records a claim that no library on any machine
supports.

`R/renv/library` is gitignored, along with the rest of `R/renv` except
`activate.R` and `settings.json`. Only the lockfile is a reviewable artifact.

The lockfile currently records 57 packages, and no code in `R/` reaches all of
them. `googledrive` is the clearest case: nothing in the repository names it,
and it brings `gargle`, `readr`, `vroom` and their dependencies with it. An
earlier version of the tooling used them. The scoped snapshot in step 4 removes
that tail, which is correct, but it is a separate decision from a version
update. Do one or the other in a commit, and say which.
