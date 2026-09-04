# Security policy

## Report a vulnerability

Send the details to <samuelbharti.io@gmail.com>. Do not open a public issue for
a security problem. You get an acknowledgement within one week.

## Scope

Recount Explorer is a Shiny app that reads public data. This is what the app
does and what it does not do:

- The app makes outbound HTTPS requests to the recount3 data servers
  (`duffel.rail.bio`) and to Bioconductor. It contacts no other host.
- The app has no accounts, no authentication, and no database.
- The app stores two things on disk. The first is the recount3 downloads that
  BiocFileCache manages. The second is a catalog snapshot under
  `tools::R_user_dir()`.
- Study titles and abstracts are third party text from SRA. The app escapes
  this text before it renders it. A report of a path that does not escape this
  text is in scope.

CAUTION: A large study allocates a lot of memory in the server process. The app
refuses any study over 500 samples by default, which is about 1 GB of peak
memory and allows 98.7 percent of the catalog.

`RECOUNT_EXPLORER_MAX_SAMPLES` moves that limit. Raise it only on a host with
memory to spare. The largest study in the catalog has 28,706 samples. It needs
roughly 56 GB. Measured: a 100-sample study occupies 194 MB, which is
30.4 bytes for each gene in each sample.

The limit is enforced on the server, not only in the interface. Hiding the
button stops nobody. Anything can send the input.

## Supported versions

The `main` branch is the supported version.
