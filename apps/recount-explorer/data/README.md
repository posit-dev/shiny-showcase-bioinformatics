# data/

## recount3_catalog.rds

Every recount3 study with its title and its abstract. The app reads this file
at startup, so the catalog appears at once and needs no network.

**This file is generated. Never edit it by hand.**

| | |
| --- | --- |
| Studies | 18,998 |
| Human | 8,882 across sra, gtex, tcga, ANSWER_ALS, ega, LIBD, TARGET_ALS |
| Mouse | 10,116, all sra |
| With an abstract | 18,819 (99.1%) |
| With a recorded download size | 18,998 (100%) |
| Size | 4.4 MB, xz compressed |
| Load time | about 0.15 s |

`recount3_catalog_meta.json` holds the same provenance in readable form, so a
reviewer can see what changed in a pull request without opening R.

## Schema

The file holds one data frame. `attr(df, "catalog_meta")` carries the
provenance.

| Column | Type | Notes |
| --- | --- | --- |
| `uid` | character | `organism/file_source/project`. The stable key. |
| `project` | character | The accession, for example `SRP107565`. |
| `organism` | character | `human` or `mouse`. |
| `file_source` | character | `sra`, `gtex`, `tcga`, and four smaller sources. |
| `project_home` | character | Needed by `recount3::create_rse()`. |
| `n_samples` | integer | |
| `download_mb` | double | Size of the gene counts file, from a HEAD request. |
| `study_title` | character | Never empty. |
| `study_abstract` | character | Empty string when recount3 has none. |

`download_mb` is what lets the app say what a study costs before anyone clicks,
and lets you filter by size. The build records it with a HEAD request for each
study, so the bodies are never transferred. Across the catalog the counts come
to 50.3 GB, and 18,323 of the 18,998 studies are under 10 MB.

Two details are load bearing.

`uid` includes the organism because the same accession appears for both human
and mouse. The accession on its own is not a key.

`organism` and `file_source` are stored as character, not as factor.
`create_rse()` reaches `match.arg()` through `annotation_options()`, and
`match.arg()` rejects a factor. Storing them as factors would break study
loading. `catalog_proj_info()` in `R/logic_catalog.R` is the function that
hands a row to `create_rse()` safely.

## Rebuild

```sh
Rscript data-raw/build_catalog.R
```

The project list always comes from `available_projects()`. Titles and
abstracts come from up to three places, in this order:

1. A study explorer export in `data-raw/`, if one is present. The official
   [recount3 study explorer](https://jhubiostatistics.shinyapps.io/recount3-study-explorer/)
   exports the whole catalog as CSV. The export is gitignored because it is a
   20 MB intermediate.
2. Curated text for the six sources that publish no abstract. Only SRA studies
   carry `study_title` and `study_abstract` in their metadata files. See
   `data-raw/catalog_manual.R`.
3. A direct request to the recount3 metadata file, for whatever is left.

With an export in `data-raw/` a rebuild takes about two minutes. Without one it
makes about 19,000 requests and takes 20 to 40 minutes. The script writes each
finished chunk to `data-raw/parts/`, so an interrupted run resumes from that
point.

Useful flags:

```sh
Rscript data-raw/build_catalog.R --workers 8 --chunk 25
Rscript data-raw/build_catalog.R --limit 20     # small slice, for a smoke test
Rscript data-raw/build_catalog.R --fresh        # ignore the resume files
```

## Licensing

The study titles and abstracts come from the study metadata that the recount3
project publishes, which in turn comes from the source archives. See
[rna.recount.bio](https://rna.recount.bio/) for the recount3 terms, and cite
the recount3 paper. `CITATION.cff` in the repository root has the reference.
