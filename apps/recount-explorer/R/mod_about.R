# About view. Static, so there is no server half.
#
# It answers the questions a first-time visitor actually has: what this is,
# where the data comes from, what it costs to load a study, how to cite it, and
# who wrote it.

REPO_URL <- "https://github.com/samuelbharti/recount-explorer"

about_ui <- function() {
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("About Recount Explorer"),
      card_body(
        markdown(
          "
Recount Explorer is a Shiny app for reading the
[recount3](https://rna.recount.bio/) collection of RNA-seq studies. recount3
holds hundreds of thousands of human and mouse samples, and one pipeline
processed all of them in the same way, so counts from different studies are
comparable.

Reading one of those studies normally means installing Bioconductor, learning
the recount3 API, and writing R. This app removes that step. Pick a study, look
at it, and take the data with you.

### What you can do

- **Browse** all 18,998 studies at once. One search box covers the accession,
  the title, and the abstract text, so a search for `glioblastoma` finds
  studies whose titles never mention it.
- **Overview** the samples: library sizes, detected genes, and the full sample
  metadata.
- **Genes**: plot the expression of any gene, split by any metadata column.
- **PCA** on the genes with the highest variance, coloured by metadata.
- **Export** the data, the figures, and a script that repeats your session.

### What a study costs

recount3 stores one file for each study, and there is no way to ask for less
of it. Downloads run at about 100 KB for each sample, and memory at about
30 bytes for each gene in each sample. The app shows both before you load
anything, and refuses studies over 500 samples so a shared server stays up.
`RECOUNT_EXPLORER_MAX_SAMPLES` moves that limit.

Most studies are small. Of the 18,998 in the catalog, 18,323 are under 10 MB.
"
        )
      )
    ),
    tagList(
      card(
        card_header("How to cite"),
        card_body(
          markdown(
            "
Cite the recount3 paper for the data:

> Wilks C, Zheng SC, Chen FY, et al. recount3: summaries and queries for
> large-scale RNA-seq expression and splicing. *Genome Biology* 22, 323 (2021).
> [doi:10.1186/s13059-021-02533-6](https://doi.org/10.1186/s13059-021-02533-6)

The Export view writes a script that carries this reference in its header,
along with the accession and the package versions you used.
"
          )
        )
      ),
      card(
        card_header("Author and licence"),
        card_body(
          markdown(
            "
Built by **Samuel Bharti**.

- [www.samuelbharti.com](https://www.samuelbharti.com)
- [ORCID 0000-0003-4190-7058](https://orcid.org/0000-0003-4190-7058)
- [github.com/samuelbharti](https://github.com/samuelbharti)

The app is MIT licensed. The
[source](https://github.com/samuelbharti/recount-explorer) is open, and
contributions are welcome. The recount3 project sets the terms for the data
itself.
"
          )
        )
      ),
      card(
        card_header("How it works"),
        card_body(
          markdown(
            "
The study catalog is a snapshot that ships with the app, so the list appears at
once and needs no network. Loading a study downloads it from the recount3
servers on a background process, which is why the rest of the app stays usable
while it runs.

Computation lives in a Shiny-free layer that is tested on its own.
"
          ),
          tags$p(
            class = "mb-0",
            tags$a(
              href = paste0(REPO_URL, "/blob/main/docs/design.md"),
              target = "_blank",
              rel = "noopener",
              "Read the design notes"
            )
          )
        )
      )
    )
  )
}
