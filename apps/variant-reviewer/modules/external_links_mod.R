# External links card. Builds deep links to common resources from the resolved
# gene identifiers.

external_links_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("External resources"),
    card_body(shinycssloaders::withSpinner(
      uiOutput(ns("content")),
      proxy.height = "80px"
    ))
  )
}

# resolved: reactive() -> mygene_resolve() result.
external_links_server <- function(id, resolved) {
  moduleServer(id, function(input, output, session) {
    output$content <- renderUI({
      res <- resolved()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(vr_empty("Search for a gene to see external links."))
      }
      links <- external_links_build(res)
      if (length(links) == 0) {
        return(vr_empty("No external links available for this gene."))
      }
      tags$div(
        class = "d-flex flex-wrap gap-2",
        lapply(names(links), function(label) {
          tags$a(
            class = "btn btn-outline-primary btn-sm d-inline-flex align-items-center gap-2",
            href = links[[label]],
            target = "_blank",
            rel = "noopener noreferrer",
            label,
            icon("up-right-from-square")
          )
        })
      )
    })
  })
}

# Pure helper: build a named list of label -> URL from a resolved gene result.
# Only includes a link when the identifier it needs is present.
external_links_build <- function(res) {
  links <- list()
  add <- function(label, url, id) {
    if (!is_blank(id)) {
      links[[label]] <<- url
    }
  }
  enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

  add(
    "GeneCards",
    paste0(
      "https://www.genecards.org/cgi-bin/carddisp.pl?gene=",
      enc(res$symbol)
    ),
    res$symbol
  )
  add(
    "Ensembl",
    paste0(
      "https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=",
      enc(res$ensembl_gene)
    ),
    res$ensembl_gene
  )
  add(
    "UniProt",
    paste0("https://www.uniprot.org/uniprotkb/", enc(res$uniprot), "/entry"),
    res$uniprot
  )
  add(
    "Open Targets",
    paste0("https://platform.opentargets.org/target/", enc(res$ensembl_gene)),
    res$ensembl_gene
  )
  add(
    "gnomAD",
    paste0(
      "https://gnomad.broadinstitute.org/gene/",
      enc(res$ensembl_gene),
      "?dataset=gnomad_r4"
    ),
    res$ensembl_gene
  )
  add(
    "ClinVar",
    paste0(
      "https://www.ncbi.nlm.nih.gov/clinvar/?term=",
      enc(res$symbol),
      "%5Bgene%5D"
    ),
    res$symbol
  )
  add(
    "STRING",
    paste0(
      "https://string-db.org/cgi/network?identifiers=",
      enc(res$symbol),
      "&species=9606"
    ),
    res$symbol
  )
  add(
    "GTEx",
    paste0("https://gtexportal.org/home/gene/", enc(res$symbol)),
    res$symbol
  )
  links
}
