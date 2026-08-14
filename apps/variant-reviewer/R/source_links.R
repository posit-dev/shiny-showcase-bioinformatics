# "View on source" links shown in each card header, so every card points to the
# authoritative external page for what it displays. The URL builders are pure and
# return NULL when the required identifier is missing.

# Small right-aligned external link for a card header (or NULL if no href).
vr_source_link <- function(href, label = "Source") {
  if (is_blank(href)) {
    return(NULL)
  }
  tags$a(
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    class = "small text-decoration-none text-nowrap",
    label,
    " ",
    icon("up-right-from-square")
  )
}

# Card header: title + a small refresh icon (left), the module's source-link
# slot `uiOutput(ns("source"))` (right). `ns` is the module's NS().
#
# The refresh button's input ID is always ns("refresh"); pair it with
# vr_retry_counter()/vr_card_refresh_observer() (below) in the module's server
# so a click re-runs this card's fetch -- most useful for retrying after an
# API error without re-running the whole search.
vr_card_header <- function(title, ns) {
  card_header(
    class = "d-flex justify-content-between align-items-center gap-2",
    tags$span(
      class = "d-flex align-items-center gap-2",
      tags$span(title),
      actionButton(
        ns("refresh"),
        label = NULL,
        icon = icon("rotate-right"),
        class = "btn-sm btn-link p-0 vr-card-refresh",
        title = "Retry this card",
        `aria-label` = "Retry this card"
      )
    ),
    uiOutput(ns("source"), inline = TRUE)
  )
}

# A counter a card's fetch reactive can depend on to be retried on demand.
# Call `$dep()` as the first line of the reactive to be retried; wire `$bump`
# to the header's refresh button with vr_card_refresh_observer(), or pass it
# up to whatever owns a shared reactive this card doesn't fetch itself (see
# gene_summary_server()/variant_summary_server()/gnomad_ancestry_server() for
# that case). Bumping does not clear the HTTP cache: a fetch that already
# succeeded stays cached (recomputing just returns it), and a fetch that
# failed was never cached in the first place, so bumping genuinely retries it.
vr_retry_counter <- function() {
  n <- reactiveVal(0)
  list(
    dep = function() n(),
    bump = function() n(isolate(n()) + 1)
  )
}

# Wire a card header's refresh button (input$refresh) to a retry counter's
# `bump`. One call per module, alongside vr_retry_counter().
vr_card_refresh_observer <- function(input, bump) {
  observeEvent(input$refresh, bump())
}

# --- Per-source URL builders --------------------------------------------------

src_ncbi_gene <- function(entrez) {
  if (is_blank(entrez)) {
    NULL
  } else {
    paste0("https://www.ncbi.nlm.nih.gov/gene/", entrez)
  }
}

src_dbsnp <- function(rsid) {
  if (is_blank(rsid)) {
    NULL
  } else {
    paste0("https://www.ncbi.nlm.nih.gov/snp/", rsid)
  }
}

src_gtex <- function(symbol) {
  if (is_blank(symbol)) {
    NULL
  } else {
    paste0("https://gtexportal.org/home/gene/", symbol)
  }
}

src_string <- function(symbol) {
  if (is_blank(symbol)) {
    return(NULL)
  }
  paste0(
    "https://string-db.org/cgi/network?identifiers=",
    utils::URLencode(symbol, reserved = TRUE),
    "&species=9606"
  )
}

src_opentargets_gene <- function(ensembl) {
  if (is_blank(ensembl)) {
    NULL
  } else {
    paste0("https://platform.opentargets.org/target/", ensembl)
  }
}

src_opentargets_drugs <- function(ensembl) {
  if (is_blank(ensembl)) {
    NULL
  } else {
    paste0("https://platform.opentargets.org/target/", ensembl, "/known_drugs")
  }
}

src_opentargets_pgx <- function(ensembl) {
  if (is_blank(ensembl)) {
    NULL
  } else {
    paste0(
      "https://platform.opentargets.org/target/",
      ensembl,
      "/pharmacogenomics"
    )
  }
}

src_gnomad_gene <- function(ensembl) {
  if (is_blank(ensembl)) {
    NULL
  } else {
    paste0(
      "https://gnomad.broadinstitute.org/gene/",
      ensembl,
      "?dataset=gnomad_r4"
    )
  }
}

src_gnomad_variant <- function(variant_id, dataset = "gnomad_r4") {
  if (is_blank(variant_id)) {
    NULL
  } else {
    paste0(
      "https://gnomad.broadinstitute.org/variant/",
      variant_id,
      "?dataset=",
      dataset
    )
  }
}

src_uniprot <- function(uniprot, section = NULL) {
  if (is_blank(uniprot)) {
    return(NULL)
  }
  paste0(
    "https://www.uniprot.org/uniprotkb/",
    uniprot,
    "/entry",
    if (!is.null(section)) paste0("#", section) else ""
  )
}

src_alphafold <- function(uniprot) {
  if (is_blank(uniprot)) {
    NULL
  } else {
    paste0("https://alphafold.ebi.ac.uk/entry/", uniprot)
  }
}

src_ensembl_variant <- function(rsid) {
  if (is_blank(rsid)) {
    NULL
  } else {
    paste0("https://www.ensembl.org/Homo_sapiens/Variation/Explore?v=", rsid)
  }
}

src_ensembl_gene <- function(ensembl_gene) {
  if (is_blank(ensembl_gene)) {
    NULL
  } else {
    paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=", ensembl_gene)
  }
}

src_clinvar_variation <- function(uid) {
  if (is_blank(uid)) {
    NULL
  } else {
    paste0("https://www.ncbi.nlm.nih.gov/clinvar/variation/", uid, "/")
  }
}

src_europepmc_search <- function(query) {
  if (is_blank(query)) {
    return(NULL)
  }
  paste0(
    "https://europepmc.org/search?query=",
    utils::URLencode(query, reserved = TRUE)
  )
}

src_monarch_gene <- function(hgnc) {
  if (is_blank(hgnc)) {
    return(NULL)
  }
  paste0("https://monarchinitiative.org/", monarch_hgnc_id(hgnc))
}
