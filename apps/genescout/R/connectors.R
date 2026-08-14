# The Connectors reference page: a human-facing catalog of every data source
# GeneScout can pull a signal from, grouped by evidence domain, with each source's
# provider, role, selection status, and a one-line description. It reads the SAME
# catalog the engine and the source picker use (genescout_source_catalog), so a source
# added to the registry shows up here automatically - the page can never silently
# omit a connector. Availability reflects the live environment (a key-gated source
# with no key reads "needs a key"). No engine logic here: presentation over the
# catalog metadata.

# One-line human descriptions + a homepage link per connector, keyed by signal key.
# This is the only page-specific content; a key without an entry still renders (from
# its catalog label/source), so the page degrades gracefully when a new connector
# lands before its blurb does.
genescout_connector_notes <- function() {
  list(
    ot_assoc = list(
      desc = "Overall gene-disease association score (0-1) integrating genetics, somatic, known drugs, and text-mining.",
      url = "https://platform.opentargets.org"
    ),
    pmc_hits = list(
      desc = "Count of Europe PMC articles mentioning the gene - broad literature recall.",
      url = "https://europepmc.org"
    ),
    pubtator = list(
      desc = "Count of articles where PubTator3 entity-tagged the gene - precision literature evidence.",
      url = "https://www.ncbi.nlm.nih.gov/research/pubtator3/"
    ),
    clinvar_path = list(
      desc = "Count of pathogenic / likely-pathogenic ClinVar variants (research evidence, never a clinical call).",
      url = "https://www.ncbi.nlm.nih.gov/clinvar/"
    ),
    dgidb = list(
      desc = "Count of curated drug-gene interactions - a druggability measure.",
      url = "https://dgidb.org"
    ),
    gnomad_loeuf = list(
      desc = "gnomAD LOEUF loss-of-function constraint (lower = more constrained, so more likely intolerant).",
      url = "https://gnomad.broadinstitute.org"
    ),
    pharos_tdl = list(
      desc = "Pharos / IDG target development level (Tclin -> Tdark) mapped to a 0-1 druggability score.",
      url = "https://pharos.nih.gov"
    ),
    reactome = list(
      desc = "Membership in disease-associated or study-context-matching Reactome pathways.",
      url = "https://reactome.org"
    ),
    hpo = list(
      desc = "Mendelian / phenotype diseases the Human Phenotype Ontology links to the gene, scoped to the study context.",
      url = "https://hpo.jax.org"
    ),
    hpa = list(
      desc = "Human Protein Atlas curated disease / cancer classifications (e.g. tumor suppressor, cancer-related).",
      url = "https://www.proteinatlas.org"
    ),
    cbioportal = list(
      desc = "Cross-cancer somatic mutation frequency in a large pan-cancer cohort (MSK-IMPACT).",
      url = "https://www.cbioportal.org"
    ),
    civic = list(
      desc = "Count of expert-curated CIViC clinical evidence items for the gene (cancer, CC0).",
      url = "https://civicdb.org"
    ),
    clingen = list(
      desc = "ClinGen expert gene-disease validity classification (Definitive -> Limited), scoped to the study disease.",
      url = "https://clinicalgenome.org"
    ),
    uniprot_disease = list(
      desc = "UniProt / Swiss-Prot curated disease involvement, scoped to the study context - independent expert curation.",
      url = "https://www.uniprot.org"
    ),
    go = list(
      desc = "Gene Ontology biological-process terms, matched to the study's pathway biology (e.g. RAS/MAPK).",
      url = "https://www.ebi.ac.uk/QuickGO/"
    ),
    pdbe = list(
      desc = "Count of experimentally solved 3D structures - structural tractability for mechanistic follow-up.",
      url = "https://www.ebi.ac.uk/pdbe/"
    ),
    impc = list(
      desc = "Distinct significant phenotypes from a mouse knockout of the gene's ortholog - hypothesis-free in-vivo functional genetics, not a study-popularity proxy.",
      url = "https://www.mousephenotype.org"
    ),
    panelapp = list(
      desc = "Genomics England diagnostic-panel confidence for the disease (green = 1.0, amber = 0.5).",
      url = "https://panelapp.genomicsengland.co.uk"
    ),
    diseases = list(
      desc = "DISEASES (Jensen Lab) knowledge + text-mining gene-disease association score.",
      url = "https://diseases.jensenlab.org"
    ),
    cross_source = list(
      desc = "How many of your OWN input sources list the gene - corroboration breadth, no external call.",
      url = ""
    ),
    gtex_tissue = list(
      desc = "GTEx expression in your tissue(s) of interest vs the gene's peak across tissues - a relevance score.",
      url = "https://gtexportal.org"
    ),
    string = list(
      desc = "Within-list STRING connectivity: how many OTHER candidates the gene interacts with at high confidence.",
      url = "https://string-db.org"
    ),
    oncokb = list(
      desc = "OncoKB oncogenicity / therapeutic actionability. Requires an API key / license.",
      url = "https://www.oncokb.org"
    ),
    cosmic_cgc = list(
      desc = "COSMIC Cancer Gene Census driver classification. Requires a key / license.",
      url = "https://cancer.sanger.ac.uk/cosmic"
    ),
    disgenet = list(
      desc = "DisGeNET aggregated gene-disease associations. Requires an API key.",
      url = "https://www.disgenet.org"
    ),
    omim = list(
      desc = "OMIM catalog of Mendelian gene-disease relationships. Requires an API key.",
      url = "https://www.omim.org"
    ),
    drugbank = list(
      desc = "DrugBank drug-target relationships. Requires a license / API key.",
      url = "https://go.drugbank.com"
    )
  )
}

# The selection status of a catalog source, as a label + a Bootstrap badge class.
#   Planned      - a catalog stub (no live client yet; usually key-gated).
#   Needs a key  - a key-gated source whose key is absent from the environment.
#   Automatic    - an auto-signal that appends from the run's shape, not a checkbox
#                  (cross-source needs >=2 of your sources; STRING needs >=5 genes).
#   Contextual   - runs only when the review supplies its context (GTEx: tissues).
#   Default on   - queried when a review selects nothing.
#   Opt-in       - available but off unless a review selects it.
connector_status <- function(s) {
  if (isTRUE(s$stub)) {
    return(list(label = "Planned", cls = "text-bg-secondary"))
  }
  if (!signal_available(s)) {
    return(list(label = "Needs a key", cls = "text-bg-warning"))
  }
  if (identical(s$needs, "input") || identical(s$needs, "network")) {
    return(list(label = "Automatic", cls = "text-bg-info"))
  }
  if (identical(s$key, "gtex_tissue")) {
    return(list(label = "Contextual", cls = "text-bg-info"))
  }
  if (isTRUE(s$default_on %||% TRUE)) {
    return(list(label = "Default on", cls = "text-bg-success"))
  }
  list(label = "Opt-in", cls = "text-bg-primary")
}

# The catalog as a plain display data frame (one row per connector), joining the
# catalog metadata to the human notes. Pure + testable: it drives both the page and
# its test. `domain` is normalized to a known key so grouping stays stable.
genescout_connector_rows <- function(
  catalog = genescout_source_catalog(),
  notes = genescout_connector_notes()
) {
  known <- names(GENESCOUT_DOMAIN_LABELS)
  do.call(
    rbind,
    lapply(catalog, function(s) {
      st <- connector_status(s)
      note <- notes[[s$key]] %||% list(desc = "", url = "")
      dom <- s$domain %||% "other"
      data.frame(
        key = s$key,
        label = s$label,
        source = s$source,
        domain = if (dom %in% known) dom else "other",
        role = s$role %||% "evidence",
        status = st$label,
        status_cls = st$cls,
        description = note$desc %||% "",
        url = note$url %||% "",
        stringsAsFactors = FALSE
      )
    })
  )
}

# A small summary of the catalog for the page header (counts by status).
genescout_connector_summary <- function(rows = genescout_connector_rows()) {
  list(
    total = nrow(rows),
    domains = length(unique(rows$domain)),
    default_on = sum(rows$status == "Default on"),
    opt_in = sum(rows$status == "Opt-in"),
    planned = sum(rows$status %in% c("Planned", "Needs a key"))
  )
}

# --- Rendering --------------------------------------------------------------

# A status badge (pill) using the Bootstrap class from connector_status().
connector_status_badge <- function(label, cls) {
  tags$span(class = paste("badge rounded-pill", cls), label)
}

# The header stat cards: how big the catalog is and how it splits by status.
connector_summary_row <- function(sm) {
  stat <- function(n, label) {
    div(
      class = "col",
      div(
        class = "border rounded p-3 text-center h-100",
        div(class = "h3 mb-0", n),
        div(class = "text-muted small", label)
      )
    )
  }
  div(
    class = "row row-cols-2 row-cols-md-5 g-2 mb-4",
    stat(sm$total, "connectors"),
    stat(sm$domains, "evidence domains"),
    stat(sm$default_on, "default on"),
    stat(sm$opt_in, "opt-in"),
    stat(sm$planned, "planned / key-gated")
  )
}

# One domain's connectors as a table inside a titled card.
connector_domain_card <- function(domain, sub) {
  title <- GENESCOUT_DOMAIN_LABELS[[domain]] %||% "Other"
  body_rows <- lapply(seq_len(nrow(sub)), function(i) {
    r <- sub[i, , drop = FALSE]
    name_cell <- if (nzchar(r$url)) {
      tags$a(href = r$url, target = "_blank", rel = "noopener", r$label)
    } else {
      r$label
    }
    tags$tr(
      tags$td(
        tags$div(class = "fw-semibold", name_cell),
        tags$div(class = "text-muted small", r$source)
      ),
      tags$td(tags$span(class = "text-muted small", r$role)),
      tags$td(connector_status_badge(r$status, r$status_cls)),
      tags$td(class = "small", r$description)
    )
  })
  bslib::card(
    class = "mb-3",
    bslib::card_header(title),
    bslib::card_body(
      tags$table(
        class = "table table-sm align-middle mb-0",
        tags$thead(tags$tr(
          tags$th("Connector"),
          tags$th("Type"),
          tags$th("Status"),
          tags$th("What it contributes")
        )),
        tags$tbody(body_rows)
      )
    )
  )
}

# The whole Connectors page: intro, a status legend, summary stats, and one card per
# evidence domain (in the report's domain order), followed by the research-use note.
# Built from the live catalog so it always reflects the registered sources + keys.
render_connectors_page <- function(
  catalog = genescout_source_catalog(),
  rows = NULL
) {
  rows <- rows %||% genescout_connector_rows(catalog)
  sm <- genescout_connector_summary(rows)
  order <- names(GENESCOUT_DOMAIN_LABELS)
  present <- c(
    intersect(order, unique(rows$domain)),
    setdiff(unique(rows$domain), order)
  )
  cards <- lapply(present, function(d) {
    connector_domain_card(d, rows[rows$domain == d, , drop = FALSE])
  })
  fluidPage(
    titlePanel("Connectors"),
    tags$p(
      class = "text-muted",
      "Every data source GeneScout can pull a ranking signal from. A review activates",
      "a SELECTED subset - a deselected source is never queried (unlike a weight of",
      "0, which still pays the network cost). Every value a connector returns is",
      "grounded: it carries a real source id (an accession or citation), and a",
      "source that finds nothing says so rather than inventing evidence."
    ),
    div(
      class = "d-flex flex-wrap gap-3 mb-3 small",
      span(
        connector_status_badge("Default on", "text-bg-success"),
        " queried when you select nothing"
      ),
      span(
        connector_status_badge("Opt-in", "text-bg-primary"),
        " available; off unless selected"
      ),
      span(
        connector_status_badge("Automatic", "text-bg-info"),
        " appends from the run's shape"
      ),
      span(
        connector_status_badge("Contextual", "text-bg-info"),
        " runs when its context is supplied"
      ),
      span(
        connector_status_badge("Needs a key", "text-bg-warning"),
        " set an API key to enable"
      ),
      span(
        connector_status_badge("Planned", "text-bg-secondary"),
        " catalog-listed; client not wired yet"
      )
    ),
    connector_summary_row(sm),
    cards,
    div(
      class = "alert alert-warning mt-3",
      role = "alert",
      tags$strong("Research use only. "),
      "These sources are used as research evidence for hypothesis prioritization,",
      "not for diagnosis, treatment, or ACMG/AMP variant classification. The exact",
      "set queried in a run is recorded in that run's provenance."
    )
  )
}
