# About page: what the app is, the annotations it pulls together, and where the
# data comes from. Static content built from bslib cards so it matches the
# dashboard's look.

# One row per annotation card on the dashboard: the label, its source, and a
# one-line description of what it contributes. Kept as data so the table below
# stays in step with the dashboard grid.
.about_annotations <- list(
  list(
    "Gene summary",
    "MyGene.info",
    "Gene name, function summary, and cross-referenced identifiers (Entrez, Ensembl, UniProt)."
  ),
  list(
    "Variant annotation",
    "MyVariant.info",
    "rsID, HGVS protein change, CADD deleteriousness score, and ClinVar significance."
  ),
  list(
    "In-silico predictions",
    "dbNSFP (MyVariant.info)",
    "REVEL, AlphaMissense, CADD, PolyPhen-2, SIFT, and MetaLR/SVM pathogenicity scores."
  ),
  list(
    "Protein context",
    "ProtVar (EMBL-EBI)",
    "Protein-level functional and structural context for the variant."
  ),
  list(
    "Protein domains & features",
    "UniProt (EBI Proteins)",
    "Domains, regions, and sites, flagging which one the variant residue falls in."
  ),
  list(
    "3D structure",
    "AlphaFold DB",
    "Predicted 3D structure with the variant residue highlighted (r3dmol viewer)."
  ),
  list(
    "Clinical significance",
    "ClinVar (NCBI)",
    "Curated clinical interpretations for the variant."
  ),
  list(
    "Population frequency",
    "gnomAD",
    "Allele frequencies across reference populations."
  ),
  list(
    "Gene constraint",
    "gnomAD",
    "pLI, LOEUF, and Z-scores for tolerance to loss-of-function and missense variation."
  ),
  list(
    "Variant consequences",
    "Ensembl VEP",
    "Predicted molecular consequences of the variant."
  ),
  list("Tissue expression", "GTEx", "Gene expression across human tissues."),
  list(
    "Protein interactions",
    "STRING",
    "Predicted and known interaction partners."
  ),
  list(
    "Disease associations",
    "Open Targets",
    "Gene-disease association evidence."
  ),
  list(
    "Phenotypes (HPO)",
    "Monarch Initiative",
    "Human Phenotype Ontology terms associated with the gene."
  ),
  list(
    "Known drugs",
    "Open Targets",
    "Drugs and clinical candidates targeting the gene, with clinical stage."
  ),
  list(
    "Pharmacogenomics",
    "Open Targets",
    "Variant and genotype effects on drug response, with evidence levels."
  ),
  list(
    "Literature",
    "Europe PMC",
    "Recent publications for the gene, refined by the variant when loaded."
  ),
  list(
    "External links",
    "derived",
    "Deep links to external resources for the gene and variant."
  ),
  list(
    "AI assistant",
    "BYOK (Gemini / OpenAI / Anthropic)",
    "Optional chat grounded in the gene and variant currently on screen."
  )
)

.about_annotation_row <- function(entry) {
  tags$tr(
    tags$td(tags$strong(entry[[1]])),
    tags$td(entry[[2]]),
    tags$td(class = "text-muted", entry[[3]])
  )
}

about_page <- tagList(
  layout_columns(
    col_widths = 12,
    card(
      card_header("Overview"),
      card_body(
        tags$p(
          "Variant Reviewer is a lightweight gene and variant interpretation ",
          "companion. Enter a gene symbol, a variant, or both (the box ",
          "suggests the gene's known pathogenic variants as you type). A ",
          "variant on its own resolves its gene, so the dashboard pulls ",
          "together, in one place, what the gene does, where it is expressed, ",
          "what it interacts with, and protein- and clinical-level context for ",
          "the variant."
        ),
        tags$p(
          class = "mb-0",
          "It is a thin, reactive front end over several public bioinformatics ",
          "APIs. The gene is resolved once and shared across every card, so ",
          "each source is queried only when needed, and successful responses ",
          "are cached in-process so repeated searches are instant."
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Scope"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            tags$h3("What it does", class = "h6"),
            tags$ul(
              class = "mb-0",
              tags$li(
                "Reviews one human gene, one variant, or both at a time."
              ),
              tags$li(
                "Aggregates public annotations, read-only, onto one dashboard."
              ),
              tags$li(
                "Links out to the primary sources for deeper investigation."
              )
            )
          ),
          div(
            tags$h3("What it doesn't", class = "h6"),
            tags$ul(
              class = "mb-0",
              tags$li("Batch or VCF-scale analysis, or variant calling."),
              tags$li("Non-human species."),
              tags$li(
                "Clinical diagnosis, treatment, or genetic-counseling advice."
              )
            )
          )
        ),
        tags$p(
          class = "text-muted small mb-0 mt-3",
          tags$strong("Assistant: "),
          "the chat helps interpret the gene or variant on screen and answers ",
          "general genomics questions within that scope. It is not a source of ",
          "medical, diagnostic, or treatment advice."
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Annotations available"),
      card_body(
        tags$table(
          class = "table table-sm align-middle mb-0",
          tags$thead(
            tags$tr(
              tags$th("Annotation"),
              tags$th("Source"),
              tags$th("What it shows")
            )
          ),
          tags$tbody(
            lapply(.about_annotations, .about_annotation_row)
          )
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Input validation"),
      card_body(
        tags$p(
          "Before any external API is queried, the gene symbol and variant you ",
          "enter are validated with the ",
          tags$a(
            href = "https://github.com/samuelbharti/biobouncer",
            target = "_blank",
            rel = "noopener",
            tags$strong("biobouncer")
          ),
          " package, using its offline ",
          tags$em("pattern"),
          " mode: a fast, reproducible, network-free grammar check. Gene ",
          "symbols are checked against the HGNC symbol grammar and rsIDs against ",
          "the dbSNP grammar; other variant forms (HGVS, protein shorthand such ",
          "as R175H) are passed through for the annotation API to resolve."
        ),
        tags$p(
          class = "mb-0",
          "Malformed input is rejected up front with an inline message, so no ",
          "lookup is ever fired on an obviously bad identifier. This applies ",
          "both to what you type and to what the assistant loads. It is a ",
          "format check, not an existence check: whether the identifier actually ",
          "exists is confirmed by the resolving services (MyGene, MyVariant)."
        )
      )
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Data sources & privacy"),
      card_body(
        tags$p(
          "All data sources are public and require no API key. Queries are ",
          "sent directly to the upstream providers listed above."
        ),
        tags$p(
          class = "mb-0",
          "The optional AI assistant is ",
          tags$strong("bring your own key (BYOK)"),
          ": your API key is held only in this session's server memory, is ",
          "never written to disk, and is redacted from any surfaced error. You ",
          "can also supply a key via an environment variable ",
          "(",
          tags$code("GEMINI_API_KEY"),
          ", ",
          tags$code("OPENAI_API_KEY"),
          ", or ",
          tags$code("ANTHROPIC_API_KEY"),
          ")."
        )
      )
    ),
    card(
      card_header("About this build"),
      card_body(
        vr_field("Version", app_version()),
        tags$p(
          class = "mb-0",
          "Built with Shiny and bslib. See the ",
          tags$a(
            href = "https://github.com/samuelbharti/variant-reviewer",
            target = "_blank",
            rel = "noopener",
            "project repository"
          ),
          " for source, documentation, and contribution guidelines."
        )
      )
    )
  )
)
