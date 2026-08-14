# About page: what GeneScout is, who builds it, and the research-use-only
# disclaimer. Wrapped in gs_page() so it shares the Review workbench's width.
about_page <- gs_page(
  div(
    class = "gs-about-head",
    style = "display:flex;align-items:center;gap:16px;",
    genescout_mascot(56, alt = ""),
    titlePanel("About GeneScout")
  ),
  hr(),
  genescout_overview_figure(),
  fluidRow(
    column(
      width = 8,
      p(
        "GeneScout investigates one or more candidate gene lists and prioritizes",
        "them for any disease or phenotype. It pulls a signal from each of several",
        "public sources,",
        "merges and de-duplicates them, and ranks the genes by a transparent",
        "composite score - every value traceable to its source."
      ),
      p(
        "Bring your own gene list from an analysis, or start from a disease and",
        "let GeneScout discover candidate genes for it (Open Targets, PanelApp,",
        "DISEASES) - or do both and merge. With a disease context the ranking",
        "becomes disease-specific rather than measuring a gene's general",
        "prominence."
      ),
      p(
        "The deterministic ranking needs no API key. To add the optional AI stages",
        "- input/final curation, the specialist verdicts, and the grounded Chat",
        "assistant - paste your own Anthropic, Google (Gemini) or OpenAI key in the",
        tags$strong("AI provider (your key)"),
        "card on the Review tab. Your key stays in this browser session only: it is",
        "never stored, logged, or sent anywhere but the provider you pick."
      ),
      div(
        class = "alert alert-warning",
        role = "alert",
        tags$strong("Research use only. "),
        "GeneScout is a hypothesis-prioritization aid for researchers. It is not a",
        "clinical decision-support tool and does not provide diagnosis,",
        "treatment guidance, or ACMG/AMP variant classification."
      )
    ),
    column(
      width = 4,
      tags$h4("How it works"),
      tags$ol(
        tags$li("Resolve every gene to a canonical id and de-duplicate."),
        tags$li(
          "Pull a per-source signal: Open Targets association, Europe PMC",
          "mentions, ClinVar pathogenic variants."
        ),
        tags$li(
          "Gate every value on a source, then rank by a weighted composite."
        ),
        tags$li("Drill into any gene to see the evidence behind each signal.")
      ),
      tags$h4(class = "mt-4", "Developer"),
      p(
        class = "mb-1",
        "Built and maintained by Samuel Bharti,",
        tags$a(
          href = "https://github.com/samuelbharti",
          target = "_blank",
          rel = "noopener",
          "@samuelbharti"
        ),
        "(University of Alabama at Birmingham)."
      ),
      p(
        class = "small",
        tags$a(
          href = GENESCOUT_REPO_URL,
          target = "_blank",
          rel = "noopener",
          "Source, issues, and docs on GitHub"
        ),
        HTML(" &middot; "),
        "MIT License."
      )
    )
  )
)
