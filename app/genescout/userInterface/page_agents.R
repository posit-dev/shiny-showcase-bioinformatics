# AI Agents page: what "Curate with AI" and the "Specialist" agents do, and where
# they sit in the pipeline. Pure static documentation (no server logic). The flow
# diagram is CSS/HTML only (no JS deps) and theme-aware via Bootstrap CSS variables.

# --- Small builders for the flow diagram ------------------------------------
flow_stage <- function(..., accent = "spine") {
  div(class = paste("genescout-stage", paste0("genescout-", accent)), ...)
}
flow_arrow <- function(label = NULL) {
  div(
    class = "genescout-arrow",
    HTML("&#8595;"),
    if (!is.null(label)) tags$small(label)
  )
}
specialist_box <- function(title, sources) {
  div(
    class = "genescout-spec",
    tags$strong(title),
    tags$div(class = "text-muted", sources)
  )
}

agents_page <- gs_page(fluidPage(
  tags$style(HTML(
    "
    .genescout-flow { max-width: 820px; }
    .genescout-stage { border: 1px solid var(--bs-border-color);
      border-radius: .5rem; padding: .85rem 1.1rem; background: var(--bs-body-bg); }
    .genescout-stage h5 { margin: 0 0 .15rem 0; font-size: 1.05rem; }
    .genescout-spine { border-left: 5px solid var(--bs-primary); }
    .genescout-ai    { border-left: 5px solid var(--bs-info); }
    .genescout-arrow { text-align: center; color: var(--bs-secondary-color);
      font-size: 1.35rem; line-height: 1.15; margin: .25rem 0; }
    .genescout-arrow small { display: block; font-size: .75rem; }
    .genescout-spec-grid { display: grid; grid-template-columns: repeat(3, 1fr);
      gap: .5rem; margin-top: .5rem; }
    .genescout-spec { border: 1px solid var(--bs-border-color); border-radius: .4rem;
      padding: .5rem .6rem; background: var(--bs-tertiary-bg); font-size: .85rem; }
    @media (max-width: 640px) { .genescout-spec-grid { grid-template-columns: 1fr; } }
    "
  )),
  titlePanel("The AI agents"),
  hr(),
  div(
    class = "alert alert-info",
    role = "alert",
    tags$strong("Both agents are optional and grounded. "),
    "GeneScout's ranking is fully deterministic and runs with no AI at all. The two",
    "agents below only ",
    tags$em("read, filter, and summarize"),
    " evidence the pipeline already retrieved and cited - they never rank, never",
    "invent a gene, and never state a fact that is not in the shown evidence. You",
    "trigger each with a button on the Review tab (agent mode ",
    tags$code("final"),
    " or ",
    tags$code("both"),
    "), and each needs an LLM provider configured in ",
    tags$code("config.yml"),
    "."
  ),

  # --- The workflow diagram --------------------------------------------------
  tags$h4("Where the agents sit"),
  div(
    class = "genescout-flow mb-4",
    flow_stage(
      accent = "spine",
      tags$h5(
        "Deterministic spine",
        tags$span(
          class = "badge bg-primary ms-2",
          "always runs"
        )
      ),
      p(
        class = "small text-muted mb-1",
        "Grounded and reproducible - the source of truth. No AI required."
      ),
      tags$div(
        "Your genes  →  resolve to canonical ids (MyGene)  →  pull a signal",
        "from ~8 public sources  →  weighted composite rank  →  grade +",
        "caveats / veto"
      )
    ),
    flow_arrow("the ranked, fully-cited gene table"),
    flow_stage(
      accent = "ai",
      tags$h5(
        "1 · Curate with AI",
        tags$span(class = "badge bg-info ms-2", "optional · 1 LLM call")
      ),
      tags$div(
        "Rank shortlist (top ~3× your target)  →  the model filters to your",
        tags$strong(" target list size"),
        " (default 30)  →  include / exclude + one-line rationale + cited",
        "PMIDs / accessions per gene"
      )
    ),
    flow_arrow("the curated shortlist"),
    flow_stage(
      accent = "ai",
      tags$h5(
        "2 · Specialist agents",
        tags$span(
          class = "badge bg-info ms-2",
          "optional · top 10 candidates"
        )
      ),
      p(
        class = "small text-muted mb-1",
        "Three domain specialists each read ONLY their domains' grounded evidence:"
      ),
      div(
        class = "genescout-spec-grid",
        specialist_box(
          "Variant & genomic",
          "ClinVar significance · gnomAD constraint · somatic / cancer alteration"
        ),
        specialist_box(
          "Pathway & disease",
          "Open Targets · Reactome pathways · tissue expression (GTEx)"
        ),
        specialist_box(
          "Literature & translation",
          "Europe PMC · PubTator · druggability"
        )
      ),
      flow_arrow("orchestrator synthesis"),
      tags$div(
        class = "text-center",
        tags$strong("One per-gene verdict"),
        ": integrated read, research plausibility",
        tags$span(
          class = "text-muted",
          " (compelling / plausible / uncertain / weak)"
        ),
        ", key caveats, and the single top next experiment"
      )
    )
  ),

  # --- Detail cards ----------------------------------------------------------
  fluidRow(
    column(
      width = 6,
      div(
        class = "card mb-3",
        div(
          class = "card-body",
          tags$h4(class = "card-title", "1 · Curate with AI"),
          p(
            tags$strong("What it is: "),
            "a final compaction step. After the deterministic ranking, it filters",
            "the list down to a usable, defensible set of about the size you choose",
            "- so a 500-gene run becomes a focused shortlist."
          ),
          tags$p(class = "fw-semibold mb-1", "How it works"),
          tags$ol(
            tags$li(
              "Takes the top-ranked genes (a pool ~3× your target size), each with",
              "its grounded per-signal evidence."
            ),
            tags$li(
              "One call to the orchestrator model: for every gene an include /",
              "exclude decision, a confidence, and a one-sentence rationale that",
              "cites the evidence shown."
            ),
            tags$li(
              "Caps the kept set to your ",
              tags$em("Target list size"),
              " (the most-confident genes)."
            )
          ),
          tags$p(
            class = "fw-semibold mb-1",
            "Guardrails (why you can trust it)"
          ),
          tags$ul(
            tags$li(
              "It can only choose from the ranked candidates - a gene it did not",
              "see is structurally dropped, so it cannot introduce one."
            ),
            tags$li(
              "Every citation is filtered to that gene's real evidence ids - a",
              "fabricated PMID or accession is removed."
            ),
            tags$li(
              "With no LLM available it falls back to the top genes by rank (same",
              "citations), so the app always works."
            )
          ),
          p(
            class = "small text-muted mb-0",
            tags$strong("Output: "),
            "the “AI-curated selection” card (+ CSV) - each gene with its",
            "confidence, rationale, and grounded sources."
          )
        )
      )
    ),
    column(
      width = 6,
      div(
        class = "card mb-3",
        div(
          class = "card-body",
          tags$h4(class = "card-title", "2 · Specialist agents"),
          p(
            tags$strong("What they are: "),
            "a deeper, per-gene read of the evidence for the top 10 candidates",
            "- of your curated list if you curated first, otherwise the top ranked",
            "genes."
          ),
          tags$p(class = "fw-semibold mb-1", "How they work"),
          tags$ol(
            tags$li(
              "Three specialists run in parallel; each reads ",
              tags$em("only"),
              " its own domains' grounded evidence for the gene and returns a short",
              "assessment, specific findings (each citing evidence ids), a strength",
              "(strong / moderate / weak / none), and a suggested next experiment."
            ),
            tags$li(
              "An orchestrator then synthesizes the three into one verdict per gene:",
              "an integrated read, a research plausibility level, the key",
              "cross-domain caveats, and the single highest-priority next experiment."
            )
          ),
          tags$p(
            class = "fw-semibold mb-1",
            "Guardrails (why you can trust it)"
          ),
          tags$ul(
            tags$li(
              "They fetch nothing new - they interpret evidence the deterministic",
              "pipeline already retrieved and cited."
            ),
            tags$li(
              "A finding whose citation is not in the gene's evidence is dropped;",
              "the verdict's citations chain through the specialists."
            )
          ),
          p(
            class = "small text-muted mb-0",
            tags$strong("Output: "),
            "the per-gene verdict and specialist cards in the drill-down, plus a",
            "Plausibility column in the ranked table."
          )
        )
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      p(
        class = "small text-muted",
        tags$strong("Models: "),
        "each agent reads its provider + model from ",
        tags$code("config.yml"),
        " (orchestration and curation on the most capable model; the three",
        "specialists on a faster one) - switching providers is a config change,",
        "never a code change."
      ),
      div(
        class = "alert alert-warning",
        role = "alert",
        tags$strong("Research use only. "),
        "The plausibility read is a prioritization aid for researchers, never a",
        "diagnosis or an ACMG/AMP pathogenicity classification."
      )
    )
  )
))
