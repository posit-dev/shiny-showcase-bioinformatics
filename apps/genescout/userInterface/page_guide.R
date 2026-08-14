# "Reading your results" page: a human guide to the ranked table - the grade
# vocabulary, the composite score, the caveats column, the plausibility column, and
# the per-gene evidence drill-down. Pure static documentation (no server logic),
# theme-aware via Bootstrap CSS variables (matches the AI Agents page).

# --- Small builders ---------------------------------------------------------
# One row of the grade legend: a badge + the grade name + what it means.
grade_row <- function(badge_class, label, threshold, meaning) {
  tags$tr(
    tags$td(
      class = "genescout-grade-cell",
      tags$span(class = paste("badge", badge_class), label)
    ),
    tags$td(class = "text-nowrap text-muted small", threshold),
    tags$td(meaning)
  )
}

guide_page <- gs_page(fluidPage(
  tags$style(HTML(
    "
    .genescout-guide { max-width: 920px; }
    .genescout-grade-table { width: 100%; border-collapse: collapse; }
    .genescout-grade-table td { padding: .5rem .6rem; vertical-align: top;
      border-top: 1px solid var(--bs-border-color); }
    .genescout-grade-cell { width: 1%; white-space: nowrap; }
    .genescout-col-card { border: 1px solid var(--bs-border-color);
      border-radius: .5rem; padding: .85rem 1.1rem; background: var(--bs-body-bg);
      height: 100%; }
    .genescout-col-card h5 { font-size: 1rem; margin: 0 0 .3rem 0; }
    .genescout-col-card code { font-size: .82rem; }
    "
  )),
  div(
    class = "genescout-guide",
    titlePanel("Reading your results"),
    hr(),
    p(
      class = "lead",
      "The ranked table is the deterministic output: genes sorted by a transparent",
      "composite score, every value traceable to the public source it came from.",
      "Select any row to open the evidence behind it. Here is what each column means."
    ),

    # --- Grade vocabulary ----------------------------------------------------
    tags$h4("The Grade column"),
    p(
      "Grade is a plain-language badge on the composite score (below). It is a",
      tags$em("relative research-prioritization signal"),
      "- not a probability, and never a clinical or pathogenicity call."
    ),
    tags$table(
      class = "genescout-grade-table mb-2",
      tags$tbody(
        grade_row(
          "bg-success",
          "High",
          "composite ≥ 0.50",
          "Strong, multi-source support in this context. A priority candidate to follow up."
        ),
        grade_row(
          "bg-primary",
          "Moderate",
          "0.20 to 0.50",
          "Real but partial support - worth a look, weaker or narrower than the High tier."
        ),
        grade_row(
          "bg-secondary",
          "Low",
          "< 0.20",
          "Little corroborating evidence in this context on the sources queried."
        ),
        grade_row(
          "bg-light text-dark border",
          "Insufficient",
          "no gradable signal",
          paste(
            "Nothing to grade on - not a negative result, just absence of evidence.",
            "A valid, first-class outcome; the engine never inflates a grade to avoid it."
          )
        ),
        grade_row(
          "bg-danger",
          "Vetoed",
          "forced to the bottom",
          paste(
            "The caveats stage overrode the score: the gene looked prominent but is a",
            "recurrent sequencing-artifact (FLAGS) gene. Sunk to last with a stated reason."
          )
        )
      )
    ),
    p(
      class = "text-muted small",
      "Ranking is always by the underlying composite, so two genes can share a grade",
      "badge yet still have a definite order."
    ),

    # --- The other columns ---------------------------------------------------
    tags$h4(class = "mt-4", "The other columns"),
    fluidRow(
      column(
        width = 6,
        div(
          class = "genescout-col-card mb-3",
          tags$h5("Composite"),
          p(
            class = "mb-1",
            "A weighted mean over the gene's evidence signals, each normalized to",
            "0-1 (a missing evidence signal counts as 0 but still divides, so breadth",
            "is rewarded); present annotations then only nudge the score up.",
            "This is the number the table is sorted by. Open the",
            tags$strong("“How the composite score works”"),
            "panel below the table to see every source, its role, and its weight -",
            "and use the weight sliders to re-rank instantly (no re-query)."
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "genescout-col-card mb-3",
          tags$h5("Caveats"),
          p(
            class = "mb-1",
            "The number of anti-bias flags raised for the gene (",
            tags$code("—"),
            "when clean). GeneScout down-ranks a candidate that looks compelling but is",
            "common in gnomAD, backed by a single weak source, or expressed only in",
            "unrelated tissue - and vetoes FLAGS genes outright. Select the row to",
            "read each caveat's reason."
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "genescout-col-card mb-3",
          tags$h5(
            "Plausibility",
            tags$span(class = "badge bg-info ms-2", "after specialists")
          ),
          p(
            class = "mb-1",
            "Appears only once you run the optional specialist agents. Their",
            "orchestrator's per-gene research-plausibility read:"
          ),
          tags$div(
            tags$span(class = "badge bg-success me-1", "compelling"),
            tags$span(class = "badge bg-primary me-1", "plausible"),
            tags$span(class = "badge bg-secondary me-1", "uncertain"),
            tags$span(class = "badge bg-light text-dark border", "weak")
          ),
          p(
            class = "text-muted small mt-2 mb-0",
            "A grounded interpretation of the same cited evidence - see the",
            tags$strong("AI Agents"),
            "tab. It never changes the deterministic rank."
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "genescout-col-card mb-3",
          tags$h5("Signal columns"),
          p(
            class = "mb-1",
            "Between Gene and Composite, one column per active source (Open Targets",
            "association, ClinVar variants, Europe PMC mentions, …) showing that",
            "gene's raw value, or",
            tags$code("—"),
            "when the source had nothing. These are the grounded inputs the",
            "composite is built from."
          )
        )
      )
    ),

    # --- The drill-down ------------------------------------------------------
    tags$h4(class = "mt-3", "The evidence drill-down"),
    p(
      "Select a gene row and the",
      tags$strong("Evidence"),
      "panel beside the table (below it on a narrow screen) opens its provenance:",
      "for every signal, the specific",
      "records behind it, each with a",
      tags$strong("source id"),
      "(a database accession or a PMID) linking out to the source. If you ran the",
      "specialists, their per-gene analysis and verdict appear here too. This is the",
      "auditable core - nothing in a GeneScout result is a claim you cannot trace to a",
      "source."
    ),
    div(
      class = "alert alert-info",
      role = "alert",
      tags$strong("Why you can trust it. "),
      "Every value is gated on a real source before it is shown - an ungrounded item",
      "is dropped, never guessed. For the full account, see",
      tags$code("docs/methodology.md"),
      "and the",
      tags$strong("scoring rubric"),
      "in",
      tags$code("prompts/scoring-rubric.md"),
      "."
    ),
    div(
      class = "alert alert-warning",
      role = "alert",
      tags$strong("Research use only. "),
      "GeneScout prioritizes hypotheses for researchers. It does not provide diagnosis,",
      "treatment guidance, or ACMG/AMP variant classification."
    )
  )
))
