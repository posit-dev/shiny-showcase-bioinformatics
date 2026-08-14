# Shared UI helpers: the mascot, the click-to-open info popover, the app footer,
# the content-page wrapper, and version/repo metadata. Kept here (auto-sourced by
# R/load_components.R) so every page and module shares one implementation and the
# app has a single, consistent chrome.

# GENESCOUT_VERSION and GENESCOUT_REPO_URL are defined once in R/http.R (sourced
# first, for the outbound User-Agent); the footer reuses them so both stay in sync.
genescout_version <- function() GENESCOUT_VERSION

# The mascot ("Scout") as an <img>, sized in px. Served from www/img/mascot.svg.
genescout_mascot <- function(
  size = 40,
  class = NULL,
  alt = "GeneScout mascot"
) {
  tags$img(
    src = "img/mascot.svg",
    class = trimws(paste("gs-mascot", class %||% "")),
    width = size,
    height = size,
    alt = alt
  )
}

# A small, click-to-open information popover: a round "i" icon whose body explains
# a control without a hover tooltip (which is easy to miss and awkward on touch).
# `...` is the popover body; keep it to a sentence or two. Uses the Bootstrap
# popover bundled with bslib, so it works app-wide with no extra dependency.
gs_info <- function(..., placement = "auto") {
  bslib::popover(
    tags$span(
      class = "gs-info",
      role = "button",
      tabindex = "0",
      `aria-label` = "More information",
      "i"
    ),
    ...,
    placement = placement
  )
}

# The GitHub mark as an inline SVG (no external asset), inheriting currentColor.
genescout_github_icon <- function(size = 18) {
  tags$svg(
    class = "gs-gh-icon",
    width = size,
    height = size,
    viewBox = "0 0 16 16",
    `aria-hidden` = "true",
    fill = "currentColor",
    tags$path(
      `fill-rule` = "evenodd",
      d = paste0(
        "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 ",
        "0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13",
        "-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66",
        ".07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15",
        "-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 ",
        "1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 ",
        "1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 ",
        "1.93-.01 2.2 0 .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8z"
      )
    )
  )
}

# A content-page wrapper that matches the Review workbench width, so the Docs and
# About pages align with home instead of the full-bleed Bootstrap container.
gs_page <- function(...) {
  div(class = "gs-page", ...)
}

# The system overview figure: the deterministic spine (input -> resolve -> enrich
# -> citation gate -> rank -> review) with the optional AI branch below it. Pure
# markup styled by www/css/genescout.css (design tokens, so it stays on-brand);
# used on the About page and reusable anywhere.
genescout_overview_figure <- function() {
  node <- function(k, t, d, class = NULL) {
    tags$div(
      class = trimws(paste("gs-node", class %||% "")),
      tags$div(class = "k", k),
      tags$div(class = "t", t),
      tags$div(class = "d", d)
    )
  }
  arrow <- function() tags$div(class = "gs-arrow", HTML("&rarr;"))
  tags$figure(
    class = "gs-figure",
    tags$figcaption(class = "gs-figure-title", "How GeneScout works"),
    tags$div(
      class = "gs-flow",
      tags$div(
        class = "gs-flow-row",
        node(
          "1 · input",
          "Candidate lists + context",
          "paste or upload; per-list weights; disease priors",
          "in"
        ),
        arrow(),
        node("2 · resolve", "Canonical IDs", "MyGene; de-duplicate"),
        arrow(),
        node(
          "3 · enrich",
          "Per-gene signals",
          "~8 public sources, in parallel"
        ),
        arrow(),
        node("4 · gate", "Citation gate", "drop any value with no source"),
        arrow(),
        node("5 · rank", "Composite → grade", "weighted mean; caveats & veto"),
        arrow(),
        node(
          "6 · output",
          "Ranked, cited review",
          "every claim traceable",
          "out"
        )
      ),
      tags$div(
        class = "gs-flow-branch",
        tags$div(class = "gs-branch-label", "then, optionally — your API key"),
        tags$div(
          class = "gs-flow-row",
          node("A", "Curate with AI", "compact to a shortlist", "ai"),
          arrow(),
          node(
            "B",
            "Specialists",
            "3 agents → verdict + next experiment",
            "ai"
          ),
          arrow(),
          node("C", "Grounded chat", "ask about this run", "ai")
        )
      )
    ),
    tags$p(
      class = "gs-figure-note",
      "The deterministic spine needs no API key, and every value is gated on a real",
      "source — no ungrounded claims. Research use only."
    )
  )
}

# The app footer: version, copyright, license, research-use note, and a GitHub
# link. Rendered once via navbarPage(footer = ...), so it sits under every tab.
genescout_footer <- function() {
  tags$footer(
    class = "gs-footer",
    div(
      class = "gs-footer-inner",
      div(
        class = "gs-footer-brand",
        genescout_mascot(size = 22, alt = ""),
        tags$b("GeneScout"),
        tags$span(class = "gs-footer-ver", paste0("v", genescout_version()))
      ),
      div(
        class = "gs-footer-meta",
        tags$span(HTML("&copy; 2026 Samuel Bharti")),
        tags$span(class = "gs-dot", "·"),
        tags$span("MIT License"),
        tags$span(class = "gs-dot", "·"),
        tags$span("Research use only")
      ),
      tags$a(
        class = "gs-footer-gh",
        href = GENESCOUT_REPO_URL,
        target = "_blank",
        rel = "noopener",
        `aria-label` = "GeneScout on GitHub",
        genescout_github_icon(),
        tags$span("GitHub")
      )
    )
  )
}
