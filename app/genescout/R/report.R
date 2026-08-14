# Report rendering. Turns a review result into an auditable artifact: a ranked
# gene x signal table (with a legend explaining the composite), per-gene
# drill-down cards showing the evidence behind each signal, and the
# research-use-only disclaimer.
#
# The same renderers serve the Shiny app (results module) and the downloadable
# standalone HTML. Every signal value and evidence row links to its source.

GENESCOUT_DISCLAIMER <- paste(
  "Research use only. GeneScout is a hypothesis-prioritization aid for researchers.",
  "It is not a clinical decision-support tool and does not provide diagnosis,",
  "treatment guidance, or ACMG/AMP variant classification."
)

# Human-readable labels for the evidence domains, in display order.
GENESCOUT_DOMAIN_LABELS <- c(
  `pathway-disease` = "Pathway & disease",
  `gene-disease` = "Gene–disease association",
  cancer = "Cancer relevance",
  literature = "Literature",
  `variant-effect` = "Variant / ClinVar",
  constraint = "Constraint (gnomAD)",
  `population-frequency` = "Population frequency (gnomAD)",
  druggability = "Druggability",
  `function` = "Molecular function (GO)",
  structure = "Protein structure (PDBe)",
  `model-organism` = "Model-organism knockout (IMPC)",
  expression = "Tissue expression (GTEx)",
  interaction = "Protein interactions (STRING)",
  `input-provenance` = "Corroborating sources (your input)"
)

# --- Ranked gene x signal table ---------------------------------------------

# The per-gene synthesized verdicts (from run_synthesis) as a map keyed by UPPER
# symbol, so the plausibility/verdict can be surfaced beyond the drill-down (the
# ranked table, the report, the CSV). Empty list when specialists were not run or
# produced no verdict, so every caller degrades to its pre-specialist output.
specialist_verdicts <- function(specialists) {
  by_gene <- specialists$by_gene
  if (is.null(by_gene)) {
    return(list())
  }
  out <- list()
  for (sym in names(by_gene)) {
    v <- by_gene[[sym]]$verdict
    if (!is.null(v) && nzchar(v$verdict %||% "")) {
      out[[sym]] <- v
    }
  }
  out
}

# The plausibility label per gene for the ranked table, or an em dash when a gene
# has no synthesized verdict.
plausibility_column <- function(symbols, verdicts) {
  vapply(
    symbols,
    function(s) {
      v <- verdicts[[toupper(s)]]
      if (is.null(v) || is_blank(v$plausibility %||% "")) {
        "—"
      } else {
        as.character(v$plausibility)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# A display data frame for the ranked matrix: Rank, Gene, one column per signal
# (raw value), Composite, Grade. `registry` is the result$registry summary.
# `verdicts` (from specialist_verdicts) is optional: when present, a Plausibility
# column is appended; when NULL/empty the frame is byte-identical to before.
gene_matrix_display <- function(genes, registry, verdicts = NULL) {
  df <- data.frame(
    Rank = genes$rank,
    Gene = genes$symbol,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (i in seq_len(nrow(registry))) {
    df[[registry$label[i]]] <- format_signal_value(genes[[registry$key[i]]])
  }
  df[["Composite"]] <- round(genes$composite, 3)
  df[["Grade"]] <- genes$grade
  df[["Caveats"]] <- caveats_count_display(genes)
  if (length(verdicts) > 0) {
    df[["Plausibility"]] <- plausibility_column(genes$symbol, verdicts)
  }
  df
}

# Per-gene caveat summary for the ranked table: the number of caveats (the reasons
# themselves are in the per-gene drill-down), or an em dash when clean. Defensive:
# a gene matrix built before the caveats stage shows "—" for every row.
caveats_count_display <- function(genes) {
  if (!"caveats" %in% names(genes)) {
    return(rep("—", nrow(genes)))
  }
  vapply(
    seq_len(nrow(genes)),
    function(i) {
      k <- length(genes$caveats[[i]])
      if (k == 0) "—" else as.character(k)
    },
    character(1)
  )
}

# Format a raw signal column for display: integers as counts, fractions to 2 dp,
# missing as an em dash.
format_signal_value <- function(v) {
  ifelse(
    is.na(v),
    "—",
    ifelse(
      v == round(v),
      formatC(v, format = "d", big.mark = ","),
      sprintf("%.2f", v)
    )
  )
}

# The signal legend: what the composite means and each source's role/weight.
registry_legend_html <- function(registry) {
  items <- lapply(seq_len(nrow(registry)), function(i) {
    role <- registry$role[i] %||% "evidence"
    dir_note <- if (identical(registry$direction[i], "lower_better")) {
      " · lower is better"
    } else {
      ""
    }
    htmltools::tags$li(
      htmltools::strong(registry$label[i]),
      sprintf(
        " — %s (%s, weight %.2g%s)",
        registry$source[i],
        role,
        registry$weight[i],
        dir_note
      )
    )
  })
  htmltools::div(
    class = "small text-muted mb-3",
    htmltools::p(
      class = "mb-1",
      paste(
        "Composite = weighted mean of each source's normalized signal.",
        "Evidence sources reward breadth (a missing one counts as 0);",
        "annotation sources (constraint, druggability) only nudge the score up",
        "and never penalize a gene. Rank is by composite (higher first).",
        "The caveats stage then down-weights weak candidates and vetoes",
        "recurrent-artifact (FLAGS) genes to the bottom, with the reason on the",
        "gene."
      )
    ),
    htmltools::tags$ul(class = "mb-0", items)
  )
}

# A flat data frame of the ranked matrix for CSV export: rank, gene, ids, each
# signal's raw + normalized value, coverage, composite, grade, and the input
# list(s) each gene came from. `verdicts` (from specialist_verdicts) is optional:
# when present, the synthesized plausibility/verdict/next_experiment are appended
# so the specialist read survives the export; when NULL those columns are omitted.
build_export_csv <- function(result, verdicts = NULL) {
  genes <- result$genes
  reg <- result$registry
  df <- data.frame(
    rank = genes$rank,
    gene = genes$symbol,
    gene_id = genes$gene_id,
    resolved = genes$resolved,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (i in seq_len(nrow(reg))) {
    k <- reg$key[i]
    df[[k]] <- genes[[k]]
    df[[paste0(k, "_norm")]] <- round(genes[[paste0(k, "_n")]], 4)
  }
  df$n_sources_present <- genes$n_sources_present
  df$composite <- round(genes$composite, 4)
  df$grade <- genes$grade
  df$vetoed <- if ("vetoed" %in% names(genes)) {
    as.logical(genes$vetoed)
  } else {
    FALSE
  }
  df$caveats <- if ("caveats" %in% names(genes)) {
    vapply(genes$caveats, function(x) paste(x, collapse = " | "), character(1))
  } else {
    ""
  }
  df$input_lists <- vapply(
    genes$input_lists,
    function(x) paste(x, collapse = ";"),
    character(1)
  )
  if (length(verdicts) > 0) {
    field <- function(name) {
      vapply(
        toupper(genes$symbol),
        function(s) as.character(verdicts[[s]][[name]] %||% ""),
        character(1),
        USE.NAMES = FALSE
      )
    }
    df$plausibility <- field("plausibility")
    df$verdict <- field("verdict")
    df$next_experiment <- field("next_experiment")
  }
  df
}

# Ranked matrix as a static HTML table (for the download). Passes `verdicts`
# through so the report table can carry the Plausibility column too.
ranked_matrix_html <- function(genes, registry, verdicts = NULL) {
  disp <- gene_matrix_display(genes, registry, verdicts = verdicts)
  htmltools::tags$table(
    class = "table table-striped",
    htmltools::tags$thead(htmltools::tags$tr(
      lapply(names(disp), htmltools::tags$th)
    )),
    htmltools::tags$tbody(lapply(seq_len(nrow(disp)), function(i) {
      htmltools::tags$tr(lapply(names(disp), function(col) {
        htmltools::tags$td(as.character(disp[i, col]))
      }))
    }))
  )
}

# --- AI curation panel ------------------------------------------------------

# How many excluded genes to list in the "not curated" panel before collapsing the
# rest to a count (the full ranking is always in the table above).
GENESCOUT_NONCURATED_MAX <- 50L

# Render the grounded evidence ids a rationale cites as a compact cell (or an em
# dash when none survived grounding).
curation_ids_cell <- function(ids) {
  ids <- ids %||% character()
  if (length(ids) == 0) {
    return(htmltools::span(class = "text-muted", "—"))
  }
  htmltools::tags$span(
    class = "small font-monospace",
    paste(ids, collapse = ", ")
  )
}

# Render a curated selection (from curate_gene_list()) as a card: a banner
# stating whether the AI ran or the deterministic fallback was used, then the
# included genes with confidence, rationale, and the grounded source ids each
# rationale cites. When `ranked` (result$genes) is supplied, a collapsed panel
# lists the genes that did NOT make the curated list, with the reason. Pure
# htmltools (the download button is added by the module).
render_curation <- function(curated, ranked = NULL) {
  ai_used <- isTRUE(attr(curated, "ai_used"))
  note <- attr(curated, "message") %||% attr(curated, "error")
  has_ids <- "source_ids" %in% names(curated)
  inc <- curated[which(curated$include), , drop = FALSE]

  banner <- htmltools::div(
    class = paste("alert py-2", if (ai_used) "alert-info" else "alert-warning"),
    htmltools::strong(
      if (ai_used) "AI-curated selection. " else "Composite-rank selection. "
    ),
    if (!is.null(note)) {
      note
    } else if (ai_used) {
      "The model filtered the ranked list to the set below."
    }
  )

  body <- if (nrow(inc) == 0) {
    htmltools::p(class = "text-muted fst-italic", "No genes selected.")
  } else {
    rows <- lapply(seq_len(nrow(inc)), function(i) {
      ids <- if (has_ids) inc$source_ids[[i]] else character()
      htmltools::tags$tr(
        htmltools::tags$td(inc$gene_symbol[i]),
        htmltools::tags$td(
          if (is.na(inc$confidence[i])) {
            "—"
          } else {
            sprintf("%.2f", inc$confidence[i])
          }
        ),
        htmltools::tags$td(inc$rationale[i]),
        htmltools::tags$td(curation_ids_cell(ids))
      )
    })
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Gene"),
        htmltools::tags$th("Confidence"),
        htmltools::tags$th("Rationale"),
        htmltools::tags$th("Grounded sources")
      )),
      htmltools::tags$tbody(rows)
    )
  }

  # The gene SELECTION is structurally gated to the ranked candidates AND each
  # rationale's cited ids are grounded (kept only when they match the evidence
  # gathered for that gene - a fabricated citation is dropped). Say so, so the
  # "Grounded sources" column is understood as a validated trail back to the
  # ranked table + per-gene drill-down, not a free-text claim.
  caveat <- if (ai_used && nrow(inc) > 0) {
    htmltools::p(
      class = "small text-muted fst-italic mt-1",
      paste(
        "Each rationale is the model's summary of the evidence shown for that",
        "gene; the Grounded sources column lists the exact evidence ids it cites,",
        "kept only when they match the evidence gathered for that gene (an",
        "ungrounded citation is dropped). See the ranked table and per-gene",
        "drill-down for the full source-linked evidence."
      )
    )
  }

  htmltools::tagList(
    htmltools::tags$h4(
      class = "h5",
      sprintf("Curated list (%d genes)", nrow(inc))
    ),
    banner,
    body,
    caveat,
    render_noncurated(curated, ranked, toupper(inc$gene_symbol))
  )
}

# The genes that did NOT make the curated list: every ranked gene minus the
# included set, sorted by rank, with the reason (the model's exclusion rationale
# when it gave one, else its rank/grade). Rendered as a collapsed accordion so it
# never crowds the curated list but the drops stay transparent. NULL when there is
# nothing to show or no ranking was supplied.
render_noncurated <- function(curated, ranked, included_syms) {
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(NULL)
  }
  ex <- ranked[!toupper(ranked$symbol) %in% included_syms, , drop = FALSE]
  if (nrow(ex) == 0) {
    return(NULL)
  }
  if ("rank" %in% names(ex)) {
    ex <- ex[order(ex$rank), , drop = FALSE]
  }
  n_total <- nrow(ex)
  shown <- utils::head(ex, GENESCOUT_NONCURATED_MAX)

  # The model's own exclusion rationales, keyed by uppercased symbol.
  excl_rat <- character()
  if (
    !is.null(curated) && all(c("include", "gene_symbol") %in% names(curated))
  ) {
    dropped <- curated[
      !is.na(curated$include) & !curated$include,
      ,
      drop = FALSE
    ]
    if (nrow(dropped) > 0) {
      excl_rat <- stats::setNames(
        dropped$rationale,
        toupper(dropped$gene_symbol)
      )
    }
  }

  rows <- lapply(seq_len(nrow(shown)), function(i) {
    sym <- shown$symbol[i]
    r <- excl_rat[toupper(sym)]
    reason <- if (is.na(r)) "Not selected for the curated list." else unname(r)
    htmltools::tags$tr(
      htmltools::tags$td(sym),
      htmltools::tags$td(
        if ("rank" %in% names(shown)) as.character(shown$rank[i]) else ""
      ),
      htmltools::tags$td(
        if ("grade" %in% names(shown)) shown$grade[i] else ""
      ),
      htmltools::tags$td(reason)
    )
  })
  overflow <- if (n_total > nrow(shown)) {
    htmltools::p(
      class = "small text-muted fst-italic",
      sprintf(
        "+ %d more below in the ranked table above.",
        n_total - nrow(shown)
      )
    )
  }

  bslib::accordion(
    class = "mt-2",
    open = FALSE,
    bslib::accordion_panel(
      sprintf(
        "Not in the curated list (%d gene%s)",
        n_total,
        if (n_total == 1) "" else "s"
      ),
      htmltools::tags$table(
        class = "table table-sm",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th("Gene"),
          htmltools::tags$th("Rank"),
          htmltools::tags$th("Grade"),
          htmltools::tags$th("Reason")
        )),
        htmltools::tags$tbody(rows)
      ),
      overflow
    )
  )
}

# A flat data frame of the curated (included) genes for CSV export, with the
# grounded evidence ids (";"-joined) each rationale cites.
build_curated_csv <- function(curated) {
  inc <- curated[which(curated$include), , drop = FALSE]
  ids <- if ("source_ids" %in% names(inc)) {
    vapply(
      inc$source_ids,
      function(x) paste(x %||% character(), collapse = "; "),
      character(1)
    )
  } else {
    rep("", nrow(inc))
  }
  data.frame(
    gene = inc$gene_symbol,
    confidence = round(inc$confidence, 3),
    rationale = inc$rationale,
    source_ids = ids,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# --- Specialist analysis (optional LLM synthesis) ---------------------------

# Canonical display order for the specialists.
GENESCOUT_SPECIALIST_ORDER <- c(
  "variant-effect",
  "pathway-disease",
  "literature"
)

# A colored badge for a specialist's domain-support strength.
specialist_strength_badge <- function(strength) {
  cls <- switch(
    strength %||% "moderate",
    strong = "text-bg-success",
    moderate = "text-bg-primary",
    weak = "text-bg-warning",
    none = "text-bg-secondary",
    "text-bg-secondary"
  )
  htmltools::span(class = paste("badge", cls), strength %||% "moderate")
}

# A colored badge for the synthesized overall research plausibility.
plausibility_badge <- function(plausibility) {
  cls <- switch(
    plausibility %||% "uncertain",
    compelling = "text-bg-success",
    plausible = "text-bg-primary",
    uncertain = "text-bg-warning",
    weak = "text-bg-secondary",
    "text-bg-warning"
  )
  htmltools::span(
    class = paste("badge", cls),
    plausibility %||% "uncertain"
  )
}

# The synthesized per-gene verdict (from run_synthesis): the integrated read, its
# plausibility, cross-domain caveats, the priority next experiment, and the ids it
# leans on. NULL when the gene has no verdict. Pure htmltools.
render_verdict <- function(verdict) {
  if (is.null(verdict) || !nzchar(verdict$verdict %||% "")) {
    return(NULL)
  }
  caveats <- if (length(verdict$caveats) > 0) {
    htmltools::div(
      class = "small mt-1",
      htmltools::strong("Caveats: "),
      htmltools::tags$ul(
        class = "small mb-0",
        lapply(verdict$caveats, htmltools::tags$li)
      )
    )
  }
  priority <- if (nzchar(verdict$next_experiment %||% "")) {
    htmltools::div(
      class = "small mt-1 fst-italic",
      htmltools::strong("Priority next experiment: "),
      verdict$next_experiment
    )
  }
  ids <- if (length(verdict$source_ids) > 0) {
    htmltools::div(
      class = "small text-muted font-monospace mt-1",
      paste0("[", paste(verdict$source_ids, collapse = ", "), "]")
    )
  }
  htmltools::div(
    class = "alert alert-primary py-2 mb-2",
    htmltools::div(
      class = "d-flex align-items-center gap-2 mb-1",
      htmltools::strong("Verdict"),
      plausibility_badge(verdict$plausibility)
    ),
    htmltools::p(class = "mb-1", verdict$verdict),
    caveats,
    priority,
    ids
  )
}

# Render one gene's specialist analysis (from run_specialists()): a card per domain
# with the grounded assessment, the findings + their cited ids, the strength, and a
# suggested next experiment. NULL when this gene was not analyzed. Pure htmltools.
render_specialist_analysis <- function(gene_row, specialists) {
  if (is.null(specialists) || is.null(specialists$by_gene)) {
    return(NULL)
  }
  g <- specialists$by_gene[[toupper(gene_row$symbol)]]
  if (is.null(g) || length(g) == 0) {
    return(NULL)
  }
  ordered <- intersect(GENESCOUT_SPECIALIST_ORDER, names(g))
  cards <- lapply(ordered, function(k) {
    sp <- g[[k]]
    findings <- if (length(sp$findings) > 0) {
      htmltools::tags$ul(
        class = "small mb-1",
        lapply(sp$findings, function(f) {
          htmltools::tags$li(
            f$point,
            htmltools::span(
              class = "text-muted font-monospace ms-1 small",
              paste0("[", paste(f$source_ids, collapse = ", "), "]")
            )
          )
        })
      )
    }
    next_exp <- if (nzchar(sp$next_experiment %||% "")) {
      htmltools::div(
        class = "small mt-1 fst-italic",
        htmltools::strong("Suggested next experiment: "),
        sp$next_experiment
      )
    }
    htmltools::div(
      class = "mb-2 pb-2 border-bottom",
      htmltools::div(
        class = "d-flex align-items-center gap-2 mb-1",
        htmltools::strong(sp$label),
        specialist_strength_badge(sp$strength)
      ),
      if (nzchar(sp$assessment %||% "")) {
        htmltools::p(class = "small mb-1", sp$assessment)
      },
      findings,
      next_exp
    )
  })
  htmltools::div(
    class = "card mb-3",
    htmltools::div(
      class = "card-body",
      htmltools::tags$h5(class = "h6 mb-1", "Specialist analysis"),
      htmltools::p(
        class = "small text-muted fst-italic mb-2",
        paste(
          "Each specialist synthesizes ONLY this gene's grounded evidence in its",
          "domain; every cited id is validated against that evidence, and an",
          "ungrounded finding is dropped."
        )
      ),
      render_verdict(g$verdict),
      cards
    )
  )
}

# --- Per-gene drill-down ----------------------------------------------------

# One gene's evidence card: header (symbol, grade, composite, source lists) plus
# the grounded evidence grouped by domain. `gene_row` is a 1-row slice of
# result$genes; `evidence` is result$evidence (filtered here by gene_id).
render_gene_evidence <- function(gene_row, evidence) {
  ev <- evidence[evidence$gene_id == gene_row$gene_id, , drop = FALSE]
  lists <- gene_row$input_lists[[1]]
  htmltools::div(
    class = "card mb-3",
    htmltools::div(
      class = "card-body",
      htmltools::tags$h4(
        class = "h5",
        gene_row$symbol,
        htmltools::span(
          class = "badge text-bg-secondary ms-2",
          gene_row$grade
        ),
        htmltools::span(
          class = "text-muted ms-2 small",
          sprintf("composite %.3f", gene_row$composite)
        )
      ),
      if (!isTRUE(gene_row$resolved)) {
        htmltools::div(
          class = "alert alert-warning py-1 px-2",
          if (nrow(ev) > 0) {
            paste(
              "Gene symbol could not be resolved to a canonical id; only",
              "disease-seed (symbol-keyed) signals were gathered."
            )
          } else {
            "Gene symbol could not be resolved; no signals were gathered."
          }
        )
      },
      if (length(lists) > 0) {
        htmltools::p(
          class = "small text-muted",
          paste0("From list(s): ", paste(lists, collapse = ", "))
        )
      },
      caveats_block(gene_row),
      evidence_sections(ev)
    )
  )
}

# The caveats/veto banner for a gene's drill-down: a red alert when vetoed, an
# amber one for down-weighting caveats, listing each recorded reason. NULL when
# the gene is clean or predates the caveats stage.
caveats_block <- function(gene_row) {
  if (!"caveats" %in% names(gene_row)) {
    return(NULL)
  }
  reasons <- gene_row$caveats[[1]]
  if (length(reasons) == 0) {
    return(NULL)
  }
  vetoed <- "vetoed" %in% names(gene_row) && isTRUE(gene_row$vetoed[1])
  htmltools::div(
    class = paste(
      "alert py-2",
      if (vetoed) "alert-danger" else "alert-warning"
    ),
    htmltools::strong(if (vetoed) "Vetoed. " else "Caveats. "),
    htmltools::tags$ul(
      class = "mb-0",
      lapply(reasons, htmltools::tags$li)
    )
  )
}

# Grounded evidence grouped into per-domain sub-tables. Empty -> a muted note.
evidence_sections <- function(evidence) {
  if (is.null(evidence) || nrow(evidence) == 0) {
    return(htmltools::p(
      class = "text-muted fst-italic",
      "No grounded evidence for this gene."
    ))
  }
  domains <- intersect(names(GENESCOUT_DOMAIN_LABELS), unique(evidence$domain))
  htmltools::tagList(lapply(domains, function(d) {
    sub <- evidence[evidence$domain == d, , drop = FALSE]
    htmltools::tagList(
      htmltools::tags$h5(
        class = "h6 text-muted mt-3",
        GENESCOUT_DOMAIN_LABELS[[d]]
      ),
      evidence_domain_table(sub)
    )
  }))
}

# One domain's evidence rows as a table (finding, detail, source link).
evidence_domain_table <- function(sub) {
  rows <- lapply(seq_len(nrow(sub)), function(i) {
    # A blank URL (e.g. input-provenance rows, whose "source" is the user's own
    # list) renders as plain text rather than a dead link.
    source_cell <- if (is_blank(sub$source_url[i])) {
      htmltools::tags$td(sub$source_id[i])
    } else {
      htmltools::tags$td(htmltools::tags$a(
        href = sub$source_url[i],
        target = "_blank",
        sub$source_id[i]
      ))
    }
    htmltools::tags$tr(
      htmltools::tags$td(sub$title[i]),
      htmltools::tags$td(sub$detail[i]),
      source_cell
    )
  })
  htmltools::tags$table(
    class = "table table-sm",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Finding"),
      htmltools::tags$th("Detail"),
      htmltools::tags$th("Source")
    )),
    htmltools::tags$tbody(rows)
  )
}

# --- Input-agent proposal (for the confirm panel) ---------------------------

# A plain display data frame of the agent's per-token decisions (for a CLI/table).
proposal_display <- function(proposal) {
  t <- proposal$tokens
  if (is.null(t) || nrow(t) == 0) {
    return(data.frame())
  }
  data.frame(
    Source = t$source_label,
    Input = t$original,
    Symbol = ifelse(is.na(t$symbol), "", t$symbol),
    Action = t$action,
    Reason = t$reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# A compact table of the tokens the agent corrected / flagged / dropped.
proposal_changes_table <- function(changed) {
  rows <- lapply(seq_len(nrow(changed)), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(changed$action[i]),
      htmltools::tags$td(changed$original[i]),
      htmltools::tags$td(changed$symbol[i] %||% ""),
      htmltools::tags$td(changed$reason[i])
    )
  })
  htmltools::tags$table(
    class = "table table-sm",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Action"),
      htmltools::tags$th("Input"),
      htmltools::tags$th("Symbol"),
      htmltools::tags$th("Reason")
    )),
    htmltools::tags$tbody(rows)
  )
}

# The proposal summary shown in the confirm panel: an AI/pass-through banner, the
# overall notes, any suggested disease context, and a table of the changes. Pure
# htmltools (no output bindings), so it drops straight into a modal.
render_proposal_summary <- function(proposal) {
  ai <- isTRUE(attr(proposal, "ai_used"))
  t <- proposal$tokens
  changed <- if (!is.null(t) && nrow(t) > 0) {
    t[t$action != "keep", , drop = FALSE]
  } else {
    t
  }
  disease <- proposal$proposed_disease
  htmltools::tagList(
    htmltools::div(
      class = if (ai) "alert alert-info py-2" else "alert alert-secondary py-2",
      if (ai) {
        "Reviewed by the input agent - edit anything below before ranking."
      } else {
        "AI unavailable - your input was passed through unchanged."
      }
    ),
    if (!is_blank(proposal$notes)) {
      htmltools::p(class = "text-muted", proposal$notes)
    },
    if (!is.null(disease) && !is_blank(disease$search_term)) {
      htmltools::p(
        htmltools::strong("Suggested disease context: "),
        sprintf(
          "%s - enter \"%s\" in the disease box to seed discovery.",
          if (is_blank(disease$name)) disease$search_term else disease$name,
          disease$search_term
        )
      )
    },
    if (!is.null(changed) && nrow(changed) > 0) {
      proposal_changes_table(changed)
    } else {
      htmltools::p(
        class = "text-muted fst-italic",
        "No corrections, flags, or drops - nothing to review."
      )
    }
  )
}

# --- Standalone HTML report -------------------------------------------------

# Write the full standalone HTML report for `result` to `file`. `specialists`
# (the optional run_specialists() output) adds the Plausibility column and a
# "Specialist synthesis" section, so the synthesized verdict survives the download;
# omitted, the report is exactly as before.
render_report <- function(result, file, specialists = NULL) {
  genes <- result$genes
  verdicts <- specialist_verdicts(specialists %||% list())
  doc <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("GeneScout gene-list review"),
      htmltools::tags$style(genescout_report_css())
    ),
    htmltools::tags$body(
      htmltools::div(
        class = "container",
        htmltools::tags$h1("GeneScout gene-list review"),
        if (!is_blank(result$description)) {
          htmltools::p(htmltools::strong("Studying: "), result$description)
        },
        htmltools::div(class = "disclaimer", GENESCOUT_DISCLAIMER),
        htmltools::tags$h2("Ranked genes"),
        registry_legend_html(result$registry),
        ranked_matrix_html(genes, result$registry, verdicts = verdicts),
        render_specialist_section(genes, verdicts),
        htmltools::tags$h2("Evidence by gene"),
        htmltools::tagList(lapply(seq_len(nrow(genes)), function(i) {
          render_gene_evidence(genes[i, , drop = FALSE], result$evidence)
        })),
        report_footer(result)
      )
    )
  )
  htmltools::save_html(doc, file = file)
  invisible(file)
}

# The "Specialist synthesis" report section: each ranked gene that has a verdict,
# in rank order, rendered via render_verdict(). NULL when no gene has one, so the
# section is absent unless specialists ran.
render_specialist_section <- function(genes, verdicts) {
  if (length(verdicts) == 0) {
    return(NULL)
  }
  syms <- toupper(genes$symbol)
  blocks <- lapply(seq_len(nrow(genes)), function(i) {
    v <- verdicts[[syms[i]]]
    if (is.null(v)) {
      return(NULL)
    }
    htmltools::div(
      class = "card",
      htmltools::div(
        class = "card-body",
        htmltools::tags$h4(class = "h5 mb-2", genes$symbol[i]),
        render_verdict(v)
      )
    )
  })
  blocks <- Filter(Negate(is.null), blocks)
  if (length(blocks) == 0) {
    return(NULL)
  }
  htmltools::tagList(
    htmltools::tags$h2("Specialist synthesis"),
    htmltools::p(
      class = "small text-muted",
      paste(
        "An orchestrator's integrated verdict per top candidate, from the",
        "grounded specialist analyses. Every cited id traces to that gene's",
        "evidence; research-use only."
      )
    ),
    blocks
  )
}

report_footer <- function(result) {
  sources <- vapply(result$provenance, function(s) s$source, character(1))
  htmltools::tags$footer(
    class = "text-muted mt-4",
    htmltools::tags$hr(),
    htmltools::p(paste0("Sources: ", paste(sources, collapse = ", "), ".")),
    htmltools::p(paste0(
      "Subjective AI ranking applied: ",
      if (isTRUE(result$generated_with_llm)) "yes" else "no",
      "."
    ))
  )
}

genescout_report_css <- function() {
  paste(
    ".container { max-width: 960px; margin: 2rem auto;",
    "  font-family: system-ui, sans-serif; padding: 0 1rem; }",
    ".disclaimer, .alert { background: #fff3cd; border: 1px solid #ffe69c;",
    "  border-radius: .375rem; }",
    ".disclaimer { padding: .75rem 1rem; margin: 1rem 0; }",
    ".alert { padding: .5rem .75rem; margin: .5rem 0; }",
    "table { border-collapse: collapse; width: 100%; margin: .5rem 0 1.5rem; }",
    "th, td { text-align: left; padding: .35rem .5rem;",
    "  border-bottom: 1px solid #dee2e6; font-size: .95rem; }",
    ".badge { background: #5b6770; color: #fff; padding: .15rem .5rem;",
    "  border-radius: .375rem; font-size: .8rem; }",
    "h1 { margin-bottom: .25rem; } h2 { margin-top: 2rem; }",
    ".card { border: 1px solid #dee2e6; border-radius: .5rem; margin: 1rem 0; }",
    ".card-body { padding: 1rem 1.25rem; }",
    ".card h4, .card .h5 { margin-top: 0; } .text-muted { color: #6c757d; }",
    ".small { font-size: .85rem; }",
    sep = "\n"
  )
}
