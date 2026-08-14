# Review-workbench renderers (the "scientific-clean" master-detail layout).
#
# Pure functions: a slice of the run_review() result in, a shiny.tag out. They
# emit the custom `.gs-*` markup styled by www/css/genescout.css and wired by
# www/js/genescout.js, and are used by the results module. Kept here (not in the
# module) so they are small, testable, and separate from the reactive plumbing.
# The downloadable standalone report keeps its own renderers in R/report.R.

# Grade label -> the CSS modifier class on `.gs-grade`.
GS_GRADE_CLASS <- c(
  High = "high",
  Moderate = "mod",
  Low = "low",
  Insufficient = "ins",
  Vetoed = "veto"
)

# Evidence domain -> a short per-row tag for the detail pane (mirrors the domain
# labels in R/report.R but abbreviated for the compact evidence list).
GS_DOMAIN_TAG <- c(
  `pathway-disease` = "Pathway",
  `gene-disease` = "Gene–dis",
  cancer = "Cancer",
  literature = "Lit",
  `variant-effect` = "ClinVar",
  constraint = "Constraint",
  `population-frequency` = "gnomAD",
  druggability = "Drug",
  `function` = "GO",
  structure = "PDBe",
  `model-organism` = "IMPC",
  expression = "GTEx",
  interaction = "STRING",
  `input-provenance` = "Your input"
)

# Composite as a 2-dp monospace string, or an em dash span when NA.
gs_composite_cell <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(tags$span(class = "dash", "—"))
  }
  sprintf("%.2f", x)
}

gs_grade_class <- function(grade) {
  cls <- GS_GRADE_CLASS[[as.character(grade)]]
  if (is.null(cls)) "ins" else cls
}

# A grade pill (`.gs-grade`).
gs_grade_pill <- function(grade) {
  tags$span(
    class = paste("gs-grade", gs_grade_class(grade)),
    as.character(grade)
  )
}

# TRUE for a signal that only nudges the score (annotation role) — colored with
# the secondary accent so the primary evidence bars stay dominant.
gs_is_annotation <- function(role) {
  !identical(role %||% "evidence", "evidence")
}

# The evidence micro-bars for one gene: one bar per registry signal, height from
# the gene's normalized (`<key>_n`, higher-is-better) value; a flat stub for a
# missing/zero signal.
gs_signal_bars <- function(gene_row, registry) {
  bars <- lapply(seq_len(nrow(registry)), function(i) {
    key_n <- paste0(registry$key[i], "_n")
    v <- if (key_n %in% names(gene_row)) gene_row[[key_n]] else NA_real_
    if (is.null(v) || length(v) == 0 || is.na(v) || v <= 0) {
      return(tags$i(class = "empty"))
    }
    h <- max(3, round(v * 16))
    cls <- if (gs_is_annotation(registry$role[i])) "teal" else NULL
    tags$i(class = cls, style = sprintf("height:%dpx", h))
  })
  tags$span(class = "gs-sig", bars)
}

# The plausibility label + class for the table/detail, from a synthesized verdict
# (or NULL). Returns list(class, label); "na" when no verdict.
gs_plaus <- function(verdict) {
  p <- verdict$plausibility %||% ""
  if (is_blank(p) || identical(p, "—")) {
    return(list(class = "na", label = "—"))
  }
  label <- paste0(toupper(substring(p, 1, 1)), substring(p, 2))
  list(class = tolower(p), label = label)
}

gs_plaus_badge <- function(verdict) {
  pl <- gs_plaus(verdict)
  tags$span(class = paste("gs-plaus", pl$class), tags$i(), pl$label)
}

# ---- context strip ---------------------------------------------------------

# Order + colors for the grade distribution bar/legend.
GS_GRADE_ORDER <- c("High", "Moderate", "Low", "Insufficient", "Vetoed")
GS_GRADE_VAR <- c(
  High = "--g-high",
  Moderate = "--g-mod",
  Low = "--g-low",
  Insufficient = "--g-ins",
  Vetoed = "--g-veto"
)

# The collapsed-setup summary bar: the study-context pill, the run counts, and a
# grade-distribution bar with a legend. "Edit setup" reopens the setup section.
gs_context_strip <- function(result, setup_id = "gs-setup") {
  genes <- result$genes
  n <- nrow(genes)
  n_sources <- length(result$provenance %||% list())
  disease <- pluck_at(result, "context", "disease")
  priors <- pluck_at(result, "context", "priors")
  ctx_label <- disease$name %||%
    disease$id %||%
    priors$label %||%
    priors$id

  pill <- if (!is_blank(ctx_label)) {
    tags$span(class = "gs-pill", "◈ ", ctx_label)
  } else {
    tags$span(
      class = "gs-pill",
      style = "background:var(--inset);color:var(--muted)",
      "No study context"
    )
  }

  seed_total <- pluck_at(result, "context", "seed_capped", "total")
  seeded_note <- if (!is.null(seed_total)) {
    sprintf(" · from %s seeded", format(seed_total, big.mark = ","))
  } else {
    ""
  }
  counts <- tags$span(
    tags$b(format(n, big.mark = ",")),
    " ",
    tags$span(
      class = "muted",
      if (n == 1) "candidate ranked" else "candidates ranked"
    ),
    " ",
    tags$span(
      class = "faint",
      sprintf(
        "%s · %d source%s",
        seeded_note,
        n_sources,
        if (n_sources == 1) "" else "s"
      )
    )
  )

  counts_by_grade <- vapply(
    GS_GRADE_ORDER,
    function(g) sum(genes$grade == g, na.rm = TRUE),
    integer(1)
  )
  present <- counts_by_grade[counts_by_grade > 0]
  total <- max(sum(present), 1)
  bar_spans <- lapply(names(present), function(g) {
    tags$span(
      style = sprintf(
        "width:%.2f%%;background:var(%s)",
        100 * present[[g]] / total,
        GS_GRADE_VAR[[g]]
      )
    )
  })
  legend_items <- lapply(names(present), function(g) {
    tags$span(
      tags$i(style = sprintf("background:var(%s)", GS_GRADE_VAR[[g]])),
      sprintf("%s %d", g, present[[g]])
    )
  })

  tags$div(
    class = "gs-strip",
    tags$div(class = "lead", pill, counts),
    tags$div(
      class = "gs-dist",
      tags$div(class = "gs-distlegend", legend_items),
      tags$div(class = "gs-distbar", title = "grade distribution", bar_spans),
      tags$button(
        type = "button",
        class = "gs-ghost",
        `data-gs-setup` = "open",
        `data-gs-target` = setup_id,
        "✎ Edit setup"
      )
    )
  )
}

# ---- ranked table ----------------------------------------------------------

# The master table. `verdicts` (specialist_verdicts map, keyed by UPPER symbol)
# adds the Plausibility column when non-empty. `selected` marks the open row.
# `input_id` is the fully-namespaced Shiny input the row-click JS writes to.
gs_ranked_table <- function(
  genes,
  registry,
  verdicts = list(),
  selected = NULL,
  input_id = "selected_symbol"
) {
  show_plaus <- length(verdicts) > 0
  head_cells <- list(
    tags$th(class = "rt", "#"),
    tags$th("Gene"),
    tags$th("Evidence signals")
  )
  if (show_plaus) {
    head_cells <- c(head_cells, list(tags$th("Plausibility")))
  }
  head_cells <- c(
    head_cells,
    list(
      tags$th(class = "rt", "Composite"),
      tags$th("Grade"),
      tags$th(class = "rt", "Caveats")
    )
  )

  rows <- lapply(seq_len(nrow(genes)), function(i) {
    g <- genes[i, , drop = FALSE]
    sym <- g$symbol
    vetoed <- "vetoed" %in% names(g) && isTRUE(g$vetoed[1])
    is_sel <- !is.null(selected) && identical(sym, selected)
    n_cav <- if ("caveats" %in% names(g)) length(g$caveats[[1]]) else 0L
    ens <- if (isTRUE(g$resolved) && !is_blank(g$gene_id)) {
      tags$span(class = "ens", g$gene_id)
    }
    cells <- list(
      tags$td(class = "rank num", g$rank),
      tags$td(class = "gene", tags$b(sym), ens),
      tags$td(gs_signal_bars(g, registry))
    )
    if (show_plaus) {
      cells <- c(cells, list(tags$td(gs_plaus_badge(verdicts[[toupper(sym)]]))))
    }
    cells <- c(
      cells,
      list(
        tags$td(class = "comp", gs_composite_cell(g$composite)),
        tags$td(gs_grade_pill(g$grade)),
        tags$td(
          class = "rt",
          tags$span(
            class = paste("gs-caveat", if (n_cav == 0) "none" else ""),
            n_cav
          )
        )
      )
    )
    tags$tr(
      class = paste(if (vetoed) "veto" else "", if (is_sel) "sel" else ""),
      tabindex = "0",
      `data-symbol` = sym,
      `data-input` = input_id,
      cells
    )
  })

  tags$div(
    class = "gs-tablewrap",
    tags$table(
      class = "gs-table",
      tags$thead(tags$tr(head_cells)),
      tags$tbody(rows)
    )
  )
}

# ---- detail pane -----------------------------------------------------------

# The sticky detail aside for one selected gene. Order puts the actionable read
# first: header, caveat/veto, the specialist verdict + next step (and the full
# per-domain analysis when present), then the composite breakdown, and finally the
# grounded evidence collapsed by default (it can be long). `verdict` is this gene's
# synthesized verdict (or NULL); `spec_ran` says whether specialists were run at
# all (to choose the empty-verdict prompt vs a blank); `specialist_analysis` is the
# optional full per-domain analysis tag (rendered by the module).
gs_detail_pane <- function(
  gene_row,
  evidence,
  registry,
  verdict = NULL,
  spec_ran = FALSE,
  specialist_analysis = NULL
) {
  g <- gene_row
  vetoed <- "vetoed" %in% names(g) && isTRUE(g$vetoed[1])
  ens <- if (isTRUE(g$resolved) && !is_blank(g$gene_id)) {
    tags$span(class = "ens", g$gene_id)
  }

  # Composite breakdown: one row per signal, normalized bar + raw value.
  breakdown <- lapply(seq_len(nrow(registry)), function(i) {
    key <- registry$key[i]
    key_n <- paste0(key, "_n")
    vn <- if (key_n %in% names(g)) g[[key_n]] else NA_real_
    vraw <- if (key %in% names(g)) g[[key]] else NA_real_
    w <- if (is.null(vn) || length(vn) == 0 || is.na(vn)) {
      0
    } else {
      max(0, min(1, vn)) * 100
    }
    bar_cls <- if (gs_is_annotation(registry$role[i])) "teal" else NULL
    tagList(
      tags$div(class = "lab", registry$label[i]),
      tags$div(
        class = "track",
        tags$i(class = bar_cls, style = sprintf("width:%.0f%%", w))
      ),
      tags$div(class = "val", format_signal_value(vraw))
    )
  })

  # Caveat / veto note.
  reasons <- if ("caveats" %in% names(g)) g$caveats[[1]] else character()
  cav_note <- if (length(reasons) > 0) {
    tags$div(
      class = paste("gs-veto-note", if (!vetoed) "caveat" else ""),
      tags$b(if (vetoed) "Vetoed. " else "Caveats. "),
      tags$ul(lapply(reasons, tags$li))
    )
  }

  # Grounded evidence, ordered by domain.
  ev <- evidence[evidence$gene_id == g$gene_id, , drop = FALSE]
  ev_block <- if (nrow(ev) == 0) {
    tags$p(
      class = "faint",
      style = "font-style:italic;margin:0",
      "No grounded evidence for this gene."
    )
  } else {
    ordered_domains <- intersect(names(GS_DOMAIN_TAG), unique(ev$domain))
    ev <- ev[order(match(ev$domain, ordered_domains)), , drop = FALSE]
    lapply(seq_len(nrow(ev)), function(i) {
      tag_lab <- GS_DOMAIN_TAG[[ev$domain[i]]] %||% ev$domain[i]
      src <- if (is_blank(ev$source_url[i])) {
        tags$span(class = "src mono", ev$source_id[i])
      } else {
        tags$a(
          class = "src mono",
          href = ev$source_url[i],
          target = "_blank",
          rel = "noopener",
          ev$source_id[i]
        )
      }
      detail <- if (!is_blank(ev$detail[i])) {
        tags$div(class = "detail", ev$detail[i])
      }
      tags$div(
        class = "gs-ev",
        tags$span(class = "tag", tag_lab),
        tags$div(
          class = "body",
          tags$div(class = "title", ev$title[i]),
          detail,
          src
        )
      )
    })
  }

  # Verdict / next step.
  verdict_block <- if (vetoed) {
    tags$div(
      class = "gs-verdict empty",
      "Vetoed: not prioritized. See the caveat above for the reason."
    )
  } else if (!is.null(verdict) && nzchar(verdict$verdict %||% "")) {
    ids <- if (length(verdict$source_ids) > 0) {
      tags$div(
        class = "ids",
        paste0("[", paste(verdict$source_ids, collapse = ", "), "]")
      )
    }
    next_exp <- if (nzchar(verdict$next_experiment %||% "")) {
      tags$div(
        class = "next",
        tags$b("Suggested next experiment"),
        verdict$next_experiment
      )
    }
    tags$div(
      class = "gs-verdict",
      tags$div(
        class = "row1",
        tags$span(class = "lbl", "Specialist verdict"),
        gs_plaus_badge(verdict)
      ),
      tags$p(verdict$verdict),
      next_exp,
      ids
    )
  } else if (spec_ran) {
    tags$div(
      class = "gs-verdict empty",
      "No specialist verdict for this gene in this run."
    )
  } else {
    tags$div(
      class = "gs-verdict empty",
      "Run ",
      tags$b("Analyze with specialists"),
      " for a plausibility verdict and a suggested next experiment."
    )
  }

  # Grounded evidence is collapsed by default (it can be a long list); the empty
  # case shows the note inline instead of an empty disclosure.
  evidence_section <- if (nrow(ev) == 0) {
    tags$div(class = "gs-sec", tags$h3("Grounded evidence"), ev_block)
  } else {
    tags$div(
      class = "gs-sec",
      tags$details(
        class = "gs-evidence",
        tags$summary(
          tags$h3(sprintf("Grounded evidence (%d)", nrow(ev)))
        ),
        tags$div(class = "gs-evidence-body", ev_block)
      )
    )
  }

  tagList(
    tags$div(
      class = "head",
      tags$div(class = "sym", tags$b(g$symbol), ens),
      tags$div(
        class = "meta",
        gs_grade_pill(g$grade),
        tags$span(
          class = "comp-big",
          tags$span(class = "lab", "Composite"),
          gs_composite_cell(g$composite)
        )
      )
    ),
    if (!is.null(cav_note)) tags$div(class = "gs-sec", cav_note),
    tags$div(class = "gs-sec", tags$h3("Verdict & next step"), verdict_block),
    if (!is.null(specialist_analysis)) {
      tags$div(class = "gs-sec", specialist_analysis)
    },
    tags$div(
      class = "gs-sec",
      tags$h3("Composite breakdown"),
      tags$div(class = "gs-bd", breakdown)
    ),
    evidence_section
  )
}

# The scoring legend as a collapsible note under the grid (reuses the report's
# registry legend body).
gs_legend <- function(registry) {
  tags$details(
    class = "gs-legend",
    tags$summary("How the composite score works"),
    tags$div(class = "body", registry_legend_html(registry))
  )
}
