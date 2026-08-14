# Build downloadable artifacts: CSV gene list + markdown report ----------------

# Join the curated selection onto the ranked evidence to produce the export
# data frame. When `curated` is NULL, returns the full ranked table.
build_export_csv <- function(ranked, curated = NULL, included_only = TRUE) {
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(data.frame())
  }
  out <- tibble::as_tibble(ranked)

  if (!is.null(curated) && nrow(curated) > 0) {
    sel <- curated[,
      intersect(
        c("gene_symbol", "include", "confidence", "rationale"),
        names(curated)
      ),
      drop = FALSE
    ]
    out <- dplyr::left_join(out, sel, by = "gene_symbol")
    out$include[is.na(out$include)] <- FALSE
    if (included_only) {
      out <- out[out$include %in% TRUE, , drop = FALSE]
    }
    out <- out[order(-out$include, out$rank), , drop = FALSE]
  }

  as.data.frame(out)
}

# A human-readable markdown report of the run.
build_report_md <- function(
  disease,
  status,
  params,
  ranked,
  curated = NULL,
  ai_used = FALSE,
  ai_notes = NULL
) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  lines <- c(
    "# Gene List Builder report",
    "",
    paste0(
      "- **Disease:** ",
      disease$name %||% "?",
      " (`",
      disease$id %||% "?",
      "`)"
    ),
    paste0("- **Generated:** ", ts),
    paste0("- **Coverage bonus:** ", params$coverage_bonus %||% NA),
    paste0("- **Target list size:** ", params$top_n %||% NA),
    paste0("- **AI curation used:** ", if (isTRUE(ai_used)) "yes" else "no"),
    ""
  )

  if (!is.null(status) && nrow(status) > 0) {
    lines <- c(lines, "## Sources", "")
    for (i in seq_len(nrow(status))) {
      lines <- c(
        lines,
        paste0(
          "- **",
          status$label[i],
          "**: ",
          # Print the real status so an exported report distinguishes a source
          # that found nothing from one that failed or timed out.
          if ("status" %in% names(status)) {
            status$status[i]
          } else if (isTRUE(status$ok[i])) {
            "ok"
          } else {
            "error"
          },
          ", ",
          status$n_genes[i],
          " genes (",
          status$message[i],
          ")"
        )
      )
    }
    lines <- c(lines, "")
  }

  final <- build_export_csv(ranked, curated, included_only = TRUE)
  lines <- c(lines, paste0("## Final gene list (", nrow(final), " genes)"), "")
  if (nrow(final) > 0) {
    lines <- c(lines, paste(final$gene_symbol, collapse = ", "), "")
  }

  if (!is.null(ai_notes) && nzchar(ai_notes)) {
    lines <- c(lines, "## AI notes", "", ai_notes, "")
  }

  paste(lines, collapse = "\n")
}
