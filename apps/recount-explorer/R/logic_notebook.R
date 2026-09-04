# The reproduction session as a Quarto or an R Markdown notebook.
#
# Both formats are plain text, so writing one needs neither the quarto CLI nor
# the rmarkdown package. The code is exactly what the .R script already
# produces, split at its comment headings and wrapped in chunks, so the three
# formats cannot drift apart.

REPRODUCTION_FORMATS <- c(
  "R script (.R)" = "r",
  "Quarto (.qmd)" = "qmd",
  "R Markdown (.Rmd)" = "rmd"
)

# Drop blank lines from both ends of a character vector.
trim_blank_ends <- function(x) {
  while (length(x) && !nzchar(trimws(x[length(x)]))) {
    x <- x[-length(x)]
  }
  while (length(x) && !nzchar(trimws(x[1L]))) {
    x <- x[-1L]
  }
  x
}

# Split the flat script into sections. A leading comment becomes the heading,
# and the lines under it become one code chunk.
script_sections <- function(script) {
  lines <- strsplit(script, "\n", fixed = TRUE)[[1L]]

  is_comment <- startsWith(lines, "#")
  is_heading <- is_comment & startsWith(lines, "# ") & !grepl("http", lines)
  is_code <- !is_comment

  # Everything before the first line of code is the script's own header:
  # provenance and the citation. The notebook says all of that in its front
  # matter and its intro, so a heading up there would end up titling the
  # library calls.
  first_code <- which(is_code & nzchar(trimws(lines)))
  first_code <- if (length(first_code)) first_code[[1L]] else length(lines) + 1L
  is_heading[seq_len(min(first_code, length(is_heading)))] <- FALSE

  # A section starts at each heading, so the section a line belongs to is the
  # number of headings at or before it.
  section_of <- cumsum(is_heading)

  lapply(split(seq_along(lines), section_of), function(idx) {
    code <- trim_blank_ends(lines[idx][is_code[idx]])
    if (!length(code)) {
      return(NULL)
    }
    heading <- idx[is_heading[idx]]
    list(
      title = if (length(heading)) {
        sub("^# ", "", lines[heading[[1L]]])
      } else {
        NULL
      },
      code = code
    )
  }) |>
    Filter(f = Negate(is.null)) |>
    unname()
}

notebook_front_matter <- function(title, subtitle, format) {
  if (identical(format, "qmd")) {
    return(c(
      "---",
      sprintf('title: "%s"', title),
      sprintf('subtitle: "%s"', subtitle),
      "format:",
      "  html:",
      "    toc: true",
      "    code-fold: show",
      "    embed-resources: true",
      "execute:",
      "  warning: false",
      "---"
    ))
  }
  c(
    "---",
    sprintf('title: "%s"', title),
    sprintf('subtitle: "%s"', subtitle),
    "output:",
    "  html_document:",
    "    toc: true",
    "    df_print: paged",
    "---",
    "",
    "```{r setup, include = FALSE}",
    "knitr::opts_chunk$set(warning = FALSE, message = FALSE)",
    "```"
  )
}

build_reproduction_notebook <- function(
  study,
  gene_state = NULL,
  pca_state = NULL,
  format = c("qmd", "rmd"),
  quality_state = NULL
) {
  format <- match.arg(format)
  sections <- script_sections(
    build_reproduction_script(study, gene_state, pca_state, quality_state)
  )

  front <- notebook_front_matter(
    title = sprintf("Recount Explorer: %s", study$project),
    subtitle = sprintf(
      "%s, %s, %s samples",
      toupper(study$source),
      study$organism,
      format(ncol(study$rse), big.mark = ",")
    ),
    format = format
  )

  intro <- c(
    "",
    sprintf(
      "Reproduces a Recount Explorer session for study **%s**.",
      study$project
    ),
    sprintf(
      "Generated on %s with R %s and recount3 %s.",
      format(Sys.Date()),
      getRversion(),
      utils::packageVersion("recount3")
    ),
    "",
    paste(
      "Data from the recount3 project. Cite Wilks et al. 2021, Genome Biology",
      "22:323, <https://doi.org/10.1186/s13059-021-02533-6>."
    ),
    ""
  )

  body <- unlist(lapply(sections, function(section) {
    c(
      if (!is.null(section$title)) c(paste("##", section$title), ""),
      "```{r}",
      section$code,
      "```",
      ""
    )
  }))

  paste(c(front, intro, body), collapse = "\n")
}

# One entry point for all three formats.
build_reproduction <- function(
  study,
  gene_state = NULL,
  pca_state = NULL,
  format = "r",
  quality_state = NULL
) {
  if (identical(format, "r")) {
    return(build_reproduction_script(
      study,
      gene_state,
      pca_state,
      quality_state
    ))
  }
  build_reproduction_notebook(
    study,
    gene_state,
    pca_state,
    format = format,
    quality_state = quality_state
  )
}

reproduction_extension <- function(format) {
  switch(format, qmd = "qmd", rmd = "Rmd", "R")
}
