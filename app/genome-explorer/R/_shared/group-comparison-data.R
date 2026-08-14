# Prepare data for a "value by group" comparison: align a per-sample value to
# the sample metadata and test for a between-group difference. Generic on
# purpose -- the value can be a gene's expression (DE gene detail) or a
# signature's score (Signature Scoring), so several apps reuse it.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md). Returns a tidy data
# frame the classic Shiny module turns into boxplots, and a plain test result
# the module can annotate with.

# values: a per-sample numeric value. Either a named vector (names = sample ids)
#   or an unnamed vector aligned to metadata$sample order.
# metadata: data frame with a `sample` column and the grouping column.
# group_col: the metadata column that defines the groups.
# Returns data.frame(sample, group [factor], value), dropping samples with a
# missing group or value.
group_comparison_data <- function(values, metadata, group_col = "group") {
  if (!"sample" %in% names(metadata)) {
    stop("metadata must have a 'sample' column")
  }
  if (!group_col %in% names(metadata)) {
    stop("metadata has no column '", group_col, "'")
  }
  if (is.null(names(values))) {
    if (length(values) != nrow(metadata)) {
      stop(
        "unnamed values must be aligned to metadata$sample (length mismatch)"
      )
    }
    names(values) <- as.character(metadata$sample)
  }

  df <- data.frame(
    sample = names(values),
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
  df$group <- metadata[[group_col]][match(
    df$sample,
    as.character(metadata$sample)
  )]
  df <- df[!is.na(df$group) & !is.na(df$value), , drop = FALSE]
  df$group <- factor(df$group)
  df
}

# Between-group difference test on the tidy data frame from
# group_comparison_data(): a two-sample t-test for two groups, Kruskal-Wallis
# for more. Returns list(method, p, n_groups); p is NA when a group is too small.
group_comparison_test <- function(df) {
  g <- droplevels(df$group)
  k <- nlevels(g)
  if (k < 2 || any(table(g) < 2)) {
    return(list(method = NA_character_, p = NA_real_, n_groups = k))
  }
  if (k == 2) {
    p <- tryCatch(
      stats::t.test(df$value ~ g)$p.value,
      error = function(e) NA_real_
    )
    list(method = "t-test", p = p, n_groups = k)
  } else {
    p <- tryCatch(
      stats::kruskal.test(df$value ~ g)$p.value,
      error = function(e) NA_real_
    )
    list(method = "Kruskal-Wallis", p = p, n_groups = k)
  }
}
