# Resolve a free-text or pasted gene list against the set of available genes.
# Used by the gene-selector module so users can paste a list (commas, spaces,
# newlines, or a mix) and get back the genes that exist plus the ones that don't.
#
# Pure logic layer -- NO shiny:: calls (see CLAUDE.md).

# text: a character scalar/vector of gene tokens in any common delimiting.
# available: the genes that exist (e.g. rownames of the expression matrix).
# Returns list(found = genes present in `available`, in input order, deduped;
#             unknown = tokens not in `available`).
parse_gene_input <- function(text, available) {
  tokens <- unlist(strsplit(
    paste(text, collapse = " "),
    "[\\s,;]+",
    perl = TRUE
  ))
  tokens <- trimws(tokens)
  tokens <- unique(tokens[nzchar(tokens)])
  available <- as.character(available)
  list(
    found = tokens[tokens %in% available],
    unknown = tokens[!tokens %in% available]
  )
}
