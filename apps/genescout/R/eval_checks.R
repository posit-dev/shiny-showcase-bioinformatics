# Eval identity guard - assert the resolved gene IS the gene the candidate named.
#
# Why this exists: a real regression once resolved the input symbol TTN to a
# DIFFERENT gene (TTR), because a fuzzy alias/retired match out-scored the exact
# symbol in MyGene's ranking. The ranking evals only checked grades and relative
# order, so a wrong-gene resolution slipped through silently - the ranking "held"
# while the identity was wrong. These pure checks make identity a first-class,
# explicitly-asserted invariant, and (via the offline test that drives them on a
# synthetic mis-resolution) they are protected in CI without any live API call.
#
# Both checks are pure over the ranked `genes` tibble (needs columns `symbol` and
# `gene_id`), so the live harness (evals/run_evals.R) and the offline test share
# one implementation.

# Every candidate symbol must appear verbatim (case-insensitive) among the resolved
# output symbols. If TTN mis-resolves to TTR, the output carries "TTR", so "TTN" is
# absent and is reported as missing. This is the free, ground-truth-free guard that
# applies to every case whose candidates are official gene symbols; it catches the
# exact bug class above (an input symbol silently becoming a different gene).
identity_missing_symbols <- function(genes, candidates) {
  candidates <- as.character(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(trimws(candidates))]
  out <- toupper(as.character(genes$symbol))
  candidates[!toupper(candidates) %in% out]
}

# Optional anchor map (expect_identity: SYMBOL -> Ensembl gene id). For each named
# gene the resolved row for that symbol must carry that exact Ensembl id - catching
# both a mis-resolution (no row for the symbol, or an UNRESOLVED:* id) and the rarer
# "right symbol, wrong id" drift. Returns a character vector of human-readable
# mismatch strings (empty when all anchors hold).
identity_id_mismatches <- function(genes, expect_identity = NULL) {
  if (is.null(expect_identity) || length(expect_identity) == 0) {
    return(character())
  }
  syms <- toupper(as.character(genes$symbol))
  ids <- as.character(genes$gene_id)
  out <- character()
  for (sym in names(expect_identity)) {
    want <- as.character(expect_identity[[sym]])
    hit <- which(syms == toupper(sym))
    got <- if (length(hit) == 0) NA_character_ else ids[hit[1]]
    if (length(hit) == 0 || is.na(got) || !identical(got, want)) {
      shown <- if (length(hit) == 0 || is.na(got)) "<absent>" else got
      out <- c(out, sprintf("%s: want %s, got %s", sym, want, shown))
    }
  }
  out
}

# Full identity assertion for one eval case. Returns
#   list(ok, missing = <candidate symbols that did not round-trip>,
#        mismatched = <anchor id-mismatch strings>, detail = <one-line summary>).
# `ok` is TRUE only when every candidate round-trips AND every anchor id matches.
assert_gene_identity <- function(genes, candidates, expect_identity = NULL) {
  missing <- identity_missing_symbols(genes, candidates)
  mismatched <- identity_id_mismatches(genes, expect_identity)
  ok <- length(missing) == 0 && length(mismatched) == 0
  parts <- character()
  if (length(missing) > 0) {
    parts <- c(
      parts,
      paste0("did not resolve to themselves: ", paste(missing, collapse = ", "))
    )
  }
  if (length(mismatched) > 0) {
    parts <- c(
      parts,
      paste0("id mismatch: ", paste(mismatched, collapse = "; "))
    )
  }
  detail <- if (ok) "identity OK" else paste(parts, collapse = "; ")
  list(ok = ok, missing = missing, mismatched = mismatched, detail = detail)
}
