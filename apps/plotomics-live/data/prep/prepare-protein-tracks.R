#!/usr/bin/env Rscript
# Prepare the protein annotation tracks behind the domain lollipop page.
#
# Two layers, two sources, both free and unauthenticated:
#
#   Pfam domains  InterPro REST
#                 https://www.ebi.ac.uk/interpro/api/entry/pfam/protein/reviewed/<acc>
#                 License: CC0 1.0 (InterPro/Pfam data is public domain)
#
#   PTM sites     UniProtKB REST, the MOD_RES feature type
#                 https://rest.uniprot.org/uniprotkb/<acc>.json
#                 License: CC BY 4.0
#
# Note the InterPro path is /protein/reviewed/, not /protein/uniprot/. The
# latter returns an empty result set rather than an error, which is an easy way
# to end up shipping a lollipop with no domains and not notice.
#
# PhosphoSitePlus would give denser PTM coverage but requires an account for
# bulk download and is CC BY-NC-SA, which is not compatible with a public repo.
# UniProt MOD_RES is evidence-backed and redistributable, so that is what we use.
#
# The mutation stems come from data/brca_lollipop.csv, written by
# prepare-brca-cohort.R from the same cBioPortal fetch as the oncoplot.
#
# Re-run to refresh; raw API responses are cached in data/raw/.

suppressWarnings(suppressMessages({
  library(jsonlite)
}))

# --- paths -----------------------------------------------------------------
this_file <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) f[1] else if (!is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else NA
}
app_dir <- if (!is.na(this_file)) {
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE)
} else getwd()
if (!dir.exists(file.path(app_dir, "srcts"))) app_dir <- getwd()
raw_dir <- file.path(app_dir, "data", "raw")
csv_dir <- file.path(app_dir, "data")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

# The BRCA drivers that have both a recognisable domain architecture and enough
# recurrent variants for a lollipop to say something. Same accessions the
# structure and PAE pages offer, so the three protein pages line up.
PROTEINS <- data.frame(
  gene = c("TP53", "PIK3CA", "PTEN", "GATA3", "CDH1", "MAP3K1"),
  uniprot = c("P04637", "P42336", "P60484", "P23771", "P12830", "Q13233"),
  stringsAsFactors = FALSE)

get_once <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("cached: ", basename(dest)); return(invisible(dest))
  }
  message("GET ", url)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}

# --- 1. Pfam domains -------------------------------------------------------
domains <- do.call(rbind, lapply(seq_len(nrow(PROTEINS)), function(i) {
  acc <- PROTEINS$uniprot[i]
  f <- file.path(raw_dir, paste0("interpro_", acc, ".json"))
  get_once(sprintf(
    "https://www.ebi.ac.uk/interpro/api/entry/pfam/protein/reviewed/%s?page_size=50",
    acc), f)
  j <- fromJSON(f, simplifyDataFrame = FALSE)
  rows <- lapply(j$results, function(r) {
    prot <- r$proteins[[1]]
    do.call(rbind, lapply(prot$entry_protein_locations, function(loc) {
      fr <- loc$fragments[[1]]
      data.frame(gene = PROTEINS$gene[i], uniprot = acc,
                 length = prot$protein_length,
                 pfam = r$metadata$accession, name = r$metadata$name,
                 start = fr$start, end = fr$end,
                 stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  message("  ", PROTEINS$gene[i], ": ", nrow(out), " domain(s)")
  out
}))
domains <- domains[order(domains$gene, domains$start), ]

# --- 2. PTM sites ----------------------------------------------------------
# Collapse UniProt's free-text descriptions ("Phosphoserine; by ATM") into the
# handful of types a track can actually distinguish by glyph.
ptm_type <- function(desc) {
  d <- tolower(desc)
  if (grepl("phospho", d)) "phospho"
  else if (grepl("acetyl", d)) "acetyl"
  else if (grepl("methyl", d)) "methyl"
  else if (grepl("ubiquitin|sumo", d)) "ubiquitin"
  else "other"
}

ptms <- do.call(rbind, lapply(seq_len(nrow(PROTEINS)), function(i) {
  acc <- PROTEINS$uniprot[i]
  f <- file.path(raw_dir, paste0("uniprot_", acc, ".json"))
  get_once(sprintf(
    "https://rest.uniprot.org/uniprotkb/%s.json?fields=ft_mod_res,sequence", acc), f)
  j <- fromJSON(f, simplifyDataFrame = FALSE)
  feats <- Filter(function(x) identical(x$type, "Modified residue"), j$features)
  if (!length(feats)) {
    message("  ", PROTEINS$gene[i], ": 0 PTM sites")
    return(NULL)
  }
  out <- do.call(rbind, lapply(feats, function(x) {
    data.frame(gene = PROTEINS$gene[i], uniprot = acc,
               position = x$location$start$value,
               type = ptm_type(x$description %||% ""),
               description = sub(";.*$", "", x$description %||% ""),
               stringsAsFactors = FALSE)
  }))
  message("  ", PROTEINS$gene[i], ": ", nrow(out), " PTM site(s)")
  out
}))
ptms <- ptms[order(ptms$gene, ptms$position), ]

# --- write -----------------------------------------------------------------
utils::write.csv(domains, file.path(csv_dir, "protein_domains.csv"),
                 row.names = FALSE)
utils::write.csv(ptms, file.path(csv_dir, "protein_ptm.csv"),
                 row.names = FALSE)
message("wrote data/protein_domains.csv (", nrow(domains), " rows)")
message("wrote data/protein_ptm.csv     (", nrow(ptms), " rows)")
