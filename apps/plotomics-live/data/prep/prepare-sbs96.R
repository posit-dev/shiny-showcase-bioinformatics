#!/usr/bin/env Rscript
# Prepare the mutational-signature data: observed 96-context spectra for a
# TCGA-BRCA cohort, plus signatures extracted de novo from them.
#
# Why not COSMIC. The COSMIC reference SBS profiles are the obvious thing to
# plot, and they are free to *use* academically, but their terms forbid
# redistribution: clause 4.7 prohibits providing "access (including without
# limitation via a public-access internet site) to the whole or any part of
# COSMIC to any third parties". Committing COSMIC_v3.x_SBS.txt to a public
# repository is squarely what that forbids. So this ships real spectra and real
# de novo signatures computed from open data instead, labelled as such. They are
# not COSMIC signatures and are not named as if they were.
#
# Source: GDC open-access TCGA-BRCA "Masked Somatic Mutation" MAFs.
#   https://api.gdc.cancer.gov  (no account, no token, open tier)
#   License: TCGA open-access, redistributable with citation.
#
# The trick that keeps this cheap: GDC MAFs carry a CONTEXT column holding an
# 11-base reference window centred on the variant, so the trinucleotide context
# is a substring. No BSgenome package (roughly 800 MB) and no reference FASTA.
#
# Re-run to refresh; the downloaded bundle is cached in data/raw/.

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
maf_dir <- file.path(raw_dir, "gdc_brca_maf")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

API <- "https://api.gdc.cancer.gov"
N_FILES <- 250L       # each MAF is one tumour, ~20 KB gzipped
N_SIGS <- 4L          # de novo signatures to extract
MIN_SNV <- 40L        # tumours below this are too sparse to fit

# --- 1. list open BRCA MAFs ------------------------------------------------
list_f <- file.path(raw_dir, "gdc_brca_maf_index.json")
if (!file.exists(list_f) || file.info(list_f)$size == 0) {
  message("querying GDC for open TCGA-BRCA MAFs ...")
  query <- toJSON(list(
    filters = list(op = "and", content = list(
      list(op = "in", content = list(field = "cases.project.project_id",
                                     value = "TCGA-BRCA")),
      list(op = "in", content = list(field = "files.data_format",
                                     value = "MAF")),
      list(op = "in", content = list(field = "files.access",
                                     value = "open")))),
    fields = "file_id,file_name",
    format = "JSON",
    size = as.character(N_FILES)), auto_unbox = TRUE)
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  curl::handle_setopt(h, post = TRUE, postfields = query)
  curl::curl_download(paste0(API, "/files"), list_f, handle = h)
}
idx <- fromJSON(list_f)
file_ids <- idx$data$hits$file_id
message("MAF files: ", length(file_ids))

# --- 2. download them as one bundle ---------------------------------------
bundle <- file.path(raw_dir, "gdc_brca_maf.tar.gz")
if (!file.exists(bundle) || file.info(bundle)$size == 0) {
  message("downloading ", length(file_ids), " MAFs from GDC ...")
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  curl::handle_setopt(h, post = TRUE,
                      postfields = toJSON(list(ids = file_ids)))
  curl::curl_download(paste0(API, "/data"), bundle, handle = h)
}
if (!dir.exists(maf_dir)) {
  dir.create(maf_dir, showWarnings = FALSE, recursive = TRUE)
  utils::untar(bundle, exdir = maf_dir)
}
mafs <- list.files(maf_dir, pattern = "\\.maf\\.gz$", recursive = TRUE,
                   full.names = TRUE)
message("extracted MAFs: ", length(mafs))
stopifnot(length(mafs) > 0)

# --- 3. the 96 trinucleotide contexts, in canonical order ------------------
BASES <- c("A", "C", "G", "T")
SUBS <- c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
ctx_grid <- expand.grid(three = BASES, five = BASES, sub = SUBS,
                        stringsAsFactors = FALSE)
ctx_grid <- ctx_grid[order(match(ctx_grid$sub, SUBS), ctx_grid$five,
                           ctx_grid$three), ]
CONTEXTS <- sprintf("%s[%s]%s", ctx_grid$five, ctx_grid$sub, ctx_grid$three)
TRINUC <- sprintf("%s%s%s", ctx_grid$five, substr(ctx_grid$sub, 1, 1),
                  ctx_grid$three)

comp <- c(A = "T", C = "G", G = "C", T = "A")
revcomp <- function(s) {
  vapply(strsplit(s, "", fixed = TRUE), function(ch) {
    paste(rev(unname(comp[ch])), collapse = "")
  }, character(1))
}

# --- 4. per-tumour 96-context counts ---------------------------------------
# SBS96 is pyrimidine-normalised: a variant whose reference base is A or G is
# reported on the opposite strand, so both the trinucleotide and the alt base
# are reverse-complemented.
message("counting contexts ...")
KEEP <- c("Variant_Type", "Reference_Allele", "Tumor_Seq_Allele2",
          "CONTEXT", "Tumor_Sample_Barcode")

spectrum_of <- function(path) {
  df <- try(utils::read.delim(gzfile(path), comment.char = "#",
                              stringsAsFactors = FALSE,
                              colClasses = "character", quote = ""),
            silent = TRUE)
  if (inherits(df, "try-error") || !all(KEEP %in% names(df))) return(NULL)
  df <- df[df$Variant_Type == "SNP", KEEP, drop = FALSE]
  if (!nrow(df)) return(NULL)
  ref <- df$Reference_Allele
  alt <- df$Tumor_Seq_Allele2
  ctx <- df$CONTEXT
  # The CONTEXT window is 11 bases with the variant at position 6, so the
  # trinucleotide is positions 5..7. Skip anything that is not that shape.
  ok <- nchar(ctx) == 11L & ref %in% BASES & alt %in% BASES & ref != alt
  if (!any(ok)) return(NULL)
  ref <- ref[ok]; alt <- alt[ok]
  tri <- substr(ctx[ok], 5L, 7L)
  # Sanity: the middle base of the trinucleotide must be the reference allele.
  ok2 <- substr(tri, 2L, 2L) == ref
  ref <- ref[ok2]; alt <- alt[ok2]; tri <- tri[ok2]
  if (!length(ref)) return(NULL)

  flip <- ref %in% c("A", "G")
  if (any(flip)) {
    tri[flip] <- revcomp(tri[flip])
    ref[flip] <- unname(comp[ref[flip]])
    alt[flip] <- unname(comp[alt[flip]])
  }
  key <- sprintf("%s[%s>%s]%s", substr(tri, 1L, 1L), ref, alt,
                 substr(tri, 3L, 3L))
  counts <- tabulate(match(key, CONTEXTS), nbins = 96L)
  list(sample = df$Tumor_Sample_Barcode[1], counts = counts)
}

spectra <- Filter(Negate(is.null), lapply(mafs, spectrum_of))
message("tumours parsed: ", length(spectra))
M <- do.call(rbind, lapply(spectra, `[[`, "counts"))
rownames(M) <- vapply(spectra, `[[`, character(1), "sample")
colnames(M) <- CONTEXTS
# Barcodes carry the aliquot suffix; trim to the sample id.
rownames(M) <- substr(rownames(M), 1L, 16L)
keep <- rowSums(M) >= MIN_SNV
message("tumours with >= ", MIN_SNV, " SNVs: ", sum(keep), " of ", nrow(M))
M <- M[keep, , drop = FALSE]

# --- 5. de novo signatures by NMF -----------------------------------------
# Lee & Seung multiplicative updates on the Frobenius objective. Written out
# rather than pulled from a package: it is 15 lines, it removes a dependency,
# and being seeded here means the shipped signatures are reproducible.
nmf_mu <- function(V, k, iters = 3000L, seed = 96L) {
  set.seed(seed)
  eps <- .Machine$double.eps
  W <- matrix(stats::runif(nrow(V) * k, 0.1, 1), nrow(V), k)
  H <- matrix(stats::runif(k * ncol(V), 0.1, 1), k, ncol(V))
  for (i in seq_len(iters)) {
    H <- H * (t(W) %*% V) / ((t(W) %*% W %*% H) + eps)
    W <- W * (V %*% t(H)) / ((W %*% H %*% t(H)) + eps)
  }
  # Scale each signature to sum to 1 and push the mass into the exposures, so
  # H rows are probability profiles and W columns are mutation counts.
  s <- rowSums(H)
  H <- H / s
  W <- sweep(W, 2, s, `*`)
  list(W = W, H = H)
}

message("extracting ", N_SIGS, " de novo signatures ...")
fit <- nmf_mu(M, N_SIGS)
sig <- t(fit$H)                       # 96 x k
colnames(sig) <- sprintf("BRCA-%s", LETTERS[seq_len(N_SIGS)])
# Order signatures by how much of the cohort they explain, so A is the largest.
ord <- order(colSums(fit$W), decreasing = TRUE)
sig <- sig[, ord, drop = FALSE]
colnames(sig) <- sprintf("BRCA-%s", LETTERS[seq_len(N_SIGS)])
expo <- fit$W[, ord, drop = FALSE]
colnames(expo) <- colnames(sig)

recon <- expo %*% t(sig)
cosine <- sum(M * recon) / sqrt(sum(M^2) * sum(recon^2))
message("reconstruction cosine similarity: ", round(cosine, 4))

# --- 6. write --------------------------------------------------------------
# The cohort catalogue: total counts per context across all tumours, which is
# what the page shows as the "observed" profile.
catalogue <- data.frame(
  context = CONTEXTS, trinuc = TRINUC, sub = ctx_grid$sub,
  count = as.integer(colSums(M)),
  stringsAsFactors = FALSE)
utils::write.csv(catalogue, file.path(csv_dir, "sbs96_catalogue.csv"),
                 row.names = FALSE)

sig_df <- data.frame(context = CONTEXTS, trinuc = TRINUC, sub = ctx_grid$sub,
                     round(sig, 6), stringsAsFactors = FALSE,
                     check.names = FALSE)
utils::write.csv(sig_df, file.path(csv_dir, "sbs96_signatures.csv"),
                 row.names = FALSE)

expo_df <- data.frame(sample = rownames(M), round(expo, 2),
                      stringsAsFactors = FALSE, check.names = FALSE)
utils::write.csv(expo_df, file.path(csv_dir, "sbs96_exposures.csv"),
                 row.names = FALSE)

message("wrote data/sbs96_catalogue.csv  (96 rows, ",
        format(sum(M), big.mark = ","), " SNVs)")
message("wrote data/sbs96_signatures.csv (96 x ", N_SIGS, ")")
message("wrote data/sbs96_exposures.csv  (", nrow(M), " tumours)")
