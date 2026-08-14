#!/usr/bin/env Rscript
# Prepare the per-sample TCGA-BRCA cohort behind the oncoplot and the protein
# domain lollipop.
#
# Source: cBioPortal REST API, study `brca_tcga_pan_can_atlas_2018`
#   Breast Invasive Carcinoma (TCGA, PanCancer Atlas), 1,084 samples, hg19.
#   https://www.cbioportal.org/study/summary?id=brca_tcga_pan_can_atlas_2018
#
# hg19 matters: it matches the coordinates already used by data/mutations.csv
# and the IGV page, so every genomic view in the gallery is on one assembly.
#
# Three layers are pulled and merged into one alteration table:
#   - somatic mutations (MUTATION_EXTENDED profile)
#   - GISTIC discrete copy number, keeping only the calls an oncoplot shows
#     (-2 deep deletion, +2 amplification)
#   - patient clinical annotation, including overall survival
#
# The bulk-download tarballs at cbioportal-datahub.s3.amazonaws.com now return
# 403, so this goes through the REST API. No account, no key.
#
# License: TCGA open-access tier, redistributable with citation. Cite TCGA and
# cBioPortal (Cerami et al. 2012; Gao et al. 2013).
#
# Re-run to refresh; raw API responses are cached in data/raw/.

suppressWarnings(suppressMessages({
  library(jsonlite)
}))

# --- paths -----------------------------------------------------------------
# Resolve the app root from this script's own location (data/prep/ -> app root),
# robust under Rscript, source(), and interactive runs.
this_file <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) f[1] else if (!is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else NA
}
app_dir <- if (!is.na(this_file)) {
  normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE)
} else getwd()
if (!dir.exists(file.path(app_dir, "srcts"))) app_dir <- getwd()  # last-resort fallback
raw_dir <- file.path(app_dir, "data", "raw")
csv_dir <- file.path(app_dir, "data")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

API <- "https://www.cbioportal.org/api"
STUDY <- "brca_tcga_pan_can_atlas_2018"

# Recurrently altered BRCA drivers. The first 15 are the set already used by
# data/mutations.csv (via lifescience-shiny-gallery); the rest add the copy
# number drivers an oncoplot of this cohort is expected to show.
GENES <- c("PIK3CA", "TP53", "GATA3", "MAP3K1", "CDH1", "KMT2C", "PTEN",
           "NCOR1", "ARID1A", "RUNX1", "AKT1", "CBFB", "SF3B1", "TBX3",
           "FOXA1", "NF1", "RB1", "ERBB2", "BRCA1", "BRCA2", "PIK3R1",
           "ATM", "CHEK2", "MAP2K4", "ZFP36L1", "GPS2", "SMAD4", "MYC",
           "CCND1", "FGFR1")

# cBioPortal's mutationType vocabulary collapsed onto the classes an oncoplot
# legend actually shows. Anything unmapped falls through to "Other".
MUT_CLASS <- c(
  Missense_Mutation      = "Missense",
  Nonsense_Mutation      = "Truncating",
  Frame_Shift_Del        = "Frameshift",
  Frame_Shift_Ins        = "Frameshift",
  Splice_Site            = "Splice",
  Splice_Region          = "Splice",
  In_Frame_Del           = "In-frame indel",
  In_Frame_Ins           = "In-frame indel",
  Translation_Start_Site = "Truncating",
  Nonstop_Mutation       = "Truncating")

# --- fetch helpers ---------------------------------------------------------
get_once <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("cached: ", basename(dest)); return(invisible(dest))
  }
  message("GET ", url)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}

# The mutation and CNA endpoints are POST-only (the gene list goes in the body),
# so these cannot use download.file. curl is already an R dependency of the
# tooling here, and using it directly avoids adding httr just for two calls.
post_once <- function(url, body, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("cached: ", basename(dest)); return(invisible(dest))
  }
  message("POST ", url)
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Content-Type" = "application/json",
                          "Accept" = "application/json")
  curl::handle_setopt(h, post = TRUE, postfields = body)
  curl::curl_download(url, dest, handle = h)
  invisible(dest)
}

# --- 1. resolve Entrez ids -------------------------------------------------
genes_f <- file.path(raw_dir, "cbio_genes.json")
post_once(paste0(API, "/genes/fetch?geneIdType=HUGO_GENE_SYMBOL"),
          toJSON(GENES), genes_f)
gene_tbl <- fromJSON(genes_f)
stopifnot(nrow(gene_tbl) > 0)
message("resolved ", nrow(gene_tbl), " / ", length(GENES), " genes")
entrez <- gene_tbl$entrezGeneId

# --- 2. mutations ----------------------------------------------------------
mut_f <- file.path(raw_dir, "cbio_mutations.json")
post_once(sprintf("%s/molecular-profiles/%s_mutations/mutations/fetch?projection=DETAILED",
                  API, STUDY),
          toJSON(list(entrezGeneIds = entrez,
                      sampleListId = paste0(STUDY, "_sequenced")),
                 auto_unbox = TRUE),
          mut_f)
mut <- fromJSON(mut_f, flatten = TRUE)
message("mutations: ", nrow(mut), " in ", length(unique(mut$sampleId)), " samples")

mut_long <- data.frame(
  gene   = mut$gene.hugoGeneSymbol,
  sample = mut$sampleId,
  class  = unname(MUT_CLASS[mut$mutationType]),
  stringsAsFactors = FALSE)
mut_long$class[is.na(mut_long$class)] <- "Other"

# --- 3. copy number (GISTIC) ----------------------------------------------
# Only homozygous deletion (-2) and high-level amplification (+2) are shown;
# shallow calls (-1 / +1) are too common to be informative on an oncoplot.
cna_f <- file.path(raw_dir, "cbio_cna.json")
post_once(sprintf("%s/molecular-profiles/%s_gistic/discrete-copy-number/fetch?discreteCopyNumberEventType=ALL&projection=DETAILED",
                  API, STUDY),
          toJSON(list(entrezGeneIds = entrez,
                      sampleListId = paste0(STUDY, "_cna")),
                 auto_unbox = TRUE),
          cna_f)
cna <- fromJSON(cna_f, flatten = TRUE)
keep <- cna$alteration %in% c(-2L, 2L)
cna_long <- data.frame(
  gene   = cna$gene.hugoGeneSymbol[keep],
  sample = cna$sampleId[keep],
  class  = ifelse(cna$alteration[keep] == 2L, "Amplification", "Deep deletion"),
  stringsAsFactors = FALSE)
message("copy-number events kept: ", nrow(cna_long),
        " of ", nrow(cna), " calls")

# --- 4. collapse to one class per gene x sample ----------------------------
# A sample carrying two different alteration classes in one gene is "Multi-hit",
# which is the convention every oncoplot implementation uses.
alt <- rbind(mut_long, cna_long)
alt <- unique(alt)
n_class <- stats::aggregate(class ~ gene + sample, data = alt,
                            FUN = function(x) length(unique(x)))
first_class <- stats::aggregate(class ~ gene + sample, data = alt,
                                FUN = function(x) sort(unique(x))[1])
onco <- merge(n_class, first_class, by = c("gene", "sample"),
              suffixes = c("_n", "_first"))
onco$class <- ifelse(onco$class_n > 1L, "Multi-hit", onco$class_first)
onco <- onco[, c("gene", "sample", "class")]
onco <- onco[order(onco$gene, onco$sample), ]
message("altered gene x sample pairs: ", nrow(onco))

# --- 5. clinical -----------------------------------------------------------
clin_f <- file.path(raw_dir, "cbio_clinical.json")
get_once(sprintf("%s/studies/%s/clinical-data?clinicalDataType=PATIENT&pageSize=10000000",
                 API, STUDY), clin_f)
clin_raw <- fromJSON(clin_f)
want <- c("SUBTYPE", "AJCC_PATHOLOGIC_TUMOR_STAGE", "SEX", "AGE",
          "OS_MONTHS", "OS_STATUS")
cl <- clin_raw[clin_raw$clinicalAttributeId %in% want, ]
# long -> wide on patientId
clin <- reshape(cl[, c("patientId", "clinicalAttributeId", "value")],
                idvar = "patientId", timevar = "clinicalAttributeId",
                direction = "wide")
names(clin) <- sub("^value\\.", "", names(clin))

# Samples are patient id + "-01"; map each sequenced sample back to its patient.
samples <- sort(unique(c(mut$sampleId, cna$sampleId)))
clin_out <- data.frame(sample = samples,
                       patient = sub("-[0-9]+$", "", samples),
                       stringsAsFactors = FALSE)
clin_out <- merge(clin_out, clin, by.x = "patient", by.y = "patientId",
                  all.x = TRUE)

# Tidy the free-text stage codes into the four AJCC stages. cBioPortal returns
# these uppercase ("STAGE IIA"), and the A/B/C substages are more granularity
# than an annotation strip can show. "STAGE X" means unstaged, so it drops out.
stage <- toupper(trimws(clin_out$AJCC_PATHOLOGIC_TUMOR_STAGE))
stage <- sub("^STAGE\\s+", "", stage)
stage <- sub("[ABC]$", "", stage)
stage[!stage %in% c("I", "II", "III", "IV")] <- NA
clin_out$stage <- stage
clin_out$subtype <- sub("^BRCA_", "", clin_out$SUBTYPE)
clin_out$sex <- clin_out$SEX
clin_out$age <- suppressWarnings(as.numeric(clin_out$AGE))
clin_out$os_months <- suppressWarnings(as.numeric(clin_out$OS_MONTHS))
# "1:DECEASED" / "0:LIVING" -> 1 / 0
clin_out$os_event <- suppressWarnings(as.integer(sub(":.*$", "", clin_out$OS_STATUS)))
clin_out <- clin_out[, c("sample", "patient", "subtype", "stage", "sex", "age",
                         "os_months", "os_event")]
clin_out <- clin_out[order(clin_out$sample), ]

# --- 6. lollipop table -----------------------------------------------------
# Per-residue recurrence for the protein domain lollipop. Kept in the same
# script because it is the same fetch: proteinPosStart comes back with the
# mutation records, so splitting it would mean pulling the cohort twice.
lp <- mut[!is.na(mut$proteinPosStart) & mut$proteinPosStart > 0, ]
lp_long <- data.frame(
  gene = lp$gene.hugoGeneSymbol,
  residue = as.integer(lp$proteinPosStart),
  protein_change = lp$proteinChange,
  class = unname(MUT_CLASS[lp$mutationType]),
  stringsAsFactors = FALSE)
lp_long$class[is.na(lp_long$class)] <- "Other"
lp_agg <- stats::aggregate(list(count = rep(1L, nrow(lp_long))),
                           by = lp_long[, c("gene", "residue",
                                            "protein_change", "class")],
                           FUN = sum)
lp_agg <- lp_agg[order(lp_agg$gene, lp_agg$residue), ]
message("lollipop rows: ", nrow(lp_agg))

# --- write -----------------------------------------------------------------
utils::write.csv(onco, file.path(csv_dir, "brca_oncoplot.csv"),
                 row.names = FALSE)
utils::write.csv(clin_out, file.path(csv_dir, "brca_clinical.csv"),
                 row.names = FALSE)
utils::write.csv(lp_agg, file.path(csv_dir, "brca_lollipop.csv"),
                 row.names = FALSE)

message("wrote data/brca_oncoplot.csv  (", nrow(onco), " rows)")
message("wrote data/brca_clinical.csv  (", nrow(clin_out), " rows)")
message("wrote data/brca_lollipop.csv  (", nrow(lp_agg), " rows)")
