# Data model

The dataset is one cell-level obs table linked to four small annotation tables and
an external variant table.

## Tables and key columns

- obs_metadata (about 100.6M rows, one per cell): drug, cell_line, plate, sample,
  drugname_drugconc (encodes the dose), QC columns (gene_count, tscp_count,
  pcnt_mito, pass_filter), cell cycle (S_score, G2M_score, phase), and
  BARCODE_SUB_LIB_ID (the AnnData obs index).
- drug_metadata (379): drug, targets, moa-broad, moa-fine, human-approved,
  clinical-trials, pubchem_cid, canonical_smiles.
- cell_line_metadata (driver-level; about 102 annotated, 50 assayed): cell_name,
  Organ, Driver_Gene_Symbol, Driver_VarType, and external ids.
- sample_metadata (1,344): sample, plate, drug, drugname_drugconc, and per-sample
  QC means.
- gene_metadata (62,710): gene_symbol, ensembl_id, token_id.
- cell_line_variants (DepMap and Cellosaurus somatic variants): cell_name, gene,
  protein_change, source.

## The pooling concept (state this whenever subsetting)

All 50 cell lines are pooled into every sample. A sample is one drug at one dose on
one plate, not a single cell line. Therefore:

- Organ, driver, and cell-line filters change the cell count, not the number of
  samples.
- Drug, dose, and plate filters change the number of samples.
