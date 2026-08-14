# Dataset provenance and refresh

Every dataset the app ships is public. It is vendored into `data/` and
`www/data/` so the app runs offline, and the scripts in this directory refresh
it from source. Anything a script downloads lands in `data/raw/`, which is
gitignored: raw inputs are never committed, only the reduced outputs are.

Some of what the app draws is not a dataset at all but seeded synthetic data
generated in R at startup. Those are listed separately below, because a figure
that looks real and is not should say so.

## Real data

| File(s) | What | Source and license | Refresh |
|---|---|---|---|
| `de_results.csv` | Differential expression, tumour vs normal: `gene, logFC, AveExpr, t, pvalue, padj` | TCGA-BRCA counts via recount3, analysed with DESeq2. TCGA open-access tier; recount3 data are freely redistributable | see "Inherited datasets" |
| `expression.csv` | DESeq2 VST expression, genes x 40 samples (20 tumour, 20 normal) | same | same |
| `metadata.csv` | sample to group (tumour / normal) | same | same |
| `mutations.csv` | Recurrent BRCA somatic variants: `chrom, pos, ref, alt, gene, protein_change, count` (hg19) | TCGA-BRCA PanCancer Atlas via the cBioPortal REST API. cBioPortal content is ODbL; the underlying TCGA data are open-access | same |
| `brca_oncoplot.csv`, `brca_clinical.csv`, `brca_lollipop.csv` | Alteration matrix (gene x sample x class) for the 967-tumour altered-driver oncoplot; clinical annotation for the full 1,080-tumour cohort (the survival page uses the 1,067 with usable follow-up); per-residue variant counts | cBioPortal REST API, study `brca_tcga_pan_can_atlas_2018`. ODbL over open-access TCGA | `Rscript data/prep/prepare-brca-cohort.R` |
| `protein_domains.csv`, `protein_ptm.csv` | Pfam domain architecture and modified residues for six BRCA drivers | InterPro (EMBL-EBI, CC0) and UniProtKB (CC BY 4.0) REST APIs | `Rscript data/prep/prepare-protein-tracks.R` |
| `sbs96_catalogue.csv`, `sbs96_signatures.csv`, `sbs96_exposures.csv` | Observed 96-context SNV catalogue, four signatures extracted de novo from it by NMF, and per-sample exposures | GDC open-access TCGA-BRCA MAFs. The trinucleotide context comes from the MAF's own `CONTEXT` column, so no reference genome is needed | `Rscript data/prep/prepare-sbs96.R` |
| `visium_spots.csv`, `visium_expr.csv`, `www/spatial/visium_he.png` | 3,798 capture spots with coordinates and graph-based cluster, a 72-gene marker panel as log1p CP10K, and the low-res H&E | 10x Genomics, Human Breast Cancer (Block A Section 1), Visium v1.1.0. CC BY 4.0 | `Rscript data/prep/prepare-visium.R` |
| `xenium_ggplot_sample.csv`, `www/data/xenium_*.{i16,json}` | 1,000,000 single-molecule transcript detections as binary blobs, plus a 40k CSV subsample for the ggplot2 side | 10x Genomics, Xenium In Situ Human Breast Cancer Rep 1, Analyzer 1.0.1. CC BY 4.0 | `Rscript data/prep/prepare-xenium.R` |
| `www/data/umap_*.{f32,i16,json}`, `umap_ggplot_sample.csv` | 584,207-cell UMAP embedding as binary blobs, plus a 40k CSV subsample | UCSC Cell Browser, Human Cell Landscape (Han et al. 2020, GSE134355). Public release; see the study for terms | `Rscript data/prep/prepare-umap.R` |
| `tahoe_perturbation.csv` | Drug x cell-line coverage matrix (cells profiled) | Tahoe-100M `obs_cell_grid` (Vevo Therapeutics / Arc Institute), aggregated with duckdb. CC BY 4.0 | `Rscript data/prep/prepare-tahoe.R <path-to-parquet>` |
| `www/data/tahoe_*.{f32,i16,json}` | 380,078-cell QC and cell-cycle sample as binary blobs | Tahoe-100M `obs_metadata`, same source and license | `Rscript data/prep/prepare-tahoe-cells.R <path-to-parquet>` |

The two Tahoe scripts read a parquet that is far too large to vendor (2.3 GB for
`obs_metadata`). Give the path as the first argument, or set `TAHOE_CELL_GRID` /
`TAHOE_OBS_METADATA`, or drop the file in `data/raw/`.

## Fetched live, never stored

| What | Where from |
|---|---|
| Protein structures (Protein page) | `https://alphafold.ebi.ac.uk/files/AF-<acc>-F1-model_v6.pdb` |
| PAE matrices (PAE page) | resolved per accession from `https://alphafold.ebi.ac.uk/api/prediction/<acc>`, which returns the current model version rather than one hardcoded here. Cached under `data/raw/` on first use |
| hg19 reference and cytobands (IGV page) | streamed by igv.js from the igv.js data server |

AlphaFold DB is CC BY 4.0.

## Seeded synthetic data

Generated deterministically in R, no files and no refresh. Each is illustrative
of a real assay's structure but is not measured data, and the About page says so
per visualization.

| Visualization | Generator | Seed |
|---|---|---|
| Gene network | `igraph::sample_sbm`, 12 modules, DrL layout | 42 |
| Hi-C contact matrix | polymer distance decay plus nested TADs and loops | 7 |
| Manhattan / QQ (GWAS) | 50k SNPs across 22 chromosomes with planted peaks | 11 |
| Single-cell ATAC coverage | pseudobulk accessibility, shared promoters plus cluster-specific enhancers | 31 |
| eQTL matrix | variant x gene effect sizes with a planted cis band | 21 |
| N-dimensional array | a 100 x 100 x 24 hyperspectral cube, Gaussian features each lighting a different channel. Written once to `www/data/ndarray.f32` by `prepare-ndarray.R` | 41 |
| Gosling genome view | GWAS-style points defined inline in the Gosling JSON spec (no file, no R generation) | none |

## Binary blob format

The UMAP, Tahoe, Xenium and ndarray pages bypass the Shiny websocket: the React
client fetches typed arrays over plain HTTP and hands them to the GPU. Half a
million points as JSON over a websocket is not a thing worth doing.

Every blob is a headerless little-endian array of one column, paired with a
`*_meta.json` sidecar that says how to read it:

- `.f32` is `Float32`, `.i16` is signed `Int16`.
- Category columns are 0-based codes into the sidecar's `levels` array, with a
  parallel `colors` array. Levels are frequency-sorted (largest first) except
  where a script fixes a semantic order.
- The sidecar's `fields` map gives, per colourable column, its `file`, display
  `label`, `levels` and `colors`.
- Xenium coordinates are additionally quantized: the sidecar's `coords.x/.y`
  carry `scale` and `offset`, and the value is `(code + 32768) * scale + offset`.
  Over a 7.5 mm section that is a 0.11 um step, finer than the instrument
  localizes a molecule to.

Both engines read the same sidecar, so the level order and palette cannot drift
between the React and ggplot2 renderings of a page.

## Inherited datasets

`de_results.csv`, `expression.csv`, `metadata.csv` and `mutations.csv` were
produced by the sibling `lifescience-shiny-gallery` project and copied here;
there is no prep script for them in this repo. Their pipelines are
`data/prep/prepare-tcga-brca.R` (recount3 to DESeq2 to VST) and
`data/prep/prepare-brca-mutations.R` (cBioPortal REST) in that repo.
