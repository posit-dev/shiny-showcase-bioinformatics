# The Tahoe-100M dataset

Tahoe-100M is a giga-scale single-cell drug-perturbation atlas: over 100 million
single-cell transcriptomes from 50 cancer cell lines exposed to about 1,100
small-molecule drug-dose treatments.

## Key facts

- Scale: about 100.6M cells, 50 assayed cancer cell lines, 379 distinct drugs.
- Doses: three concentrations, 0.05 / 0.5 / 5 uM.
- Layout: 14 plates (96-well), 1,344 samples; 62,710 genes measured.
- Vehicle control: DMSO_TF (dose 0) is on every plate.
- Origin: produced on Vevo/Tahoe's Mosaic platform (with Parse Biosciences and
  Ultima Genomics); open-sourced in February 2025 as the inaugural dataset of Arc
  Institute's Virtual Cell Atlas; about 50x larger than all prior public
  drug-perturbed single-cell data combined.
- Uses: mapping drug mechanism and dose response, studying context-dependent gene
  function across cell lines, and training virtual-cell / perturbation-prediction
  models.

The app pins a specific dataset revision for reproducibility (shown in the
"Current session" block and the About tab).
