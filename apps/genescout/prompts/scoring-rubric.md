# Scoring & caveats rubric

How GeneScout turns gated evidence + disease-context priors into a plausibility
grade, and when the caveats stage down-ranks or vetoes a candidate. This rubric
is explicit and reviewable on purpose - it is the spine of the eventual methods
evaluation.

## Plausibility grade

Combine, per candidate:

- **Functional evidence** - predicted consequence (VEP), rarity (gnomAD),
  ClinVar research evidence.
- **Disease/pathway fit** - association strength (Open Targets) and membership in
  a context pathway (Reactome); membership in a known driver or context pathway
  raises the grade.
- **Network connectivity** *(multi-gene lists only)* - how many of the OTHER
  candidates in the list a gene interacts with at high confidence (STRING), i.e.
  whether it sits inside a connected module rather than standing alone. This is
  *cohort-relative* (its value depends on the rest of the list), so it is an
  *annotation* that only nudges a connected gene up and never penalizes an isolated
  one - an isolated candidate may simply be a novel finding. Each interaction is
  grounded evidence (a STRING edge).
- **Tissue relevance** *(when tissue(s) of interest are given)* - GTEx median
  expression in the study's tissue(s) of interest, relative to the gene's peak
  across all tissues. Expression in the relevant tissue raises the grade
  (annotation); expression only in unrelated tissues is a caveat (below).
- **Literature support** - grounded gene-level literature signal from two angles:
  the Europe PMC symbol-mention count (recall) and the PubTator3 entity-tagged
  article count (precision, so ambiguous names are not over-counted).
- **Cross-source corroboration** *(multi-source runs only)* - how many of the
  user's OWN input sources (e.g. their WES calls, DEGs, ATAC-seq hits) a gene
  appears in. This is an *evidence* signal, so breadth is rewarded: a gene
  corroborated across several of the user's sources out-ranks one that is loud in
  a single external source. It is appended only when the user provides two or more
  sources (so single-source runs are unchanged), the disease-discovery seed set is
  never counted, and the saturating normalizer caps a breadth-only gene *below*
  the High grade - a top grade still needs external evidence. Each corroboration
  is grounded provenance (`user-list:<label>`), not an external biological claim.

Grades: **High / Moderate / Low / Insufficient evidence**. "Insufficient
evidence" is a valid, first-class outcome - never inflate a grade to avoid it.

## Caveats and veto (anti-familiar-gene-bias)

Down-rank, or **veto** (force to the bottom with a stated reason), a candidate
that looks compelling but is:

- **Known artifact / FLAGS gene** - recurrent-artifact genes (e.g. TTN, MUC16).
  *Implemented (veto).* `R/caveats.R` reads the FLAGS list from `rubric.yml`
  (`caveats.flags_genes`, extended by a disease context's `flags_genes`) and sinks
  the gene to the bottom with a reason.
- **Single weak source** - one low-quality citation or a single database hit.
  *Implemented (caveat).* One present evidence signal whose normalized value is
  below `caveats.single_source.max_norm` down-weights the composite by
  `caveats.single_source.penalty`.
- **Common in gnomAD** - population frequency too high to be a plausible driver.
  *Implemented (caveat).* When the opt-in gnomAD common-LoF-frequency signal
  (`gnomad_af`) ran, a gene whose maximum predicted-loss-of-function allele
  frequency exceeds `caveats.common_variant.min_af` is down-weighted by
  `caveats.common_variant.penalty` - the general population tolerates losing the
  gene, so it is a weaker driver candidate. This is a gene-level frequency signal
  (max pLoF AF across the gene's variants), distinct from LOEUF, which is
  constraint, not frequency, and is deliberately **not** used as a substitute.
- **Unrelated-tissue-only** - the gene is expressed (GTEx) but essentially not in
  the study's tissue(s) of interest. *Implemented (caveat).* When the GTEx signal
  ran, a gene whose tissue relevance is below `caveats.unrelated_tissue.
  max_relevance` is down-weighted by `caveats.unrelated_tissue.penalty`.

Everything in this stage is deterministic and reproducible (no LLM in the ranking
path). Always record *why* a caveat or veto was applied, on the candidate itself.

## Output

For each candidate: grade, the evidence behind it (with source ids), any caveats
with reasons, and a suggested next experiment or analysis.
