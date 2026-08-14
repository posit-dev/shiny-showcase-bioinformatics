# About page: methodology, sources, scoring, configuration --------------------

about_page <- bslib::page_fluid(
  bslib::card(
    bslib::card_header("About Gene List Builder"),
    bslib::card_body(
      markdown(
        "
A lightweight tool for building a curated gene list for a disease. It replaces
the manual, API-by-hand curation workflow with a reproducible pipeline:

1. **Resolve** a disease name to an ontology term (EFO/MONDO) via Open Targets.
2. **Query** multiple gene-disease-association sources.
3. **Aggregate + dedupe** results into a common schema (one row per gene).
4. **Rank** transparently with tunable, source-weighted scoring.
5. **Curate** the final list with an AI agent (Google Gemini), on demand.
6. **Export** the gene list (CSV) and a run report (Markdown).

### Sources

**Evidence sources** (add candidate genes and count toward coverage):

- **Open Targets** - overall target-disease association score (0-1). Also
  resolves the disease name to an ontology term.
- **PanelApp** (Genomics England) - expert-curated diagnostic gene panels;
  green/amber confidence mapped to a score.
- **DISEASES** (Jensen Lab) - text-mined + curated disease-gene associations.
- **ClinVar** - genes carrying pathogenic / likely-pathogenic variants for the
  disease (via NCBI E-utilities).
- **DGIdb** - number of known drug-gene interactions (druggability).

**Annotation sources** (prioritizers; score genes but do *not* inflate the
multi-source coverage bonus):

- **gnomAD constraint** - loss-of-function intolerance (LOEUF); more constrained
  genes score higher.
- **Pharos** - Target Development Level (Tclin > Tchem > Tbio > Tdark).

The source registry is extensible: adding DisGeNET, OMIM, GWAS Catalog, or other
databases is a matter of writing one adapter that returns the common schema.

### Scoring

For each gene:

```
weighted_evidence = sum over sources of weight * normalized_score
coverage_factor   = 1 + bonus * (n_sources - 1) / (n_total - 1)
combined_score    = weighted_evidence * coverage_factor
```

Open Targets/PanelApp scores pass through (already 0-1); count-based scores
(DISEASES, ClinVar, DGIdb) are rank-normalized; gnomAD is rank-normalized so
lower LOEUF scores higher. Annotation sources contribute to the weighted score
but `n_sources`/coverage count evidence sources only. Weights and the
multi-source bonus are tunable in the sidebar.

### AI curation

The AI step is optional. With a `GEMINI_API_KEY` set, Gemini selects the final
panel from the ranked candidates with a rationale per gene; hallucinated symbols
are dropped. Without a key (or on any error) it falls back to the top genes by
combined rank, so the app stays fully functional.
"
      ),
      uiOutput("about_key_status")
    )
  )
)
