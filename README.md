# Charity Brand-Equity Architectures — Research Compendium

Reproducible compendium for the within-dataset comparison of three nonprofit
brand-equity architectures (Faircloth 2005; Boenigk & Becker 2016; Ríos Romero
et al. 2023) on an Austrian dataset, supporting the AMJ Special Issue submission
*"Do Charity Brand Dashboards Deliver?"*.

## Pipeline at a glance

```
RAW (SoSciSurvey export)
   │   R/90_load_data_pipeline.R   (legacy main_EURNOVA.R — scripts being supplied)
   ▼
fragebogen.rds  +  daten_standardisiert.RData        ── data/derived/
   │   R/10_datasets.R   → builds DATASETS = list(fc, bo, ro, cross)
   │                       context vars: SES_z, OF02_01_num_log,
   │                       OF02_02_num_log, OF_Spender
   ▼
R/12_lavaan_models.R   (model registry: cfa_fc_*, cfa_bo_original,
                        cfa_ro_original, sem_*_original with ses_block)
   │
   ├── analysis/01_main_analysis.qmd        EFA → CFA → SEM → criterion R²
   └── analysis/02_supplements_addendum.qmd HTMT, CMV, ICC, invariance,
                                            deviations, robustness
```

## Repository layout

| Path | Purpose |
|---|---|
| `R/00_paths.R` | Single portable path config (override with `CBE_DATA_DIR`). |
| `R/01_packages.R` | Package loader (sync with `renv.lock`). |
| `R/10_datasets.R` | Builds the `DATASETS` registry (refactor of legacy `00_config_data.R`). |
| `R/12_lavaan_models.R` | lavaan model registry (your working file, intact). |
| `R/20_fit_cfa.R`, `R/21_fit_sem.R` | CFA / SEM runners + tidy fit, paths, R². |
| `R/30…33` | HTMT, CMV (Harman + CFA-CMF), ICC, invariance — functional. |
| `R/40_tables.R` | Table builders (paper-table logic to be ported from legacy qmd). |
| `R/90_load_data_pipeline.R` | RAW → fragebogen build (stub; scripts incoming). |
| `analysis/` | The two Quarto documents (main + supplement). |
| `data/raw`, `data/derived` | **Git-ignored.** Embargoed / identifiable data live here. |
| `outputs/` | **Git-ignored** generated CSVs, figures, tables, rendered docs. |
| `osf/` | Registration link + deviations note. |
| `scripts/legacy/` | Original `main_EURNOVA.R` and `00_config_data.R` for reference. |

## How to run

```r
# 1. Put the derived data where the compendium expects it:
#    data/derived/daten_standardisiert.RData   (+ fragebogen.rds)
#    or point elsewhere:  Sys.setenv(CBE_DATA_DIR = "/path/to/derived")

# 2. Restore packages (once renv is initialised):
renv::restore()

# 3. Render:
quarto::quarto_render("analysis/01_main_analysis.qmd")
quarto::quarto_render("analysis/02_supplements_addendum.qmd")
```

For a fully reproducible run, set the context source explicitly before the
datasets module selects one heuristically:

```r
Sys.setenv(CBE_CONTEXT_SOURCE = "fragebogen$qnr1")   # adjust to the real source
```

## Data availability & de-identification

Raw and derived survey data are **not** committed (see `.gitignore`). Before any
file goes into a public OSF repository, remove direct identifiers and anything
that could re-identify partner organisations or respondents (organisation names,
SES detail, free-text, exact amounts where risky). For peer review, share the
anonymised OSF **view-only** link only.

## Scripts still needed (you will supply incrementally)

These complete `R/90_load_data_pipeline.R` (RAW → fragebogen). Drop them under
`R/pipeline/` keeping the legacy names:

- data: `external_Sources.R`, `load_data.R`, `validate_data.R`,
  `split_validated_data.R`, `recode_reversed_items.R`,
  `attach_sociodemographics.R`, `berechne_alle_skalen.R`,
  `merge_awareness_data.R`, `combine_posterior.R`
- extract: `join_followup_questionnaires.R`, `extract_donation_data.R`,
  `join_followup_fallbacks.R`, `extract_spendenbetrag.R`,
  `extract_start_awareness_org.R`
- modules: `match_org_code.R`, `z_standardisieren.R`, `awareness_utils.R`,
  `run_cfa.R`, `cfa_model_builder.R`, `io_utils.R`, `analysis_container.R`
- analysis: `efa_analysis.R`, `cfa_analysis.R`, `summarize_cfa_results.R`,
  `run_sem_analysis.R`, `run_sem_model.R`, `run_sem_model_template.R`
- diagnostics: `valid_bayes_score.R`
- export: `save_outputs.R`, `export_cfa_report.R`
- `config/settings.R`, `scripts/packages.R`

Also useful: the R²-difference bootstrap routine (→ `R/21_fit_sem.R`,
`r2_difference_bootstrap()`), and the construct→item maps for Faircloth and
Romero (→ HTMT/CMV chunks in the supplement).

## Turn this folder into a GitHub repo

```bash
cd charity-brand-equity
git init -b main
git add .
git commit -m "Initial research compendium scaffold"

# with the GitHub CLI (creates the remote and pushes in one step):
gh repo create charity-brand-equity --private --source=. --push

# or manually, after creating an empty repo on github.com:
git remote add origin git@github.com:<you>/charity-brand-equity.git
git push -u origin main
```

## License

Code under MIT (see `LICENSE`); manuscript text/figures suggested under CC-BY-4.0.
Adjust to your institution's and the journal's requirements.
