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

## Pipeline status

### ✅ Supplied and wired (17 scripts in `R/pipeline/`)

| Script | Provides |
|---|---|
| `external_Sources.R` | `org_synonyme` (26 NPOs + fuzzy synonyms), `fields`, `skalen`, `skalen_SEM`, `zielvariablen`, `source_links`, `at03_labels` |
| `settings.R` | `config` (validation thresholds, EFA params, output paths; API URL via env var) |
| `load_data.R` | `load_data()` (API/sav/rds/csv) |
| `validate_data.R` | `validate_data()`, `detect_alternating()` |
| `split_validated_data.R` | `split_validated_data()` → start01/qnr1/qnr2/qnr4/qnr5 |
| `recode_reversed_items.R` | `recode_reversed_items()`, `reverse_specific_items()`, `drop_avector()` |
| `berechne_alle_skalen.R` | `berechne_skalen_rekursiv()` |
| `extract_awareness_org.R` | `extract_start_awareness()` v1 (exact match, overwritten by v2) |
| `extract_start_awareness_org.R` | `extract_start_awareness()` v2 (fuzzy), `get_start_awareness_data()` |
| `merge_awareness_data.R` | `merge_awareness_data()` → TOM/SAW/BA_A/BA_T into qnr1/qnr2 |
| `extract_donation_data.R` | `extract_donation_data()` → OF_Spender, OF_last, OF_2024 |
| `extract_spendenbetrag.R` | `extract_spendenbetrag()`, `extract_spenden_from_columns()`, `spenden_kategorien()`, `spenden_kategorien1()` |
| `join_followup_questionnaires.R` | `join_followup_cross_questionnaires()` → fragebogen\$cross; `harmonize_df()` |
| `join_followup_fallbacks.R` | `combine_main_questionnaires_with_supplements()` → fragebogen\$FC_BO / \$RO |
| `attach_sociodemographics.R` | `attach_sociodemographics()` → SD01/03/11/14/16/21 + EW02_* |
| `fields.R` | shim sourcing external_Sources.R |
| `skalen_liste.R` | exposes `skalen_SEM` as `skalen_liste` |

### ⏳ Still needed

| Script | Provides | Impact if absent |
|---|---|---|
| `awareness_utils.R` | `add_awareness()`, `z_standardisieren()` | awareness on full frame skipped (fallback: per-qnr) |
| `valid_bayes_score.R` | `bayes_valid_score()` | Bayes scoring skipped |
| `combine_posterior.R` | posterior combination | step skipped |
| Bootstrap routine | `r2_difference_bootstrap()` | S20 CSV not generated |

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
