# Data directory

**Nothing in `raw/` or `derived/` is committed to git** (see repo `.gitignore`).

## Expected files

- `derived/daten_standardisiert.RData` — standardised workspace consumed by
  `R/10_datasets.R`. Must contain the model-ready objects `FC_BO_orig`,
  `BO_orig`, `RO_orig`, `cross_orig` plus an outcome/SES source.
- `derived/fragebogen.rds` — assembled questionnaire object
  (`qnr1`, `qnr2`, `qnr4`, `start01`, and the `*_orig` datasets). Used by the
  diagnostics that recompute from raw.
- `raw/` — the original SoSciSurvey export (never shared publicly).

## Pointing elsewhere

If the embargoed data live outside the repo:

```r
Sys.setenv(CBE_DATA_DIR = "/secure/path/to/derived")
Sys.setenv(CBE_RAW_DIR  = "/secure/path/to/raw")
```

## De-identification (before any public OSF upload)

Remove organisation names, free-text, and anything re-identifying; consider
coarsening SES and exact donation amounts. Keep an un-shared master mapping
offline.
