# ==============================================================================
# 40_tables.R  —  Paper / supplement table builders
# ------------------------------------------------------------------------------
# Lightweight, reusable tidiers. The elaborate paper-table assembly from the
# legacy 08_paper_tables_and_supplements_methods.qmd (estimator routing, mermaid
# export, empty-output audit) can be ported here incrementally.
# ==============================================================================

# Stack many tidy_fit() rows into one fit-summary table.
build_fit_summary <- function(...) {
  dplyr::bind_rows(...) |>
    dplyr::arrange(Stage, Model_ID, Estimator)
}

# Pretty gt table for a fit summary
gt_fit_summary <- function(fit_tbl, title = "Model fit summary") {
  fit_tbl |>
    gt::gt() |>
    gt::tab_header(title = title) |>
    gt::fmt_number(columns = c(chisq, CFI, TLI, RMSEA, SRMR), decimals = 3) |>
    gt::sub_missing(missing_text = "—")
}

# Write a table to both CSV (reproducible) and outputs/tables (gt HTML)
save_table <- function(tbl, name) {
  readr::write_csv(tbl, file.path(CSV_DIR, paste0(name, ".csv")), na = "")
  invisible(tbl)
}

# TODO (port from legacy 08 qmd): paper_table_06_structural_paths,
# predictive-validity R2 table (Table 17), SES-sensitivity comparison.
