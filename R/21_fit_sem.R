# ==============================================================================
# 21_fit_sem.R  —  Structural model runner + explained variance
# ------------------------------------------------------------------------------
# Consumes the sem_* builder functions in R/12_lavaan_models.R, which take
# (outcome, ses_mode, dat) and embed the ses_block(). Outcomes used in the paper:
#   OF02_01_num_log  (most-recent donation, log)
#   OF02_02_num_log  (annual donation, log)        <- primary amount outcome
#   OF_Spender       (binary donor status)
# ==============================================================================

fit_sem <- function(sem_builder, data, outcome, ses_mode = "none",
                    estimator = "MLR", ordered = NULL, model_id = NA_character_) {
  syntax <- sem_builder(outcome = outcome, ses_mode = ses_mode, dat = data)
  if (!is.na(model_id)) {
    writeLines(syntax,
               file.path(SYNTAX_DIR, sprintf("sem_%s_%s.lav", model_id, estimator)))
  }
  tryCatch(
    lavaan::sem(syntax, data = data, estimator = estimator, ordered = ordered,
                missing = if (estimator == "MLR") "fiml" else "pairwise",
                std.lv = TRUE),
    error = function(e) e)
}

# Standardised structural path(s) + R2 for the outcome
sem_paths_r2 <- function(fit, outcome, model_id = NA_character_) {
  if (inherits(fit, "error") || !lavaan::lavInspect(fit, "converged")) {
    return(tibble::tibble(Model_ID = model_id, outcome = outcome,
                          beta_std = NA, pvalue = NA, R2 = NA, converged = FALSE))
  }
  std <- lavaan::standardizedSolution(fit)
  path <- std[std$op == "~" & std$lhs == outcome, ]
  r2 <- tryCatch(lavaan::lavInspect(fit, "rsquare")[[outcome]], error = function(e) NA)
  tibble::tibble(
    Model_ID = model_id, outcome = outcome,
    predictor = paste(path$rhs, collapse = " + "),
    beta_std = path$est.std[1], pvalue = path$pvalue[1],
    R2 = r2, converged = TRUE
  )
}

# ------------------------------------------------------------------------------
# TODO (send script): R2-difference bootstrap across models / outcomes.
# Legacy artifact: outputs/sem_debug/supplements/S20_r2_difference_bootstrap.csv
# Port your bootstrap routine here so it writes to file.path(CSV_DIR, "S20_...").
# ------------------------------------------------------------------------------
r2_difference_bootstrap <- function(...) {
  stop("r2_difference_bootstrap(): port the legacy bootstrap routine here. ",
       "See README 'Scripts still needed'.")
}
