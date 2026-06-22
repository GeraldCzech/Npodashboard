# ==============================================================================
# 20_fit_cfa.R  —  CFA runner
# ------------------------------------------------------------------------------
# Consumes the model functions in R/12_lavaan_models.R (e.g. cfa_bo_original(),
# cfa_fc_higher_order_original(), cfa_ro_original()) and a dataset from DATASETS.
# ==============================================================================

fit_cfa <- function(model_syntax, data, estimator = "MLR", ordered = NULL,
                    model_id = NA_character_, write_syntax = TRUE) {
  if (write_syntax && !is.na(model_id)) {
    writeLines(model_syntax,
               file.path(SYNTAX_DIR, sprintf("cfa_%s_%s.lav", model_id, estimator)))
  }
  fit <- tryCatch(
    lavaan::cfa(model_syntax, data = data, estimator = estimator,
                ordered = ordered,
                missing = if (estimator == "MLR") "fiml" else "pairwise",
                std.lv = TRUE),
    error = function(e) e)
  fit
}

tidy_fit <- function(fit, model_id = NA_character_, estimator = NA_character_,
                     stage = "CFA") {
  if (inherits(fit, "error") || is.null(fit) || !lavaan::lavInspect(fit, "converged")) {
    return(tibble::tibble(Stage = stage, Model_ID = model_id, Estimator = estimator,
                          converged = FALSE, n = NA_real_,
                          chisq = NA, df = NA, CFI = NA, TLI = NA, RMSEA = NA, SRMR = NA))
  }
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))
  tibble::tibble(
    Stage = stage, Model_ID = model_id, Estimator = estimator, converged = TRUE,
    n = lavaan::lavInspect(fit, "nobs"),
    chisq = fm[["chisq"]], df = fm[["df"]], CFI = fm[["cfi"]],
    TLI = fm[["tli"]], RMSEA = fm[["rmsea"]], SRMR = fm[["srmr"]]
  )
}

# Reliability / convergent validity for a fitted CFA
reliability_ave <- function(fit) {
  list(
    omega = tryCatch(semTools::compRelSEM(fit), error = function(e) NA),
    ave   = tryCatch(semTools::AVE(fit),         error = function(e) NA)
  )
}
