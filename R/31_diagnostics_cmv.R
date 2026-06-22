# ==============================================================================
# 31_diagnostics_cmv.R  —  Common method variance
# ------------------------------------------------------------------------------
# (a) harman_single_factor(): first unrotated component variance (PCA/PAF).
#     NOTE: deliberately weak diagnostic; reported for completeness only.
# (b) cmv_cfa_comparison(): theoretical trait model vs single-factor vs latent
#     common-method-factor (CLF). The single-factor contrast is the informative
#     one; the CLF model may not converge for parsimonious models.
# ==============================================================================

# (a) Harman ------------------------------------------------------------------
harman_single_factor <- function(data, items, label = NA_character_) {
  X <- data[, items, drop = FALSE]
  X <- X[stats::complete.cases(X), , drop = FALSE]
  pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
  var_explained <- (pca$sdev^2) / sum(pca$sdev^2)
  first <- 100 * var_explained[[1]]
  tibble::tibble(
    architecture = label,
    n_cases = nrow(X),
    n_items = length(items),
    harman_first_var_pct = first,
    interpretation = ifelse(first >= 50, "problematic (>= 50%)", "acceptable (< 50%)")
  )
}

# (b) CFA-based comparison -----------------------------------------------------
# trait_model: lavaan measurement syntax (the theoretical structure).
# estimator:   "MLR" (continuous) or "WLSMV" (ordinal).
# items:       character vector of all indicators (for single-factor + CLF).
# run_clf:     latent common-method-factor (extra factor loading on all items).
cmv_cfa_comparison <- function(trait_model, data, items,
                               estimator = "MLR", ordered = NULL,
                               run_clf = FALSE) {
  fit_one <- function(model, tag) {
    out <- tryCatch(
      lavaan::cfa(model, data = data, estimator = estimator, ordered = ordered,
                  missing = if (estimator == "MLR") "fiml" else "pairwise"),
      error = function(e) e)
    if (inherits(out, "error") || !lavaan::lavInspect(out, "converged")) {
      return(tibble::tibble(model = tag, status = "Failed/No Convergence",
                            chisq = NA, df = NA, pvalue = NA, CFI = NA, TLI = NA,
                            RMSEA = NA, SRMR = NA))
    }
    fm <- lavaan::fitMeasures(out, c("chisq", "df", "pvalue", "cfi", "tli",
                                     "rmsea", "srmr"))
    tibble::tibble(model = tag, status = "Converged",
                   chisq = fm[["chisq"]], df = fm[["df"]], pvalue = fm[["pvalue"]],
                   CFI = fm[["cfi"]], TLI = fm[["tli"]],
                   RMSEA = fm[["rmsea"]], SRMR = fm[["srmr"]])
  }

  single_model <- paste0("G =~ ", paste(items, collapse = " + "))

  rows <- list(
    fit_one(trait_model, "Theoretical Trait Model"),
    fit_one(single_model, "Single-Factor Model")
  )

  if (run_clf) {
    clf_model <- paste0(trait_model, "\n  CMF =~ ", paste(items, collapse = " + "))
    rows <- c(rows, list(fit_one(clf_model, "Latent Common Method Factor Model")))
  }

  dplyr::bind_rows(rows)
}
