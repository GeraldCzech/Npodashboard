# ==============================================================================
# 33_invariance.R  —  Multi-group measurement invariance
# ------------------------------------------------------------------------------
# Configural -> metric -> scalar across a grouping variable (default OF_Spender:
# donor vs non-donor). Returns nested-model fit and DCFI/DRMSEA.
# Also a by-group reliability/AVE helper (mirrors Advanced Report Tables 13-15).
# ==============================================================================

invariance_steps <- function(model, data, group = "OF_Spender",
                              estimator = "MLR", ordered = NULL) {
  d <- data[!is.na(data[[group]]), , drop = FALSE]
  d[[group]] <- as.factor(d[[group]])

  cfg <- lavaan::cfa(model, data = d, group = group, estimator = estimator,
                     ordered = ordered)
  met <- lavaan::cfa(model, data = d, group = group, estimator = estimator,
                     ordered = ordered, group.equal = "loadings")
  sca <- lavaan::cfa(model, data = d, group = group, estimator = estimator,
                     ordered = ordered, group.equal = c("loadings", "intercepts"))

  grab <- function(fit, level) {
    fm <- lavaan::fitMeasures(fit, c("cfi", "rmsea", "chisq", "df"))
    tibble::tibble(level = level, CFI = fm[["cfi"]], RMSEA = fm[["rmsea"]],
                   chisq = fm[["chisq"]], df = fm[["df"]])
  }
  out <- dplyr::bind_rows(grab(cfg, "configural"),
                          grab(met, "metric"),
                          grab(sca, "scalar"))
  out |>
    dplyr::mutate(dCFI = CFI - dplyr::lag(CFI),
                  dRMSEA = RMSEA - dplyr::lag(RMSEA),
                  # Chen (2007): metric/scalar supported if dCFI >= -.01
                  decision = dplyr::case_when(
                    is.na(dCFI) ~ "baseline",
                    dCFI >= -0.010 ~ "supported",
                    TRUE ~ "not supported"))
}

# By-group reliability / AVE (requires semTools::compRelSEM / AVE on a grouped fit)
reliability_by_group <- function(model, data, group = "OF_Spender",
                                 estimator = "MLR", ordered = NULL) {
  d <- data[!is.na(data[[group]]), , drop = FALSE]
  d[[group]] <- as.factor(d[[group]])
  fit <- lavaan::cfa(model, data = d, group = group, estimator = estimator,
                     ordered = ordered)
  list(
    fit = fit,
    reliability = tryCatch(semTools::compRelSEM(fit), error = function(e) NA),
    ave = tryCatch(semTools::AVE(fit), error = function(e) NA)
  )
}
