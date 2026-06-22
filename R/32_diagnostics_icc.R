# ==============================================================================
# 32_diagnostics_icc.R  —  Clustering / non-independence (REF, organisation)
# ------------------------------------------------------------------------------
# Evaluations are nested within respondents (REF) and within organisations.
# These ICCs quantify how much single-level SEM understates dependence.
# ==============================================================================

# Simple one-way ICC for a single grouping factor (e.g. REF or organisation).
icc_simple <- function(data, response, group) {
  d <- data[, c(response, group)]
  names(d) <- c("y", "g")
  d <- d[stats::complete.cases(d), ]
  d$g <- as.factor(d$g)
  m <- lme4::lmer(y ~ 1 + (1 | g), data = d, REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))
  v_between <- vc$vcov[vc$grp == "g"]
  v_resid   <- vc$vcov[vc$grp == "Residual"]
  tibble::tibble(
    response = response, group = group,
    var_between = v_between, var_residual = v_resid,
    ICC = v_between / (v_between + v_resid),
    n_groups = nlevels(d$g), n_obs = nrow(d)
  )
}

# Crossed REF x organisation ICCs from a single cross-classified model.
icc_crossed <- function(data, response, group1 = "REF", group2 = "organisation") {
  d <- data[, c(response, group1, group2)]
  names(d) <- c("y", "g1", "g2")
  d <- d[stats::complete.cases(d), ]
  d$g1 <- as.factor(d$g1); d$g2 <- as.factor(d$g2)
  m <- lme4::lmer(y ~ 1 + (1 | g1) + (1 | g2), data = d, REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))
  v1 <- vc$vcov[vc$grp == "g1"]; v2 <- vc$vcov[vc$grp == "g2"]
  vr <- vc$vcov[vc$grp == "Residual"]; tot <- v1 + v2 + vr
  tibble::tibble(
    response = response,
    ICC_respondent = v1 / tot, ICC_organisation = v2 / tot,
    var_residual = vr, n_obs = nrow(d)
  )
}
