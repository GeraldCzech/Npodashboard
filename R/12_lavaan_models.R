# ==============================================================================
# 01_lavaan_models.R
# Lavaan syntax registry for CFA and SEM debugging
# ==============================================================================

# The SEM builders use OUTCOME as placeholder and can include SES_z paths.
# ses_mode:
#   "none"           : OUTCOME ~ BrandEquity
#   "outcome"        : OUTCOME ~ BrandEquity + SES_z
#   "latent_outcome" : BrandEquity ~ SES_z; OUTCOME ~ BrandEquity + SES_z

ses_block <- function(be_name, outcome, ses_mode = c("none", "outcome", "latent_outcome"), dat = NULL) {
  ses_mode <- match.arg(ses_mode)
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)

  if (ses_mode != "none" && !has_ses) {
    warning("SES_z requested but not found in data. Falling back to ses_mode = 'none'.")
    ses_mode <- "none"
  }

  if (ses_mode == "none") {
    return(sprintf("%s ~ %s", outcome, be_name))
  }

  if (ses_mode == "outcome") {
    return(sprintf("%s ~ %s + SES_z", outcome, be_name))
  }

  if (ses_mode == "latent_outcome") {
    return(paste(
      sprintf("%s ~ SES_z", be_name),
      sprintf("%s ~ %s + SES_z", outcome, be_name),
      sep = "\n"
    ))
  }
}

# --- Faircloth CFA variants ----------------------------------------------------

cfa_fc_first_order_original <- function() {
  '
  FC_BR =~ FC01_01 + FC01_02 + FC01_03
  FC_BD =~ FC01_04 + FC01_05 + FC01_06
  FC_BC =~ FC02_01 + FC02_02 + FC02_03 + FC02_04 + FC02_05 + FC02_06 + FC02_07 + FC02_08
  FC_BS =~ FC02_09 + FC02_10_rev + FC02_11 + FC02_12_rev
  FC_BF =~ FC03_01 + FC03_02 + FC03_03
  FC_RC =~ TOM + SAW
  '
}

cfa_fc_higher_order_original <- function() {
  '
  FC_BR =~ FC01_01 + FC01_02 + FC01_03
  FC_BD =~ FC01_04 + FC01_05 + FC01_06
  FC_BC =~ FC02_01 + FC02_02 + FC02_03 + FC02_04 + FC02_05 + FC02_06 + FC02_07 + FC02_08
  FC_BS =~ FC02_09 + FC02_10_rev + FC02_11 + FC02_12_rev
  FC_BF =~ FC03_01 + FC03_02 + FC03_03
  FC_RC =~ TOM + SAW

  FC_BP =~ FC_BR + FC_BD
  FC_BI =~ FC_BC + FC_BS
  FC_BA =~ FC_RC + FC_BF
  FC_BE =~ FC_BP + FC_BI + FC_BA
  '
}

cfa_fc_purified_A <- function() {
  '
  FC_BR =~ FC01_01 + FC01_02 + FC01_03
  FC_BD =~ FC01_04 + FC01_05 + FC01_06
  FC_BC =~ FC02_01 + FC02_02 + FC02_03 + FC02_04 + FC02_05 + FC02_06 + FC02_07 + FC02_08
  FC_BS =~ bs*FC02_09 + bs*FC02_11
  FC_BF =~ FC03_01 + FC03_02 + FC03_03
  FC_RC =~ TOM + SAW

  FC_BP =~ FC_BR + FC_BD
  FC_BI =~ FC_BC + FC_BS
  FC_BA =~ FC_RC + FC_BF
  FC_BE =~ FC_BP + FC_BI + FC_BA
  '
}

cfa_fc_core_B <- function() {
  '
  FC_BR =~ FC01_01 + FC01_02 + FC01_03
  FC_BD =~ FC01_04 + FC01_05 + FC01_06
  FC_BF =~ FC03_01 + FC03_02 + FC03_03
  FC_RC =~ TOM + SAW

  FC_BE =~ FC_BR + FC_BD + FC_BF + FC_RC
  '
}



# --- Boenigk and Romero CFA benchmark models ---------------------------------

cfa_bo_original <- function() {
  '
  BO_TR =~ B101_01 + B101_02 + B101_03
  BO_CO =~ B102_01 + B102_02 + B102_03
  BO_BF =~ FC03_01 + FC03_02 + FC03_03
  BO_RC =~ TOM + SAW

  BO_BE =~ BO_TR + BO_CO + BO_BF + BO_RC
  '
}

cfa_ro_original <- function() {
  '
  RO_BF =~ R201_01 + R201_02 + R201_03 + R201_04
  RO_BS =~ R201_05 + R201_06 + R201_07
  RO_BI =~ R202_01 + R202_02 + R202_03 + R202_04
  RO_BW =~ R202_05 + R202_06 + R202_07 + R202_08
  RO_BD =~ R203_01 + R203_02 + R203_03 + R203_04 + R203_05
  RO_BR =~ R203_06 + R203_07 + R203_08 + R203_09
  RO_AC =~ R204_01 + R204_02 + R204_03 + R204_04
  RO_EC =~ R204_05 + R204_06 + R204_07 + R204_08 + R204_09
  RO_ID =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07

  RO_BC =~ RO_AC + RO_EC
  RO_BA =~ RO_BF + RO_BS
  RO_BP =~ RO_BD + RO_BW + RO_BR
  RO_BE =~ RO_BC + RO_BP + RO_BI + RO_BA
  '
}

# --- SEM variants --------------------------------------------------------------

sem_fc_original <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    cfa_fc_higher_order_original(),
    ses_block("FC_BE", outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_fc_purified_A <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    cfa_fc_purified_A(),
    ses_block("FC_BE", outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_fc_core_B <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    cfa_fc_core_B(),
    ses_block("FC_BE", outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_bo_original <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    '
    BO_TR =~ B101_01 + B101_02 + B101_03
    BO_CO =~ B102_01 + B102_02 + B102_03
    BO_BF =~ FC03_01 + FC03_02 + FC03_03
    BO_RC =~ TOM + SAW

    BO_BE =~ BO_TR + BO_CO + BO_BF + BO_RC
    ',
    ses_block("BO_BE", outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_ro_original <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    '
    RO_BF =~ R201_01 + R201_02 + R201_03 + R201_04
    RO_BS =~ R201_05 + R201_06 + R201_07
    RO_BI =~ R202_01 + R202_02 + R202_03 + R202_04
    RO_BW =~ R202_05 + R202_06 + R202_07 + R202_08
    RO_BD =~ R203_01 + R203_02 + R203_03 + R203_04 + R203_05
    RO_BR =~ R203_06 + R203_07 + R203_08 + R203_09
    RO_AC =~ R204_01 + R204_02 + R204_03 + R204_04
    RO_EC =~ R204_05 + R204_06 + R204_07 + R204_08 + R204_09
    RO_ID =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07

    RO_BC =~ RO_AC + RO_EC
    RO_BA =~ RO_BF + RO_BS
    RO_BP =~ RO_BD + RO_BW + RO_BR
    RO_BE =~ RO_BC + RO_BP + RO_BI + RO_BA
    ',
    ses_block("RO_BE", outcome, ses_mode, dat),
    sep = "\n"
  )
}

# --- Combined exploratory models ---------------------------------------------

sem_comb_hierarchical <- function(outcome, ses_mode = "none", dat = NULL) {
  paste(
    '
    COG_ACCESS =~ FC03_01 + FC03_02 + FC03_03 + TOM + SAW + R201_01 + R201_02 + R201_03 + R201_04
    EVAL_IMAGE =~ FC01_01 + FC01_02 + FC01_03 + FC01_04 + FC01_05 + FC01_06 +
                  R202_01 + R202_02 + R202_03 + R202_04 +
                  R203_01 + R203_02 + R203_03 + R203_04 + R203_05 +
                  R203_06 + R203_07 + R203_08 + R203_09
    REL_CORE =~ B101_01 + B101_02 + B101_03 + B102_01 + B102_02 + B102_03 +
                R204_01 + R204_02 + R204_03 + R204_04 +
                R204_05 + R204_06 + R204_07 + R204_08 + R204_09

    BRAND_EQUITY =~ COG_ACCESS + EVAL_IMAGE + REL_CORE
    ',
    ses_block("BRAND_EQUITY", outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_comb_process_chain <- function(outcome, ses_mode = "none", dat = NULL) {
  # SES in this model can stabilise INTENTION and the final outcome.
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)
  ses_lines <- switch(
    ses_mode,
    none = "",
    outcome = if (has_ses) sprintf("%s ~ INTENTION + REL_CORE + EVAL_IMAGE + COG_ACCESS + SES_z", outcome) else sprintf("%s ~ INTENTION + REL_CORE + EVAL_IMAGE + COG_ACCESS", outcome),
    latent_outcome = if (has_ses) paste("COG_ACCESS ~ SES_z", "EVAL_IMAGE ~ SES_z", "REL_CORE ~ SES_z", "INTENTION ~ SES_z", sprintf("%s ~ INTENTION + REL_CORE + EVAL_IMAGE + COG_ACCESS + SES_z", outcome), sep="\n") else sprintf("%s ~ INTENTION + REL_CORE + EVAL_IMAGE + COG_ACCESS", outcome)
  )
  if (ses_mode == "none") ses_lines <- sprintf("%s ~ INTENTION + REL_CORE + EVAL_IMAGE + COG_ACCESS", outcome)

  paste(
    '
    COG_ACCESS =~ FC03_01 + FC03_02 + FC03_03 + TOM + SAW + R201_01 + R201_02 + R201_03 + R201_04
    EVAL_IMAGE =~ FC01_01 + FC01_02 + FC01_03 + FC01_04 + FC01_05 + FC01_06 +
                  R202_01 + R202_02 + R202_03 + R202_04 +
                  R203_01 + R203_02 + R203_03 + R203_04 + R203_05 +
                  R203_06 + R203_07 + R203_08 + R203_09
    REL_CORE =~ B101_01 + B101_02 + B101_03 + B102_01 + B102_02 + B102_03 +
                R204_01 + R204_02 + R204_03 + R204_04 +
                R204_05 + R204_06 + R204_07 + R204_08 + R204_09
    INTENTION =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07

    EVAL_IMAGE ~ COG_ACCESS
    REL_CORE ~ EVAL_IMAGE + COG_ACCESS
    INTENTION ~ REL_CORE + EVAL_IMAGE + COG_ACCESS
    ',
    ses_lines,
    sep = "\n"
  )
}

sem_comb_latent_network <- function(outcome, ses_mode = "none", dat = NULL) {
  # Network model: SES can be added as direct predictor of all latent nodes and outcome.
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)
  lhs <- "COG_ACCESS + EVAL_IMAGE + REL_CORE + INTENTION"
  if (ses_mode == "none") {
    final <- sprintf("%s ~ %s", outcome, lhs)
  } else if (ses_mode == "outcome" && has_ses) {
    final <- sprintf("%s ~ %s + SES_z", outcome, lhs)
  } else if (ses_mode == "latent_outcome" && has_ses) {
    final <- paste(
      "COG_ACCESS ~ SES_z",
      "EVAL_IMAGE ~ SES_z",
      "REL_CORE ~ SES_z",
      "INTENTION ~ SES_z",
      sprintf("%s ~ %s + SES_z", outcome, lhs),
      sep = "\n"
    )
  } else {
    final <- sprintf("%s ~ %s", outcome, lhs)
  }

  paste(
    '
    COG_ACCESS =~ FC03_01 + FC03_02 + FC03_03 + TOM + SAW + R201_01 + R201_02 + R201_03 + R201_04
    EVAL_IMAGE =~ FC01_01 + FC01_02 + FC01_03 + FC01_04 + FC01_05 + FC01_06 +
                  R202_01 + R202_02 + R202_03 + R202_04 +
                  R203_01 + R203_02 + R203_03 + R203_04 + R203_05 +
                  R203_06 + R203_07 + R203_08 + R203_09
    REL_CORE =~ B101_01 + B101_02 + B101_03 + B102_01 + B102_02 + B102_03 +
                R204_01 + R204_02 + R204_03 + R204_04 +
                R204_05 + R204_06 + R204_07 + R204_08 + R204_09
    INTENTION =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07

    COG_ACCESS ~~ EVAL_IMAGE + REL_CORE + INTENTION
    EVAL_IMAGE ~~ REL_CORE + INTENTION
    REL_CORE ~~ INTENTION
    ',
    final,
    sep = "\n"
  )
}


# --- Chatzi-inspired architecture extension models ---------------------------
# These models operationalise nonprofit brand equity either as a parsimonious
# hierarchical latent construct or as a network/process architecture of mutually
# reinforcing brand perceptions. They are intended as exploratory theory-extension
# models, not as replacements for the confirmatory replication grid.

cfa_bo_first_order_network <- function() {
  '
  BO_TR =~ B101_01 + B101_02 + B101_03
  BO_CO =~ B102_01 + B102_02 + B102_03
  BO_BF =~ FC03_01 + FC03_02 + FC03_03
  BO_RC =~ TOM + SAW
  '
}

cfa_fc_core_B_first_order_network <- function() {
  '
  FC_BR =~ FC01_01 + FC01_02 + FC01_03
  FC_BD =~ FC01_04 + FC01_05 + FC01_06
  FC_BF =~ FC03_01 + FC03_02 + FC03_03
  FC_RC =~ TOM + SAW
  '
}

cfa_ro_first_order_network <- function() {
  '
  RO_BF =~ R201_01 + R201_02 + R201_03 + R201_04
  RO_BS =~ R201_05 + R201_06 + R201_07
  RO_BI =~ R202_01 + R202_02 + R202_03 + R202_04
  RO_BW =~ R202_05 + R202_06 + R202_07 + R202_08
  RO_BD =~ R203_01 + R203_02 + R203_03 + R203_04 + R203_05
  RO_BR =~ R203_06 + R203_07 + R203_08 + R203_09
  RO_AC =~ R204_01 + R204_02 + R204_03 + R204_04
  RO_EC =~ R204_05 + R204_06 + R204_07 + R204_08 + R204_09
  RO_ID =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07
  '
}

ses_network_block <- function(predictors, outcome, ses_mode = c("none", "outcome", "latent_outcome"), dat = NULL, latent_nodes = predictors) {
  ses_mode <- match.arg(ses_mode)
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)
  pred_rhs <- paste(predictors, collapse = " + ")

  if (ses_mode == "none" || !has_ses) {
    return(sprintf("%s ~ %s", outcome, pred_rhs))
  }

  if (ses_mode == "outcome") {
    return(sprintf("%s ~ %s + SES_z", outcome, pred_rhs))
  }

  paste(
    paste0(latent_nodes, " ~ SES_z", collapse = "\n"),
    sprintf("%s ~ %s + SES_z", outcome, pred_rhs),
    sep = "\n"
  )
}

sem_bo_network_predictors <- function(outcome, ses_mode = "none", dat = NULL) {
  predictors <- c("BO_BF", "BO_TR", "BO_CO", "BO_RC")
  paste(
    cfa_bo_first_order_network(),
    '# Network-like relational specification: first-order brand-equity nodes predict the criterion directly.',
    ses_network_block(predictors, outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_fc_core_B_network_predictors <- function(outcome, ses_mode = "none", dat = NULL) {
  predictors <- c("FC_BR", "FC_BD", "FC_BF", "FC_RC")
  paste(
    cfa_fc_core_B_first_order_network(),
    '# Faircloth core-B network sensitivity: no higher-order FC_BE factor.',
    ses_network_block(predictors, outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_ro_network_predictors <- function(outcome, ses_mode = "none", dat = NULL) {
  predictors <- c("RO_BF", "RO_BS", "RO_BI", "RO_BW", "RO_BD", "RO_BR", "RO_AC", "RO_EC", "RO_ID")
  paste(
    cfa_ro_first_order_network(),
    '# Romero network specification: first-order donor-facing brand nodes predict the criterion directly.',
    ses_network_block(predictors, outcome, ses_mode, dat),
    sep = "\n"
  )
}

sem_ro_process_chain <- function(outcome, ses_mode = "none", dat = NULL) {
  # Process interpretation: access/strength -> perceptions -> commitment -> intention -> behaviour.
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)
  final <- if (ses_mode == "none" || !has_ses) {
    sprintf("%s ~ RO_ID + RO_AC + RO_EC + RO_BI + RO_BR", outcome)
  } else if (ses_mode == "outcome") {
    sprintf("%s ~ RO_ID + RO_AC + RO_EC + RO_BI + RO_BR + SES_z", outcome)
  } else {
    paste(
      "RO_BF ~ SES_z",
      "RO_BS ~ SES_z",
      "RO_BI ~ SES_z",
      "RO_BW ~ SES_z",
      "RO_BD ~ SES_z",
      "RO_BR ~ SES_z",
      "RO_AC ~ SES_z",
      "RO_EC ~ SES_z",
      "RO_ID ~ SES_z",
      sprintf("%s ~ RO_ID + RO_AC + RO_EC + RO_BI + RO_BR + SES_z", outcome),
      sep = "\n"
    )
  }

  paste(
    cfa_ro_first_order_network(),
    '
    RO_BI + RO_BW + RO_BD + RO_BR ~ RO_BF + RO_BS
    RO_AC + RO_EC ~ RO_BI + RO_BR + RO_BD + RO_BW + RO_BF + RO_BS
    RO_ID ~ RO_AC + RO_EC + RO_BI + RO_BR + RO_BD + RO_BW
    ',
    final,
    sep = "\n"
  )
}

sem_hybrid_bo_ro_process <- function(outcome, ses_mode = "none", dat = NULL) {
  # Hybrid cross-sample process model connecting the Boenigk relational core to
  # Romero's identification/commitment/intention logic.
  has_ses <- !is.null(dat) && "SES_z" %in% names(dat)
  final <- if (ses_mode == "none" || !has_ses) {
    sprintf("%s ~ RO_ID + BO_CO + BO_TR + RO_BI + RO_BR", outcome)
  } else if (ses_mode == "outcome") {
    sprintf("%s ~ RO_ID + BO_CO + BO_TR + RO_BI + RO_BR + SES_z", outcome)
  } else {
    paste(
      "BO_BF ~ SES_z",
      "BO_RC ~ SES_z",
      "BO_TR ~ SES_z",
      "BO_CO ~ SES_z",
      "RO_BR ~ SES_z",
      "RO_BI ~ SES_z",
      "RO_AC ~ SES_z",
      "RO_EC ~ SES_z",
      "RO_ID ~ SES_z",
      sprintf("%s ~ RO_ID + BO_CO + BO_TR + RO_BI + RO_BR + SES_z", outcome),
      sep = "\n"
    )
  }

  paste(
    '
    BO_TR =~ B101_01 + B101_02 + B101_03
    BO_CO =~ B102_01 + B102_02 + B102_03
    BO_BF =~ FC03_01 + FC03_02 + FC03_03
    BO_RC =~ TOM + SAW

    RO_BR =~ R203_06 + R203_07 + R203_08 + R203_09
    RO_BI =~ R202_01 + R202_02 + R202_03 + R202_04
    RO_AC =~ R204_01 + R204_02 + R204_03 + R204_04
    RO_EC =~ R204_05 + R204_06 + R204_07 + R204_08 + R204_09
    RO_ID =~ R205_01 + R205_02 + R205_03 + R205_04 + R205_05 + R205_06 + R205_07

    BO_TR ~ BO_BF + BO_RC + RO_BR
    RO_BI ~ BO_BF + BO_TR + RO_BR
    RO_AC + RO_EC ~ BO_TR + BO_CO + RO_BI + RO_BR
    BO_CO ~ BO_TR + RO_BI + RO_AC + RO_EC
    RO_ID ~ BO_CO + BO_TR + RO_BI + RO_AC + RO_EC
    ',
    final,
    sep = "\n"
  )
}

CFA_REGISTRY <- list(
  fc_first_order_original = list(fun = cfa_fc_first_order_original, data = "fc", family = "Faircloth", role = "First-order original"),
  fc_higher_order_original = list(fun = cfa_fc_higher_order_original, data = "fc", family = "Faircloth", role = "Higher-order original"),
  fc_purified_A = list(fun = cfa_fc_purified_A, data = "fc", family = "Faircloth", role = "Purified FC02 test"),
  fc_core_B = list(fun = cfa_fc_core_B, data = "fc", family = "Faircloth", role = "Core without FC02 block"),
  bo_original = list(fun = cfa_bo_original, data = "bo", family = "Boenigk", role = "Relational benchmark"),
  ro_original = list(fun = cfa_ro_original, data = "ro", family = "Romero", role = "Donor-based multidimensional benchmark"),
  bo_first_order_network = list(fun = cfa_bo_first_order_network, data = "bo", family = "Boenigk", role = "Exploratory first-order relational network"),
  fc_core_B_first_order_network = list(fun = cfa_fc_core_B_first_order_network, data = "fc", family = "Faircloth", role = "Exploratory first-order core-B network"),
  ro_first_order_network = list(fun = cfa_ro_first_order_network, data = "ro", family = "Romero", role = "Exploratory first-order donor-brand network")
)

SEM_REGISTRY <- list(
  fc_original = list(fun = sem_fc_original, data = "fc", be = "FC_BE"),
  fc_purified_A = list(fun = sem_fc_purified_A, data = "fc", be = "FC_BE"),
  fc_core_B = list(fun = sem_fc_core_B, data = "fc", be = "FC_BE"),
  bo_original = list(fun = sem_bo_original, data = "bo", be = "BO_BE"),
  ro_original = list(fun = sem_ro_original, data = "ro", be = "RO_BE"),
  comb_hierarchical = list(fun = sem_comb_hierarchical, data = "cross", be = "BRAND_EQUITY"),
  comb_process_chain = list(fun = sem_comb_process_chain, data = "cross", be = NA_character_),
  comb_latent_network = list(fun = sem_comb_latent_network, data = "cross", be = NA_character_),
  bo_network_predictors = list(fun = sem_bo_network_predictors, data = "bo", be = NA_character_),
  fc_core_B_network_predictors = list(fun = sem_fc_core_B_network_predictors, data = "fc", be = NA_character_),
  ro_network_predictors = list(fun = sem_ro_network_predictors, data = "ro", be = NA_character_),
  ro_process_chain = list(fun = sem_ro_process_chain, data = "ro", be = NA_character_),
  hybrid_bo_ro_process = list(fun = sem_hybrid_bo_ro_process, data = "cross", be = NA_character_)
)
