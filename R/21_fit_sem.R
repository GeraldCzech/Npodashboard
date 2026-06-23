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

  # Coerce data types (OF_Spender logical -> 0L/1L, log-outcomes as numeric)
  if (exists("coerce_sem_data", mode = "function")) {
    data <- coerce_sem_data(data)
  }

  # For WLSMV: ordered= must list the Likert indicators from the model syntax
  # (not just the outcome name). Use extract_observed_vars() — the lavaan-parser
  # based approach from 11_fit_helpers.R — then exclude continuous log-outcomes.
  CONTINUOUS_OUTCOMES <- c("OF02_01_num_log", "OF02_02_num_log",
                           "OF02_01_num",     "OF02_02_num")
  if (is_wls_estimator_local(estimator)) {
    ordered <- tryCatch({
      if (exists("extract_observed_vars", mode = "function")) {
        all_ov <- extract_observed_vars(syntax)
      } else {
        # Minimal fallback via lavaan's own parser
        pm  <- lavaan::lavParseModelString(syntax)
        all_ov <- unique(c(pm$lhs[pm$op != "~1"], pm$rhs[pm$op != "~1"]))
        all_ov <- all_ov[nzchar(all_ov) & !grepl("^[0-9]", all_ov)]
      }
      # Keep only vars present in data; drop continuous log-outcomes
      all_ov <- intersect(all_ov, names(data))
      setdiff(all_ov, CONTINUOUS_OUTCOMES)
    }, error = function(e) {
      warning("ordered= extraction failed (", e$message, ") — using ordered=NULL")
      NULL
    })
  }

  missing_method <- if (is_wls_estimator_local(estimator)) "pairwise" else "fiml"

  if (!is.na(model_id)) {
    writeLines(syntax,
               file.path(SYNTAX_DIR, sprintf("sem_%s_%s.lav", model_id, estimator)))
  }
  tryCatch(
    lavaan::sem(syntax, data = data, estimator = estimator, ordered = ordered,
                missing = missing_method, std.lv = TRUE),
    error = function(e) e)
}

# Local helper — avoids hard dependency on 11_fit_helpers.R
is_wls_estimator_local <- function(est) {
  toupper(est) %in% c("WLSMV","WLS","WLSM","DWLS","ULS")
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
# r2_difference_bootstrap()
# Thin wrapper around R/22_bootstrap_r2.R (reviewer-grade paired nonparametric
# bootstrap of pairwise ΔR² between architectures).
#
# Configure via environment before calling:
#   BOOT_B        <- 1000L            # number of replicates
#   BOOT_SEED     <- 20260602L        # reproducibility seed
#   BOOT_MODELS   <- c("bo_original","fc_core_B","ro_original")
#   BOOT_OUTCOMES <- c("OF02_01_num_log","OF02_02_num_log","OF_Spender")
#   BOOT_NCORES   <- parallel::detectCores() - 1L
#   BOOT_BCA      <- FALSE            # BCa off by default (slow jackknife)
#
# Results are written to outputs/csv/supplements/S20_r2_difference_bootstrap_*.csv
# and returned invisibly as a list(summary = <tibble>, long = <tibble>).
# ------------------------------------------------------------------------------
r2_difference_bootstrap <- function(
    B       = 1000L,
    seed    = 20260602L,
    models  = c("bo_original", "fc_core_B", "ro_original"),
    outcomes = c("OF02_01_num_log", "OF02_02_num_log", "OF_Spender"),
    ncores  = max(1L, parallel::detectCores() - 1L),
    bca     = FALSE,
    ordered_items = FALSE
) {
  # Set configuration variables that 22_bootstrap_r2.R reads via get0()
  BOOT_B             <<- as.integer(B)
  BOOT_SEED          <<- as.integer(seed)
  BOOT_MODELS        <<- models
  BOOT_OUTCOMES      <<- outcomes
  BOOT_NCORES        <<- as.integer(ncores)
  BOOT_BCA           <<- bca
  BOOT_ORDERED_ITEMS <<- ordered_items

  message("Starting R² difference bootstrap (B=", B, ", models: ",
          paste(models, collapse=", "), ")...")
  source(here::here("R", "22_bootstrap_r2.R"), encoding = "UTF-8", local = FALSE)

  # Collect outputs written by the script
  supp_dir <- file.path(CSV_DIR, "supplements")
  summary_file <- file.path(supp_dir, "S20_r2_difference_bootstrap_all_levels.csv")
  long_file    <- file.path(supp_dir, "S20_r2_difference_bootstrap_long_r2.csv")

  result <- list(
    summary = if (file.exists(summary_file)) readr::read_csv(summary_file, show_col_types = FALSE) else NULL,
    long    = if (file.exists(long_file))    readr::read_csv(long_file,    show_col_types = FALSE) else NULL
  )
  invisible(result)
}
