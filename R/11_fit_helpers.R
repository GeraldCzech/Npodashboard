# ==============================================================================
# R/11_fit_helpers.R  —  SEM registry, data coercion, binary-outcome detection
# ==============================================================================
# Replaces the legacy scripts/sem_debug/02_fit_helpers.R.
# Must be sourced AFTER R/12_lavaan_models.R (needs the sem_* builders).
#
# Provides:
#   SEM_REGISTRY        named list: model_id -> list(fun, be_name, data_key)
#   coerce_sem_data()   clean a dataset for lavaan (types, avector, log outcomes)
#   is_binary_outcome() TRUE iff an outcome should be treated as ordered/probit
# ==============================================================================

# ------------------------------------------------------------------
# SEM_REGISTRY
# Each entry:
#   fun      — sem builder function(outcome, ses_mode, dat) -> lavaan syntax
#   be_name  — name of the higher-order Brand Equity latent variable
#   data_key — which DATASETS slot this model belongs to ("fc","bo","ro","cross")
# ------------------------------------------------------------------
SEM_REGISTRY <- list(
  fc_original   = list(fun = sem_fc_original,   be_name = "FC_BE", data_key = "fc"),
  fc_purified_A = list(fun = sem_fc_purified_A, be_name = "FC_BE", data_key = "fc"),
  fc_core_B     = list(fun = sem_fc_core_B,     be_name = "FC_BE", data_key = "fc"),
  bo_original   = list(fun = sem_bo_original,   be_name = "BO_BE", data_key = "bo"),
  ro_original   = list(fun = sem_ro_original,   be_name = "RO_BE", data_key = "ro")
)

# ------------------------------------------------------------------
# coerce_sem_data()
# Prepare a dataset for lavaan:
#   - strip haven_labelled labels → numeric
#   - remove avector class
#   - ensure log-outcomes exist (create from raw if absent)
#   - return a plain data.frame (lavaan prefers base over tibble)
# ------------------------------------------------------------------
coerce_sem_data <- function(dat) {
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  dat <- as.data.frame(dat)

  for (nm in names(dat)) {
    x <- dat[[nm]]
    # Strip haven_labelled
    if (inherits(x, c("haven_labelled", "labelled"))) {
      x <- haven::zap_labels(x)
    }
    # Strip avector
    if (inherits(x, "avector")) {
      class(x) <- setdiff(class(x), "avector")
    }
    # Strip vctrs / ordered factors that lavaan can't handle as numeric
    if (inherits(x, "vctrs_vctr") && !is.factor(x)) {
      x <- unclass(x); attributes(x) <- NULL
    }
    dat[[nm]] <- x
  }

  # Ensure log-transformed outcomes (create from raw if absent)
  if (!"OF02_01_num_log" %in% names(dat) && "OF02_01_num" %in% names(dat))
    dat$OF02_01_num_log <- log1p(suppressWarnings(as.numeric(dat$OF02_01_num)))
  if (!"OF02_02_num_log" %in% names(dat) && "OF02_02_num" %in% names(dat))
    dat$OF02_02_num_log <- log1p(suppressWarnings(as.numeric(dat$OF02_02_num)))

  # Ensure OF_Spender is integer 0/1 (not factor or logical)
  if ("OF_Spender" %in% names(dat)) {
    x <- dat$OF_Spender
    if (is.logical(x)) x <- as.integer(x)
    if (is.factor(x))  x <- as.integer(as.character(x))
    ux <- sort(unique(stats::na.omit(as.numeric(x))))
    if (length(ux) == 2) {
      x <- dplyr::case_when(
        is.na(x)          ~ NA_integer_,
        as.numeric(x) == ux[[1]] ~ 0L,
        as.numeric(x) == ux[[2]] ~ 1L,
        TRUE ~ NA_integer_
      )
    }
    dat$OF_Spender <- as.integer(x)
  }

  dat
}

# ------------------------------------------------------------------
# is_binary_outcome()
# Returns TRUE when the outcome should be estimated with an ordered
# (probit) link rather than OLS (i.e., WLSMV rather than MLR).
# Primary criterion: the outcome is OF_Spender.
# Fallback: if the column exists and has only 2 distinct non-NA values.
# ------------------------------------------------------------------
is_binary_outcome <- function(outcome, dat = NULL) {
  if (outcome == "OF_Spender") return(TRUE)
  if (!is.null(dat) && outcome %in% names(dat)) {
    vals <- unique(stats::na.omit(as.numeric(dat[[outcome]])))
    return(length(vals) <= 2)
  }
  FALSE
}

message("R/11_fit_helpers.R loaded — SEM_REGISTRY: ",
        paste(names(SEM_REGISTRY), collapse = ", "))
