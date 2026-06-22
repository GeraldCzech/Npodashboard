# ==============================================================================
# R/11_fit_helpers.R  (compendium port of legacy 02_fit_helpers.R)
# Paths adapted: SEM_DEBUG_OUT -> OUT_DIR/CSV_DIR from R/00_paths.R.
# SEM_REGISTRY added (needed by R/22_bootstrap_r2.R; not in legacy original).
# Everything else is the original 02_fit_helpers.R content, unchanged.
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(lavaan)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
  if (requireNamespace("digest", quietly = TRUE)) {
    library(digest)
  }
})

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (!is.null(x)) x else y
}

# ==============================================================================
# SEM_REGISTRY  (added for compendium — not in legacy 02_fit_helpers.R)
# Maps model IDs used by the bootstrap (R/22_bootstrap_r2.R) to their sem_*
# builder functions from R/12_lavaan_models.R.
#   fun      — builder function(outcome, ses_mode, dat) -> lavaan syntax string
#   be_name  — higher-order Brand Equity latent variable name
#   data_key — which DATASETS slot (fc / bo / ro / cross)
# Must be sourced AFTER R/12_lavaan_models.R.
# ==============================================================================
SEM_REGISTRY <- list(
  fc_original   = list(fun = sem_fc_original,   be_name = "FC_BE", data_key = "fc"),
  fc_purified_A = list(fun = sem_fc_purified_A, be_name = "FC_BE", data_key = "fc"),
  fc_core_B     = list(fun = sem_fc_core_B,     be_name = "FC_BE", data_key = "fc"),
  bo_original   = list(fun = sem_bo_original,   be_name = "BO_BE", data_key = "bo"),
  ro_original   = list(fun = sem_ro_original,   be_name = "RO_BE", data_key = "ro")
)


# Portable path: use OUT_DIR from R/00_paths.R if sourced, else fallback.
if (!exists("OUT_DIR", inherits = TRUE)) {
  if (!exists("SEM_DEBUG_OUT", inherits = TRUE)) {
    SEM_DEBUG_OUT <- here::here("outputs")
  }
} else {
  SEM_DEBUG_OUT <- OUT_DIR
}

# Sub-directories (mirrors legacy layout but rooted at OUT_DIR)
dir.create(SEM_DEBUG_OUT,                    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SEM_DEBUG_OUT, "csv"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SEM_DEBUG_OUT, "fits"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SEM_DEBUG_OUT, "syntax"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SEM_DEBUG_OUT, "logs"),   recursive = TRUE, showWarnings = FALSE)

if (!exists("write_debug_csv", mode = "function")) {
  write_debug_csv <- function(x, filename) {
    readr::write_csv(as_tibble(x), file.path(SEM_DEBUG_OUT, "csv", filename), na = "")
    invisible(x)
  }
}

# Caching control. Set SEM_DEBUG_FORCE_REFIT <- TRUE before sourcing scripts if a full rerun is required.
SEM_DEBUG_USE_CACHE <- get0("SEM_DEBUG_USE_CACHE", ifnotfound = TRUE, inherits = TRUE)
SEM_DEBUG_FORCE_REFIT <- get0("SEM_DEBUG_FORCE_REFIT", ifnotfound = FALSE, inherits = TRUE)

safe_hash <- function(x) {
  x <- paste(as.character(x), collapse = "\n")
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(x, algo = "xxhash64"))
  }
  paste0("len", nchar(x), "_sum", sum(utf8ToInt(x)))
}

normalise_fit_types <- function(x) {
  x <- tibble::as_tibble(x)
  numeric_cols <- c("n", "CFI", "TLI", "RMSEA", "SRMR", "Chi2", "df", "p", "Elapsed_sec")
  for (v in intersect(numeric_cols, names(x))) {
    x[[v]] <- suppressWarnings(as.numeric(x[[v]]))
  }
  integer_cols <- c("Row")
  for (v in intersect(integer_cols, names(x))) {
    x[[v]] <- suppressWarnings(as.integer(x[[v]]))
  }
  logical_cols <- c("converged")
  for (v in intersect(logical_cols, names(x))) {
    x[[v]] <- dplyr::case_when(
      as.character(x[[v]]) %in% c("TRUE", "True", "true", "1") ~ TRUE,
      as.character(x[[v]]) %in% c("FALSE", "False", "false", "0") ~ FALSE,
      TRUE ~ NA
    )
  }
  character_cols <- c("Model_ID", "Outcome", "SES_Mode", "Estimator", "Refit", "Error", "Cache", "Syntax_Hash", "Data_Hash", "Status")
  for (v in intersect(character_cols, names(x))) {
    x[[v]] <- as.character(x[[v]])
  }
  x
}

read_debug_csv_safe <- function(path) {
  out <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  normalise_fit_types(out)
}

bind_debug_csv_files <- function(files) {
  if (length(files) == 0) return(tibble::tibble())
  purrr::map_dfr(files, read_debug_csv_safe) %>% normalise_fit_types()
}

# Strong cache validation. A cached fit is used only if the fit summary belongs
# to the current syntax/estimator and does not represent a skipped/error row.
# This prevents stale skipped/error CSVs from blocking a later valid run.
SEM_DEBUG_CACHE_ERRORS <- get0("SEM_DEBUG_CACHE_ERRORS", ifnotfound = FALSE, inherits = TRUE)

cache_available <- function(rds_path, csv_path = NULL,
                            expected_syntax_hash = NULL,
                            expected_estimator = NULL) {
  if (!isTRUE(SEM_DEBUG_USE_CACHE) || isTRUE(SEM_DEBUG_FORCE_REFIT)) return(FALSE)
  if (!file.exists(rds_path)) return(FALSE)
  if (is.null(csv_path)) return(TRUE)
  if (!file.exists(csv_path)) return(FALSE)

  x <- tryCatch(read_debug_csv_safe(csv_path), error = function(e) tibble::tibble())
  if (nrow(x) == 0) return(FALSE)
  x <- x[nrow(x), , drop = FALSE]

  if (!is.null(expected_syntax_hash) && "Syntax_Hash" %in% names(x)) {
    old_hash <- x$Syntax_Hash[[1]]
    if (!is.na(old_hash) && nzchar(old_hash) && !identical(old_hash, expected_syntax_hash)) {
      message("Ignoring stale cache because syntax hash changed: ", basename(csv_path))
      return(FALSE)
    }
  }

  if (!is.null(expected_estimator) && "Estimator" %in% names(x)) {
    old_est <- toupper(as.character(x$Estimator[[1]]))
    if (!is.na(old_est) && nzchar(old_est) && old_est != toupper(expected_estimator)) {
      message("Ignoring stale cache because estimator changed: ", basename(csv_path))
      return(FALSE)
    }
  }

  status <- if ("Status" %in% names(x)) tolower(as.character(x$Status[[1]])) else ""
  err <- if ("Error" %in% names(x)) as.character(x$Error[[1]]) else NA_character_
  conv <- if ("converged" %in% names(x)) x$converged[[1]] else NA

  has_error <- !is.na(err) && nzchar(err) && !identical(err, "NA")
  is_skipped <- !is.na(status) && status %in% c("skipped", "skip")
  not_converged <- !is.na(conv) && !isTRUE(conv)

  if (!isTRUE(SEM_DEBUG_CACHE_ERRORS) && (has_error || is_skipped || not_converged)) {
    message("Ignoring stale failed/skipped cache: ", basename(csv_path))
    return(FALSE)
  }

  TRUE
}


load_cached_fit <- function(rds_path, label = basename(rds_path)) {
  message("Using cached lavaan result: ", label)
  tryCatch(readRDS(rds_path), error = function(e) {
    warning("Could not read cached fit: ", rds_path, " | ", conditionMessage(e))
    NULL
  })
}

# ------------------------------------------------------------------------------
# General utilities
# ------------------------------------------------------------------------------

safe_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("[^A-Za-z0-9_\\-\\.]", "_") |>
    stringr::str_replace_all("\\.+", ".") |>
    stringr::str_replace_all("_+", "_")
}

clean_binary01_local <- function(x) {
  if (inherits(x, "haven_labelled") && requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(as.integer(x))

  y <- as.character(x)
  dplyr::case_when(
    is.na(y) ~ NA_integer_,
    y %in% c("1", "1L", "TRUE", "True", "true", "yes", "Yes", "YES", "Ja", "ja") ~ 1L,
    y %in% c("0", "0L", "FALSE", "False", "false", "no", "No", "NO", "Nein", "nein") ~ 0L,
    suppressWarnings(!is.na(as.numeric(y))) ~ as.integer(as.numeric(y) > 0),
    TRUE ~ NA_integer_
  )
}

coerce_sem_data <- function(dat) {
  dat <- as.data.frame(dat)

  for (v in c("OF02_01_num_log", "OF02_02_num_log", "SES_z")) {
    if (v %in% names(dat)) dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
  }

  for (v in c("OF_Spender", "OF_Spender_bin")) {
    if (v %in% names(dat)) {
      # Prefer the project-level cleaner if it exists, otherwise use local fallback.
      if (exists("clean_binary01", mode = "function")) {
        dat[[v]] <- clean_binary01(dat[[v]])
      } else {
        dat[[v]] <- clean_binary01_local(dat[[v]])
      }
    }
  }

  if ("OF_Spender" %in% names(dat)) {
    dat$OF_Spender_bin <- dat$OF_Spender
  } else if ("OF_Spender_bin" %in% names(dat)) {
    dat$OF_Spender <- dat$OF_Spender_bin
  }

  dat
}

# ------------------------------------------------------------------------------
# Estimator-specific lavaan options
# ------------------------------------------------------------------------------

is_wls_estimator <- function(estimator) {
  toupper(estimator) %in% c("WLSMV", "DWLS", "ULSMV", "WLSM", "WLS")
}

is_ml_estimator <- function(estimator) {
  toupper(estimator) %in% c("ML", "MLR", "MLM", "MLMV", "MLF", "MLMVS")
}

missing_method_for_estimator <- function(estimator) {
  estimator <- toupper(estimator)

  if (is_wls_estimator(estimator)) return("pairwise")
  if (is_ml_estimator(estimator)) return("fiml")

  "listwise"
}


is_binary_outcome <- function(outcome, dat = NULL) {
  if (is.null(outcome)) return(FALSE)
  if (outcome %in% c("OF_Spender", "OF_Spender_bin")) return(TRUE)
  if (!is.null(dat) && outcome %in% names(dat)) {
    vals <- unique(stats::na.omit(dat[[outcome]]))
    return(length(vals) <= 2)
  }
  FALSE
}

outcome_usability <- function(dat, outcome) {
  if (is.null(outcome) || !outcome %in% names(dat)) {
    return(tibble::tibble(Outcome = outcome, Available = FALSE, Nonmissing = 0L, Levels = 0L, Usable = FALSE, Note = "Outcome variable not found"))
  }
  x <- dat[[outcome]]
  nn <- sum(!is.na(x))
  lev <- dplyr::n_distinct(x, na.rm = TRUE)
  tibble::tibble(
    Outcome = outcome,
    Available = TRUE,
    Nonmissing = as.integer(nn),
    Levels = as.integer(lev),
    Usable = nn > 0 && lev >= 2,
    Note = ifelse(nn > 0 && lev >= 2, "OK", paste0("Outcome unusable: non-missing n = ", nn, ", distinct levels = ", lev))
  )
}

# Extract observed variables from lavaan syntax.
# LHS of =~ are latent variables. RHS variables that are themselves latent variables
# must not be audited as observed data columns.
extract_observed_vars <- function(syntax) {
  # v9: use lavaan's own parser rather than a handwritten line parser.
  # This correctly handles multi-line =~ definitions and multiple LHS regressions
  # such as RO_AC + RO_EC ~ RO_BI + RO_BR.
  syntax_chr <- paste(syntax, collapse = "\n")
  if (is.null(syntax_chr) || !nzchar(trimws(syntax_chr))) return(character())

  pt <- tryCatch(
    lavaan::lavaanify(
      model = syntax_chr,
      auto = FALSE,
      fixed.x = FALSE,
      warn = FALSE
    ),
    error = function(e1) {
      tryCatch(
        lavaan::lavParseModelString(syntax_chr),
        error = function(e2) {
          stop(
            "Could not parse lavaan syntax for observed-variable audit. ",
            "lavaanify: ", conditionMessage(e1),
            " | lavParseModelString: ", conditionMessage(e2),
            call. = FALSE
          )
        }
      )
    }
  )

  out <- lavaan::lavNames(pt, type = "ov")
  sort(unique(out[!is.na(out) & nzchar(out)]))
}

hash_model_data <- function(dat, vars) {
  vars <- sort(intersect(vars, names(dat)))
  if (length(vars) == 0) return(safe_hash("no_model_data_vars"))
  # Use only variables actually required by the lavaan model. This prevents
  # unrelated workspace/data changes from invalidating the cache but protects
  # against stale fits when model data change at the same nrow/column names.
  obj <- dat[, vars, drop = FALSE]
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(obj, algo = "xxhash64"))
  }
  safe_hash(capture.output(utils::str(obj)))
}

make_model_cache_hash <- function(syntax, dat, estimator, missing_method, ordered_vars = NULL, fixed.x = FALSE) {
  required_vars <- extract_observed_vars(syntax)
  data_hash <- hash_model_data(dat, required_vars)
  safe_hash(list(
    syntax = paste(syntax, collapse = "\n"),
    estimator = toupper(estimator),
    missing = missing_method,
    ordered = sort(ordered_vars %||% character()),
    fixed.x = fixed.x,
    required_vars = sort(required_vars),
    data_hash = data_hash
  ))
}

ordered_vars_for_estimator <- function(dat, syntax, estimator, outcome = NULL) {
  if (!is_wls_estimator(estimator)) return(NULL)

  vars <- extract_observed_vars(syntax)
  vars <- intersect(vars, names(dat))

  # Continuous variables must not be passed as ordered.
  continuous_vars <- c("OF02_01_num_log", "OF02_02_num_log", "SES_z")
  ordered_vars <- setdiff(vars, continuous_vars)

  # Binary outcome in WLSMV SEM is categorical and should be ordered.
  if (!is.null(outcome) && is_binary_outcome(outcome, dat) && outcome %in% names(dat)) {
    ordered_vars <- union(ordered_vars, outcome)
  }

  ordered_vars <- unique(ordered_vars)
  if (length(ordered_vars) == 0) return(NULL)
  ordered_vars
}

lavaan_args <- function(model, data, estimator, syntax, outcome = NULL,
                        std.lv = TRUE, fixed.x = FALSE, warn = TRUE,
                        check.gradient = TRUE, check.vcov = TRUE) {
  estimator <- toupper(estimator)
  data <- coerce_sem_data(data)

  args <- list(
    model = model,
    data = data,
    estimator = estimator,
    missing = missing_method_for_estimator(estimator),
    std.lv = std.lv,
    fixed.x = fixed.x,
    warn = warn,
    check.gradient = check.gradient,
    check.vcov = check.vcov
  )

  ord <- ordered_vars_for_estimator(data, syntax = syntax, estimator = estimator, outcome = outcome)
  if (!is.null(ord)) args$ordered <- ord

  args
}

# ------------------------------------------------------------------------------
# Fit measures and detailed extraction
# ------------------------------------------------------------------------------

fit_measures_vector_safe <- function(fit) {
  if (is.null(fit) || inherits(fit, "try-error")) return(tibble::tibble())
  fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
  if (is.null(fm) || length(fm) == 0) return(tibble::tibble())
  tibble::tibble(Measure = names(fm), Value = suppressWarnings(as.numeric(fm)))
}

fit_measures_safe <- function(fit) {
  empty <- tibble(
    n = NA_integer_, converged = NA, CFI = NA_real_, TLI = NA_real_,
    RMSEA = NA_real_, SRMR = NA_real_, Chi2 = NA_real_, df = NA_real_, p = NA_real_
  )
  if (is.null(fit) || inherits(fit, "try-error")) return(empty)
  fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
  if (is.null(fm)) return(empty)

  # lavaan exposes different fit-measure names depending on estimator and model class.
  # MLR often has *.robust, WLSMV often has *.scaled, and some older cached fits only have unscaled names.
  # We therefore take the first available, finite value in a priority vector rather than using a single fallback.
  get_fm_any <- function(candidates) {
    for (nm in candidates) {
      if (nm %in% names(fm)) {
        val <- suppressWarnings(as.numeric(fm[[nm]]))
        if (length(val) == 1 && !is.na(val) && is.finite(val)) return(val)
      }
    }
    NA_real_
  }

  tibble(
    n = tryCatch(as.integer(lavaan::lavInspect(fit, "ntotal")), error = function(e) NA_integer_),
    converged = tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) NA),
    CFI = get_fm_any(c("cfi.robust", "cfi.scaled", "cfi")),
    TLI = get_fm_any(c("tli.robust", "tli.scaled", "tli")),
    RMSEA = get_fm_any(c("rmsea.robust", "rmsea.scaled", "rmsea")),
    SRMR = get_fm_any(c("srmr")),
    Chi2 = get_fm_any(c("chisq.scaled", "chisq")),
    df = get_fm_any(c("df.scaled", "df")),
    p = get_fm_any(c("pvalue.scaled", "pvalue"))
  )
}

# Refresh summary/detail CSVs from a cached lavaan object. This is important because
# older cached CSVs may have NA fit measures due to an older extractor, even though
# the cached .rds object contains a valid lavaan fit.
write_cfa_outputs_from_fit <- function(fit, model_id, estimator, syntax_hash, fit_csv_name,
                                       refit = NA_character_, elapsed_sec = NA_real_, cache_label = "cached") {
  fit_tbl <- fit_measures_safe(fit) %>%
    mutate(Model_ID = model_id, Estimator = toupper(estimator), Refit = as.character(refit %||% NA_character_),
           Error = NA_character_, Elapsed_sec = suppressWarnings(as.numeric(elapsed_sec)),
           Cache = cache_label, Syntax_Hash = syntax_hash, .before = 1) %>%
    normalise_fit_types()
  write_debug_csv(fit_tbl, fit_csv_name)
  write_debug_csv(fit_measures_vector_safe(fit), sub("_fit\\.csv$", "_fitmeasures_all.csv", fit_csv_name))
  tabs <- standardized_tables(fit)
  prefix <- sub("_fit\\.csv$", "", fit_csv_name)
  write_debug_csv(tabs$loadings, paste0(prefix, "_loadings.csv"))
  write_debug_csv(tabs$latent_cor, paste0(prefix, "_latent_cor.csv"))
  write_debug_csv(head(tabs$mi, 100), paste0(prefix, "_mi_top100.csv"))
  write_debug_csv(tabs$pe, paste0(prefix, "_parameter_estimates.csv"))
  invisible(fit_tbl)
}

write_sem_outputs_from_fit <- function(fit, model_id, outcome, ses_mode, estimator, syntax_hash, fit_csv_name,
                                       refit = NA_character_, elapsed_sec = NA_real_, cache_label = "cached") {
  fit_tbl <- fit_measures_safe(fit) %>%
    mutate(Model_ID = model_id, Outcome = outcome, SES_Mode = ses_mode, Estimator = toupper(estimator),
           Refit = as.character(refit %||% NA_character_), Error = NA_character_,
           Elapsed_sec = suppressWarnings(as.numeric(elapsed_sec)), Cache = cache_label,
           Syntax_Hash = syntax_hash, .before = 1) %>%
    normalise_fit_types()
  write_debug_csv(fit_tbl, fit_csv_name)
  write_debug_csv(fit_measures_vector_safe(fit), sub("_fit\\.csv$", "_fitmeasures_all.csv", fit_csv_name))
  tabs <- standardized_tables(fit)
  prefix <- sub("_fit\\.csv$", "", fit_csv_name)
  write_debug_csv(tabs$paths, paste0(prefix, "_paths.csv"))
  write_debug_csv(tabs$loadings, paste0(prefix, "_loadings.csv"))
  write_debug_csv(tabs$latent_cor, paste0(prefix, "_latent_cor.csv"))
  write_debug_csv(tabs$rsq, paste0(prefix, "_rsq.csv"))
  write_debug_csv(head(tabs$mi, 100), paste0(prefix, "_mi_top100.csv"))
  write_debug_csv(tabs$pe, paste0(prefix, "_parameter_estimates.csv"))
  invisible(fit_tbl)
}

fit_lavaan_verbose <- function(model, data, type = c("sem", "cfa"), outcome = NULL,
                               estimator = NULL, label = "fit") {
  type <- match.arg(type)

  if (is.null(estimator)) {
    estimator <- if (!is.null(outcome) && is_binary_outcome(outcome, data)) "WLSMV" else "MLR"
  }
  estimator <- toupper(estimator)

  data <- coerce_sem_data(data)
  miss <- missing_method_for_estimator(estimator)
  ord <- ordered_vars_for_estimator(data, syntax = model, estimator = estimator, outcome = outcome)

  cat("\n============================================================\n")
  cat("Running ", toupper(type), ": ", label, "\n", sep = "")
  cat("Estimator: ", estimator, " | missing: ", miss, "\n", sep = "")
  if (!is.null(ord)) cat("Ordered: ", paste(ord, collapse = ", "), "\n", sep = "")
  cat("Rows: ", nrow(data), " | Cols: ", ncol(data), "\n", sep = "")
  cat("============================================================\n")

  fit_call <- function(check_gradient = TRUE, check_vcov = TRUE) {
    args <- lavaan_args(
      model = model,
      data = data,
      estimator = estimator,
      syntax = model,
      outcome = outcome,
      std.lv = TRUE,
      fixed.x = FALSE,
      warn = TRUE,
      check.gradient = check_gradient,
      check.vcov = check_vcov
    )

    if (type == "cfa") {
      do.call(lavaan::cfa, args)
    } else {
      do.call(lavaan::sem, args)
    }
  }

  t0 <- Sys.time()
  fit <- tryCatch(
    fit_call(check_gradient = TRUE, check_vcov = TRUE),
    error = function(e) {
      cat("\nLAVAAN ERROR:\n", conditionMessage(e), "\n")
      structure(list(error = conditionMessage(e)), class = "lavaan_error")
    }
  )
  elapsed <- difftime(Sys.time(), t0, units = "secs")

  if (inherits(fit, "lavaan_error")) {
    return(list(fit = NULL, error = fit$error, elapsed = as.numeric(elapsed), refit = "not_attempted"))
  }

  conv <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE)
  cat("Strict converged: ", conv, " | elapsed seconds: ", round(as.numeric(elapsed), 2), "\n", sep = "")

  if (!isTRUE(conv)) {
    cat("Strict fit did not converge. Trying diagnostic refit with check.gradient = FALSE, check.vcov = FALSE...\n")
    t1 <- Sys.time()
    fit2 <- tryCatch(
      fit_call(check_gradient = FALSE, check_vcov = FALSE),
      error = function(e) {
        cat("\nREFIT ERROR:\n", conditionMessage(e), "\n")
        NULL
      }
    )
    elapsed2 <- difftime(Sys.time(), t1, units = "secs")
    conv2 <- if (!is.null(fit2)) tryCatch(lavaan::lavInspect(fit2, "converged"), error = function(e) FALSE) else FALSE
    cat("Diagnostic refit converged: ", conv2, " | elapsed seconds: ", round(as.numeric(elapsed2), 2), "\n", sep = "")

    # v9: return diagnostic refits only if they actually converge. Even then
    # they are marked as gradient_vcov_tolerant and should not be interpreted as
    # primary evidence in the QMD/manuscript tables.
    if (!is.null(fit2) && isTRUE(conv2)) {
      return(list(fit = fit2, error = NA_character_, elapsed = as.numeric(elapsed) + as.numeric(elapsed2), refit = "gradient_vcov_tolerant"))
    }

    return(list(fit = fit, error = "Strict fit did not converge; diagnostic refit did not converge and was not accepted as primary output.", elapsed = as.numeric(elapsed) + as.numeric(elapsed2), refit = "strict_not_converged"))
  }

  list(fit = fit, error = NA_character_, elapsed = as.numeric(elapsed), refit = "none")
}

standardized_tables <- function(fit) {
  if (is.null(fit)) {
    return(list(pe = tibble(), loadings = tibble(), paths = tibble(), latent_cor = tibble(), mi = tibble(), rsq = tibble()))
  }

  pe <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE), error = function(e) tibble())
  pe <- as_tibble(pe)

  loadings <- pe %>% filter(op == "=~") %>% select(any_of(c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")))
  paths <- pe %>% filter(op == "~") %>% select(any_of(c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")))
  latent_cor <- pe %>% filter(op == "~~", lhs != rhs) %>% select(any_of(c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")))
  mi <- tryCatch(lavaan::modindices(fit, sort. = TRUE), error = function(e) tibble()) %>% as_tibble()
  rsq <- tryCatch(lavaan::inspect(fit, "rsquare"), error = function(e) NULL)
  rsq_tbl <- if (is.null(rsq) || length(rsq) == 0) tibble() else tibble(Variable = names(rsq), R2 = as.numeric(rsq))

  list(pe = pe, loadings = loadings, paths = paths, latent_cor = latent_cor, mi = mi, rsq = rsq_tbl)
}

# ------------------------------------------------------------------------------
# Variable audit
# ------------------------------------------------------------------------------

audit_required_observed_vars <- function(syntax, dat) {
  obs <- extract_observed_vars(syntax)

  tibble(
    Variable = obs,
    Available = obs %in% names(dat),
    Class = purrr::map_chr(obs, ~ if (.x %in% names(dat)) paste(class(dat[[.x]]), collapse = "/") else NA_character_),
    Missing_N = purrr::map_int(obs, ~ if (.x %in% names(dat)) sum(is.na(dat[[.x]])) else NA_integer_),
    Nonmissing_N = purrr::map_int(obs, ~ if (.x %in% names(dat)) sum(!is.na(dat[[.x]])) else NA_integer_),
    Unique_N = purrr::map_int(obs, ~ if (.x %in% names(dat)) dplyr::n_distinct(dat[[.x]], na.rm = TRUE) else NA_integer_),
    Usable = Available & Nonmissing_N > 0 & Unique_N >= 2
  ) %>% distinct() %>% arrange(!Available, !Usable, Variable)
}

# ------------------------------------------------------------------------------
# Main wrappers used by 04/05/06 scripts
# ------------------------------------------------------------------------------

run_one_cfa <- function(model_id,
                        estimator = "MLR",
                        data_override = NULL,
                        print_summary = TRUE) {
  stopifnot(model_id %in% names(CFA_REGISTRY))
  reg <- CFA_REGISTRY[[model_id]]
  dat <- data_override %||% DATASETS[[reg$data]]
  dat <- coerce_sem_data(dat)
  syntax <- reg$fun()
  estimator <- toupper(estimator)

  syntax_hash <- make_model_cache_hash(syntax, dat, estimator, missing_method_for_estimator(estimator), ordered_vars_for_estimator(dat, syntax, estimator), fixed.x = FALSE)

  syn_file <- file.path(SEM_DEBUG_OUT, "syntax", paste0("cfa_", model_id, "_", estimator, ".lav"))
  writeLines(syntax, syn_file)

  rds_path <- file.path(SEM_DEBUG_OUT, "fits", paste0("cfa_", model_id, "_", estimator, ".rds"))
  fit_csv_path <- file.path(SEM_DEBUG_OUT, "csv", paste0("cfa_", model_id, "_", estimator, "_fit.csv"))
  if (cache_available(rds_path, fit_csv_path, expected_syntax_hash = syntax_hash, expected_estimator = estimator)) {
    cached_fit <- load_cached_fit(rds_path, paste0("cfa_", model_id, "_", estimator))
    old_meta <- tryCatch(read_debug_csv_safe(fit_csv_path), error = function(e) tibble::tibble())
    old_refit <- if ("Refit" %in% names(old_meta) && nrow(old_meta) > 0) old_meta$Refit[[nrow(old_meta)]] else NA_character_
    old_elapsed <- if ("Elapsed_sec" %in% names(old_meta) && nrow(old_meta) > 0) old_meta$Elapsed_sec[[nrow(old_meta)]] else NA_real_
    write_cfa_outputs_from_fit(cached_fit, model_id, estimator, syntax_hash,
                               paste0("cfa_", model_id, "_", estimator, "_fit.csv"),
                               refit = old_refit, elapsed_sec = old_elapsed, cache_label = "cached_refreshed")
    return(invisible(cached_fit))
  }

  var_audit <- audit_required_observed_vars(syntax, dat)
  write_debug_csv(var_audit, paste0("cfa_", model_id, "_var_audit.csv"))
  missing_vars <- var_audit %>% filter(!Available)
  if (nrow(missing_vars) > 0) {
    print(missing_vars)
    stop("Missing variables in CFA syntax. See CSV var audit.")
  }

  res <- fit_lavaan_verbose(syntax, dat, type = "cfa", estimator = estimator, label = model_id)
  fit <- res$fit

  saveRDS(fit, rds_path)

  fit_tbl <- write_cfa_outputs_from_fit(fit, model_id, estimator, syntax_hash,
                                        paste0("cfa_", model_id, "_", estimator, "_fit.csv"),
                                        refit = res$refit, elapsed_sec = res$elapsed, cache_label = "computed")

  if (print_summary && !is.null(fit)) {
    print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
    print(tryCatch(lavaan::lavInspect(fit, "optim"), error = function(e) conditionMessage(e)))
  }

  invisible(fit)
}

run_one_sem <- function(model_id,
                        outcome,
                        ses_mode = c("none", "outcome", "latent_outcome"),
                        estimator = NULL,
                        data_override = NULL,
                        print_summary = TRUE) {
  ses_mode <- match.arg(ses_mode)
  stopifnot(model_id %in% names(SEM_REGISTRY))
  reg <- SEM_REGISTRY[[model_id]]
  dat <- data_override %||% DATASETS[[reg$data]]
  dat <- coerce_sem_data(dat)

  if (!outcome %in% names(dat)) stop("Outcome not found in data: ", outcome)
  if (is_binary_outcome(outcome, dat)) dat[[outcome]] <- clean_binary01_local(dat[[outcome]])

  outcome_check <- outcome_usability(dat, outcome)

  syntax <- reg$fun(outcome, ses_mode = ses_mode, dat = dat)
  effective_estimator <- toupper(estimator %||% ifelse(is_binary_outcome(outcome, dat), "WLSMV", "MLR"))

  syntax_hash <- make_model_cache_hash(syntax, dat, effective_estimator, missing_method_for_estimator(effective_estimator), ordered_vars_for_estimator(dat, syntax, effective_estimator, outcome), fixed.x = FALSE)

  syn_file <- file.path(SEM_DEBUG_OUT, "syntax", paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_", effective_estimator, ".lav"))
  writeLines(syntax, syn_file)

  rds_name <- paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_", effective_estimator, ".rds")
  rds_path <- file.path(SEM_DEBUG_OUT, "fits", rds_name)
  fit_csv_path <- file.path(SEM_DEBUG_OUT, "csv", paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_fit.csv"))
  if (cache_available(rds_path, fit_csv_path, expected_syntax_hash = syntax_hash, expected_estimator = effective_estimator)) {
    cached_fit <- load_cached_fit(rds_path, paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_", effective_estimator))
    old_meta <- tryCatch(read_debug_csv_safe(fit_csv_path), error = function(e) tibble::tibble())
    old_refit <- if ("Refit" %in% names(old_meta) && nrow(old_meta) > 0) old_meta$Refit[[nrow(old_meta)]] else NA_character_
    old_elapsed <- if ("Elapsed_sec" %in% names(old_meta) && nrow(old_meta) > 0) old_meta$Elapsed_sec[[nrow(old_meta)]] else NA_real_
    write_sem_outputs_from_fit(cached_fit, model_id, outcome, ses_mode, effective_estimator, syntax_hash,
                               paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_fit.csv"),
                               refit = old_refit, elapsed_sec = old_elapsed, cache_label = "cached_refreshed")
    return(invisible(cached_fit))
  }

  if (!isTRUE(outcome_check$Usable[[1]])) {
    msg <- outcome_check$Note[[1]]
    message("Skipping SEM because outcome is not usable: ", msg)
    fit_tbl <- outcome_check %>%
      transmute(Model_ID = model_id, Outcome = outcome, SES_Mode = ses_mode, Estimator = effective_estimator,
                Status = "skipped", Refit = "not_attempted", Error = msg, Elapsed_sec = 0,
                Cache = "computed", Syntax_Hash = syntax_hash, n = NA_real_, converged = NA,
                CFI = NA_real_, TLI = NA_real_, RMSEA = NA_real_, SRMR = NA_real_, Chi2 = NA_real_, df = NA_real_, p = NA_real_) %>%
      normalise_fit_types()
    write_debug_csv(fit_tbl, paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_fit.csv"))
    saveRDS(NULL, rds_path)
    return(invisible(NULL))
  }

  var_audit <- audit_required_observed_vars(syntax, dat)
  write_debug_csv(var_audit, paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_var_audit.csv"))
  missing_vars <- var_audit %>% filter(!Available)
  if (nrow(missing_vars) > 0) {
    print(missing_vars)
    stop("Missing variables in SEM syntax. See CSV var audit.")
  }

  res <- fit_lavaan_verbose(
    syntax,
    dat,
    type = "sem",
    outcome = outcome,
    estimator = effective_estimator,
    label = paste(model_id, outcome, ses_mode)
  )
  fit <- res$fit

  saveRDS(fit, rds_path)

  fit_tbl <- write_sem_outputs_from_fit(fit, model_id, outcome, ses_mode, effective_estimator, syntax_hash,
                                        paste0("sem_", model_id, "_", outcome, "_", ses_mode, "_fit.csv"),
                                        refit = res$refit, elapsed_sec = res$elapsed, cache_label = "computed")

  if (print_summary && !is.null(fit)) {
    print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
    print(tryCatch(lavaan::lavInspect(fit, "optim"), error = function(e) conditionMessage(e)))
  }

  invisible(fit)
}

inspect_saved_fit <- function(path) {
  fit <- readRDS(path)
  if (is.null(fit)) {
    message("Saved fit is NULL: ", path)
    return(invisible(NULL))
  }
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  print(tryCatch(lavaan::lavInspect(fit, "optim"), error = function(e) conditionMessage(e)))
  invisible(fit)
}

message("R/11_fit_helpers.R ready (compendium port of 02_fit_helpers.R). SEM_REGISTRY: ",
        paste(names(SEM_REGISTRY), collapse = ", "))
