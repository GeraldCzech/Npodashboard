# ==============================================================================
# 22_bootstrap_r2.R   (compendium version of 11_bootstrap_r2_reviewerfit.R)
# Reviewer-grade paired nonparametric bootstrap of pairwise outcome-R²
# differences (ΔR²) between the benchmark brand-equity architectures.
#
# Changes vs. the version that hung for 24h (execution only, NOT methodology):
#   * BLAS/OpenMP threads pinned to 1 (prevents thread oversubscription, the
#     usual cause of a parallel lavaan job crawling for days).
#   * fork-based parallelism via mclapply on Unix (no PSOCK export, no ses_block
#     dependency on workers); serial fallback elsewhere.
#   * batched progress + timing output, so the run is never silent.
#   * per-fit elapsed-time limit, so a single pathological resample cannot hang
#     the whole run (it just returns NA and is dropped).
#   * BCa is OPTIONAL (off by default): when off, the serial jackknife is
#     skipped. Percentile + bias-corrected (BC) intervals are always reported;
#     turn BOOT_BCA <- TRUE on once the run time is known to be acceptable.
#   * default level = row_level only (CASE is unique per row here, so the
#     respondent-clustered scheme is identical to row-level; add it back via
#     BOOT_LEVELS if a true respondent key becomes available).
#
# Methodology unchanged: paired case bootstrap, resample size = full n, all
# models refit on the identical resample, MLR+FIML / WLSMV+pairwise(ordered),
# admissible solutions only (converged, no Heywood), percentile 95% CI + two-
# sided bootstrap p, Holm-adjusted within each outcome. Held in memory, written
# once (no CSV round-trip), so per-replicate R² cannot be lost.
# ==============================================================================

suppressPackageStartupMessages({
  library(lavaan); library(dplyr); library(tidyr)
  library(tibble); library(purrr); library(readr); library(parallel)
})
stopifnot(requireNamespace("here", quietly = TRUE))

# --- pin BLAS/OpenMP to 1 thread BEFORE anything heavy / before forking -------
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  try(RhpcBLASctl::blas_set_num_threads(1), silent = TRUE)
  try(RhpcBLASctl::omp_set_num_threads(1),  silent = TRUE)
}

# Compendium modules (replace legacy scripts/sem_debug/ paths)
source(here::here("R", "00_paths.R"),      encoding = "UTF-8")  # DATA_DIR, CSV_DIR etc.
source(here::here("R", "01_packages.R"),   encoding = "UTF-8")  # library() calls
source(here::here("R", "10_datasets.R"),   encoding = "UTF-8")  # DATASETS registry
source(here::here("R", "12_lavaan_models.R"), encoding = "UTF-8")  # sem_* builders
source(here::here("R", "11_fit_helpers.R"),   encoding = "UTF-8")  # SEM_REGISTRY, coerce_sem_data(), is_binary_outcome()

# ------------------------------------------------------------------ Configuration
BOOT_B          <- get0("BOOT_B",            ifnotfound = 1000L,     inherits = TRUE)
BOOT_SEED       <- get0("BOOT_SEED",         ifnotfound = 20260602L, inherits = TRUE)
BOOT_OUTCOMES   <- get0("BOOT_OUTCOMES",     ifnotfound = c("OF02_01_num_log", "OF02_02_num_log", "OF_Spender"), inherits = TRUE)
BOOT_MODELS     <- get0("BOOT_MODELS",       ifnotfound = c("bo_original", "fc_core_B", "ro_original"), inherits = TRUE)
BOOT_LEVELS     <- get0("BOOT_LEVELS",       ifnotfound = c("row_level"), inherits = TRUE)   # clustered redundant here
BOOT_CLUSTER    <- get0("BOOT_CLUSTER_VAR",  ifnotfound = "CASE",    inherits = TRUE)
BOOT_SES_MODE   <- get0("BOOT_SES_MODE",     ifnotfound = "none",    inherits = TRUE)
BOOT_SAMPLE_LAB <- get0("BOOT_SAMPLE_LABEL", ifnotfound = "common-case cross", inherits = TRUE)
BOOT_NCORES     <- get0("BOOT_NCORES",       ifnotfound = max(1L, parallel::detectCores() - 1L), inherits = TRUE)
BOOT_BATCH      <- get0("BOOT_BATCH",        ifnotfound = 50L,       inherits = TRUE)   # progress granularity
BOOT_FIT_TIMEOUT<- get0("BOOT_FIT_TIMEOUT",  ifnotfound = 60,        inherits = TRUE)   # seconds per single fit
BOOT_BCA        <- get0("BOOT_BCA",          ifnotfound = FALSE,     inherits = TRUE)   # jackknife only if TRUE
BOOT_ORDERED_ITEMS <- get0("BOOT_ORDERED_ITEMS", ifnotfound = FALSE,   inherits = TRUE)   # TRUE = all Likert items ordinal (slow, matches WLSMV sensitivity); FALSE = only the binary DV is categorical, indicators continuous (primary treatment, ~20-30x faster)
CONTINUOUS_VARS <- c("OF02_01_num_log", "OF02_02_num_log", "SES_z")

SUPP_DIR <- file.path(CSV_DIR, "supplements")  # portable: outputs/csv/supplements/
dir.create(SUPP_DIR, recursive = TRUE, showWarnings = FALSE)

boot_data <- coerce_sem_data(DATASETS$cross)
if (is.null(boot_data)) stop("DATASETS$cross unavailable.")
BOOT_MODELS <- BOOT_MODELS[BOOT_MODELS %in% names(SEM_REGISTRY)]

use_fork <- .Platform$OS.type == "unix" && BOOT_NCORES > 1L
message(sprintf("Reviewer-fit bootstrap | models: %s | B=%d | levels: %s | cores=%d (%s) | BCa=%s | ordered_items=%s",
                paste(BOOT_MODELS, collapse = ", "), BOOT_B, paste(BOOT_LEVELS, collapse = ","),
                BOOT_NCORES, if (use_fork) "fork" else "serial", BOOT_BCA, BOOT_ORDERED_ITEMS))

par_map <- function(X, f) if (use_fork) parallel::mclapply(X, f, mc.cores = BOOT_NCORES, mc.preschedule = TRUE) else lapply(X, f)

# ------------------------------------------------------------------ Specs (main proc)
build_spec <- function(model_id, outcome) {
  syn    <- SEM_REGISTRY[[model_id]]$fun(outcome, ses_mode = BOOT_SES_MODE, dat = boot_data)
  binary <- is_binary_outcome(outcome, boot_data)
  spec <- list(model = syn, std.lv = TRUE, fixed.x = FALSE, warn = FALSE)
  if (binary) {
    spec$estimator <- "WLSMV"; spec$missing <- "pairwise"
    if (isTRUE(BOOT_ORDERED_ITEMS)) {
      ov <- lavaan::lavNames(lavaan::lavParseModelString(syn), "ov")
      spec$ordered <- setdiff(intersect(ov, names(boot_data)), CONTINUOUS_VARS)
    } else {
      # Only the binary donor variable is categorical (probit); Likert indicators
      # are treated as continuous, exactly as in the PRIMARY main-analysis models.
      # This avoids hundreds of polychoric correlations per fit (the bottleneck).
      spec$ordered <- outcome
    }
  } else {
    spec$estimator <- "MLR";   spec$missing <- "fiml"
  }
  spec
}
model_ov <- function(model_id, outcome)
  lavaan::lavNames(lavaan::lavParseModelString(
    SEM_REGISTRY[[model_id]]$fun(outcome, ses_mode = BOOT_SES_MODE, dat = boot_data)), "ov")

spec_key <- function(model_id, outcome) paste(model_id, outcome, sep = "@@")
FITSPEC <- list()
for (m in BOOT_MODELS) for (o in BOOT_OUTCOMES) FITSPEC[[spec_key(m, o)]] <- build_spec(m, o)

# ------------------------------------------------------------------ Fit (timeout-guarded)
fit_r2 <- function(spec, outcome, dat) {
  tryCatch({
    setTimeLimit(elapsed = BOOT_FIT_TIMEOUT, transient = TRUE)
    fit <- do.call(lavaan::sem, c(spec, list(data = dat)))
    setTimeLimit()
    val <- NA_real_
    if (isTRUE(lavaan::lavInspect(fit, "converged"))) {
      pe <- lavaan::parameterEstimates(fit)
      heywood <- any(pe$op == "~~" & pe$lhs == pe$rhs & pe$est < -1e-6, na.rm = TRUE)
      if (!heywood) {
        r2 <- lavaan::inspect(fit, "rsquare")
        if (outcome %in% names(r2)) {
          v <- unname(r2[[outcome]]); if (length(v) == 1 && is.finite(v)) val <- v
        }
      }
    }
    val
  }, error = function(e) { setTimeLimit(); NA_real_ })
}
r2_all <- function(outcome, dat)
  vapply(BOOT_MODELS, function(m) fit_r2(FITSPEC[[spec_key(m, outcome)]], outcome, dat), numeric(1))

resample_rows <- function(cc, level, clusters) {
  n <- nrow(cc)
  if (level == "row_level" || is.null(clusters)) return(cc[sample.int(n, n, replace = TRUE), , drop = FALSE])
  ids <- sample(names(clusters), length(clusters), replace = TRUE)
  cc[unlist(clusters[ids], use.names = FALSE), , drop = FALSE]
}

# ------------------------------------------------------------------ Intervals
pct_ci <- function(t, conf = .95) stats::quantile(t, c((1-conf)/2, 1-(1-conf)/2), names = FALSE, na.rm = TRUE)
two_sided_p <- function(t) min(1, 2 * min(mean(t <= 0), mean(t >= 0)))
bc_ci <- function(theta_hat, t, conf = .95) {                    # bias-corrected (no jackknife)
  t <- t[is.finite(t)]; if (length(t) < 50) return(c(NA_real_, NA_real_))
  z0 <- qnorm(mean(t < theta_hat)); if (!is.finite(z0)) return(c(NA_real_, NA_real_))
  za <- qnorm(c((1-conf)/2, 1-(1-conf)/2))
  stats::quantile(t, pnorm(2 * z0 + za), names = FALSE)
}
bca_ci <- function(theta_hat, t, theta_jack, conf = .95) {       # bias-corrected & accelerated
  t <- t[is.finite(t)]; theta_jack <- theta_jack[is.finite(theta_jack)]
  if (length(t) < 50 || length(theta_jack) < 3) return(c(NA_real_, NA_real_))
  z0 <- qnorm(mean(t < theta_hat)); if (!is.finite(z0)) return(c(NA_real_, NA_real_))
  jbar <- mean(theta_jack); den <- 6 * (sum((jbar - theta_jack)^2))^1.5
  a <- if (den == 0) 0 else sum((jbar - theta_jack)^3) / den
  za <- qnorm(c((1-conf)/2, 1-(1-conf)/2))
  adj <- pnorm(z0 + (z0 + za) / (1 - a * (z0 + za)))
  if (any(!is.finite(adj))) return(c(NA_real_, NA_real_))
  stats::quantile(t, adj, names = FALSE)
}

fmt_dur <- function(s) if (s < 90) sprintf("%.0fs", s) else if (s < 5400) sprintf("%.1f min", s/60) else sprintf("%.1f h", s/3600)

# ------------------------------------------------------------------ Driver per (outcome, level)
run_cell <- function(outcome, level) {
  ov_all <- intersect(unique(unlist(lapply(BOOT_MODELS, model_ov, outcome = outcome))), names(boot_data))
  cc <- boot_data[stats::complete.cases(boot_data[, ov_all, drop = FALSE]), , drop = FALSE]
  n  <- nrow(cc)
  if (n < 50) { message("  skip ", outcome, "/", level, " (n=", n, ")"); return(tibble()) }
  clusters <- NULL
  if (level == "respondent_clustered") {
    if (!BOOT_CLUSTER %in% names(cc)) { message("  no cluster var; skip clustered."); return(tibble()) }
    clusters <- split(seq_len(n), cc[[BOOT_CLUSTER]])
  }
  t0 <- Sys.time()
  message(sprintf("  [%s] %s / %s | n=%d | B=%d ...", format(t0, "%H:%M:%S"), outcome, level, n, BOOT_B))
  
  boot_one <- function(b) {
    set.seed(BOOT_SEED + b)
    tryCatch(r2_all(outcome, resample_rows(cc, level, clusters)),
             error = function(e) rep(NA_real_, length(BOOT_MODELS)))
  }
  batches <- split(seq_len(BOOT_B), ceiling(seq_len(BOOT_B) / BOOT_BATCH))
  reps <- list(); done <- 0L
  for (bi in seq_along(batches)) {
    reps <- c(reps, par_map(batches[[bi]], boot_one))
    done <- done + length(batches[[bi]])
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    message(sprintf("      %d/%d replicates | elapsed %s | ETA %s",
                    done, BOOT_B, fmt_dur(el), fmt_dur(el / done * (BOOT_B - done))))
  }
  R2 <- do.call(rbind, reps); colnames(R2) <- BOOT_MODELS
  r2_point <- r2_all(outcome, cc)
  
  theta_jack_pair <- NULL
  if (BOOT_BCA) {
    message("      jackknife for BCa ...")
    jack_units <- if (level == "respondent_clustered") clusters else as.list(seq_len(n))
    jack <- vapply(jack_units, function(drop_idx) r2_all(outcome, cc[-drop_idx, , drop = FALSE]),
                   numeric(length(BOOT_MODELS)))
    rownames(jack) <- BOOT_MODELS
    theta_jack_pair <- jack
  }
  
  pairs <- utils::combn(sort(BOOT_MODELS), 2, simplify = FALSE)
  res <- purrr::map_dfr(pairs, function(pr) {
    a <- pr[[1]]; b <- pr[[2]]
    d <- R2[, a] - R2[, b]; d <- d[is.finite(d)]
    dp <- r2_point[[a]] - r2_point[[b]]; Beff <- length(d)
    ci   <- if (Beff >= 50) pct_ci(d) else c(NA_real_, NA_real_)
    cibc <- bc_ci(dp, d)
    cibca <- if (BOOT_BCA) bca_ci(dp, d, theta_jack_pair[a, ] - theta_jack_pair[b, ]) else c(NA_real_, NA_real_)
    tibble(
      Sample = BOOT_SAMPLE_LAB, Outcome = outcome, Model_A = a, Model_B = b,
      R2_A = r2_point[[a]], R2_B = r2_point[[b]], Delta_R2 = dp, Delta_R2_pp = 100 * dp,
      CI_low = ci[1], CI_high = ci[2],                       # primary = percentile
      p_value = if (Beff >= 50) two_sided_p(d) else NA_real_,
      CI_low_bc = cibc[1], CI_high_bc = cibc[2],
      CI_low_bca = cibca[1], CI_high_bca = cibca[2],
      bias = if (Beff >= 50) mean(d) - dp else NA_real_,
      se   = if (Beff >= 50) stats::sd(d) else NA_real_,
      n = n, B_target = as.integer(BOOT_B), B = Beff, conv_rate = Beff / BOOT_B,
      Bootstrap_Level = level,
      Note = sprintf("paired %s bootstrap; resample size n=%d; %d/%d admissible", level, n, Beff, BOOT_B)
    )
  })
  res$p_holm <- p.adjust(res$p_value, method = "holm")
  attr(res, "long_r2") <- as_tibble(R2) %>%
    mutate(Iteration = row_number(), Outcome = outcome, Bootstrap_Level = level) %>%
    tidyr::pivot_longer(all_of(BOOT_MODELS), names_to = "Model_ID", values_to = "R2")
  message(sprintf("  done %s / %s in %s (admissible: %s)",
                  outcome, level, fmt_dur(as.numeric(difftime(Sys.time(), t0, units = "secs"))),
                  paste(sprintf("%s-%s:%d", substr(sapply(pairs, `[`, 1),1,6),
                                substr(sapply(pairs, `[`, 2),1,6), sapply(pairs, function(pr) sum(is.finite(R2[,pr[1]]-R2[,pr[2]])))), collapse=" ")))
  res
}

# ------------------------------------------------------------------ Run grid
# Run continuous (fast MLR) outcomes first so core results land within minutes;
# the categorical donor-status cell runs last. expand_grid preserves this order.
.ord  <- order(vapply(BOOT_OUTCOMES, is_binary_outcome, logical(1), dat = boot_data))
grid  <- tidyr::expand_grid(outcome = BOOT_OUTCOMES[.ord], level = BOOT_LEVELS)
cells <- purrr::pmap(grid, function(outcome, level) run_cell(outcome, level))
summary_tbl <- dplyr::bind_rows(cells)
long_tbl    <- dplyr::bind_rows(lapply(cells, function(x) attr(x, "long_r2")))

if (nrow(summary_tbl) == 0) {
  warning("No bootstrap rows produced.")
} else {
  for (lv in unique(summary_tbl$Bootstrap_Level))
    readr::write_csv(dplyr::filter(summary_tbl, Bootstrap_Level == lv),
                     file.path(SUPP_DIR, paste0("S20_r2_difference_bootstrap_", lv, ".csv")), na = "")
  readr::write_csv(summary_tbl, file.path(SUPP_DIR, "S20_r2_difference_bootstrap_all_levels.csv"), na = "")
  readr::write_csv(long_tbl,    file.path(SUPP_DIR, "S20_r2_difference_bootstrap_long_r2.csv"),    na = "")
  message("\nWrote exports to ", SUPP_DIR)
  print(summary_tbl %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
          select(Outcome, Model_A, Model_B, Delta_R2, CI_low, CI_high, p_value, p_holm, B, conv_rate))
}
message("\n11_bootstrap_r2_reviewerfit.R finished.")