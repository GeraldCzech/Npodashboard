#!/usr/bin/env Rscript
# ==============================================================================
# run_bootstrap.R  —  Console entry point for the R² difference bootstrap
# ==============================================================================
# Usage (three variants, all equivalent):
#
#   # 1. Rscript (recommended for server — detach with screen/tmux)
#   Rscript run_bootstrap.R
#
#   # 2. Interactive R console
#   source("run_bootstrap.R")
#
#   # 3. With custom parameters (set env vars before calling)
#   BOOT_B=500 BOOT_NCORES=8 Rscript run_bootstrap.R
#
# Environment variables (all optional, sensible defaults):
#   CBE_DATA_DIR   path to folder with daten_standardisiert.RData
#                  default: data/derived/ inside the repo
#   BOOT_B         number of bootstrap replicates  (default: 1000)
#   BOOT_NCORES    parallel cores to use           (default: detectCores()-1)
#   BOOT_SEED      random seed                     (default: 20260602)
#   BOOT_BCA       "TRUE" to add BCa intervals — slow (default: FALSE)
#
# Output: outputs/csv/supplements/S20_r2_difference_bootstrap_*.csv
# ==============================================================================

# --- 0. Survive disconnects: print timestamps so output is never silent --------
message("\n", strrep("=", 70))
message("run_bootstrap.R started at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(strrep("=", 70))

# --- 1. Bootstrap parameters from env (override before calling if needed) -----
BOOT_B       <- as.integer(Sys.getenv("BOOT_B",      unset = "1000"))
BOOT_NCORES  <- as.integer(Sys.getenv("BOOT_NCORES",
                 unset = as.character(max(1L, parallel::detectCores() - 1L))))
BOOT_SEED    <- as.integer(Sys.getenv("BOOT_SEED",   unset = "20260602"))
BOOT_BCA     <- identical(toupper(Sys.getenv("BOOT_BCA", unset = "FALSE")), "TRUE")

message(sprintf("Parameters: B=%d | cores=%d | seed=%d | BCa=%s",
                BOOT_B, BOOT_NCORES, BOOT_SEED, BOOT_BCA))

# --- 2. Locate project root ----------------------------------------------------
# Works whether called via Rscript from the repo root, from a sub-directory,
# or sourced interactively from any working directory.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

# If called via Rscript the script itself is the anchor; otherwise use here()
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(0)$ofile)),   # interactive source()
  error = function(e)
    tryCatch(dirname(normalizePath(commandArgs(trailingOnly = FALSE)[
      grep("--file=", commandArgs(trailingOnly = FALSE))
    ][1L], winslash = "/")),                     # Rscript --file=
    error = function(e) here::here())
)
if (!is.na(script_dir) && nzchar(script_dir) && dir.exists(script_dir)) {
  setwd(script_dir)
  message("Working directory set to: ", script_dir)
} else {
  message("Working directory: ", getwd())
}

# --- 3. Source modules in dependency order ------------------------------------
message("\nLoading modules...")
source("R/00_paths.R")        # DATA_DIR, CSV_DIR, OUT_DIR
source("R/01_packages.R")     # library() calls
source("R/10_datasets.R")     # DATASETS = list(fc, bo, ro, cross)
source("R/12_lavaan_models.R")# sem_* builders
source("R/11_fit_helpers.R")  # SEM_REGISTRY, coerce_sem_data(), is_binary_outcome(), ...

# --- 4. Run the bootstrap -----------------------------------------------------
message("\nStarting bootstrap...")
source("R/22_bootstrap_r2.R")

message("\nrun_bootstrap.R finished at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
