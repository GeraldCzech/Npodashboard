# ==============================================================================
# 00_paths.R  —  Single, portable path configuration
# ------------------------------------------------------------------------------
# Replaces every hard-coded "~/10787172/..." or "/home/gerald/..." path.
# Everything is resolved relative to the project root via {here}.
# Override the data location with the environment variable CBE_DATA_DIR if your
# raw/derived data live outside the repo (recommended for the embargoed data).
# ==============================================================================

suppressPackageStartupMessages(library(here))

# Project root (the repo). here::here() finds it from any working directory.
PROJECT_ROOT <- here::here()

# Data directories ------------------------------------------------------------
# Default: data/derived inside the repo. Override via Sys.setenv(CBE_DATA_DIR=...)
DATA_DIR <- Sys.getenv("CBE_DATA_DIR", unset = here::here("data", "derived"))
RAW_DIR  <- Sys.getenv("CBE_RAW_DIR",  unset = here::here("data", "raw"))

# The standardised analysis workspace consumed by R/10_datasets.R
# (contains FC_BO_orig / BO_orig / RO_orig / cross_orig + outcome source).
DATEN_STANDARDISIERT <- file.path(DATA_DIR, "daten_standardisiert.RData")

# The assembled questionnaire object (qnr1/qnr2/qnr4/start01 + *_orig datasets)
FRAGEBOGEN_RDS <- file.path(DATA_DIR, "fragebogen.rds")

# Output directories ----------------------------------------------------------
OUT_DIR     <- here::here("outputs")
CSV_DIR     <- here::here("outputs", "csv")
FIG_DIR     <- here::here("outputs", "figures")
TAB_DIR     <- here::here("outputs", "tables")
SYNTAX_DIR  <- here::here("outputs", "syntax")
LOG_DIR     <- here::here("outputs", "logs")

for (d in c(OUT_DIR, CSV_DIR, FIG_DIR, TAB_DIR, SYNTAX_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Reproducible source selection (see R/10_datasets.R). Set this BEFORE the
# datasets module runs to avoid workspace-dependent heuristic selection.
# Example: SEM_DEBUG_CONTEXT_SOURCE_NAME <- "fragebogen$qnr1"
if (!exists("SEM_DEBUG_CONTEXT_SOURCE_NAME")) {
  SEM_DEBUG_CONTEXT_SOURCE_NAME <- Sys.getenv("CBE_CONTEXT_SOURCE", unset = NA_character_)
}

message("PROJECT_ROOT = ", PROJECT_ROOT)
message("DATA_DIR     = ", DATA_DIR)
