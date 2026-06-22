# ==============================================================================
# 00_config_data.R
# Stable SEM debug data loading
# Project structure is fixed:
#   scripts/sem_debug/
#   outputs/sem_debug/
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(readr)
  library(stringr)
})

# ------------------------------------------------------------------------------
# Stable paths
# ------------------------------------------------------------------------------

PROJECT_ROOT <- here::here()
SCRIPT_DIR   <- here::here("scripts", "sem_debug")
OUTPUT_DIR   <- here::here("outputs", "sem_debug")
CSV_DIR      <- here::here("outputs", "sem_debug", "csv")
FIT_DIR      <- here::here("outputs", "sem_debug", "fits")
SYNTAX_DIR   <- here::here("outputs", "sem_debug", "syntax")
LOG_DIR      <- here::here("outputs", "sem_debug", "logs")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CSV_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SYNTAX_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

message("PROJECT_ROOT = ", PROJECT_ROOT)
message("SCRIPT_DIR   = ", SCRIPT_DIR)
message("OUTPUT_DIR   = ", OUTPUT_DIR)

# ------------------------------------------------------------------------------
# Load raw standardised data
# ------------------------------------------------------------------------------

DATA_FILE <- here::here("output", "daten_standardisiert.RData")

if (!file.exists(DATA_FILE)) {
  stop(
    "Data file not found: ", DATA_FILE,
    "\nExpected file: here::here('output', 'daten_standardisiert.RData')"
  )
}

message("Loading data from: ", DATA_FILE)

loaded_names <- load(DATA_FILE)

message("Objects loaded from RData:")
print(loaded_names)

# ------------------------------------------------------------------------------
# Helper: find object safely
# ------------------------------------------------------------------------------

get_first_existing <- function(candidates, env = .GlobalEnv) {
  existing <- candidates[candidates %in% ls(envir = env)]
  if (length(existing) == 0) return(NULL)
  get(existing[[1]], envir = env)
}

# ------------------------------------------------------------------------------
# Identify datasets
# Do NOT reduce columns here.
# ------------------------------------------------------------------------------

if (!exists("dat_fc")) {
  dat_fc <- get_first_existing(c("FC_BO_orig", "dat_fc_orig", "fc", "dat_faircloth"))
}

if (!exists("dat_bo")) {
  dat_bo <- get_first_existing(c("BO_orig", "dat_bo_orig", "bo", "dat_boenigk"))
}

if (!exists("dat_ro")) {
  dat_ro <- get_first_existing(c("RO_orig", "dat_ro_orig", "ro", "dat_romero"))
}

if (!exists("dat_cross")) {
  dat_cross <- get_first_existing(c("cross_orig", "dat_cross_orig", "cross", "dat_common"))
}

if (is.null(dat_fc) || is.null(dat_bo) || is.null(dat_ro) || is.null(dat_cross)) {
  message("Available objects after loading:")
  print(ls())
  
  stop(
    "Could not identify one or more required analysis datasets: ",
    "dat_fc, dat_bo, dat_ro, dat_cross. ",
    "Please check object names in daten_standardisiert.RData."
  )
}

dat_fc    <- as_tibble(dat_fc)
dat_bo    <- as_tibble(dat_bo)
dat_ro    <- as_tibble(dat_ro)
dat_cross <- as_tibble(dat_cross)

# ------------------------------------------------------------------------------
# Helper: locate possible master/source object for SES_z and outcomes
# ------------------------------------------------------------------------------

possible_sources <- list()

# Important: this script is often sourced repeatedly in the same R session.
# Therefore we must NOT reuse derived objects from earlier runs (for example
# `best_source`, `DATASETS`, or previous audit tables) as outcome sources.
# Otherwise a stale helper object may be selected as "best source", which can
# overwrite valid outcome variables with all-NA columns.
internal_source_exclude <- c(
  "possible_sources", "source_inventory", "source_scores", "best_source",
  "best_source_name", "DATASETS", "dataset_audit", "src_join", "vars_present",
  "dat", "obj", "nm", "subnm", "x"
)

candidate_names <- setdiff(ls(), internal_source_exclude)

for (nm in candidate_names) {
  obj <- get(nm)
  if (is.data.frame(obj)) {
    possible_sources[[nm]] <- as_tibble(obj)
  }
}

# Also inspect list objects such as fragebogen$qnr1, fragebogen$start01.
# Again, avoid internal objects from previous runs.
for (nm in candidate_names) {
  obj <- get(nm)
  if (is.list(obj) && !is.data.frame(obj)) {
    for (subnm in names(obj)) {
      if (is.data.frame(obj[[subnm]])) {
        possible_sources[[paste0(nm, "$", subnm)]] <- as_tibble(obj[[subnm]])
      }
    }
  }
}

source_inventory <- purrr::imap_dfr(possible_sources, function(x, nm) {
  tibble(
    Source = nm,
    Rows = nrow(x),
    Cols = ncol(x),
    Has_CASE = "CASE" %in% names(x),
    Has_REF = "REF" %in% names(x),
    Has_SES_z = "SES_z" %in% names(x),
    Has_OF02_01_num_log = "OF02_01_num_log" %in% names(x),
    Has_OF02_02_num_log = "OF02_02_num_log" %in% names(x),
    Has_OF_Spender = "OF_Spender" %in% names(x),
    Has_OF_Spender_bin = "OF_Spender_bin" %in% names(x),
    Has_OF02_01 = "OF02_01" %in% names(x),
    Has_OF02_02 = "OF02_02" %in% names(x)
  )
})

readr::write_csv(
  source_inventory,
  here::here("outputs", "sem_debug", "csv", "00_source_inventory.csv"),
  na = ""
)

message("Source inventory written to outputs/sem_debug/csv/00_source_inventory.csv")

# ------------------------------------------------------------------------------
# Helper: choose best source for SES_z and outcomes
# ------------------------------------------------------------------------------

score_source <- function(x) {
  score <- 0
  nms <- names(x)

  nonmiss <- function(v) {
    if (!v %in% nms) return(0L)
    sum(!is.na(x[[v]]))
  }

  levels_nonmiss <- function(v) {
    if (!v %in% nms) return(0L)
    dplyr::n_distinct(x[[v]], na.rm = TRUE)
  }

  # Score usable information, not mere column presence.
  if ("SES_z" %in% nms && nonmiss("SES_z") > 0) score <- score + 10
  if ("OF02_01_num_log" %in% nms && nonmiss("OF02_01_num_log") > 0) score <- score + 10
  if ("OF02_02_num_log" %in% nms && nonmiss("OF02_02_num_log") > 0) score <- score + 10
  if ("OF_Spender" %in% nms && nonmiss("OF_Spender") > 0 && levels_nonmiss("OF_Spender") >= 2) score <- score + 20
  if ("OF_Spender_bin" %in% nms && nonmiss("OF_Spender_bin") > 0 && levels_nonmiss("OF_Spender_bin") >= 2) score <- score + 12
  if ("OF02_01" %in% nms && nonmiss("OF02_01") > 0) score <- score + 3
  if ("OF02_02" %in% nms && nonmiss("OF02_02") > 0) score <- score + 3
  if ("CASE" %in% nms) score <- score + 3
  if ("REF" %in% nms) score <- score + 3

  score
}

source_scores <- purrr::imap_dfr(possible_sources, function(x, nm) {
  tibble(
    Source = nm,
    Score = score_source(x),
    Rows = nrow(x),
    Cols = ncol(x)
  )
}) %>%
  arrange(desc(Score), desc(Rows))

readr::write_csv(
  source_scores,
  here::here("outputs", "sem_debug", "csv", "00_source_scores.csv"),
  na = ""
)

# v9 reproducibility option: for the final dissertation/paper pipeline, set
# SEM_DEBUG_CONTEXT_SOURCE_NAME before sourcing this script to avoid workspace-
# dependent heuristic source selection. Example:
# SEM_DEBUG_CONTEXT_SOURCE_NAME <- "spenden_context"  # or "fragebogen$qnr1"
SEM_DEBUG_CONTEXT_SOURCE_NAME <- get0("SEM_DEBUG_CONTEXT_SOURCE_NAME", ifnotfound = NA_character_, inherits = TRUE)

if (!is.na(SEM_DEBUG_CONTEXT_SOURCE_NAME) && nzchar(SEM_DEBUG_CONTEXT_SOURCE_NAME)) {
  if (!SEM_DEBUG_CONTEXT_SOURCE_NAME %in% names(possible_sources)) {
    stop(
      "SEM_DEBUG_CONTEXT_SOURCE_NAME was set to '", SEM_DEBUG_CONTEXT_SOURCE_NAME,
      "', but that object/list data frame was not found among possible sources. Available sources: ",
      paste(names(possible_sources), collapse = ", "),
      call. = FALSE
    )
  }
  best_source_name <- SEM_DEBUG_CONTEXT_SOURCE_NAME
  best_source <- possible_sources[[best_source_name]]
  message("Using explicitly configured SES/outcome source: ", best_source_name)
} else {
  best_source_name <- source_scores$Source[[1]]
  best_source <- possible_sources[[best_source_name]]
  message("Best source for SES/outcomes selected heuristically: ", best_source_name)
  message("For final reproducible runs, set SEM_DEBUG_CONTEXT_SOURCE_NAME explicitly before sourcing 00_config_data.R.")
}

# ------------------------------------------------------------------------------
# Helper: normalise ID columns
# ------------------------------------------------------------------------------

normalise_id_cols <- function(dat) {
  dat <- as_tibble(dat)
  
  if ("CASE" %in% names(dat)) {
    dat <- dat %>% mutate(CASE = as.character(CASE))
  }
  
  if ("REF" %in% names(dat)) {
    dat <- dat %>% mutate(REF = as.character(REF))
  }
  
  dat
}

dat_fc    <- normalise_id_cols(dat_fc)
dat_bo    <- normalise_id_cols(dat_bo)
dat_ro    <- normalise_id_cols(dat_ro)
dat_cross <- normalise_id_cols(dat_cross)
best_source <- normalise_id_cols(best_source)

# ------------------------------------------------------------------------------
# Helper: create log outcomes and binary donor if possible
# ------------------------------------------------------------------------------

clean_binary01 <- function(x) {
  if (inherits(x, "haven_labelled") && requireNamespace("haven", quietly = TRUE)) {
    x <- haven::zap_labels(x)
  }
  if (is.logical(x)) return(as.integer(x))

  # Numeric two-level variables are recoded by their two observed levels.
  # This avoids the previous error where coded values 1/2 both became 1.
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(stats::na.omit(as.numeric(x))))
    if (length(ux) == 2) {
      return(dplyr::case_when(
        is.na(x) ~ NA_integer_,
        as.numeric(x) == ux[[1]] ~ 0L,
        as.numeric(x) == ux[[2]] ~ 1L,
        TRUE ~ NA_integer_
      ))
    }
    return(dplyr::case_when(
      is.na(x) ~ NA_integer_,
      as.numeric(x) == 0 ~ 0L,
      as.numeric(x) == 1 ~ 1L,
      TRUE ~ NA_integer_
    ))
  }

  if (is.factor(x)) x <- as.character(x)
  y_raw <- as.character(x)
  y <- stringr::str_to_lower(stringr::str_trim(y_raw))

  mapped <- dplyr::case_when(
    is.na(y) | y == "" ~ NA_integer_,
    y %in% c("0", "0l", "false", "falsch", "no", "nein", "n", "non-donor", "nondonor", "nichtspender", "nicht-spender", "kein spender", "keine spende") ~ 0L,
    y %in% c("1", "1l", "true", "wahr", "yes", "ja", "j", "donor", "spender", "hat gespendet") ~ 1L,
    stringr::str_detect(y, "nicht|kein|keine|non[-_ ]?donor|no donor|nein") ~ 0L,
    stringr::str_detect(y, "spender|donor|gespendet|spende") ~ 1L,
    suppressWarnings(!is.na(as.numeric(y))) & suppressWarnings(as.numeric(y)) == 0 ~ 0L,
    suppressWarnings(!is.na(as.numeric(y))) & suppressWarnings(as.numeric(y)) == 1 ~ 1L,
    TRUE ~ NA_integer_
  )

  # Last-resort fallback for an otherwise valid two-level categorical variable.
  # The alphabetically first level is coded 0 and the second 1. This is reported
  # in the source audit and prevents a fully usable Boolean from becoming all NA.
  if (sum(!is.na(mapped)) == 0 && dplyr::n_distinct(y, na.rm = TRUE) == 2) {
    lv <- sort(unique(y[!is.na(y)]))
    mapped <- dplyr::case_when(
      is.na(y) ~ NA_integer_,
      y == lv[[1]] ~ 0L,
      y == lv[[2]] ~ 1L,
      TRUE ~ NA_integer_
    )
  }

  mapped
}

prepare_source_vars <- function(src) {
  src <- as_tibble(src)
  
  # Coerce SES
  if ("SES_z" %in% names(src)) {
    src <- src %>% mutate(SES_z = suppressWarnings(as.numeric(SES_z)))
  }
  
  # Create log outcomes only if raw variables exist
  if (!"OF02_01_num_log" %in% names(src) && "OF02_01" %in% names(src)) {
    src <- src %>%
      mutate(
        OF02_01_num = suppressWarnings(as.numeric(OF02_01)),
        OF02_01_num_log = log1p(OF02_01_num)
      )
  }
  
  if (!"OF02_02_num_log" %in% names(src) && "OF02_02" %in% names(src)) {
    src <- src %>%
      mutate(
        OF02_02_num = suppressWarnings(as.numeric(OF02_02)),
        OF02_02_num_log = log1p(OF02_02_num)
      )
  }

  # Preferred binary donor status is OF_Spender.
  # OF_Spender_bin is kept only as compatibility alias for older scripts/outputs.
  if ("OF_Spender" %in% names(src)) {
    src <- src %>% mutate(OF_Spender = clean_binary01(OF_Spender))
  } else if ("OF_Spender_bin" %in% names(src)) {
    src <- src %>% mutate(OF_Spender = clean_binary01(OF_Spender_bin))
  } else {
    if ("OF02_01_num" %in% names(src)) {
      src <- src %>% mutate(OF_Spender = ifelse(!is.na(OF02_01_num) & OF02_01_num > 0, 1L, 0L))
    } else if ("OF02_01_num_log" %in% names(src)) {
      src <- src %>% mutate(OF_Spender = ifelse(!is.na(OF02_01_num_log) & OF02_01_num_log > 0, 1L, 0L))
    }
  }

  if ("OF_Spender" %in% names(src)) {
    src <- src %>% mutate(OF_Spender_bin = OF_Spender)
  }
  
  src
}

best_source <- prepare_source_vars(best_source)

# ------------------------------------------------------------------------------
# Helper: add SES_z and outcomes without reducing existing dat_* columns
# ------------------------------------------------------------------------------

add_context_vars <- function(dat, src, dataset_name = "dat") {
  dat <- as_tibble(dat)

  vars_to_add <- c(
    "SES_z",
    "OF02_01_num_log",
    "OF02_02_num_log",
    "OF_Spender",
    "OF_Spender_bin"
  )

  vars_present <- intersect(vars_to_add, names(src))

  if (length(vars_present) == 0) {
    warning("No SES/outcome variables available in source for ", dataset_name)
    return(dat)
  }

  # Helper: coalesce joined variables into existing variables instead of blindly
  # deleting the existing columns. This preserves valid outcomes already present
  # in dat_* and only fills missing values from the best source.
  coalesce_joined_vars <- function(out) {
    for (v in vars_present) {
      v_src <- paste0(v, ".src")
      if (v %in% names(out) && v_src %in% names(out)) {
        out[[v]] <- dplyr::coalesce(out[[v]], out[[v_src]])
        out[[v_src]] <- NULL
      } else if (!v %in% names(out) && v_src %in% names(out)) {
        names(out)[names(out) == v_src] <- v
      }
    }
    out
  }

  join_done <- FALSE

  if ("REF" %in% names(dat) && "CASE" %in% names(src)) {
    src_join <- src %>%
      select(CASE, all_of(vars_present)) %>%
      distinct(CASE, .keep_all = TRUE) %>%
      rename_with(~ paste0(.x, ".src"), all_of(vars_present))

    dat <- dat %>%
      mutate(REF = as.character(REF)) %>%
      left_join(
        src_join %>% mutate(CASE = as.character(CASE)),
        by = c("REF" = "CASE")
      ) %>%
      coalesce_joined_vars()

    join_done <- TRUE
  }

  if (!join_done && "CASE" %in% names(dat) && "CASE" %in% names(src)) {
    src_join <- src %>%
      select(CASE, all_of(vars_present)) %>%
      distinct(CASE, .keep_all = TRUE) %>%
      rename_with(~ paste0(.x, ".src"), all_of(vars_present))

    dat <- dat %>%
      mutate(CASE = as.character(CASE)) %>%
      left_join(
        src_join %>% mutate(CASE = as.character(CASE)),
        by = "CASE"
      ) %>%
      coalesce_joined_vars()

    join_done <- TRUE
  }

  if (!join_done && "REF" %in% names(dat) && "REF" %in% names(src)) {
    src_join <- src %>%
      select(REF, all_of(vars_present)) %>%
      distinct(REF, .keep_all = TRUE) %>%
      rename_with(~ paste0(.x, ".src"), all_of(vars_present))

    dat <- dat %>%
      mutate(REF = as.character(REF)) %>%
      left_join(
        src_join %>% mutate(REF = as.character(REF)),
        by = "REF"
      ) %>%
      coalesce_joined_vars()

    join_done <- TRUE
  }

  if (!join_done) {
    warning(
      "Could not join SES/outcome variables for ", dataset_name,
      ". No compatible CASE/REF key found. Existing variables were preserved."
    )
  }

  dat
}

dat_fc    <- add_context_vars(dat_fc, best_source, "dat_fc")
dat_bo    <- add_context_vars(dat_bo, best_source, "dat_bo")
dat_ro    <- add_context_vars(dat_ro, best_source, "dat_ro")
dat_cross <- add_context_vars(dat_cross, best_source, "dat_cross")

# ------------------------------------------------------------------------------
# Final coercion
# ------------------------------------------------------------------------------

coerce_context_vars <- function(dat) {
  dat <- as_tibble(dat)
  
  for (v in c("SES_z", "OF02_01_num_log", "OF02_02_num_log")) {
    if (v %in% names(dat)) {
      dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
    }
  }
  
  for (v in c("OF_Spender", "OF_Spender_bin")) {
    if (v %in% names(dat)) {
      dat[[v]] <- clean_binary01(dat[[v]])
    }
  }

  if ("OF_Spender" %in% names(dat)) {
    dat$OF_Spender_bin <- dat$OF_Spender
  } else if ("OF_Spender_bin" %in% names(dat)) {
    dat$OF_Spender <- dat$OF_Spender_bin
  }
  
  dat
}

dat_fc    <- coerce_context_vars(dat_fc)
dat_bo    <- coerce_context_vars(dat_bo)
dat_ro    <- coerce_context_vars(dat_ro)
dat_cross <- coerce_context_vars(dat_cross)

# ------------------------------------------------------------------------------
# Dataset audit
# ------------------------------------------------------------------------------

dataset_audit <- tibble(
  Dataset = c("fc", "bo", "ro", "cross"),
  Rows = c(nrow(dat_fc), nrow(dat_bo), nrow(dat_ro), nrow(dat_cross)),
  Columns = c(ncol(dat_fc), ncol(dat_bo), ncol(dat_ro), ncol(dat_cross)),
  Has_SES_z = c(
    "SES_z" %in% names(dat_fc),
    "SES_z" %in% names(dat_bo),
    "SES_z" %in% names(dat_ro),
    "SES_z" %in% names(dat_cross)
  ),
  SES_nonmissing = c(
    if ("SES_z" %in% names(dat_fc)) sum(!is.na(dat_fc$SES_z)) else NA_integer_,
    if ("SES_z" %in% names(dat_bo)) sum(!is.na(dat_bo$SES_z)) else NA_integer_,
    if ("SES_z" %in% names(dat_ro)) sum(!is.na(dat_ro$SES_z)) else NA_integer_,
    if ("SES_z" %in% names(dat_cross)) sum(!is.na(dat_cross$SES_z)) else NA_integer_
  ),
  Has_OF02_01_num_log = c(
    "OF02_01_num_log" %in% names(dat_fc),
    "OF02_01_num_log" %in% names(dat_bo),
    "OF02_01_num_log" %in% names(dat_ro),
    "OF02_01_num_log" %in% names(dat_cross)
  ),
  Has_OF02_02_num_log = c(
    "OF02_02_num_log" %in% names(dat_fc),
    "OF02_02_num_log" %in% names(dat_bo),
    "OF02_02_num_log" %in% names(dat_ro),
    "OF02_02_num_log" %in% names(dat_cross)
  ),
  Has_OF_Spender = c(
    "OF_Spender" %in% names(dat_fc),
    "OF_Spender" %in% names(dat_bo),
    "OF_Spender" %in% names(dat_ro),
    "OF_Spender" %in% names(dat_cross)
  ),
  OF_Spender_nonmissing = c(
    if ("OF_Spender" %in% names(dat_fc)) sum(!is.na(dat_fc$OF_Spender)) else NA_integer_,
    if ("OF_Spender" %in% names(dat_bo)) sum(!is.na(dat_bo$OF_Spender)) else NA_integer_,
    if ("OF_Spender" %in% names(dat_ro)) sum(!is.na(dat_ro$OF_Spender)) else NA_integer_,
    if ("OF_Spender" %in% names(dat_cross)) sum(!is.na(dat_cross$OF_Spender)) else NA_integer_
  ),
  OF_Spender_levels = c(
    if ("OF_Spender" %in% names(dat_fc)) dplyr::n_distinct(dat_fc$OF_Spender, na.rm = TRUE) else NA_integer_,
    if ("OF_Spender" %in% names(dat_bo)) dplyr::n_distinct(dat_bo$OF_Spender, na.rm = TRUE) else NA_integer_,
    if ("OF_Spender" %in% names(dat_ro)) dplyr::n_distinct(dat_ro$OF_Spender, na.rm = TRUE) else NA_integer_,
    if ("OF_Spender" %in% names(dat_cross)) dplyr::n_distinct(dat_cross$OF_Spender, na.rm = TRUE) else NA_integer_
  ),
  Has_OF_Spender_bin = c(
    "OF_Spender_bin" %in% names(dat_fc),
    "OF_Spender_bin" %in% names(dat_bo),
    "OF_Spender_bin" %in% names(dat_ro),
    "OF_Spender_bin" %in% names(dat_cross)
  )
)

readr::write_csv(
  dataset_audit,
  here::here("outputs", "sem_debug", "csv", "01_dataset_audit.csv"),
  na = ""
)
# ------------------------------------------------------------------------------
# Dataset registry for downstream scripts
# ------------------------------------------------------------------------------

DATASETS <- list(
  fc    = dat_fc,
  bo    = dat_bo,
  ro    = dat_ro,
  cross = dat_cross
)

message("DATASETS registry created: ", paste(names(DATASETS), collapse = ", "))
print(dataset_audit)

message("00_config_data.R ready.")