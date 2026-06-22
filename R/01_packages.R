# ==============================================================================
# 01_packages.R  —  Package loader
# ------------------------------------------------------------------------------
# Keep this list in sync with renv.lock. Install missing packages with:
#   renv::restore()    (preferred, reproducible)
# or, ad hoc:
#   install.packages(setdiff(.cbe_pkgs, rownames(installed.packages())))
# ==============================================================================

.cbe_pkgs <- c(
  # data wrangling
  "here", "dplyr", "tidyr", "tibble", "purrr", "readr", "stringr", "forcats",
  # SEM / psychometrics
  "lavaan", "semTools", "psych",
  # multilevel / ICC
  "lme4", "performance",
  # tables / output
  "gt", "knitr", "kableExtra",
  # plotting
  "ggplot2",
  # labelled survey data
  "haven"
)

.cbe_load <- function(pkgs = .cbe_pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) {
    warning("Missing packages (install via renv::restore()): ",
            paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(lapply(intersect(pkgs, rownames(installed.packages())),
                   function(p) suppressPackageStartupMessages(
                     library(p, character.only = TRUE))))
}

.cbe_load()
