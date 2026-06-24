# ==============================================================================
# start_local.R  —  Lokales Starten der Analyse auf Windows
# ==============================================================================
# Verwendung:
#   1. Dieses Skript in das entpackte Npodashboard-Verzeichnis legen
#   2. In RStudio öffnen und Zeile für Zeile ausführen
#   ODER in einer R-Konsole:
#      Rscript start_local.R
#
# Voraussetzung: Tarball entpackt, Pfad zu den .RData/.rds-Dateien bekannt.
# ==============================================================================

# --- 1. Pakete installieren (einmalig) ----------------------------------------
required <- c(
  "here", "dplyr", "tidyr", "tibble", "purrr", "readr",
  "stringr", "forcats", "glue", "haven",
  "lavaan", "semTools", "psych",
  "lme4", "gt", "knitr", "kableExtra", "ggplot2",
  "rmarkdown", "quarto"
)
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installiere fehlende Pakete: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# --- 2. Datenpfad setzen ------------------------------------------------------
# Passe diesen Pfad auf dein Windows-System an.
# Beispiel: "C:/Users/Gerald/Documents/npodashboard_data"
# Der Ordner muss enthalten:
#   - daten_standardisiert.RData
#   - fragebogen.rds  (optional)
#   - analysis.rds    (optional)

DATA_DIR_LOCAL <- readline(
  "Pfad zum Datenordner (Enter = aktuelles Verzeichnis): "
)
if (!nzchar(trimws(DATA_DIR_LOCAL))) {
  DATA_DIR_LOCAL <- getwd()
}
DATA_DIR_LOCAL <- normalizePath(DATA_DIR_LOCAL, winslash = "/", mustWork = FALSE)

# Umgebungsvariable setzen — wird von R/00_paths.R gelesen
Sys.setenv(CBE_DATA_DIR = DATA_DIR_LOCAL)
message("CBE_DATA_DIR = ", DATA_DIR_LOCAL)

# --- 3. Projektverzeichnis setzen ---------------------------------------------
# Wenn dieses Skript im Npodashboard-Verzeichnis liegt, stimmt der Pfad.
if (file.exists("R/00_paths.R")) {
  setwd(normalizePath(".", winslash = "/"))
  message("Arbeitsverzeichnis: ", getwd())
} else {
  message("HINWEIS: Skript liegt nicht im Npodashboard-Ordner.")
  message("Bitte manuell setwd('/pfad/zu/Npodashboard') ausführen.")
}

# --- 4. Module laden ----------------------------------------------------------
message("\nLade Module...")
source("R/00_paths.R")
source("R/01_packages.R")
source("R/12_lavaan_models.R")
source("R/11_fit_helpers.R")
source("R/20_fit_cfa.R")
source("R/21_fit_sem.R")
source("R/40_tables.R")
source("R/10_datasets.R")   # lädt daten_standardisiert.RData → DATASETS

if (exists("DATASETS")) {
  message("\nERFOLG. DATASETS geladen:")
  message("  fc:    ", nrow(DATASETS$fc),    " Zeilen, ", ncol(DATASETS$fc),    " Spalten")
  message("  bo:    ", nrow(DATASETS$bo),    " Zeilen, ", ncol(DATASETS$bo),    " Spalten")
  message("  ro:    ", nrow(DATASETS$ro),    " Zeilen, ", ncol(DATASETS$ro),    " Spalten")
  message("  cross: ", nrow(DATASETS$cross), " Zeilen, ", ncol(DATASETS$cross), " Spalten")
} else {
  warning("DATASETS nicht geladen — Pfad prüfen!")
}

# --- 5. Quarto-Dokument rendern (optional) ------------------------------------
# Auskommentieren und ausführen wenn Quarto installiert ist:

# quarto::quarto_render("analysis/01_main_analysis.qmd")
# quarto::quarto_render("analysis/02_supplements_addendum.qmd")
# quarto::quarto_render("analysis/03_bootstrap_results.qmd")

message("\nBereit. DATASETS ist im Workspace verfügbar.")
message("Zum Rendern: quarto::quarto_render('analysis/01_main_analysis.qmd')")
