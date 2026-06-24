@echo off
REM ===========================================================================
REM start_local.bat  —  Analyse starten auf Windows ohne RStudio
REM ===========================================================================
REM Doppelklick auf diese Datei startet die Analyse.
REM Voraussetzung: R ist installiert (https://cran.r-project.org)
REM ===========================================================================

REM Pfad zu den entpackten Daten — HIER ANPASSEN
set DATA_DIR=C:\Users\Gerald\npodashboard\home\gerald\10787172\scripts\research2\output

REM Arbeitsverzeichnis = Ordner wo diese .bat liegt
cd /d "%~dp0"

echo ===========================================
echo  NPO Dashboard Analyse
echo  Daten: %DATA_DIR%
echo ===========================================

REM Pakete installieren + Module laden + DATASETS aufbauen
Rscript -e ^
  "Sys.setenv(CBE_DATA_DIR='%DATA_DIR%'); source('start_local.R')"

pause
