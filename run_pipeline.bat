@echo off
REM ===========================================================================
REM run_pipeline.bat  —  Gesamte Pipeline auf Windows
REM ===========================================================================
REM Reihenfolge:
REM   1. Daten laden (daten_standardisiert.RData)
REM   2. Quarto rendern: 01_main_analysis + 02_supplements + 03_bootstrap
REM   3. Bootstrap (optional, dauert Stunden — standardmässig deaktiviert)
REM ===========================================================================

REM ── KONFIGURATION (hier anpassen) ──────────────────────────────────────────
set DATA_DIR=C:\Users\Gerald\npodashboard\home\gerald\10787172\scripts\research2\output
set BOOT_B=1000
set BOOT_NCORES=3
REM Bootstrap mitrechnen? 1 = ja (dauert Stunden), 0 = nein
set RUN_BOOTSTRAP=0
REM ───────────────────────────────────────────────────────────────────────────

cd /d "%~dp0"
set CBE_DATA_DIR=%DATA_DIR%

echo.
echo ============================================
echo  NPO Dashboard – Gesamte Pipeline
echo  Daten: %DATA_DIR%
echo  Bootstrap: %RUN_BOOTSTRAP% (B=%BOOT_B%)
echo ============================================
echo.

REM --- Schritt 1: Umgebung prüfen -------------------------------------------
echo [1/4] Pruefe Umgebung...
Rscript -e "source('start_local.R')" || goto :error

REM --- Schritt 2: Hauptanalyse rendern --------------------------------------
echo.
echo [2/4] Rendere 01_main_analysis.qmd...
quarto render analysis/01_main_analysis.qmd || goto :error

REM --- Schritt 3: Supplement rendern ----------------------------------------
echo.
echo [3/4] Rendere 02_supplements_addendum.qmd...
quarto render analysis/02_supplements_addendum.qmd || goto :error

REM --- Schritt 4: Bootstrap (optional) + Bootstrap-Ergebnisse ---------------
if "%RUN_BOOTSTRAP%"=="1" (
    echo.
    echo [4/4] Starte Bootstrap ^(B=%BOOT_B%, %BOOT_NCORES% Kerne^)...
    echo       Kann mehrere Stunden dauern. Fenster offen lassen.
    Rscript -e ^
      "Sys.setenv(CBE_DATA_DIR='%DATA_DIR%', BOOT_B='%BOOT_B%', BOOT_NCORES='%BOOT_NCORES%'); source('run_bootstrap.R')" ^
      || goto :error
    echo.
    echo Rendere 03_bootstrap_results.qmd...
    quarto render analysis/03_bootstrap_results.qmd || goto :error
) else (
    echo.
    echo [4/4] Bootstrap uebersprungen ^(RUN_BOOTSTRAP=0^).
    echo       Bootstrap-CSV bereits vorhanden? Dann rendern mit:
    echo       quarto render analysis/03_bootstrap_results.qmd
)

echo.
echo ============================================
echo  Fertig. Outputs in outputs\rendered\
echo ============================================
goto :end

:error
echo.
echo FEHLER aufgetreten – Pipeline abgebrochen.
echo Letzter Fehlercode: %ERRORLEVEL%

:end
pause
