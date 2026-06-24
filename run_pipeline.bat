@echo off
REM ===========================================================================
REM run_pipeline.bat  —  Gesamte Pipeline auf Windows
REM HINWEIS: Aus cmd.exe starten (nicht PowerShell):
REM   Win+R -> cmd -> cd C:\...\Npodashboard -> run_pipeline.bat
REM ===========================================================================

REM ── KONFIGURATION (hier anpassen) ──────────────────────────────────────────
set DATA_DIR=C:\Users\Gerald\npodashboard\home\gerald\10787172\scripts\research2\output
set BOOT_B=1000
set BOOT_NCORES=3
set RUN_BOOTSTRAP=0
REM ───────────────────────────────────────────────────────────────────────────

cd /d "%~dp0"
set CBE_DATA_DIR=%DATA_DIR%

REM --- R-Bibliothek auf lokalen Pfad (kein OneDrive, keine Umlaute) ----------
set R_LIBS_USER=C:\Rlib
if not exist C:\Rlib md C:\Rlib

REM --- Rscript.exe automatisch finden ----------------------------------------
set RSCRIPT=
REM Zuerst bekannten R 4.6.0 Pfad prüfen
if exist "%LOCALAPPDATA%\Programs\R\R-4.6.0\bin\Rscript.exe" (
    set RSCRIPT=%LOCALAPPDATA%\Programs\R\R-4.6.0\bin\Rscript.exe
)
if "%RSCRIPT%"=="" where Rscript >nul 2>&1 && set RSCRIPT=Rscript

if "%RSCRIPT%"=="" (
    for /d %%v in ("%ProgramFiles%\R\R-*") do set RSCRIPT=%%v\bin\Rscript.exe
)
if "%RSCRIPT%"=="" (
    for /d %%v in ("%ProgramFiles(x86)%\R\R-*") do set RSCRIPT=%%v\bin\Rscript.exe
)
if "%RSCRIPT%"=="" (
    for /d %%v in ("%LOCALAPPDATA%\Programs\R\R-*") do set RSCRIPT=%%v\bin\Rscript.exe
)
if "%RSCRIPT%"=="" (
    echo FEHLER: R nicht gefunden. Bitte R installieren:
    echo   https://cran.r-project.org/bin/windows/base/
    echo Oder Pfad manuell setzen:  set RSCRIPT=C:\Programme\R\R-4.x.x\bin\Rscript.exe
    pause & exit /b 1
)
echo Rscript gefunden: %RSCRIPT%

REM --- Quarto finden ---------------------------------------------------------
set QUARTO=
where quarto >nul 2>&1 && set QUARTO=quarto
if "%QUARTO%"=="" (
    if exist "%LOCALAPPDATA%\Programs\Quarto\bin\quarto.cmd" (
        set QUARTO=%LOCALAPPDATA%\Programs\Quarto\bin\quarto.cmd
    )
)
if "%QUARTO%"=="" (
    echo WARNUNG: Quarto nicht gefunden - nur R-Skripte werden ausgefuehrt.
    echo Quarto installieren: https://quarto.org/docs/get-started/
)

echo.
echo ============================================
echo  NPO Dashboard - Gesamte Pipeline
echo  Daten:     %DATA_DIR%
echo  Bootstrap: %RUN_BOOTSTRAP% (B=%BOOT_B%)
echo ============================================
echo.

REM --- Schritt 1: Umgebung laden ---------------------------------------------
echo [1/4] Lade Module und Daten...
"%RSCRIPT%" start_local.R || goto :error

REM --- Schritt 2: Hauptanalyse -----------------------------------------------
echo.
echo [2/4] Rendere 01_main_analysis.qmd...
"%RSCRIPT%" -e "Sys.setenv(R_LIBS_USER='C:/Rlib'); .libPaths(c('C:/Rlib',.libPaths())); rmarkdown::render('analysis/01_main_analysis.qmd', output_dir='outputs/rendered')" || goto :error

echo.
echo [3/4] Rendere 02_supplements_addendum.qmd...
"%RSCRIPT%" -e "Sys.setenv(R_LIBS_USER='C:/Rlib'); .libPaths(c('C:/Rlib',.libPaths())); rmarkdown::render('analysis/02_supplements_addendum.qmd', output_dir='outputs/rendered')" || goto :error

REM --- Schritt 4: Bootstrap --------------------------------------------------
if "%RUN_BOOTSTRAP%"=="1" (
    echo.
    echo [4/4] Bootstrap ^(B=%BOOT_B%, %BOOT_NCORES% Kerne^) - kann Stunden dauern...
    set BOOT_B=%BOOT_B%
    set BOOT_NCORES=%BOOT_NCORES%
    "%RSCRIPT%" run_bootstrap.R || goto :error
    if not "%QUARTO%"=="" (
        "%QUARTO%" render analysis/03_bootstrap_results.qmd || goto :error
    )
) else (
    echo [4/4] Bootstrap uebersprungen ^(RUN_BOOTSTRAP=0^).
)

echo.
echo ============================================
echo  Fertig!
echo ============================================
goto :end

:error
echo.
echo FEHLER - Pipeline abgebrochen ^(Code: %ERRORLEVEL%^)

:end
pause
