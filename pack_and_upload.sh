#!/bin/bash
# ==============================================================================
# pack_and_upload.sh  —  Tarball aller Analyse-Daten + Cloud-Upload
# ==============================================================================
# Auf dem Server ausführen:
#   chmod +x pack_and_upload.sh
#   ./pack_and_upload.sh
#
# Ergebnis: ~/npodashboard_backup_DATUM.tar.gz  + optionaler Cloud-Upload
# ==============================================================================

set -e
DATE=$(date +%Y%m%d_%H%M)
ARCHIVE="$HOME/npodashboard_backup_${DATE}.tar.gz"
REPO="$HOME/Npodashboard"
DATA_PRIMARY="/home/gerald/10787172/scripts/research2/output"
BOOTSTRAP_OUT="/home/gerald/10787172/outputs/sem_debug/supplements"
LOG="$HOME/pack_upload_${DATE}.log"

echo "============================================" | tee "$LOG"
echo "pack_and_upload.sh gestartet: $(date)"        | tee -a "$LOG"
echo "Archiv: $ARCHIVE"                             | tee -a "$LOG"
echo "============================================" | tee -a "$LOG"

# --- Dateien einsammeln -------------------------------------------------------
INCLUDES=()

# 1. Primäre Analysedaten (daten_standardisiert, fragebogen)
for f in \
    "$DATA_PRIMARY/daten_standardisiert.RData" \
    "$DATA_PRIMARY/fragebogen.rds" \
    "$DATA_PRIMARY/analysis.rds"; do
  [ -f "$f" ] && INCLUDES+=("$f") && echo "  + $f" | tee -a "$LOG"
done

# 2. Bootstrap-Output-CSVs
if [ -d "$BOOTSTRAP_OUT" ]; then
  while IFS= read -r -d '' f; do
    INCLUDES+=("$f"); echo "  + $f" | tee -a "$LOG"
  done < <(find "$BOOTSTRAP_OUT" -name "*.csv" -print0)
fi

# 3. Compendium-Outputs (gerechnete Analyse-CSVs, gerenderte HTMLs)
for d in \
    "$REPO/outputs/csv" \
    "$REPO/outputs/tables" \
    "$REPO/outputs/rendered"; do
  [ -d "$d" ] && INCLUDES+=("$d") && echo "  + $d/" | tee -a "$LOG"
done

# 4. Gerenderte HTML-Dokumente (falls direkt im analysis/ Ordner)
while IFS= read -r -d '' f; do
  INCLUDES+=("$f"); echo "  + $f" | tee -a "$LOG"
done < <(find "$REPO/analysis" -name "*.html" -print0 2>/dev/null)

# 5. Manuskript (neueste .docx)
while IFS= read -r -d '' f; do
  INCLUDES+=("$f"); echo "  + $f" | tee -a "$LOG"
done < <(find "$HOME" -maxdepth 3 -name "Article_v1_9*.docx" -print0 2>/dev/null)

# 6. Das ganze Repo (ohne Cache und große Output-Binaries)
INCLUDES+=("$REPO")

# --- Tarball erstellen --------------------------------------------------------
echo "" | tee -a "$LOG"
echo "Erstelle Tarball..." | tee -a "$LOG"
tar -czf "$ARCHIVE" \
  --exclude="$REPO/.git" \
  --exclude="$REPO/analysis/*_cache" \
  --exclude="$REPO/analysis/*_files" \
  --exclude="$REPO/outputs/figures" \
  --exclude="*.so" --exclude="*.o" \
  "${INCLUDES[@]}" 2>> "$LOG"

SIZE=$(du -sh "$ARCHIVE" | cut -f1)
echo "Archiv erstellt: $ARCHIVE ($SIZE)" | tee -a "$LOG"

# --- Cloud-Upload (wähle eine Methode) ----------------------------------------
echo "" | tee -a "$LOG"
echo "--- Cloud-Upload ---" | tee -a "$LOG"

# METHODE A: rclone (Nextcloud / OneDrive / Google Drive / Dropbox)
# Voraussetzung: rclone konfiguriert mit  rclone config
# Remote-Name anpassen (z.B. "onedrive", "gdrive", "nextcloud"):
RCLONE_REMOTE="${RCLONE_REMOTE:-}"   # leer = kein rclone-Upload
RCLONE_PATH="${RCLONE_PATH:-npodashboard_backups}"

if [ -n "$RCLONE_REMOTE" ] && command -v rclone &>/dev/null; then
  echo "rclone Upload -> $RCLONE_REMOTE:$RCLONE_PATH/" | tee -a "$LOG"
  rclone copy "$ARCHIVE" "$RCLONE_REMOTE:$RCLONE_PATH/" \
    --progress 2>&1 | tee -a "$LOG"
  echo "Upload OK" | tee -a "$LOG"
else
  echo "rclone nicht konfiguriert — Archiv liegt lokal: $ARCHIVE" | tee -a "$LOG"
  echo "" | tee -a "$LOG"
  echo "Upload-Optionen:" | tee -a "$LOG"
  echo "  Nextcloud/WU:  rclone copy $ARCHIVE nextcloud:npodashboard_backups/" | tee -a "$LOG"
  echo "  Google Drive:  rclone copy $ARCHIVE gdrive:npodashboard_backups/" | tee -a "$LOG"
  echo "  OneDrive:      rclone copy $ARCHIVE onedrive:npodashboard_backups/" | tee -a "$LOG"
  echo "  scp lokal:     scp gerald@SERVER:$ARCHIVE ~/Downloads/" | tee -a "$LOG"
fi

# METHODE B: direktes scp auf lokalen Mac (falls SSH-Verbindung umgekehrt möglich)
# scp "$ARCHIVE" deinmacuser@dein-mac.local:~/Downloads/

echo "" | tee -a "$LOG"
echo "Fertig: $(date)" | tee -a "$LOG"
echo "Archiv: $ARCHIVE ($SIZE)" | tee -a "$LOG"
