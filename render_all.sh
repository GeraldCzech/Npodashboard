#!/bin/bash
# ==============================================================================
# render_all.sh  —  Render all Quarto documents in sequence
# ==============================================================================
# Usage:
#   chmod +x render_all.sh
#   ./render_all.sh                    # render all, log to render_all.log
#   nohup ./render_all.sh &            # background, survives disconnect
#   tail -f render_all.log             # follow progress
# ==============================================================================

set -e

LOG="render_all.log"
QUARTO=$(which quarto 2>/dev/null || echo "/opt/quarto/bin/quarto")

# ---------------------------------------------------------------------------
# Deprioritise competing R/RStudio processes so this render gets more CPU.
# Finds all R and RStudio-server processes owned by the current user and
# renices them to 19. Non-fatal if some PIDs have already exited.
# ---------------------------------------------------------------------------
renice_rstudio() {
  local PIDS
  PIDS=$(pgrep -u "$USER" -f "rstudio-server|/usr/lib/R/bin" 2>/dev/null || true)
  if [ -n "$PIDS" ]; then
    echo "Renicing RStudio/R background processes to nice=19: $(echo $PIDS | tr '\n' ' ')" | tee -a "$LOG"
    # shellcheck disable=SC2086
    renice 19 $PIDS >> "$LOG" 2>&1 || true
  else
    echo "No competing RStudio/R processes found." | tee -a "$LOG"
  fi
}

# ---------------------------------------------------------------------------
render_doc() {
  local DOC="$1"
  echo "" | tee -a "$LOG"
  echo "--- $(date '+%H:%M:%S') Rendering: $DOC ---" | tee -a "$LOG"
  "$QUARTO" render "$DOC" >> "$LOG" 2>&1 \
    && echo "    OK: $DOC" | tee -a "$LOG" \
    || { echo "    FAILED: $DOC" | tee -a "$LOG"; return 1; }
}

# ---------------------------------------------------------------------------
echo "============================================================" | tee -a "$LOG"
echo "render_all.sh started at $(date '+%Y-%m-%d %H:%M:%S')"       | tee -a "$LOG"
echo "Quarto: $QUARTO ($($QUARTO --version 2>/dev/null))"           | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"

# Deprioritise RStudio/R background jobs before starting
renice_rstudio

# Render in order
render_doc "analysis/01_main_analysis.qmd"
render_doc "analysis/02_supplements_addendum.qmd"
# render_doc "analysis/03_bootstrap_results.qmd"  # activate after run_bootstrap.R
# render_doc "analysis/04_addon_missing_values.qmd"  # missing values add-on

echo "" | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"
echo "All done at $(date '+%Y-%m-%d %H:%M:%S')"                     | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"
# Restore normal priority for RStudio/R background processes
echo "" | tee -a "$LOG"
echo "Restoring RStudio/R background processes to nice=0..." | tee -a "$LOG"
RESTORE_PIDS=$(pgrep -u "$USER" -f "rstudio-server|/usr/lib/R/bin" 2>/dev/null || true)
if [ -n "$RESTORE_PIDS" ]; then
  # shellcheck disable=SC2086
  renice 0 $RESTORE_PIDS >> "$LOG" 2>&1 || true
  echo "Restored: $(echo $RESTORE_PIDS | tr '\n' ' ')" | tee -a "$LOG"
else
  echo "No processes to restore." | tee -a "$LOG"
fi
