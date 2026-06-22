#!/bin/bash
# ==============================================================================
# render_all.sh  —  Render all Quarto documents in sequence
# ==============================================================================
# Usage:
#   chmod +x render_all.sh
#   ./render_all.sh                        # render all, log to render_all.log
#   ./render_all.sh 2>&1 | tee my.log     # also print to terminal
#   nohup ./render_all.sh &               # background, survives disconnect
# ==============================================================================

set -e   # stop on first error (remove if you want to continue despite failures)

LOG="render_all.log"
QUARTO=$(which quarto 2>/dev/null || echo "/opt/quarto/bin/quarto")
START=$(date "+%Y-%m-%d %H:%M:%S")

echo "============================================================" | tee -a "$LOG"
echo "render_all.sh started at $START"                              | tee -a "$LOG"
echo "Quarto: $QUARTO ($($QUARTO --version 2>/dev/null))"          | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"

render_doc() {
  local DOC="$1"
  echo ""                                           | tee -a "$LOG"
  echo "--- $(date '+%H:%M:%S') Rendering: $DOC ---" | tee -a "$LOG"
  "$QUARTO" render "$DOC" >> "$LOG" 2>&1 \
    && echo "    OK: $DOC" | tee -a "$LOG" \
    || { echo "    FAILED: $DOC" | tee -a "$LOG"; return 1; }
}

# Render in order — edit list as needed
render_doc "analysis/01_main_analysis.qmd"
render_doc "analysis/02_supplements_addendum.qmd"
# render_doc "analysis/03_bootstrap_results.qmd"   # only after run_bootstrap.R

echo ""                                                             | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"
echo "All done at $(date '+%Y-%m-%d %H:%M:%S')"                    | tee -a "$LOG"
echo "============================================================" | tee -a "$LOG"
