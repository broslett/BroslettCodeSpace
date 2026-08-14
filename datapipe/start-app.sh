#!/usr/bin/env bash
# Launch the Data Pipeline Builder. Binds to 127.0.0.1 only -- see SECURITY.md.
set -euo pipefail
cd "$(dirname "$0")"
command -v Rscript >/dev/null 2>&1 || { echo "Rscript not found. Install R 4.1 or newer."; exit 1; }
PORT="${DATAPIPE_PORT:-8080}"
echo "Starting on http://127.0.0.1:${PORT}  (Ctrl-C to stop)"
DATAPIPE_PORT="$PORT" exec Rscript app.R
