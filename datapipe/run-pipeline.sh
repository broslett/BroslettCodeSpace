#!/usr/bin/env bash
# Run a saved pipeline without the interface:  ./run-pipeline.sh <name> [options]
set -euo pipefail
cd "$(dirname "$0")"
exec Rscript run_pipeline.R "$@"
