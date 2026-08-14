#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# make_zip.sh -- build a self-contained distribution archive.
#
#   tools/make_zip.sh [version]
#
# Produces dist/datapipe-<version>.zip containing the application, its example
# data, tests, setup helpers and documentation. Run artefacts (output/,
# uploads/) and anything machine-specific are excluded, so the archive is the
# same wherever it is built.
# ---------------------------------------------------------------------------
set -euo pipefail

cd "$(dirname "$0")/.."
APP_DIR="$(pwd)"
VERSION="${1:-1.0.0}"
STAGE="$(mktemp -d)"
OUT_DIR="$APP_DIR/dist"
OUT="$OUT_DIR/datapipe-$VERSION.zip"
trap 'rm -rf "$STAGE"' EXIT

command -v zip >/dev/null 2>&1 || { echo "zip is not installed."; exit 1; }

mkdir -p "$STAGE/datapipe" "$OUT_DIR"

# Everything that belongs in the archive, listed explicitly so nothing
# unexpected can be swept in.
INCLUDE=(
  app.R
  run_pipeline.R
  verify.R
  README.md
  SECURITY.md
  start-app.sh start-app.bat
  run-pipeline.sh run-pipeline.bat
  R
  locale
  pipelines
  tests
  examples
  setup
  tools
)

for item in "${INCLUDE[@]}"; do
  if [ -e "$item" ]; then
    cp -R "$item" "$STAGE/datapipe/"
  else
    echo "warning: $item is missing, skipping" >&2
  fi
done

# Regenerable or machine-specific things never ship.
rm -rf "$STAGE/datapipe/output" "$STAGE/datapipe/uploads" "$STAGE/datapipe/dist"
find "$STAGE/datapipe" \( -name '.Rhistory' -o -name '.RData' -o -name '.DS_Store' \
     -o -name '*.Rproj.user' -o -name '__MACOSX' \) -exec rm -rf {} + 2>/dev/null || true

# The exports folder should exist but start empty.
mkdir -p "$STAGE/datapipe/output"
cat > "$STAGE/datapipe/output/.gitkeep" <<'KEEP'
Exports land here by default. Safe to empty at any time.
KEEP

chmod +x "$STAGE/datapipe"/*.sh "$STAGE/datapipe/tools"/*.sh 2>/dev/null || true

cat > "$STAGE/datapipe/VERSION" <<EOF
datapipe $VERSION
built $(date -u +%Y-%m-%dT%H:%M:%SZ)
EOF

rm -f "$OUT"
( cd "$STAGE" && zip -qr "$OUT" datapipe -x '*.git*' )

echo "built  $OUT"
echo "size   $(du -h "$OUT" | cut -f1)"
echo "files  $(unzip -Z1 "$OUT" | wc -l)"
echo
echo "Contents:"
unzip -Z1 "$OUT" | sed 's|^|  |' | head -40
