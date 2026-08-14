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

# Mail gateways, download scanners and corporate proxies routinely strip or
# quarantine .bat files. They ship with a .txt suffix so the archive travels
# intact; removing that suffix restores them. Nothing else refers to them by
# name, so an archive whose .bat files are never restored still works fully --
# just via the .sh launchers or a direct Rscript call.
for b in "$STAGE/datapipe"/*.bat; do
  [ -e "$b" ] || continue
  mv "$b" "$b.txt"
done

if ls "$STAGE/datapipe"/*.bat.txt >/dev/null 2>&1; then
  cat > "$STAGE/datapipe/WINDOWS-README.txt" <<'NOTE'
Windows launchers
=================

The two Windows launcher scripts in this folder end in .bat.txt:

    start-app.bat.txt
    run-pipeline.bat.txt

That extra .txt is only there so the archive survives email filters and
download scanners, which commonly block or strip .bat files. The files are
plain text and were never executable in transit.

To use them, delete the .txt from the end of each name:

    start-app.bat.txt     ->  start-app.bat
    run-pipeline.bat.txt  ->  run-pipeline.bat

In Windows Explorer you may need to turn on
  View > Show > File name extensions
first, otherwise the .txt is hidden and the rename appears to do nothing.

Or from a Command Prompt in this folder:

    ren start-app.bat.txt start-app.bat
    ren run-pipeline.bat.txt run-pipeline.bat

You do not have to do any of this. The launchers are a convenience only --
everything works equally well by calling R directly:

    Rscript app.R                                 (start the web interface)
    Rscript run_pipeline.R example_monthly_sales  (run a saved pipeline)
    Rscript verify.R                              (check the installation)

See README.md for the full guide and SECURITY.md for how to confirm the
application makes no network connections.
NOTE
fi

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
