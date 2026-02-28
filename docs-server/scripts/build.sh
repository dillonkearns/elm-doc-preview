#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SEED_DIR="$SCRIPT_DIR/../elm-home-seed"

rm -rf "$SEED_DIR"
bash "$SCRIPT_DIR/../benchmark/generate-seed.sh" "$SEED_DIR"

# Apply "stripped" strategy — remove docs/readme/license from packages
find "$SEED_DIR/0.19.1/packages" -mindepth 3 \
  \( -name "docs.json" -o -name "README.md" -o -name "LICENSE" \) -delete

# Vercel requires a non-empty output directory after buildCommand
mkdir -p "$SCRIPT_DIR/../public"
echo '<!-- placeholder -->' > "$SCRIPT_DIR/../public/index.html"
