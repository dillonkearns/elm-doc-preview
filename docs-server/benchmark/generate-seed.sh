#!/usr/bin/env bash
#
# generate-seed.sh — Create a fresh ELM_HOME seed by compiling the test project.
#
# Usage: bash generate-seed.sh <output-dir> [--with-community]
#
# The output directory will contain a fully populated ELM_HOME after
# running elm make on the benchmark test project. If --with-community
# is passed, popular community packages are also fetched.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/test-project"
DOCS_SERVER_DIR="$SCRIPT_DIR/.."

# --- Resolve elm binary ---
ELM="${ELM_BIN:-}"
if [ -z "$ELM" ]; then
    # Try docs-server/node_modules first, then root node_modules
    for candidate in \
        "$DOCS_SERVER_DIR/node_modules/elm/bin/elm" \
        "$DOCS_SERVER_DIR/../node_modules/elm/bin/elm"; do
        if [ -x "$candidate" ]; then
            ELM="$(cd "$(dirname "$candidate")" && pwd)/$(basename "$candidate")"
            break
        fi
    done
fi

if [ -z "$ELM" ]; then
    echo "Error: elm binary not found. Run 'npm install' in docs-server/ first." >&2
    exit 1
fi

echo "Using elm: $ELM ($($ELM --version))"

# --- Parse arguments ---
OUTPUT_DIR="${1:-}"
WITH_COMMUNITY=false

for arg in "$@"; do
    case "$arg" in
        --with-community) WITH_COMMUNITY=true ;;
    esac
done

if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: bash generate-seed.sh <output-dir> [--with-community]" >&2
    exit 1
fi

# --- Create output directory ---
mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)"

# --- Build test project with fresh ELM_HOME ---
echo "Building test project into seed: $OUTPUT_DIR"

# Use a temporary working copy so we don't pollute the test-project dir
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

cp "$PROJECT_DIR/elm.json" "$WORK_DIR/elm.json"
mkdir -p "$WORK_DIR/src"
cp "$PROJECT_DIR/src/Main.elm" "$WORK_DIR/src/Main.elm"

(cd "$WORK_DIR" && ELM_HOME="$OUTPUT_DIR" "$ELM" make src/Main.elm --output=/dev/null) \
    2>&1 | sed 's/^/  /' \
    || { echo "Error: elm make failed" >&2; exit 1; }

# --- Optionally fetch community packages ---
if [ "$WITH_COMMUNITY" = true ]; then
    echo "Fetching community packages..."

    # Create a second project that depends on community packages
    COMMUNITY_DIR=$(mktemp -d)
    # Update trap to clean up both
    trap 'rm -rf "$WORK_DIR" "$COMMUNITY_DIR"' EXIT

    cat > "$COMMUNITY_DIR/elm.json" << 'ELMJSON'
{
    "type": "application",
    "source-directories": ["src"],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/virtual-dom": "1.0.3",
            "elm-community/list-extra": "8.7.0",
            "NoRedInk/elm-json-decode-pipeline": "1.0.1",
            "mdgriffith/elm-ui": "1.1.8",
            "rtfeldman/elm-css": "18.0.0"
        },
        "indirect": {
            "elm/parser": "1.1.0",
            "elm/svg": "1.0.1",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "robinheghan/murmur3": "1.0.0",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
ELMJSON

    mkdir -p "$COMMUNITY_DIR/src"
    cat > "$COMMUNITY_DIR/src/Main.elm" << 'ELM'
module Main exposing (main)

import Browser
import Css
import Element
import Html exposing (Html, text)
import Json.Decode.Pipeline
import List.Extra

main : Html msg
main = text "community"
ELM

    (cd "$COMMUNITY_DIR" && ELM_HOME="$OUTPUT_DIR" "$ELM" make src/Main.elm --output=/dev/null 2>&1 | sed 's/^/  /')
    rm -rf "$COMMUNITY_DIR"
fi

echo ""
echo "Seed generated: $OUTPUT_DIR"
echo "Seed size: $(du -sh "$OUTPUT_DIR" | cut -f1)"
