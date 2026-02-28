#!/usr/bin/env bash
#
# run.sh — Benchmark ELM_HOME warm cache strategies.
#
# Runs elm make with different ELM_HOME seed configurations and reports
# wall-clock timing + seed sizes in a summary table.
#
# Usage: bash run.sh [iterations]
#   iterations: number of runs per config (default: 3)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/test-project"
DOCS_SERVER_DIR="$SCRIPT_DIR/.."
ITERATIONS="${1:-3}"

# --- Resolve elm binary ---
ELM="${ELM_BIN:-}"
if [ -z "$ELM" ]; then
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

echo "Elm: $ELM ($($ELM --version))"
echo "Iterations per config: $ITERATIONS"
echo ""

# --- Generate full seed ---
echo "=== Generating full seed ==="
FULL_SEED=$(mktemp -d)
bash "$SCRIPT_DIR/generate-seed.sh" "$FULL_SEED"
echo ""

# --- Generate community seed ---
echo "=== Generating community seed ==="
COMMUNITY_SEED=$(mktemp -d)
bash "$SCRIPT_DIR/generate-seed.sh" "$COMMUNITY_SEED" --with-community
echo ""

# --- Cleanup on exit ---
CLEANUP_DIRS="$FULL_SEED $COMMUNITY_SEED"
cleanup() {
    for d in $CLEANUP_DIRS; do
        rm -rf "$d"
    done
}
trap cleanup EXIT

# --- Helper: run a single timed elm make ---
# Uses a fresh working directory each time, with the given ELM_HOME.
# Prints the wall-clock time in seconds.
run_timed() {
    local elm_home="$1"

    local work_dir
    work_dir=$(mktemp -d)

    cp "$PROJECT_DIR/elm.json" "$work_dir/elm.json"
    mkdir -p "$work_dir/src"
    cp "$PROJECT_DIR/src/Main.elm" "$work_dir/src/Main.elm"

    # Write time output to a temp file to avoid interleaving with elm stderr
    local time_file
    time_file=$(mktemp)

    { /usr/bin/time -p sh -c "cd '$work_dir' && ELM_HOME='$elm_home' '$ELM' make src/Main.elm --output=/dev/null >/dev/null 2>&1" ; } 2>"$time_file"

    rm -rf "$work_dir"

    # Extract "real" time from time -p output
    awk '/^real/ { print $2 }' "$time_file"
    rm -f "$time_file"
}

# --- Helper: prepare a seed variant ---
# Copies the full seed and applies modifications based on the config name.
# Prints the path to the prepared seed.
prepare_seed() {
    local config="$1"
    local seed_dir
    seed_dir=$(mktemp -d)
    CLEANUP_DIRS="$CLEANUP_DIRS $seed_dir"

    case "$config" in
        cold)
            # Empty ELM_HOME — nothing to copy
            ;;

        full)
            cp -a "$FULL_SEED/." "$seed_dir/"
            ;;

        no-artifacts)
            cp -a "$FULL_SEED/." "$seed_dir/"
            # Remove compiled artifact .dat files but keep registry.dat
            find "$seed_dir/0.19.1/packages" -mindepth 3 -name "*.dat" -delete
            ;;

        registry-only)
            # Only copy registry.dat
            mkdir -p "$seed_dir/0.19.1/packages"
            cp "$FULL_SEED/0.19.1/packages/registry.dat" "$seed_dir/0.19.1/packages/"
            ;;

        stripped)
            cp -a "$FULL_SEED/." "$seed_dir/"
            # Remove docs.json, README.md, LICENSE from all packages
            find "$seed_dir/0.19.1/packages" -mindepth 3 \( -name "docs.json" -o -name "README.md" -o -name "LICENSE" \) -delete
            ;;

        latest-only)
            cp -a "$FULL_SEED/." "$seed_dir/"
            # Keep only the latest (highest) version directory for each package.
            # Since our test project pins exact versions, there should only be one
            # version per package anyway, but this demonstrates the approach.
            for pkg_dir in "$seed_dir"/0.19.1/packages/*/*; do
                [ -d "$pkg_dir" ] || continue
                versions=$(ls -1 "$pkg_dir" | sort -V)
                latest=$(echo "$versions" | tail -1)
                for v in $versions; do
                    if [ "$v" != "$latest" ]; then
                        rm -rf "$pkg_dir/$v"
                    fi
                done
            done
            ;;

        with-community)
            cp -a "$COMMUNITY_SEED/." "$seed_dir/"
            ;;

        *)
            echo "Unknown config: $config" >&2
            return 1
            ;;
    esac

    echo "$seed_dir"
}

# --- Helper: compute directory size in MB ---
dir_size_mb() {
    local dir="$1"
    if [ -d "$dir" ] && [ "$(ls -A "$dir" 2>/dev/null)" ]; then
        du -sk "$dir" | awk '{ printf "%.1f MB", $1 / 1024 }'
    else
        echo "0.0 MB"
    fi
}

# --- Run benchmarks ---
CONFIGS="cold full no-artifacts registry-only stripped latest-only with-community"

# Store results in a temp file: one line per config
# Format: config|size|t1|t2|...|avg
RESULTS_FILE=$(mktemp)
CLEANUP_DIRS="$CLEANUP_DIRS $RESULTS_FILE"

echo "=== Running benchmarks ==="
echo ""

for config in $CONFIGS; do
    echo "--- Config: $config ---"

    # Prepare seed once per config to measure size
    seed_dir=$(prepare_seed "$config")
    size=$(dir_size_mb "$seed_dir")
    echo "  Seed size: $size"

    times=""
    for i in $(seq 1 "$ITERATIONS"); do
        # Each run gets a fresh copy of the seed so previous runs don't warm it
        run_seed=$(mktemp -d)
        CLEANUP_DIRS="$CLEANUP_DIRS $run_seed"
        if [ "$config" != "cold" ]; then
            cp -a "$seed_dir/." "$run_seed/"
        fi

        t=$(run_timed "$run_seed")
        if [ -z "$times" ]; then
            times="$t"
        else
            times="$times $t"
        fi
        echo "  Run $i: ${t}s"

        rm -rf "$run_seed"
    done

    # Compute average
    avg=$(echo "$times" | tr ' ' '\n' | awk '{ sum += $1 } END { printf "%.2f", sum/NR }')

    echo "  Avg: ${avg}s"
    echo ""

    # Save result line
    echo "$config|$size|$times|$avg" >> "$RESULTS_FILE"

    rm -rf "$seed_dir"
done

# --- Print summary table ---
echo ""
echo "=== Results ==="
echo ""

# Header
header=$(printf "%-15s | %-9s" "Config" "Seed Size")
for i in $(seq 1 "$ITERATIONS"); do
    header="$header | $(printf '%-6s' "Run $i")"
done
header="$header | $(printf '%-6s' "Avg")"
echo "$header"

# Separator
sep=$(echo "$header" | sed 's/[^|+]/-/g; s/|/+/g')
echo "$sep"

# Rows
while IFS='|' read -r config size times avg; do
    row=$(printf "%-15s | %-9s" "$config" "$size")
    for t in $times; do
        row="$row | $(printf '%-6s' "${t}s")"
    done
    row="$row | $(printf '%-6s' "${avg}s")"
    echo "$row"
done < "$RESULTS_FILE"

echo ""
echo "Done."
