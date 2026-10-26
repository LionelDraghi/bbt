#!/bin/sh
# Generate source metrics badges (lines of code, cyclomatic complexity)
# from the real gnatmetric output on src/.
# Requires gnatmetric (libadalang_tools).

set -eu

OUT_DIR=docs/metrics

GNATMETRIC=$(command -v gnatmetric || true)
if [ -z "$GNATMETRIC" ] && [ -x "$HOME/.alire/bin/gnatmetric" ]; then
    GNATMETRIC="$HOME/.alire/bin/gnatmetric"
fi
if [ -z "$GNATMETRIC" ]; then
    echo "gen_metrics_badges: gnatmetric not found (install libadalang_tools)" >&2
    exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# One run over all sources, with the default (full) metric set: this is what
# makes the global section of the XML complete. gnatmetric exits 1 if any
# unit fails to parse (e.g. bbt-main.adb with the GNAT "return when"
# extension) but the report still contains the metrics of all the units it
# could parse, which is what the badges want.
# The consolidated XML report (global + per-file + per-unit metrics) is
# kept under git in docs/metrics/metrix.xml; the per-source detailed files
# and the text report are thrown away.
mkdir -p $OUT_DIR

$GNATMETRIC \
    --output-dir="$TMP" \
    --generate-xml-output --xml-file-name="$PWD/$OUT_DIR/metrix.xml" \
    --no-text-output \
    src/*.ads src/*.adb || true

# Badge values come from the global section of the XML report, i.e. the
# metrics listed after the last per-file section.
XML=$OUT_DIR/metrix.xml
GLOBAL=$(awk '/<coupling>/{exit} {buf[NR]=$0; if ($0 ~ /<\/file>/) last=NR} END {for (i=last+1;i<=NR;i++) print buf[i]}' "$XML")
CODE_LINES=$(printf '%s\n' "$GLOBAL" | grep -m1 'name="code_lines"' | sed 's/.*>\([0-9]*\)<.*/\1/')
AVG_CC=$(printf '%s\n' "$GLOBAL" | grep -m1 'name="average_complexity"' | sed 's/.*>\([0-9.]*\)<.*/\1/')

if [ -z "$CODE_LINES" ] || [ -z "$AVG_CC" ]; then
    echo "gen_metrics_badges: could not parse gnatmetric output" >&2
    exit 1
fi

# --- badge generator --------------------------------------------------------
gen_badge () { # label message color outfile
    label="$1"; message="$2"; color="$3"; outfile="$4"
    lw=$(( ${#label} * 6 + 12 ))
    mw=$(( ${#message} * 6 + 12 ))
    width=$(( lw + mw ))
    cat > "$outfile" <<EOF
<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="20" role="img" aria-label="$label: $message"><title>$label: $message</title><g shape-rendering="crispEdges"><rect width="$lw" height="20" fill="#555"/><rect x="$lw" width="$mw" height="20" fill="$color"/></g><g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" text-rendering="geometricPrecision" font-size="110"><text x="$((lw * 5))" y="140" textLength="$((lw * 10 - 40))" transform="scale(.1)">$label</text><text x="$((lw * 10 + mw * 5))" y="140" textLength="$((mw * 10 - 40))" transform="scale(.1)">$message</text></g></svg>
EOF
}

gen_badge "loc" "$CODE_LINES" "#007ec6" "$OUT_DIR/loc.svg"
gen_badge "cyclomatic complexity" "$AVG_CC" "#4c1" "$OUT_DIR/complexity.svg"

echo "metrics badges updated: $OUT_DIR/loc.svg, $OUT_DIR/complexity.svg (code lines: $CODE_LINES, avg cc: $AVG_CC)"
