#!/usr/bin/env bash
# record-demo.sh — capture a leksah-warp interaction and encode it into
# web-ready animation formats (MP4 + VP9-WebM + optimized GIF).
#
# All tools (node/playwright, ffmpeg, gifski) are pulled on demand via
# `nix shell`, so nothing needs to be installed globally.  Playwright's
# Chromium is cached under $PW_BROWSERS (default ~/.cache/leksah-rec).
#
# Usage:
#   docs/website/record-demo.sh [selftest|tour|breakout]
#
# Env knobs (all optional):
#   OUT_DIR   output directory        (default: docs/website/media)
#   NAME      output basename         (default: the story name)
#   LEKSAH_URL warp URL               (default: http://127.0.0.1:3367/)
#   W H       recording size, CSS px  (default: 1440 900)
#   FPS       output frame rate       (default: 24)
#   MAXW      max output width, px    (default: 1280; downscale for smaller files)
#   GIF       1 to also emit a GIF    (default: 1)
#   HEADED    1 to watch the browser  (default: headless)
#
# Examples:
#   docs/website/record-demo.sh selftest            # no leksah needed; proves the toolchain
#   LEKSAH_URL=http://127.0.0.1:3367/ docs/website/record-demo.sh tour
#   MAXW=960 FPS=20 docs/website/record-demo.sh breakout
set -euo pipefail

STORY="${1:-selftest}"
HERE="$(cd "$(dirname "$0")" && pwd)"
OUT_DIR="${OUT_DIR:-$HERE/media}"
NAME="${NAME:-$STORY}"
W="${W:-1440}"; H="${H:-900}"
FPS="${FPS:-24}"; MAXW="${MAXW:-1280}"
GIF="${GIF:-1}"
export LEKSAH_URL="${LEKSAH_URL:-http://127.0.0.1:3367/}"
export STORY W H HEADED="${HEADED:-0}"

# Playwright browser cache (kept out of the repo).
export PLAYWRIGHT_BROWSERS_PATH="${PW_BROWSERS:-$HOME/.cache/leksah-rec/ms-playwright}"
REC_DIR="${REC_DIR:-$HOME/.cache/leksah-rec}"
mkdir -p "$OUT_DIR" "$REC_DIR"

NIXPKGS="nixpkgs#nodejs_22 nixpkgs#ffmpeg nixpkgs#gifski"

echo ">> [1/4] ensuring Playwright + Chromium in $REC_DIR"
# A tiny throwaway node project holds the playwright dep; the browser binary
# lands in PLAYWRIGHT_BROWSERS_PATH and is reused across runs.
if [ ! -f "$REC_DIR/package.json" ]; then
  cat > "$REC_DIR/package.json" <<'JSON'
{ "name": "leksah-rec", "private": true, "type": "module", "dependencies": { "playwright": "^1.49.0" } }
JSON
fi
nix shell $NIXPKGS -c bash -c "cd '$REC_DIR' && npm install --no-audit --no-fund >/dev/null 2>&1 && npx playwright install chromium >/dev/null 2>&1"

RAW="$OUT_DIR/$NAME.raw.webm"
echo ">> [2/4] recording story '$STORY' ($W x $H) -> $RAW"
# ESM resolves bare imports from the module's own directory, so stage the
# driver + stand-in into the cache project (which has node_modules) and run
# them there.  Keeps the repo free of node_modules.
cp "$HERE/record-demo.mjs" "$HERE/standin.html" "$REC_DIR/"
nix shell $NIXPKGS -c bash -c "cd '$REC_DIR' && node '$REC_DIR/record-demo.mjs' '$RAW'"

echo ">> [3/4] encoding MP4 + WebM (scale to <= ${MAXW}px, ${FPS}fps)"
VF="fps=$FPS,scale='min($MAXW,iw)':-2:flags=lanczos"
# H.264 MP4 — universal <video> autoplay; yuv420p + faststart for the web.
nix shell $NIXPKGS -c ffmpeg -y -loglevel error -i "$RAW" \
  -vf "$VF" -c:v libx264 -pix_fmt yuv420p -crf 23 -preset veryslow \
  -movflags +faststart -an "$OUT_DIR/$NAME.mp4"
# VP9 WebM — smaller, modern browsers.
nix shell $NIXPKGS -c ffmpeg -y -loglevel error -i "$RAW" \
  -vf "$VF" -c:v libvpx-vp9 -b:v 0 -crf 32 -row-mt 1 -an "$OUT_DIR/$NAME.webm"

if [ "$GIF" = "1" ]; then
  echo ">> [4/4] encoding optimized GIF via gifski"
  # gifski wants frames; pipe PNGs from ffmpeg. Cap GIF width harder (GIFs are big).
  GIFW=$(( MAXW < 960 ? MAXW : 960 ))
  TMPD="$(mktemp -d)"
  nix shell $NIXPKGS -c ffmpeg -y -loglevel error -i "$RAW" \
    -vf "fps=$FPS,scale=$GIFW:-2:flags=lanczos" "$TMPD/f%05d.png"
  nix shell $NIXPKGS -c gifski --quality 85 --fps "$FPS" -o "$OUT_DIR/$NAME.gif" "$TMPD"/f*.png >/dev/null 2>&1
  rm -rf "$TMPD"
else
  echo ">> [4/4] GIF skipped (GIF=0)"
fi

echo ">> done. outputs in $OUT_DIR:"
( cd "$OUT_DIR" && ls -la "$NAME".* | awk '{printf "   %-8s %s\n", $5, $NF}' )
echo "   (raw .webm kept for re-encoding; delete if not needed)"
