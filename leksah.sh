#!/usr/bin/env bash -e

# Build-and-relaunch loop for developing leksah with leksah.  Plain cabal in
# the ambient environment: ghc/cabal/tmux/leksah-server must be on PATH, cabal
# picks the compiler, and builds land in cabal's default dist-newstyle.


usage() {
    echo "Usage: ./leksah.sh [--warp|--ghci] [--in-tmux] [LEKSAH_ARGS]"
    echo
    echo "  (default) : the native web front end, exe:leksah (WKWebView on macOS,"
    echo "              WebKitGTK on Linux — one exe, chosen per-OS in the cabal file)."
    echo "  --warp    : the browser front end, exe:leksah-warp (http://127.0.0.1:PORT/)."
    echo "  --ghci    : run the native web front end INTERPRETED in a cabal multi-repl"
    echo "              (cabal repl exe:leksah lib:leksah) inside a tmux session,"
    echo "              so 'leksah-cmd rebuild-self' becomes :reload + :main (seconds,"
    echo "              no relink) and 'leksah-cmd hs eval' can poke the live IDE."
    echo "              First load compiles everything to bytecode — slow, once."
    echo "  --in-tmux : (web front ends) run leksah inside a tmux session so its own"
    echo "              output shows up as \"Terminal 0\" in leksah's Terminals pane"
    echo
    echo "  Commands run in the AMBIENT environment: ghc, cabal, tmux and"
    echo "  leksah-server must already be on PATH.  cabal picks the compiler and"
    echo "  the build dir (dist-newstyle)."
    echo
    echo "  Env LEKSAH_PORT=N runs a SECOND instance alongside the default one:"
    echo "  N (default 3367) is the UI port; leksah keys its control socket and"
    echo "  tmux server off it, and this script its run log + \"Terminal 0\" session."
    echo
    echo "Examples: ./leksah.sh"
    echo "          ./leksah.sh --verbosity=DEBUG"
    echo "          ./leksah.sh --warp --in-tmux"
    echo "          LEKSAH_PORT=3368 ./leksah.sh   # 2nd instance"
    echo
    echo "For details of other LEKSAH_ARGS run: ./leksah.sh --help"
}

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    usage
    exit 0
fi

# Remember the full invocation before we consume the arguments, so it can be
# echoed into the run log below.
INVOCATION="$0 $*"

# Pull the script's own flags out from anywhere in the argument list; whatever
# is left is positional (LEKSAH_ARGS).  There is a single native web exe
# (exe:leksah, chosen per-OS in the cabal file) — the default; --warp selects
# the browser front end instead.  (The classic Gtk IDE is no longer part of this
# project: it lives in leksah-classic/, which has its own cabal.project/flake.)
IN_TMUX=0
GHCI=0
UI=leksah
POS=()
for a in "$@"; do
    case "$a" in
        --nix)     echo "Note: --nix support has been removed; running in the ambient environment." >&2 ;;
        ghc[0-9]*) echo "Note: compiler selection has been removed ('$a' ignored); cabal picks the GHC on PATH." >&2 ;;
        --in-tmux) IN_TMUX=1 ;;
        --ghci)    GHCI=1 ;;
        --warp)    UI=warp ;;
        *)         POS+=("$a") ;;
    esac
done
set -- "${POS[@]}"

# On macOS the default exe:leksah is the WKWebView front end, which we run from a
# real Leksah.app bundle (correct name in the menu bar / Dock / ⌘-Tab).  --warp
# doesn't; nor does the Linux exe:leksah (WebKitGTK).
RUN_FROM_APP=0
if [ "$UI" = "leksah" ] && [ "$(uname)" = "Darwin" ]; then RUN_FROM_APP=1; fi

# UI port for this instance — must match IDE.Web.Instance's default; LEKSAH_PORT
# overrides it.  It's already inherited by the launched binary (which keys its
# control socket and tmux server off it); we also export it (belt and braces)
# and derive a name suffix so a second loop on another port keeps its own run
# log and, with --in-tmux, its own "Terminal 0" session — no clobbering.
#   Still SHARED across loops (not port-isolated here): the build dir,
#   ~/.leksah/rebuild.sh, and the Leksah.app bundle.  So run a second instance
#   run-only, or at least don't rebuild two loops into the one build dir at
#   once; and note rebuild.sh reflects whichever loop wrote it last (its UI
#   target), which matters only when the two loops use different UIs.
LEKSAH_PORT="${LEKSAH_PORT:-3367}"
export LEKSAH_PORT
if [ "$LEKSAH_PORT" = "3367" ]; then INSTANCE_TAG=""; else INSTANCE_TAG="-$LEKSAH_PORT"; fi

# Log-file discriminator: empty for the default front end (exe:leksah), so its
# logs are just leksah[-run].log; -warp for the browser front end.
if [ "$UI" = "leksah" ]; then LOG_TAG=""; else LOG_TAG="-$UI"; fi

# Tee everything (build output + leksah's own stdout/stderr, including the
# leksah-server metadata logs) to a file under ~/.leksah so a run can be
# examined after the fact.  Truncated each run.
RUNLOGDIR="$HOME/.leksah"
mkdir -p "$RUNLOGDIR"
RUNLOG="$RUNLOGDIR/leksah-run$LOG_TAG$INSTANCE_TAG.log"
exec > >(tee "$RUNLOG") 2>&1
echo "Logging this run to $RUNLOG"
echo "Invocation: $INVOCATION"
echo "Parsed: UI=$UI IN_TMUX=$IN_TMUX LEKSAH_PORT=$LEKSAH_PORT LEKSAH_ARGS=[$*]"

# Machine-readable launch state for scripts/agents: one line per phase
# transition, truncated at every launch, so a monitor can poll THIS FILE
# instead of scraping the tmux pane (pane scrollback holds the PREVIOUS run's
# outcome text and the launch command's own echo, both of which false-match
# naive pattern watches).  The EXIT trap records every way the script can end
# — including set -e failures like a nix/cabal error — EXCEPT an exec (the
# ghci arm execs its attach/tail, so it writes its terminal "up" line first).
STATUS_FILE="$RUNLOGDIR/launch-status$INSTANCE_TAG"
status() { printf '%s %s\n' "$(date '+%H:%M:%S')" "$*" >> "$STATUS_FILE"; }
: > "$STATUS_FILE"
status "starting: $INVOCATION"
trap 'status "exited: code=$?"' EXIT

# A self-contained rebuild script for `leksah-cmd rebuild-self`.  It runs in
# the already-running leksah's environment (cabal/ghc are already on PATH), so
# it calls cabal directly with the same target leksah was launched with.
# Map the UI selector to its cabal executable target.  The native web front end
# (WKWebView on macOS, WebKitGTK on Linux, WebView2 on Windows) is a single
# exe:leksah selected per-OS in the cabal file — the default; --warp is
# exe:leksah-warp.
case "$UI" in
    warp)    EXE_TARGET="exe:leksah-warp" ;;
    *)       EXE_TARGET="exe:leksah" ;;
esac
REBUILD_TARGET="$EXE_TARGET"
cat > "$RUNLOGDIR/rebuild.sh" <<EOF
#!/bin/sh
# Generated by leksah.sh; run by 'leksah-cmd rebuild-self'.  leksah is launched
# directly (not via 'cabal run'), so its environment is identical to the build
# environment — calling cabal directly here matches the loop's build config and
# stays incremental.
cd "$(pwd)" || exit 1
exec cabal build $REBUILD_TARGET exe:leksah-cmd exe:ffcabal
EOF

# macOS: run the wkwebview front end from a real .app bundle so CFBundleName
# names it "Leksah" in the menu bar, the Dock and the ⌘-Tab switcher (an
# unbundled binary is named after the executable, "leksah-wkwebview").  The
# bundle is a thin wrapper: its executable is hard-linked to the freshly built
# binary on each launch (see launch_leksah below), and data files still come
# from the repo via leksah_datadir — so this stays a normal incremental dev
# loop, it just gives the process a proper bundle identity.
APPBUNDLE="$(pwd)/Leksah.app"
if [ "$RUN_FROM_APP" = "1" ]; then
    mkdir -p "$APPBUNDLE/Contents/MacOS" "$APPBUNDLE/Contents/Resources"
    cat > "$APPBUNDLE/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>Leksah</string>
  <key>CFBundleDisplayName</key><string>Leksah</string>
  <key>CFBundleExecutable</key><string>leksah</string>
  <key>CFBundleIdentifier</key><string>org.leksah.leksah</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleInfoDictionaryVersion</key><string>6.0</string>
  <key>CFBundleShortVersionString</key><string>0.17.0</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>LSMinimumSystemVersion</key><string>11.0</string>
  <key>CFBundleIconFile</key><string>leksah</string>
</dict>
</plist>
PLIST
    # Prefer the macOS app icon (leksah glyph on a dark-grey gradient); fall
    # back to the plain logo.
    if [ -f leksah/osx/leksah-macapp.icns ]; then
        cp -f leksah/osx/leksah-macapp.icns "$APPBUNDLE/Contents/Resources/leksah.icns"
    elif [ -f leksah/osx/leksah.icns ]; then
        cp -f leksah/osx/leksah.icns "$APPBUNDLE/Contents/Resources/leksah.icns"
    fi
fi

if [ "$IN_TMUX" = "1" ]; then
    # A tmux config matching the one leksah uses for its own terminals, so the
    # session leksah runs in shares it (no status bar, titles passed through,
    # the login shell).  Whoever starts the `leksah` socket server first wins,
    # so set it here.
    LOGIN_SHELL="$(dscl . -read "/Users/$(id -un)" UserShell 2>/dev/null | awk '{print $2}')"
    [ -z "$LOGIN_SHELL" ] && LOGIN_SHELL="$(getent passwd "$(id -un)" 2>/dev/null | cut -d: -f7)"
    [ -z "$LOGIN_SHELL" ] && LOGIN_SHELL="${SHELL:-/bin/bash}"
    CONF="${TMPDIR:-/tmp}/leksah.tmux.conf"
    cat > "$CONF" <<EOF
set -g status off
set -g set-titles on
set -g set-titles-string "#T"
set -g default-shell "$LOGIN_SHELL"
set -g mouse on
set -g history-limit 50000
EOF
    LOGFILE="$RUNLOGDIR/leksah$LOG_TAG$INSTANCE_TAG.log"
fi

# Build the helper exes AND (for the binary arms) the front end with one
# `cabal build` (one project plan — `cabal install` would resolve a separate
# plan and rebuild the world), then symlink the helpers onto PATH.  Every
# cabal step shares the bin/ PATH prefix, and the app is launched DIRECTLY
# rather than via `cabal run`:
#   * cabal treats a different PATH as "configuration changed" and rebuilds
#     everything, so the prefix must be identical across all cabal calls;
#   * `cabal run` augments the launched app's PATH with build-tool dirs, which
#     would make an in-app `cabal build` (rebuild-self) see a different config
#     and rebuild everything.  Launching the binary directly keeps the app's
#     environment identical to the build environment, so rebuild-self stays
#     incremental.
build_and_link() {
    # $@ = extra cabal targets beyond the helper exes.  Explicit `|| return`:
    # callers use `build_and_link … || read`, which turns off `set -e` inside
    # the function — without it a failed build would fall through to the
    # symlinking below.
    PATH="$(pwd)/bin:$PATH" cabal build exe:leksah-server exe:leksah-cmd exe:ffcabal "$@" || return 1
    mkdir -p bin
    ln -sf "$(PATH="$(pwd)/bin:$PATH" cabal list-bin exe:leksah-server | grep '^/' | tail -1)" bin/leksah-server
    ln -sf "$(PATH="$(pwd)/bin:$PATH" cabal list-bin exe:leksah-cmd | grep '^/' | tail -1)"    bin/leksah-cmd
    ln -sf "$(PATH="$(pwd)/bin:$PATH" cabal list-bin exe:ffcabal | grep '^/' | tail -1)"       bin/ffcabal
}

# --ghci: run the native web front end INTERPRETED in a cabal multi-repl
# instead of building + launching the binary.  The repl lives in a tmux pane
# (on leksah's own tmux server) so `leksah-cmd hs eval` and the reload flow
# (rebuild-self against a ghci instance = :reload + :main) can drive the
# prompt with send-keys; the pane's exact environment — including the PATH
# invariant every cabal call must share — is captured into a generated script.
# There is no exit-2/3 relaunch loop here: the ghci session IS the loop.
#
# NB the repl flips cabal flags (-objc-in-library) and shares the
# default dist-newstyle build dir with the binary arm, so switching between
# --ghci and binary launches makes cabal reconfigure the flag-flipped packages.
if [ "$GHCI" = "1" ]; then
  if [ "$UI" != "leksah" ]; then
    echo "--ghci only applies to the native web front end (exe:leksah); ignoring --$UI." >&2
  fi
  rm -f .ghc.environment.*
  mkdir -p bin
  # Same helper prebuild as the binary arm (leksah-server/leksah-cmd/ffcabal
  # must be on PATH for the IDE), same PATH prefix — the repl's dependency
  # builds then share the same plan and stay incremental.
  status "ghci: building helper exes"
  build_and_link
  status "ghci: compiling native dylibs"

  # GHCi's RTS linker can load Objective-C objects but never registers their
  # classes with the ObjC runtime (only dyld does) — so in ghci mode the ObjC
  # is kept OUT of the Haskell archives (-objc-in-library flags) and preloaded
  # as dylibs instead (ghci -L/-l), where the Haskell foreign imports resolve
  # against them.  Compile both dylibs: leksah's own native glue and
  # jsaddle-wkwebview's (from the unpacked source-repository-package).
  GHCI_NATIVE="$RUNLOGDIR/ghci-native"
  mkdir -p "$GHCI_NATIVE"
  # Both dylibs are OS-native (Cocoa/WebKit/ApplicationServices), so LINK them
  # with the SYSTEM toolchain (xcrun clang).  env -i so that if this shell
  # carries cc-wrapper vars (e.g. running inside some dev shell) they don't
  # redirect the system clang to another linker.  Only the RTS include dirs
  # and the unpacked jsaddle source path come from the ambient ghc/cabal.
  # jsaddle-wkwebview's source: a LOCAL package path in cabal.project wins
  # (a development checkout of jsaddle); otherwise the copy cabal unpacked
  # from the source-repository-package.
  JS_SRC=$(grep -oE '^[[:space:]]*/[^[:space:]]*/jsaddle-wkwebview[[:space:]]*$' cabal.project 2>/dev/null | tr -d '[:space:]' | head -1)
  [ -n "$JS_SRC" ] && [ -d "$JS_SRC" ] || \
    JS_SRC=$(ls -d dist-newstyle/src/jsaddle-*/jsaddle-wkwebview 2>/dev/null | head -1)
  HS_INC=""
  for d in $(ghc-pkg field rts include-dirs --simple-output); do HS_INC="$HS_INC -I$d"; done
  if [ -z "$JS_SRC" ]; then
    echo "jsaddle-wkwebview source not found (no local package in cabal.project, nothing unpacked in dist-newstyle/src)" >&2
    exit 1
  fi
  syscc() { env -i PATH=/usr/bin:/bin HOME="$HOME" /usr/bin/xcrun clang "$@"; }
  syscc -dynamiclib leksah/main/leksah-mac-menu.m \
     -framework Cocoa -framework ApplicationServices -framework AVFoundation \
     -o "$GHCI_NATIVE/libleksah-mac-menu.dylib"
  syscc -dynamiclib "$JS_SRC/cbits-cocoa/WKWebView-AppDelegate.m" \
     -DUSE_COCOA -I"$JS_SRC/cbits" $HS_INC -Wno-everything \
     -framework Foundation -framework WebKit -framework Cocoa \
     -o "$GHCI_NATIVE/libjsaddle-wkwebview-objc.dylib"

  TMUXSOCK="leksah$INSTANCE_TAG"
  GHCI_LOG="$RUNLOGDIR/ghci$INSTANCE_TAG.log"
  GHCI_RUN="$RUNLOGDIR/ghci-run$INSTANCE_TAG.sh"
  TMUX_BIN=$(command -v tmux) || { echo "tmux not found on PATH — required for --ghci." >&2; exit 1; }
  # Snapshot the FULL launching environment (not just PATH): the repl is
  # spawned by the tmux SERVER, whose environment is whatever shell started
  # it — not this one.  A cabal solve/build inside the repl needs the same
  # toolchain env this shell has (e.g. PKG_CONFIG_PATH, so pkg-config deps
  # resolve the same way when leksah.sh is run inside `nix develop`).
  # tmux/terminal-specific vars are dropped; the
  # explicit exports in ghci-run.sh below override the snapshot where needed.
  ENV_SNAPSHOT="$RUNLOGDIR/ghci-env$INSTANCE_TAG.sh"
  export -p | grep -v -E '^declare -x (TMUX|TMUX_PANE|TERM|PWD|OLDPWD|SHLVL|_)=' > "$ENV_SNAPSHOT"
  cat > "$GHCI_RUN" <<EOF
#!/usr/bin/env bash
# Generated by leksah.sh --ghci; exec'd inside the ghci tmux pane.  Restores
# the launching shell's environment so every cabal call (this repl, the
# binary-arm builds, rebuild-self) sees the SAME PATH and toolchain vars —
# cabal treats a different PATH as "configuration changed" and rebuilds the
# world, and a fresh solve needs pkg-config et al.
cd "$(pwd)" || exit 1
. "$ENV_SNAPSHOT"
export LEKSAH_PORT=$LEKSAH_PORT
export LEKSAH_GHCI=1
export leksah_datadir="$(pwd)/leksah"
export PATH="$(pwd)/bin:$PATH"
# TERM=dumb makes ghci's haskeline drop all cursor/keypad control escapes, so
# the prompt no longer rewrites its line — echoed input and command output land
# on separate clean lines.  leksah-cmd (hs eval / rebuild-self) scrapes the pane
# for its output fences; without this, haskeline glues a command echo to the
# previous output and the scrape misses (or swallows) results.
export TERM=dumb
# NB do NOT set GHCRTS here to make the RTS return freed blocks sooner (-Fd1):
# it is inherited by every Haskell program this session spawns, and leksah-cmd is
# linked with the default -rtsopts=some, so it dies with "Most RTS options are
# disabled" — including when typed in one of leksah's own terminal panes.  Nor
# does --repl-options=+RTS work: cabal passes repl options through a @response
# file, which the RTS never parses.
exec cabal repl leksah:exe:leksah leksah:lib:leksah \\
  --enable-multi-repl \\
  --constraint="leksah -objc-in-library" \\
  --constraint="jsaddle-wkwebview -objc-in-library" \\
  --repl-options=-fno-ghci-sandbox \\
  --repl-options=-L"$GHCI_NATIVE" \\
  --repl-options=-lleksah-mac-menu \\
  --repl-options=-ljsaddle-wkwebview-objc
EOF
  chmod +x "$GHCI_RUN"
  # The :main line (with this run's LEKSAH_ARGS) — leksah-cmd replays it after
  # a :reload.
  printf ':main %s\n' "$*" > "$RUNLOGDIR/ghci-main$INSTANCE_TAG"

  if "$TMUX_BIN" -L "$TMUXSOCK" has-session -t ghci 2>/dev/null; then
    echo "A ghci session already exists on tmux -L $TMUXSOCK — reusing it."
    echo "(kill it with: tmux -L $TMUXSOCK kill-session -t ghci)"
  else
    # leksah-cmd hs eval / rebuild-self read the ghci output from the pane's
    # RENDERED scrollback (haskeline's control codes make the raw pipe log
    # unsplittable); raise the server's history-limit so long :reload output
    # isn't truncated before the fence markers.
    "$TMUX_BIN" -L "$TMUXSOCK" set-option -g history-limit 100000 2>/dev/null || true
    # The GHC RTS MachO linker sometimes can't relocate a large static archive
    # (reflex, ghc-lib-parser, …) at :main time: an info-table SUBTRACTOR falls
    # out of the signed-32-bit range when ASLR happens to map that archive's
    # low region and the dyld region >2 GB apart.  It's a per-run memory-layout
    # lottery (see docs/building.md "ghci mode"), so retry the whole load a few
    # times — a fresh process re-rolls the layout.  (The real fix is in the
    # RTS linker; this keeps the dev loop usable meanwhile.)
    # Killing the tmux session does NOT stop the repl: cabal's `ghc
    # --interactive` child is REPARENTED, keeps running leksah, and holds
    # LEKSAH_PORT and ~/.leksah/cmd.sock — so the next attempt's instance comes
    # up with no control socket (startCmdServer sees a live listener and declines
    # to steal it), leaving an app that heartbeats but can't be driven by
    # leksah-cmd at all, plus an orphan nothing can address.  Kill the pane's
    # whole process tree, TERM then KILL, before the session goes.
    proc_tree() {   # print PID and every descendant (pane sh → cabal → ghc)
      local root=$1 kid
      echo "$root"
      for kid in $(ps -o pid=,ppid= -ax | awk -v p="$root" '$2 == p { print $1 }'); do
        proc_tree "$kid"
      done
    }
    ghci_session_kill() {
      local pane_pid pids p
      pane_pid=$("$TMUX_BIN" -L "$TMUXSOCK" display-message -p -t ghci '#{pane_pid}' 2>/dev/null) \
        || pane_pid=""
      if [ -n "$pane_pid" ]; then
        # Snapshot the tree BEFORE signalling anything: TERMing a parent
        # reparents its children to launchd, so a second walk would no longer
        # find them — and the process that must not survive (ghc, still running
        # leksah) is exactly such a grandchild.
        pids=$(proc_tree "$pane_pid")
        for p in $pids; do kill -TERM "$p" 2>/dev/null || true; done
        sleep 2
        # ghc regularly survives SIGTERM (it sits in the Cocoa run loop).
        for p in $pids; do kill -KILL "$p" 2>/dev/null || true; done
      fi
      "$TMUX_BIN" -L "$TMUXSOCK" kill-session -t ghci 2>/dev/null || true
    }

    ghci_ok=0
    for gattempt in 1 2 3 4 5 6; do
      ghci_session_kill
      : > "$GHCI_LOG"
      PANE=$("$TMUX_BIN" -L "$TMUXSOCK" new-session -d -P -F '#{pane_id}' -s ghci "$GHCI_RUN")
      "$TMUX_BIN" -L "$TMUXSOCK" pipe-pane -o -t "$PANE" "cat >> $GHCI_LOG"
      printf '%s %s\n' "$TMUXSOCK" "$PANE" > "$RUNLOGDIR/ghci-pane$INSTANCE_TAG"
      status "ghci: repl loading (attempt $gattempt)"
      echo "cabal repl starting in tmux (-L $TMUXSOCK, session ghci, pane $PANE; attempt $gattempt)"
      echo "Log: $GHCI_LOG   Pane file: $RUNLOGDIR/ghci-pane$INSTANCE_TAG"
      echo "Waiting for the ghci prompt (the first load compiles/loads everything — slow, once)…"
      prompt=0
      while :; do
        dead=$("$TMUX_BIN" -L "$TMUXSOCK" display-message -p -t "$PANE" '#{pane_dead}' 2>/dev/null) || dead=1
        # Only keep waiting while tmux AFFIRMS the pane is alive.  If the repl
        # dies early enough that its whole session goes (e.g. cabal fails to
        # build the library), display-message can exit 0 with EMPTY output — so
        # testing for "1" waited forever on a pane that no longer exists, and the
        # retry below never ran.
        if [ "$dead" != "0" ]; then break; fi
        last=$("$TMUX_BIN" -L "$TMUXSOCK" capture-pane -p -t "$PANE" 2>/dev/null | grep -v '^[[:space:]]*$' | tail -1)
        case "$last" in
          *"ghci>"*) prompt=1; break ;;
        esac
        sleep 2
      done
      if [ "$prompt" != 1 ]; then
        status "ghci: repl died before the prompt (attempt $gattempt)"
        echo "ghci exited before the prompt (attempt $gattempt) — see $GHCI_LOG; retrying."
        continue
      fi
      status "ghci: prompt up, sending :main (attempt $gattempt)"
      # The Cocoa run loop (and NSWindow creation) must be on the process main
      # OS thread.  -fno-ghci-sandbox makes GHCi run statements on its own
      # (bound, thread-0) REPL thread instead of a forked worker — but cabal's
      # --enable-multi-repl does NOT propagate --repl-options=-fno-ghci-sandbox
      # to the interactive session, so set it here at the prompt (it persists
      # across :reload for the whole session).  Without this, :main builds the
      # NSWindow on a worker thread and Cocoa aborts ("NSWindow should only be
      # instantiated on the main thread!").
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" -l ':set -fno-ghci-sandbox'
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" Enter
      # Turn OFF the pty's line-discipline echo.  When leksah-cmd (hs eval /
      # rebuild-self) drives the prompt with send-keys, the tty would otherwise
      # echo each keystroke char-by-char AS IT ARRIVES — interleaving it with
      # ghci's concurrent output (e.g. `[1,2:!echo…END,3,4]`) and racing/over-
      # writing on wrap boundaries, which broke the pane scrape.  With -echo the
      # only remaining echo is haskeline's own clean `ghci> <cmd>` line, trivially
      # stripped.  Persists across :reload; a human attaching won't see their
      # own typing, which is fine for this driver pane.
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" -l ':!stty -echo'
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" Enter
      # Multi-repl: the interactive scope starts empty, so bring Main in first
      # (":main" runs whatever `main` is in scope).
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" -l ':module + Main'
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" Enter
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" -l ":main $*"
      "$TMUX_BIN" -L "$TMUXSOCK" send-keys -t "$PANE" Enter
      echo "Sent :main — waiting for the UI to build (or a linker-lottery crash)…"
      outcome=""
      dom=""
      LEKSAHCMD="$(pwd)/bin/leksah-cmd"
      # 300s, not 150: the FIRST :main of a session (fresh bytecode, cold
      # metadata, workspace scans) has been measured past 150s, and the retry
      # this timeout triggers is destructive — it kills a perfectly good
      # instance.  Real failures still short-circuit below (pane dead, reloc).
      for _wait in $(seq 1 150); do
        if grep -q "Relocation out of range" "$GHCI_LOG" 2>/dev/null; then outcome=reloc; break; fi
        dead=$("$TMUX_BIN" -L "$TMUXSOCK" display-message -p -t "$PANE" '#{pane_dead}' 2>/dev/null) || dead=1
        if [ "$dead" = "1" ]; then outcome=dead; break; fi
        if grep -q "] alive" "$GHCI_LOG" 2>/dev/null; then
          # The heartbeat is up — but the RTS linker can also load reflex with a
          # SILENTLY bad relocation (no crash message): the app runs yet the
          # reflex network is corrupt and never builds the DOM.  So require the
          # DOM to actually populate (max across windows) before declaring
          # success; otherwise it's a dud layout — retry.
          dom=$("$LEKSAHCMD" js eval 'document.querySelectorAll("*").length' 2>/dev/null \
                  | grep -oE '[0-9]+' | sort -n | tail -1)
          if [ -n "$dom" ] && [ "$dom" -gt 500 ] 2>/dev/null; then outcome=up; break; fi
        fi
        sleep 2
      done
      case "$outcome" in
        up) ghci_ok=1; echo "leksah is up (attempt $gattempt, DOM=$dom)."; break ;;
        reloc) status "ghci: relocation lottery lost (attempt $gattempt)"
               echo "RTS-linker relocation lottery lost (attempt $gattempt) — retrying with a fresh layout." ;;
        *) status "ghci: UI did not build (attempt $gattempt, outcome=${outcome:-frozen})"
           echo "ghci UI did not build (attempt $gattempt, outcome=${outcome:-frozen}) — retrying with a fresh layout." ;;
      esac
    done
    if [ "$ghci_ok" != 1 ]; then
      status "failed: gave up starting the ghci session after retries"
      echo "Gave up starting the ghci session after retries — see $GHCI_LOG." >&2
      exit 1
    fi
  fi
  # Terminal state, written BEFORE the execs below (an exec skips the EXIT
  # trap): the app is up; the repl keeps running in the ghci tmux session.
  status "up: ghci instance running (DOM=${dom:-reused})"
  # Interactive: attach to the repl pane (not from inside another tmux —
  # nested attach refuses); in a tmux pane, tail the log instead; headless
  # (scripts/agents), just leave the repl running detached.
  if [ -t 0 ] && [ -z "${TMUX:-}" ]; then
    exec "$TMUX_BIN" -L "$TMUXSOCK" attach -t ghci </dev/tty >/dev/tty 2>&1
  elif [ -t 0 ]; then
    echo "(inside tmux — tailing $GHCI_LOG; the repl runs in session ghci)"
    exec tail -n +1 -f "$GHCI_LOG"
  else
    echo "Not a terminal — the repl keeps running detached in tmux."
    echo "Attach with: tmux -L $TMUXSOCK attach -t ghci"
    exit 0
  fi
fi

# ===========================================================================
# Zero-downtime handoff supervisor (opt-in: LEKSAH_HANDOFF=1; native web UI
# only — not --warp, and ghci has its own arm above).  Keeps the
# current instance up while its successor starts on an ephemeral asset port,
# and retires it only once the successor's UI signals ready.  See
# IDE.Web.Handoff.  When off, control falls through to the ordinary exit-2/3
# relaunch loop below, which is unchanged.
# ===========================================================================
if [ "${LEKSAH_HANDOFF:-0}" = "1" ] && [ "$UI" != "warp" ]; then
  export LEKSAH_HANDOFF=1

  # Same launch recipe as the loop's launch_leksah, duplicated so the default
  # loop stays byte-for-byte unchanged.
  handoff_launch_str='
    app="$1"; tgt="$2"; shift 2
    export leksah_datadir="$(pwd)/leksah"
    bin="$(cabal list-bin "$tgt" | grep "^/" | tail -1)"
    if [ "$app" = "1" ]; then
      macos="$(pwd)/Leksah.app/Contents/MacOS"
      ln -f "$bin" "$macos/leksah" 2>/dev/null || cp -f "$bin" "$macos/leksah"
      exec "$macos/leksah" --develop-leksah "$@"
    fi
    exec "$bin" --develop-leksah "$@"'

  handoff_build() {
    rm -f .ghc.environment.*
    build_and_link "$EXE_TARGET"
  }

  # $1 = "successor" (ephemeral asset port, LEKSAH_SUCCESSOR=1) or "" (primary).
  # Backgrounds the instance and echoes its job pid; extra leksah args follow.
  handoff_launch() {
    local kind="$1"; shift
    rm -f .ghc.environment.*
    if [ "$kind" = "successor" ]; then
      LEKSAH_SUCCESSOR=1 LEKSAH_ASSET_PORT=0 \
        PATH="$(pwd)/bin:$PATH" \
        bash -c "$handoff_launch_str" _ "$RUN_FROM_APP" "$EXE_TARGET" "$@" \
        >> "$RUNLOG" 2>&1 &
    else
      PATH="$(pwd)/bin:$PATH" \
        bash -c "$handoff_launch_str" _ "$RUN_FROM_APP" "$EXE_TARGET" "$@" \
        >> "$RUNLOG" 2>&1 &
    fi
    echo $!
  }

  REQ="$RUNLOGDIR/handoff-request"
  READY="$RUNLOGDIR/handoff-ready"
  rm -f "$REQ" "$READY"
  echo "LEKSAH_HANDOFF=1: zero-downtime handoff supervisor active."
  handoff_build || read -n 1 -s -r -p "Build failed.  Press any key to run the last built version."
  PID=$(handoff_launch "" "$@")
  echo "handoff: instance up (job $PID)."
  while :; do
    # Wait for a restart request (handoff) OR for the instance to exit on its own.
    while kill -0 "$PID" 2>/dev/null && [ ! -f "$REQ" ]; do sleep 0.3; done
    if ! kill -0 "$PID" 2>/dev/null; then
      wait "$PID" 2>/dev/null; code=$?
      case "$code" in
        2) handoff_build || true; PID=$(handoff_launch "" "$@") ;;   # legacy/crash restart
        3) PID=$(handoff_launch "" "$@") ;;
        *) echo "handoff: instance exited ($code) — supervisor stopping."; exit "$code" ;;
      esac
      continue
    fi
    mode=$(cat "$REQ" 2>/dev/null); rm -f "$REQ"
    echo "handoff: restart requested (${mode:-rebuild}); building successor — current stays up."
    [ "$mode" != "norebuild" ] && { handoff_build || echo "handoff: build failed; launching successor from last build."; }
    rm -f "$READY"
    NEW=$(handoff_launch "successor" "$@")
    echo "handoff: successor launched (job $NEW); waiting for its UI to come up…"
    waited=0; ok=0
    while [ "$waited" -lt 200 ]; do        # ~60s
      [ -f "$READY" ] && { ok=1; break; }
      kill -0 "$NEW" 2>/dev/null || { echo "handoff: successor exited before signalling ready."; break; }
      sleep 0.3; waited=$((waited+1))
    done
    if [ "$ok" = 1 ]; then
      echo "handoff: successor ready — the old instance retires itself now."
      # The old instance self-retires (exit 0) once it sees the ready file, so it
      # releases cmd.sock/windows to the successor; just wait for it to go.
      w2=0
      while kill -0 "$PID" 2>/dev/null && [ "$w2" -lt 100 ]; do sleep 0.3; w2=$((w2+1)); done
      wait "$PID" 2>/dev/null || true
      rm -f "$READY"
      PID="$NEW"
      echo "handoff: complete — now supervising job $PID."
    else
      echo "handoff: successor did not come up in time — aborting, keeping the current instance."
      kill -TERM "$NEW" 2>/dev/null || true
      wait "$NEW" 2>/dev/null || true
    fi
  done
fi

LEKSAH_EXIT_CODE=2

# Exit 2 => relaunch after rebuilding (in-IDE / rebuild-self); exit 3 =>
# `leksah-cmd restart --no-rebuild`: relaunch but skip the cabal build, since
# rebuild-self already produced the binary.
while [ $LEKSAH_EXIT_CODE -eq 2 ] || [ $LEKSAH_EXIT_CODE -eq 3 ]; do
  SKIP_REBUILD=0
  [ "$LEKSAH_EXIT_CODE" -eq 3 ] && SKIP_REBUILD=1
  rm -f .ghc.environment.*
  mkdir -p bin

  # Web front ends (default exe:leksah, or --warp): leksah-server must be on
  # PATH (for metadata) and tmux is needed for persistent terminals.  With
  # --develop-leksah leksah exits with code 2 when rebuilt (in-IDE or via
  # `leksah-cmd rebuild-self`), so this loop relaunches it.
  if [ "$SKIP_REBUILD" != 1 ]; then
    status "building"
    build_and_link "$EXE_TARGET" \
        || { status "build failed (offering last built version)"
             read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."; }
  else
    echo "leksah-cmd restart --no-rebuild: skipping build, relaunching."
  fi
  rm -f .ghc.environment.*
  status "up: launching instance"

  # Launch the freshly-built binary directly, with the data dir `cabal run`
  # would have set (the package root).  `exec` so leksah's exit code
  # propagates (2 => rebuilt => relaunch).
  launch_leksah='
    app="$1"; tgt="$2"; shift 2
    export leksah_datadir="$(pwd)/leksah"
    bin="$(cabal list-bin "$tgt" | grep "^/" | tail -1)"
    if [ "$app" = "1" ]; then
      # Run from the .app so [NSBundle mainBundle] is Leksah.app (correct name
      # everywhere).  cabal relinks a new inode each build, so refresh the
      # bundle executable (hard link; copy across volumes) every launch.  exec
      # so leksah'\''s exit code still drives the relaunch loop.
      macos="$(pwd)/Leksah.app/Contents/MacOS"
      ln -f "$bin" "$macos/leksah" 2>/dev/null || cp -f "$bin" "$macos/leksah"
      exec "$macos/leksah" --develop-leksah "$@"
    fi
    exec "$bin" --develop-leksah "$@"'

  LEKSAH_EXIT_CODE=0
  if [ "$IN_TMUX" = "1" ]; then
    # A persistent session (shown as "Terminal 0" in leksah) tails leksah's
    # own log.  Created once on the `leksah` socket; closing it just stops the
    # tail, not leksah.
    # On leksah's own tmux server (leksah / leksah-<port>, see IDE.Web.Instance)
    # so it lists in THIS instance's Terminals pane, with a matching name.
    # NB: plain `new-session -d` (NOT `-A`): if the session already exists (a
    # relaunch), -A would turn this into an attach-session and BLOCK the loop
    # forever; without -A it just fails "duplicate session" and `|| true`
    # no-ops, leaving the existing log tail in place.
    tmux -L "leksah$INSTANCE_TAG" -f "$CONF" new-session -d -s "leksah$INSTANCE_TAG-0" tail -n +1 -F "$LOGFILE" || true
    echo "Launching leksah$LOG_TAG; its output appears as \"Terminal 0\" inside leksah (log: $LOGFILE)"
    PATH="$(pwd)/bin:$PATH" \
      bash -c "$launch_leksah" _ "$RUN_FROM_APP" "$EXE_TARGET" "$@" > "$LOGFILE" 2>&1 \
      || LEKSAH_EXIT_CODE=$?
  else
    PATH="$(pwd)/bin:$PATH" \
      bash -c "$launch_leksah" _ "$RUN_FROM_APP" "$EXE_TARGET" "$@" \
      || LEKSAH_EXIT_CODE=$?
  fi

  if [ "$UI" = "warp" ] && [ $LEKSAH_EXIT_CODE -eq 2 ]; then
    echo "leksah-warp rebuilt — relaunching (reload http://127.0.0.1:$LEKSAH_PORT/ when ready)"
  fi
  status "instance exited: code=$LEKSAH_EXIT_CODE"
done
