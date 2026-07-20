#!/usr/bin/env bash -e

if [ $# -eq 0 ]
  then
    echo "Usage: ./leksah.sh [--nix] [--warp|--classic|--ghci] [GHCVER] [--in-tmux] [LEKSAH_ARGS]"
    echo
    echo "  --nix     : re-enter the nix dev shell for every build/run command"
    echo "              (nix develop \".?submodules=1#GHCVER\").  WITHOUT --nix"
    echo "              (the default) commands run in the AMBIENT environment, so"
    echo "              you must already be inside a dev shell (or otherwise have"
    echo "              ghc/cabal/tmux/leksah-server on PATH) — this skips the slow"
    echo "              per-command nix eval.  leksah-server/leksah-cmd/ffcabal and"
    echo "              the front end are still built with cabal either way."
    echo "  (default) : the native web front end, exe:leksah (WKWebView on macOS,"
    echo "              WebKitGTK on Linux — one exe, chosen per-OS in the cabal file)."
    echo "  --warp    : the browser front end, exe:leksah-warp (http://127.0.0.1:PORT/)."
    echo "  --classic : the classic Gtk front end, exe:leksah-classic."
    echo "  --ghci    : run the native web front end INTERPRETED in a cabal multi-repl"
    echo "              (cabal repl exe:leksah lib:leksah-nogtk) inside a tmux session,"
    echo "              so 'leksah-cmd rebuild-self' becomes :reload + :main (seconds,"
    echo "              no relink) and 'leksah-cmd hs eval' can poke the live IDE."
    echo "              First load compiles everything to bytecode — slow, once."
    echo "  GHCVER    : optional, defaults to ghc914 (web front ends); ghc8107 or ghc98 (--classic)"
    echo "  --in-tmux : (web front ends) run leksah inside a tmux session so its own"
    echo "              output shows up as \"Terminal 0\" in leksah's Terminals pane"
    echo
    echo "  Env LEKSAH_PORT=N runs a SECOND instance alongside the default one:"
    echo "  N (default 3367) is the UI port; leksah keys its control socket and"
    echo "  tmux server off it, and this script its run log + \"Terminal 0\" session."
    echo
    echo "Examples: ./leksah.sh --nix                       # GHCVER defaults to ghc914"
    echo "          ./leksah.sh --nix ghc914"
    echo "          ./leksah.sh --nix ghc914 --verbosity=DEBUG"
    echo "          ./leksah.sh --warp ghc914 --in-tmux    # ambient shell, no nix"
    echo "          ./leksah.sh --nix --classic ghc98"
    echo "          LEKSAH_PORT=3368 ./leksah.sh --nix ghc914  # 2nd instance"
    echo
    echo "For details of other LEKSAH_ARGS run: ./leksah.sh --nix --classic ghc98 --help"
    exit 1
fi

# Remember the full invocation before we consume the arguments, so it can be
# echoed into the run log below.
INVOCATION="$0 $*"

# Pull the script's own flags out from anywhere in the argument list; whatever
# is left is positional (GHCVER, then LEKSAH_ARGS).  There is a single native
# web exe (exe:leksah, chosen per-OS in the cabal file) — the default; --warp
# and --classic select the two alternative front ends instead.
USE_NIX=0
IN_TMUX=0
GHCI=0
UI=leksah
POS=()
for a in "$@"; do
    case "$a" in
        --nix)     USE_NIX=1 ;;
        --in-tmux) IN_TMUX=1 ;;
        --ghci)    GHCI=1 ;;
        --warp)    UI=warp ;;
        --classic) UI=classic ;;
        *)         POS+=("$a") ;;
    esac
done
set -- "${POS[@]}"

# GHCVER is optional and defaults to ghc914 (the only enabled dev shell).  A
# leading positional that looks like a GHC version (ghc<digits>) is consumed as
# GHCVER; otherwise every positional is treated as a LEKSAH_ARG (so e.g.
# `./leksah.sh --nix --verbosity=DEBUG` runs ghc914 and passes the flag on).
case "${1:-}" in
    ghc[0-9]*) GHCARG=$1; shift ;;
    *)         GHCARG=ghc914 ;;
esac

# On macOS the default exe:leksah is the WKWebView front end, which we run from a
# real Leksah.app bundle (correct name in the menu bar / Dock / ⌘-Tab).  --warp
# and --classic don't; nor does the Linux exe:leksah (WebKitGTK).
RUN_FROM_APP=0
if [ "$UI" = "leksah" ] && [ "$(uname)" = "Darwin" ]; then RUN_FROM_APP=1; fi

# How every build/run command is dispatched.  With --nix we re-enter the dev
# shell per command (slow nix eval, but self-contained); the default runs the
# command directly in the ambient environment (fast — assumes the toolchain is
# already on PATH, i.e. you're inside a dev shell).  Used as a command prefix:
# `"${DEV[@]}" cabal build …` expands to the plain command when DEV is empty.
# NIX_ARGS is intentionally unquoted so a multi-word override word-splits.
# NIX_ARGS='--system x86_64-darwin'
if [ "$USE_NIX" = 1 ]; then
    # Flake variants are disabled for now (hix.nix flake.variants is commented
    # out), so there is no `#ghc914` dev shell any more — the DEFAULT shell is
    # ghc914 (hix.nix compiler-nix-name).  Map ghc914 to it; other GHCVERs
    # would need their variant re-enabled.
    SHELL_ATTR=$GHCARG
    [ "$GHCARG" = "ghc914" ] && SHELL_ATTR=default
    DEV=(nix $NIX_ARGS develop ".?submodules=1#$SHELL_ATTR" --show-trace --command)
else
    DEV=()
fi

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
# logs are just leksah[-run].log; -warp / -classic for the alternatives.
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
echo "Parsed: GHCVER=$GHCARG UI=$UI USE_NIX=$USE_NIX IN_TMUX=$IN_TMUX LEKSAH_PORT=$LEKSAH_PORT LEKSAH_ARGS=[$*]"

# Match the build dir leksah itself uses (dist-ghc-<version>), so the build this
# script drives and leksah's own builds/metadata read the same place.  Querying
# the dev shell's GHC gives the exact version leksah was built with.
GHCNUMVER=$("${DEV[@]}" ghc --numeric-version 2>/dev/null | tail -1)
BUILDDIR="dist-ghc-${GHCNUMVER:-$GHCARG}"
echo "Using build dir: $BUILDDIR"

# A self-contained rebuild script for `leksah-cmd rebuild-self`.  It runs inside
# the already-running leksah's dev-shell environment (cabal/ghc are already on
# PATH), so it calls cabal directly rather than re-entering `nix develop`.  Uses
# the same build dir + target leksah was launched with.
# Map the UI selector to its cabal executable target.  The native web front end
# (WKWebView on macOS, WebKitGTK on Linux, WebView2 on Windows) is a single
# exe:leksah selected per-OS in the cabal file — the default; --classic is the
# classic GTK exe:leksah-classic; --warp is exe:leksah-warp.
case "$UI" in
    classic) EXE_TARGET="exe:leksah-classic" ;;
    warp)    EXE_TARGET="exe:leksah-warp" ;;
    *)       EXE_TARGET="exe:leksah" ;;
esac
REBUILD_TARGET="$EXE_TARGET"
mkdir -p "$HOME/.leksah"
cat > "$HOME/.leksah/rebuild.sh" <<EOF
#!/bin/sh
# Generated by leksah.sh; run by 'leksah-cmd rebuild-self'.  leksah is launched
# directly (not via 'cabal run'), so its environment is identical to the build
# environment — calling cabal directly here matches the loop's build config and
# stays incremental (no slow nested 'nix develop', no full rebuild).
cd "$(pwd)" || exit 1
exec cabal build --builddir "$BUILDDIR" $REBUILD_TARGET exe:leksah-cmd exe:ffcabal
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
    if [ -f osx/leksah-macapp.icns ]; then
        cp -f osx/leksah-macapp.icns "$APPBUNDLE/Contents/Resources/leksah.icns"
    elif [ -f osx/leksah.icns ]; then
        cp -f osx/leksah.icns "$APPBUNDLE/Contents/Resources/leksah.icns"
    fi
fi

if [ "$IN_TMUX" = "1" ] && [ "$UI" = "classic" ]; then
    echo "Note: --in-tmux only applies to the web front ends; ignoring for --classic."
    IN_TMUX=0
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

# --ghci: run the native web front end INTERPRETED in a cabal multi-repl
# instead of building + launching the binary.  The repl lives in a tmux pane
# (on leksah's own tmux server) so `leksah-cmd hs eval` and the reload flow
# (rebuild-self against a ghci instance = :reload + :main) can drive the
# prompt with send-keys; the pane's exact environment — including the PATH
# invariant every cabal call must share — is captured into a generated script.
# There is no exit-2/3 relaunch loop here: the ghci session IS the loop.
if [ "$GHCI" = "1" ]; then
  if [ "$UI" != "leksah" ]; then
    echo "--ghci only applies to the native web front end (exe:leksah); ignoring --$UI." >&2
  fi
  rm -f .ghc.environment.*
  mkdir -p bin
  # Same helper prebuild as the binary arm (leksah-server/leksah-cmd/ffcabal
  # must be on PATH for the IDE), same PATH prefix — the repl's dependency
  # builds then share the same plan and stay incremental.
  PATH=$(pwd)/bin/$GHCARG:$PATH "${DEV[@]}" \
    bash -c '
      set -e
      bd="$1"; gd="$2"
      if command -v haskell-nix-cabal-project-local-sync >/dev/null 2>&1; then
        haskell-nix-cabal-project-local-sync --force
      fi
      if command -v haskell-nix-cabal-store-sync >/dev/null 2>&1; then
        haskell-nix-cabal-store-sync --force
      fi
      cabal build --builddir "$bd" exe:leksah-server exe:leksah-cmd exe:ffcabal
      mkdir -p "bin/$gd"
      ln -sf "$(cabal list-bin --builddir "$bd" exe:leksah-server)" "bin/$gd/leksah-server"
      ln -sf "$(cabal list-bin --builddir "$bd" exe:leksah-cmd)"    "bin/$gd/leksah-cmd"
      ln -sf "$(cabal list-bin --builddir "$bd" exe:ffcabal)"       "bin/$gd/ffcabal"
    ' _ "$BUILDDIR" "$GHCARG"

  # ghci uses its OWN builddir: its config differs from the binary loop's
  # (the -objc-in-library flags below), and sharing a builddir would make
  # cabal reconfigure + rebuild the whole leksah package on every switch
  # between the two modes.  Dependencies still come from the shared store.
  GHCI_BUILDDIR="dist-ghci-${GHCNUMVER:-$GHCARG}"

  # GHCi's RTS linker can load Objective-C objects but never registers their
  # classes with the ObjC runtime (only dyld does) — so in ghci mode the ObjC
  # is kept OUT of the Haskell archives (-objc-in-library flags) and preloaded
  # as dylibs instead (ghci -L/-l), where the Haskell foreign imports resolve
  # against them.  Compile both dylibs: leksah's own native glue and
  # jsaddle-wkwebview's (from the unpacked source-repository-package).
  GHCI_NATIVE="$RUNLOGDIR/ghci-native"
  mkdir -p "$GHCI_NATIVE"
  # Both dylibs are OS-native (Cocoa/WebKit/ApplicationServices).  We LINK them
  # with the SYSTEM toolchain (xcrun clang) rather than the dev-shell cc: some
  # pinned haskell.nix toolchains drive an old cctools `ld` (ld64-956.6) from
  # clang-21 + llvm-21's libLTO, which SIGTRAPs (`Trace/BPT trap`) on any
  # `-dynamiclib` link that pulls in Cocoa.  The system linker isn't affected,
  # and native dylibs are exactly what it's for.  Only the RTS include dirs and
  # the unpacked jsaddle source path come from the dev shell (last stdout line).
  DYLIB_INFO=$("${DEV[@]}" bash -c '
      js=$(ls -d dist-ghc-*/src/jsaddle-*/jsaddle-wkwebview 2>/dev/null | head -1)
      inc=""; for d in $(ghc-pkg field rts include-dirs --simple-output); do inc="$inc -I$d"; done
      printf "%s %s\n" "$js" "$inc"' 2>/dev/null | tail -1)
  read -r JS_SRC HS_INC <<< "$DYLIB_INFO"
  if [ -z "$JS_SRC" ]; then
    echo "jsaddle-wkwebview source not unpacked yet — building dependencies first" >&2
    exit 1
  fi
  # env -i so that when leksah.sh runs inside a dev shell (the no-`--nix` path) the
  # nix cc-wrapper vars don't redirect the system clang back to the broken linker.
  syscc() { env -i PATH=/usr/bin:/bin HOME="$HOME" /usr/bin/xcrun clang "$@"; }
  syscc -dynamiclib main/leksah-mac-menu.m \
     -framework Cocoa -framework ApplicationServices -framework AVFoundation \
     -o "$GHCI_NATIVE/libleksah-mac-menu.dylib"
  syscc -dynamiclib "$JS_SRC/cbits-cocoa/WKWebView-AppDelegate.m" \
     -DUSE_COCOA -I"$JS_SRC/cbits" $HS_INC -Wno-everything \
     -framework Foundation -framework WebKit -framework Cocoa \
     -o "$GHCI_NATIVE/libjsaddle-wkwebview-objc.dylib"

  TMUXSOCK="leksah$INSTANCE_TAG"
  GHCI_LOG="$RUNLOGDIR/ghci$INSTANCE_TAG.log"
  GHCI_RUN="$RUNLOGDIR/ghci-run$INSTANCE_TAG.sh"
  # tmux comes from the dev shell with --nix; resolve it once (polling through
  # `nix develop` would re-evaluate the flake every 2s).
  TMUX_BIN=$("${DEV[@]}" bash -c 'command -v tmux')
  if [ "$USE_NIX" = 1 ]; then
    DEVSTR="nix $NIX_ARGS develop \".?submodules=1#$SHELL_ATTR\" --show-trace --command"
  else
    DEVSTR=""
  fi
  cat > "$GHCI_RUN" <<EOF
#!/usr/bin/env bash
# Generated by leksah.sh --ghci; exec'd inside the ghci tmux pane.  Captures
# the launching shell's environment so every cabal call (this repl, the
# binary-arm builds, rebuild-self) sees the SAME PATH — cabal treats a
# different PATH as "configuration changed" and rebuilds the world.
cd "$(pwd)" || exit 1
export LEKSAH_PORT=$LEKSAH_PORT
export LEKSAH_GHCI=1
export leksah_datadir="$(pwd)"
export PATH="$(pwd)/bin/$GHCARG:$PATH"
# TERM=dumb makes ghci's haskeline drop all cursor/keypad control escapes, so
# the prompt no longer rewrites its line — echoed input and command output land
# on separate clean lines.  leksah-cmd (hs eval / rebuild-self) scrapes the pane
# for its output fences; without this, haskeline glues a command echo to the
# previous output and the scrape misses (or swallows) results.
export TERM=dumb
exec $DEVSTR cabal repl leksah:exe:leksah leksah:lib:leksah-nogtk \\
  --enable-multi-repl --builddir "$GHCI_BUILDDIR" \\
  --constraint="leksah -objc-in-library" \\
  --constraint="leksah +no-hlint" \\
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
    ghci_ok=0
    for gattempt in 1 2 3 4 5 6; do
      "$TMUX_BIN" -L "$TMUXSOCK" kill-session -t ghci 2>/dev/null || true
      : > "$GHCI_LOG"
      PANE=$("$TMUX_BIN" -L "$TMUXSOCK" new-session -d -P -F '#{pane_id}' -s ghci "$GHCI_RUN")
      "$TMUX_BIN" -L "$TMUXSOCK" pipe-pane -o -t "$PANE" "cat >> $GHCI_LOG"
      printf '%s %s\n' "$TMUXSOCK" "$PANE" > "$RUNLOGDIR/ghci-pane$INSTANCE_TAG"
      echo "cabal repl starting in tmux (-L $TMUXSOCK, session ghci, pane $PANE; attempt $gattempt)"
      echo "Log: $GHCI_LOG   Pane file: $RUNLOGDIR/ghci-pane$INSTANCE_TAG"
      echo "Waiting for the ghci prompt (the first load compiles/loads everything — slow, once)…"
      prompt=0
      while :; do
        dead=$("$TMUX_BIN" -L "$TMUXSOCK" display-message -p -t "$PANE" '#{pane_dead}' 2>/dev/null) || dead=1
        if [ "$dead" = "1" ]; then break; fi
        last=$("$TMUX_BIN" -L "$TMUXSOCK" capture-pane -p -t "$PANE" 2>/dev/null | grep -v '^[[:space:]]*$' | tail -1)
        case "$last" in
          *"ghci>"*) prompt=1; break ;;
        esac
        sleep 2
      done
      if [ "$prompt" != 1 ]; then
        echo "ghci exited before the prompt (attempt $gattempt) — see $GHCI_LOG; retrying."
        continue
      fi
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
      LEKSAHCMD="$(pwd)/bin/$GHCARG/leksah-cmd"
      for _wait in $(seq 1 75); do
        if grep -q "Relocation out of range" "$GHCI_LOG" 2>/dev/null; then outcome=reloc; break; fi
        dead=$("$TMUX_BIN" -L "$TMUXSOCK" display-message -p -t "$PANE" '#{pane_dead}' 2>/dev/null) || dead=1
        if [ "$dead" = "1" ]; then outcome=dead; break; fi
        if grep -q "alive ideVer" "$GHCI_LOG" 2>/dev/null; then
          # The heartbeat is up — but the RTS linker can also load reflex with a
          # SILENTLY bad relocation (no crash message): the app runs yet the
          # reflex network is corrupt and never builds the DOM.  So require the
          # DOM to actually populate (max across windows) before declaring
          # success; otherwise it's a dud layout — retry.
          dom=$("$LEKSAHCMD" js eval 'document.querySelectorAll("*").length' 2>/dev/null \
                  | grep -oE '[0-9]+' | sort -n | tail -1)
          if [ -n "$dom" ] && [ "$dom" -gt 2000 ] 2>/dev/null; then outcome=up; break; fi
        fi
        sleep 2
      done
      case "$outcome" in
        up) ghci_ok=1; echo "leksah is up (attempt $gattempt, DOM=$dom)."; break ;;
        reloc) echo "RTS-linker relocation lottery lost (attempt $gattempt) — retrying with a fresh layout." ;;
        *) echo "ghci UI did not build (attempt $gattempt, outcome=${outcome:-frozen}) — retrying with a fresh layout." ;;
      esac
    done
    if [ "$ghci_ok" != 1 ]; then
      echo "Gave up starting the ghci session after retries — see $GHCI_LOG." >&2
      exit 1
    fi
  fi
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

LEKSAH_EXIT_CODE=2

# Exit 2 => relaunch after rebuilding (in-IDE / rebuild-self); exit 3 =>
# `leksah-cmd restart --no-rebuild`: relaunch but skip the cabal build (and its
# `nix develop`), since rebuild-self already produced the binary.
while [ $LEKSAH_EXIT_CODE -eq 2 ] || [ $LEKSAH_EXIT_CODE -eq 3 ]; do
  SKIP_REBUILD=0
  [ "$LEKSAH_EXIT_CODE" -eq 3 ] && SKIP_REBUILD=1
  rm -f .ghc.environment.*
  mkdir -p bin

  if [ "$UI" = "classic" ]; then
    # Classic Gtk: install the binaries, then launch through the `launch-leksah`
    # wrapper (which sets up the Gtk runtime environment).
    if [ "$SKIP_REBUILD" != 1 ]; then
      "${DEV[@]}" \
        cabal install --builddir "$BUILDDIR" --installdir bin/$GHCARG --overwrite-policy=always \
          exe:leksah-server exe:leksah-classic exe:leksahecho exe:vcswrapper exe:vcsgui exe:vcsgui-askpass \
          || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
    else
      echo "leksah-cmd restart --no-rebuild: skipping build, relaunching."
    fi
    rm -f .ghc.environment.*

    LEKSAH_EXIT_CODE=0
    if [ "$USE_NIX" = 1 ]; then
      # launch-leksah is a nix app that sets up the Gtk runtime environment.
      PATH=$(pwd)/bin/$GHCARG:$PATH nix $NIX_ARGS run .?submodules=1#launch-leksah -- ./bin/$GHCARG/leksah-classic --develop-leksah "$@" \
        || LEKSAH_EXIT_CODE=$?
    else
      # Ambient: assume the Gtk runtime env is already in place (dev shell).
      PATH=$(pwd)/bin/$GHCARG:$PATH ./bin/$GHCARG/leksah-classic --develop-leksah "$@" \
        || LEKSAH_EXIT_CODE=$?
    fi
  else
    # Web front ends (default exe:leksah, or --warp): leksah-server must be on
    # PATH (for metadata) and tmux is needed for persistent terminals — both come
    # from the dev shell.  With --develop-leksah leksah exits with code 2 when rebuilt
    # (in-IDE or via `leksah-cmd rebuild-self`), so this loop relaunches it.
    #
    # Build the helper exes AND the front end with one `cabal build` (one project
    # plan — `cabal install` would resolve a separate plan and rebuild the world),
    # symlink the helpers onto PATH, then launch the built binary DIRECTLY rather
    # than via `cabal run`.  Two reasons every cabal step shares the bin/$GHCARG
    # PATH prefix and we avoid `cabal run`:
    #   * cabal treats a different PATH as "configuration changed" and rebuilds
    #     everything, so the prefix must be identical across all cabal calls;
    #   * `cabal run` augments the launched app's PATH with build-tool dirs, which
    #     would make an in-app `cabal build` (rebuild-self) see a different config
    #     and rebuild everything.  Launching the binary directly keeps the app's
    #     environment identical to the build environment, so rebuild-self stays
    #     incremental.
    if [ "$SKIP_REBUILD" != 1 ]; then
      PATH=$(pwd)/bin/$GHCARG:$PATH "${DEV[@]}" \
        bash -c '
          set -e
          bd="$1"; gd="$2"; tgt="$3"
          # haskell.nix dev shells ship sync helpers that write
          # cabal.project.local (mirroring the shell'\''s cabalProjectLocal) and
          # prime the cabal store for this compiler.  They must run before the
          # build and in the SAME shell (one nix develop entry); --force
          # replaces stale state left by a different compiler (e.g. a
          # ghc914-sh cabal.project.local breaks mainline ghc914 configure).
          if command -v haskell-nix-cabal-project-local-sync >/dev/null 2>&1; then
            haskell-nix-cabal-project-local-sync --force
          fi
          if command -v haskell-nix-cabal-store-sync >/dev/null 2>&1; then
            haskell-nix-cabal-store-sync --force
          fi
          cabal build --builddir "$bd" exe:leksah-server exe:leksah-cmd exe:ffcabal "$tgt"
          mkdir -p "bin/$gd"
          ln -sf "$(cabal list-bin --builddir "$bd" exe:leksah-server)" "bin/$gd/leksah-server"
          ln -sf "$(cabal list-bin --builddir "$bd" exe:leksah-cmd)"    "bin/$gd/leksah-cmd"
          ln -sf "$(cabal list-bin --builddir "$bd" exe:ffcabal)"       "bin/$gd/ffcabal"
        ' _ "$BUILDDIR" "$GHCARG" "$EXE_TARGET" \
          || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
    else
      echo "leksah-cmd restart --no-rebuild: skipping build, relaunching."
    fi
    rm -f .ghc.environment.*

    # Launch the freshly-built binary directly, inside the dev shell, with the
    # data dir `cabal run` would have set (the package root).  `exec` so leksah's
    # exit code propagates (2 => rebuilt => relaunch).
    launch_leksah='
      bd="$1"; app="$2"; tgt="$3"; shift 3
      export leksah_datadir="$(pwd)"
      bin="$(cabal list-bin --builddir "$bd" "$tgt")"
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
      "${DEV[@]}" \
        tmux -L "leksah$INSTANCE_TAG" -f "$CONF" new-session -d -s "leksah$INSTANCE_TAG-0" tail -n +1 -F "$LOGFILE" || true
      echo "Launching leksah$LOG_TAG; its output appears as \"Terminal 0\" inside leksah (log: $LOGFILE)"
      PATH=$(pwd)/bin/$GHCARG:$PATH "${DEV[@]}" \
        bash -c "$launch_leksah" _ "$BUILDDIR" "$RUN_FROM_APP" "$EXE_TARGET" "$@" > "$LOGFILE" 2>&1 \
        || LEKSAH_EXIT_CODE=$?
    else
      PATH=$(pwd)/bin/$GHCARG:$PATH "${DEV[@]}" \
        bash -c "$launch_leksah" _ "$BUILDDIR" "$RUN_FROM_APP" "$EXE_TARGET" "$@" \
        || LEKSAH_EXIT_CODE=$?
    fi

    if [ "$UI" = "warp" ] && [ $LEKSAH_EXIT_CODE -eq 2 ]; then
      echo "leksah-warp rebuilt — relaunching (reload http://127.0.0.1:$LEKSAH_PORT/ when ready)"
    fi
  fi
done
