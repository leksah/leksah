# Working in this repo

## Build & run (primary dev loop)
- Primary front end: **`leksah-wkwebview`** (native macOS WKWebView), GHC **9.14.1**.
- **If leksah is already running, build with `leksah-cmd rebuild-self
  --use-cabal --no-restart` (agents/scripts) — do NOT run a separate
  `nix develop … cabal build`.** rebuild-self builds in leksah's own dev-shell
  env; a `nix develop` build from another shell has a *different* PATH, and
  alternating the two makes cabal treat it as "configuration changed" and
  rebuild the world (see the incremental-build invariant below). Use
  `--no-restart` to iterate without relaunching, then `leksah-cmd restart
  --no-rebuild` when ready.
- **`leksah-cmd rebuild-self [--no-restart] [--use-cabal]`** — rebuilds
  **incrementally, in place, while the app stays up**. Default: through
  **leksah's own build system** (errors/warnings land in the IDE's Errors/Log
  panes; with ghci mode on it goes via ffcabal's cached repls) — the reply is a
  fire-and-forget ack, NOT streamed output. **`--use-cabal` is the failsafe**
  (and what agents should use for scripted builds): it bypasses leksah's build
  code entirely — in case it's broken — running `~/.leksah/rebuild.sh` (written
  by leksah-nix.sh) directly and streaming output back; also the automatic
  fallback when no leksah package is open in the workspace. On success both
  paths `exit(2)` → relaunch — unless `--no-restart` (build lands on disk;
  relaunch later). Prefer `--no-restart` while iterating: restarting per build
  is how duplicate instances pile up (single-instance rule below). On failure
  leksah stays up. Don't close the socket mid-build (`… | head`); redirect to a
  file or `tail`.
- `leksah-cmd restart [--no-rebuild]` — exit immediately so `leksah-nix.sh`
  relaunches; plain restart exits 2 (loop rebuilds first), `--no-rebuild` exits
  3 (loop skips the build — use after rebuild-self already built). Both replace
  the older `./dev-relaunch.sh`.
- **When NO instance is running**, build with the captured env (config-identical
  PATH): `sh -c '. ~/.leksah/env.sh; cd <repo>; cabal build --builddir
  dist-ghc-9.14.1 <targets>'` (`~/.leksah/env.sh` is a snapshot of the running
  leksah's environment). A bare `nix develop` build only as a last resort.
- **ffcabal** (its own repo, `leksah/ffcabal`; pulled in via a
  `source-repository-package` in `cabal.project`, no longer a `vendor/`
  submodule): fail-fast cabal wrapper — checks
  each local component in cached tmux repls (session `ffcabal`, default server)
  in dep order, then builds in parallel. Leksah's native builds use it when
  **ghci mode (the `debug` pref) is on**; ghci mode off = plain cabal.
  Tests: `FFCABAL_BIN=$(cabal list-bin --builddir dist-ghc-9.14.1 ffcabal)
  cabal test --builddir dist-ghc-9.14.1 ffcabal-test --test-show-details=direct`.
  `FFCABAL_TMUX_ARGS="-L sock"` redirects its repls to a scratch tmux server.
- **Run exactly ONE `leksah-nix.sh` loop / one instance.** Each loop relaunches
  its own instance on `exit(2)`, and every instance's `startCmdServer` unlinks and
  rebinds `~/.leksah/cmd.sock` — so with several instances the newest wins the
  socket and the rest are orphaned (uncontrollable), and `leksah-cmd js eval` /
  `rebuild-self` hit whichever one currently owns it. If things get confused, kill
  all `leksah-wkwebview` + `leksah-nix.sh`, then start one. When counting with
  `pgrep -f <pattern>`, **exclude the self-match** — your own `pgrep`/shell command
  line contains the pattern and counts itself (match the running binary's exact
  argv, or `grep -v` your shell).
- **Where the loop runs / recovery.** The `leksah-nix.sh` loop runs in the
  **`launch`** tmux session (`tmux -L leksah capture-pane -p -t launch`), logging
  to `~/.leksah/leksah-nix-wkwebview.log`. It relaunches leksah on `exit(2/3)`;
  but if the loop *itself* dies — e.g. a `nix` eval error from a
  `cabal.project`/`flake.nix` edit that doesn't evaluate — **nothing relaunches**,
  and `leksah-cmd restart --wait` then **hangs forever** waiting for an instance
  that never comes. Restart the loop by running, in the `launch` session,
  `cd ~/haskell/leksah && ./leksah-nix.sh ghc914 wkwebview 2>&1 | tee ~/.leksah/leksah-nix-wkwebview.log`.
  **Before** pointing `cabal.project`/`flake.nix` at a not-yet-pushed
  `source-repository-package`, confirm the commit is on its remote
  (`git ls-remote <url> <rev>`) — an unfetchable ref fails the loop's nix eval and
  takes leksah down.
- **git while leksah runs.** leksah sets `GIT_OPTIONAL_LOCKS=0` (see `newIDE`) so
  its Changes/file-tree/workspace `git status`/`diff` pollers skip the
  index-refresh lock; your `git commit`/`add` in a terminal won't contend on
  `.git/index.lock`. (An old instance built before this fix still contends —
  retry the commit, or rebuild+restart to pick up the fix.)
- Full driver: `./leksah-nix.sh GHCVER [gtk|warp|wkwebview|webkitgtk]`
  (GHCVER ∈ ghc96/ghc98/ghc910/ghc912/ghc914; oldest supported GHC is 9.6.7).
- Build dir convention is `dist-ghc-<numeric-version>` — matches what leksah’s own
  in-IDE builds use, so don’t use a different `--builddir`.
- **Editing `leksah-nix.sh` requires restarting it** — a running `bash` reads the
  whole script at start, so loop edits only take effect on a fresh launch.
- **Incremental-build invariant (web UIs):** every `cabal` invocation must see the
  *same* `PATH` — cabal treats a different `PATH` as "configuration changed" and
  rebuilds the world. So `leksah-nix.sh` prefixes `bin/$GHCARG` on PATH for every
  cabal call, and — crucially — **launches the built binary directly** (`exec`
  via `cabal list-bin`, with `leksah_datadir="$(pwd)"`) rather than `cabal run`.
  `cabal run` augments the launched app's PATH with build-tool dirs, which would
  make the in-app `cabal build` of `rebuild-self` reconfigure + rebuild everything.
  Don't reintroduce `cabal run` for launching, and don't vary the cabal PATH.

## leksah-cmd (control socket)
- A running web-UI leksah listens on a Unix socket at `~/.leksah/cmd.sock`
  (server: `src/IDE/Web/CmdServer.hs`, started from `newIDE`). The `leksah-cmd`
  CLI (`main/Cmd.hs`, a tiny standalone exe — no leksah deps) drives it:
  - `leksah-cmd rebuild-self [--no-restart]` — incremental rebuild in place; with
    `--no-restart` the app stays up (build lands on disk), else it restarts on
    success. Preferred way to build while an instance is running; see above.
  - `leksah-cmd restart` — blunt relaunch (exits 2 immediately; see above).
  - `leksah-cmd cm open FILE…` — open files in the editor (CodeMirror).
  - `leksah-cmd project open FILE…` — add project files to the workspace.
  - `leksah-cmd js eval 'CODE'` — **evaluate JS in the running leksah and print
    the result.** Very useful for inspecting/poking the live page from a shell
    (e.g. `leksah-cmd js eval 'document.querySelectorAll(".tab").length'`). Runs
    in every live jsaddle context via `ideJSM`; relative paths in the other
    commands resolve against the shell's cwd (sent over the socket), not leksah's.
    `js eval -f FILE` / `js eval -` read the code from a file / stdin (avoids
    shell-quoting a big blob).
  - `leksah-cmd ping` / `leksah-cmd wait-ready` — is the socket answering / block
    until it does. Use `wait-ready` after a restart before scripting further.
  - `leksah-cmd restart [--no-rebuild] [--wait]` — `--wait` blocks until the NEW
    UI answers (⚠ hangs if the loop is dead — see recovery above); `--no-rebuild`
    skips the build (pair with a prior `rebuild-self --no-restart`).
  - `leksah-cmd rebuild-self [--no-restart] [--use-cabal]` — `--use-cabal` is the
    failsafe agents should use for scripted builds (bypasses leksah's own build
    code, streams output); `--no-restart` keeps the app up to iterate.
  - `leksah-cmd screenshot FILE` — capture the UI to a PNG (wkwebview only).
  - `leksah-cmd grab-region [TARGET]` — select a screen region; types its PNG path
    into the AI-target pane (`aiTarget`/`regionCaptureTarget` pref).
- **Recipes for verifying/driving a running leksah:**
  - *Inspect the live DOM* — `leksah-cmd js eval '<JS returning a value>'` (element
    `getBoundingClientRect`, counts, `getComputedStyle`). Fastest way to check a
    layout/CSS change; often enough on its own, no screenshot needed.
  - *See the UI* — `leksah-cmd screenshot /tmp/x.png`, then Read the PNG. Crop to
    the region of interest with ImageMagick (`magick /tmp/x.png -crop WxH+X+Y
    /tmp/c.png`, in the dev shell) or `sips -c H W --cropOffset Y X`.
  - *Drive native menus* (to test menu commands / confirm wiring) — the wkwebview
    process is named `leksah`: `osascript -e 'tell application "System Events" to
    tell process "leksah" to click menu item "NAME" of menu "MENU" of menu bar 1'`
    (needs Accessibility permission; `get name of every menu item of menu "MENU"…`
    reads them without clicking).
- **Status traffic light** (top-right; each state a distinct colour *and* shape
  for colour-blind accessibility). Set it so the user knows when to keep hands
  off: `leksah-cmd js eval 'leksahRestarting()'` (**blue diamond**) around a
  rebuild/restart, `'leksahStatus("red")'` (**red octagon**) or `'leksahTestStart()'`
  (orange triangle → beep → red) while interactively testing, and
  `'leksahTestEnd()'` / `'leksahStatus("green")'` (**green circle**, safe) when
  done. Use **blue for rebuild/restart**, red only for active tests.
- **Freeze/deadlock debugging.** A wedged window (heartbeat stops in the loop
  log) is almost always one window's reflex *frame thread* blocked on an `MVar`.
  Diagnostics: `leksah-cmd threads` / `stacks [SUBSTR]` / `resync-state`, the
  `[win N] alive ideVer=…` heartbeat lines, and `js eval 'Math.random()'` to
  test which jsaddle transports are live. Full toolset + step-by-step method:
  [docs/development/debugging-web-ui-freezes.md](docs/development/debugging-web-ui-freezes.md).

## Web UI architecture (src/IDE/Web, lib leksah-nogtk)
- Front ends share code: `leksah-warp` (browser at http://127.0.0.1:3367/),
  `leksah-wkwebview` (macOS), `leksah-webkitgtk`.
- **Do NOT set the WKWebView `uiDelegate`** — jsaddle-wkwebview uses it for its
  synchronous JS↔Haskell bridge. Overriding it silently breaks jsaddle, so the
  DOM/CSS never build (toolbar renders vertical, app unusable).
- CSS is generated with Clay and injected via `mainWidgetWithCss` **through
  jsaddle** — anything that breaks jsaddle also kills all styling.
- jsaddle-wkwebview dispatches events **asynchronously**, so `preventDefault` from
  a Haskell handler is unreliable; gate/handle in JS where it must be synchronous.
- Native code lives in `main/` (not the shared lib), so native↔reflex comms go
  through process-global `Chan`/`IORef` bridge modules in `src/IDE/Web/`
  (`CloseRequest`, `SaveRequest`, `FindRequest`, `OpenFileRequest`, `OpenPanel`,
  `RecentFiles`): drop a token, drain it from a background thread into an Event.
- To add front-end JS, prefer an **inline string eval’d in `Main.hs`** (see
  `revealCheckJs`, `terminalLinksJs`, `focusFindJs`) over editing the CodeMirror
  bundle: `cm6/leksah-cm6.js` is the built artifact; `cm6/src/leksah-cm6.mjs` is
  source and needs the bundler to rebuild.
- xterm.js terminals: the SearchAddon needs `allowProposedApi: true` on the
  Terminal, or its highlight decorations throw.

## Nix / haskell.nix
- The flake reads the **dirty working tree** (uncommitted edits ARE picked up;
  `git add` doesn’t change what nix sees — only file contents do).
- Uses haskell.nix `builderVersion = 2` (per-component "slice" builds). Each
  component exposes a `.checkAgainstPlan` derivation: build it and read
  `$out/diff-report.txt` to see why a slice’s UnitIds diverge from plan-nix.
- haskell.nix dev checkout: `/Users/hamish/iohk/haskell.nix`. Test builder changes
  with `--override-input haskellNix path:/Users/hamish/iohk/haskell.nix` — note
  this is **rebuild-the-world** (changes every slice’s drv hash).
