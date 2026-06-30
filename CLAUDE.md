# Working in this repo

## Build & run (primary dev loop)
- Primary front end: **`leksah-wkwebview`** (native macOS WKWebView), GHC **9.14.1**.
- Build:
  `nix develop ".?submodules=1#ghc914" --command cabal build --builddir dist-ghc-9.14.1 exe:leksah-wkwebview`
- Relaunch a running instance (preferred): **`leksah-cmd rebuild-self`** —
  rebuilds **incrementally, in place, while the app stays up** (streaming build
  output to the terminal), and only on success `exit(2)`s so `leksah-nix.sh`
  relaunches the already-built binary (a quick restart, no waiting for a rebuild
  with the window gone). On a build failure leksah is left running. The build
  command is `~/.leksah/rebuild.sh`, written by `leksah-nix.sh` to match the
  launch options (same `--builddir`/target); it calls `cabal build` directly in
  leksah's own dev-shell env (no nested `nix develop`), so a no-op build is
  ~instant (`Up to date`).
- `leksah-cmd restart` — the blunt version: `exit(2)` immediately, then
  `leksah-nix.sh` rebuilds + relaunches (window is gone during the rebuild). Both
  replace the older `./dev-relaunch.sh`.
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
  - `leksah-cmd rebuild-self` — incremental rebuild in place, then restart on
    success (preferred dev relaunch; see above).
  - `leksah-cmd restart` — blunt relaunch (exits 2 immediately; see above).
  - `leksah-cmd cm open FILE…` — open files in the editor (CodeMirror).
  - `leksah-cmd project open FILE…` — add project files to the workspace.
  - `leksah-cmd js eval 'CODE'` — **evaluate JS in the running leksah and print
    the result.** Very useful for inspecting/poking the live page from a shell
    (e.g. `leksah-cmd js eval 'document.querySelectorAll(".tab").length'`). Runs
    in every live jsaddle context via `ideJSM`; relative paths in the other
    commands resolve against the shell's cwd (sent over the socket), not leksah's.

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
