# Working in this repo

## Build & run (primary dev loop)
- Primary front end: **exe:leksah** (native macOS WKWebView); plain cabal in
  the **ambient** environment — cabal picks the GHC from PATH, builds land in
  cabal's default **`dist-newstyle`**. (leksah.sh no longer has any nix
  support, compiler selection, or builddir logic.)
- **If leksah is already running, build with `leksah-cmd rebuild-self
  --use-cabal --no-restart` (agents/scripts) — do NOT run a separate
  `cabal build` from another shell.** rebuild-self builds with the running
  loop's exact PATH (the `bin/` prefix); a build from a shell with a
  *different* PATH makes cabal treat it as "configuration changed" and
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
  by leksah.sh) directly and streaming output back; also the automatic
  fallback when no leksah package is open in the workspace. On success both
  paths `exit(2)` → relaunch — unless `--no-restart` (build lands on disk;
  relaunch later). Prefer `--no-restart` while iterating: restarting per build
  is how duplicate instances pile up (single-instance rule below). On failure
  leksah stays up. Don't close the socket mid-build (`… | head`); redirect to a
  file or `tail`.
- `leksah-cmd restart [--no-rebuild]` — exit immediately so `leksah.sh`
  relaunches; plain restart exits 2 (loop rebuilds first), `--no-rebuild` exits
  3 (loop skips the build — use after rebuild-self already built).
- **ghci mode**: `./leksah.sh --ghci` runs the app INTERPRETED in a cabal
  multi-repl (tmux session `ghci` on `-L leksah`; first bytecode load is slow,
  once). The repl flips cabal flags (`leksah -objc-in-library`,
  `jsaddle-wkwebview -objc-in-library`) and shares `dist-newstyle` with the
  binary arm, so switching between --ghci and binary launches makes cabal
  reconfigure the flag-flipped packages. Against a ghci instance
  (`leksah-cmd mode` → `ghci`),
  `rebuild-self`/`restart` become `:reload` + `:main` at the prompt (seconds,
  no relink; `--no-restart`/`--use-cabal` don't apply), and
  `leksah-cmd hs eval 'CODE'` evaluates Haskell in the live process (suspends
  the UI run loop, evals at the prompt, resumes — `js eval`'s Haskell
  sibling). The ObjC + foreign-export glue lives in the compiled
  `leksah-mac-glue` sublib (foreign export is illegal interpreted);
  `IDE.Web.GhciMode` gates the exit sites so "restart" stops `[NSApp run]`
  instead of killing the ghci process. See docs/building.md "ghci mode".
- **After a `cabal.project` / dependency change** use a plain `restart` (exit
  2 — the loop rebuilds against the new plan), not `rebuild-self` +
  `restart --no-rebuild`.
- **When NO instance is running**, build with the loop's PATH prefix so the
  config matches: `cd <repo> && PATH="$(pwd)/bin:$PATH" cabal build <targets>`.
- **ffcabal** (its own repo, `leksah/ffcabal`; pulled in via a
  `source-repository-package` in `cabal.project`, no longer a `vendor/`
  submodule): fail-fast cabal wrapper — checks
  each local component in cached tmux repls (session `ffcabal`, default server)
  in dep order, then builds in parallel. Leksah's native builds use it when
  **ghci mode (the `debug` pref) is on**; ghci mode off = plain cabal.
  Tests: `FFCABAL_BIN=$(cabal list-bin ffcabal)
  cabal test ffcabal-test --test-show-details=direct`.
  `FFCABAL_TMUX_ARGS="-L sock"` redirects its repls to a scratch tmux server.
- **Run exactly ONE `leksah.sh` loop / one instance.** Each loop relaunches
  its own instance on `exit(2)`, and every instance's `startCmdServer` unlinks and
  rebinds `~/.leksah/cmd.sock` — so with several instances the newest wins the
  socket and the rest are orphaned (uncontrollable), and `leksah-cmd js eval` /
  `rebuild-self` hit whichever one currently owns it. If things get confused, kill
  all `leksah-wkwebview` + `leksah.sh`, then start one. When counting with
  `pgrep -f <pattern>`, **exclude the self-match** — your own `pgrep`/shell command
  line contains the pattern and counts itself (match the running binary's exact
  argv, or `grep -v` your shell).
- **Where the loop runs / recovery.** The `leksah.sh` loop runs in the
  **`launch`** tmux session (`tmux -L leksah capture-pane -p -t launch`); the
  script tees its own output to `~/.leksah/leksah-run.log`. It relaunches
  leksah on `exit(2/3)`; but if the loop *itself* dies — e.g. a build error
  that exits the script — **nothing relaunches**, and `leksah-cmd restart
  --wait` then **hangs forever** waiting for an instance that never comes.
  Restart the loop by running, in the `launch` session,
  `cd ~/haskell/leksah && ./leksah.sh` (ghc/cabal/tmux must be
  on that shell's PATH; the default front end is exe:leksah — WKWebView on
  macOS).
  **Before** pointing `cabal.project` at a not-yet-pushed
  `source-repository-package`, confirm the commit is on its remote
  (`git ls-remote <url> <rev>`) — an unfetchable rev fails the loop's build and
  takes leksah down.
- **Watching a launch (agents): poll `~/.leksah/launch-status`, not the pane.**
  leksah.sh appends one line per phase transition (`starting:` → `building` /
  `ghci: repl loading (attempt N)` → `up:` / `failed:` / `exited: code=N`,
  truncated at every launch, EXIT-trapped so even a set -e death writes a
  line).  A file has no scrollback and describes exactly one run — the two
  ways pane-grep watchers went wrong (below) can't happen.  If you must watch
  a pane anyway, four MUSTs, each one a real incident:
  1. **Arm-time zero-match check**: run the pattern against the pane once
     BEFORE arming; if it already matches (previous run's outcome text in
     scrollback, or your own command echo containing the marker), the pattern
     is broken — fix it first.
  2. **A terminal condition for process death** (launcher pid gone / prompt
     returned / broad `error:`): the failure vocabulary you can enumerate is
     never complete, and silence looks identical to "still building".
  3. **On fire, print the matched line**; watcher-says-event while the pane
     shows nothing IS the finding — resolve it before ending the turn.
  4. **Never report a watcher as "armed" without checking it is still
     running** — a completed watcher is not a watcher.
- **A Stop hook enforces this** (`.claude/hooks/leksah-health.sh`, wired in
  `.claude/settings.local.json`, gitignored — per-machine): whenever an agent
  turn ends while an instance is *expected* (ghci session / leksah-wkwebview /
  leksah.sh alive), it checks the two primary signals — `leksah-cmd ping`
  answers `ok` (6s timeout) and the `[win N] alive` heartbeat log is <45s old —
  and **blocks the stop once** with the launch-status tail if either fails.  A
  blocked stop is not noise: it means the app is down or the frame thread is
  wedged, so investigate (`~/.leksah/launch-status`, `ghci.log`, the `ghci`
  pane) before ending the turn, or state plainly that it is intentionally down.
  Never route health commands (`ping`, `wait-ready`, `rebuild-self`) to
  `/dev/null` in a loop — capture and assert on their output.
  Also peek ~2 min after any launch: the fast failure modes (nix eval /
  cabal solver) all surface in the first minutes; success takes tens of
  minutes.
- **git while leksah runs.** leksah sets `GIT_OPTIONAL_LOCKS=0` (see `newIDE`) so
  its Changes/file-tree/workspace `git status`/`diff` pollers skip the
  index-refresh lock; your `git commit`/`add` in a terminal won't contend on
  `.git/index.lock`. (An old instance built before this fix still contends —
  retry the commit, or rebuild+restart to pick up the fix.)
- Full driver: `./leksah.sh [--warp|--ghci] [--in-tmux] [ARGS]`
  (a bare `./leksah.sh` runs the default front end; `--help` prints usage).
  **Front end**: default is the native web exe:leksah (WKWebView on macOS,
  WebKitGTK on Linux — one exe, chosen per-OS in the cabal file); `--warp` is
  exe:leksah-warp (browser).
  **The classic Gtk IDE is NOT part of this project any more**: `leksah-classic/`
  stays GPLv2 and has its **own cabal.project, flake.nix/flake.lock,
  nix/hix.nix, hie.yaml and vendor/** (see docs/relicensing.md), meant to be
  liftable into its own repo. Build it from inside that directory
  (`cd leksah-classic && nix develop` / `cabal build leksah-classic:exe:leksah-classic`);
  it pins its own compiler (ghc914-sh) and carries its own copies of the
  toolchain workarounds and of `vendor/gi-gtkosxapplication`. Nothing in the root
  project — cabal.project, flake, leksah.sh, hie.yaml — refers to it. Don't add
  it back to the root plan.
  Commands run in the **ambient** environment (ghc/cabal/tmux must be on
  PATH); leksah-cmd/ffcabal + the front end are built with cabal.  (leksah.sh
  still builds and links exe:leksah-server, and the installers ship it, but
  nothing runs it: the app has no invocation of it and the Metadata pane is a
  tombstone — so it is not a PATH requirement.)
- Build dir is cabal's default `dist-newstyle` (never pass `--builddir` —
  leksah's own in-IDE builds use the default too, via `cabalBuildDir`).
- **Editing `leksah.sh` requires restarting it** — a running `bash` reads the
  whole script at start, so loop edits only take effect on a fresh launch.
- **Incremental-build invariant (web UIs):** every `cabal` invocation must see the
  *same* `PATH` — cabal treats a different `PATH` as "configuration changed" and
  rebuilds the world. So `leksah.sh` prefixes `bin/` on PATH for every
  cabal call, and — crucially — **launches the built binary directly** (`exec`
  via `cabal list-bin`, with `leksah_datadir="$(pwd)"`) rather than `cabal run`.
  `cabal run` augments the launched app's PATH with build-tool dirs, which would
  make the in-app `cabal build` of `rebuild-self` reconfigure + rebuild everything.
  Don't reintroduce `cabal run` for launching, and don't vary the cabal PATH.

## Repo layout
- **The `leksah` package lives in `leksah/`**, not at the repo root: `src/`,
  `main/`, `src-mac-glue/`, `src-ghcjs-stub/`, `osx/`, `linux/` and the runtime
  asset dirs `pics/`, `cm6/`, `xterm/`, `monaco/`, `fonts/` are all under it,
  next to `leksah.cabal`. `cabal.project` lists it like any other package
  (`leksah/`, `lsp-types-client/`, `sandpit/breakout/`). Don't add sources or
  assets back at the root.
- **`leksah/` IS the datadir.** `leksah.sh` exports
  `leksah_datadir="$(pwd)/leksah"`, so `/pics`, `/cm6`, `/xterm` and `/fonts`
  are served straight out of the package directory — the same layout an
  installed build gets from `Paths_leksah.getDataDir`. Move an asset dir without
  that and the failure reads as "the UI is broken", not as a 404: the CSS and the
  editor bundles arrive through jsaddle.
- Repo-level things stay at the root: `cabal.project`, `flake.nix`, `nix/`,
  `leksah.sh`, `bin/`, `scripts/`, `hie.yaml`, `docs/`, `Leksah.app/`, and the
  sibling packages `lsp-types-client/`, `sandpit/`, `leksah-classic/`.
- Anything that names those paths needs updating together: `hie.yaml`'s cradle
  (a wrong path there makes HLS load nothing at all, silently), the `${src}/…`
  copies in `nix/macos-app.nix` and `nix/windows-installer.nix`, `${../leksah/linux}`
  in `nix/hix.nix`, and the repo-relative reads in `docs/website/try/`
  (`assemble-site.hs`, `DemoManifest.hs`).

## leksah-cmd (control socket)
- A running web-UI leksah listens on a Unix socket at `~/.leksah/cmd.sock`
  (server: `leksah/src/IDE/Web/CmdServer.hs`, started from `newIDE`). The
  `leksah-cmd` CLI (`leksah/main/Cmd.hs`, a tiny standalone exe — no leksah
  deps) drives it:
  - `leksah-cmd rebuild-self [--no-restart]` — incremental rebuild in place; with
    `--no-restart` the app stays up (build lands on disk), else it restarts on
    success. Preferred way to build while an instance is running; see above.
  - `leksah-cmd restart` — blunt relaunch (exits 2 immediately; see above).
  - `leksah-cmd editor open FILE…` — open files in the editor (alias: `cm`).
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
- **`leksah-cmd agent …` — you can put ANOTHER agent in a pane beside you**
  (`leksah/src/IDE/Web/Agent.hs`; the same thing is exposed to a session as the
  `fork_agent` MCP tool). `agent fork [--below|--tab] [--fresh] [--dir D]
  'PROMPT'` splits your own pane and starts a `claude` there, **forked from your
  conversation by default** (`--session-id` pins the child's id, so the reply is
  a handle you can use at once) — it starts knowing what you know, with no
  briefing. Unlike a Task subagent it is a real session: visible, interruptible,
  it can ask the user things, and it outlives your turn. Then `agent list`
  (id/state/pane/dir/title, yours marked), `agent read SID [--last N]` (what it
  said), `agent wait SID` (blocks — client-side polling), `agent send SID
  [--submit] TEXT` (multi-line goes through a bracketed paste), `agent show SID`,
  `agent me`. A forked child is told to report back with `agent send`, and is
  allow-listed for exactly that one command (not `agent fork` — no silent
  fan-out).
  `agent describe [SID] --title T --html H` (also the `describe_agent` MCP tool)
  is how a session says what it is doing for the **Agents pane** (side bar,
  ⌥⌘2): the tree of sessions by who forked whom, each row expandable to the
  description it wrote about itself. Its ⟳ button sends you the request; the
  HTML is sanitized on the way in (whitelist tags, `href` → `data-href`), so
  keep it to ~4 rendered lines and put real links to PRs/CI in it — the user
  opens them from there. Every leksah-launched session is allow-listed for
  `agent describe` (it writes only `~/.leksah-0.17/agents.json`). Two things to know: a **forked conversation can't change directory**
  (`--resume` only finds a session in its own project folder, so `--dir` needs
  `--fresh`), and a fork into a **directory claude hasn't seen** stops on the
  folder-trust question — `agent status` then says `starting`, and someone has to
  press a key in that pane. There are **no worktrees** here: children share the
  parent's checkout, so give them non-overlapping work or use the Tasks queue.
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
- **Status light** (top-right dot) **and the macOS menu-bar item** both show what
  the **live Claude sessions** are doing — red triangle = one is blocked on an
  approval prompt, amber diamond = one is working, green circle = all idle, grey
  ring = none running (distinct shape *and* colour, for colour-blind
  accessibility; same colours as the workspace tree's badges). One poll feeds
  both (`IDE.Web.ClaudeStatus`), so they can't disagree: the page pulls it per
  window on its tick, the menu-bar item is pushed. Hovering the dot lists the
  sessions; clicking the menu-bar item lists them and shows the one you pick.
  A session is named by **what it called itself** (`agent describe` →
  `agentTitles`, handed to the poll), falling back to its `/rename` name then its
  first prompt — so the dot, the menu-bar item, the AI picker and the Agents pane
  all print the same title, and describing yourself renames you everywhere.
- **Agent-coordination state** — "is it safe to touch leksah right now" — is
  still yours to set, and you should keep setting it:
  `leksah-cmd js eval 'leksahRestarting()'` around a rebuild/restart,
  `'leksahStatus("red")'` or `'leksahTestStart()'` (beep, then testing after 3s)
  while interactively testing, `'leksahTestEnd()'` / `'leksahStatus("green")'`
  when done. It no longer colours the dot (the sessions do): it is the **last
  line of the dot's hover text and the menu-bar item's menu line** — and it
  appears **only when it is NOT safe**, so green shows nothing at all (a line
  saying "safe to use" only confuses people who aren't modifying leksah). So
  also say in chat when you are about to take the UI — the absence of a warning
  is not a signal you can point at.
- **Freeze/deadlock debugging.** A wedged window (heartbeat stops in the loop
  log) is almost always one window's reflex *frame thread* blocked on an `MVar`.
  Diagnostics: `leksah-cmd threads` / `stacks [SUBSTR]` / `resync-state`, the
  `[win N] alive ideVer=…` heartbeat lines, and `js eval 'Math.random()'` to
  test which jsaddle transports are live. Full toolset + step-by-step method:
  [docs/development/debugging-web-ui-freezes.md](docs/development/debugging-web-ui-freezes.md).

## Web UI architecture (leksah/src/IDE/Web, the leksah library)
- Front ends share code: `leksah-warp` (browser at http://127.0.0.1:3367/),
  `leksah-wkwebview` (macOS), `leksah-webkitgtk`.
- **Do NOT set the WKWebView `uiDelegate`** — jsaddle-wkwebview uses it for its
  synchronous JS↔Haskell bridge. Overriding it silently breaks jsaddle, so the
  DOM/CSS never build (toolbar renders vertical, app unusable).
- CSS is generated with Clay and injected via `mainWidgetWithCss` **through
  jsaddle** — anything that breaks jsaddle also kills all styling.
- jsaddle-wkwebview dispatches events **asynchronously**, so `preventDefault` from
  a Haskell handler is unreliable; gate/handle in JS where it must be synchronous.
- Native code lives in `leksah/main/` (not the shared lib), so native↔reflex
  comms go through process-global `Chan`/`IORef` bridge modules in
  `leksah/src/IDE/Web/`
  (`CloseRequest`, `SaveRequest`, `FindRequest`, `OpenFileRequest`, `OpenPanel`,
  `RecentFiles`): drop a token, drain it from a background thread into an Event.
- To add front-end JS, prefer an **inline string eval’d in `Main.hs`** (see
  `revealCheckJs`, `terminalLinksJs`, `focusFindJs`) over editing the CodeMirror
  bundle: `leksah/cm6/leksah-cm6.js` is the built artifact;
  `leksah/cm6/src/leksah-cm6.mjs` is source and needs the bundler to rebuild.
- xterm.js terminals: the SearchAddon needs `allowProposedApi: true` on the
  Terminal, or its highlight decorations throw.

## Nix / haskell.nix
- **The dev loop (leksah.sh) no longer uses nix at all** — the flake remains
  for packaging/CI and standalone nix builds only.  Running leksah.sh *inside*
  `nix develop` (as a toolchain provider) works, with one trap:
- **`nix develop .#` evaluates the WORKING TREE's `cabal.project`**
  (haskell.nix shellFor → plan-to-nix parses it inside a derivation), so any
  cabal.project state nix can't reproduce — above all **absolute local
  `packages:` paths** (a development checkout of a dependency) — fails shell
  ENTRY itself with "The package location '…' does not exist", before
  leksah.sh runs.  Workaround while iterating on a local dep: evaluate the
  shell from the clean HEAD commit instead —
  `nix develop "git+file://$PWD?rev=$(git rev-parse HEAD)"` —
  the shell only provides the toolchain; the in-shell cabal then solves the
  real working-tree project (local packages build from source).
- The flake reads the **dirty working tree** (uncommitted edits ARE picked up;
  `git add` doesn’t change what nix sees — only file contents do).
- Uses haskell.nix `builderVersion = 2` (per-component "slice" builds). Each
  component exposes a `.checkAgainstPlan` derivation: build it and read
  `$out/diff-report.txt` to see why a slice’s UnitIds diverge from plan-nix.
- haskell.nix dev checkout: `/Users/hamish/iohk/haskell.nix`. Test builder changes
  with `--override-input haskellNix path:/Users/hamish/iohk/haskell.nix` — note
  this is **rebuild-the-world** (changes every slice’s drv hash).
