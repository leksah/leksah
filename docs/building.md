# Building from source

## The short version

```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
./leksah.sh --nix ghc914
```

`leksah.sh` is the build-and-run driver:

```
./leksah.sh [--nix] [--warp|--classic] GHCVER [--in-tmux] [LEKSAH_ARGS]
```

* `--nix` re-enters the Nix dev shell (`nix develop ".?submodules=1#GHCVER"`)
  for every build/run command. Without it, commands run in your ambient
  environment — you need `ghc`, `cabal`, `tmux` and `leksah-server` on
  PATH already.
* Default front end is the native `exe:leksah` (WKWebView on macOS,
  WebKitGTK on Linux); `--warp` builds/runs the browser front end;
  `--classic` the Gtk one.
* `GHCVER`: `ghc914` for the web front ends (the default); the classic
  front end also builds with older compilers (e.g. `ghc98`). The libraries
  support GHC 9.6.7 through 9.14.
* `--in-tmux` runs leksah inside a tmux session so its own output shows up
  as "Terminal 0" in the Terminals pane.
* `LEKSAH_PORT=N ./leksah.sh …` runs a second instance side by side.

Builds use the `dist-ghc-<version>` build directory (e.g.
`dist-ghc-9.14.1`) — the same one Leksah's own in-IDE builds use, so the
two share incremental state.

## Nix notes

* The flake reads the **dirty working tree**: uncommitted edits are picked
  up, but **new files must be `git add`ed** before Nix can see them.
* Flake attributes need submodules: `nix build '.?submodules=1#…'`.
* Packaging outputs: `leksah-macos-app` / `leksah-macos-dmg` (macOS),
  `leksah-windows-installer` (WebView2), and `leksah-linux` /
  `leksah-windows` run apps for cross-built binaries.

## Developing Leksah in Leksah

Leksah can rebuild itself in place. With an instance running, the
`leksah-cmd` CLI talks to it over `~/.leksah/cmd.sock`:

```shell
leksah-cmd rebuild-self               # incremental rebuild via leksah's own
                                      # build system, restart on success
leksah-cmd rebuild-self --no-restart  # build only; relaunch later with
leksah-cmd restart --no-rebuild
leksah-cmd rebuild-self --use-cabal   # failsafe: bypass leksah's build code
                                      # and stream plain cabal output
```

Other useful commands: `leksah-cmd editor open FILE…`, `leksah-cmd project
open FILE…`, `leksah-cmd js eval 'CODE'` (evaluate JavaScript in the live
UI), `leksah-cmd screenshot FILE.png`, `leksah-cmd ping` /
`wait-ready`.

Run exactly **one** instance per control socket: each instance rebinds
`~/.leksah/cmd.sock`, so extra instances end up orphaned. Use
`LEKSAH_PORT` if you genuinely need two.

## ghci mode (`--ghci`): reload instead of rebuild

On macOS the native front end can run *interpreted* in a cabal multi-repl:

```shell
./leksah.sh --nix --ghci ghc914
```

This starts `cabal repl leksah:exe:leksah leksah:lib:leksah-nogtk
--enable-multi-repl` in a tmux pane (session `ghci` on leksah's own tmux
server) and types `:main`. The first load compiles everything to bytecode —
slow, once per session. After that:

- `leksah-cmd rebuild-self` (and `leksah-cmd restart`) become **`:reload` +
  `:main` at the repl prompt** — seconds instead of a full relink and
  relaunch. On compile errors the output is shown and the prompt is left
  waiting; fix and run it again.
- `leksah-cmd hs eval 'CODE'` (also `-f FILE` / `-`) evaluates Haskell in the
  live process: the Cocoa run loop is suspended (the UI freezes), the code
  runs at the ghci prompt with every leksah module loaded, and the UI
  resumes immediately after. `js eval`'s Haskell sibling.

How it works (and the aarch64-darwin / `ghc914-sh` static-toolchain hurdles
this had to clear — ghci RTS-links every dependency archive):

- **Objective-C.** `foreign export` is illegal in interpreted code, so the ObjC
  glue and its callbacks live in the compiled `leksah-mac-glue` sublibrary. The
  RTS linker, though, never registers ObjC classes with the runtime (only dyld
  does), so ghci mode builds the glue (and jsaddle-wkwebview's) with ObjC kept
  *out* of the archives (`-objc-in-library` off) and preloads it as dylibs
  (`~/.leksah/ghci-native/*.dylib`, `-L/-l` in the repl) — dyld loads them and
  registers the classes. ObjC→Haskell calls go through FunPtr-registered
  callbacks (a jsaddle-wkwebview patch), not extern foreign-export symbols.
- **`no-hlint` flag.** hlint drags in ghc-lib-parser, whose ~125 MB static
  archive the RTS linker can't relocate (`SUBTRACTOR` out of range). The native
  web UI only carries hlint's `Idea` *type*, so ghci mode drops the dep
  (`--constraint="leksah +no-hlint"`) and uses the same `Idea` stand-in the JS
  backend uses.
- **Main thread.** Cocoa must build its window on OS thread 0.
  `-fno-ghci-sandbox` puts repl evaluation on the bound REPL thread — but cabal
  `--enable-multi-repl` doesn't forward `--repl-options`, so leksah.sh types
  `:set -fno-ghci-sandbox` at the prompt before `:main`. Stopping is
  `[NSApp stop:]`, not `exit` — `IDE.Web.GhciMode` gates every exit site.
- **Reload brings the UI back.** A second `:main` can't reuse jsaddle's
  one-shot app-launch path, so on reload window 0 is recreated through leksah's
  runtime new-window path; and `stopForGhci` first kills the previous run's
  network threads (`reflex-frames-*`, `bridge-drain-*`, …) so they don't
  deadlock the reloaded window on the shared Cocoa main-queue bridge.
- **Linker lottery.** Even with ghc-lib-parser gone, whether a large archive
  (e.g. reflex) relocates depends on the run's ASLR layout, so the initial load
  crashes intermittently; leksah.sh just retries the load (a fresh process
  re-rolls the layout).

`leksah-cmd hs eval` / `rebuild-self` read the ghci output from the pane's
*rendered* scrollback (the session runs with `TERM=dumb` so haskeline emits no
cursor escapes to glue lines together).

Escape hatches: `tmux -L leksah attach -t ghci` for the raw prompt;
`tmux -L leksah kill-session -t ghci` to end the session; a plain
`./leksah.sh --nix ghc914` still runs the compiled binary.

## Editor bundles

The editor front ends are prebuilt JavaScript bundles checked into the
repo: `cm6/` (CodeMirror 6) and `monaco/` (Monaco). Each has a
`package.json` with a `build` script (esbuild); rebuild them only when
changing their sources (`cm6/src/leksah-cm6.mjs`,
`monaco/src/leksah-monaco.mjs`) and commit the built artifacts.

## More developer docs

* [Debugging web-UI freezes](development/debugging-web-ui-freezes.md)
* [tmux control-mode terminals](tmux-control-mode.md)
* [The website & in-browser demo pipeline](website/) —
  `docs/website/try/copy-assets.sh` builds the demo with the GHC
  JavaScript backend; `docs/website/record-demo.md` documents the
  screencast pipeline.
