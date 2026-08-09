# Contributing to Leksah

This document is meant to get you started hacking on the Leksah code base.

Questions and ideas are welcome on the
[issue tracker](https://github.com/leksah/leksah/issues) and in
[GitHub discussions](https://github.com/leksah/leksah/discussions).

## Getting started

1. Click "Fork" at the top of the leksah repository page
2. Clone your fork locally (with submodules):

   ```
   git clone <url-to-your-fork>
   ```

3. Make sure you can build and run Leksah from source — see
   [docs/building.md](docs/building.md). The short version:

   ```
   ./leksah.sh --nix ghc914
   ```

## Making a contribution

1. Have a look at the [issue tracker](https://github.com/leksah/leksah/issues)
   or think of something new
2. Create a branch, hack, commit, push to your fork, open a pull request

The best development environment for Leksah is Leksah: open
`cabal.project` in it and use `leksah-cmd rebuild-self` to rebuild the
running instance with your changes in place
(see [docs/building.md](docs/building.md#developing-leksah-in-leksah)).

## Repository layout

| Where | What |
|---|---|
| `leksah/` | The `leksah` package: everything the IDE itself is built from, and its datadir |
| `leksah/src/IDE/` | The `leksah` library — core state, workspaces, packages, builds |
| `leksah/src/IDE/Web/` | The reflex-dom web UI: `Main.hs` builds the whole DOM, widgets live in `Widget/` (Editor, Terminals, Workspace, FileTree, Grep, Changes, GitLog, Preferences, …) |
| `leksah/src/IDE/Web/MenuModel.hs` | The menu/keyboard-shortcut model shared by all front ends |
| `leksah/src/IDE/LSP.hs` | Language Server Protocol client glue (per-language server table) |
| `leksah/main/` | The native shells: `wkwebview/` / `webkitgtk/` / `webview2/` / `ghcjs/` `Main.hs` (per-OS `exe:leksah`), `Warp.hs` (`exe:leksah-warp`), `Cmd.hs` (`exe:leksah-cmd`) |
| `leksah/cm6/` | CodeMirror 6 editor bundle (`src/leksah-cm6.mjs` → built `leksah-cm6.js`, checked in) |
| `leksah/monaco/` | Monaco editor bundle, same pattern |
| `leksah/pics/`, `leksah/xterm/`, `leksah/fonts/` | The rest of the runtime datadir, served over HTTP by the front ends |
| `leksah-server` | Background metadata/build server (separate repo, pulled in by the flake) |
| `docs/website/` | leksah.org sources, including the in-browser demo (`try/`) |

The classic Gtk front end is no longer part of this project.  It lives in
`leksah-classic/` as a frozen GPLv2 fork with its own `cabal.project` and flake
(build it from that directory); nothing here depends on it.

## Architecture notes (web UI)

* The UI is **one reflex-dom program**. Native shells embed it through
  [jsaddle](https://github.com/ghcjs/jsaddle) (WKWebView / WebKitGTK /
  WebView2), `leksah-warp` serves it over a websocket, and the GHC
  JavaScript backend compiles it for the in-browser demo. UI code must
  work in all of them — avoid platform assumptions in `src/IDE/Web`.
* Native-side code lives in `main/` and can't call into reflex directly;
  communication goes through small process-global bridge modules in
  `src/IDE/Web/` (drop a token in a `Chan`/`IORef`, drain it into an
  `Event`).
* jsaddle dispatches DOM events **asynchronously** — anything that needs a
  synchronous `preventDefault` has to be handled in JavaScript (see the
  inline JS snippets in `src/IDE/Web/Main.hs`).
* Shared application state is the `IDE` record
  (`src/IDE/Core/Types.hs`), operated on in the `IDEM` monad
  (`readIDE` / `modifyIDE_` / `reflectIDE`); the web UI observes it as a
  reflex `Dynamic`.
* Editor backends are prebuilt JS bundles exposing the same API surface
  (`window.LeksahCM` / `window.LeksahMonaco`), driven from
  `src/IDE/Web/Widget/Editor.hs`. Rebuild a bundle only when you change
  its `.mjs` source, and commit the built artifact.
* Debugging a frozen or misbehaving UI:
  [docs/development/debugging-web-ui-freezes.md](docs/development/debugging-web-ui-freezes.md).
  Terminals: [docs/tmux-control-mode.md](docs/tmux-control-mode.md).

## Useful tools while hacking

A running Leksah listens on `~/.leksah/cmd.sock`:

```shell
leksah-cmd rebuild-self          # rebuild the running instance in place
leksah-cmd js eval 'CODE'        # poke the live DOM from a shell
leksah-cmd screenshot out.png    # capture the UI
leksah-cmd editor open FILE      # open a file in the editor (alias: cm)
```

## FAQ

### Can I use AI to contribute?

Yes. AI has been used very extensively to revive this project — without it,
Leksah would likely still be dormant. We will endeavour to judge pull requests
on their merits and not on how they were created, and AI assistance will likely
be used to help us assess them too.
