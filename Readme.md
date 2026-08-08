# Leksah — an IDE written in Haskell

[Leksah](https://leksah.org/) is a fast, hackable development environment
written in Haskell. The whole UI is a functional-reactive program
([reflex-dom](https://reflex-frp.org/)) that runs everywhere: as a native
app on macOS, Linux and Windows, served to any browser, and even compiled
to JavaScript and run entirely in-page — no install, no server.

That last one is the easiest way to see it:

**[▶ Try Leksah in your browser](https://leksah.org/try/)** — the real IDE,
compiled with GHC's JavaScript backend, editing a small game project with
prerecorded language-server hovers and terminal output.

## What you get

* **One codebase, every platform.** A single `leksah` executable with a
  per-OS native shell — WKWebView on macOS, GTK4 + WebKitGTK on Linux,
  Edge WebView2 on Windows. `leksah-warp` serves the identical UI over HTTP
  to any browser, and the GHC JavaScript backend builds the in-browser demo
  from the same source.
* **Your choice of editor.** CodeMirror 6 by default, or Monaco — the editor
  component from VS Code — via a preference. Syntax highlighting for
  Haskell, TypeScript/JavaScript, Rust, Go, Python, C/C++, HTML/CSS, YAML,
  shell and more.
* **Language servers.** Deep integration with `haskell-language-server`
  (diagnostics in the Errors pane, hover, completion, go-to-definition,
  find references), and other servers — `typescript-language-server`,
  `rust-analyzer`, `gopls`, `pyright`, `clangd`, `nixd` — picked up
  automatically when they are on your PATH. A `.leksah-lsp` file in the
  project root overrides the Haskell server command per project.
* **Real terminals.** Terminal panes are tmux sessions (control mode):
  persistent, searchable, and still there after a restart.
* **Git built in.** A live changes pane and a history browser with
  side-by-side diffs.
* **Remote projects over SSH** *(experimental)* — open, edit and build a
  project that lives on another machine, with the language server running
  remotely too.
* **AI assistant integration** *(experimental)* — send the current file or
  a selected region to an AI tool running in a terminal pane.

Haskell development is still where Leksah is deepest — it understands
Cabal workspaces, packages and components, drives incremental builds, and
can rebuild *itself* from inside itself — but the editor, terminals, git
and LSP support work the same for any language.

## Getting started

Install [Nix](https://nixos.org/download/), then:

```shell
git clone https://github.com/leksah/leksah.git
cd leksah
./leksah.sh --nix ghc914
```

That builds and launches the native front end (WKWebView on macOS,
WebKitGTK on Linux). Other front ends:

```shell
./leksah.sh --nix --warp ghc914       # browser UI at http://127.0.0.1:3367/
./leksah.sh --nix --classic ghc98     # the classic Gtk front end
```

`./leksah.sh` with no arguments prints the full usage, including
`--in-tmux` (show leksah's own output as a terminal inside leksah) and
`LEKSAH_PORT` for running a second instance.

`--nix` re-enters the Nix dev shell for every build/run command. If you
already have `ghc`, `cabal`, `tmux` and `leksah-server` on your PATH you
can drop it and build in your ambient environment.

Leksah builds with GHC 9.6.7 through 9.14; the web UI front ends use
GHC 9.14 (the default). The Nix flake also has packaging outputs for a
macOS app bundle/DMG, a Windows installer and a portable Linux build.

## Documentation

* [Getting started](docs/getting-started.md)
* [Features](docs/features.md)
* [Building from source](docs/building.md)
* [Contributing](Contributing.md)
* [Changelog](CHANGELOG.md)

## Why Haskell?

Leksah has been written in Haskell since 2007. The current UI is a single
reflex-dom program: the same functional-reactive Haskell renders through a
native WebView via [jsaddle](https://github.com/ghcjs/jsaddle), over a
websocket to your browser, or compiled to JavaScript outright. If you want
to see a substantial, long-lived Haskell application in the wild — or hack
on one — the source is right here, and Leksah is developed in Leksah.

## License

[GPL-2.0](LICENSE). Copyright 2007–2026 the Leksah team.
