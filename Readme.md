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

You need `ghc`, `cabal` and `tmux` on your PATH. Then:

```shell
git clone https://github.com/leksah/leksah.git
cd leksah
./leksah.sh
```

That builds and launches the native front end (WKWebView on macOS,
WebKitGTK on Linux). Cabal picks the compiler and the build directory.
Other front ends:

```shell
./leksah.sh --warp    # browser UI at http://127.0.0.1:3367/
./leksah.sh --ghci    # interpreted in a cabal multi-repl: rebuilds in
                      # seconds without relinking, at the cost of a slow
                      # first load
```

`./leksah.sh --help` prints the full usage, including `--in-tmux` (show
leksah's own output as a terminal inside leksah) and `LEKSAH_PORT` for
running a second instance alongside the first.

Leksah builds with GHC 9.6.7 through 9.14; the web UI front ends use
GHC 9.14. If you would rather not assemble the toolchain yourself, the
Nix flake provides it — but the dev loop above no longer uses Nix itself.
The flake's other outputs are for packaging: a macOS app bundle/DMG, a
Windows installer and a portable Linux build.

The classic Gtk UI is in [`leksah-classic/`](leksah-classic/), with its own
`cabal.project` and flake — build it from inside that directory.

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

[Apache-2.0](LICENSE). Copyright 2007–2026 the Leksah team.

(The classic Gtk UI in `leksah-classic/` remains under GPL-2.0.)
