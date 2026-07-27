<!-- # Changelog -->

## Unreleased — the web UI era

Leksah's UI was rewritten as a single
[reflex-dom](https://reflex-frp.org/) program, replacing the GTK+ /
GtkSourceView interface:

* **Front ends sharing one codebase**: native `leksah` (WKWebView on
  macOS, GTK4 + WebKitGTK on Linux, Edge WebView2 on Windows),
  `leksah-warp` (any browser over HTTP), and an in-browser demo compiled
  with GHC's JavaScript backend
  ([leksah.org/try](https://leksah.org/try/)). The Gtk front end lives on
  as `leksah-classic`.
* **Editors**: CodeMirror 6 by default, Monaco (VS Code's editor
  component) as a preference; shared find bar, dirty gutter, in-editor
  diffs.
* **Language Server Protocol support**: haskell-language-server plus
  automatic use of `typescript-language-server`, `rust-analyzer`, `gopls`,
  `pyright`, `clangd` and `nixd` when on PATH; diagnostics, hover,
  completion, go-to-definition, references.
* **tmux terminals**: terminal panes are persistent tmux control-mode
  sessions with search and clickable file references.
* **Git integration**: live changes pane and a history browser with
  side-by-side diffs.
* **Multiple OS windows**, a tab flipper, and a command socket
  (`leksah-cmd`) for driving a running instance — including
  `rebuild-self`, which Leksah uses to rebuild itself from within itself.
* **Remote projects over SSH** *(experimental)*: edit, build and run the
  language server on another machine.
* **AI assistant menu** *(experimental)*: send selections, file
  references, errors or screen regions to an AI tool in a terminal pane.
* **Nix flake build** with dev shells for GHC 9.6.7–9.14 and packaging
  outputs for macOS (app/DMG), Windows (installer) and Linux.

Release notes for the GTK-era versions (up to 0.8, 2010) are preserved in
[docs/old_manual/intro.rst](docs/old_manual/intro.rst).
