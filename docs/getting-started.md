# Getting started

## Try it first

The quickest look at Leksah needs no install at all: the
[in-browser demo](https://leksah.org/try/) is the real IDE compiled to
JavaScript, editing a small game project with prerecorded language-server
hovers and terminal output.

## Install and run

Install [Nix](https://nixos.org/download/), then:

```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
./leksah.sh --nix ghc914
```

The first build takes a while (it builds `leksah-server`, the front end and
their dependencies); after that builds are incremental. `leksah.sh` launches
the native front end — WKWebView on macOS, GTK4 + WebKitGTK on Linux.

Variants:

```shell
./leksah.sh --nix --warp ghc914     # serve the UI to your browser
                                    # (http://127.0.0.1:3367/)
./leksah.sh --nix --classic ghc98   # the classic Gtk front end
```

Run `./leksah.sh` with no arguments for the full usage. Without `--nix`,
commands run in your ambient environment — use that if you already have
`ghc`, `cabal`, `tmux` and `leksah-server` on PATH and want to skip the
per-command Nix evaluation.

## Opening a project

Use **File ▸ Open…** to edit individual files and **File ▸ Open Project…**
to add a project (its `cabal.project` or `.cabal` file) to the workspace:
the Workspace pane shows the project's packages and components, and the
file tree follows the project directory. **File ▸ Add Remote Project…**
opens a project on another machine over SSH (experimental).

From a shell, a running Leksah can be driven with `leksah-cmd`:

```shell
leksah-cmd cm open src/Main.hs        # open files in the editor
leksah-cmd project open cabal.project # add a project to the workspace
```

## Language servers

Open a Haskell file and, if `haskell-language-server` is on PATH (it is in
the Nix dev shell), you get diagnostics, hover, completion,
go-to-definition and find-references automatically. The same goes for other
languages when their server is installed: `typescript-language-server`,
`rust-analyzer`, `gopls`, `pyright`, `clangd` and `nixd` are recognised out
of the box. See [features.md](features.md#language-servers).

## Preferences worth knowing

Open **Preferences** from the menu. A few highlights:

* **Editor** — switch new editors to Monaco (VS Code's editor component)
  instead of CodeMirror 6; choose the monospace font and size.
* **LSP** — enable/disable language-server support, override the Haskell
  server command.
* **Terminal** — tmux control-mode terminals (recommended; keeps terminal
  state across restarts).

Preferences persist in `~/.leksah-0.17/prefs.lkshp`.
