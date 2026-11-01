# Getting started

## Try it first

The quickest look at Leksah needs no install at all: the
[in-browser demo](https://leksah.github.io/try/) is the real IDE compiled to
JavaScript, editing a small game project with prerecorded language-server
hovers and terminal output.

## Install and run

With `ghc`, `cabal` and `tmux` on your PATH (or inside `nix develop`,
which provides them):

```shell
git clone https://github.com/leksah/leksah.git
cd leksah
./leksah.sh
```

The first build takes a while (the front end and its dependencies);
after that builds are incremental. `leksah.sh` launches
the native front end — WKWebView on macOS, GTK4 + WebKitGTK on Linux.

Variants:

```shell
./leksah.sh --warp     # serve the UI to your browser (http://127.0.0.1:3367/)
./leksah.sh --ghci     # run it interpreted in a cabal multi-repl
```

Run `./leksah.sh --help` for the full usage.  (The classic Gtk front end is
no longer part of this project — it lives in `leksah-classic/`, which has its
own `cabal.project` and flake.)

## Opening a project

Use **Workspace ▸ Open…** to edit individual files and **Workspace ▸ Open Project…**
to add a project (its `cabal.project` or `.cabal` file) to the workspace:
the Workspace pane shows the project's packages and components, and the
file tree follows the project directory. **Workspace ▸ Add Remote Project…**
opens a project on another machine over SSH (experimental).

You often don't need to: the active project follows the pane you are in,
and opening a file — or `cd`-ing a terminal — into a project the workspace
doesn't have adds it as a *temporary* project, which goes away again when
nothing shows it. **Pin to Workspace** on its context menu keeps it. See
[features.md](features.md#the-active-project-follows-you).

From a shell, a running Leksah can be driven with `leksah-cmd`:

```shell
leksah-cmd editor open src/Main.hs    # open files in the editor
leksah-cmd project open cabal.project # add a project to the workspace
leksah-cmd project list               # the projects, active / temporary
leksah-cmd window new                 # a new window, with a terminal
leksah-cmd command terminal.newTerminal  # run any command by its id
```

`leksah-cmd help` lists the rest.

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

Preferences persist in `~/.config/leksah/settings.json` — sectioned JSON
(`editor` / `font` / `theme` / `ui` / `build` / `terminal` / `lsp` / `ai`),
and only the keys you changed from the defaults are written, so the file
stays a readable list of your customisations.

## Keyboard shortcuts

**Edit ▸ Keyboard Shortcuts…** (mod+/) shows every binding.  Rebind keys in
`~/.config/leksah/keybindings.json`, VS Code style — rules append to the
defaults, a later rule wins its chord, and a leading `-` on the command
removes a default binding:

```json
[ { "key": "cmd+shift+m", "command": "package.build" },
  { "key": "ctrl+shift+b", "command": "-package.build" },
  { "key": "cmd+8", "command": "nav.split", "args": 8 } ]
```

Saving the file applies it immediately (menus, the shortcut sheet and the
key handler all follow), as does **Edit ▸ Reload Keybindings**.  Command ids
are listed in the Shortcuts pane's source registry (`IDE.Web.Commands`), and
`leksah-cmd command ID` runs one from a shell the way its menu item would.

A few to start with: **⌘T** a new terminal tab, **⌘N** a new window (with a
terminal), **⌘D** / **⌘⇧D** split the current pane, **ctrl+tab** flip
between recent tabs, **⌥⌘1–3** the side panes (Workspace, Agents,
Terminals), **⌃⌘1–5** the bottom panes (Errors, Log, Grep, Changes,
Manager).
