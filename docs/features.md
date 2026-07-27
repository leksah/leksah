# Features

## Front ends

The UI is one reflex-dom program with interchangeable shells:

| Executable | What it is |
|---|---|
| `leksah` | Native app: WKWebView (macOS), GTK4 + WebKitGTK (Linux), Edge WebView2 (Windows) — one exe, the shell is chosen per-OS at build time |
| `leksah-warp` | Serves the same UI over HTTP to any browser |
| `leksah-classic` | The original Gtk front end |
| (GHC JS backend) | The same UI compiled to JavaScript — the [in-browser demo](https://leksah.org/try/) |

## Editors

New editor tabs use **CodeMirror 6** by default, or **Monaco** (the editor
component from VS Code) when the *Use Monaco* preference is on. Both give
you syntax highlighting (Haskell, TypeScript/JavaScript, JSON, CSS, YAML,
shell, TOML, XML/HTML and more; Monaco adds dozens of languages), find and
replace through the shared find bar, a dirty-change gutter against the last
git-committed version, and side-by-side or inline diffs against the
original from the gutter menu.

## Language servers

Leksah talks LSP to a server per project root, chosen by file extension:

| Language | Server | Project root |
|---|---|---|
| Haskell | `haskell-language-server` | `cabal.project` / `stack.yaml` / `*.cabal` |
| Nix | `nixd` | `flake.nix` / `shell.nix` / `default.nix` |
| TypeScript / JavaScript | `typescript-language-server` | `tsconfig.json` / `package.json` |
| Rust | `rust-analyzer` | `Cargo.toml` |
| Go | `gopls` | `go.work` / `go.mod` |
| Python | `pyright-langserver` | `pyproject.toml` / `setup.py` / `requirements.txt` |
| C / C++ | `clangd` | `compile_commands.json` / `.clangd` |

A server is started the first time you open one of its files, and only if
the binary is on PATH — nothing to configure. When no marker file is found
the nearest `.git` directory serves as the root.

You get: diagnostics in the Errors pane (same path as GHC build errors,
including editor squiggles), hover, completion, go-to-definition
(F12 / cmd-click) and find-references (shift-F12, results in the Grep
pane).

Per-project override for the Haskell server: put the command in a
`.leksah-lsp` file in the project root (first non-comment line). The
global preference *LSP server command* does the same for all projects, and
*Enable LSP* turns the whole feature off.

## Terminals

Terminal panes are real **tmux** sessions. With control mode on (the
default preference) Leksah attaches as a tmux control-mode client, so
terminals are persistent — restart Leksah and your shells, scrollback and
running processes are still there. Terminals are searchable (same find bar
as everything else) and file references in output (`src/Foo.hs:12`,
git-diff headers, etc.) are clickable links with hover tooltips.

## Git

* **Changes pane** — live `git status` of the workspace with staged /
  unstaged hunks and diffs.
* **History browser** — branches, commit log, and per-file side-by-side
  diffs for any commit.
* Editors show a dirty gutter against the committed version of the file.

## Haskell projects

The Workspace pane understands `cabal.project`, packages and components.
Builds run through cabal (with an optional fail-fast GHCi mode that
type-checks components in cached REPLs before building), errors land in
the Errors pane, and background build keeps it all up to date as you type.
GHCi debugging (step, continue) is available from the Debug menu. Nix
users: *Workspace ▸ Refresh Nix Environment* caches the project's dev
shell so builds don't pay Nix evaluation on every command.

## Remote projects over SSH *(experimental)*

**Workspace ▸ Add Remote Project…** opens a project that lives on another
machine (`ssh://host/path`). Editing, building and the language server all
run on the remote host; the UI stays local.

## AI assistant *(experimental)*

Run your AI tool (e.g. Claude Code) in a terminal pane and mark it as the
AI target. The **AI** menu then sends context to it: the current selection
(cmd+ctrl+s), a `file#L10-20` reference (cmd+ctrl+r), the current error
(cmd+ctrl+e), or a screenshot region (cmd+ctrl+g).

## Multiple windows

**Workspace ▸ New Window** opens additional OS windows onto the same IDE
session; tabs can live in any window and the tab flipper (ctrl+tab)
switches between recently used tabs.
