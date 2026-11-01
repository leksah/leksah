# Features

## Front ends

The UI is one reflex-dom program with interchangeable shells:

| Executable | What it is |
|---|---|
| `leksah` | Native app: WKWebView (macOS), GTK4 + WebKitGTK (Linux), Edge WebView2 (Windows) — one exe, the shell is chosen per-OS at build time |
| `leksah-warp` | Serves the same UI over HTTP to any browser |
| `leksah-classic` | The original Gtk front end — GPLv2, built from its own project in `leksah-classic/` |
| (GHC JS backend) | The same UI compiled to JavaScript — the [in-browser demo](https://leksah.github.io/try/) |

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

**Inline images** work in terminal panes in all three protocols: SIXEL, the
iTerm2 protocol (`imgcat`) and the **kitty graphics protocol** (`chafa -f
kitty`, `timg -pk`, …). Because a program inside tmux has to send images
through tmux's DCS passthrough — which tmux hands to Leksah verbatim — Leksah
unwraps that itself; nothing needs configuring. It also serves the kitty
transmission media a browser engine cannot read on its own — `t=s` (POSIX
shared memory) and `t=f`/`t=t` (a file, deleted afterwards for `t=t`) — which
is what lets something streaming frames, like an RDP viewer, skip base64-ing
every frame through the pty. Those are read on Leksah's own host, so a pane on
a remote machine has to send its pixels inline.

Two consequences of tmux not being able to see inside a passthrough: tmux
reserves no rows for the image (a program should send kitty's `C=1` and move
the cursor itself), and anything that repaints a pane from tmux's own screen
(a resize, a re-attach) drops the images.

**Terminal ▸ New Terminal** (⌘T) opens a terminal tab in the front window,
starting in the directory of whatever the active pane shows — a terminal's
working directory, an editor's folder — else the active project's. ⌘D / ⌘⇧D
split the current pane instead, and **Workspace ▸ New Window** (⌘N) opens a
new window with a terminal in it.

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

## The active project follows you

The active project — the one builds, the status bar and the Errors pane are
about — follows the pane you are working in. Focus an editor, a git log or a
terminal and its project becomes active; `cd` in a terminal and it follows
the shell there (within a couple of seconds). Each project remembers the
package and component you last picked in it.

A path in a project the workspace doesn't have yet opens that project as a
**temporary** one: Leksah walks up to the git checkout's root and takes the
outermost `cabal.project` / `stack.yaml` / `Cargo.toml` / `flake.nix` /
`Makefile` it finds (or the checkout itself, as a folder). Temporary projects
are shown in *italics*, are never saved to the `.leksah-workspace` file, and
leave again once no tab or pane shows anything in them. **Pin to Workspace**
on the project's context menu (or opening it with **Open Project…**) makes it
a regular project.

```shell
leksah-cmd project list          # root, type, active / temporary
leksah-cmd project pin .         # keep the project this path is in
leksah-cmd project remove PATH   # drop it from the workspace
```

## Remote projects over SSH *(experimental)*

**Workspace ▸ Add Remote Project…** opens a project that lives on another
machine (`ssh://host/path`). Editing, building and the language server all
run on the remote host; the UI stays local.

## AI assistant *(experimental)*

Run your AI tool (Claude Code) in a terminal pane, and the **AI** menu sends
context to it: the current selection (cmd+ctrl+s), a `file#L10-20` reference
(cmd+ctrl+r), the current error (cmd+ctrl+e), or a screenshot region
(cmd+ctrl+g). cmd+ctrl+j just goes to the session.

**Every pane has a default AI session**, so those commands aim somewhere
sensible without being configured: a pane running a session targets itself, a
file's pane targets its project's most recent session, and a pane a session
opened for you targets that session. Each command shows the live sessions with
that default on top — tap the chord and let go to use it, or hold the modifier
and press again (or use the arrow keys) to pick another, which then becomes
that pane's default. A session you pick that has since exited is resumed.

**Agents have names and faces.** Every Claude Code session Leksah sees is
given an animal name — Wren, Otter, Heron… — and a matching face, used
everywhere it appears: its tab, its tmux session, the tab flipper (per pane,
so a window holding two agents shows both), the status light, the menu-bar
item and the spoken terminal bell. An agent keeps its name when Claude Code
continues its conversation under a new session id (backgrounding one, for
example).

**Agents can start agents.** A session running here can put a second one in a
pane beside it with `leksah-cmd agent fork 'do this'` (or its `fork_agent` IDE
tool) — by default a *fork* of its own conversation, so the new agent starts
already knowing what the first one knew; `--as ROLE` starts a fresh one in a
role defined by a Claude Code subagent file (`.claude/agents/*.md`), and
`agent find QUERY` asks which existing agent fits a job. Unlike an in-process
subagent it is a real session: you can watch it, talk to it and approve its
tools, and it outlives the turn that made it. The two then talk over
`leksah-cmd agent send/read/wait/list`, and a forked agent reports back with
`agent done --summary …` when it finishes.

**The Agents pane** (side bar, ⌥⌘2) shows a card per agent — as a tree by
who started whom, or as a list — ordered by what needs you first: ▲ waiting
on you, ◆ working, ● idle, ✓ done, ❚ suspended, ○ exited. Each
card says what the agent is doing right now (the tool it is running, the
approval it is waiting for, an API error its last turn ended on), the files
it edited, the branch and pull request it is on, and the description it
wrote for itself, with links to its PRs, CI builds and issues. **⟳** asks it
to refresh that; **＋ New agent** and a card's **Branch off** start another.
Clicking a card brings the agent's pane to the front, or resumes it where it
left off. Leksah learns all this from Claude Code's hooks, which it installs
for the sessions it launches — the same hooks warn an agent that is about to
edit a file another live agent edited recently.

**The Team pane** (AI ▸ Team) shows a bio card for every agent; **the Manager
pane** (bottom bar, ⌃⌘5) is the team's manager agent — one per workspace,
briefed to coordinate the others — as a chat.

**Suspend and resume.** `leksah-cmd agent suspend NAME` ends an idle agent's
process on purpose but keeps it on the team (shown as *suspended*, for as
long as it stays so); `agent resume NAME` brings it back in its own
conversation, with the notes it was launched with. An agent that simply
exited stays listed, dimmed, for a day, and can be resumed the same way.

## Multiple windows

**Workspace ▸ New Window** (⌘N) opens additional OS windows onto the same IDE
session. A new window starts with a terminal (in the active project) and
with the side and bottom panes set the way the window it came from has
them. Tabs can live in any window, and the tab flipper (ctrl+tab) switches
between recently used tabs across all of them.
