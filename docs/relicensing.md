# Relicensing the new leksah to Apache-2.0

The new (web-UI) leksah is being relicensed from GPL-2.0-or-later to
**Apache-2.0**.  Relicensing needs the consent of every copyright holder, so
the approach is: split the classic Gtk IDE into `leksah-classic/` (a frozen
GPLv2 fork with its own copies of the shared code), then **remove from the
main package every file whose git history has authors other than Hamish
Mackenzie**, replacing each with modern, less Haskell-specific code — or
getting sign-off from its authors where a rewrite would be wasteful.

The `.cabal` `license:` field flips to `Apache-2.0` only when the
"remaining" table below is empty.  Until then the package stays
GPL-2.0-or-later.  Hamish-authored files gain
`SPDX-License-Identifier: Apache-2.0` headers as they are touched (they can
be dual-licensed by their author at any time); classic's copies keep GPL
headers.

Authorship data: per-file `git log --follow --format='%an'` over every tree
that remains in the main package (2026-08-06).  **140 files are sole-authored
by Hamish; 41 have other authors.**  All 105 `IDE.Web.*` modules, the
front-end dirs (`src-wkwebview`, `src-webkitgtk`, `src-webview2`,
`src-ghcjs*`, `src-mac-glue`, `src-nogtk`), `main/Cmd.hs` and `main/Warp.hs`
are Hamish-only.

## Author identities seen in the history

| Name(s) in git | Person |
|---|---|
| `jutaro`, `Jutaro`, `jnf`, `info`, `Jürgen (Jutaro) Nicklisch-Franken` | Juergen Nicklisch-Franken (original author) |
| `Jacco Krijnen` | Jacco Krijnen |
| `jpmoresmau` | JP Moresmau |
| `forste`, `Stephan Fortelny` | Stephan Fortelny |
| `haja`, `Harald Jagenteufel` | Harald Jagenteufel |
| `Sanny Sanoff` | Sanny Sanoff |
| others (1–8 commits each) | Mansour, Catherine Galkina, Thiago Silva, Francisco Soares, Jens Petersen, Martijn Fleuren, Asafe Ribeiro, Brian Wignall, nwuensche, achirkin, philipp.erlacher, Jonathan.Paugh, Threestrikes |
| in `leksah-server`: `Herbert Valerio Riedel` | `IDE.Utils.CabalPlan` (2016) |

(`jutaro`/`jnf`/`info` are treated as Juergen pending confirmation from the
commit emails.)

## Files with other authors — fate

### Leaving the main package with the classic split (Stage 1)

These are classic-only and `git mv` to `leksah-classic/`; no rewrite needed.

`src/IDE/BufferMode.hs`, `src/IDE/Command.hs`, `src/IDE/Command/Print.hs`,
`src/IDE/Command/VCS.hs`, `src/IDE/Command/VCS/Common.hs`,
`src/IDE/Command/VCS/Common/GUI.hs`, `src/IDE/Command/VCS/Common/Helper.hs`,
`src/IDE/Command/VCS/Common/Workspaces.hs`, `src/IDE/Command/VCS/GIT.hs`,
`src/IDE/Command/VCS/Mercurial.hs`, `src/IDE/Command/VCS/SVN.hs`,
`src/IDE/Command/VCS/Types.hs`, `src/IDE/Completion.hs`, `src/IDE/Find.hs`,
`src/IDE/GUIHistory.hs`, `src/IDE/HLint.hs`, `src/IDE/ImportTool.hs`,
`src/IDE/Keymap.hs`, `src/IDE/LPaste.hs`, `src/IDE/Leksah.hs`,
`src/IDE/NotebookFlipper.hs`, `src/IDE/PaneGroups.hs`, `src/IDE/Session.hs`,
`src/IDE/Statusbar.hs`, `src/IDE/SymbolNavigation.hs`,
`src/IDE/TextEditor/Yi/Config.hs`, `main/Main.hs`
(also all of `src-gtk/`, which was classic-only already).

### Removed with the metadata soft-delete (Stage 2)

| file | note |
|---|---|
| `src/IDE/Metainfo/Provider.hs` | excluded by the `metadata` flag; classic keeps its copy |
| `src/IDE/Utils/ServerConnection.hs` | excluded by the `metadata` flag |

### Replaced by new code, stage by stage

| file | other authors (commits) | replaced in |
|---|---|---|
| `src/IDE/Preferences.hs` | Jacco Krijnen (29), Juergen (30), Stephan Fortelny (4), others | Stage 3 (JSON prefs) |
| `src/IDE/SourceCandy.hs` | Juergen (11) | Stage 3 (feature dropped) |
| `src/IDE/LogRef.hs` | Juergen (13), Stephan Fortelny (4), others | Stage 5 (`IDE.Diagnostics`) |
| `src/IDE/Package.hs` | Juergen (45), JP Moresmau (8), Stephan Fortelny (7), others | Stage 6 (project model) |
| `src/IDE/Workspaces.hs` | Juergen (26), Stephan Fortelny (12), Jacco Krijnen (6), others | Stage 6 |
| `src/IDE/Workspaces/Writer.hs` | Stephan Fortelny (1 — the 2011 module split; content originally Juergen's) | Stage 6 |
| `src/IDE/Build.hs` | Juergen (6), Sanny Sanoff (1), JP Moresmau (1) | Stage 6 (`IDE.Project.Build`) |
| `src/IDE/PackageFlags.hs` | Juergen (3), info (1) | Stage 6 (feature dropped) |
| `src/IDE/Debug.hs` | Juergen (16), Stephan Fortelny (2), Mansour (2) | Stage 7 (feature dropped; DAP later) |
| `src/IDE/Core/State.hs` | Juergen (39), Jacco Krijnen (4) | Stage 7 (barrel module deleted) |
| `src/IDE/Core/Types.hs` | Juergen (44), Jacco Krijnen (15), Stephan Fortelny (5), others | Stages 3–7 by attrition → new `IDE.Core` |

### Vendored GPL code compiled into the main package (Stage 2 drops)

| source | used surface | replacement |
|---|---|---|
| `ltk` `Control.Event` (Juergen + Hamish) | event bus (`registerEvent`, `EventSelector`) | fresh `IDE.Core.EventBus` |
| `leksah-server` `IDE.Core.CTypes` | `SrcSpan`/`Location` + identifiers | fresh `IDE.Core.Location` + `IDE.Utils.CabalIdent`, then `IDE.Diagnostics` |
| `leksah-server` `IDE.Utils.FileUtils` / `Tool` / `Utils` | file + process utilities | fresh `IDE.Utils.Files` / `IDE.Utils.Process` |
| `leksah-server` `IDE.Utils.Project` | `ProjectKey` | fresh `IDE.Project.Key`, then the Stage-6 model |
| `leksah-server` `IDE.Utils.CabalProject` / `GHCUtils` / `VersionUtils` | small cabal helpers | fresh `IDE.Utils.CabalTool` |
| `leksah-server` `IDE.Utils.CabalPlan` (Herbert Valerio Riedel) | one function, metadata-only | dies with metadata |
| `vcswrapper` `VCSWrapper.Common` types | `VCSConf` in `.lkshw` | field dropped (web UI uses `IDE.Git` directly) |

### False positives

- `src/IDE/Web/Widget/LwView.hs` — newly added, no git history yet
  (Hamish-only).

## Remaining GPL-encumbered files in the main package

This is the gate for the license flip; delete rows as stages land.

| file | blocked on |
|---|---|
| `src/IDE/Metainfo/Provider.hs` | Stage 2 |
| `src/IDE/Utils/ServerConnection.hs` | Stage 2 |
| vendored `Control.Event` + 11 `leksah-server` modules | Stage 2 |
| `src/IDE/Preferences.hs` | Stage 3 |
| `src/IDE/SourceCandy.hs` | Stage 3 |
| `src/IDE/LogRef.hs` | Stage 5 |
| `src/IDE/Package.hs` | Stage 6 |
| `src/IDE/Workspaces.hs` | Stage 6 |
| `src/IDE/Workspaces/Writer.hs` | Stage 6 |
| `src/IDE/Build.hs` | Stage 6 |
| `src/IDE/PackageFlags.hs` | Stage 6 |
| `src/IDE/Debug.hs` | Stage 7 |
| `src/IDE/Core/State.hs` | Stage 7 |
| `src/IDE/Core/Types.hs` | Stages 3–7 (attrition) |

Sign-off from the original authors can strike any row early if a rewrite
turns out to be wasteful — record who agreed, when, and how, next to the row
it strikes.
