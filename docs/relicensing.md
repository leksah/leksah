# Relicensing the new leksah to Apache-2.0

The new (web-UI) leksah is being relicensed from GPL-2.0-or-later to
**Apache-2.0**.  Relicensing needs the consent of every copyright holder, so
the approach is: split the classic Gtk IDE into `leksah-classic/` (which stays
GPLv2, with its own copies of the shared code), then **remove from the
main package every file whose git history has authors other than Hamish
Mackenzie**, replacing each with modern, less Haskell-specific code — or
getting sign-off from its authors where a rewrite would be wasteful.

The `.cabal` `license:` field flips to `Apache-2.0` only when the
"remaining" table below is empty.  **It is now empty — see the residue note
there — and `leksah.cabal` says `Apache-2.0`.**  Hamish-authored files gain
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

### Deleted outright in Stage 2 (no soft-delete flag — classic keeps its
### copies, and git history preserves everything)

| file | note |
|---|---|
| `src/IDE/Metainfo/Provider.hs` | metadata subsystem removed |
| `src/IDE/Utils/ServerConnection.hs` | metadata subsystem removed |
| `src/IDE/SourceCandy.hs` | feature dropped (fonts provide ligatures) |
| `src/IDE/Debug.hs` | feature dropped (ghci terminal panes; DAP later) |
| `src/IDE/PackageFlags.hs` | `.lkshf` feature dropped |
| `src/IDE/TextEditor/Yi/Config.hs` | yi support dropped from the new leksah |
| `src/IDE/Utils/CabalUtils.hs` | git-history Hamish-only but header credits the old authors; its one 10-line utility was re-expressed fresh in Package.hs |
| `src/IDE/Web/Widget/Metadata.hs` | (Hamish-authored) metadata UI, dies with the feature |

### Replaced by new code, stage by stage

| file | other authors (commits) | replaced in |
|---|---|---|
| `src/IDE/Preferences.hs` | Jacco Krijnen (29), Juergen (30), Stephan Fortelny (4), others | **done** — step B (2026-08-07): fresh `IDE.Settings` (flat `Prefs`, sectioned `~/.config/leksah/settings.json`, only non-default keys written); the `Prefs`/`PrefsFile`/`EditorStyle` blocks left `Core/Types` with it; 21 dead GTK-era fields (incl. the 10 unread highlight colours, vado, `.lkshp`) dropped |
| `src/IDE/LogRef.hs` | Juergen (13), Stephan Fortelny (4), others | **done** — step D (2026-08-07): fresh `IDE.Diagnostics` (GHC/stack/nix state machine re-expressed from the output formats; the cargo parser and remote-root span handling carried over from Hamish's recent work).  Dropped with it: elm support, doctest-failure parsing, ghci breakpoint/context sinks and the error-navigation half (the web UI navigates its own `allLogRefs`) |
| `src/IDE/Package.hs` | Juergen (45), JP Moresmau (8), Stephan Fortelny (7), others | **done** — step E (2026-08-07): split into fresh `IDE.Project.{Commands,Nix,Run,Build}`.  Hamish-authored recent code (nix env cache, `withToolCommand`, ffcabal/repl/component-terminal runners, remote snapshot enumeration) carried per-hunk against `git blame`; the Juergen-era parts (package-list scraping, `idePackageFromGPD`, the run/test/clean command shapes) re-expressed fresh |
| `src/IDE/Workspaces.hs` | Juergen (26), Stephan Fortelny (12), Jacco Krijnen (6), others | **done** — step E: mutations (activate/open/remove/settings) now in `IDE.Project.WorkspaceFile`, taking explicit arguments instead of the reader-monad tower; the uncalled entry points (workspaceClean/Make, projectNewHere, projectAddPackage', constructAndOpenMainModules) dropped |
| `src/IDE/Workspaces/Writer.hs` | Stephan Fortelny (1 — the 2011 module split; content originally Juergen's) | **done** — step E: the JSON codec, deferred remote resolution and fsnotify watchers (all Hamish's, 434/478 lines) carried into `IDE.Project.WorkspaceFile`; the few foreign lines re-expressed.  The `.lkshw` format is unchanged (v4), so existing workspaces keep loading |
| `src/IDE/Build.hs` | Juergen (6), Sanny Sanoff (1), JP Moresmau (1) | **done** — step E: the make-chain machinery (foreign in design) replaced by a flat sequential engine in `IDE.Project.Build`.  Fixes two real bugs: a single-package project resolved to an empty chain and silently did nothing, and the target list asked for a `lib:<pkg>` that no longer exists |
| `src/IDE/Core/State.hs` | Juergen (39), Jacco Krijnen (4) | step F (barrel module deleted) |
| `src/IDE/Core/Types.hs` | Juergen (44), Jacco Krijnen (15), Stephan Fortelny (5), others | step F (new `IDE.Core`) |

### Vendored GPL code compiled into the main package (Stage 2 drops)

All done (step C, 2026-08-07).  What actually happened:

| source | used surface | outcome |
|---|---|---|
| `ltk` `Control.Event` (Juergen + Hamish) | event bus (`registerEvent`, `EventSelector`) | **deleted, not replaced** — an audit found exactly one live listener (`QuitToRestart`); it became the tiny `IDE.Web.RestartRequest` bridge module and every other fire was dead classic-era code |
| `leksah-server` `IDE.Core.CTypes` | `SrcSpan`/`Location` + package identifiers | fresh `IDE.Core.Location` (identifier helpers included; retires with the step-F core rewrite) |
| `leksah-server` `IDE.Utils.FileUtils` / `Tool` / `Utils` | file + process utilities | fresh `IDE.Utils.Files` / `IDE.Utils.Process` (batch-only — the interactive-ghci half of `Tool` died with `IDE.Debug`; automatic doctest running and its plan-scoped `PackageDBs` machinery were dropped rather than rewritten) |
| `leksah-server` `IDE.Utils.Project` | `ProjectKey` | **moved verbatim** — git history is Hamish-only, so the module was copied into the main package unchanged (`Show`/aeson encodings untouched); still to be superseded by the Stage-6 model |
| `leksah-server` `IDE.Utils.CabalProject` / `GHCUtils` / `VersionUtils` | small cabal helpers | `cabalProjectBuildDir`/`findCabalProjectRoot` re-expressed fresh in `IDE.Utils.Files` (`CabalProject`'s header credits Juergen despite Hamish-only commits, so treated as foreign); `GHCUtils` promoted from the Hamish-authored ghcjs stub; `VersionUtils` had no remaining users |
| `leksah-server` `IDE.Utils.CabalPlan` (Herbert Valerio Riedel) | one function, metadata-only | died with metadata (step A) |
| `vcswrapper` `VCSWrapper.Common` types | `VCSConf` in `.lkshw` | field dropped (web UI uses `IDE.Git` directly) |

Both vendor submodules (`vendor/ltk`, `vendor/leksah-server`) are deleted;
the packages remain as source-repository-packages for `leksah-classic` only.

### False positives

- `src/IDE/Web/Widget/LwView.hs` — newly added, no git history yet
  (Hamish-only).

## Remaining GPL-encumbered files in the main package

This is the gate for the license flip; delete rows as stages land.

**The list is empty.**  Step F (2026-08-07) finished the two core files:

| file | what happened |
|---|---|
| `src/IDE/Core/Types.hs` | 1192 → 405 lines.  The models it used to declare moved to their own modules — `Prefs` to `IDE.Settings` (step B), the project model to `IDE.Project` (step E), the compiler-message types to a fresh `IDE.Diagnostics.Model`, the web UI's tab\/window\/split types to a fresh `IDE.Web.Model` — and what remains (the `IDE` record, the monad stack, the derived getters) was rewritten with fresh structure and documentation.  `SearchHint`, `KeymapI` and `SearchMode` were deleted: nothing referenced them |
| `src/IDE/Core/State.hs` | Rewritten framing: a module header describing what it actually is (the state accessors, the cross-thread reflect\/reify pair, failure handling, the message helpers), section headings in the export list, and documentation on everything that lacked it |

### The residue, and why it is not a barrier

`git blame` still attributes some lines in these two files to earlier
authors.  Every one of them is in this list:

- **blank lines** and the `module … (` \/ `where` lines;
- **export-list entries** — a bare name per line (`,   readIDE`);
- **one import line** (`import Distribution.Package`);
- **one type synonym**, `type IDEM = ReaderT IDERef IO`, whose spelling is
  dictated by the types it names.

There is no expressive content left from another author: no function body,
no data declaration, no algorithm.  A name, an import statement and a blank
line are not copyrightable expression, so the flip is not waiting on
anything — but this is the one judgement call in the whole exercise, so it is
written down here rather than left implicit.

Sign-off is therefore not needed from anyone.  Everything else was either
deleted, moved out to `leksah-classic/` (still GPLv2), or rewritten.

Deleted so far: `Metainfo/Provider.hs`, `Utils/ServerConnection.hs`,
`SourceCandy.hs`, `Debug.hs`, `PackageFlags.hs`, `TextEditor/Yi/Config.hs`,
`Utils/CabalUtils.hs`, `Web/Widget/Metadata.hs` (Stage 2, step A); the
vendored `Control.Event` + `leksah-server` modules and both vendor
submodules (step C — see the vendored-code table above).
Interim edits to the files still listed above are **severance only** (cutting
imports of deleted modules) — never feature work; they are replaced as whole
files at their step.

Sign-off from the original authors can strike any row early if a rewrite
turns out to be wasteful — record who agreed, when, and how, next to the row
it strikes.

## Round 2 (2026-08-07) — derivative remnants purged

Review of the finished stages found that several "rewritten" modules were
still built by carrying hunks and shapes out of the old GPL files.  A
per-file `git log` showing only one author is not sufficient when the file's
*content* was derived from a GPL original — so the criterion tightens from
"file history" to "provenance of the text itself".

Deleted outright (to be replaced by genuinely new designs — no compat
shims, no carried hunks; large pieces written clean-room by agents that
never saw the old code):

- `IDE/Core/Types.hs`, `IDE/Core/State.hs` — the monolithic `IDE` type
  itself is retired with them (replaced by observable cells + services,
  `docs/plans/reflex-core.md`).
- `IDE/Core/Location.hs` — old `SrcSpan` shape.
- `IDE/Diagnostics.hs`, `IDE/Diagnostics/Model.hs` — carried the LogRef
  model and parser structure.
- `IDE/Settings.hs` — flat `Prefs` mirrored the old record.
- `IDE/Project.hs` + `IDE/Project/{Build,Commands,Nix,Run,WorkspaceFile}.hs`
  — `IDEPackage` compat accessors, carried command builders, and the old
  `.lkshw` codec.

Also deleted in the same sweep:

- `IDE/Utils/ExternalTool.hs`, `IDE/Utils/DirectoryUtils.hs` — both still
  carried literal GPL headers naming Juergen (a straight round-1 miss).
- `IDE/Utils/Files.hs`, `IDE/Utils/Process.hs` — API mirrors of
  leksah-server's `FileUtils`/`Tool`.
- `IDE/Utils/Project.hs` — a verbatim move (legally clean, Hamish-only
  history) but the old `ProjectKey` design; dies with the new project model.
- the whole `src-nogtk/` shim tree (`IDE.Gtk.*`, `IDE.Pane.*`) — dual-compile
  vestiges with no importers once the old core is gone.
- the `hlint` library dependency, the `no-hlint` flag and the `Idea`
  stand-ins: nothing links ghc-lib-parser any more; linting becomes an
  external tool whose output is parsed like any other build tool's.  (ghci
  mode now flips only the objc flags.)

The `license: Apache-2.0` field stays: the suspect content is deleted in
the same commit that records this list.
