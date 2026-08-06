# Reflex-core migration recipe

Companion to `reflex-core.md`.  That file says what the new core is; this
one says exactly how the 38 broken modules move onto it.  Ported code must
be *new plumbing on existing widget logic* — the widget bodies are ours and
stay; only state access, action dispatch and the deleted models change.

## What replaces what

### The state seam

Old: every widget takes `Dynamic t IDE`, fed by MVar truth (`IDERef`) +
per-window resync polls (`WindowBridge.registerResync`, `ideVersion`
guard, 5 s heartbeat tick, ack dance).

New: every widget takes `Ctx t` (defined in `IDE.Web.Ctx`):

```haskell
data Ctx t = Ctx
  { cApp      :: App                     -- services, for actions
  , cWindowId :: WindowId
  , cUi       :: Dynamic t WebUi         -- window/pane/session model
  , cWs       :: Dynamic t Ws            -- workspace model
  , cCfg      :: Dynamic t Config        -- settings
  , cProblems :: Dynamic t (Map Text [Problem])
  }
newCtx :: … => App -> WindowId -> m (Ctx t)   -- one cellDyn per cell, per window
```

`newCtx` runs once near each window's root; everything below receives the
`Ctx`.  Widget bodies keep their `holdUniqDyn (view l <$> d)` slicing —
only the source Dynamic changes.  **Delete** the resync machinery:
`registerResync`/`unregisterResync`/`resyncStates`/`resyncGlobalLock`, the
`ideVersion` field and guard, the 5 s fallback tick, the ack `delay 0`
(`resync-state` in CmdServer reports the cell world instead — keep the
socket verb, change the payload).

### The IDE record fields

| old field(s) | new home |
|---|---|
| `webWindows, leksahWindows, nextLeksahWin, hiddenWindows, activeWindow, nextWindowId, flipMirror, flipMru, paneAISession` | `WebUi` record (+lenses) in `IDE.Web.Model`; `Cell WebUi` in `App` (one cell — multi-field updates stay atomic) |
| `workspace, bufferProjCache` | `Ws` record in `IDE.Workspace` over the `IDE.Ws` model; `Cell Ws` in `App` |
| `prefs` | `IDE.Config` (`ConfigService`, done) |
| `allLogRefs, currentEBC/currentError` | `IDE.Problems` (done); current-error nav state is a `Cell (Maybe Problem)` |
| `logLineMap` | gone — `IDE.BuildLog` bytes + xterm pane |
| `runningTool, triggerBuild, autoCommand, autoURI` | `IDE.Builder` service (build queue over `IDE.Run`) |
| `nixCache` | `Cell NixCache` in `IDE.Builder`; fresh JSON sidecar load/save |
| `fsnotify, watchers, externalModified` | `IDE.Watch` service |
| `jsContexts` | `IDE.Windows` service: register/unregister + `appJSM` (the `ideJSM` successor: run a JSM in every live context, async) |
| `exitCode, currentState, developLeksah` | `IDE.App` lifecycle bits (`appExit :: IORef ExitCode`, `Cell RunState`, plain `Bool` field) |
| `_ideGtk, hlintQueue, logLaunches, recentFiles/Workspaces, currentHist` | dead — never read; do not port |

### Actions

Old: `Event t IDEAction` (`IDEM ()` over `IDERef`), `reflectIDE`,
`getGlobalIDERef`, `workspaceTry/projectTry/packageTry` wrappers.

New: `type AppAction = App -> IO ()`.  Widgets keep emitting typed events;
the fold in `Main` maps them to `AppAction`s and `performEvent_` runs
`act app`.  Native menus / request drains use `getGlobalApp`
(`IDE.App.AppStore`, replacing `IDERefStore`).  The `*Try` wrappers become
`withActiveProject/withActivePackage :: App -> (… -> IO ()) -> IO ()`
reading the `Ws` cell (no-op with a `blNote` when nothing is active).

### The old workspace model → `IDE.Ws`

| old | new |
|---|---|
| `Workspace`, `wsProjects`, `wsFile`, `wsActivePackFile` | `Ws { _wsPath, _wsSpec :: IDE.Ws.File.Workspace, _wsProjects :: Map ProjectKey Project, _wsActive :: Maybe ActiveTarget }` |
| `Project`, `pjKey`, `pjDir`, `pjFile`, `pjFileOrDir`, `pjPackages`, `pjCabalFile` | `IDE.Ws.Types.Project`; `prKey`; `pkRoot . prKey`; `pkFile . prKey`; `fromMaybe root file`; `prPackages` (list; key by `pkgManifest`) |
| `IDEPackage`, `ipdCabalFile`, `ipdPackageDir`, `ipdPackageName`, `ipdPackageId`, `ipdSrcDirs`, `ipdLib` | `Package`; `pkgManifest`; `pkgDir`; `pkgName`; `pkgName <> "-" <> pkgVersion`; `pkgSrcDirs`; `any ((== KLib) . cKind) . pkgComponents` |
| `ProjectKey` ADT (`CabalTool`…), `filePathToProjectKey`, `pjIsCabal`… | uniform `ProjectKey` record; detection via `IDE.Ws.Registry.detectProject` |
| `ProjectSettings` / `psCmdPrefix` / `wsSettingsFor` / `setProjectSettings` | `WsProject` gains `wpCmdPrefix :: Maybe Text` (extend `IDE.Ws.File`, default absent) next to `commandOverrides`; per-project lookup goes through `_wsSpec` |
| `packageIdentifierToString` | `pkgName <> "-" <> pkgVersion` |
| `installWorkspace/readWorkspace/resolveDeferredProjects`, `.lkshw` v4 | `IDE.Workspace` service ops over `IDE.Ws.File` (`.leksah.json`).  The `.lkshw` format is dead: if the session points at one, treat as "no workspace" and note it in the build log.  For self-hosting, commit a `leksah.leksah.json` for this repo. |
| `projectOpenPath/projectOpenThis/workspaceActivatePackage/workspaceRemoveProject/dirProjectKey` | same-named ops on `IDE.Workspace` (detect → add to spec → save → enumerate → update cell) |

### Diagnostics consumers

`LogRef/LogRefType/SrcSpan` → `Problem/Severity/Range` (+ `Loc` for
jump-to-source events: add `data Loc = Loc FilePath Range` to
`IDE.Problems.Types`).  Mappings: `logRefFullFilePath` → resolve `pPath`
against the producer root (the `Ws` project dir); `logRefType`
ErrorRef/WarningRef/LintRef → `pSeverity`; `activeProjectLogRefs` →
filter the problems map by source-key prefix of the active project;
Errors-pane grouping and Statusbar counts keep their logic on the new
type.  LSP publishes straight into `setProblems (lsp:<root>)` — no
conversion through a build-shaped type.  Editor gutters consume
`problemsByPath`.

### Log pane → xterm.js (delete `Widget/Log.hs` content)

New `IDE.Web.Widget.BuildLogPane`: xterm instance per window
(`disableStdin: true`, `convertEol: true`, `allowProposedApi: true`),
`blAttach` on mount (replay + follow, detach on teardown), reuse the
existing terminal font/theme plumbing and `terminalLinksJs`.  Boot
messages (settings parse error, keybinding warnings, duplicate command
ids) become `blNote` lines instead of `logLineMap` seeds; the one
`ideMessage` call (`Commands.hs:132`) becomes `blNote`.

### Small fresh helpers (write, do not port)

- `IDE.Paths`: `getDataDir` (`leksah_datadir` env override → `Paths_leksah`),
  `sidecarPath :: FilePath -> IO FilePath` (`~/.leksah-0.17/<name>`, dir
  ensured — the `getConfigFilePathForSave` successor), `isSubPath`
  (trailing-slash-safe prefix test on canonical-ish paths).
- `IDE.DebugLog`: `focusLog`/`metaLog` (env-gated stderr lines, as today).
- `IDE.Builder` nix env cache: JSON sidecar (`nix-cache.json`) read/write.

### `IDE.Builder` (the build service — new functional code)

Owns: build queue (one run per project root, dirty-flag re-run — use
`IDE.Web.Coalesce`), `Cell BuilderState` (running? which target), nix env
cache, and the **rebuild-self contract**: building the leksah package with
success-exit(2) relaunch must keep working (`CmdServer` `rebuild-self`,
`QuitToRestart` semantics — see `IDE.Web.RestartRequest`).  Commands come
from `IDE.Ws` `ptCommand` + `applyOverrides` + `wpCmdPrefix`; ghci mode
(`bcGhci`) routes cabal builds through ffcabal (`IDE.Web.ReplTmux` repls)
with the plain-cabal fallback on "matches no local component".  Output:
`blWrite` raw + a `parseLine` fold per run → `setProblems ("build:" <> root)`.
Toggles (`makeMode`, `runUnitTests`, …) are `saveConfig` updates on
`BuildC` — the old `*Toggled` functions die.

## Port order (each step compiles greener; commit per step)

1. Enablers: `IDE.Paths`, `IDE.DebugLog`, `WebUi` in `IDE.Web.Model`,
   `IDE.App`, `IDE.Workspace`, `IDE.Builder`, `IDE.Web.Ctx`, `Loc`,
   `wpCmdPrefix`.  (`IDE.Web.IDERefStore` → `IDE.App` global store.)
2. Leaf type-swaps: `Events`, `ConvertRequest`, `TerminalInput`,
   `RemoteSettingsRequest`, `SplitLayout`, `Session`, `Shortcuts`,
   `Toolbar`, `Grep`, `Browser`, `AgentInfo`, `Model` re-exports.
3. Workspace consumers: `Widget/Workspace`, `Changes`, `Statusbar`,
   `ReplTmux`, `AISession`, `LSP`, `RemoteSettings`, `AddRemote`,
   `Review`, `NewWorktree`, `ClaudeQueue`, `AddServer`.
4. Commands: `Command.hs` (`AppAction`, Builder calls, Config toggles),
   `Commands.hs`, menus (`MacMenu`, `GtkApp`, `GtkMenu`, `Win32Menu`).
5. Diagnostics consumers: `Errors`, `Editor` marks, `TerminalCC`/
   `Terminal` links, `LwView`.
6. `Widget/Preferences` → Config sections (keep the `wired` flag idea).
7. `Widget/Log` → `BuildLogPane`.
8. `Main.hs`: `newIDE` → build `App` + services; delete resync/ideVersion;
   `newCtx` per window; `CmdServer` ports (`ping`/heartbeat stays;
   `rebuild-self` first with `--use-cabal`); `WindowBridge` drains emit
   `AppAction`s.
9. `cabal build` green (loop PATH prefix), ghci relaunch, self-host E2E.

## Verification gates

- After step 8: `PATH="$(pwd)/bin:$PATH" cabal build exe:leksah exe:leksah-cmd`.
- Boot: workspace tree shows this repo (via the committed
  `leksah.leksah.json`), terminals open, Claude panes list, build of
  leksah-welcome produces xterm log output + Errors-pane problems,
  `leksah-cmd rebuild-self --use-cabal --no-restart` streams, settings
  toggle writes only its key, `leksah-cmd js eval` answers.
- The freeze rules hold: no sync JS fan-out, cell watchers never block
  (`cellDyn` only), services never wait on windows.
