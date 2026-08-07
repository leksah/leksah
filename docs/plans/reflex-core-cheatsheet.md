# Port cheat sheet (old idiom → new, exact)

Read the new APIs before porting: `src/IDE/App.hs`, `src/IDE/Workspace.hs`,
`src/IDE/Builder.hs`, `src/IDE/Web/Ctx.hs`, `src/IDE/Config.hs`,
`src/IDE/Problems{,.Types}.hs` (severity/positions), `src/IDE/BuildLog.hs`,
`src/IDE/Paths.hs`, `src/IDE/DebugLog.hs`, `src/IDE/Reactive{,.Dyn}.hs`,
`src/IDE/Ws/Types.hs`, `src/IDE/Ws/File.hs`, `src/IDE/Web/Model.hs`.

## State & widgets
- Widget param `Dynamic t IDE` → `Ctx t` (import `IDE.Web.Ctx`).  UI-model
  slices keep their lens names: `view webWindows <$> ide` becomes
  `view webWindows <$> cUi ctx` (lenses `webWindows leksahWindows
  nextLeksahWin hiddenWindows activeWindow nextWindowId flipMirror flipMru
  paneAISession` now live on `WebUi` in `IDE.Web.Model`).
- Snapshot reads: `readIDE l` → `view l <$> readCell (appUi app)` (UI
  lenses) / `readCell (wsCell (appWorkspace app))` + a `IDE.Workspace`
  view.  `modifyIDE_ (l %~ f)` → `modifyCell (appUi app) (l %~ f)`.
- `reflectIDE act ideR` + `getGlobalIDERef` → `withApp (\app -> …)`
  (`IDE.App`).  `IDEAction` → `AppAction` (= `App -> IO ()`); events that
  carried `IDEAction` now carry `AppAction`; `liftIDE`/`IDEM` are gone —
  plain `IO` with the `App` (or a service) in scope.
- `ideJSM act` → `appJSM app act`.  `ideMessage _lvl txt` →
  `appNote app txt`.  `focusLog`/`metaLog` → `IDE.DebugLog` (String).

## Settings
- `Prefs` → `Config`; read: `cCfg ctx` (Dynamic) or
  `currentConfig (appConfig app)`; write: `saveConfig (appConfig app) cfg'`.
- Field map (old → section record accessor): showLineNumbers→`ecLineNumbers`,
  rightMargin→`ecRightMargin`, tabWidth→`ecTabWidth`, wrapLines→`ecWrapLines`,
  editorChoice/monacoEditor/externalEditor→`ecEditor` (`Editor` ADT:
  `EditorCodeMirror|EditorMonaco|EditorExternal cmd`),
  forceLineEnds→`ecFixLineEnds`, removeTBlanks→`ecStripBlanks`,
  autoLoad→`ecAutoReload`, monospaceFont→`fcMonoFamily`,
  monospaceFontSize→`fcMonoSize`, monacoThemeDark/Light→`thMonacoDark/Light`,
  codeMirrorThemeDark/Light→`thCodeMirrorDark/Light`,
  xtermThemeDark/Light→`thXtermDark/Light`, uiSelectionColor→`thSelectionColor`,
  uiHoverColor→`thHoverColor`, showHiddenFiles→`uiShowHiddenFiles`,
  showIgnoredFiles→`uiShowIgnoredFiles`, showWorkspaceIcons→`uiWorkspaceIcons`,
  colorfulIcons→`uiColorfulIcons`, collapseErrors→`uiCollapseErrors`,
  saveSessionOnClose→`uiSaveSession`, showShortcutBadges→`uiShortcutBadges`,
  saveAllBeforeBuild→`bcSaveAllFirst`, hlintOnSave→`bcLintOnSave`,
  jumpToWarnings→`bcJumpToWarnings`, backgroundBuild→`bcBackground`,
  native→`bcNative`, javaScript→`bcJavaScript`, debug→`bcGhci`,
  makeDocs→`bcDocs`, runUnitTests→`bcTests`, runBenchmarks→`bcBenchmarks`,
  makeMode→`bcMakeMode`, terminalFileLinks→`tcFileLinks`,
  terminalControlMode→`tcControlMode`, tmuxInterceptPrefix→`tcTmuxPrefix`,
  remoteHosts→`rcHosts`, lspEnabled→`lcEnabled`,
  lspServerCommand→`lcServerCommand`, regionCaptureTarget→`acCaptureTarget`.
  Sections via `cfgEditor/cfgFont/cfgTheme/cfgUi/cfgBuild/cfgTerminal/
  cfgRemote/cfgLsp/cfgAi`.
- Old `*Toggled` command helpers → flip the `BuildC` field and
  `saveConfig`.

## Workspace model
- `readIDE workspace :: Maybe Workspace` → `readCell (wsCell (appWorkspace
  app)) :: Ws` (never Maybe; empty Ws = no workspace, `wsPath` is the
  Maybe).  Dynamic: `cWs ctx`.
- `wsProjects` → `IDE.Workspace.wsProjects :: Ws -> [Project]`;
  `pjKey`→`prKey`, `pjDir`→`prDir`, `pjFile`→`pkFile . prKey`,
  `pjFileOrDir`→`prFileOrDir`, `pjPackages` (Map) → `prPackages` (list;
  key by `pkgManifest`), `pjCabalFile`→for cabal projects
  `pkFile (prKey p)`.
- `IDEPackage`→`Package`: `ipdCabalFile`→`pkgManifest`,
  `ipdPackageDir`→`pkgDir`, `ipdPackageName`→`pkgName`,
  `ipdPackageId`→`packageIdText p`, `ipdSrcDirs`→`pkgSrcDirs` (RELATIVE to
  `pkgDir` now — prepend `pkgDir </>` where an absolute dir was expected),
  `ipdLib`→`any ((== KLib) . cKind) (pkgComponents p)`.
- `activeProject/activePack/activeComponent` → `activeProject/
  activePackage/activeComponent :: Ws -> Maybe …`.
- `wsSettingsFor`/`psCmdPrefix` → `wsSpecFor key ws >>= wpCmdPrefix` or
  `wsCmdPrefix key ws`; `setProjectSettings` → `setProjectCmdPrefix`.
- `projectOpenPath/projectOpenThis/workspaceActivatePackage/
  workspaceRemoveProject` → same-named ops on `appWorkspace` (see
  `IDE.Workspace`; `projectOpenThis`→`projectOpenKey`).
- `filePathToProjectKey`/`dirProjectKey` → `detectProject defaultEffects
  path` (`IDE.Ws.Registry`) or build a `ProjectKey` record directly.
- `packageIdentifierToString`→`packageIdText`.
- Build/run/etc entry points: `makePackage`/`buildActiveTarget` →
  `buildActiveTarget (appBuilder app)`; `packageClean/Test/Bench/Run` →
  `runVerb (appBuilder app) key mbPkg mbComp VClean/VTest/VBench/VRun`;
  a component's shell command for a terminal: `verbCommand ws pr mbPkg
  mbComp verb :: Maybe ToolCmd` (absolute `tcDir`).
- `projectRefreshNix` and the nix dev-env command wrapping are DROPPED
  for now (commands run in the ambient env — same as the dev loop);
  replace the action with `appNote app "nix env refresh is not
  reimplemented yet"` and keep the menu/button wiring.
- `workspaceTry f`/`projectTry`/`packageTry` → read the Ws cell; run `f`
  with the active project/package or `appNote` "no active project/package".

## Diagnostics
- `LogRef`→`Problem`; `LogRefType`→`Severity` (`ErrorRef`→`SevError`,
  `WarningRef`→`SevWarning`, `LintRef`→`SevHint`, breakpoints/context are
  gone).  `SrcSpan`→`Loc`/`Range` (`IDE.Problems.Types`, 0-BASED lines and
  columns, half-open; old SrcSpan was 1-based inclusive — convert at
  render/jump sites: display line = `posLine + 1`).
- `allLogRefs` → `cProblems ctx :: Dynamic t (Map Text [Problem])`
  (flatten: `concat . M.elems`); `activeProjectLogRefs` → filter the map
  to keys `"build:" <> root` / `"lsp:" <> root` of the active project's
  root (`prDir`).
- `logRefFullFilePath lr` → `pPath p` when absolute, else the producing
  root `</>` it (the source key's root is the resolver).
  `refDescription`→`pMessage`, `logRefSrcSpan`→`pRange`,
  `logRefFilePath`→`makeRelative root (pPath p)`.
- LSP diagnostics: publish `[Problem]` via `setProblems (appProblems app)
  ("lsp:" <> T.pack root)` — no build-shaped intermediary.

## Misc
- `getConfigFilePathForSave name` → `sidecarPath name`;
  `getConfigFilePathForLoad` → `sidecarPath` too (it returns the path;
  read side just also works). `isSubPath`/`getDataDir` → `IDE.Paths`.
- `currentState`/`IDEState` → `cRunState ctx :: Dynamic t RunState`
  (`IsStartingUp|IsRunning|IsShuttingDown`).
- `IDERef`, `IDE`, `IDEM`, `MonadIDE`, `liftIDE`, resync: gone.  A
  function that took `IDERef` now takes `App` (or nothing + `withApp`).
- Do NOT edit `leksah.cabal`, do NOT commit, do NOT run `cabal build`.
