# Reflex core (round 2): cells, services, and no monolith

The round-2 purge (`docs/relicensing.md`) deleted the old core outright:
the `IDE` record, `IDERef`, `IDEM`, the `Prefs` record, the LogRef
diagnostics model, and the carried project/build modules.  This document is
the design their replacements are built to.  Nothing here is a port; where
behaviour must match (file formats, socket protocol), the *requirement* is
stated and the code is written fresh — large pieces by clean-room agents
that have never seen the old implementation.

## Principles

1. **No monolithic state.**  There is no `IDE` record and no successor.
   State lives inside *services* — one handle per subsystem, created in
   `main`, wired together explicitly.  A function takes the services it
   uses, not a bag of everything.
2. **Reflex-first.**  Widgets consume `Dynamic`s and produce `Event`s.
   The bridge from services to widgets is one primitive (`Cell`), lifted
   per reflex host with `cellDyn`.  No snapshot reads inside widget builds,
   no whole-state resync fan-out.
3. **Async boundaries.**  A service never blocks on a window and a window
   never blocks on a service.  Cell notification is fire-and-forget into
   each subscriber's own executor (for reflex, a trigger-event fire).
   This is the structural fix for the frame-thread-wedge family of bugs.
4. **Processes over libraries.**  Compilers, linters and VCS are external
   tools whose output is parsed; leksah links neither ghc-lib nor hlint.

## The primitive: `IDE.Reactive`

~100 lines, no dependencies beyond base/containers/reflex.

```haskell
data Cell a                                  -- observable state cell
newCell    :: a -> IO (Cell a)
readCell   :: MonadIO m => Cell a -> m a
writeCell  :: MonadIO m => Cell a -> a -> m ()
modifyCell :: MonadIO m => Cell a -> (a -> a) -> m ()
watchCell  :: Cell a -> (a -> IO ()) -> IO (IO ())   -- unsubscribe action
cellDyn    :: (TriggerEvent t m, MonadHold t m, MonadIO m)
           => Cell a -> m (Dynamic t a)
```

- Updates are serialized per cell; watchers see every committed value in
  order.  Watchers must be cheap and non-blocking (post to your own
  executor); `cellDyn`'s watcher is a trigger fire, which is exactly that.
- A watcher must not synchronously write its own cell (documented, not
  enforced).
- Cross-window consistency falls out for free: every window's `cellDyn`
  subscribes to the same cell, each fires through its own trigger, no
  window ever evaluates JS on another window's behalf.

## Services

Each service is an opaque record of functions + cells, created once in
`main` (or lazily), passed explicitly.  `App` is the wiring record handed
to top-level widgets — immutable, not a state bag:

```haskell
data App = App
  { appSettings :: Settings.Service
  , appWorkspace:: Workspace.Service
  , appBuild    :: Build.Service
  , appProblems :: Problems.Service
  , appBuildLog :: BuildLog.Service
  , appWindows  :: Windows.Service
  , appLsp      :: Lsp.Service
  }
```

There is deliberately no `AppM`/reader monad: new code names its
dependencies in its type.  (`IDE.Web.*` migrates module-by-module; during
migration a module may take `App` whole, but leaf functions take services.)

### Settings (`IDE.Settings2` → takes the `IDE.Settings` name when green)

Per-section records — `EditorS`, `FontS`, `ThemeS`, `UiS`, `BuildS`,
`TerminalS`, `LspS`, `AiS` — composed into `Settings`.  The field set is
derived from what the UI *reads today*, not from the old record.  One
`Cell Settings`; sections exposed as focused `Dynamic`s via `holdUniqDyn`.
File: `~/.config/leksah/settings.json`, sectioned objects, only
non-default keys written, fsnotify reload, parse errors surface in the
build log (never silent).

### BuildLog (read-only xterm.js — replaces the Log pane)

The log is a terminal, not a text widget:

- Service holds a bounded byte-history (drop oldest) + broadcast of new
  chunks.  `blWrite :: Service -> ByteString -> IO ()`, `blClear`.
- Tools run with colour forced (`-fdiagnostics-color=always`,
  `--color=always`) — the pane shows real compiler colours; the Problems
  parser strips ANSI from its tap of the same stream.
- Widget: one xterm.js instance per window showing the pane
  (`disableStdin: true`; `allowProposedApi: true` for SearchAddon;
  the existing terminalLinksJs file:line links Just Work).
- On mount: replay history, then stream.  No line model in Haskell at all.

### Problems (`IDE.Problems.*` — the diagnostics successor)

LSP-shaped model in `IDE.Problems.Types` (0-based positions):
`Pos`, `Range`, `Severity (Error|Warning|Hint|Info)`,
`Problem { pPath, pRange, pSeverity, pCode, pMessage, pTool }`.

- `IDE.Problems.Parse` (clean-room agent): incremental line parser,
  auto-detecting GHC/cabal/stack and cargo/rustc shapes, ANSI-tolerant.
  `parseLine :: St -> Text -> (St, [Problem])` + `parseEnd :: St -> [Problem]`.
- Service: `Cell (Map RootDir (Seq Problem))`, fed by Build's tap and by
  LSP publishDiagnostics (already this shape).  Errors pane, gutter marks
  and status-bar counts are `cellDyn` consumers.

### Build

- A fresh process runner (`IDE.Run`): spawn tool, stream stdout+stderr
  chunks to (a) BuildLog and (b) a per-run Problems parse fold; exit code
  completes the run.  Replaces Utils/Process + ExternalTool.  Interrupt =
  kill the process group of the current run.
- Queue semantics: one run per project root at a time; a re-request while
  running marks dirty and re-runs once (the Coalesce pattern).
- What to run comes from the project model (below).  The rebuild-self /
  `QuitToRestart` contract is a requirement on this service: build of the
  leksah package + exit(2) handshake must keep working.

### Project model (clean-room agent, `IDE.Ws.*`)

The original round-1 design, built for real this time:

```haskell
data ProjectKey  = ProjectKey { pkType :: Text, pkRoot :: FilePath
                              , pkFile :: Maybe FilePath }
data ComponentKind = KLib | KExe | KTest | KBench | KOther
data Component   = Component { cKind :: ComponentKind, cName :: Text }
data Package     = Package { pName, pVersion :: Text
                           , pManifest :: FilePath, pDir :: FilePath
                           , pComponents :: [Component], pSrcDirs :: [FilePath] }
data ProjectType = ProjectType   -- record-of-functions registry entry
  { ptId        :: Text
  , ptMarkers   :: [FilePath -> Bool]          -- does this dir/file claim it
  , ptEnumerate :: Effects -> FilePath -> IO [Package]
  , ptCommand   :: Verb -> Scope -> Maybe ToolCmd   -- build/run/test/bench/clean/repl
  }
```

Effects (readFile/listDirectory/runTool) are a parameter record so
enumeration is testable and remote-capable (ssh:// roots via the existing
RemotePath seam).  Instances: cabal (cabal.project/`*.cabal`), stack,
cargo (`cargo metadata` JSON), nix flake, make, plain dir.  Workspace
file: fresh `<name>.leksah.json` — version, projects (type/root/file,
per-project command overrides), active target; paths relative to the file.

### Windows

Owns the live jsaddle contexts: per-window "run JS/DOM action" executors
(the `ideJSM` successor), window enumeration, the native bridges
(menu, status item).  Fan-out is per-window and async, per the
no-cross-window-broadcast rule.

## Migration map (old spine → new)

| old (deleted) | new |
|---|---|
| `IDE` record + `IDERef` + `IDEM` | services + `App` wiring |
| `readIDE`/`modifyIDE_` + resync | `Cell` / `cellDyn` |
| `Prefs` (flat) | `Settings` (sectioned records) |
| Log pane line model (`logLineMap`) | BuildLog bytes → xterm.js |
| `LogRef`/`SrcSpan`/parsers | `IDE.Problems.*` |
| `IDEPackage`/`ProjectKey` ADT/Build.hs | `IDE.Ws.*` ProjectType registry |
| Utils/Process + ExternalTool | `IDE.Run` streaming runner |

## Order of work

1. `IDE.Reactive` + `IDE.Problems.Types` (hand-written, tiny).  ← here
2. Clean-room agents: `IDE.Problems.Parse`; `IDE.Ws` enumeration.
3. `IDE.Run` + BuildLog service + xterm pane.
4. Settings v2 + Windows service; `main` builds `App`.
5. Migrate `IDE.Web.*` importers module-by-module (agents, with recipe);
   delete each compat crutch as its last importer moves.
6. Green `cabal build`, full ghci relaunch, self-host E2E.
