{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module IDE.Web.Widget.Editor
  ( editorCss
  , editorWidget
  , ensureMonacoLoaded
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void, when, unless)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.HostFlags (getBrowserHosted)
import IDE.Web.RemoteRefresh (RefreshReason(..), requestRemoteRefresh)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens (view, (^..), (^.))
import System.Log.Logger (errorM)

-- File access goes through the IDE.Web.FS seam (real FS natively; the
-- in-memory demo tree in the browser build).
import IDE.Web.FS (fsReadFile, fsWriteFile, fsDoesFileExist)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DM (lookup)
import Data.Foldable (toList)
import Data.Functor.Misc (Const2(..))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as M
       (lookup, fromListWith, toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack, null, unlines)
import qualified Data.Text.IO as TIO (readFile)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Traversable (forM)

import Clay
       (solid, borderBottom, orange, red, textDecorationColor, wavy,
        textDecorationStyle, underline, textDecorationLine, blue,
        background, color, px, width, pct, height, (?), Css, Color(..))

import Language.Javascript.JSaddle
       (fun, MonadJSM, JSM, js0, js1, js2, js3, js4, jsg, jss, obj, valToNumber,
        valToText, valToBool, eval, liftJSM, JSVal)

import Reflex
       (ffilter, leftmost, attach, holdUniqDyn, foldDyn, fanMap, select,
        fmapMaybe, getPostBuild, performEvent, performEvent_, ffor, constDyn,
        switchHold, never, Dynamic, Event, fan, current, updated, holdDyn,
        newTriggerEvent, delay, gate, sample)
import Reflex.Dom.Core
       ((=:), MonadWidget, elAttr, elAttr', dyn, _element_raw, blank)

import IDE.Core.Location
       (SrcSpan(..), srcSpanEndColumn, srcSpanEndLine, srcSpanStartColumn,
        srcSpanStartLine)
import IDE.Core.State
       (LogRef, logRefType, logRefSrcSpan, allLogRefs, logRefFullFilePath, IDE,
        prefs, externalEditor, monacoEditor, getDataDir)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TerminalEvents(..), _OpenFile, TabKey(..),
        _ErrorsGoto, _MetadataGoto, _GrepGoto, _ChangesOpen, _ProjectFileEvents,
        _PackageFileEvents, _ProjectPackageEvents)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.Grep (GrepResult(..))
import qualified IDE.LSP as LSP

import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import IDE.Git (runGitBatch)

editorCss :: Css
editorCss = do
    ".editor" ? do
        height (pct 100)
        width (pct 100)
    -- CodeMirror 6 injects most of its own styles (incl. the oneDark theme).
    ".cm-editor" ?
        height (pct 100)
    -- The side-by-side MergeView ships no height of its own (and forces its
    -- inner editors to height:auto), so it must be told to fill the pane.
    ".cm-mergeView" ?
        height (pct 100)
    -- Modified-code highlighting (vs the git original): a subtle line tint and
    -- a more noticeable mark on the line-number gutter.
    ".cm-dirty-line" ?
        background (Rgba 226 192 141 0.08)
    ".cm-dirty-gutter" ? do
        background (Rgba 226 192 141 0.22)
        color (Rgba 226 192 141 1.0)
    -- The Monaco backend's twins of the dirty/find styling (the wavy LogRef
    -- underline classes below are shared as-is via inlineClassName).
    ".editor .monaco-editor" ?
        height (pct 100)
    ".monaco-dirty-line" ?
        background (Rgba 226 192 141 0.08)
    ".monaco-dirty-gutter" ? do
        background (Rgba 226 192 141 0.22)
        width (px 3)
    ".monaco-leksah-find" ?
        background (Rgba 255 200 0 0.35)
    ".monaco-leksah-find-active" ?
        background (Rgba 255 140 0 0.6)
    -- LogRef decorations (error/warning/lint underlines), applied as CM6 mark
    -- decorations with these classes.
    ".ErrorRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor red
        borderBottom (px 1) solid red
    ".WarningRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor orange
        borderBottom (px 1) solid orange
    ".LintRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor blue
        borderBottom (px 1) solid blue

-- | Whether new editors should use the Monaco backend (the in-browser demo is
-- always CodeMirror — no datadir to load the bundle from).
useMonacoPref :: IDE -> Bool
#if defined(ghcjs_HOST_OS)
useMonacoPref _ = False
#else
useMonacoPref = monacoEditor . view prefs
#endif

-- | Load the Monaco bundle once (lazily, on the first Monaco editor):
-- the worker source is published to @window.leksahMonacoWorkerSrc@ first
-- (the bundle's @MonacoEnvironment.getWorker@ builds Blob-URL workers from
-- it — origin-safe under wkwebview, where the page isn't served from the
-- warp origin), then the extracted CSS is injected and the bundle eval'd.
-- Running lazily also keeps the injected <style> clear of the
-- mainWidgetWithCss <head> rebuild at startup.
ensureMonacoLoaded :: JSM ()
#if defined(ghcjs_HOST_OS)
ensureMonacoLoaded = return ()
#else
ensureMonacoLoaded = do
    loaded <- valToBool =<< eval ("!!window.LeksahMonaco" :: Text)
    unless loaded $ do
        dataDir <- getDataDir
        worker <- liftIO . TIO.readFile $ dataDir </> "monaco/leksah-monaco-worker.js"
        css    <- liftIO . TIO.readFile $ dataDir </> "monaco/leksah-monaco.css"
        mainJs <- liftIO . TIO.readFile $ dataDir </> "monaco/leksah-monaco.js"
        w <- jsg ("window" :: Text)
        _ <- w ^. jss ("leksahMonacoWorkerSrc" :: Text) worker
        _ <- w ^. jss ("leksahMonacoCss" :: Text) css
        _ <- eval (("(function(){var s=document.createElement('style');"
                 <> "s.id='leksah-monaco-css';s.textContent=window.leksahMonacoCss;"
                 <> "document.head.appendChild(s);delete window.leksahMonacoCss})()") :: Text)
        void $ eval mainJs
        -- Native front end: free ⌘D for the Terminal menu's Split Right key
        -- equivalent (the convert-to-tmux-pane gesture).  Monaco binds ⌘D to
        -- "Add Selection To Next Find Match" and preventDefaults it inside
        -- the page, so the native menu never sees the key; move that action
        -- to ⌥⌘D instead.  Browser-hosted (warp) keeps Monaco's default —
        -- there is no native menu to feed there.
        browser <- liftIO getBrowserHosted
        unless browser . void $ eval $ T.unlines
          [ "(function(){var m=window.LeksahMonaco&&window.LeksahMonaco.monaco;"
          , "if(!m)return;"
          , "m.editor.addKeybindingRule({keybinding:m.KeyMod.CtrlCmd|m.KeyCode.KeyD,command:null});"
          , "m.editor.addKeybindingRule({keybinding:m.KeyMod.CtrlCmd|m.KeyMod.Alt|m.KeyCode.KeyD,"
          , "  command:'editor.action.addSelectionToNextFindMatch'});})()" ]
#endif

-- Which view of the original to show from the gutter context menu.
data DiffAction = SideBySide | Inline | HideDiff

diffActionJs :: DiffAction -> Text
diffActionJs SideBySide = "showSideBySide"
diffActionJs Inline     = "showInline"
diffActionJs HideDiff   = "hideDiff"

-- | The committed (HEAD) contents of a file for dirty-diffing:
--   * `Nothing`  — not in a git repo (don't highlight anything)
--   * `Just ""`  — in a repo but untracked/new (every line is new)
--   * `Just txt` — tracked: the HEAD version
gitOriginal :: FilePath -> IO (Maybe Text)
#if defined(ghcjs_HOST_OS)
-- Browser demo: no processes (and no git repo) — skip dirty-line
-- highlighting rather than dying on the createProcess IOException.
gitOriginal _ = return Nothing
#else
gitOriginal file = do
  let dir = takeDirectory file
      name = takeFileName file
  -- One batch = one ssh round trip when the file is remote (IDE.Git routes).
  results <- runGitBatch dir
    [ ["rev-parse", "--is-inside-work-tree"]
    , ["show", "HEAD:./" <> T.pack name]
    ]
  return $ case results of
    [(ExitSuccess, _, _), (rc, out, _)] ->
        Just (if rc == ExitSuccess then out else "")
    _ -> Nothing
#endif

-- | Push the current LogRefs to the editor as mark decorations (apiNs is the
-- backend's JS namespace: LeksahCM or LeksahMonaco).
updateTextMarks :: Text -> JSVal -> [LogRef] -> JSM ()
updateTextMarks apiNs editorView logRefs = do
  marks <- forM logRefs $ \logRef -> do
      let sp = logRefSrcSpan logRef
      m <- obj
      m ^. jss ("fromLine" :: Text) (srcSpanStartLine sp)
      m ^. jss ("fromCh"   :: Text) (srcSpanStartColumn sp)
      m ^. jss ("toLine"   :: Text) (srcSpanEndLine sp)
      m ^. jss ("toCh"     :: Text) (srcSpanEndColumn sp + 1)
      m ^. jss ("cls"      :: Text) (T.pack . show $ logRefType logRef)
      return m
  void $ jsg apiNs ^. js2 ("setMarks" :: Text) editorView marks

gotoSrcSpan :: MonadJSM m => Text -> JSVal -> SrcSpan -> m ()
gotoSrcSpan apiNs editorView srcSpan = liftJSM . void $
  jsg apiNs ^. js3 ("gotoPos" :: Text) editorView
      (srcSpanStartLine srcSpan) (srcSpanStartColumn srcSpan)

editorWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Event t (DMap IDEWidget Identity)
  -> Event t FilePath        -- ^ save the editor for this file (write to disk)
  -> m
    ( Event t (Map FilePath (Text, Maybe ()))  -- ^ built-in (CodeMirror) opens
    , Event t (FilePath, Int)                   -- ^ every open with its line (ungated;
                                                --   Main gates external-editor opens
                                                --   and drives backing shell panes)
    , Event t [GrepResult]                      -- ^ LSP find-references results (→ Grep pane)
    , Event t FilePath                          -- ^ a requested save SETTLED for this
                                                --   file (written, or failed and
                                                --   reported) — what save-then-act
                                                --   sequencing waits on
    , FilePath -> Event t () -> Dynamic t Bool -> m (Event t ()))
      -- ^ 'makeEditor' file focusPulseE focusOnCreateD: build one editor.
      --   @focusPulseE@ = \"take keyboard focus now\" (a tab's select pulse,
      --   or a view leaf's reconciler pulse); @focusOnCreateD@ gates the
      --   grab-focus-when-created behaviour — 'True' for tabs (opening a
      --   file focuses it), and \"is this the leksah window's focused leaf\"
      --   for view leaves, so a restored background leaf can't steal the
      --   keyboard at build time.
editorWidget ide allEvents saveFileE = do
  -- LSP navigation bridges (fired from an editor's F12/Shift-F12 handler on the
  -- LSP client thread): go-to-definition feeds the unified 'gotoSpanE' below;
  -- find-references is returned for the Grep pane.
  (defGotoE, fireDefGoto) <- newTriggerEvent
  (refsE, fireRefs)       <- newTriggerEvent
  -- Save-settled pulses from every editor instance (see the saveThisE
  -- handler in 'makeEditor').
  (savedE, fireSaved)     <- newTriggerEvent
  let tabEvents = select (fan allEvents) TabWidget
      workspaceEvents = select (fan (select (fanMap tabEvents) (Const2 WorkspaceKey))) WorkspaceTab
      -- Files opened from the Changes pane (existence is checked below).
      changesOpenE :: Event t FilePath = fmapMaybe listToMaybe $ (^.. _ChangesOpen) <$>
        select (fan (select (fanMap tabEvents) (Const2 ChangesKey))) ChangesTab
      openFileRequestsE = leftmost
        [ fmapMaybe (listToMaybe . (^.. traverse . _ProjectFileEvents . traverse . _OpenFile)) workspaceEvents
        , fmapMaybe (listToMaybe . (^.. traverse . _ProjectPackageEvents . traverse . _PackageFileEvents . traverse . _OpenFile)) workspaceEvents
        , (False,) <$> changesOpenE
        ]
  openFileE :: Event t FilePath <- fmapMaybe id <$> performEvent (ffor openFileRequestsE $ \case
    (True, file) -> return $ Just file
    (_, file) ->
      liftIO (fsDoesFileExist file) >>= \case
        False -> return Nothing
        True -> return $ Just file)
  let gotoLocationE :: Event t LogRef = fmapMaybe listToMaybe $ (^.. _ErrorsGoto) <$>
        select (fan (select (fanMap tabEvents) (Const2 ErrorsKey))) ErrorsTab
      -- Navigation from the metadata tree (file + span).
      metadataGotoE :: Event t SrcSpan = fmapMaybe listToMaybe $ (^.. _MetadataGoto) <$>
        select (fan (select (fanMap tabEvents) (Const2 MetadataKey))) MetadataTab
      -- Navigation from a Grep result (file + line).
      grepGotoE :: Event t SrcSpan = fmapMaybe listToMaybe $ (^.. _GrepGoto) <$>
        select (fan (select (fanMap tabEvents) (Const2 GrepKey))) GrepTab
      -- Ctrl+click on a project-file path in any terminal's output.  Terminals
      -- are keyed dynamically (LeksahWinKey / remote TerminalKey), so collect
      -- from the raw tab map rather than a fixed Const2 key.
      terminalGotoE :: Event t SrcSpan = fmapMaybe
        (\m -> listToMaybe [ sp | (_, dm) <- M.toList m
                                , Just (Identity (TerminalGoto sp)) <- [DM.lookup TerminalTab dm] ])
        tabEvents
      -- Unified "go to a source span"; the span's filename is the file to open.
      -- For errors the span's filename is set to the LogRef's full path.
      gotoSpanE :: Event t SrcSpan = leftmost
        [ ffor gotoLocationE $ \lr -> (logRefSrcSpan lr) { srcSpanFilename = logRefFullFilePath lr }
        , metadataGotoE
        , grepGotoE
        , terminalGotoE
        , defGotoE ]        -- LSP go-to-definition
      fileE = leftmost [ openFileE, srcSpanFilename <$> gotoSpanE ]
      -- Every open with its line (1 for a plain file open, the span's start line
      -- for a grep/error/metadata/terminal-link goto).  Used only for external
      -- opens (vim +line); the CM path reveals the line itself via locationsD.
      fileWithLineE = leftmost
        [ (, 1) <$> openFileE
        , (\sp -> (srcSpanFilename sp, srcSpanStartLine sp)) <$> gotoSpanE ]
      -- When an external editor is configured, files open there instead of in the
      -- built-in editor.  Gate the built-in stream on the (live) preference;
      -- the (file, line) stream is returned UNGATED — Main re-derives the
      -- external-editor opens with its own gate, and also uses every open to
      -- keep the file's backing tmux shell pane in step (see ensureShellPane).
      extActiveB = current ((not . T.null . externalEditor . view prefs) <$> ide)
  logRefsByFileD <- fmap (M.fromListWith (<>) . map (\lr -> (logRefFullFilePath lr, [lr])) . toList) <$> holdUniqDyn (view allLogRefs <$> ide)
  locationsD <- foldDyn (<>) mempty $ (\sp -> srcSpanFilename sp =: sp) <$> gotoSpanE
  return
    ( gate (not <$> extActiveB) ((=:("wide0", Just())) <$> fileE)
    , fileWithLineE
    , refsE
    , savedE
    , \file selectedE focusOnCreateD -> do
      (changeE, triggerChangeE) <- newTriggerEvent
      -- Editor backend, decided when the tab is created (like the terminals'
      -- control-mode pref): existing tabs keep their editor until reopened.
      useMonaco <- useMonacoPref <$> sample (current ide)
      let apiNs :: Text
          apiNs = if useMonaco then "LeksahMonaco" else "LeksahCM"
      -- LSP hover: the CM6 hover source calls back with (reqId, line, ch); the
      -- reply from the language server is delivered here (reqId, maybe text)
      -- from the LSP client thread via this trigger, then resolved into the
      -- editor's JS Promise in this window's own context.
      (hoverRespE, fireHoverResp) <- newTriggerEvent
      -- LSP completion: same round-trip as hover — the CM6 completion source
      -- calls back with (reqId, line, ch); the server's reply (reqId, JSON
      -- items) arrives here via this trigger and is resolved into the editor's
      -- pending Promise in this window's own context.
      (compRespE, fireCompResp) <- newTriggerEvent
      logRefsD <- holdUniqDyn $ fromMaybe [] . M.lookup file <$> logRefsByFileD
      exists <- liftIO (fsDoesFileExist file)
      liftIO (if exists then decodeUtf8' <$> fsReadFile file else return (Right "")) >>= \case
        Left _e -> return ()
        Right contents -> mdo
          -- data-file lets inline JS map LeksahCM.activeView back to its path
          -- (for the AI ▸ Send… menu commands, which mention @file / @file#Lx-Ly).
          (editorEl, _) <- elAttr' "div"
              ("class" =: "editor" <> "data-file" =: T.pack file) blank
          postBuild <- getPostBuild
          (gutterMenuE, triggerGutterMenu) <- newTriggerEvent
          editorE <- performEvent $ ffor postBuild $ \_ -> liftJSM $ do
              when useMonaco ensureMonacoLoaded
              jsg apiNs ^. js4 ("createEditor" :: Text)
                  (_element_raw editorEl) contents
                  (fun $ \_ _ _ -> liftIO $ triggerChangeE ())
                  (fun $ \_ _ args -> case args of
                      (xv:yv:_) -> do
                          x <- valToNumber xv
                          y <- valToNumber yv
                          liftIO $ triggerGutterMenu (round x :: Int, round y :: Int)
                      _ -> return ())
          -- Tell the editor the file's committed contents (drives the
          -- dirty-line highlighting and the diff views).  Fetched off the
          -- frame thread — for a remote file it's an ssh round trip.
          (origE, fireOrig) <- newTriggerEvent
          performEvent_ $ ffor editorE $ \editorView ->
              liftIO . void . forkIO $ gitOriginal file >>= \case
                Just orig -> fireOrig (editorView, orig)
                Nothing   -> return ()
          performEvent_ $ ffor origE $ \(editorView, orig) ->
              liftJSM . void $ jsg apiNs ^. js2 ("setOriginal" :: Text) editorView orig
          performEvent_ $ ffor (attach (current $ (,) <$> locationsD <*> logRefsD) editorE) $ \((locations, logRefs), editorView) -> liftJSM $ do
              case M.lookup file locations of
                Nothing -> return ()
                Just sp -> gotoSrcSpan apiNs editorView sp
              updateTextMarks apiNs editorView logRefs
          editorD <- holdDyn Nothing $ Just <$> editorE
          -- LSP (Stage 1): mirror this document to the language server — open
          -- it when the editor is created, and send full-text changes as it is
          -- edited.  Diagnostics come back asynchronously as LogRefs.
          performEvent_ $ ffor editorE $ \_ ->
              liftIO $ LSP.documentOpened file contents
          performEvent_ $ ffor (attach (current editorD) changeE) $ \case
              (Just editorView, ()) -> do
                  txt <- liftJSM $ valToText =<< jsg apiNs ^. js1 ("getDoc" :: Text) editorView
                  liftIO $ LSP.documentChanged file txt
              _ -> return ()
          -- LSP (Stage 2): register the hover callback so the CM6 hover source
          -- asks the language server; resolve the JS Promise when it replies.
          performEvent_ $ ffor editorE $ \editorView -> liftJSM . void $
              jsg apiNs ^. js2 ("setHoverHandler" :: Text) editorView
                  (fun $ \_ _ args -> case args of
                      (idv:lnv:chv:_) -> do
                          rid <- valToNumber idv
                          ln  <- valToNumber lnv
                          ch  <- valToNumber chv
                          liftIO $ LSP.requestHover file (round ln) (round ch) $ \mtext ->
                              fireHoverResp (round rid :: Int, mtext)
                      _ -> return ())
          performEvent_ $ ffor hoverRespE $ \(rid, mtext) -> liftJSM . void $
              jsg apiNs ^. js2 ("resolveHover" :: Text) rid (fromMaybe "" mtext)
          -- LSP (Stage 3): register the completion callback so the CM6
          -- completion source asks the language server; resolve the JS Promise
          -- (a JSON items array) when it replies.
          performEvent_ $ ffor editorE $ \editorView -> liftJSM . void $
              jsg apiNs ^. js2 ("setCompletionHandler" :: Text) editorView
                  (fun $ \_ _ args -> case args of
                      (idv:lnv:chv:_) -> do
                          rid <- valToNumber idv
                          ln  <- valToNumber lnv
                          ch  <- valToNumber chv
                          liftIO $ LSP.requestCompletion file (round ln) (round ch) $ \items ->
                              fireCompResp (round rid :: Int, items)
                      _ -> return ())
          performEvent_ $ ffor compRespE $ \(rid, items) -> liftJSM . void $
              jsg apiNs ^. js2 ("resolveComplete" :: Text) rid items
          -- LSP (Stage 4): F12 go-to-definition (jumps via the unified goto,
          -- opening the target file if needed) and Shift-F12 find-references
          -- (populates the Grep pane).  Both are fire-and-forget from JS.
          performEvent_ $ ffor editorE $ \editorView -> liftJSM . void $
              jsg apiNs ^. js3 ("setNavHandlers" :: Text) editorView
                  (fun $ \_ _ args -> case args of
                      (lnv:chv:_) -> do
                          ln <- valToNumber lnv
                          ch <- valToNumber chv
                          liftIO $ LSP.requestDefinition file (round ln) (round ch) $
                              maybe (return ()) fireDefGoto
                      _ -> return ())
                  (fun $ \_ _ args -> case args of
                      (lnv:chv:_) -> do
                          ln <- valToNumber lnv
                          ch <- valToNumber chv
                          liftIO $ LSP.requestReferences file (round ln) (round ch) $ \rs ->
                              fireRefs [ GrepResult f l c | (f, l, c) <- rs ]
                      _ -> return ())
          -- Focus the editor on its focus pulse (a tab select / a view leaf's
          -- reconciler pulse), and when it's created — the latter gated by
          -- focusOnCreateD so only the pane that SHOULD own the keyboard
          -- grabs it at build time (a restored background view leaf must
          -- not).  Via requestAnimationFrame so the tab's visibility is
          -- applied first — focusing a hidden element is a no-op.
          focusE <- delay 0 $ leftmost
              [ gate (current focusOnCreateD) (() <$ editorE), selectedE ]
          performEvent_ $ ffor (attach (current editorD) focusE) $ \case
            (Just editorView, ()) -> liftJSM . void $
                jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
                    (fun $ \_ _ _ -> void $ editorView ^. js0 ("focus" :: Text))
            _ -> return ()
          performEvent_ $ ffor (attach (current editorD) (updated logRefsD)) $ \case
            (Nothing, _) -> return ()
            (Just editorView, logRefs) -> liftJSM $ updateTextMarks apiNs editorView logRefs
          let gotoE = ffilter ((==file) . srcSpanFilename) gotoSpanE
          performEvent_ $ ffor (attach (current editorD) gotoE) $ \case
              (Just editorView, sp) -> gotoSrcSpan apiNs editorView sp
              _ -> return ()
          -- File ▸ Save / the Save toolbar button: write this editor's current
          -- contents to disk.  (The dirty-line highlighting is relative to git,
          -- not the saved state, so there's nothing to reset here.)
          let saveThisE = ffilter (== file) saveFileE
          performEvent_ $ ffor (attach (current editorD) saveThisE) $ \case
              (Just editorView, _) -> do
                  txt <- liftJSM $ valToText =<< jsg apiNs ^. js1 ("getDoc" :: Text) editorView
                  -- A failed write (a remote host down, a permissions error)
                  -- must be reported, not silently dropped — and must not
                  -- kill this window's frame thread.
                  liftIO $ try (fsWriteFile file (encodeUtf8 txt)) >>= \case
                      Left (e :: SomeException) ->
                          errorM "leksah" ("Failed to save " <> file <> ": " <> show e)
                      Right () -> do
                          LSP.documentSaved file txt
                          -- Remote panes refresh on events, not timers.
                          when (isRemotePath file) $
                              requestRemoteRefresh (RefreshSaved file)
                  -- The write SETTLED (either way): everything sequenced on
                  -- this save — a waiting 'requestSaveActiveFileWait', a
                  -- dirty-editor close/convert — may proceed now.
                  liftIO $ fireSaved file
              _ -> return ()
          -- Gutter context menu (rendered in Reflex; the chosen action calls
          -- the CM6 diff toggles in the bundle).
          menuPosD <- holdDyn Nothing $ leftmost [ Just <$> gutterMenuE, Nothing <$ menuActionE ]
          menuActionE <- switchHold never =<< dyn (ffor menuPosD $ \case
              Nothing -> return never
              Just (x, y) ->
                elAttr "div" ("class" =: "context-menu"
                    <> "style" =: T.pack ("position:fixed;left:" <> show x <> "px;top:" <> show y <> "px")) $
                  menu [ constDyn ("Show original side by side", SideBySide)
                       , constDyn ("Show original inline", Inline)
                       , constDyn ("Hide original", HideDiff) ])
          performEvent_ $ ffor (attach (current editorD) menuActionE) $ \case
              (Just editorView, action) -> liftJSM . void $
                  jsg apiNs ^. js1 (diffActionJs action) editorView
              _ -> return ()
          return ()
      return changeE)
