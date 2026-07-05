{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module IDE.Web.Widget.Editor
  ( editorCss
  , editorWidget
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens (view, (^..), (^.))

import qualified Data.ByteString as BS (readFile, writeFile)
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
import qualified Data.Text as T (pack, null)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Traversable (forM)

import Clay
       (solid, borderBottom, orange, red, textDecorationColor, wavy,
        textDecorationStyle, underline, textDecorationLine, blue,
        background, color, px, width, pct, height, (?), Css, Color(..))

import Language.Javascript.JSaddle
       (fun, MonadJSM, JSM, js0, js1, js2, js3, js4, jsg, jss, obj, valToNumber,
        valToText, liftJSM, JSVal)

import Reflex
       (ffilter, leftmost, attach, holdUniqDyn, foldDyn, fanMap, select,
        fmapMaybe, getPostBuild, performEvent, performEvent_, ffor, constDyn,
        switchHold, never, Dynamic, Event, fan, current, updated, holdDyn,
        newTriggerEvent, delay, gate)
import Reflex.Dom.Core
       ((=:), MonadWidget, elAttr, elAttr', dyn, _element_raw, blank)

import IDE.Core.CTypes
       (SrcSpan(..), srcSpanEndColumn, srcSpanEndLine, srcSpanStartColumn,
        srcSpanStartLine)
import IDE.Core.State
       (LogRef, logRefType, logRefSrcSpan, allLogRefs, logRefFullFilePath, IDE,
        prefs, externalEditor)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TerminalEvents(..), _OpenFile, TabKey(..),
        _ErrorsGoto, _MetadataGoto, _GrepGoto, _ChangesOpen, _ProjectFileEvents,
        _PackageFileEvents, _ProjectPackageEvents)
import IDE.Web.Widget.Menu (menu)

import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)

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
gitOriginal file = do
  let dir = takeDirectory file
      name = takeFileName file
  (treeRC, _, _) <- readProcessWithExitCode "git" ["-C", dir, "rev-parse", "--is-inside-work-tree"] ""
  case treeRC of
    ExitSuccess -> do
      (rc, out, _) <- readProcessWithExitCode "git" ["-C", dir, "show", "HEAD:./" <> name] ""
      return . Just $ case rc of
        ExitSuccess -> T.pack out
        _           -> ""
    _ -> return Nothing

-- | Push the current LogRefs to the editor as CM6 mark decorations.
updateTextMarks :: JSVal -> [LogRef] -> JSM ()
updateTextMarks editorView logRefs = do
  marks <- forM logRefs $ \logRef -> do
      let sp = logRefSrcSpan logRef
      m <- obj
      m ^. jss ("fromLine" :: Text) (srcSpanStartLine sp)
      m ^. jss ("fromCh"   :: Text) (srcSpanStartColumn sp)
      m ^. jss ("toLine"   :: Text) (srcSpanEndLine sp)
      m ^. jss ("toCh"     :: Text) (srcSpanEndColumn sp + 1)
      m ^. jss ("cls"      :: Text) (T.pack . show $ logRefType logRef)
      return m
  void $ jsg ("LeksahCM" :: Text) ^. js2 ("setMarks" :: Text) editorView marks

gotoSrcSpan :: MonadJSM m => JSVal -> SrcSpan -> m ()
gotoSrcSpan editorView srcSpan = liftJSM . void $
  jsg ("LeksahCM" :: Text) ^. js3 ("gotoPos" :: Text) editorView
      (srcSpanStartLine srcSpan) (srcSpanStartColumn srcSpan)

editorWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Event t (DMap IDEWidget Identity)
  -> Event t FilePath        -- ^ save the editor for this file (write to disk)
  -> m
    ( Event t (Map FilePath (Text, Maybe ()))  -- ^ built-in (CodeMirror) opens
    , Event t (FilePath, Int)                   -- ^ external-editor opens (file, line)
    , FilePath -> Event t () -> Dynamic t (Maybe ()) -> m (Event t ()))
editorWidget ide allEvents saveFileE = do
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
      liftIO (doesFileExist file) >>= \case
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
      -- are keyed dynamically (TerminalKey n), so collect from the raw tab map
      -- rather than a fixed Const2 key.
      terminalGotoE :: Event t SrcSpan = fmapMaybe
        (\m -> listToMaybe [ sp | (TerminalKey _, dm) <- M.toList m
                                , Just (Identity (TerminalGoto sp)) <- [DM.lookup TerminalTab dm] ])
        tabEvents
      -- Unified "go to a source span"; the span's filename is the file to open.
      -- For errors the span's filename is set to the LogRef's full path.
      gotoSpanE :: Event t SrcSpan = leftmost
        [ ffor gotoLocationE $ \lr -> (logRefSrcSpan lr) { srcSpanFilename = logRefFullFilePath lr }
        , metadataGotoE
        , grepGotoE
        , terminalGotoE ]
      fileE = leftmost [ openFileE, srcSpanFilename <$> gotoSpanE ]
      -- Every open with its line (1 for a plain file open, the span's start line
      -- for a grep/error/metadata/terminal-link goto).  Used only for external
      -- opens (vim +line); the CM path reveals the line itself via locationsD.
      fileWithLineE = leftmost
        [ (, 1) <$> openFileE
        , (\sp -> (srcSpanFilename sp, srcSpanStartLine sp)) <$> gotoSpanE ]
      -- When an external editor is configured, files open there instead of in the
      -- built-in editor.  Gate the two open streams on the (live) preference.
      extActiveB = current ((not . T.null . externalEditor . view prefs) <$> ide)
  logRefsByFileD <- fmap (M.fromListWith (<>) . map (\lr -> (logRefFullFilePath lr, [lr])) . toList) <$> holdUniqDyn (view allLogRefs <$> ide)
  locationsD <- foldDyn (<>) mempty $ (\sp -> srcSpanFilename sp =: sp) <$> gotoSpanE
  return
    ( gate (not <$> extActiveB) ((=:("wide0", Just())) <$> fileE)
    , gate extActiveB fileWithLineE
    , \file selectedE _ -> do
      (changeE, triggerChangeE) <- newTriggerEvent
      logRefsD <- holdUniqDyn $ fromMaybe [] . M.lookup file <$> logRefsByFileD
      exists <- liftIO (doesFileExist file)
      liftIO (if exists then decodeUtf8' <$> BS.readFile file else return (Right "")) >>= \case
        Left _e -> return ()
        Right contents -> mdo
          -- data-file lets inline JS map LeksahCM.activeView back to its path
          -- (for the AI ▸ Send… menu commands, which mention @file / @file#Lx-Ly).
          (editorEl, _) <- elAttr' "div"
              ("class" =: "editor" <> "data-file" =: T.pack file) blank
          postBuild <- getPostBuild
          (gutterMenuE, triggerGutterMenu) <- newTriggerEvent
          editorE <- performEvent $ ffor postBuild $ \_ -> liftJSM $
              jsg ("LeksahCM" :: Text) ^. js4 ("createEditor" :: Text)
                  (_element_raw editorEl) contents
                  (fun $ \_ _ _ -> liftIO $ triggerChangeE ())
                  (fun $ \_ _ args -> case args of
                      (xv:yv:_) -> do
                          x <- valToNumber xv
                          y <- valToNumber yv
                          liftIO $ triggerGutterMenu (round x :: Int, round y :: Int)
                      _ -> return ())
          -- Tell the editor the file's committed contents (drives the
          -- dirty-line highlighting and the diff views).
          performEvent_ $ ffor editorE $ \editorView ->
              liftIO (gitOriginal file) >>= \case
                Just orig -> liftJSM . void $ jsg ("LeksahCM" :: Text) ^. js2 ("setOriginal" :: Text) editorView orig
                Nothing   -> return ()
          performEvent_ $ ffor (attach (current $ (,) <$> locationsD <*> logRefsD) editorE) $ \((locations, logRefs), editorView) -> liftJSM $ do
              case M.lookup file locations of
                Nothing -> return ()
                Just sp -> gotoSrcSpan editorView sp
              updateTextMarks editorView logRefs
          editorD <- holdDyn Nothing $ Just <$> editorE
          -- Focus the editor when it's created and whenever its tab is selected,
          -- so opening/flipping to a file puts the cursor in it (and the find
          -- bar then targets it).  Via requestAnimationFrame so the tab's
          -- visibility is applied first — focusing a hidden element is a no-op.
          focusE <- delay 0 $ leftmost [ () <$ editorE, selectedE ]
          performEvent_ $ ffor (attach (current editorD) focusE) $ \case
            (Just editorView, ()) -> liftJSM . void $
                jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
                    (fun $ \_ _ _ -> void $ editorView ^. js0 ("focus" :: Text))
            _ -> return ()
          performEvent_ $ ffor (attach (current editorD) (updated logRefsD)) $ \case
            (Nothing, _) -> return ()
            (Just editorView, logRefs) -> liftJSM $ updateTextMarks editorView logRefs
          let gotoE = ffilter ((==file) . srcSpanFilename) gotoSpanE
          performEvent_ $ ffor (attach (current editorD) gotoE) $ \case
              (Just editorView, sp) -> gotoSrcSpan editorView sp
              _ -> return ()
          -- File ▸ Save / the Save toolbar button: write this editor's current
          -- contents to disk.  (The dirty-line highlighting is relative to git,
          -- not the saved state, so there's nothing to reset here.)
          let saveThisE = ffilter (== file) saveFileE
          performEvent_ $ ffor (attach (current editorD) saveThisE) $ \case
              (Just editorView, _) -> do
                  txt <- liftJSM $ valToText =<< jsg ("LeksahCM" :: Text) ^. js1 ("getDoc" :: Text) editorView
                  liftIO $ BS.writeFile file (encodeUtf8 txt)
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
                  jsg ("LeksahCM" :: Text) ^. js1 (diffActionJs action) editorView
              _ -> return ()
          return ()
      return changeE)
