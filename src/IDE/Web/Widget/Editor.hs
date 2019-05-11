{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.Editor
  ( editorCss
  , editorWidget
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens (view, (^..), (.~), (^.))

import qualified Data.ByteString as BS (readFile)
import Data.Default (Default(..))
import Data.Dependent.Map (DMap)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Misc (Const2(..))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as M
       (lookup, fromListWith)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Traversable (forM)

import Clay
       (solid, borderBottom, orange, red, textDecorationColor, wavy,
        textDecorationStyle, underline, textDecorationLine, blue,
        fontFamily, px, fontSize, width, pct, height, (?), Css)

import Language.Javascript.JSaddle
       (fun, MonadJSM, JSM, call, eval, js1, js0, js2, jsg, jss, obj,
        liftJSM, JSVal)

import Reflex
       (ffilter, leftmost, attach, holdUniqDyn, foldDyn, fanMap, select,
        fmapMaybe, delay, getPostBuild, performEvent, performEvent_, ffor,
        Dynamic, Event, never, fan, current, updated, holdDyn, newTriggerEvent)
import Reflex.Dom.Core
       ((=:), textAreaConfig_initialValue, textArea, MonadWidget,
        _textArea_element)

import IDE.Core.CTypes
       (SrcSpan(..), srcSpanEndColumn, srcSpanEndLine, srcSpanStartColumn,
        srcSpanStartLine)
import IDE.Core.State
       (LogRef, logRefType, logRefSrcSpan, allLogRefs, logRefFullFilePath, IDE)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), _OpenFile, TabKey(..), _ErrorsGoto,
        _ProjectFileEvents, _PackageFileEvents, _ProjectPackageEvents)
import System.Directory (doesFileExist)

editorCss :: Css
editorCss = do
    ".editor" ? do
        height (pct 100)
        width (pct 100)
    ".CodeMirror" ? do
        fontSize (px 16)
        fontFamily ["Hasklig"] []
    ".ErrorRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor red
        borderBottom solid (px 1) red
    ".WarningRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor orange
        borderBottom solid (px 1) orange
    ".LintRef" ? do
        textDecorationLine underline
        textDecorationStyle wavy
        textDecorationColor blue
        borderBottom solid (px 1) blue

updateTextMarks :: JSVal -> [LogRef] -> JSM ()
updateTextMarks editor logRefs  = do
  newMarks <- forM logRefs $ \logRef -> do
      from <- obj
      from ^. jss ("line" :: Text) (srcSpanStartLine (logRefSrcSpan logRef) - 1)
      from ^. jss ("ch" :: Text) (srcSpanStartColumn (logRefSrcSpan logRef))
      to <- obj
      to ^. jss ("line" :: Text) (srcSpanEndLine (logRefSrcSpan logRef) - 1)
      to ^. jss ("ch" :: Text) (srcSpanEndColumn (logRefSrcSpan logRef) + 1)
      m <- obj
      m ^. jss ("from" :: Text) from
      m ^. jss ("to" :: Text) to
      m ^. jss ("t" :: Text) (show $ logRefType logRef)
      return m
  f <- eval (
    "(function(editor, newMarks) {\n\
    \  var marksToAdd = new Set(newMarks.map(JSON.stringify));\n\
    \  editor.getAllMarks().forEach(function(mark) {\n\
    \    var loc = mark.find();\n\
    \    if( loc !== undefined && mark.leksahLogRefType !== undefined ) {\n\
    \      if( !marksToAdd.delete(JSON.stringify({from: {line: loc.from.line, ch: loc.from.ch }, to: {line: loc.to.line, ch: loc.to.ch}, t: mark.leksahLogRefType}))) {\n\
    \        mark.clear();\n\
    \      }\n\
    \    }\n\
    \  });\n\
    \  marksToAdd.forEach(function(json) {\n\
    \    var m = JSON.parse(json);\n\
    \    var newMark = editor.markText(m.from, m.to, {className: m.t});\n\
    \    newMark.leksahLogRefType = m.t;\n\
    \  });\n\
    \})" :: Text)
  void $ call f f (editor, newMarks)

gotoSrcSpan :: MonadJSM m => JSVal -> SrcSpan -> m ()
gotoSrcSpan editor srcSpan = liftJSM $ do
  p <- obj
  p ^. jss ("line" :: Text) (srcSpanStartLine srcSpan - 1)
  p ^. jss ("ch" :: Text) (srcSpanStartColumn srcSpan)
  void $ editor ^. js2 ("scrollIntoView" :: Text) p (100 :: Double)
  void $ editor ^. js1 ("setCursor" :: Text) p
  void $ editor ^. js0 ("focus" :: Text)

editorWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Event t (DMap IDEWidget Identity)
  -> m
    ( Event t (Map FilePath (Text, Maybe ()))
    , FilePath -> Dynamic t (Maybe ()) -> m (Event t ()))
editorWidget ide allEvents = do
  let tabEvents = select (fan allEvents) TabWidget
      workspaceEvents = select (fan (select (fanMap tabEvents) (Const2 WorkspaceKey))) WorkspaceTab
      openFileRequestsE = leftmost
        [ fmapMaybe (listToMaybe . (^.. traverse . _ProjectFileEvents . traverse . _OpenFile)) workspaceEvents
        , fmapMaybe (listToMaybe . (^.. traverse . _ProjectPackageEvents . traverse . _PackageFileEvents . traverse . _OpenFile)) workspaceEvents
        ]
  -- Check file exists or should be created
  openFileE :: Event t FilePath <- fmapMaybe id <$> performEvent (ffor openFileRequestsE $ \case
    (True, file) -> return $ Just file
    (_, file) ->
      liftIO (doesFileExist file) >>= \case
        False -> return Nothing
        True -> return $ Just file)
  let gotoLocationE :: Event t LogRef = fmapMaybe listToMaybe $ (^.. _ErrorsGoto) <$>
        select (fan (select (fanMap tabEvents) (Const2 ErrorsKey))) ErrorsTab
      fileE = leftmost [ openFileE, logRefFullFilePath <$> gotoLocationE ]
  logRefsByFileD <- fmap (M.fromListWith (<>) . map (\lr -> (logRefFullFilePath lr, [lr])) . toList) <$> holdUniqDyn (view allLogRefs <$> ide)
  locationsD <- foldDyn (<>) mempty $ (\lr -> logRefFullFilePath lr =: lr) <$> gotoLocationE
  -- recentFilesD <- foldDyn (\f fs -> f <> filter (`notElem` f) fs) [] filesE
  return
    ( (=:("wide0", Just())) <$> fileE
    , \file _ -> do
--      locationD <- holdUniqDyn $ M.lookup file <$> locationsD
      (changeE, triggerChangeE) <- newTriggerEvent
      logRefsD <- holdUniqDyn $ fromMaybe [] . M.lookup file <$> logRefsByFileD
      exists <- liftIO (doesFileExist file)
      liftIO (if exists then decodeUtf8' <$> BS.readFile file else return (Right "")) >>= \case
        Left _e -> return ()
        Right contents -> do
          ta <- textArea $ def & textAreaConfig_initialValue .~ contents
          postBuild <- getPostBuild
          editorE <- delay 1 =<< performEvent (liftJSM (do
              code <- obj
              code ^. jss ("mode" :: Text) ("haskell" :: Text)
              editor :: JSVal <- jsg ("CodeMirror" :: Text) ^. js2 ("fromTextArea" :: Text) (_textArea_element ta) code
              _ <- editor ^. js2 ("setOption" :: Text) ("theme" :: Text) ("abcdef" :: Text)
              _ <- editor ^. js2 ("setOption" :: Text) ("lineNumbers" :: Text) ("true" :: Text)
              _ <- editor ^. js2 ("setSize" :: Text) ("100%" :: Text) ("100%" :: Text)
              _ <- editor ^. js2 ("on" :: Text) ("change" :: Text) (fun $ \ _ _ _ -> liftIO $ triggerChangeE ())
              return editor) <$ postBuild)
          performEvent_ $ ffor (attach (current $ (,) <$> locationsD <*> logRefsD) editorE) $ \((locations, logRefs), editor) -> liftJSM $ do
              case M.lookup file locations of
                Nothing -> void $ editor ^. js0 ("refresh" :: Text)
                Just logRef -> gotoSrcSpan editor (logRefSrcSpan logRef)
              updateTextMarks editor logRefs
          editorD <- holdDyn Nothing $ Just <$> editorE
          performEvent_ $ ffor (attach (current editorD) (updated logRefsD)) $ \case
            (Nothing, _) -> return ()
            (Just editor, logRefs) -> liftJSM $ updateTextMarks editor logRefs
          let gotoE = ffilter ((==file) . logRefFullFilePath) gotoLocationE
          performEvent_ $ ffor (attach (current editorD) gotoE) $ \case
              (Just editor, logRef) -> gotoSrcSpan editor (logRefSrcSpan logRef)
              _ -> return ()
          return ()
      return changeE)
