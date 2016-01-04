{-# LANGUAGE RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.HIE
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Integration with haskell-ide-engine
--
-----------------------------------------------------------------------------
module IDE.HIE (
    hieType
  , hieInfo
  , shutDownHie
  , initHie
  , resetHie
  , refactorCommands
  , runHIECommand
  , applyRefactor
) where

import IDE.Core.State
import IDE.Pane.SourceBuffer
       (selectedRange,selectedModuleName,inActiveBufContext,IDEBuffer(..),selectedTextOrCurrentIdentifier)
import IDE.Pane.Log (getDefaultLogLaunch)
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.Workspaces (packageTry)
import IDE.LogRef
import IDE.Utils.GUIUtils
import IDE.Utils.Tool
import IDE.TextEditor

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when, void, join)
import Control.Monad.Trans.Reader (ask)

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (foldM, fold, consume,head,take)
import Data.Conduit (($$))
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

import System.Exit (ExitCode(..))

import Graphics.UI.Gtk
import Graphics.UI.Editor.MakeEditor (buildEditor,FieldDescription(..),mkField)
import Graphics.UI.Editor.Parameters
       (paraMinSize, paraMultiSel, Parameter(..), emptyParams, (<<<-),
        paraName)
import Graphics.UI.Editor.Simple
       (textEditor)
import System.Log.Logger (debugM)

import Haskell.Ide.Engine.PluginTypes hiding (ideMessage)
import Haskell.Ide.Engine.SemanticTypes

import System.Directory (removeFile)

-- Display hie type of the selected text
hieType :: IDEAction
hieType = do
    withHieState $ \(HieState hie cs)->do
        rg <- selectedRange
        case (rg,getTypeCommand cs) of
            (Just leksahSel,Just cmd) -> do
                let ghcModSel@((sl,sc),_) = toGhcModSel leksahSel
                inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
                    let mfn = fileName ideBuf
                    case mfn of
                      Just fn -> getToolOutput hie (IdeRequest (cmdName cmd)
                                                                          (M.insert "file" (ParamValP (ParamFile $ T.pack fn)) $
                                                                           M.insert "start_pos" (ParamValP (ParamPos (sl,sc))) $
                                                                           M.empty
                                                                           )) $ \types -> do
                        let mMatching= matchingType ghcModSel $ results types
                        case mMatching of
                            Nothing -> return ()
                            Just matching -> showPopupText edView eBuf $ trText matching
                      Nothing -> return ()
                    return $ Just ()
                return ()
            _ -> return ()

-- Display hie info of the selected text
hieInfo :: IDEAction
hieInfo = do
    withHieState $ \(HieState hie cs)->do
        mt <- selectedTextOrCurrentIdentifier
        case (mt,getInfoCommand cs) of
            (Just t,Just cmd) -> do
                inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
                    let mfn = fileName ideBuf
                    case mfn of
                      Just fn -> getToolOutput hie (IdeRequest (cmdName cmd)
                                                                        ( M.insert "file" (ParamValP (ParamFile $ T.pack fn)) $
                                                                          M.insert "expr" (ParamValP (ParamText t)) $
                                                                            M.empty
                                                                            )) $ \info -> do
                        showPopupText edView eBuf info
                      Nothing -> return ()
                    return $ Just ()
                return ()
            _ -> return ()

-- | Run an action with the HIE executable
withHieState f= do
    mhiePath <- hiePath <$> readIDE prefs
    case mhiePath of
        Nothing -> return ()
        Just hiePath -> do
            mhieState <- readIDE hieState
            case mhieState of
                        Just st -> f st
                        Nothing -> do
                            st <- liftIO $ newToolState
                            let clr=noInputCommandLineReader {sepCharacter=Just '\STX'}
                            liftIO $ runInteractiveTool st clr hiePath [] Nothing
                            getToolOutput st (IdeRequest "base:plugins" M.empty) $ \plugins -> do
                                      let hs = HieState st plugins
                                      modifyIDE (\i->(i{hieState=Just hs},st))
                                      f hs

shutDownHie :: IDEAction
shutDownHie = do
    mhieState <- readIDE hieState
    case mhieState of
        Nothing -> return ()
        Just (HieState hie cs) -> liftIO $ do
            tp <- toolProcess hie
            terminateProcess tp

initHie :: IDEAction
initHie =  withHieState $ \_-> return ()

resetHie :: IDEAction
resetHie = shutDownHie >> initHie

-- | List all refactor commands
refactorCommands :: IDEM  [CommandDescriptor]
refactorCommands = do
    mhieState <- readIDE hieState
    return $ case mhieState of
        Just (HieState _ cs) -> getRefactorCommands cs
        Nothing -> []

-- | run a HIE command
runHIECommand :: (ValidResponse r) => CommandDescriptor -> (r -> IDEAction) -> IDEAction
runHIECommand c f = do
    liftIO $ debugM "leksah" ("executing " ++ (T.unpack $ cmdName c))
    withHieState $ \(HieState hie cs)->do
        rg <- selectedRange
        void $ inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
            let ghcModSel = fmap toGhcModSel rg
            let mfn = fileName ideBuf
            --liftIO $ debugM "leksah" (show $ hcContexts c)
            let pvsCtx = foldl' collectParamsCtx (ParamValues ghcModSel mfn M.empty [] []) (cmdContexts c)
            let pvs = foldl' collectParams pvsCtx (cmdAdditionalParams c)
            liftIO $ debugM "leksah" (show pvs)
            case pvError pvs of
                [] -> askForParams pvs $ \ps -> getToolOutput hie (IdeRequest (cmdName c) ps) f
                errs -> ideMessage High $ T.pack $ show errs
            return $ Just ()

-- | Ask for additional parameters
askForParams :: ParamValues -> (ParamMap-> IDEAction) -> IDEAction
askForParams pvs f = case pvPending pvs of
    []        -> f $ pvOK pvs
    pends -> do
        parent <- liftIDE getMainWindow

        (resp,mval) <- liftIO $ do
            dia                        <-   dialogNew
            set dia [ windowTransientFor := parent
                    , windowTitle := __ "Construct new module" ]
            windowSetDefaultSize dia 400 100
            upper                      <-   dialogGetContentArea dia
            lower                      <-   dialogGetActionArea dia
            (widget,inj,ext,_)  <-   buildEditor (paramFields pends)
                                                M.empty
            bb      <-  hButtonBoxNew
            boxSetSpacing bb 6
            buttonBoxSetLayout bb ButtonboxSpread
            cancel  <-  buttonNewFromStock "gtk-cancel"
            ok      <-  buttonNewFromStock "gtk-ok"
            boxPackEnd bb cancel PackNatural 0
            boxPackEnd bb ok PackNatural 0

            errorLabel <-  labelNew (Nothing :: Maybe String)
            labelSetLineWrap errorLabel True
            widgetSetName errorLabel ("errorLabel" :: String)

            on ok buttonActivated $ do
                fields <- ext M.empty
                case fmap M.size fields == Just (length pends) of
                    True -> dialogResponse dia ResponseOk
                    False -> do
                                boxPackStart (castToBox upper) errorLabel PackNatural 0
                                boxReorderChild (castToBox upper) errorLabel 0
                                labelSetText errorLabel ("Fill in all fields" :: String)
                                widgetShow errorLabel

            on cancel buttonActivated (dialogResponse dia ResponseCancel)
            boxPackStart (castToBox upper) widget PackGrow 0
            boxPackEnd (castToBox lower) bb PackNatural 5
            set ok [widgetCanDefault := True]
            widgetGrabDefault ok
            widgetShowAll dia
            resp  <- dialogRun dia
            value <- ext M.empty
            widgetDestroy dia
            return (resp,value)
        case (resp,mval) of
            (ResponseOk,Just value)    -> f (M.union (pvOK pvs) (M.map (ParamValP . ParamText) value))
            _             -> return ()

-- | Build parameter field definition
-- we build a simple map name -> Text value, we use the proper types (ParamValP, etc.) later
paramFields :: [ParamDescription] -> FieldDescription (M.Map ParamName T.Text)
paramFields pds = VFD emptyParams $ map (\p->
        mkField
            (paraName <<<- ParaName (__ (pHelp p))
                    $ emptyParams)
            (M.findWithDefault "" (pName p))
            (\ a b -> M.insert (pName p) a b)
            (textEditor (const True) True)) pds


-- | Collect parameters values for context
collectParamsCtx :: ParamValues -> AcceptedContext -> ParamValues
collectParamsCtx pv@(ParamValues sel fn ok err pen) ctx
    | ctx == CtxNone = pv
    | ctx == CtxFile
    , Just f <- fn =  ParamValues sel fn
            (M.insert "file" (ParamValP (ParamFile $ T.pack f)) ok) err pen
    | ctx == CtxPoint
    , Just f <- fn
    , Just ((sl,sc),_) <- sel =  ParamValues sel fn
            (M.insert "file" (ParamValP (ParamFile $ T.pack f)) $
                M.insert "start_pos" (ParamValP (ParamPos (sl,sc)))
                ok) err pen
    | ctx == CtxRegion
    , Just f <- fn
    , Just ((sl,sc),_) <- sel
    , Just (_,(el,ec)) <- sel = ParamValues sel fn
            (M.insert "file" (ParamValP (ParamFile $ T.pack f)) $
                M.insert "start_pos" (ParamValP (ParamPos (sl,sc))) $
                M.insert "end_pos" (ParamValP (ParamPos (el,ec)))
                ok) err pen
    | otherwise = ParamValues sel fn ok (T.pack (show ctx):err) pen

-- | Collect parameters values
collectParams :: ParamValues -> ParamDescription -> ParamValues
collectParams (ParamValues sel fn ok err pen) p
    | pType p == PtFile
    , Just f <- fn = ParamValues sel fn (M.insert (pName p) (ParamValP (ParamFile $ T.pack f)) ok) err pen
    | pType p == PtPos
    , pName p == "start_pos"
    , Just ((sl,sc),_) <- sel = ParamValues sel fn (M.insert (pName p) (ParamValP (ParamPos (sl,sc))) ok) err pen
    | pType p == PtPos
    , pName p == "end_pos"
    , Just (_,(el,ec)) <- sel = ParamValues sel fn (M.insert (pName p) (ParamValP (ParamPos (el,ec))) ok) err pen
    | pType p == PtText = ParamValues sel fn ok err (p:pen)
    | otherwise = ParamValues sel fn ok (pName p:err) pen

-- | Collected parameters values, some from the current information, some provided by the user
data ParamValues = ParamValues
    { pvSelection :: Maybe ((Int,Int),(Int,Int))
    ,  pvFileName :: Maybe FilePath
    ,  pvOK :: ParamMap
    ,  pvError :: [T.Text]
    ,  pvPending :: [ParamDescription]
    } deriving (Show)

-- | Apply refactoring result
applyRefactor :: RefactorResult -> IDEAction
applyRefactor rr = do
    ideMessage High $ T.pack $ show rr
    void $ inActiveBufContext Nothing $ \_ _ eBuf ideBuf _ -> do
        mapM_ (applyRefactor1 (fileName ideBuf) eBuf) $ rrDiffs rr
        return Nothing
    where
       applyRefactor1 mfn eBuf (HieDiff f1 f2 df) = do
                if Just f1 == mfn
                    then do
                        -- set text of current buffer
                        -- TODO apply diff to current text
                        t <- liftIO $ T.readFile f2
                        setText eBuf t
                    else
                        -- write file, let leksah pick up the new code
                        liftIO (T.readFile f2 >>= T.writeFile f1)
                liftIO $ removeFile f2

-- | Find all commands matching the given return types and possible contexts
findCommands :: ReturnType -> [AcceptedContext] -> IdePlugins -> [CommandDescriptor]
findCommands ret [] = fullCmdNames . map (\(p,cs)-> (p,filter (\c->(cmdReturnType c)==ret) cs)) . M.assocs . ipMap
findCommands ret ctxs = fullCmdNames . map (\(p,cs)->(p,filter (\c->any (`elem` (cmdContexts c)) ctxs && (cmdReturnType c)==ret) cs)) . M.assocs . ipMap

-- | Get type command
getTypeCommand ::  IdePlugins -> Maybe CommandDescriptor
getTypeCommand = listToMaybe . findCommands "TypeInfo" [CtxPoint]

-- | Get info command
getInfoCommand ::  IdePlugins -> Maybe CommandDescriptor
getInfoCommand = listToMaybe . filter (\c->cmdName c=="ghcmod:info") . findCommands "Text" [CtxFile]

-- | Get all refactor commands
getRefactorCommands :: IdePlugins -> [CommandDescriptor]
getRefactorCommands = findCommands "RefactorResult" [CtxPoint,CtxRegion]

-- | Get the full command name (plugin:command)
fullCmdName :: PluginId -> CommandDescriptor -> CommandDescriptor
fullCmdName pid c@(CommandDesc {..})= c{cmdName=pid <> ":" <> cmdName}

fullCmdNames :: [(PluginId,[CommandDescriptor])] -> [CommandDescriptor]
fullCmdNames cmds = concatMap (\(p,cs)->map (fullCmdName p) cs) cmds

-- | Show the given text in popup over the editor selection
showPopupText :: (TextEditor editor) => (EditorView editor) -> (EditorBuffer editor) -> T.Text -> IDEAction
showPopupText edView eBuf txt = do
        window <- liftIDE getMainWindow
        mbDrawWindow <- liftIDE $ getWindow edView
        case mbDrawWindow of
            Nothing -> return ()
            Just drawWindow -> do
                (start, end) <- liftIDE $ getSelectionBounds eBuf
                Rectangle x y _ height <- liftIDE $ getIterLocation edView start
                (wx,wy)<-liftIDE $ bufferToWindowCoords edView (x,y+height)
                liftIO $ do
                    (mwx, mwy)  <- drawWindowGetOrigin drawWindow
                    popup <- windowNew -- to be able to get focus
                    set popup [
                         windowTypeHint        := WindowTypeHintUtility,
                         windowDecorated     := False,
                         windowResizable       := False,
                         windowTransientFor  := window]
                    lbl <- labelNew (Just txt)
                    set lbl [labelSelectable := True]
                    containerAdd popup lbl
                    popup `on` focusOutEvent $ do
                        liftIO $ widgetDestroy popup
                        return True
                    widgetShowAll popup
                    windowMove popup (wx+mwx) (wy+mwy)
                    widgetGrabFocus popup
        return ()

-- | Run the given command given as a ToJSON and run an action on the result
getToolOutput :: (ValidResponse b) => ToolState -> IdeRequest -> (b -> IDEAction) -> IDEAction
getToolOutput ts cmdObj f = do
    ideR <- ask
    let cmd = toTextCmd cmdObj
    --liftIO $ debugM "leksah" "in getToolOutput1"
    liftIO $ executeCommand ts (cmd <> "\STX") cmd $ do
        --liftIO $ debugM "leksah" "in getToolOutput2"
        output <- CL.consume
        --liftIO $ debugM "leksah" "in getToolOutput3"
        liftIO $ debugM "leksah" ("hie output:" ++ show output)
        liftIO . (`reflectIDE` ideR) . postAsyncIDE $ do
            let err = T.concat $ mapMaybe errorOnly output
            when (not $ T.null err) $
                ideMessage High $ err
            let t = T.takeWhile (/= '\STX') . T.concat $ mapMaybe outputOnly output
            liftIO $ debugM "leksah" $ T.unpack $ "t:"<> t
            let ret = fromTextResp t
            case ret of
                Just (IdeResponseOk r)  -> f r
                Just (IdeResponseError ir) -> ideMessage High $ T.pack $ show ir
                Just (IdeResponseFail ir) -> ideMessage High $ T.pack $ show ir
                _ -> return ()

-- | Transform the text viewer selection (0 based), into a ghc-mod selection (1-base)
toGhcModSel :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
toGhcModSel ((a,b),(c,d))=((a+1,b+1),(c+1,d+1))

-- | Get only the normal output from a tool
outputOnly :: ToolOutput -> Maybe T.Text
outputOnly (ToolOutput l)=Just l
outputOnly _ = Nothing

-- | Get only the normal output from a tool
errorOnly :: ToolOutput -> Maybe T.Text
errorOnly (ToolError l)=Just l
errorOnly _ = Nothing

toTextCmd :: (ToJSON a) => a -> T.Text
toTextCmd v = (toStrict $ toLazyText $ encodeToTextBuilder $ toJSON v)

fromTextResp :: (ValidResponse a)=> T.Text -> Maybe (IdeResponse a)
fromTextResp = decode . BSL.fromStrict . T.encodeUtf8


-- | Find the best matching type from the selection
matchingType :: ((Int,Int),(Int,Int)) -> [TypeResult] -> Maybe TypeResult
matchingType _ [] = Nothing
matchingType (a,b) (x:_) | a==b = Just x -- single point: take the smallest type
matchingType (a,b) xs = listToMaybe $ filter (matchResult a b) xs -- take the smallest type
                                                                                                        -- encompassing the full selection range
    where
        matchResult s1 e1 tr = matchPos s1 e1 (trStart tr) (trEnd tr)
        matchPos  s1 e1 s2 e2 = s1 `afterPos` s2 && e1 `before` e2
        before (l1,c1) (l2,c2) = l1 < l2 || (l1==l2 && c1<=c2)
        afterPos (l1,c1) (l2,c2) = l1 > l2 || (l1==l2 && c1>=c2)

