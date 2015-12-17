{-# LANGUAGE RecordWildCards #-}
{-# Language OverloadedStrings #-}
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
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (foldM, fold, consume,head,take)
import Data.Conduit (($$))
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

import System.Exit (ExitCode(..))

import Graphics.UI.Gtk
import System.Log.Logger (debugM)

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
                      Just fn -> getToolOutput hie (object ["cmd".= cmdName cmd
                                                                        ,"params".= object
                                                                            ["file".= object ["file".= fn]
                                                                            ,"start_pos".=object ["line" .=sl,"col".=sc]]]) $ \types -> do
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
                      Just fn -> getToolOutput hie (object ["cmd".= cmdName cmd
                                                                        ,"params".= object
                                                                            ["file".= object ["file".= fn]
                                                                            ,"expr".=object ["text" .= t]]]) $ \info -> do
                        showPopupText edView eBuf (irText info)
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
                            getToolOutput st (object ["cmd".= ("base:plugins"::T.Text)
                                                                            ,"params".= object []]) $ \plugins -> do
                                      let cs=fromMaybe [] $ parseMaybe allCommandsParser plugins
                                      let hs = HieState st cs
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

refactorCommands :: IDEM  [HieCommand]
refactorCommands = do
    mhieState <- readIDE hieState
    return $ case mhieState of
        Just (HieState _ cs) -> getRefactorCommands cs
        Nothing -> []

-- | run a HIE command
runHIECommand :: HieCommand -> IDEAction
runHIECommand c = do
    liftIO $ debugM "leksah" ("executing " ++ (T.unpack $ hcName c))
    withHieState $ \(HieState hie cs)->do
        rg <- selectedRange
        void $ inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
            let ghcModSel = fmap toGhcModSel rg
            let mfn = fileName ideBuf
            --liftIO $ debugM "leksah" (show $ hcContexts c)
            let pvsCtx = foldl' collectParamsCtx (ParamValues ghcModSel mfn [] [] []) (hcContexts c)
            let pvs = foldl' collectParams pvsCtx (hcParams c)
            liftIO $ debugM "leksah" (show pvs)
            case pvError pvs of
                [] -> do
                    getToolOutput hie (object ["cmd".= cmdName c
                                                             ,"params".= object (pvOK pvs)]) $ \res -> do
                       liftIO $ debugM "leksah" (show (encode (res::Value)))
                errs -> liftIO $ debugM "leksah" (show errs)
            return $ Just ()


-- | Collect parameters values for context
collectParamsCtx :: ParamValues -> T.Text -> ParamValues
collectParamsCtx pv@(ParamValues sel fn ok err pen) ctx
    | ctx == "none" = pv
    | ctx == "file"
    , Just f <- fn =  ParamValues sel fn
            (("file",object ["file".= fn])
                :ok) err pen
    | ctx == "point"
    , Just f <- fn
    , Just ((sl,sc),_) <- sel =  ParamValues sel fn
            (("file",object ["file".= fn])
                :("start_pos",object ["line" .=sl,"col".=sc])
                :ok) err pen
    | ctx == "region"
    , Just f <- fn
    , Just ((sl,sc),_) <- sel
    , Just (_,(el,ec)) <- sel = ParamValues sel fn
            (("file",object ["file".= fn])
                :("start_pos",object ["line" .=sl,"col".=sc])
                :("end_pos",object ["line" .=el,"col".=ec])
                :ok) err pen
    | otherwise = ParamValues sel fn ok (ctx:err) pen

-- | Collect parameters values
collectParams :: ParamValues -> HieParameter -> ParamValues
collectParams (ParamValues sel fn ok err pen) p
    | hpType p == "file"
    , Just f <- fn = ParamValues sel fn ((hpName p,toJSON f):ok) err pen
    | hpType p == "pos"
    , hpName p == "start_pos"
    , Just ((sl,sc),_) <- sel = ParamValues sel fn ((hpName p,object ["line" .=sl,"col".=sc]):ok) err pen
    | hpType p == "pos"
    , hpName p == "end_pos"
    , Just (_,(el,ec)) <- sel = ParamValues sel fn ((hpName p,object ["line" .=el,"col".=ec]):ok) err pen
    | hpType p == "text" = ParamValues sel fn ok err (p:pen)
    | otherwise = ParamValues sel fn ok (hpName p:err) pen

-- | Collected parameters values, some from the current information, some provided by the user
data ParamValues = ParamValues
    { pvSelection :: Maybe ((Int,Int),(Int,Int))
    ,  pvFileName :: Maybe FilePath
    ,  pvOK :: [(T.Text,Value)]
    ,  pvError :: [T.Text]
    ,  pvPending :: [HieParameter]
    } deriving (Show)

-- | Parse all commands exposed via HIE
allCommandsParser (Object v) = do
  v2 <- v .: "plugins"
  parseInPlugin v2
  where
    parseInPlugin (Object v2) = parseCmdList v2
    parseInPlugin _ = mempty
    parseCmdList o =
      let ass = H.toList o
      in concat <$> mapM findRef ass
      where
        findRef (pl,Array cmds) = mapM (getRef pl) $ V.toList cmds
        findRef _ = mempty
        getRef pl (Object m) =
          HieCommand pl <$> m .: "name"
                    <*> m .:? "ui_description" .!= ""
                    <*> m .: "contexts"
                    <*> m .: "additional_params"
                    <*> m .: "return_type"
        getRef _ _ = mempty
allCommandsParser _ = mempty



-- | Find all commands matching the given return types and possible contexts
findCommands :: T.Text -> [T.Text] -> [HieCommand] -> [HieCommand]
findCommands ret [] = filter (\c-> (hcRet c)==ret)
findCommands ret ctxs = filter (\c->any (`elem` (hcContexts c)) ctxs && (hcRet c)==ret)

-- | Get type command
getTypeCommand ::  [HieCommand] -> Maybe HieCommand
getTypeCommand = listToMaybe . findCommands "TypeInfo" ["point"]

-- | Get info command
getInfoCommand ::  [HieCommand] -> Maybe HieCommand
getInfoCommand = listToMaybe . filter (\c->hcName c=="info" && hcPlugin c=="ghcmod") . findCommands "Text" ["file"]

-- | Get all refactor commands
getRefactorCommands :: [HieCommand] -> [HieCommand]
getRefactorCommands = findCommands "RefactorResult" ["point","region"]

-- | Get the full command name (plugin:command)
cmdName :: HieCommand -> T.Text
cmdName HieCommand {..}= hcPlugin <> ":" <> hcName

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
getToolOutput :: (ToJSON a,FromJSON b) => ToolState -> a -> (b -> IDEAction) -> IDEAction
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
            --liftIO $ debugM "leksah" $ T.unpack $ "t:"<> t
            let ret = fromTextResp t
            case ret of
                Just r  -> f r
                Nothing -> return ()

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

fromTextResp :: (FromJSON a)=> T.Text -> Maybe a
fromTextResp = decode . BSL.fromStrict . T.encodeUtf8

-- | ghc-mod list of types
data TypeInfo = TypeInfo { results :: [TypeResult] }
  deriving (Show,Read,Eq,Ord)

-- | One type result from ghc-mod
data TypeResult = TypeResult
    { trStart :: (Int,Int)
    ,  trEnd :: (Int,Int)
    ,  trText :: T.Text
    } deriving (Show,Read,Eq,Ord)

instance FromJSON TypeResult where
  parseJSON (Object v) = TypeResult
    <$> (jsonToPos =<< (v .: "start"))
    <*> (jsonToPos =<< (v .: "end"))
    <*> v .: "type"
  parseJSON _ = empty

instance FromJSON TypeInfo where
    parseJSON (Object v) = TypeInfo <$> v .: "type_info"
    parseJSON _ = empty


jsonToPos :: Value -> Parser (Int,Int)
jsonToPos (Object v) = (,) <$> v .: "line" <*> v.: "col"
jsonToPos _ = empty

data InfoResult = InfoResult {
    irText :: T.Text
    }

instance FromJSON InfoResult where
  parseJSON (Object v) = InfoResult <$> v .: "ok"
  parseJSON _ = empty

-- | Find the best matching type from the selection
matchingType :: ((Int,Int),(Int,Int)) -> [TypeResult] -> Maybe TypeResult
matchingType _ [] = Nothing
matchingType (a,b) (x:_) | a==b = Just x -- single point: take the smallest type
matchingType (a,b) xs = listToMaybe $ filter (matchResult a b) xs -- take the smallest type
                                                                                                        -- encompassing the full selection range
    where
        matchResult s1 e1 tr = matchPos s1 e1 (trStart tr) (trEnd tr)
        matchPos  s1 e1 s2 e2 = s1 `after` s2 && e1 `before` e2
        before (l1,c1) (l2,c2) = l1 < l2 || (l1==l2 && c1<=c2)
        after (l1,c1) (l2,c2) = l1 > l2 || (l1==l2 && c1>=c2)

