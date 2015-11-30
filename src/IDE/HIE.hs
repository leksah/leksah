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
) where

import IDE.Core.State
import IDE.Pane.SourceBuffer
       (selectedRange,selectedModuleName,inActiveBufContext,IDEBuffer(..))
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
hieType :: FilePath -> IDEAction
hieType hiePath = do
    withHieState hiePath $ \(HieState hie cs)->do
        rg <- selectedRange
        maybeModuleName <- selectedModuleName
        case (rg,maybeModuleName,getTypeCommand cs) of
            (Just leksahSel,Just mn,Just cmd) -> do
                let ghcModSel@((sl,sc),_) = toGhcModSel leksahSel
                inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
                    let mfn = fileName ideBuf
                    case mfn of
                      Just fn -> getToolOutput hie (object ["cmd".= cmdName cmd
                                                                        ,"params".= object
                                                                            ["file".= object ["file".=toJSON fn]
                                                                            ,"start_pos".=object ["line" .=toJSON sl,"col".=toJSON sc]]]) $ \types -> do
                        let mMatching= matchingType ghcModSel $ results types
                        case mMatching of
                            Nothing -> return ()
                            Just matching -> showPopupText edView eBuf $ trText matching
                      Nothing -> return ()
                    return $ Just ()
                return ()
            _ -> return ()

-- | Run an action with the HIE executable
withHieState hiePath f= do
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

