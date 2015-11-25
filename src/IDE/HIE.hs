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
import Control.Monad (when, void)
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

import System.Exit (ExitCode(..))

import Graphics.UI.Gtk
import System.Log.Logger (debugM)

-- Display hie type of the selected text
hieType :: FilePath -> IDEAction
hieType hiePath = do
    mhieState <- readIDE hieState
    hie <- case mhieState of
                Just st -> return st
                Nothing -> do
                    st <- liftIO $ newToolState
                    let clr=noInputCommandLineReader {sepCharacter=Just '\STX'}
                    liftIO $ runInteractiveTool st clr hiePath [] Nothing
                    modifyIDE (\i->(i{hieState=Just st},st))
    rg <- selectedRange
    maybeModuleName <- selectedModuleName
    case (rg,maybeModuleName) of
        (Just leksahSel,Just mn) -> do
            let ghcModSel@((sl,sc),_) = toGhcModSel leksahSel
            inActiveBufContext Nothing $ \_ edView eBuf ideBuf _ -> do
                let mfn = fileName ideBuf
                case mfn of
                  Just fn -> getToolOutput hie (object ["cmd".= ("ghcmod:type"::T.Text)
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

-- | Run the given executable with the given arguments in the current package folder
-- Gather all output lines and feed to given function
--getToolOutput :: FilePath -> [T.Text]
--                        -> ([T.Text] -> PackageAction) -> IDEAction
--getToolOutput ghcModPath args f= packageTry $ do
--    package <- ask
--    logLaunch <- getDefaultLogLaunch
--    mvar <- liftIO newEmptyMVar
--    runExternalTool' (__ "ghc-mod type")
--            ghcModPath args
--            (ipdPackageDir package) $ do
--                output <- CL.consume
--                liftIO . putMVar mvar $ case take 1 $ reverse output of
--                    [ToolExit ExitSuccess] ->
--                        catMaybes $ map outputOnly output
--                    _ -> []
--    out <- liftIO $ takeMVar mvar
--    f out

getToolOutput :: (ToJSON a,FromJSON b) => ToolState -> a -> (b -> IDEAction) -> IDEAction
getToolOutput ts cmdObj f = do
    let cmd = toTextCmd cmdObj
    mvar <- liftIO newEmptyMVar
    liftIO $ debugM "leksah" "in getToolOutput1"
    liftIO $ executeCommand ts (cmd <> "\STX") cmd $ do
                liftIO $ debugM "leksah" "in getToolOutput2"
                output <- CL.take 3
                liftIO $ debugM "leksah" "in getToolOutput3"
                liftIO $ debugM "leksah" $ show output
                liftIO . putMVar mvar $ catMaybes $ map outputOnly output
    out <- liftIO $ takeMVar mvar
    let t = T.takeWhile (/= '\STX') $ T.concat out
    liftIO $ debugM "leksah" $ T.unpack $ "t:"<> t
    let ret = fromTextResp $ t
    case ret of
        Just r->  f r
        Nothing -> return ()

-- | Transform the text viewer selection (0 based), into a ghc-mod selection (1-base)
toGhcModSel :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
toGhcModSel ((a,b),(c,d))=((a+1,b+1),(c+1,d+1))

-- | Get only the normal output from a tool
outputOnly :: ToolOutput -> Maybe T.Text
outputOnly (ToolOutput l)=Just l
outputOnly _ = Nothing

toTextCmd :: (ToJSON a) => a -> T.Text
toTextCmd v = (toStrict $ toLazyText $ encodeToTextBuilder $ toJSON v)

fromTextResp :: (FromJSON a)=> T.Text -> Maybe a
fromTextResp = decode . BSL.fromStrict . T.encodeUtf8

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

