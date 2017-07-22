{-# LANGUAGE OverloadedStrings#-}
module IDE.LPaste (uploadToLpaste) where

import IDE.Core.State
import IDE.Core.Types
import IDE.Pane.SourceBuffer
import IDE.Utils.GUIUtils (showDialog, showInputDialog, showDialogOptions)

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Network.HTTP

import qualified Data.Text as T
import Data.Monoid
import GI.Gtk.Enums (MessageType(..))
import Control.Monad (void)
import Control.Exception (SomeException, catch)
import Network.Stream (ConnError(..))
import GI.Gtk (Window)

type Parameter = (String, String)

baseUrl :: String
baseUrl = "http://lpaste.net"

-- | Default Leksah parameters
leksahParams :: [Parameter]
leksahParams =
    [ ("private", "Private")
    , ("author" , "Leksah Haskell IDE")
    , ("channel", "")
    , ("language", "haskell") -- Might change in the future
    , ("email"  , "")
    ]

mkReq :: String -> String
mkReq str =
    let post = [("title", ""), ("paste", str)] -- Randomly generate title?
    in baseUrl ++ "/new?" ++ urlEncodeVars (leksahParams ++ post)

-- | Lookup the value of the location header.
locationLookup :: [Header] -> String
locationLookup [] = ""
locationLookup (Header k v:xs) =
    if HdrLocation == k then v else locationLookup xs

-- | Main purpose function: Perform all the necessary actions for uploading and
-- return the link to the submission.
uploadSelected :: String -> IO (Maybe String)
uploadSelected str = do
    result <- simpleHTTP (postRequest $ mkReq str) `catch` handler
    case result of
        Right x -> return . Just . (++) baseUrl . locationLookup $ rspHeaders x
        Left _  -> return Nothing
    where
        handler e = return . Left  . ErrorMisc $ show (e :: SomeException)

uploadToLpaste :: IDEM ()
uploadToLpaste = do
    mainWindow <- getMainWindow
    maybeText <- selectedTextOrCurrentLine
    liftIO $ showDialogOptions
                (Just mainWindow)
                "Confirm upload to lpaste.net?"
                MessageTypeQuestion
                [("OK", uploadToLpaste' mainWindow maybeText), ("Cancel", return ())]
                (Just 0)

uploadToLpaste' :: Window -> Maybe T.Text -> IO ()
uploadToLpaste' mainWindow maybeText =
    case maybeText of
        Just text -> do
            mbLink <- uploadSelected $ T.unpack text
            case mbLink of
                Just link -> void $ showInputDialog (Just mainWindow) "LPaste link:" (T.pack link)
                Nothing   -> showDialog (Just mainWindow) ("Could not reach " <> T.pack baseUrl) MessageTypeError
        Nothing ->
            showDialog (Just mainWindow) "Please select some text in the editor" MessageTypeError
