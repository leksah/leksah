module IDE.LPaste where

import IDE.Core.State
import IDE.Core.Types
import IDE.Pane.SourceBuffer
import IDE.Utils.GUIUtils (showInputDialog)

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Network.HTTP

import qualified Data.Text as T

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
--
-- The simpleHTTP will return a Right on succes, the unwrapping is a workaround
-- and should be replaced with an Except monad.
uploadSelected :: String -> IO String
uploadSelected str =
    (\(Right x) -> (++) baseUrl . locationLookup $ rspHeaders x) <$>
    simpleHTTP (postRequest $ mkReq str)

uploadToLpaste :: IDEM ()
uploadToLpaste = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            link <- liftIO $ uploadSelected $ T.unpack text
            liftIO $ showInputDialog (T.pack "LPaste link:") (T.pack link)
            return ()
        Nothing -> ideMessage Normal $ T.pack "Please select some text in the editor"
