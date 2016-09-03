module IDE.LPaste where

import Data.Maybe
import Network.HTTP

type Parameter = (String, String)

-- | Default Leksah parameters
leksahParams :: [Parameter]
leksahParams =
    [ ("public", "Public")
    , ("author" , "Leksah Haskell IDE")
    , ("channel", "")
    , ("language", "haskell") -- Might change in the future
    , ("email"  , "")
    ]

mkReq :: String -> String
mkReq str =
    let post = [("title", "leksah test"), ("paste", str)] -- Randomly generate te title
    in "http://lpaste.net/new?" ++ urlEncodeVars (leksahParams ++ post)

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
    (\(Right x) -> (++) "http://lpaste.net" . locationLookup $ rspHeaders x) <$>
    simpleHTTP (postRequest $ mkReq str)

