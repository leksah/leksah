module IDE.LPaste where

import Data.Maybe
import Data.String.Utils
import Network.HTTP
import Network.Stream

-- | Post record type containing the @content@ and optionally the @title@.
data Post = Post {
    title :: Maybe String,
    content :: String
}

-- | Parameters of the LPaste upload. Maybe fields are optional
data Parameters = Parameters {
    private :: Bool,
    author  :: Maybe String,
    channel :: Maybe String,
    email   :: Maybe String
}

-- | Make the link from a parameter and a post record
makeLink :: Parameters -> Post -> String
makeLink parameters post =
    replace " " "+" $
    concat [ "http://lpaste.net/new"
           , "?private="
           , if private parameters then "Private" else "Public"
           , "&title="
           , fromMaybe "" $ title post
           , "&author="
           , fromMaybe "" $ author parameters
           , "&language=haskell"
           , "&channel="
           , fromMaybe "" $ channel parameters
           , "&paste="
           , content post
           , "&email="
           , fromMaybe "" $ email parameters
           ]

-- | Default Leksah parameters
defaultParameters :: Parameters
defaultParameters = Parameters {
    private = False,
    author = Just "Leksah Haskell IDE",
    channel = Nothing,
    email = Nothing
}

-- | Lookup the value of the location header.
locationLookup :: [Header] -> String
locationLookup [] = ""
locationLookup (Header k v:xs) =
    if HdrLocation == k then v else locationLookup xs

-- | Dirty function, has to be removed or made better at leaast.. .:'(
extractHeaders :: Either ConnError (Response a) -> [Header]
extractHeaders result =
    case result of
        Left _ -> []
        Right rsp -> rspHeaders rsp

-- | Main purpose function: Perform all the necessary actions for uploading and
-- return the link to the submission.
uploadSelected :: String -> IO String
uploadSelected str =
    let link = makeLink defaultParameters (Post { title = Nothing, content = str})
    in ((++) "http://lpaste.net" . locationLookup . extractHeaders) <$> simpleHTTP (getRequest link)

