module IDE.LPaste where

import Data.Maybe
import Data.String.Utils
import Network.HTTP
import Network.Stream

data Post = Post {
    title :: Maybe String,
    paste :: String
}

data Parameters = Parameters {
    private :: Bool,
    author :: Maybe String,
    channel :: Maybe String,
    email :: Maybe String
}

type URL = String

baseUrl :: URL
baseUrl = "http://lpaste.net"

createLink :: Parameters -> Post -> String
createLink parameters post =
    concat [ baseUrl ++ "/new"
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
           , replace " " "+" $ paste post
           , "&email="
           , fromMaybe "" $ email parameters
           ]

defaultParameters :: Parameters
defaultParameters = Parameters { private = True, author = Nothing, channel = Nothing, email = Nothing}

somePost :: Post
somePost = Post { title = Nothing, paste = "Some Post"}

headerLookup :: [Header] -> HeaderName -> Maybe String
headerLookup headers key = let header = filter (\(Header k v) -> k == key) headers
                           in
                            if length header == 0 then Nothing
                            else let (Header k v) = (head header) in Just v

extractHeaders :: Either ConnError (Response a) -> [Header]
extractHeaders result = case result of Left _ -> []
                                       Right rsp -> rspHeaders rsp

extractLocationHeader :: Either ConnError (Response a) -> String
extractLocationHeader result = fromMaybe "" $ flip headerLookup HdrLocation $ extractHeaders result

main :: IO ()
main = do
      let link = createLink defaultParameters somePost
      result <- simpleHTTP (getRequest link)
      let location = extractLocationHeader result
      putStrLn $ baseUrl ++ location
