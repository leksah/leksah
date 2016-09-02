import Network.HTTP
import Data.Maybe
import Data.String.Utils
import Network.Stream

data Post = Post { title :: Maybe String,
                   paste :: String}

data Parameters = Parameters { private :: Bool,
                               author :: Maybe String,
                               channel :: Maybe String,
                               email :: Maybe String
                             }

createLink :: Parameters -> Post -> String
createLink parameters post =
    concat [ "http://lpaste.net/new?private="
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

baseUrl :: String
baseUrl = "http://lpaste.net"

main :: IO ()
main = do
      let link = createLink defaultParameters somePost
      openURL <- getResponseBody =<< simpleHTTP (getRequest link)
      result <- simpleHTTP (getRequest link)
      let headers = case result of Left _ -> []
                                   Right rsp -> rspHeaders rsp
      let location = fromMaybe "" $ headerLookup headers HdrLocation
      putStrLn $ baseUrl ++ location
