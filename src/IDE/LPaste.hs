-- http://lpaste.net/new?private=Private&title=&author=&language=haskell&channel=&paste=mgasogpnagmoaf&email=

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

-- headerLookup :: [Header] -> String -> Maybe String
-- headerLookup headers key = filter ()

main :: IO ()
main = do
      -- let link = "http://www.haskell.org/"
      let link = createLink defaultParameters somePost
      openURL <- getResponseBody =<< simpleHTTP (getRequest link)
      -- print $ take 100 openURL
      -- print link
      -- Result r <- simpleHTTP (getRequest link)
      result <- simpleHTTP (getRequest link)
      let headers = case result of Left _ -> []
                                   Right rsp -> rspHeaders rsp
      let location = headerLookup headers
      print headers
      print location
      -- let Response x = r
              -- fetch document and return it (as a 'String'.)

      -- case rsp ofprint $ rspReason rsp
      -- rspHeaders rsp
      -- let headers = case rsp of Left _ -> ""
                                -- Right x -> rspHeaders x
      -- let headers = rspReason r
      -- print headers
      putStrLn ""
      -- print $ head headers
      -- output <- take 100 <$> getResponseBody rsp
      -- putStrLn output
