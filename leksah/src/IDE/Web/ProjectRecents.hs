{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Add Project… dialog's recently-used inputs, and the process-global
-- mirror that keeps every window's dialog looking at the same list.
--
-- Persistence lives in the web session ('IDE.Web.Session.wsProjectRecents'), but
-- that file is assembled per OS window in "IDE.Web.Main" — a 'foldDyn' in window
-- A teaches window B nothing until the next restart.  So the value also lives in
-- one 'IORef' here: the background add thread writes it as part of the add, and a
-- dialog reads it when it builds.  Same shape as 'IDE.Web.RecentFiles', two lines
-- of state, and it makes a second window's dialog correct immediately.
--
-- The reflex side still gets an event per add (see
-- 'IDE.Web.Widget.AddProject.addProjectDialog'); its only job is to keep the
-- session save in step.
module IDE.Web.ProjectRecents
  ( ProjectRecents(..)
  , emptyProjectRecents
  , RecentEntry(..)
  , addRecentEntry
  , recentPathsFor
  , readProjectRecents
  , setProjectRecents
  , recordProjectRecent
  ) where

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

-- | One successful add, in the dialog's own display vocabulary: the server as
-- typed (@"local"@ or an ssh host), the path still in its @~\/@-collapsed form,
-- and the optional command prefix.
data RecentEntry = RecentEntry
  { reServer :: Text
  , rePath   :: Text
  , rePrefix :: Text
  } deriving (Eq, Show)

-- | The three MRU lists behind the dialog's dropdowns, newest first.
data ProjectRecents = ProjectRecents
  { prServers  :: [Text]
      -- ^ servers used, newest first
  , prPaths    :: [(Text, Text)]
      -- ^ @(server, path)@ — paired so the path dropdown can show only the
      --   selected server's history rather than a mixture of local and remote.
  , prPrefixes :: [Text]
      -- ^ command prefixes used, newest first
  } deriving (Eq, Show, Generic)

emptyProjectRecents :: ProjectRecents
emptyProjectRecents = ProjectRecents [] [] []

-- | How many of each to keep.  Paths get a bigger cap because they are split
-- across servers at render time.
maxServers, maxPaths, maxPrefixes :: Int
maxServers  = 20
maxPaths    = 40
maxPrefixes = 20

-- | Push an entry to the front of each list, dropping any earlier copy.  Empty
-- strings are never recorded (an omitted prefix is not a choice worth
-- remembering).
addRecentEntry :: RecentEntry -> ProjectRecents -> ProjectRecents
addRecentEntry (RecentEntry server path prefix) r = ProjectRecents
    { prServers  = push maxServers  server        (prServers r)
    , prPaths    = push maxPaths    (server,path) (prPaths r)
    , prPrefixes = if prefix == "" then prPrefixes r
                                   else push maxPrefixes prefix (prPrefixes r)
    }
  where
    push :: Eq a => Int -> a -> [a] -> [a]
    push cap x xs = take cap (x : filter (/= x) xs)

-- | The paths remembered for one server, newest first.
recentPathsFor :: Text -> ProjectRecents -> [Text]
recentPathsFor server r = [ p | (s, p) <- prPaths r, s == server ]

{-# NOINLINE recentsRef #-}
recentsRef :: IORef ProjectRecents
recentsRef = unsafePerformIO (newIORef emptyProjectRecents)

readProjectRecents :: IO ProjectRecents
readProjectRecents = readIORef recentsRef

-- | Seed the mirror from a restored session (called once at boot).
setProjectRecents :: ProjectRecents -> IO ()
setProjectRecents r = atomicModifyIORef' recentsRef $ const (r, ())

-- | Record a successful add and return the updated lists.
recordProjectRecent :: RecentEntry -> IO ProjectRecents
recordProjectRecent e = atomicModifyIORef' recentsRef $ \r ->
    let r' = addRecentEntry e r in (r', r')
