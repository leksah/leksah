{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The browser demo's canned agents (@window.leksahDemoAgents@) and their
-- panes.  There are no claude processes in the browser, so a canned agent's
-- pane is found by NAME: the canned terminal named after it
-- ("IDE.Web.DemoTerminals").  Natively 'demoShowAgent' is 'Nothing' and the
-- caller does the real thing.
module IDE.Web.DemoAgents
  ( demoShowAgent
  ) where

import Data.Text (Text)

#if defined(ghcjs_HOST_OS)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe)

import IDE.App (appUi, getGlobalApp)
import IDE.Reactive (modifyCell, readCell)
import IDE.Web.DemoTerminals (demoAgentName, demoTerminals)
import IDE.Web.Model
       (LeksahWindow(..), PaneContent(..), PaneKind(..), TabKey(..), WebUi(..))
import IDE.Web.RemoteTermRequest (requestLocalTerm)
#endif

-- | Bring up canned agent @sid@'s pane: the leksah window showing its
-- terminal — its own window, or the showcase window holding it as a view
-- leaf (whose focus moves to that leaf).  @Just False@ when the agent has no
-- pane; 'Nothing' natively.
demoShowAgent :: Text -> IO (Maybe Bool)
#if defined(ghcjs_HOST_OS)
demoShowAgent sid = fmap Just $ demoAgentName sid >>= \case
  Nothing -> return False
  Just name -> do
    terms <- demoTerminals
    mapp <- getGlobalApp
    case ([ t | (t, n) <- terms, n == name ], mapp) of
      (tsid : _, Just app) -> do
        let leafOf lw = listToMaybe
              [ l | (l, PaneContent (PaneView (TerminalKey t)) _) <- M.toList (lwPanes lw)
                  , t == tsid ]
        ui <- readCell (appUi app)
        case [ (i, leafOf lw) | (i, lw) <- M.toAscList (_leksahWindows ui)
                              , lwSession lw == Just tsid || isJust (leafOf lw) ] of
          [] -> return False
          ((i, mleaf) : _) -> do
            mapM_ (\l -> modifyCell (appUi app) $ \u ->
                     u { _leksahWindows = M.adjust (\lw -> lw { lwFocused = Just l }) i
                                                   (_leksahWindows u) })
                  mleaf
            requestLocalTerm i
            return True
      _ -> return False
#else
demoShowAgent _ = return Nothing
#endif
