-- SPDX-License-Identifier: Apache-2.0

-- | What a window's widgets are given: the services (for actions) and a
-- push-fed 'Dynamic' per model cell.  Created once near each window's
-- root by 'newCtx' — cell subscriptions are host-lived, so building them
-- at the root and passing the 'Ctx' down keeps a window at exactly one
-- subscription per cell, however many widgets read it.
module IDE.Web.Ctx
  ( Ctx(..)
  , newCtx
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Reflex (Dynamic, MonadHold, Reflex, TriggerEvent)

import IDE.App (App(..))
import IDE.Config (Config, configCell)
import IDE.Problems (problemsCell)
import IDE.Problems.Types (Problem)
import IDE.Reactive.Dyn (cellDyn)
import IDE.Web.Model (WebUi, WindowId)
import IDE.Workspace (Ws, wsCell)

data Ctx t = Ctx
    { cApp      :: App
    , cWindowId :: WindowId
    , cUi       :: Dynamic t WebUi
    , cWs       :: Dynamic t Ws
    , cCfg      :: Dynamic t Config
    , cProblems :: Dynamic t (Map Text [Problem])
    }

-- | Lift the app's cells into this window's reflex host.
newCtx
    :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m)
    => App -> WindowId -> m (Ctx t)
newCtx app wid = Ctx app wid
    <$> cellDyn (appUi app)
    <*> cellDyn (wsCell (appWorkspace app))
    <*> cellDyn (configCell (appConfig app))
    <*> cellDyn (problemsCell (appProblems app))
