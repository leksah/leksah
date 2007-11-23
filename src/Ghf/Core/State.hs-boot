
module Ghf.Core.State (
-- * IDE State
    GhfRef
,   GhfM
,   GhfAction

) where

import GHC.IOBase
import Control.Monad.Reader

data Ghf
--
-- | A mutable reference to the IDE state
--
type GhfRef = IORef Ghf

--
-- | A reader monad for a mutable reference to the IDE state
--
type GhfM = ReaderT (GhfRef) IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
-- | which does not return a value
--
type GhfAction = GhfM ()


