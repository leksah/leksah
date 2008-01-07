
module IDE.Core.State (
-- * IDE State
    IDERef
,   IDEM
,   IDEAction

) where

import GHC.IOBase
import Control.Monad.Reader

data IDE
--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

--
-- | A reader monad for a mutable reference to the IDE state
--
type IDEM = ReaderT (IDERef) IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
-- | which does not return a value
--
type IDEAction = IDEM ()


