{-# OPTIONS_GHC -fglasgow-exts #-}
module IDE.Core.State (
    IDEObject
,   IDEPaneC
,   IDEEditor
,   IDERef
,   IDEM
,   IDEAction
,   readIDE
,   modifyIDE
,   modifyIDE_
,   withIDE

,   module IDE.Core.Exception
) where

import GHC.IOBase hiding(BufferState)
import Control.Monad.Reader

import IDE.Core.Exception


class IDEObject o

class IDEObject o => IDEPaneC o

class IDEObject o => IDEEditor o

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


readIDE :: (IDE -> beta) -> IDEM beta
modifyIDE_ :: (IDE -> IO IDE) -> IDEM ()
modifyIDE :: (IDE -> IO (IDE,beta)) -> IDEM beta
withIDE :: (IDE -> IO alpha) -> IDEM alpha

