{-# OPTIONS_GHC -fglasgow-exts #-}
module IDE.Core.State (
    IDEObject
,   IDERef
,   IDEM
,   IDEAction
) where

import Data.IORef
import Control.Monad.Reader


class IDEObject o

data IDE
type IDERef = IORef IDE
type IDEM = ReaderT (IDERef) IO
type IDEAction = IDEM ()


