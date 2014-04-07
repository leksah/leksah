-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Types
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module IDE.Command.VCS.Types (
    --types
    VCSAction(..)
    ,askIDERef
    ,readIDE'
    ,reflectIDE'
) where
import Control.Monad.Reader
import Control.Applicative
import IDE.Core.Types
import IDE.Core.State

newtype VCSAction a = VCSAction (ReaderT (VCSConf,FilePath) IDEM a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (VCSConf,FilePath))

askIDERef :: VCSAction IDERef
askIDERef = VCSAction $ lift ask

--liftIDE :: ReaderT (VCSConf,FilePath) IDEM a
--liftIDE = VCSAction $ lift

readIDE' :: (IDE -> a) -> VCSAction a
readIDE' f = VCSAction $ lift $ readIDE f

reflectIDE' :: IDEM a -> IDERef -> IO a
reflectIDE' = reflectIDE
