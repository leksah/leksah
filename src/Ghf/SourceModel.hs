-----------------------------------------------------------------------------
--
-- Module      :  Ghf.SourceModel
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The source editor model part of GHF
--
-----------------------------------------------------------------------------------


module Ghf.SourceModel (
    selectSourceBuf
) where

import System.FilePath
import System.Directory
import Control.Monad.Reader

import Ghf.Core.State
import Ghf.SourceEditor

selectSourceBuf :: FilePath -> GhfM Bool
selectSourceBuf fp = do
    fpc <-  lift $canonicalizePath fp
    buffers <- allBuffers
    let buf = filter (\b -> case fileName b of
                                Just fn -> equalFilePath fn fpc
                                Nothing -> False) buffers
    case buf of
        hdb:tl -> do
            makeActive hdb
            return True
        otherwise -> do
            fe <- lift $doesFileExist fpc
            if fe
                then do
                    path <- standardSourcePanePath
                    newTextBuffer path (takeFileName fpc) (Just fpc)
                    message "opened new buffer"
                    return True
                else return False

