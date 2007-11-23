{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.RecoverPanes
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-------------------------------------------------------------------------------
module Ghf.RecoverPanes (
) where

import Data.Maybe

import Ghf.Core.State
import Ghf.Log
import Ghf.SourceEditor
import Ghf.ModulesPane
import Ghf.InfoPane

instance RecoverablePane GhfPane PaneState where
    saveState p | isIt BufferCasting p
        =   do  st <- saveState (fromJust (downCast BufferCasting p))
                case st of
                    Nothing -> return Nothing
                    Just s  -> return (Just (BufferSt s))
    saveState p | isIt InfoCasting p
        =   do  st <- saveState (fromJust (downCast InfoCasting p))
                case st of
                    Nothing -> return Nothing
                    Just s  -> return (Just (InfoSt s))
    saveState p | isIt LogCasting p
        =   do  st <- saveState (fromJust (downCast LogCasting p))
                case st of
                    Nothing -> return Nothing
                    Just s  -> return (Just (LogSt s))
    saveState p | isIt ModulesCasting p
        =   do  st <- saveState (fromJust (downCast ModulesCasting p))
                case st of
                    Nothing -> return Nothing
                    Just s  -> return (Just (ModulesSt s))

    recoverState pp (BufferSt st)
        =   do  p <- (recoverState pp st)
                case p of
                    Nothing -> return Nothing
                    Just (p :: GhfBuffer)  -> return (Just (PaneC p))
    recoverState pp (InfoSt st)
        =   do  p <- (recoverState pp st)
                case p of
                    Nothing -> return Nothing
                    Just (p :: GhfInfo)  -> return (Just (PaneC p))
    recoverState pp (LogSt st)
        =   do  p <- (recoverState pp st)
                case p of
                    Nothing -> return Nothing
                    Just (p :: GhfLog)  -> return (Just (PaneC p))
    recoverState pp (ModulesSt st)
        =   do  p <- (recoverState pp st)
                case p of
                    Nothing -> return Nothing
                    Just (p :: GhfModules)   -> return (Just (PaneC p))

