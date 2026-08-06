{-# OPTIONS_GHC  #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.GUIHistory
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Recording the history for going back and forth
--
---------------------------------------------------------------------------------


module IDE.GUIHistory (
    recordHistory
,   historyBack
,   historyForward
,   withoutRecordingDo
) where

import Prelude ()
import Prelude.Compat

import Control.Lens ((.~), _Just, pre)
import Control.Monad (unless)

import IDE.Core.State
       (IDEAction, readIDE, ideGtk, modifyIDE_, triggerEventIDE_,
        IDEEvent(..), SensitivityMask(..))
import IDE.Gtk.State
       (GUIHistory, GUIHistory'(..), IDEPane(..), guiHistory,
        withoutRecordingDo, deactivatePane, mbPaneFromName, makeActive)
import IDE.Pane.Modules
import IDE.Pane.Info

recordHistory :: GUIHistory -> IDEAction
recordHistory entry =
    readIDE (pre $ ideGtk . _Just . guiHistory) >>= mapM_ (\(b,l,n) ->
        unless (b || (n >= 0 && fst entry == fst (l !! n))) $ do
            modifyIDE_ $ ideGtk . _Just . guiHistory .~ (b,entry : drop n l,0)
            triggerEventIDE_ (Sensitivity
                [(SensitivityForwardHist,False),(SensitivityBackwardHist,0 < length (drop n l) - 1)]))
            -- liftIO $ putStrLn $ "record n : " ++ show 0 -- ++ " hist: " ++ show (entry:(drop n l))

historyBack :: IDEAction
historyBack =
    readIDE (pre $ ideGtk . _Just . guiHistory) >>= mapM_ (\(b,list,index) ->
        case (list,index) of
             (_,-1)                 ->   return ()
             (l,n)  | n + 1 >= length l  ->   return ()
                    | otherwise     ->   do
                withoutRecordingDo (activateHistory (snd (l !! n)))
                modifyIDE_ $ ideGtk . _Just . guiHistory .~ (b,l, n + 1)
                triggerEventIDE_ (Sensitivity
                    [(SensitivityForwardHist,(n + 1) > 0),(SensitivityBackwardHist,(n + 1) < length l - 1)]))
                -- liftIO $ putStrLn $ "back n : " ++ show (n + 1) -- ++ " hist: " ++ show l


historyForward :: IDEAction
historyForward =
    readIDE (pre $ ideGtk . _Just . guiHistory) >>= mapM_ (\(b,list,index) ->
        case (list,index) of
            (l,n)  | n < 1      ->  return ()
                   | otherwise  ->  do
                withoutRecordingDo (activateHistory (fst (l !! n)))
                modifyIDE_ $ ideGtk . _Just . guiHistory .~ (b,l, n - 1)
                triggerEventIDE_ (Sensitivity
                    [(SensitivityForwardHist,(n - 1) > 0),(SensitivityBackwardHist,(n - 1) < length l - 1)]))
                -- liftIO $ putStrLn $ "forward n : " ++ show (n - 1) -- ++ " hist: " ++ show l

activateHistory :: GUIHistory' -> IDEAction
activateHistory (ModuleSelected s1 s2) =
    --  liftIO $ putStrLn $ "activate with module selected " ++ show s1 ++ " " ++ show s2
    replaySelHistory s1 s2
activateHistory (ScopeSelected bl sc) =
    -- liftIO $ putStrLn $ "activate with scope selected " ++ show ms
    replayScopeHistory bl sc
activateHistory (InfoElementSelected descr) =
    -- liftIO $ putStrLn $ "activate with  " ++ show ms
    replayInfoHistory descr
activateHistory (PaneSelected mbPaneName)  =
    -- liftIO $ putStrLn $ "activate with " ++ show ms
    case mbPaneName of
        Nothing ->  withoutRecordingDo deactivatePane
        Just paneName' -> do
            mbPane <- mbPaneFromName paneName'
            case mbPane of
                Nothing -> return ()
                Just (PaneC p)  -> do
                    makeActive p
                    return ()



