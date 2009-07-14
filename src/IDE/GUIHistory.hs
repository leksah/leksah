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

import IDE.Core.State
import IDE.Pane.Modules
import IDE.Pane.Info
import Control.Monad.Reader

recordHistory :: GUIHistory -> IDEAction
recordHistory entry = do
    (b,l,n) <- readIDE guiHistory
    if b || (n >= 0 && fst entry == fst (l !! n))
        then return ()
        else do
            modifyIDE_ (\ide -> ide{guiHistory = (b,entry:(drop n l),0)})
            triggerEventIDE (Sensitivity
                [(SensitivityForwardHist,False),(SensitivityBackwardHist,0 < length (drop n l) - 1)])
            return ()
            -- liftIO $ putStrLn $ "record n : " ++ show 0 -- ++ " hist: " ++ show (entry:(drop n l))

historyBack :: IDEAction
historyBack = do
    (b,list,index) <- readIDE guiHistory
    case (list,index) of
         (_,-1)                 ->   return ()
         (l,n)  | n + 1 >= length l  ->   return ()
                | otherwise     ->   do
            withoutRecordingDo (activateHistory (snd (l !! n)))
            modifyIDE_ (\ide -> ide{guiHistory = (b,l, n + 1)})
            triggerEventIDE (Sensitivity
                [(SensitivityForwardHist,(n + 1) > 0),(SensitivityBackwardHist,(n + 1) < (length l) - 1)])
            return ()
            -- liftIO $ putStrLn $ "back n : " ++ show (n + 1) -- ++ " hist: " ++ show l


historyForward :: IDEAction
historyForward = do
    (b,list,index) <- readIDE guiHistory
    case (list,index) of
        (l,n)  | n < 1      ->  return ()
               | otherwise  ->  do
            withoutRecordingDo (activateHistory (fst (l !! n)))
            modifyIDE_ (\ide -> ide{guiHistory = (b,l, n - 1)})
            triggerEventIDE (Sensitivity
                [(SensitivityForwardHist,(n - 1) > 0),(SensitivityBackwardHist,(n - 1) < (length l) - 1)])
            return ()
            -- liftIO $ putStrLn $ "forward n : " ++ show (n - 1) -- ++ " hist: " ++ show l

activateHistory :: GUIHistory' -> IDEAction
activateHistory ms@(ModuleSelected s1 s2) = do
    --  liftIO $ putStrLn $ "activate with module selected " ++ show s1 ++ " " ++ show s2
    replaySelHistory s1 s2
activateHistory ms@(ScopeSelected bl sc) = do
    -- liftIO $ putStrLn $ "activate with scope selected " ++ show ms
    replayScopeHistory bl sc
activateHistory ms@(InfoElementSelected descr) = do
    -- liftIO $ putStrLn $ "activate with  " ++ show ms
    replayInfoHistory descr
activateHistory ms@(PaneSelected mbPaneName)  = do
    -- liftIO $ putStrLn $ "activate with " ++ show ms
    case mbPaneName of
        Nothing ->  withoutRecordingDo deactivatePane
        Just paneName -> do
            mbPane <- mbPaneFromName paneName
            case mbPane of
                Nothing -> return ()
                Just (PaneC p)  -> do
                    makeActive p
                    return ()



