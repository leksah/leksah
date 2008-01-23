{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.FindPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The pane of ide for searching in a text buffer
--
-------------------------------------------------------------------------------

module IDE.FindPane (
    IDEFind(..)
,   FindAction(..)
,   FindState(..)
,   editFind
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView
import Data.Maybe
import Control.Monad.Reader
import Data.List

import IDE.Core.State
import IDE.Framework.ViewFrame
import IDE.SourceEditor

-------------------------------------------------------------------------------
--
-- * Interface
--

class FindAction alpha where
    doFind              ::   alpha
    editFindInc         ::   SearchHint -> alpha
    editFindKey         ::   Event -> alpha
    editGotoLine        ::   alpha
    editGotoLineEnd     ::   alpha
    editGotoLineKey     ::   Event -> alpha

instance FindAction IDEAction where
    doFind              =   doFind'
    editFindInc         =   editFindInc'
    editFindKey         =   editFindKey'
    editGotoLine        =   editGotoLine'
    editGotoLineEnd     =   editGotoLineEnd'
    editGotoLineKey     =   editGotoLineKey'

instance IDEObject IDEFind

instance CastablePane IDEFind where
    casting _               =   FindCasting
    downCast _ (PaneC a)    =   case casting a of
                                    FindCasting  -> Just a
                                    _               -> Nothing


instance Recoverable FindState where
    toPaneState a           =   FindSt a

instance Pane IDEFind
    where
    primPaneName _  =   "Find"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . findBox
    paneId b        =   "*Find"
    makeActive p    =   throwIDE "don't activate find bar"
    close pane      =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                lift $notebookRemovePage nb i
                removePaneAdmin pane


instance RecoverablePane IDEFind FindState where
    saveState p     =   do
        mbFind <- getPane FindCasting
        case mbFind of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                return (Just (StateC FindState))
    recoverState pp FindState  =  do
            nb          <-  getNotebook pp
            initFind pp nb

-------------------------------------------------------------------------------
--
-- * Implementation
--

doFind' :: IDEAction
doFind' = do
    find :: IDEFind <- getFind
    lift $ bringPaneToFront find
    lift $ widgetGrabFocus (getTopWidget find)


getFind :: IDEM IDEFind
getFind = do
    mbFind <- getPane FindCasting
    case mbFind of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initFind pp nb
            mbFind <- getPane FindCasting
            case mbFind of
                Nothing ->  throwIDE "Can't init find pane"
                Just m  ->  return m
        Just m ->   return m

initFind :: PanePath -> Notebook -> IDEAction
initFind panePath nb = do
    ideR        <-  ask
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  lift $ do
            hBox <- hBoxNew False 10
            bBox <- hButtonBoxNew

            entry <- entryNew
            widgetSetName entry "searchEntry"

            caseSensitiveButton <- toggleButtonNew
            buttonSetLabel caseSensitiveButton "Case sensitive"
            widgetSetName caseSensitiveButton "caseSensitiveButton"

            entireWordButton <- toggleButtonNew
            buttonSetLabel entireWordButton "Entire word"
            widgetSetName entireWordButton "entireWordButton"

            wrapAroundButton <- toggleButtonNew
            buttonSetLabel wrapAroundButton "Wrap around"
            toggleButtonSetActive wrapAroundButton True
            widgetSetName wrapAroundButton "wrapAroundButton"

            spinL <- spinButtonNewWithRange 1.0 1000.0 10.0
            widgetSetName spinL "gotoLineEntry"

            boxPackStart hBox entry PackNatural 10
            boxPackStart bBox caseSensitiveButton PackNatural 10
            boxPackStart bBox entireWordButton PackNatural 10
            boxPackStart bBox wrapAroundButton PackNatural 10
            boxPackStart hBox bBox PackNatural 10
            boxPackStart hBox spinL PackNatural 10

            entry `afterInsertText` (\ _ _ -> do
                runReaderT ((editFindInc Insert)::IDEAction) ideR
                t <- entryGetText entry
                return (length t))
            entry `afterDeleteText` (\ _ _ -> do runReaderT ((editFindInc Delete)::IDEAction)
                                                    ideR; return ())
            entry `afterKeyPress`  (\ e -> do runReaderT ((editFindKey e)::IDEAction)
                                                    ideR; return True)
            spinL `afterKeyPress`  (\ e -> do runReaderT ((editGotoLineKey e)::IDEAction)
                                                    ideR; return True)
            spinL `afterEntryActivate` runReaderT editGotoLineEnd ideR
            spinL `afterFocusOut` (\ _ -> do runReaderT (editGotoLineEnd::IDEAction) ideR;
                                                        return False)
            let find = IDEFind hBox caseSensitiveButton wrapAroundButton entireWordButton
                                    spinL entry
            notebookInsertOrdered nb hBox (paneName find)
            widgetShowAll hBox
            return (find,[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (findBox buf)

getFindEntry' :: IDEFind -> Entry
getFindEntry' f =  findEntry f

getCaseSensitive' :: IDEFind ->  ToggleButton
getCaseSensitive' f = caseSensitive f

getWrapAround' :: IDEFind ->  ToggleButton
getWrapAround' f = wrapAround f

getEntireWord' :: IDEFind ->  ToggleButton
getEntireWord' f = entireWord f

getGotoLineSpin' :: IDEFind ->  SpinButton
getGotoLineSpin' f = gotoLine f

red = Color 640000 10000 10000
white = Color 64000 64000 64000
black = Color 0 0 0

{-- can't be used currently becuase of an export error
  toEnum 1 = SourceSearchVisibleOnly
  toEnum 2 = SourceSearchTextOnly
  toEnum 4 = SourceSearchCaseInsensitive
--}


-- | Keys for searching
editFindKey' :: Event -> IDEAction
editFindKey' k@(Key _ _ _ _ _ _ _ _ _ _)
    | eventKeyName k == "Down" =
        editFindInc Forward
    | eventKeyName k == "Up" =
        editFindInc Backward
    | eventKeyName k == "Escape" = do
        --entry   <- getFindEntry
        inBufContext' () $ \_ gtkbuf currentBuffer _ -> lift $ do
            i1 <- textBufferGetStartIter gtkbuf
            i2 <- textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "found" i1 i2
            startMark <- textBufferGetInsert gtkbuf
            st1 <- textBufferGetIterAtMark gtkbuf startMark
            textBufferPlaceCursor gtkbuf st1
            widgetGrabFocus $ sourceView currentBuffer
    | otherwise = return ()
editFindKey' _ = return ()

editFindInc' :: SearchHint -> IDEAction
editFindInc' hint = do
    find :: IDEFind  <-  getFind
    let entry =  getFindEntry' find
    lift $ widgetGrabFocus entry
    lift $ bringPaneToFront find
    when (hint == Initial) $ lift $ editableSelectRegion entry 0 (-1)
    search  <- lift $entryGetText entry
    if null search
        then return ()
        else do
            let caseSensitiveW =  getCaseSensitive' find
            caseSensitive <- lift $toggleButtonGetActive caseSensitiveW
            let entireWButton =  getEntireWord' find
            entireW <- lift $toggleButtonGetActive entireWButton
            let wrapAroundButton =  getWrapAround' find
            wrapAround <- lift $toggleButtonGetActive wrapAroundButton
            res <- editFind entireW caseSensitive wrapAround search "" hint
            if res || null search
                then lift $do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else lift $do
                    widgetModifyBase entry StateNormal red
                    widgetModifyText entry StateNormal white
            lift $do
                widgetGrabFocus entry
                editableSelectRegion entry 0 (-1)


editFind :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround search dummy hint =
    let searchflags = (if caseSensitive then [] else [toEnum 4]) ++ [toEnum 1,toEnum 2] in
    if null search
        then return False
        else inBufContext' False $ \_ gtkbuf currentBuffer _ -> lift $ do
            i1 <- textBufferGetStartIter gtkbuf
            i2 <- textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "found" i1 i2
            startMark <- textBufferGetInsert gtkbuf
            st1 <- textBufferGetIterAtMark gtkbuf startMark
            mbsr2 <-
                if hint == Backward
                    then do
                        textIterBackwardChar st1
                        textIterBackwardChar st1
                        mbsr <- backSearch st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do backSearch i2 search searchflags entireWord searchflags
                                    else return Nothing
                            Just (start,end) -> return (Just (start,end))
                    else do
                        if hint == Forward
                            then textIterForwardChar st1
                            else return True
                        mbsr <- forwardSearch st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do forwardSearch i1 search searchflags entireWord searchflags
                                    else return Nothing
                            Just (start,end) -> return (Just (start,end))
            case mbsr2 of
                Just (start,end) -> do --found
                    --widgetGrabFocus sourceView
                    textViewScrollToIter (sourceView currentBuffer) start 0.2 Nothing
                    textBufferApplyTagByName gtkbuf "found" start end
                    textBufferPlaceCursor gtkbuf start
                    return True
                Nothing -> return False
    where
        backSearch iter string flags entireWord searchflags = do
            mbsr <- sourceIterBackwardSearch iter search searchflags Nothing
            case mbsr of
                Nothing -> return Nothing
                Just (iter1,iter2) ->
                    if entireWord
                        then do
                            b1 <- textIterStartsWord iter1
                            b2 <- textIterEndsWord iter2
                            if b1 && b2 then return $Just (iter1,iter2) else return Nothing
                        else return (Just (iter1,iter2))
        forwardSearch iter string flags entireWord searchflags = do
            mbsr <- sourceIterForwardSearch iter search searchflags Nothing
            case mbsr of
                Nothing -> return Nothing
                Just (iter1,iter2) ->
                    if entireWord
                        then do
                            b1 <- textIterStartsWord iter1
                            b2 <- textIterEndsWord iter2
                            if b1 && b2 then return $Just (iter1,iter2) else return Nothing
                        else return $Just (iter1,iter2)

editGotoLine' :: IDEAction
editGotoLine' = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    find :: IDEFind        <-  getFind
    let spin    =   getGotoLineSpin' find
    lift $ bringPaneToFront find
    lift $do
        max <- textBufferGetLineCount gtkbuf
        spinButtonSetRange spin 1.0 (fromIntegral max)
        widgetGrabFocus spin

editGotoLineKey' :: Event -> IDEAction
editGotoLineKey' k@(Key _ _ _ _ _ _ _ _ _ _)
    | eventKeyName k == "Escape"  =
        inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
            find :: IDEFind <- getFind
            let spin =  getGotoLineSpin' find
            lift $ do
                widgetGrabFocus $ sourceView currentBuffer
    | otherwise = return ()
editGotoLineKey' _ = return ()

editGotoLineEnd' :: IDEAction
editGotoLineEnd' = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    find :: IDEFind   <-  getFind
    let spin =  getGotoLineSpin' find
    lift $ bringPaneToFront find
    lift $ do
        line <- spinButtonGetValueAsInt spin
        iter <- textBufferGetStartIter gtkbuf
        textIterSetLine iter (line - 1)
        textBufferPlaceCursor gtkbuf iter
        textViewScrollToIter (sourceView currentBuffer) iter 0.2 Nothing
        widgetGrabFocus $ sourceView currentBuffer




