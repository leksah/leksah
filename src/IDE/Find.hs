{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Find
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The toolbar for searching and replacing in a text buffer
--
-------------------------------------------------------------------------------

module IDE.Find (
    toggleFindbar
,   constructFindReplace
,   hideFindbar
,   showFindbar
,   focusFindEntry
,   editFindInc
,   editGotoLine
,   FindState(..)
,   getFindState
,   setFindState
,   editFind

,   showToolbar
,   hideToolbar
,   toggleToolbar


) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Gdk.Events
import Data.Maybe
import Control.Monad.Reader
import Data.List

import IDE.Core.State
import IDE.Pane.SourceBuffer
import Data.Char (toUpper)

data FindState = FindState {
            entryStr        ::    String
        ,   entryHist       ::    [String]
        ,   replaceStr      ::    String
        ,   replaceHist     ::    [String]
        ,   caseSensitive   ::    Bool
        ,   entireWord      ::    Bool
        ,   wrapAround      ::    Bool
        ,   backward        ::    Bool
        ,   lineNr          ::    Int}
    deriving(Eq,Ord,Show,Read)

getFindState :: IDEM FindState
getFindState = do
    fb            <- readIDE findbar
    liftIO $ do
        lineNr        <- getLineEntry fb >>= (\e -> spinButtonGetValueAsInt (castToSpinButton e))
        replaceStr    <- getReplaceEntry fb >>= (\e -> entryGetText (castToEntry e))
        entryStr      <- getFindEntry fb >>=  (\e -> entryGetText (castToEntry e))
        entireWord    <- getEntireWord fb
        wrapAround    <- getWrapAround fb
        caseSensitive <- getCaseSensitive fb
        backward      <- getBackward fb
        return FindState{
                entryStr        =   entryStr
            ,   entryHist       =   []
            ,   replaceStr      =   replaceStr
            ,   replaceHist     =   []
            ,   caseSensitive   =   caseSensitive
            ,   entireWord      =   entireWord
            ,   wrapAround      =   wrapAround
            ,   backward        =   backward
            ,   lineNr          =   lineNr}

setFindState :: FindState -> IDEAction
setFindState fs = do
    fb            <- readIDE findbar
    liftIO $ do
        getLineEntry fb >>= (\e -> spinButtonSetValue (castToSpinButton e) (fromIntegral (lineNr fs)))
        getReplaceEntry fb >>= (\e -> entrySetText (castToEntry e) (replaceStr fs))
        getFindEntry fb >>=  (\e -> entrySetText (castToEntry e) (entryStr fs))
        setEntireWord fb (entireWord fs)
        setWrapAround fb (wrapAround fs)
        setCaseSensitive fb (caseSensitive fs)
        setBackward fb (backward fs)


hideToolbar :: IDEAction
hideToolbar = do
    mbtb <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> return (ide{toolbarVisible = False}))
            liftIO $ widgetHideAll tb

showToolbar :: IDEAction
showToolbar = do
    mbtb <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> return (ide{toolbarVisible = True}))
            liftIO $ widgetShowAll tb

toggleToolbar :: IDEAction
toggleToolbar = do
    toolBarVisible' <- readIDE toolbarVisible
    if toolBarVisible'
        then hideToolbar
        else showToolbar

hideFindbar :: IDEAction
hideFindbar = do
    fb <- readIDE findbar
    modifyIDE_ (\ide -> return (ide{findbarVisible = False}))
    liftIO $ widgetHideAll fb

showFindbar :: IDEAction
showFindbar = do
    fb <- readIDE findbar
    modifyIDE_ (\ide -> return (ide{findbarVisible = True}))
    liftIO $ widgetShowAll fb

focusFindEntry :: IDEAction
focusFindEntry = do
    fb              <- readIDE findbar
    findbarVisible' <- readIDE findbarVisible
    when findbarVisible' $ liftIO $ do
        entry <- getFindEntry fb
        widgetGrabFocus entry



toggleFindbar :: IDEAction
toggleFindbar = do
    findbarVisible' <- readIDE findbarVisible
    if findbarVisible'
        then hideFindbar
        else showFindbar

constructFindReplace :: Toolbar -> IDEM (Toolbar)
constructFindReplace toolbar = reifyIDE $ \ ideR   -> do
    closeButton <- toolButtonNewFromStock "gtk-close"
    toolbarInsert toolbar closeButton 0

    sep0 <- separatorToolItemNew
    separatorToolItemSetDraw sep0 False
    toolItemSetExpand sep0 True
    toolbarInsert toolbar sep0 0

    spinTool <- toolItemNew
    spinL <- spinButtonNewWithRange 1.0 1000.0 10.0
    widgetSetName spinL "gotoLineEntry"
    containerAdd spinTool spinL
    widgetSetName spinTool "gotoLineEntryTool"
    toolbarInsert toolbar spinTool 0

    labelTool3 <- toolItemNew
    label3 <- labelNew (Just "Goto Line :")
    containerAdd labelTool3 label3
    toolbarInsert toolbar labelTool3 0

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar sep1 0

    replaceAllButton <- toolButtonNew (Nothing :: Maybe Widget) (Just "Replace All")
    toolbarInsert toolbar replaceAllButton 0

    replaceButton <- toolButtonNewFromStock "gtk-find-and-replace"
    toolbarInsert toolbar replaceButton 0

    replaceTool <- toolItemNew
    rentry <- entryNew
    widgetSetName rentry "replaceEntry"
    containerAdd replaceTool rentry
    widgetSetName replaceTool "replaceTool"
    toolbarInsert toolbar replaceTool 0

    labelTool2 <- toolItemNew
    label2 <- labelNew (Just "Replace: ")
    containerAdd labelTool2 label2
    toolbarInsert toolbar labelTool2 0

    sep2 <- separatorToolItemNew
    toolbarInsert toolbar sep2 0

    backwardButton <- toggleToolButtonNew
    toolButtonSetLabel backwardButton (Just "Backward")
    widgetSetName backwardButton "backwardButton"
    toolbarInsert toolbar backwardButton 0

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just "Wrap around")
    widgetSetName wrapAroundButton "wrapAroundButton"
    toolbarInsert toolbar wrapAroundButton 0

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just "Entire word")
    widgetSetName entireWordButton "entireWordButton"
    toolbarInsert toolbar entireWordButton 0

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just "Case sensitive")
    widgetSetName caseSensitiveButton "caseSensitiveButton"
    toolbarInsert toolbar caseSensitiveButton 0

    findButton <- toolButtonNewFromStock "gtk-find"
    toolbarInsert toolbar findButton 0

    entryTool <- toolItemNew
    entry <- entryNew
    widgetSetName entry "searchEntry"
    containerAdd entryTool entry
    widgetSetName entryTool "searchEntryTool"
    toolbarInsert toolbar entryTool 0

    labelTool <- toolItemNew
    label <- labelNew (Just "Search: ")
    containerAdd labelTool label
    toolbarInsert toolbar labelTool 0

    findButton `onToolButtonClicked` (doSearch toolbar Forward ideR  )

    entry `afterInsertText` (\t i -> do
        doSearch toolbar Insert ideR
        return i)
    entry `afterDeleteText` (\ _ _ -> doSearch toolbar Delete ideR  )
    entry `afterKeyPress`  (\ e -> case e of
        k@(Key _ _ _ _ _ _ _ _ _ _)
            | eventKeyName k == "Down"                 -> do
                doSearch toolbar Forward ideR
                return True
            | eventKeyName k == "Up"                   -> do
                doSearch toolbar Backward ideR
                return True
            | eventKeyName k == "Escape"               -> do
                getOut ideR
                return True
            | otherwise                ->  return True
        _                              ->  return True)
    entry `onEntryActivate` (doSearch toolbar Forward ideR)
    replaceButton `onToolButtonClicked` replace toolbar Forward ideR

    replaceAllButton `onToolButtonClicked` replaceAll toolbar Forward ideR


    spinL `afterFocusIn` (\ _ -> (reflectIDE (inActiveBufContext True $ \_ gtkbuf currentBuffer _ -> do
        max <- textBufferGetLineCount gtkbuf
        spinButtonSetRange spinL 1.0 (fromIntegral max)
        return True) ideR  ))


    spinL `afterEntryActivate` (reflectIDE (inActiveBufContext () $ \_ gtkbuf currentBuffer _ -> do
        line <- spinButtonGetValueAsInt spinL
        iter <- textBufferGetStartIter gtkbuf
        textIterSetLine iter (line - 1)
        textBufferPlaceCursor gtkbuf iter
        textViewScrollToIter (sourceView currentBuffer) iter 0.2 Nothing
        return ()) ideR  )

    closeButton `onToolButtonClicked` do
        reflectIDE hideFindbar ideR

    set toolbar [ toolbarChildHomogeneous spinTool := False ]
    set toolbar [ toolbarChildHomogeneous wrapAroundButton := False ]
    set toolbar [ toolbarChildHomogeneous entireWordButton := False ]
    set toolbar [ toolbarChildHomogeneous caseSensitiveButton := False ]
    set toolbar [ toolbarChildHomogeneous backwardButton := False ]
    set toolbar [ toolbarChildHomogeneous replaceAllButton := False ]
    set toolbar [ toolbarChildHomogeneous labelTool  := False ]
    set toolbar [ toolbarChildHomogeneous labelTool2 := False ]
    set toolbar [ toolbarChildHomogeneous labelTool3 := False ]

    return toolbar
        where getOut = reflectIDE $ do
                            hideFindbar
                            mbbuf <- maybeActiveBuf
                            case mbbuf of
                                Nothing  -> return ()
                                Just buf -> do
                                    liftIO $ widgetGrabFocus (sourceView buf)
                                    return ()


doSearch :: Toolbar -> SearchHint -> IDERef -> IO ()
doSearch fb hint ideR   = do
    entry         <- getFindEntry fb
    search        <- entryGetText (castToEntry entry)
    entireWord    <- getEntireWord fb
    caseSensitive <- getCaseSensitive fb
    wrapAround    <- getWrapAround fb
    res           <- reflectIDE (editFind entireWord caseSensitive wrapAround search "" hint) ideR
    if res || null search
        then do
            widgetModifyBase entry StateNormal white
            widgetModifyText entry StateNormal black
        else do
            widgetModifyBase entry StateNormal red
            widgetModifyText entry StateNormal white
    return ()

replace :: Toolbar -> SearchHint -> IDERef -> IO ()
replace fb hint ideR   =  do
    entry          <- getFindEntry fb
    search         <- entryGetText (castToEntry entry)
    rentry         <- getReplaceEntry fb
    replace        <- entryGetText (castToEntry rentry)
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    wrapAround     <- getWrapAround fb
    found <- reflectIDE (editReplace entireWord caseSensitive wrapAround search replace hint)
                ideR
    return ()


replaceAll :: Toolbar -> SearchHint -> IDERef -> IO ()
replaceAll fb hint ideR   =  do
    entry          <- getFindEntry fb
    search         <- entryGetText (castToEntry entry)
    rentry         <- getReplaceEntry fb
    replace        <- entryGetText (castToEntry rentry)
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    wrapAround     <- getWrapAround fb
    found <- reflectIDE (editReplaceAll entireWord caseSensitive wrapAround search replace hint)
                ideR
    return ()

editFind :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround search dummy hint =
    let searchflags = (if caseSensitive then [] else [toEnum 4]) ++ [toEnum 1,toEnum 2] in
    if null search
        then return False
        else inActiveBufContext' False $ \_ gtkbuf currentBuffer _ -> liftIO $ do
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

editReplace :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround search replace hint =
    editReplace' entireWord caseSensitive wrapAround search replace hint True

editReplace' :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround search replace hint mayRepeat =
    inActiveBufContext' False $ \_ gtkbuf currentBuffer _ -> do
        startMark <- liftIO $ textBufferGetInsert gtkbuf
        iter      <- liftIO $ textBufferGetIterAtMark gtkbuf startMark
        iter2     <- liftIO $ textIterCopy iter
        liftIO $ textIterForwardChars iter2 (length search)
        str1      <- liftIO $ textIterGetText iter iter2
        if compare str1 search caseSensitive
            then do
                liftIO $ textBufferDelete gtkbuf iter iter2
                liftIO $ textBufferInsert gtkbuf iter replace
                editFind entireWord caseSensitive wrapAround search "" hint
            else do
                r <- editFind entireWord caseSensitive wrapAround search "" hint
                if r
                    then editReplace' entireWord caseSensitive wrapAround search
                            replace hint False
                    else return False
    where
        compare s1 s2 True = s1 == s2
        compare s1 s2 False = map toUpper s1 == map toUpper s2

editReplaceAll :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplaceAll entireWord caseSensitive wrapAround search replace hint = do
    res <- editReplace' entireWord caseSensitive False search replace hint True
    if res
        then editReplaceAll entireWord caseSensitive False search replace hint
        else return False

red = Color 640000 10000 10000
white = Color 64000 64000 64000
black = Color 0 0 0

editFindInc :: SearchHint -> IDEAction
editFindInc hint = do
    ideR <- ask
    fb <- readIDE findbar
    case hint of
        Initial -> inActiveBufContext' () $ \_ gtkbuf currentBuffer _ -> liftIO $ do
            hasSelection <- textBufferHasSelection gtkbuf
            when hasSelection $ do
                (i1,i2)   <- textBufferGetSelectionBounds gtkbuf
                text      <- textBufferGetText gtkbuf i1 i2 False
                findEntry <- getFindEntry fb
                entrySetText (castToEntry findEntry) text
                return ()
        _ -> return ()
    showFindbar
    reifyIDE $ \ideR   -> do
        entry <- getFindEntry fb
        widgetGrabFocus entry
        case hint of
            Forward  -> doSearch fb Forward ideR
            Backward -> doSearch fb Backward ideR
            _        -> return ()

editGotoLine :: IDEAction
editGotoLine = do
    showFindbar
    fb <- readIDE findbar
    entry <- liftIO $ getLineEntry fb
    liftIO $ widgetGrabFocus entry

getLineEntry, getReplaceEntry, getFindEntry :: Toolbar -> IO Widget
getLineEntry    = getWidget "gotoLineEntryTool"
getReplaceEntry = getWidget "replaceTool"
getFindEntry    = getWidget "searchEntryTool"

getWidget :: String -> Toolbar -> IO Widget
getWidget str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> do
            mbw <- binGetChild (castToBin w)
            case mbw of
                Nothing -> throwIDE "Find>>getWidget not found(1)"
                Just w -> return w
        _   -> throwIDE "Find>>getWidget not found(2)"

getEntireWord, getWrapAround, getCaseSensitive :: Toolbar -> IO Bool
getEntireWord    = getSelection "entireWordButton"
getWrapAround    = getSelection "wrapAroundButton"
getCaseSensitive = getSelection "caseSensitiveButton"
getBackward      = getSelection "backwardButton"

getSelection :: String -> Toolbar -> IO Bool
getSelection str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonGetActive (castToToggleToolButton w)
        _   -> throwIDE "Find>>getIt widget not found"

setEntireWord, setWrapAround, setCaseSensitive, setBackward :: Toolbar -> Bool -> IO ()
setEntireWord    = setSelection "entireWordButton"
setWrapAround    = setSelection "wrapAroundButton"
setCaseSensitive = setSelection "caseSensitiveButton"
setBackward      = setSelection "backwardButton"

setSelection :: String -> Toolbar -> Bool ->  IO ()
setSelection str tb bool = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonSetActive (castToToggleToolButton w) bool
        _   -> throwIDE "Find>>getIt widget not found"
