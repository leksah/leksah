{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Find
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portablea
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

import IDE.Core.State
import IDE.Pane.SourceBuffer
import Data.Char (toUpper)
import Data.Char (toLower)
import Text.Regex.Posix.String (compile)
import Text.Regex.Posix
    (Regex(..), compNewline, compExtended, execBlank, compIgnoreCase)
import Data.Bits ((.|.))
import Data.List (find, stripPrefix, isPrefixOf)
import Data.Array ((!))
import Text.Regex.Base.RegexLike (matchAll)
import Text.Regex (subRegex)


data FindState = FindState {
            entryStr        ::    String
        ,   entryHist       ::    [String]
        ,   replaceStr      ::    String
        ,   replaceHist     ::    [String]
        ,   caseSensitive   ::    Bool
        ,   entireWord      ::    Bool
        ,   wrapAround      ::    Bool
        ,   backward        ::    Bool
        ,   regex           ::    Bool
        ,   lineNr          ::    Int}
    deriving(Eq,Ord,Show,Read)

getFindState :: IDEM FindState
getFindState = do
    (fb,ls) <- needFindbar
    liftIO $ do
        lineNr        <- getLineEntry fb >>= (\e -> spinButtonGetValueAsInt (castToSpinButton e))
        replaceStr    <- getReplaceEntry fb >>= (\e -> entryGetText (castToEntry e))
        entryStr      <- getFindEntry fb >>=  (\e -> entryGetText (castToEntry e))
        entryHist     <- listStoreToList ls
        entireWord    <- getEntireWord fb
        wrapAround    <- getWrapAround fb
        caseSensitive <- getCaseSensitive fb
        backward      <- getBackward fb
        regex         <- getRegex fb
        return FindState{
                entryStr        =   entryStr
            ,   entryHist       =   entryHist
            ,   replaceStr      =   replaceStr
            ,   replaceHist     =   []
            ,   caseSensitive   =   caseSensitive
            ,   entireWord      =   entireWord
            ,   wrapAround      =   wrapAround
            ,   backward        =   backward
            ,   regex           =   regex
            ,   lineNr          =   lineNr}

setFindState :: FindState -> IDEAction
setFindState fs = do
    (fb,ls)      <- needFindbar
    liftIO $ do
        getLineEntry fb >>= (\e -> spinButtonSetValue (castToSpinButton e) (fromIntegral (lineNr fs)))
        getReplaceEntry fb >>= (\e -> entrySetText (castToEntry e) (replaceStr fs))
        getFindEntry fb >>=  (\e -> entrySetText (castToEntry e) (entryStr fs))
        listStoreClear ls
        mapM_ (\s -> listStoreAppend ls s) (entryHist fs)
        setEntireWord fb (entireWord fs)
        setWrapAround fb (wrapAround fs)
        setCaseSensitive fb (caseSensitive fs)
        setBackward fb (backward fs)
        setRegex fb (regex fs)

hideToolbar :: IDEAction
hideToolbar = do
    (_,mbtb) <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> return (ide{toolbar = (False,snd (toolbar ide))}))
            liftIO $ widgetHideAll tb

showToolbar :: IDEAction
showToolbar = do
    (_,mbtb) <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> return (ide{toolbar = (True,snd (toolbar ide))}))
            liftIO $ widgetShowAll tb

toggleToolbar :: IDEAction
toggleToolbar = do
    toolbar' <- readIDE toolbar
    if fst toolbar'
        then hideToolbar
        else showToolbar

hideFindbar :: IDEAction
hideFindbar = do
    (_,mbfb) <- readIDE findbar
    modifyIDE_ (\ide -> return (ide{findbar = (False,mbfb)}))
    case mbfb of
        Nothing -> return ()
        Just (fb,_) -> liftIO $ widgetHideAll fb

showFindbar :: IDEAction
showFindbar = do
    (_,mbfb) <- readIDE findbar
    modifyIDE_ (\ide -> return (ide{findbar = (True,mbfb)}))
    case mbfb of
        Nothing -> return ()
        Just (fb,_) -> liftIO $ widgetShowAll fb

focusFindEntry :: IDEAction
focusFindEntry = do
    (fb,_) <- needFindbar
    liftIO $ do
        entry <- getFindEntry fb
        widgetGrabFocus entry

toggleFindbar :: IDEAction
toggleFindbar = do
    findbar <- readIDE findbar
    if fst findbar
        then hideFindbar
        else showFindbar

constructFindReplace :: IDEM Toolbar
constructFindReplace = reifyIDE $ \ ideR   -> do
    toolbar <- toolbarNew
    toolbarSetStyle toolbar ToolbarIcons
    closeButton <- toolButtonNewFromStock "gtk-close"
    toolbarInsert toolbar closeButton 0

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

    findButton <- toolButtonNewFromStock "gtk-find"
    toolbarInsert toolbar findButton 0

    backwardButton <- toggleToolButtonNew
    toolButtonSetLabel backwardButton (Just "back")
    widgetSetName backwardButton "backwardButton"
    toolbarInsert toolbar backwardButton 0

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just "wrap")
    widgetSetName wrapAroundButton "wrapAroundButton"
    toolbarInsert toolbar wrapAroundButton 0

    entryTool <- toolItemNew
    entry <- entryNew
    widgetSetName entry "searchEntry"
    containerAdd entryTool entry
    widgetSetName entryTool "searchEntryTool"
    toolItemSetExpand entryTool True
    toolbarInsert toolbar entryTool 0

    store <- listStoreNew []
    customStoreSetColumn store (makeColumnIdString 0) id

    completion <- entryCompletionNew
    entrySetCompletion entry completion

    set completion [entryCompletionModel := Just store]
    cell <- cellRendererTextNew
    cellLayoutPackStart completion cell True
    cellLayoutSetAttributes completion cell store
        (\cd -> [cellText := cd])
    entryCompletionSetMatchFunc completion (matchFunc store)
    on completion matchSelected $ \ model iter -> do
        txt <- treeModelGetValue model iter (makeColumnIdString 0)
        entrySetText entry txt
        doSearch toolbar Forward ideR
        return True

    regexButton <- toggleToolButtonNew
    toolButtonSetLabel regexButton (Just "regex")
    widgetSetName regexButton "regexButton"
    toolbarInsert toolbar regexButton 0

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just "word")
    widgetSetName entireWordButton "entireWordButton"
    toolbarInsert toolbar entireWordButton 0

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just "c. S.")
    widgetSetName caseSensitiveButton "caseSensitiveButton"
    toolbarInsert toolbar caseSensitiveButton 0

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
            | otherwise                ->  return False
        _                              ->  return False)
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
    set toolbar [ toolbarChildHomogeneous regexButton := False ]
    set toolbar [ toolbarChildHomogeneous replaceAllButton := False ]
    set toolbar [ toolbarChildHomogeneous labelTool  := False ]
    set toolbar [ toolbarChildHomogeneous labelTool2 := False ]
    set toolbar [ toolbarChildHomogeneous labelTool3 := False ]

    reflectIDE (modifyIDE_ (\ide -> return ide{findbar = (False,Just (toolbar,store))})) ideR
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
    regex         <- getRegex fb
    res           <- reflectIDE (editFind entireWord caseSensitive wrapAround regex search "" hint) ideR
    if res || null search
        then do
            widgetModifyBase entry StateNormal white
            widgetModifyText entry StateNormal black
        else do
            widgetModifyBase entry StateNormal red
            widgetModifyText entry StateNormal white
    reflectIDE (addToHist search) ideR
    return ()

matchFunc :: ListStore String -> String -> TreeIter -> IO Bool
matchFunc model str iter = do
  tp <- treeModelGetPath model iter
  r <- case tp of
         (i:_) -> do row <- listStoreGetValue model i
                     return (isPrefixOf (map toLower str) (map toLower row) && length str < length row)
         otherwise -> return False
  return r

addToHist :: String -> IDEAction
addToHist str =
    if null str
        then return ()
        else do
            (_,ls)      <- needFindbar
            liftIO $ do
                entryHist   <- listStoreToList ls
                when (null (filter (\e -> (str `isPrefixOf` e)) entryHist)) $ do
                    let newList = take 12 (str : filter (\e -> not (e `isPrefixOf` str)) entryHist)
                    listStoreClear ls
                    mapM_ (\s -> listStoreAppend ls s) newList


replace :: Toolbar -> SearchHint -> IDERef -> IO ()
replace fb hint ideR   =  do
    entry          <- getFindEntry fb
    search         <- entryGetText (castToEntry entry)
    rentry         <- getReplaceEntry fb
    replace        <- entryGetText (castToEntry rentry)
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    wrapAround     <- getWrapAround fb
    regex          <- getRegex fb
    found <- reflectIDE (editReplace entireWord caseSensitive wrapAround regex search replace hint)
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
    regex          <- getRegex fb
    found <- reflectIDE (editReplaceAll entireWord caseSensitive wrapAround regex search replace hint)
                ideR
    return ()

editFind :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround regex search dummy hint =
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
                        mbsr <- backSearch regex st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do backSearch regex i2 search searchflags entireWord searchflags
                                    else return Nothing
                            Just (start,end) -> return (Just (start,end))
                    else do
                        if hint == Forward
                            then textIterForwardChar st1
                            else return True
                        mbsr <- forwardSearch regex st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do forwardSearch regex i1 search searchflags entireWord searchflags
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
        backSearch False iter string flags entireWord searchflags = do
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

        backSearch True iter string flags entireWord searchflags = do
            mbExp <- compileRegex caseSensitive search
            case mbExp of
                Just exp -> do
                    gtkbuf <- textIterGetBuffer iter
                    iterStart <- textBufferGetStartIter gtkbuf
                    iterEnd <- textBufferGetEndIter gtkbuf
                    before <- textBufferGetText gtkbuf iterStart iter True
                    after <- textBufferGetText gtkbuf iter iterEnd True
                    if null before
                        then return Nothing
                        else
                            case find ((<= length before) . fst . (!0)) (reverse (matchAll exp (before ++ after))) of
                                Just matches -> do
                                    iter1 <- textIterCopy iterStart
                                    textIterForwardChars iter1 (fst (matches!0))
                                    iter2 <- textIterCopy iter1
                                    textIterForwardChars iter2 (snd (matches!0))
                                    return $ Just (iter1, iter2)
                                Nothing -> return Nothing
                Nothing -> return Nothing

        forwardSearch False iter string flags entireWord searchflags = do
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

        forwardSearch True iter string flags entireWord searchflags = do
            mbExp <- compileRegex caseSensitive search
            case mbExp of
                Just exp -> do
                    gtkbuf <- textIterGetBuffer iter
                    iterStart <- textBufferGetStartIter gtkbuf
                    iterEnd <- textBufferGetEndIter gtkbuf
                    before <- textBufferGetText gtkbuf iterStart iter True
                    after <- textBufferGetText gtkbuf iter iterEnd True
                    if null after
                        then return Nothing
                        else
                            case find ((>= length before) . fst . (!0)) (matchAll exp (before ++ after)) of
                                Just matches -> do
                                    iter1 <- textIterCopy iterStart
                                    textIterForwardChars iter1 (fst (matches!0))
                                    iter2 <- textIterCopy iter1
                                    textIterForwardChars iter2 (snd (matches!0))
                                    return $ Just (iter1, iter2)
                                Nothing -> return Nothing
                Nothing -> return Nothing

editReplace :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround regex search replace hint =
    editReplace' entireWord caseSensitive wrapAround regex search replace hint True

editReplace' :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replace hint mayRepeat =
    inActiveBufContext' False $ \_ gtkbuf currentBuffer _ -> do
        insertMark <- liftIO $ textBufferGetInsert gtkbuf
        iter       <- liftIO $ textBufferGetIterAtMark gtkbuf insertMark
        iterStart  <- liftIO $ textBufferGetStartIter gtkbuf
        iterEnd    <- liftIO $ textBufferGetEndIter gtkbuf
        before     <- liftIO $ textIterGetText iterStart iter
        after      <- liftIO $ textIterGetText iter iterEnd
        match      <- liftIO $ subst before after search replace caseSensitive regex
        case match of
            Just (matchLength, replacement) -> do
                iter2     <- liftIO $ textIterCopy iter
                liftIO $ textIterForwardChars iter2 matchLength
                liftIO $ textBufferDelete gtkbuf iter iter2
                liftIO $ textBufferInsert gtkbuf iter replacement
                editFind entireWord caseSensitive wrapAround regex search "" hint
            Nothing -> do
                r <- editFind entireWord caseSensitive wrapAround regex search "" hint
                if r
                    then editReplace' entireWord caseSensitive wrapAround regex search
                            replace hint False
                    else return False
    where
        subst _ after search replace True False  = do
            case stripPrefix search after of
                Just remaining -> return $ Just (length search, replace)
                Nothing        -> return Nothing
        subst _ after search replcae False False = do
            if map toUpper search `isPrefixOf` map toUpper after
                then return $ Just (length search, replace)
                else return Nothing
        subst before after search replace caseSensitive True = do
            mbExp <- compileRegex caseSensitive search
            case mbExp of
                Just exp -> do
                    case find ((== length before) . fst . (!0)) (matchAll exp (before ++ after)) of
                        Just matches -> return $ Just (snd (matches!0), subRegex exp (take (snd (matches!0)) after) replace)
                        Nothing      -> return Nothing
                Nothing -> return Nothing

editReplaceAll :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplaceAll entireWord caseSensitive wrapAround regex search replace hint = do
    res <- editReplace' entireWord caseSensitive False regex search replace hint True
    if res
        then editReplaceAll entireWord caseSensitive False regex search replace hint
        else return False

compileRegex :: Bool -> String -> IO (Maybe Regex)
compileRegex caseSense searchString = do
    res <- compile ((if caseSense then id else (compIgnoreCase .|.))
                    (compExtended .|. compNewline))
                execBlank searchString
    case res of
        Left err -> do
            sysMessage Normal (show err)
            return Nothing
        Right regex -> return $ Just regex


red = Color 640000 10000 10000
white = Color 64000 64000 64000
black = Color 0 0 0

needFindbar :: IDEM (Toolbar,ListStore String)
needFindbar = do
    (_,mbfb) <- readIDE findbar
    case mbfb of
        Nothing -> throwIDE "Find>>needFindbar: Findbar not initialized"
        Just p  -> return p

editFindInc :: SearchHint -> IDEAction
editFindInc hint = do
    ideR <- ask
    (fb,_) <- needFindbar
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
    (fb,_) <- needFindbar
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

getEntireWord, getWrapAround, getCaseSensitive, getBackward, getRegex :: Toolbar -> IO Bool
getEntireWord    = getSelection "entireWordButton"
getWrapAround    = getSelection "wrapAroundButton"
getCaseSensitive = getSelection "caseSensitiveButton"
getBackward      = getSelection "backwardButton"
getRegex         = getSelection "regexButton"

getSelection :: String -> Toolbar -> IO Bool
getSelection str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonGetActive (castToToggleToolButton w)
        _   -> throwIDE "Find>>getIt widget not found"

setEntireWord, setWrapAround, setCaseSensitive, setBackward, setRegex :: Toolbar -> Bool -> IO ()
setEntireWord    = setSelection "entireWordButton"
setWrapAround    = setSelection "wrapAroundButton"
setCaseSensitive = setSelection "caseSensitiveButton"
setBackward      = setSelection "backwardButton"
setRegex         = setSelection "regexButton"

setSelection :: String -> Toolbar -> Bool ->  IO ()
setSelection str tb bool = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonSetActive (castToToggleToolButton w) bool
        _   -> throwIDE "Find>>getIt widget not found"
