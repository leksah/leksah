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
import Graphics.UI.Gtk.Gdk.Events
import Data.Maybe
import Control.Monad.Reader

import IDE.Core.State
import IDE.Pane.SourceBuffer
import Data.Char (toLower)
import Text.Regex.Posix.String (compile)
import Text.Regex.Posix
    (Regex(..), compNewline, compExtended,  execBlank, compIgnoreCase)
import Data.Bits ((.|.))
import Data.List (nub, find, isPrefixOf)
import Data.Array ((!))
import Text.Regex.Base.RegexLike (matchAll)
import Text.Regex (subRegex)
import IDE.Tool (runTool)
import Control.Concurrent (forkIO)
import IDE.Pane.Grep
import GHC.Unicode (isAlphaNum)
import IDE.Package (getPackageDescriptionAndPath)
import Distribution.PackageDescription (allBuildInfo, hsSourceDirs)


data FindState = FindState {
            entryStr        ::    String
        ,   entryHist       ::    [String]
        ,   replaceStr      ::    String
        ,   replaceHist     ::    [String]
        ,   caseSensitive   ::    Bool
        ,   entireWord      ::    Bool
        ,   wrapAround      ::    Bool
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
        regex         <- getRegex fb
        return FindState{
                entryStr        =   entryStr
            ,   entryHist       =   entryHist
            ,   replaceStr      =   replaceStr
            ,   replaceHist     =   []
            ,   caseSensitive   =   caseSensitive
            ,   entireWord      =   entireWord
            ,   wrapAround      =   wrapAround
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
        setRegex fb (regex fs)

hideToolbar :: IDEAction
hideToolbar = do
    (_,mbtb) <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> ide{toolbar = (False,snd (toolbar ide))})
            liftIO $ widgetHideAll tb

showToolbar :: IDEAction
showToolbar = do
    (_,mbtb) <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> ide{toolbar = (True,snd (toolbar ide))})
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
    modifyIDE_ (\ide -> ide{findbar = (False,mbfb)})
    case mbfb of
        Nothing -> return ()
        Just (fb,_) -> liftIO $ widgetHideAll fb

showFindbar :: IDEAction
showFindbar = do
    (_,mbfb) <- readIDE findbar
    modifyIDE_ (\ide -> ide{findbar = (True,mbfb)})
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
    tooltips <- tooltipsNew
    toolbarSetStyle toolbar ToolbarIcons
    toolbarSetIconSize toolbar IconSizeSmallToolbar
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

    grepButton <- toolButtonNew (Nothing :: Maybe Widget) (Just "Grep")
    toolbarInsert toolbar grepButton 0
    grepButton `onToolButtonClicked` (reflectIDE (doGrep toolbar) ideR)
    tooltipsSetTip tooltips grepButton "Search in multiple files" ""

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

    nextButton <- toolButtonNewFromStock "gtk-go-forward"
    toolbarInsert toolbar nextButton 0
    tooltipsSetTip tooltips nextButton "Search for the next match in the current file" ""
    nextButton `onToolButtonClicked` (doSearch toolbar Forward ideR  )

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just "Wrap")
    widgetSetName wrapAroundButton "wrapAroundButton"
    toolbarInsert toolbar wrapAroundButton 0
    tooltipsSetTip tooltips wrapAroundButton "When selected searching will continue from the top when no more matches are found" ""

    previousButton <- toolButtonNewFromStock "gtk-go-back"
    toolbarInsert toolbar previousButton 0
    tooltipsSetTip tooltips previousButton "Search for the previous match in the current file" ""
    previousButton `onToolButtonClicked` (doSearch toolbar Backward ideR  )

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
    toolButtonSetLabel regexButton (Just "Regex")
    widgetSetName regexButton "regexButton"
    toolbarInsert toolbar regexButton 0
    regexButton `onToolButtonClicked` (doSearch toolbar Insert ideR)
    tooltipsSetTip tooltips regexButton "When selected the search string is used as a regular expression" ""

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just "Words")
    widgetSetName entireWordButton "entireWordButton"
    toolbarInsert toolbar entireWordButton 0
    entireWordButton `onToolButtonClicked` (doSearch toolbar Insert ideR)
    tooltipsSetTip tooltips entireWordButton "When selected only entire words are matched" ""

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just "c. S.")
    widgetSetName caseSensitiveButton "caseSensitiveButton"
    toolbarInsert toolbar caseSensitiveButton 0
    caseSensitiveButton `onToolButtonClicked` (doSearch toolbar Insert ideR)
    tooltipsSetTip tooltips caseSensitiveButton "When selected the search is case sensitive" ""

    labelTool <- toolItemNew
    label <- labelNew (Just "Search: ")
    containerAdd labelTool label
    toolbarInsert toolbar labelTool 0

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
    set toolbar [ toolbarChildHomogeneous regexButton := False ]
    set toolbar [ toolbarChildHomogeneous replaceAllButton := False ]
    set toolbar [ toolbarChildHomogeneous labelTool  := False ]
    set toolbar [ toolbarChildHomogeneous labelTool2 := False ]
    set toolbar [ toolbarChildHomogeneous labelTool3 := False ]

    reflectIDE (modifyIDE_ (\ide -> ide{findbar = (False,Just (toolbar,store))})) ideR
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
    mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
    case mbExpAndMatchIndex of
        Just (exp, matchIndex) -> do
            res           <- reflectIDE (editFind entireWord caseSensitive wrapAround regex search "" hint) ideR
            if res || null search
                then do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else do
                    widgetModifyBase entry StateNormal red
                    widgetModifyText entry StateNormal white
        Nothing -> do
            if null search
                then do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else do
                    widgetModifyBase entry StateNormal orange
                    widgetModifyText entry StateNormal black
    reflectIDE (addToHist search) ideR
    return ()

doGrep :: Toolbar -> IDEAction
doGrep fb   = do
    ideR          <- ask
    entry         <- liftIO $ getFindEntry fb
    search        <- liftIO $ entryGetText (castToEntry entry)
    entireWord    <- liftIO $ getEntireWord fb
    caseSensitive <- liftIO $ getCaseSensitive fb
    wrapAround    <- liftIO $ getWrapAround fb
    regex         <- liftIO $ getRegex fb
    let (regexString, _) = regexStringAndMatchIndex entireWord regex search
    mbPD <- getPackageDescriptionAndPath
    case mbPD of
        Nothing             -> ideMessage Normal "No package description"
        Just (pd,cabalPath) -> do
            let srcPaths = nub $ concatMap hsSourceDirs $ allBuildInfo pd
            liftIO $ forkIO $ do
                (output, pid) <- runTool "grep" ((if caseSensitive then [] else ["-i"])
                    ++ ["-r", "-E", "-n", "--exclude=*~", regexString] ++ srcPaths)
                reflectIDE (setGrepResults output) ideR
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
editFind entireWord caseSensitive wrapAround regex search dummy hint = do
    mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
    case mbExpAndMatchIndex of
        Nothing -> return False
        Just (exp, matchIndex) -> editFind' exp matchIndex wrapAround dummy hint

editFind' :: Regex -> Int -> Bool -> String -> SearchHint -> IDEM Bool
editFind' exp matchIndex wrapAround dummy hint =
    inActiveBufContext' False $ \_ gtkbuf currentBuffer _ -> liftIO $ do
    i1 <- textBufferGetStartIter gtkbuf
    i2 <- textBufferGetEndIter gtkbuf
    textBufferRemoveTagByName gtkbuf "found" i1 i2
    startMark <- textBufferGetInsert gtkbuf
    st1 <- textBufferGetIterAtMark gtkbuf startMark
    mbsr2 <- liftIO $ do
        if hint == Backward
            then do
                textIterBackwardChar st1
                textIterBackwardChar st1
                mbsr <- backSearch exp matchIndex st1
                case mbsr of
                    Nothing ->
                        if wrapAround
                            then do backSearch exp matchIndex i2
                            else return Nothing
                    Just (start,end) -> return (Just (start,end))
            else do
                if hint == Forward
                    then textIterForwardChar st1
                    else return True
                mbsr <- forwardSearch exp matchIndex st1
                case mbsr of
                    Nothing ->
                        if wrapAround
                            then do forwardSearch exp matchIndex i1
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
        backSearch exp matchIndex iter = do
            gtkbuf <- textIterGetBuffer iter
            offset  <- textIterGetOffset iter
            findMatch exp matchIndex gtkbuf (<= offset) True

        forwardSearch exp matchIndex iter = do
            gtkbuf <- textIterGetBuffer iter
            offset  <- textIterGetOffset iter
            findMatch exp matchIndex gtkbuf (>= offset) False

regexAndMatchIndex :: Bool -> Bool -> Bool -> String -> IO (Maybe (Regex, Int))
regexAndMatchIndex caseSensitive entireWord regex string = do
    if null string
        then return Nothing
        else do
            let (regexString, index) = regexStringAndMatchIndex entireWord regex string
            mbRegex <- compileRegex caseSensitive regexString
            return $ maybe Nothing (Just . (flip (,) index)) mbRegex

regexStringAndMatchIndex :: Bool -> Bool -> String -> (String, Int)
regexStringAndMatchIndex entireWord regex string =
    -- Escape non regex string
    let regexString = if regex
                        then string
                        else foldl (\s c -> s ++ if isAlphaNum c then [c] else ['\\', c]) "" string in
    -- Regular expression with word filter if needed
    if entireWord
        then ("(^|[^a-zA-Z0-9])(" ++ regexString ++ ")($|[^a-zA-Z0-9])", 2)
        else (regexString, 0)

findMatch :: Regex -> Int -> TextBuffer -> (Int -> Bool) -> Bool -> IO (Maybe (TextIter, TextIter))
findMatch exp matchIndex gtkbuf offsetPred findLast = do
    iterStart <- textBufferGetStartIter gtkbuf
    iterEnd <- textBufferGetEndIter gtkbuf
    text <- textBufferGetText gtkbuf iterStart iterEnd True
    let matches = (if findLast then reverse else id) (matchAll exp text)
    case find (offsetPred . fst . (!matchIndex)) matches of
        Just matches -> do
            iter1 <- textIterCopy iterStart
            textIterForwardChars iter1 (fst (matches!matchIndex))
            iter2 <- textIterCopy iter1
            textIterForwardChars iter2 (snd (matches!matchIndex))
            return $ Just (iter1, iter2)
        Nothing -> return Nothing

editReplace :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround regex search replace hint =
    editReplace' entireWord caseSensitive wrapAround regex search replace hint True

editReplace' :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replace hint mayRepeat =
    inActiveBufContext' False $ \_ gtkbuf currentBuffer _ -> do
        insertMark <- liftIO $ textBufferGetInsert gtkbuf
        iter       <- liftIO $ textBufferGetIterAtMark gtkbuf insertMark
        offset     <- liftIO $ textIterGetOffset iter
        mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
        case mbExpAndMatchIndex of
            Just (exp, matchIndex) -> do
                match      <- liftIO $ findMatch exp matchIndex gtkbuf (== offset) False
                case match of
                    Just (iterStart, iterEnd) -> do
                        old    <- liftIO $ textIterGetText iterStart iterEnd
                        mbText <- liftIO $ replacementText regex old replace
                        case mbText of
                            Just text -> do
                                liftIO $ textBufferDelete gtkbuf iterStart iterEnd
                                liftIO $ textBufferInsert gtkbuf iterStart text
                            Nothing -> do
                                sysMessage Normal
                                    "Should never happen. findMatch worked but repleacementText failed"
                                return ()
                        editFind entireWord caseSensitive wrapAround regex search "" hint
                    Nothing -> do
                        r <- editFind entireWord caseSensitive wrapAround regex search "" hint
                        if r
                            then editReplace' entireWord caseSensitive wrapAround regex search
                                    replace hint False
                            else return False
            Nothing -> return False
    where
        replacementText False old replace = return $ Just replace
        replacementText True old replace = do
            mbExp <- compileRegex caseSensitive search
            case mbExp of
                Just exp -> return $ Just $ subRegex exp old replace
                Nothing  -> return Nothing

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


red = Color 64000 10000 10000
orange = Color 64000 48000 0
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

getEntireWord, getWrapAround, getCaseSensitive, getRegex :: Toolbar -> IO Bool
getEntireWord    = getSelection "entireWordButton"
getWrapAround    = getSelection "wrapAroundButton"
getCaseSensitive = getSelection "caseSensitiveButton"
getRegex         = getSelection "regexButton"

getSelection :: String -> Toolbar -> IO Bool
getSelection str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonGetActive (castToToggleToolButton w)
        _   -> throwIDE "Find>>getIt widget not found"

setEntireWord, setWrapAround, setCaseSensitive, setRegex :: Toolbar -> Bool -> IO ()
setEntireWord    = setSelection "entireWordButton"
setWrapAround    = setSelection "wrapAroundButton"
setCaseSensitive = setSelection "caseSensitiveButton"
setRegex         = setSelection "regexButton"

setSelection :: String -> Toolbar -> Bool ->  IO ()
setSelection str tb bool = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (\w -> liftM  (== str) (widgetGetName w) ) widgets
    case entryL of
        [w] -> toggleToolButtonSetActive (castToToggleToolButton w) bool
        _   -> throwIDE "Find>>getIt widget not found"
