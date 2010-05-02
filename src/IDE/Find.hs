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

import Graphics.UI.Gtk
       (toggleToolButtonSetActive, castToToggleToolButton,
        toggleToolButtonGetActive, castToBin, binGetChild, widgetGetName,
        containerGetChildren, listStoreGetValue, treeModelGetPath,
        TreeIter, ListStore, widgetModifyText, widgetModifyBase,
        toolbarChildHomogeneous, afterEntryActivate, spinButtonSetRange,
        afterFocusIn, onEntryActivate, afterKeyPress, afterDeleteText,
        afterInsertText, treeModelGetValue, matchSelected,
        entryCompletionSetMatchFunc, cellText, cellLayoutSetAttributes,
        cellLayoutPackStart, cellRendererTextNew, entryCompletionModel,
        entrySetCompletion, entryCompletionNew, makeColumnIdString,
        customStoreSetColumn, listStoreNew, toolItemSetExpand,
        toolButtonSetLabel, toggleToolButtonNew, entryNew, tooltipsSetTip,
        onToolButtonClicked, Widget, toolButtonNew, separatorToolItemNew,
        labelNew, containerAdd, widgetSetName, spinButtonNewWithRange,
        toolItemNew, toolbarInsert, toolButtonNewFromStock,
        toolbarSetIconSize, toolbarSetStyle, tooltipsNew, toolbarNew,
        Toolbar, widgetGrabFocus, widgetShowAll, widgetHideAll,
        listStoreAppend, listStoreClear, entrySetText, spinButtonSetValue,
        listStoreToList, castToEntry, entryGetText, castToSpinButton,
        spinButtonGetValueAsInt, StateType(..), ToolbarStyle(..),
        IconSize(..), AttrOp(..), set, on, Color(..))
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Reader

import IDE.Core.State
import IDE.TextEditor hiding(afterFocusIn)
import IDE.Pane.SourceBuffer
import Data.Char (digitToInt, isDigit, toLower, isAlphaNum)
import Text.Regex.TDFA hiding (caseSensitive)
import qualified Text.Regex.TDFA as Regex
import Text.Regex.TDFA.String (compile)
import Data.List (nub, find, isPrefixOf)
import Data.Array (bounds, (!), inRange)
import IDE.Utils.Tool (runTool)
import Control.Concurrent (forkIO)
import IDE.Pane.Grep
import IDE.Package (getPackageDescriptionAndPath)
import Distribution.PackageDescription (allBuildInfo, hsSourceDirs)
import System.FilePath (dropFileName)

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
    label <- labelNew (Just "Find: ")
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
        max <- getLineCount gtkbuf
        liftIO $ spinButtonSetRange spinL 1.0 (fromIntegral max)
        return True) ideR))

    spinL `afterEntryActivate` (reflectIDE (inActiveBufContext () $ \_ gtkbuf currentBuffer _ -> do
        line  <- liftIO $ spinButtonGetValueAsInt spinL
        iter  <- getIterAtLine gtkbuf (line - 1)
        placeCursor gtkbuf iter
        scrollToIter (sourceView currentBuffer) iter 0.2 Nothing
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
                                    grabFocus (sourceView buf)
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
            let dir = dropFileName (cabalPath)
            liftIO $ forkIO $ do
                (output, pid) <- runTool "grep" ((if caseSensitive then [] else ["-i"])
                    ++ ["-r", "-E", "-n", "--exclude=*~", regexString] ++ srcPaths) (Just dir)
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
    inActiveBufContext False $ \_ gtkbuf currentBuffer _ -> do
    i1 <- getStartIter gtkbuf
    i2 <- getEndIter gtkbuf
    text <- getText gtkbuf i1 i2 True
    removeTagByName gtkbuf "found" i1 i2
    startMark <- getInsertMark gtkbuf
    st1 <- getIterAtMark gtkbuf startMark
    mbsr2 <- do
        if hint == Backward
            then do
                st2 <- backwardCharC st1
                st3 <- backwardCharC st2
                mbsr <- backSearch exp matchIndex gtkbuf text st3
                case mbsr of
                    Nothing ->
                        if wrapAround
                            then do backSearch exp matchIndex gtkbuf text i2
                            else return Nothing
                    m -> return m
            else do
                st2 <- if hint == Forward
                    then forwardCharC st1
                    else return st1
                mbsr <- forwardSearch exp matchIndex gtkbuf text st2
                case mbsr of
                    Nothing ->
                        if wrapAround
                            then do forwardSearch exp matchIndex gtkbuf text i1
                            else return Nothing
                    m -> return m
    case mbsr2 of
        Just (start,end,_) -> do --found
            --widgetGrabFocus sourceView
            scrollToIter (sourceView currentBuffer) start 0.2 Nothing
            applyTagByName gtkbuf "found" start end
            placeCursor gtkbuf start
            return True
        Nothing -> return False
    where
        backSearch exp matchIndex gtkbuf text iter = do
            offset <- getOffset iter
            findMatch exp matchIndex gtkbuf text (<= offset) True

        forwardSearch exp matchIndex gtkbuf text iter = do
            offset <- getOffset iter
            findMatch exp matchIndex gtkbuf text (>= offset) False

regexAndMatchIndex :: Bool -> Bool -> Bool -> String -> IO (Maybe (Regex, Int))
regexAndMatchIndex caseSensitive entireWord regex string = do
    if null string
        then return Nothing
        else do
            let (regexString, index) = regexStringAndMatchIndex entireWord regex string
            case compileRegex caseSensitive regexString of
                Left err -> do
                    sysMessage Normal err
                    return Nothing
                Right regex -> return $ Just (regex, index)

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

findMatch :: Regex -> Int -> EditorBuffer -> String -> (Int -> Bool) -> Bool -> IDEM (Maybe (EditorIter, EditorIter, MatchArray))
findMatch exp matchIndex gtkbuf text offsetPred findLast = do
    let matches = (if findLast then reverse else id) (matchAll exp text)
    case find (offsetPred . fst . (!matchIndex)) matches of
        Just matches -> do
            iterStart <- getStartIter gtkbuf
            iter1     <- forwardCharsC iterStart (fst (matches!matchIndex))
            iter2     <- forwardCharsC iter1 (snd (matches!matchIndex))
            return $ Just (iter1, iter2, matches)
        Nothing -> return Nothing

editReplace :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround regex search replace hint =
    editReplace' entireWord caseSensitive wrapAround regex search replace hint True

editReplace' :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replace hint mayRepeat =
    inActiveBufContext False $ \_ gtkbuf currentBuffer _ -> do
        insertMark <- getInsertMark gtkbuf
        iter       <- getIterAtMark gtkbuf insertMark
        offset     <- getOffset iter
        mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
        case mbExpAndMatchIndex of
            Just (exp, matchIndex) -> do
                iStart <- getStartIter gtkbuf
                iEnd   <- getEndIter gtkbuf
                text   <- getText gtkbuf iStart iEnd True
                match  <- findMatch exp matchIndex gtkbuf text (== offset) False
                case match of
                    Just (iterStart, iterEnd, matches) -> do
                        mbText <- liftIO $ replacementText regex text matchIndex matches replace
                        case mbText of
                            Just text -> do
                                delete gtkbuf iterStart iterEnd
                                insert gtkbuf iterStart text
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
        replacementText False _ _ _ replace = return $ Just replace
        replacementText True text matchIndex matches replace = do
            case compileRegex caseSensitive search of
                Left err -> do
                    sysMessage Normal err
                    return Nothing
                Right exp -> return $ Just $ regexReplacement text matchIndex matches replace

regexReplacement :: String -> Int -> MatchArray -> String -> String
regexReplacement _ _ _ [] = []
regexReplacement text matchIndex matches ('\\' : '\\' : xs) = '\\' : regexReplacement text matchIndex matches xs
regexReplacement text matchIndex matches ('\\' : n : xs) | isDigit n =
    let subIndex = matchIndex + digitToInt n
        value    = if inRange (bounds matches) subIndex
                    then
                        let subExp = matches!(matchIndex + digitToInt n) in
                        take (snd subExp) $ drop (fst subExp) text
                    else ['\\', n] in
    value ++ regexReplacement text matchIndex matches xs

regexReplacement text matchIndex matches (x : xs) = x : regexReplacement text matchIndex matches xs

editReplaceAll :: Bool -> Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool
editReplaceAll entireWord caseSensitive wrapAround regex search replace hint = do
    res <- editReplace' entireWord caseSensitive False regex search replace hint True
    if res
        then editReplaceAll entireWord caseSensitive False regex search replace hint
        else return False

compileRegex :: Bool -> String -> Either String Regex
compileRegex caseSense searchString =
    let compOption = defaultCompOpt {
                            Regex.caseSensitive = caseSense
                        ,   multiline = True } in
    compile compOption defaultExecOpt searchString

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
        Initial -> inActiveBufContext () $ \_ gtkbuf currentBuffer _ -> do
            hasSelection <- hasSelection gtkbuf
            when hasSelection $ do
                (i1,i2)   <- getSelectionBounds gtkbuf
                text      <- getText gtkbuf i1 i2 False
                findEntry <- liftIO $ getFindEntry fb
                liftIO $ entrySetText (castToEntry findEntry) text
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
