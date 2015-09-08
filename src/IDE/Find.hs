{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Find
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
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

import Graphics.UI.Gtk
       (toToolbar, ToolbarClass, toggleToolButtonSetActive,
        castToToggleToolButton, toggleToolButtonGetActive, castToBin,
        binGetChild, widgetGetName, containerGetChildren,
        listStoreGetValue, treeModelGetPath, TreeIter, ListStore,
        widgetModifyText, widgetModifyBase, toolbarChildHomogeneous, after,
        entryActivate, spinButtonSetRange, focusInEvent, keyPressEvent,
        deleteText, insertText, treeModelGetValue, matchSelected,
        entryCompletionSetMatchFunc, cellText, cellLayoutSetAttributes,
        cellLayoutPackStart, cellRendererTextNew, entryCompletionModel,
        entrySetCompletion, entryCompletionNew, makeColumnIdString,
        customStoreSetColumn, listStoreNew, toolItemSetExpand,
        toolButtonSetLabel, toggleToolButtonNew, entryNew,
        onToolButtonClicked, Widget, toolButtonNew, separatorToolItemNew,
        labelNew, containerAdd, widgetSetName, spinButtonNewWithRange,
        toolItemNew, toolbarInsert, toolButtonNewFromStock,
        toolbarSetStyle, toolbarNew, Toolbar, widgetGrabFocus,
        widgetShowAll, widgetHide, listStoreAppend, listStoreClear,
        entrySetText, spinButtonSetValue, listStoreToList, castToEntry,
        entryGetText, castToSpinButton, spinButtonGetValueAsInt,
        StateType(..), ToolbarStyle(..), IconSize(..), AttrOp(..), set, on,
        Color(..), widgetTooltipText)
import Graphics.UI.Gtk.Gdk.EventM
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Buttons.ToggleButton
import Graphics.UI.Gtk.Buttons.CheckButton

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.TextEditor hiding(afterFocusIn)
import IDE.Pane.SourceBuffer
import Data.Char (digitToInt, isDigit, toLower, isAlphaNum)
import Text.Regex.TDFA hiding (caseSensitive, after)
import qualified Text.Regex.TDFA as Regex
import Text.Regex.TDFA.Text (compile)
import Data.List (find, isPrefixOf)
import Data.Array (bounds, (!), inRange)
import IDE.Pane.Grep (grepWorkspace)
import IDE.Workspaces (workspaceTry, packageTry)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad (liftM, filterM, when, unless)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr(..))
import Foreign.ForeignPtr (withForeignPtr)
import Graphics.UI.GtkInternals (unToolbar)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, singleton, isPrefixOf, length, null, toLower)
import Data.Monoid ((<>))
import Graphics.UI.Gtk.Entry.Entry (entrySetPlaceholderText)

foreign import ccall safe "gtk_toolbar_set_icon_size"
  gtk_toolbar_set_icon_size :: Ptr Toolbar -> CInt -> IO ()

toolbarSetIconSize :: ToolbarClass self => self -> IconSize -> IO ()
toolbarSetIconSize self iconSize =
  withForeignPtr (unToolbar $ toToolbar self) $
    \selfPtr ->gtk_toolbar_set_icon_size selfPtr (fromIntegral $ fromEnum iconSize)

data FindState = FindState {
            entryStr        ::    Text
        ,   entryHist       ::    [Text]
        ,   replaceStr      ::    Text
        ,   replaceHist     ::    [Text]
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
        lineNr        <- getLineEntry fb >>= (spinButtonGetValueAsInt . castToSpinButton)
        replaceStr    <- getReplaceEntry fb >>= (entryGetText . castToEntry)
        entryStr      <- getFindEntry fb >>=  (entryGetText . castToEntry)
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
        mapM_ (listStoreAppend ls) (entryHist fs)
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
            liftIO $ widgetHide tb

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
        Just (fb,_) -> liftIO $ widgetHide fb

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
    toolbarSetStyle toolbar ToolbarIcons
    toolbarSetIconSize toolbar IconSizeSmallToolbar
    closeButton <- toolButtonNewFromStock "gtk-close"
    toolbarInsert toolbar closeButton 0

    spinTool <- toolItemNew
    spinL <- spinButtonNewWithRange 1.0 1000.0 10.0
    widgetSetName spinL ("gotoLineEntry" :: Text)
    containerAdd spinTool spinL
    widgetSetName spinTool ("gotoLineEntryTool" :: Text)
    toolbarInsert toolbar spinTool 0

    labelTool3 <- toolItemNew
    label3 <- labelNew (Just (__"Goto Line :"))
    containerAdd labelTool3 label3
    toolbarInsert toolbar labelTool3 0

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar sep1 0

    let performGrep = reflectIDE (packageTry $ doGrep toolbar) ideR
    grepButton <- toolButtonNew (Nothing :: Maybe Widget) (Just (__"Grep"))
    toolbarInsert toolbar grepButton 0
    grepButton `onToolButtonClicked` performGrep
    set grepButton [widgetTooltipText := Just (__"Search in multiple files")]

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar sep1 0

    replaceAllButton <- toolButtonNew (Nothing :: Maybe Widget) (Just (__"Replace All"))
    toolbarInsert toolbar replaceAllButton 0

    replaceButton <- toolButtonNewFromStock "gtk-find-and-replace"
    toolbarInsert toolbar replaceButton 0

    replaceTool <- toolItemNew
    rentry <- entryNew
    widgetSetName rentry ("replaceEntry" :: Text)
    entrySetPlaceholderText rentry $ Just ("Replace with" :: Text)
    containerAdd replaceTool rentry
    widgetSetName replaceTool ("replaceTool" :: Text)
    toolbarInsert toolbar replaceTool 0

    sep2 <- separatorToolItemNew
    toolbarInsert toolbar sep2 0

    nextButton <- toolButtonNewFromStock "gtk-go-forward"
    toolbarInsert toolbar nextButton 0
    set nextButton [widgetTooltipText := Just (__"Search for the next match in the current file")]
    nextButton `onToolButtonClicked` doSearch toolbar Forward ideR

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just (__"Wrap"))
    widgetSetName wrapAroundButton ("wrapAroundButton" :: Text)
    toolbarInsert toolbar wrapAroundButton 0
    set wrapAroundButton [widgetTooltipText := Just (__"When selected searching will continue from the top when no more matches are found")]

    previousButton <- toolButtonNewFromStock "gtk-go-back"
    toolbarInsert toolbar previousButton 0
    set previousButton [widgetTooltipText := Just (__"Search for the previous match in the current file")]
    previousButton `onToolButtonClicked` doSearch toolbar Backward ideR

    entryTool <- toolItemNew
    entry <- entryNew
    widgetSetName entry ("searchEntry" :: Text)
    entrySetPlaceholderText entry $ Just ("Find" :: Text)
    containerAdd entryTool entry
    widgetSetName entryTool ("searchEntryTool" :: Text)
    toolItemSetExpand entryTool True
    toolbarInsert toolbar entryTool 0

    let column0 = makeColumnIdString 0
    store <- listStoreNew []
    customStoreSetColumn store column0 id

    completion <- entryCompletionNew
    entrySetCompletion entry completion

    set completion [entryCompletionModel := Just store]
    cell <- cellRendererTextNew
    cellLayoutPackStart completion cell True
    cellLayoutSetAttributes completion cell store
        (\ cd -> [cellText := cd])
    entryCompletionSetMatchFunc completion (matchFunc store)
    on completion matchSelected $ \ model iter -> do
        txt <- treeModelGetValue model iter column0
        entrySetText entry txt
        doSearch toolbar Forward ideR
        return True

    regexButton <- toggleToolButtonNew
    toolButtonSetLabel regexButton (Just (__"Regex"))
    widgetSetName regexButton ("regexButton" :: Text)
    toolbarInsert toolbar regexButton 0
    regexButton `onToolButtonClicked` doSearch toolbar Insert ideR
    set regexButton [widgetTooltipText := Just (__"When selected the search string is used as a regular expression")]

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just (__"Words"))
    widgetSetName entireWordButton ("entireWordButton" :: Text)
    toolbarInsert toolbar entireWordButton 0
    entireWordButton `onToolButtonClicked` doSearch toolbar Insert ideR
    set entireWordButton [widgetTooltipText := Just (__"When selected only entire words are matched")]

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just (__"Case"))
    widgetSetName caseSensitiveButton ("caseSensitiveButton" :: Text)
    toolbarInsert toolbar caseSensitiveButton 0
    caseSensitiveButton `onToolButtonClicked`
       doSearch toolbar Insert ideR
    set caseSensitiveButton [widgetTooltipText := Just (__"When selected the search is case sensitive")]

    after entry insertText (\ (t::Text) i -> do
        doSearch toolbar Insert ideR
        return i)
    after entry deleteText (\ _ _ -> doSearch toolbar Delete ideR)

    on entry entryActivate $ doSearch toolbar Forward ideR
    on entry focusInEvent $ do
        liftIO $ reflectIDE (triggerEventIDE (Sensitivity [(SensitivityEditor, False)])) ideR
        return False

    replaceButton `onToolButtonClicked` replace toolbar Forward ideR
    let performReplaceAll = replaceAll toolbar Initial ideR
    replaceAllButton `onToolButtonClicked` performReplaceAll

    let ctrl "c" = toggleToolButton caseSensitiveButton >> return True
        ctrl "e" = toggleToolButton regexButton >> return True
        ctrl "w" = toggleToolButton entireWordButton >> return True
        ctrl "p" = toggleToolButton wrapAroundButton >> return True
        ctrl "r" = performReplaceAll >> return True
        ctrl "g" = performGrep >> return True
        ctrl _ = return False
        toggleToolButton btn = do
            old <- toggleToolButtonGetActive btn
            toggleToolButtonSetActive btn $ not old

    entry `on` keyPressEvent $ do
        name <- eventKeyName
        mods <- eventModifier
        case name of
            "Down"   -> liftIO $ doSearch toolbar Forward ideR >> return True
            "Up"     -> liftIO $ doSearch toolbar Backward ideR >> return True
            "Escape" -> liftIO $ getOut ideR >> return True
            "Tab"    -> liftIO $ do
                re <- getReplaceEntry toolbar
                widgetGrabFocus re
                --- widgetAc
                return True
            _ | mapControlCommand Control `elem` mods -> liftIO . ctrl $ T.toLower name
            _        -> return False

    rentry `on` keyPressEvent $ do
        name <- eventKeyName
        mods <- eventModifier
        case () of
           _ | name == "Tab" || name == "ISO_Left_Tab" -> liftIO $ do
                    fe <- getFindEntry toolbar
                    widgetGrabFocus fe
                    return True
             | mapControlCommand Control `elem` mods ->
                        liftIO . ctrl $ T.toLower name
             | otherwise -> return False

    after spinL focusInEvent . liftIO $ reflectIDE (inActiveBufContext True $ \ _ _ ebuf _ _ -> do
        max <- getLineCount ebuf
        liftIO $ spinButtonSetRange spinL 1.0 (fromIntegral max)
        return True) ideR

    spinL `on` keyPressEvent $ do
        name <- eventKeyName
        mods <- eventModifier
        case name of
            "Escape" -> liftIO $ getOut ideR >> return True
            "Tab"    -> liftIO $ do
                re <- getFindEntry toolbar
                widgetGrabFocus re
                return True
            _ | mapControlCommand Control `elem` mods -> liftIO . ctrl $ T.toLower name
            _ -> return False

    after spinL entryActivate $ reflectIDE (inActiveBufContext () $ \ _ sv ebuf _ _ -> do
        line <- liftIO $ spinButtonGetValueAsInt spinL
        iter <- getIterAtLine ebuf (line - 1)
        placeCursor ebuf iter
        scrollToIter sv iter 0.2 Nothing
        liftIO $ getOut ideR
        return ()) ideR

    closeButton `onToolButtonClicked` reflectIDE hideFindbar ideR

    set toolbar [toolbarChildHomogeneous spinTool := False]
    set toolbar [toolbarChildHomogeneous wrapAroundButton := False]
    set toolbar [toolbarChildHomogeneous entireWordButton := False]
    set toolbar [toolbarChildHomogeneous caseSensitiveButton := False]
    set toolbar [toolbarChildHomogeneous regexButton := False]
    set toolbar [toolbarChildHomogeneous replaceAllButton := False]
    set toolbar [toolbarChildHomogeneous labelTool3 := False]

    reflectIDE (modifyIDE_ (\ ide -> ide{findbar = (False, Just (toolbar, store))})) ideR
    return toolbar
  where
    getOut = reflectIDE $ do
        hideFindbar
        maybeActiveBuf ?>>= makeActive


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
            if res || T.null search
                then do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else do
                    widgetModifyBase entry StateNormal red
                    widgetModifyText entry StateNormal white
        Nothing ->
            if T.null search
                then do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else do
                    widgetModifyBase entry StateNormal orange
                    widgetModifyText entry StateNormal black
    reflectIDE (addToHist search) ideR
    return ()

doGrep :: Toolbar -> PackageAction
doGrep fb   = do
    package       <- ask
    ideR          <- lift ask
    entry         <- liftIO $ getFindEntry fb
    search        <- liftIO $ entryGetText (castToEntry entry)
    entireWord    <- liftIO $ getEntireWord fb
    caseSensitive <- liftIO $ getCaseSensitive fb
    wrapAround    <- liftIO $ getWrapAround fb
    regex         <- liftIO $ getRegex fb
    let (regexString, _) = regexStringAndMatchIndex entireWord regex search
    liftIDE $ workspaceTry $ grepWorkspace regexString caseSensitive

matchFunc :: ListStore Text -> Text -> TreeIter -> IO Bool
matchFunc model str iter = do
  tp <- treeModelGetPath model iter
  case tp of
         (i:_) -> do row <- listStoreGetValue model i
                     return (T.isPrefixOf (T.toLower str) (T.toLower row) && T.length str < T.length row)
         otherwise -> return False

addToHist :: Text -> IDEAction
addToHist str =
    unless (T.null str) $
       do (_, ls) <- needFindbar
          liftIO $
            do entryHist <- listStoreToList ls
               unless (any (str `T.isPrefixOf`) entryHist) $
                 do let newList
                          = take 12
                              (str : filter (\ e -> not (e `T.isPrefixOf` str)) entryHist)
                    listStoreClear ls
                    mapM_ (listStoreAppend ls) newList

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

editFind :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround regex search dummy hint = do
    mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
    case mbExpAndMatchIndex of
        Nothing -> return False
        Just (exp, matchIndex) -> editFind' exp matchIndex wrapAround dummy hint

editFind' :: Regex -> Int -> Bool -> Text -> SearchHint -> IDEM Bool
editFind' exp matchIndex wrapAround dummy hint =
    inActiveBufContext False $ \_ sv ebuf _ _ -> do
        i1 <- getStartIter ebuf
        i2 <- getEndIter ebuf
        text <- getText ebuf i1 i2 True
        removeTagByName ebuf "found"
        startMark <- getInsertMark ebuf
        st1 <- getIterAtMark ebuf startMark
        mbsr2 <-
            if hint == Backward
                then do
                    st2 <- backwardCharC st1
                    st3 <- backwardCharC st2
                    mbsr <- backSearch exp matchIndex ebuf text st3
                    case mbsr of
                        Nothing ->
                            if wrapAround
                                then backSearch exp matchIndex ebuf text i2
                                else return Nothing
                        m -> return m
                else do
                    st2 <- if hint == Forward
                        then forwardCharC st1
                        else return st1
                    mbsr <- if hint == Initial
                            then initialSearch exp matchIndex ebuf text st2
                            else forwardSearch exp matchIndex ebuf text st2
                    case mbsr of
                        Nothing ->
                            if wrapAround
                                then forwardSearch exp matchIndex ebuf text i1
                                else return Nothing
                        m -> return m
        case mbsr2 of
            Just (start,end,_) -> do --found
                --widgetGrabFocus sourceView
                scrollToIter sv start 0.2 Nothing
                applyTagByName ebuf "found" start end
                placeCursor ebuf start
                return True
            Nothing -> return False
    where
        backSearch exp matchIndex ebuf text iter = do
            offset <- getOffset iter
            findMatch exp matchIndex ebuf text (<= offset) True

        forwardSearch exp matchIndex ebuf text iter = do
            offset <- getOffset iter
            findMatch exp matchIndex ebuf text (>= offset) False

        initialSearch exp matchIndex ebuf text iter = findMatch exp matchIndex ebuf text (>= 0) False

regexAndMatchIndex :: Bool -> Bool -> Bool -> Text -> IO (Maybe (Regex, Int))
regexAndMatchIndex caseSensitive entireWord regex string =
    if T.null string
        then return Nothing
        else do
            let (regexString, index) = regexStringAndMatchIndex entireWord regex string
            case compileRegex caseSensitive regexString of
                Left err -> do
                    sysMessage Normal $ T.pack err
                    return Nothing
                Right regex -> return $ Just (regex, index)

regexStringAndMatchIndex :: Bool -> Bool -> Text -> (Text, Int)
regexStringAndMatchIndex entireWord regex string =
    -- Escape non regex string
    let regexString = if regex
                        then string
                        else foldl (\s c -> s <> if isAlphaNum c then T.singleton c else "\\"<>T.singleton c) "" $ T.unpack string in
    -- Regular expression with word filter if needed
    if entireWord
        then ("(^|[^a-zA-Z0-9])(" <> regexString <> ")($|[^a-zA-Z0-9])", 2)
        else (regexString, 0)

findMatch :: TextEditor editor => Regex -> Int -> EditorBuffer editor -> Text -> (Int -> Bool) -> Bool -> IDEM (Maybe (EditorIter editor, EditorIter editor, MatchArray))
findMatch exp matchIndex gtkbuf text offsetPred findLast = do
    let matches = (if findLast then reverse else id) (matchAll exp text)
    case find (offsetPred . fst . (!matchIndex)) matches of
        Just matches -> do
            iterStart <- getStartIter gtkbuf
            iter1     <- forwardCharsC iterStart (fst (matches!matchIndex))
            iter2     <- forwardCharsC iter1 (snd (matches!matchIndex))
            return $ Just (iter1, iter2, matches)
        Nothing -> return Nothing

editReplace :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround regex search replace hint =
    editReplace' entireWord caseSensitive wrapAround regex search replace hint True

editReplace' :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replace hint mayRepeat =
    inActiveBufContext False $ \_ _ ebuf _ _ -> do
        insertMark <- getInsertMark ebuf
        iter       <- getIterAtMark ebuf insertMark
        offset   <- getOffset iter
        let offset' = if mayRepeat then 0 else offset
        mbExpAndMatchIndex <- liftIO $ regexAndMatchIndex caseSensitive entireWord regex search
        case mbExpAndMatchIndex of
            Just (exp, matchIndex) -> do
                iStart <- getStartIter ebuf
                iEnd   <- getEndIter ebuf
                text   <- getText ebuf iStart iEnd True
                match  <- findMatch exp matchIndex ebuf text (== offset') False
                case match of
                    Just (iterStart, iterEnd, matches) -> do
                        mbText <- liftIO $ replacementText regex text matchIndex matches $ T.unpack replace
                        case mbText of
                            Just text -> do
                                beginUserAction ebuf
                                delete ebuf iterStart iterEnd
                                insert ebuf iterStart (T.pack text)
                                endUserAction ebuf
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
        replacementText True text matchIndex matches replace =
            case compileRegex caseSensitive search of
                Left err -> do
                    sysMessage Normal $ T.pack err
                    return Nothing
                Right exp -> return $ Just $ regexReplacement text matchIndex matches replace

regexReplacement :: Text -> Int -> MatchArray -> String -> String
regexReplacement _ _ _ [] = []
regexReplacement text matchIndex matches ('\\' : '\\' : xs) = '\\' : regexReplacement text matchIndex matches xs
regexReplacement text matchIndex matches ('\\' : n : xs) | isDigit n =
    let subIndex = matchIndex + digitToInt n
        value    = if inRange (bounds matches) subIndex
                    then
                        let subExp = matches!(matchIndex + digitToInt n) in
                        take (snd subExp) $ drop (fst subExp) $ T.unpack text
                    else ['\\', n] in
    value ++ regexReplacement text matchIndex matches xs

regexReplacement text matchIndex matches (x : xs) = x : regexReplacement text matchIndex matches xs


editReplaceAll :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editReplaceAll entireWord caseSensitive wrapAround regex search replace hint = do
    res <- editReplace' entireWord caseSensitive False regex search replace hint True
    if res
        then editReplaceAll entireWord caseSensitive False regex search replace hint
        else return False

compileRegex :: Bool -> Text -> Either String Regex
compileRegex caseSense searchString =
    let compOption = defaultCompOpt {
                            Regex.caseSensitive = caseSense
                        ,   multiline = True } in
    compile compOption defaultExecOpt searchString

red = Color 64000 10000 10000
orange = Color 64000 48000 0
white = Color 64000 64000 64000
black = Color 0 0 0

needFindbar :: IDEM (Toolbar,ListStore Text)
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
        Initial -> inActiveBufContext () $ \_ _ ebuf _ _ -> do
            hasSelection <- hasSelection ebuf
            when hasSelection $ do
                (i1,i2)   <- getSelectionBounds ebuf
                text      <- getText ebuf i1 i2 False
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

getWidget :: Text -> Toolbar -> IO Widget
getWidget str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (liftM (== str) . widgetGetName) widgets
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

getSelection :: Text -> Toolbar -> IO Bool
getSelection str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (liftM (== str) . widgetGetName) widgets
    case entryL of
        [w] -> toggleToolButtonGetActive (castToToggleToolButton w)
        _   -> throwIDE "Find>>getIt widget not found"

setEntireWord, setWrapAround, setCaseSensitive, setRegex :: Toolbar -> Bool -> IO ()
setEntireWord    = setSelection "entireWordButton"
setWrapAround    = setSelection "wrapAroundButton"
setCaseSensitive = setSelection "caseSensitiveButton"
setRegex         = setSelection "regexButton"

setSelection :: Text -> Toolbar -> Bool ->  IO ()
setSelection str tb bool = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (liftM (== str) . widgetGetName ) widgets
    case entryL of
        [w] -> toggleToolButtonSetActive (castToToggleToolButton w) bool
        _   -> throwIDE "Find>>getIt widget not found"
