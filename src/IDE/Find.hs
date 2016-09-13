{-# LANGUAGE CPP #-}
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

import Control.Applicative (Applicative)
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
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, singleton, isPrefixOf, length, null, toLower)
import Data.Monoid ((<>))
import GI.Gtk.Objects.SpinButton
       (spinButtonSetRange, spinButtonNewWithRange, spinButtonSetValue,
        SpinButton(..), spinButtonGetValueAsInt)
import Data.GI.Base (set, unsafeCastTo)
import GI.Gtk.Objects.Entry
       (afterEntryActivate, onEntryActivate, entrySetCompletion,
        entrySetPlaceholderText, entryNew, entrySetText, entryGetText,
        Entry(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, SeqStore(..), seqStoreNew, seqStoreAppend,
        seqStoreClear, seqStoreToList)
import GI.Gtk.Objects.Widget
       (widgetGetName, widgetModifyText, widgetModifyBase,
        afterWidgetFocusInEvent, onWidgetKeyPressEvent,
        onWidgetFocusInEvent, setWidgetTooltipText, Widget(..), widgetSetName,
        widgetGrabFocus, widgetShowAll, widgetHide)
import GI.Gtk.Objects.Toolbar
       (toolbarInsert, toolbarSetIconSize, toolbarSetStyle, toolbarNew,
        Toolbar(..))
import GI.Gtk.Enums (StateType(..), IconSize(..), ToolbarStyle(..))
import GI.Gtk.Objects.ToolButton
       (toolButtonSetLabel, onToolButtonClicked, toolButtonNew,
        toolButtonNewFromStock)
import GI.Gtk.Objects.ToolItem (toolItemSetExpand, toolItemNew)
import GI.Gtk.Objects.Container
       (containerGetChildren, containerChildSetProperty, containerAdd)
import GI.Gtk.Objects.Label (labelNew)
import GI.Gtk.Objects.SeparatorToolItem (separatorToolItemNew)
import GI.Gtk.Objects.ToggleToolButton
       (ToggleToolButton(..), toggleToolButtonSetActive,
        toggleToolButtonGetActive, toggleToolButtonNew)
import Data.GI.Gtk.ModelView.TreeModel (makeColumnIdString, treeModelGetPath, treeModelGetValue)
import Data.GI.Gtk.ModelView.CustomStore (customStoreSetColumn)
import GI.Gtk.Objects.EntryCompletion
       (EntryCompletion(..), onEntryCompletionMatchSelected,
        entryCompletionSetMatchFunc, setEntryCompletionModel,
        entryCompletionNew)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction, cellLayoutPackStart)
import GI.Gtk.Interfaces.Editable
       (afterEditableDeleteText, afterEditableInsertText)
import GI.Gdk.Structs.EventKey
       (getEventKeyState, getEventKeyKeyval)
import GI.Gdk.Functions (keyvalName)
import GI.Gdk.Flags (ModifierType(..))
import Data.GI.Base.GValue (IsGValue(..))
import GI.Gtk.Structs.TreeIter (TreeIter(..))
import GI.Gtk.Objects.Bin (Bin(..), binGetChild)
import IDE.Core.Types (toGdkColor)
import GI.Gtk.Structs.TreePath (treePathGetIndices)

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
    do
        lineNr        <- getLineEntry fb >>= spinButtonGetValueAsInt
        replaceStr    <- getReplaceEntry fb >>= entryGetText
        entryStr      <- getFindEntry fb >>= entryGetText
        entryHist     <- seqStoreToList ls
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
            ,   lineNr          =   fromIntegral lineNr}

setFindState :: FindState -> IDEAction
setFindState fs = do
    (fb,ls)      <- needFindbar
    getLineEntry fb >>= (\sb -> spinButtonSetValue sb (fromIntegral (lineNr fs)))
    getReplaceEntry fb >>= (\e -> entrySetText e (replaceStr fs))
    getFindEntry fb >>= (\e -> entrySetText e (entryStr fs))
    seqStoreClear ls
    mapM_ (seqStoreAppend ls) (entryHist fs)
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
            widgetHide tb

showToolbar :: IDEAction
showToolbar = do
    (_,mbtb) <- readIDE toolbar
    case mbtb of
        Nothing -> return ()
        Just tb -> do
            modifyIDE_ (\ide -> ide{toolbar = (True,snd (toolbar ide))})
            widgetShowAll tb

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
        Just (fb,_) -> widgetHide fb

showFindbar :: IDEAction
showFindbar = do
    (_,mbfb) <- readIDE findbar
    modifyIDE_ (\ide -> ide{findbar = (True,mbfb)})
    case mbfb of
        Nothing -> return ()
        Just (fb,_) -> widgetShowAll fb

focusFindEntry :: IDEAction
focusFindEntry = do
    (fb,_) <- needFindbar
    do
        entry <- getFindEntry fb
        widgetGrabFocus entry

toggleFindbar :: IDEAction
toggleFindbar = do
    findbar <- readIDE findbar
    if fst findbar
        then hideFindbar
        else showFindbar

constructFindReplace :: IDEM Toolbar
constructFindReplace = do
    ideR <- ask
    toolbar <- toolbarNew
    toolbarSetStyle toolbar ToolbarStyleIcons
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
    label3 <- labelNew (Just (__"Goto Line :"))
    containerAdd labelTool3 label3
    toolbarInsert toolbar labelTool3 0

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar sep1 0

    let performGrep = reflectIDE (packageTry $ doGrep toolbar) ideR
    grepButton <- toolButtonNew (Nothing :: Maybe Widget) (Just (__"Grep"))
    toolbarInsert toolbar grepButton 0
    onToolButtonClicked grepButton performGrep
    setWidgetTooltipText grepButton $ __"Search in multiple files"

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar sep1 0

    replaceAllButton <- toolButtonNew (Nothing :: Maybe Widget) (Just (__"Replace All"))
    toolbarInsert toolbar replaceAllButton 0

    replaceButton <- toolButtonNewFromStock "gtk-find-and-replace"
    toolbarInsert toolbar replaceButton 0

    replaceTool <- toolItemNew
    rentry <- entryNew
    widgetSetName rentry "replaceEntry"
    entrySetPlaceholderText rentry (Just "Replace with")
    containerAdd replaceTool rentry
    widgetSetName replaceTool "replaceTool"
    toolbarInsert toolbar replaceTool 0

    sep2 <- separatorToolItemNew
    toolbarInsert toolbar sep2 0

    nextButton <- toolButtonNewFromStock "gtk-go-forward"
    toolbarInsert toolbar nextButton 0
    setWidgetTooltipText nextButton $ __"Search for the next match in the current file"
    nextButton `onToolButtonClicked` doSearch toolbar Forward ideR

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just (__"Wrap"))
    widgetSetName wrapAroundButton "wrapAroundButton"
    toolbarInsert toolbar wrapAroundButton 0
    setWidgetTooltipText wrapAroundButton $ __"When selected searching will continue from the top when no more matches are found"

    previousButton <- toolButtonNewFromStock "gtk-go-back"
    toolbarInsert toolbar previousButton 0
    setWidgetTooltipText previousButton $ __"Search for the previous match in the current file"
    previousButton `onToolButtonClicked` doSearch toolbar Backward ideR

    entryTool <- toolItemNew
    entry <- entryNew
    widgetSetName entry "searchEntry"
    entrySetPlaceholderText entry (Just "Find")
    containerAdd entryTool entry
    widgetSetName entryTool "searchEntryTool"
    toolItemSetExpand entryTool True
    toolbarInsert toolbar entryTool 0

    let column0 = makeColumnIdString 0
    store <- seqStoreNew []
    customStoreSetColumn store column0 id

    completion <- entryCompletionNew
    entrySetCompletion entry (Just completion)

    setEntryCompletionModel completion store
    cell <- cellRendererTextNew
    cellLayoutPackStart completion cell True
    cellLayoutSetDataFunction completion cell store
        (setCellRendererTextText cell)
    entryCompletionSetMatchFunc completion (matchFunc store)
    onEntryCompletionMatchSelected completion $ \ model iter -> do
        txt <- treeModelGetValue model iter column0
        entrySetText entry txt
        doSearch toolbar Forward ideR
        return True

    regexButton <- toggleToolButtonNew
    toolButtonSetLabel regexButton (Just (__"Regex"))
    widgetSetName regexButton "regexButton"
    toolbarInsert toolbar regexButton 0
    onToolButtonClicked regexButton $ doSearch toolbar Insert ideR
    setWidgetTooltipText regexButton $ __"When selected the search string is used as a regular expression"

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just (__"Words"))
    widgetSetName entireWordButton "entireWordButton"
    toolbarInsert toolbar entireWordButton 0
    entireWordButton `onToolButtonClicked` doSearch toolbar Insert ideR
    setWidgetTooltipText entireWordButton $ __"When selected only entire words are matched"

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just (__"Case"))
    widgetSetName caseSensitiveButton "caseSensitiveButton"
    toolbarInsert toolbar caseSensitiveButton 0
    caseSensitiveButton `onToolButtonClicked`
       doSearch toolbar Insert ideR
    setWidgetTooltipText caseSensitiveButton $ __"When selected the search is case sensitive"

    afterEditableInsertText entry (\ t _ i -> do
        doSearch toolbar Insert ideR
        return i)
    afterEditableDeleteText entry (\ _ _ -> doSearch toolbar Delete ideR)

    onEntryActivate entry $ doSearch toolbar Forward ideR
    onIDE onWidgetFocusInEvent entry $ do
        liftIDE $ triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
        return False

    onToolButtonClicked replaceButton $ replace toolbar Forward ideR
    let performReplaceAll = replaceAll toolbar Initial ideR
    onToolButtonClicked replaceAllButton performReplaceAll

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

    onWidgetKeyPressEvent entry $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
            Just "Return" | ModifierTypeShiftMask `elem`  mods ->
                                doSearch toolbar Backward ideR >> return True
            Just "Down"   -> doSearch toolbar Forward ideR >> return True
            Just "Up"     -> doSearch toolbar Backward ideR >> return True
            Just "Escape" -> getOut ideR >> return True
            Just "Tab"    -> do
                re <- getReplaceEntry toolbar
                widgetGrabFocus re
                --- widgetAc
                return True
            Just name | mapControlCommand ModifierTypeControlMask `elem` mods -> ctrl $ T.toLower name
            _        -> return False

    onWidgetKeyPressEvent rentry $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
           Just name
             | name == "Tab" || name == "ISO_Left_Tab" -> do
                    fe <- getFindEntry toolbar
                    widgetGrabFocus fe
                    return True
             | mapControlCommand ModifierTypeControlMask `elem` mods ->
                        ctrl $ T.toLower name
           _ -> return False

    onIDE afterWidgetFocusInEvent spinL . liftIDE $ inActiveBufContext True $ \ _ _ ebuf _ _ -> do
        max <- getLineCount ebuf
        spinButtonSetRange spinL 1.0 (fromIntegral max)
        return True

    onWidgetKeyPressEvent spinL $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
            Just "Escape" -> getOut ideR >> return True
            Just "Tab"    -> do
                re <- getFindEntry toolbar
                widgetGrabFocus re
                return True
            Just name | mapControlCommand ModifierTypeControlMask `elem` mods -> ctrl $ T.toLower name
            _ -> return False

    afterEntryActivate spinL . (`reflectIDE` ideR) $ inActiveBufContext () $ \ _ sv ebuf _ _ -> do
        line <- spinButtonGetValueAsInt spinL
        iter <- getIterAtLine ebuf (fromIntegral line - 1)
        placeCursor ebuf iter
        scrollToIter sv iter 0.2 Nothing
        getOut ideR
        return ()

    onToolButtonClicked closeButton $ reflectIDE hideFindbar ideR

    liftIO $ do
        containerChildSetProperty toolbar spinTool "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar wrapAroundButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar entireWordButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar caseSensitiveButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar regexButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar replaceAllButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar labelTool3 "homogeneous" =<< toGValue False

    modifyIDE_ (\ ide -> ide{findbar = (False, Just (toolbar, store))})
    return toolbar
  where
    getOut ideR = liftIO $ reflectIDE (do
        hideFindbar
        maybeActiveBuf ?>>= makeActive) ideR


doSearch :: Toolbar -> SearchHint -> IDERef -> IO ()
doSearch fb hint ideR   = do
    entry         <- getFindEntry fb
    search        <- entryGetText entry
    entireWord    <- getEntireWord fb
    caseSensitive <- getCaseSensitive fb
    wrapAround    <- getWrapAround fb
    regex         <- getRegex fb
    mbExpAndMatchIndex <- regexAndMatchIndex caseSensitive entireWord regex search
    case mbExpAndMatchIndex of
        Just (exp, matchIndex) -> do
            res           <- reflectIDE (editFind entireWord caseSensitive wrapAround regex search "" hint) ideR
            if res || T.null search
                then do
                    widgetModifyBase entry StateTypeNormal . Just =<< toGdkColor white
                    widgetModifyText entry StateTypeNormal . Just =<< toGdkColor black
                else do
                    widgetModifyBase entry StateTypeNormal . Just =<< toGdkColor red
                    widgetModifyText entry StateTypeNormal . Just =<< toGdkColor white
        Nothing ->
            if T.null search
                then do
                    widgetModifyBase entry StateTypeNormal . Just =<< toGdkColor white
                    widgetModifyText entry StateTypeNormal . Just =<< toGdkColor black
                else do
                    widgetModifyBase entry StateTypeNormal . Just =<< toGdkColor orange
                    widgetModifyText entry StateTypeNormal . Just =<< toGdkColor black
    reflectIDE (addToHist search) ideR
    return ()

doGrep :: Toolbar -> PackageAction
doGrep fb   = do
    package       <- ask
    ideR          <- lift ask
    entry         <- getFindEntry fb
    search        <- entryGetText entry
    entireWord    <- getEntireWord fb
    caseSensitive <- getCaseSensitive fb
    wrapAround    <- getWrapAround fb
    regex         <- getRegex fb
    let (regexString, _) = regexStringAndMatchIndex entireWord regex search
    liftIDE $ workspaceTry $ grepWorkspace regexString caseSensitive

matchFunc :: (Applicative m, MonadIO m) => SeqStore Text -> EntryCompletion -> Text -> TreeIter -> m Bool
matchFunc model completion str iter = do
  tp <- treeModelGetPath model iter >>= treePathGetIndices
  case tp of
         (i:_) -> do row <- seqStoreGetValue model i
                     return (T.isPrefixOf (T.toLower str) (T.toLower row) && T.length str < T.length row)
         _     -> return False

addToHist :: Text -> IDEAction
addToHist str =
    unless (T.null str) $ do
        (_, ls) <- needFindbar
        entryHist <- seqStoreToList ls
        unless (any (str `T.isPrefixOf`) entryHist) $ do
            let newList = take 12 (str : filter (\ e -> not (e `T.isPrefixOf` str)) entryHist)
            seqStoreClear ls
            mapM_ (seqStoreAppend ls) newList

replace :: Toolbar -> SearchHint -> IDERef -> IO ()
replace fb hint ideR   =  do
    entry          <- getFindEntry fb
    search         <- entryGetText entry
    rentry         <- getReplaceEntry fb
    replace        <- entryGetText rentry
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
    search         <- entryGetText entry
    rentry         <- getReplaceEntry fb
    replace        <- entryGetText rentry
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    wrapAround     <- getWrapAround fb
    regex          <- getRegex fb
    found <- reflectIDE (editReplaceAll entireWord caseSensitive regex search replace hint)
                ideR
    return ()

editFind :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround regex search dummy hint = do
    mbExpAndMatchIndex <- regexAndMatchIndex caseSensitive entireWord regex search
    case mbExpAndMatchIndex of
        Nothing -> return False
        Just (exp, matchIndex) -> editFind' exp matchIndex wrapAround dummy hint

editFind' :: Regex -> Int -> Bool -> Text -> SearchHint -> IDEM Bool
editFind' exp matchIndex wrapAround dummy hint =
    inActiveBufContext False $ \_ sv ebuf _ _ -> do
        i1 <- getStartIter ebuf
        i2 <- getEndIter ebuf
        text <- getText ebuf i1 i2 True
        removeTagByName ebuf "search-match"
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
                applyTagByName ebuf "search-match" start end
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

regexAndMatchIndex :: MonadIO m => Bool -> Bool -> Bool -> Text -> m (Maybe (Regex, Int))
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
    editReplace' entireWord caseSensitive wrapAround regex search replace hint False

editReplace' :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replace hint fromStart =
    inActiveBufContext False $ \_ _ ebuf _ _ -> do
        insertMark <- getInsertMark ebuf
        iter       <- getIterAtMark ebuf insertMark
        offset   <- getOffset iter
        let offset' = if fromStart then 0 else offset
        mbExpAndMatchIndex <- regexAndMatchIndex caseSensitive entireWord regex search
        case mbExpAndMatchIndex of
            Just (exp, matchIndex) -> do
                iStart <- getStartIter ebuf
                iEnd   <- getEndIter ebuf
                text   <- getText ebuf iStart iEnd True
                match  <- findMatch exp matchIndex ebuf text (== offset') False
                case match of
                    Just (iterStart, iterEnd, matches) -> do
                        mbText <- replacementText regex text matchIndex matches $ T.unpack replace
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

editReplaceAll :: Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editReplaceAll = editReplaceAll' True

editReplaceAll' :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editReplaceAll' fromStart entireWord caseSensitive regex search replace hint = do
    res <- editReplace' entireWord caseSensitive False regex search replace hint fromStart
    if res
        then editReplaceAll' False entireWord caseSensitive regex search replace hint
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

needFindbar :: IDEM (Toolbar,SeqStore Text)
needFindbar = do
    (_,mbfb) <- readIDE findbar
    case mbfb of
        Nothing -> throwIDE "Find>>needFindbar: Findbar not initialized"
        Just p  -> return p

-- | Find action. If SearchHint==Initial then we'll use the selected text or current identifier to populate the find text box
editFindInc :: SearchHint -> IDEAction
editFindInc hint = do
    ideR <- ask
    (fb,_) <- needFindbar
    case hint of
        Initial -> do
               mbtext <- selectedTextOrCurrentIdentifier -- if no text selected, search for current identifier
               case mbtext of
                 Just text -> do
                     findEntry <- getFindEntry fb
                     entrySetText findEntry text
                 Nothing -> return ()
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
    entry <- getLineEntry fb
    widgetGrabFocus entry

getLineEntry :: (Applicative m, MonadIO m) => Toolbar -> m SpinButton
getLineEntry tb    = getWidget "gotoLineEntryTool" tb >>= liftIO . unsafeCastTo SpinButton

getReplaceEntry, getFindEntry :: (Applicative m, MonadIO m) => Toolbar -> m Entry
getReplaceEntry tb = getWidget "replaceTool" tb >>= liftIO . unsafeCastTo Entry
getFindEntry tb    = getWidget "searchEntryTool" tb >>= liftIO . unsafeCastTo Entry

getWidget :: (Applicative m, MonadIO m) => Text -> Toolbar -> m Widget
getWidget str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (fmap (== str) . widgetGetName) widgets
    case entryL of
        [w] -> liftIO (unsafeCastTo Bin w) >>= binGetChild
        _   -> throwIDE "Find>>getWidget not found(2)"

getEntireWord, getWrapAround, getCaseSensitive, getRegex :: (Applicative m, MonadIO m) => Toolbar -> m Bool
getEntireWord    = getSelection "entireWordButton"
getWrapAround    = getSelection "wrapAroundButton"
getCaseSensitive = getSelection "caseSensitiveButton"
getRegex         = getSelection "regexButton"

getSelection :: (Applicative m, MonadIO m) => Text -> Toolbar -> m Bool
getSelection str tb = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (fmap (== str) . widgetGetName) widgets
    case entryL of
        [w] -> liftIO (unsafeCastTo ToggleToolButton w) >>= toggleToolButtonGetActive
        _   -> throwIDE "Find>>getIt widget not found"

setEntireWord, setWrapAround, setCaseSensitive, setRegex :: (Applicative m, MonadIO m) => Toolbar -> Bool -> m ()
setEntireWord    = setSelection "entireWordButton"
setWrapAround    = setSelection "wrapAroundButton"
setCaseSensitive = setSelection "caseSensitiveButton"
setRegex         = setSelection "regexButton"

setSelection :: (Applicative m, MonadIO m) => Text -> Toolbar -> Bool ->  m ()
setSelection str tb bool = do
    widgets <- containerGetChildren tb
    entryL <-  filterM (fmap (== str) . widgetGetName ) widgets
    case entryL of
        [w] -> liftIO (unsafeCastTo ToggleToolButton w) >>= \ttb -> toggleToolButtonSetActive ttb bool
        _   -> throwIDE "Find>>getIt widget not found"
