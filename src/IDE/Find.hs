{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
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

import Prelude ()
import Prelude.Compat
import Control.Applicative (Applicative)
import IDE.Core.State
       (MessageLevel(..), sysMessage, (?>>=), triggerEventIDE,
        reflectIDE, modifyIDE_, readIDE, WorkspaceAction, IDEAction, IDEM,
        IDERef, throwIDE, reifyIDE, ideGtk, modifyIDE)
import IDE.Gtk.State
       (onIDE, toolbar, findbar)
import IDE.Utils.GUIUtils
import IDE.TextEditor hiding(afterFocusIn)
import IDE.Pane.SourceBuffer
import Data.Char (digitToInt, isDigit, isAlphaNum)
import Text.Regex.TDFA hiding (caseSensitive, after)
import qualified Text.Regex.TDFA as Regex
import Text.Regex.TDFA.Text (compile)
import Data.List (find)
import Data.Array (bounds, (!), inRange)
import Data.Maybe (fromJust)
import Data.Function ((&))
import IDE.Pane.Grep (grepWorkspace)
import IDE.Gtk.Workspaces (workspaceTry)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad (filterM, unless, void)
import Control.Lens (pre, _Just, _1, _2, (.~), (^?))
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, singleton, isPrefixOf, length, null, toLower)
import qualified Data.Text.Encoding as T (encodeUtf8)
import GI.Gtk.Objects.SpinButton
       (spinButtonSetRange, spinButtonNewWithRange, spinButtonSetValue,
        SpinButton(..), spinButtonGetValueAsInt)
import Data.GI.Base (unsafeCastTo)
import GI.Gtk.Objects.Entry
       (afterEntryActivate, onEntryActivate, entrySetCompletion,
        entrySetPlaceholderText, entryNew, entrySetText, entryGetText,
        Entry(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, SeqStore(..), seqStoreNew, seqStoreAppend,
        seqStoreClear, seqStoreToList)
import GI.Gtk.Objects.Widget
       (widgetGetName, afterWidgetFocusInEvent, onWidgetKeyPressEvent,
        onWidgetFocusInEvent, setWidgetTooltipText, Widget(..), widgetSetName,
        widgetGrabFocus, widgetShowAll, widgetHide)
import GI.Gtk.Objects.Toolbar
       (toolbarInsert, toolbarSetIconSize, toolbarSetStyle, toolbarNew,
        Toolbar(..))
import GI.Gtk.Enums (IconSize(..), ToolbarStyle(..))
import GI.Gtk.Objects.ToolButton
       (toolButtonSetLabel, onToolButtonClicked, toolButtonNew)
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
import IDE.Core.Types
       (SensitivityMask(..), IDEEvent(..), MonadIDE(..), SearchHint(..))
import GI.Gtk.Structs.TreePath (treePathGetIndices)
import GI.Gtk
       (CssProvider, pattern STYLE_PROVIDER_PRIORITY_APPLICATION,
        styleContextAddProvider, cssProviderLoadFromData,
        widgetGetStyleContext, cssProviderNew,
        noWidget, imageNewFromIconName)
import Graphics.UI.Frame.Panes (RecoverablePane(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

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
    deriving(Eq,Ord,Show,Read,Generic)

instance ToJSON FindState
instance FromJSON FindState

getFindState :: IDEM FindState
getFindState = do
    (fb,ls,_) <- needFindbar
    do
        lineNr        <- fmap fromIntegral $ getLineEntry fb >>= spinButtonGetValueAsInt
        replaceStr    <- getReplaceEntry fb >>= entryGetText
        entryStr      <- getFindEntry fb >>= entryGetText
        entryHist     <- seqStoreToList ls
        entireWord    <- getEntireWord fb
        wrapAround    <- getWrapAround fb
        caseSensitive <- getCaseSensitive fb
        regex         <- getRegex fb
        let replaceHist= []
        return FindState{..}

setFindState :: FindState -> IDEAction
setFindState fs = do
    (fb,ls,_)      <- needFindbar
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
hideToolbar =
    readIDE (pre $ ideGtk . _Just . toolbar) >>= mapM_ (\(_,mbtb) ->
        case mbtb of
            Nothing -> return ()
            Just tb -> do
                modifyIDE_ $ ideGtk . _Just . toolbar . _1 .~ False
                widgetHide tb)

showToolbar :: IDEAction
showToolbar =
    readIDE (pre $ ideGtk . _Just . toolbar) >>= mapM_ (\(_,mbtb) ->
        case mbtb of
            Nothing -> return ()
            Just tb -> do
                modifyIDE_ $ ideGtk . _Just . toolbar . _1 .~ True
                widgetShowAll tb)

toggleToolbar :: IDEAction
toggleToolbar =
    readIDE (pre $ ideGtk . _Just . toolbar) >>= mapM_ (\toolbar' ->
        if fst toolbar'
            then hideToolbar
            else showToolbar)

hideFindbar :: IDEAction
hideFindbar =
    modifyIDE (\ide -> ( ide &  ideGtk . _Just . findbar . _1 .~ False
                       , ide ^? ideGtk . _Just . findbar . _2)) >>= mapM_ (\case
        Nothing -> return ()
        Just (fb,_,_) -> do
            widgetHide fb
            void $ inActiveBufContext False $ \_ ebuf _ -> do
                removeTagByName ebuf "search-match"
                return True)

showFindbar :: IDEAction
showFindbar =
    modifyIDE (\ide -> ( ide &  ideGtk . _Just . findbar . _1 .~ True
                       , ide ^? ideGtk . _Just . findbar . _2)) >>= mapM_ (\case
        Nothing -> return ()
        Just (fb,_,_) -> widgetShowAll fb)

focusFindEntry :: IDEAction
focusFindEntry = do
    (fb,_,_) <- needFindbar
    do
        entry <- getFindEntry fb
        widgetGrabFocus entry

toggleFindbar :: IDEAction
toggleFindbar =
    readIDE (pre $ ideGtk . _Just . findbar) >>= mapM_ (\findbar' ->
        if fst findbar'
            then hideFindbar
            else showFindbar)

constructFindReplace :: IDEM Toolbar
constructFindReplace = do
    ideR <- ask
    toolbar' <- toolbarNew
    toolbarSetStyle toolbar' ToolbarStyleIcons
    toolbarSetIconSize toolbar' IconSizeSmallToolbar
    let newButtonFromIconName name = imageNewFromIconName
                                        (Just name)
                                        (fromIntegral $ fromEnum IconSizeSmallToolbar)
                                    >>= (`toolButtonNew` Nothing) . Just
    closeButton <- newButtonFromIconName "window-close"
    toolbarInsert toolbar' closeButton 0

    entryCssProvider <- cssProviderNew
    let doSearch = doSearch' toolbar' entryCssProvider

    spinTool <- toolItemNew
    spinL <- spinButtonNewWithRange 1.0 1000.0 10.0
    widgetSetName spinL "gotoLineEntry"
    containerAdd spinTool spinL
    widgetSetName spinTool "gotoLineEntryTool"
    toolbarInsert toolbar' spinTool 0

    labelTool3 <- toolItemNew
    label3 <- labelNew (Just (__"Goto Line :"))
    containerAdd labelTool3 label3
    toolbarInsert toolbar' labelTool3 0

    sep1 <- separatorToolItemNew
    toolbarInsert toolbar' sep1 0

    let performGrep = reflectIDE (workspaceTry $ doGrep toolbar') ideR
    grepButton <- toolButtonNew noWidget (Just (__"Grep"))
    toolbarInsert toolbar' grepButton 0
    _ <- onToolButtonClicked grepButton performGrep
    setWidgetTooltipText grepButton $ __"Search in multiple files"

    sep2 <- separatorToolItemNew
    toolbarInsert toolbar' sep2 0

    replaceAllButton <- toolButtonNew noWidget (Just (__"Replace All"))
    toolbarInsert toolbar' replaceAllButton 0

    replaceButton <- newButtonFromIconName "edit-find-replace"
    toolbarInsert toolbar' replaceButton 0

    replaceTool <- toolItemNew
    rentry <- entryNew
    widgetSetName rentry "replaceEntry"
    entrySetPlaceholderText rentry (Just "Replace with")
    containerAdd replaceTool rentry
    widgetSetName replaceTool "replaceTool"
    toolbarInsert toolbar' replaceTool 0

    sep3 <- separatorToolItemNew
    toolbarInsert toolbar' sep3 0

    nextButton <- newButtonFromIconName "go-next"
    toolbarInsert toolbar' nextButton 0
    setWidgetTooltipText nextButton $ __"Search for the next match in the current file"
    _ <- onToolButtonClicked nextButton $ doSearch Forward ideR

    wrapAroundButton <- toggleToolButtonNew
    toolButtonSetLabel wrapAroundButton (Just (__"Wrap"))
    widgetSetName wrapAroundButton "wrapAroundButton"
    toolbarInsert toolbar' wrapAroundButton 0
    setWidgetTooltipText wrapAroundButton $ __"When selected searching will continue from the top when no more matches are found"

    previousButton <- newButtonFromIconName "go-previous"
    toolbarInsert toolbar' previousButton 0
    setWidgetTooltipText previousButton $ __"Search for the previous match in the current file"
    _ <- onToolButtonClicked previousButton $ doSearch Backward ideR

    entryTool <- toolItemNew
    entry <- entryNew
    styleContext <- widgetGetStyleContext entry
    styleContextAddProvider styleContext entryCssProvider (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)
    widgetSetName entry "searchEntry"
    entrySetPlaceholderText entry (Just "Find")
    containerAdd entryTool entry
    widgetSetName entryTool "searchEntryTool"
    toolItemSetExpand entryTool True
    toolbarInsert toolbar' entryTool 0

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
    _ <- onEntryCompletionMatchSelected completion $ \ model iter -> do
        txt <- treeModelGetValue model iter column0
        entrySetText entry txt
        doSearch Forward ideR
        return True

    regexButton <- toggleToolButtonNew
    toolButtonSetLabel regexButton (Just (__"Regex"))
    widgetSetName regexButton "regexButton"
    toolbarInsert toolbar' regexButton 0
    _ <- onToolButtonClicked regexButton $ doSearch Insert ideR
    setWidgetTooltipText regexButton $ __"When selected the search string is used as a regular expression"

    entireWordButton <- toggleToolButtonNew
    toolButtonSetLabel entireWordButton (Just (__"Words"))
    widgetSetName entireWordButton "entireWordButton"
    toolbarInsert toolbar' entireWordButton 0
    _ <- onToolButtonClicked entireWordButton $ doSearch Insert ideR
    setWidgetTooltipText entireWordButton $ __"When selected only entire words are matched"

    caseSensitiveButton <- toggleToolButtonNew
    toolButtonSetLabel caseSensitiveButton (Just (__"Case"))
    widgetSetName caseSensitiveButton "caseSensitiveButton"
    toolbarInsert toolbar' caseSensitiveButton 0
    _ <- onToolButtonClicked caseSensitiveButton $ doSearch Insert ideR
    setWidgetTooltipText caseSensitiveButton $ __"When selected the search is case sensitive"

    _ <- afterEditableInsertText entry (\ _t _ i -> do
        doSearch Insert ideR
        return i)
    _ <- afterEditableDeleteText entry (\ _ _ -> doSearch Delete ideR)

    _ <- onEntryActivate entry $ doSearch Forward ideR
    _ <- onIDE onWidgetFocusInEvent entry $ do
        _ <- liftIDE $ triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
        return False

    _ <- onToolButtonClicked replaceButton $ replace toolbar' Forward ideR
    let performReplaceAll = replaceAll toolbar' Initial ideR
    _ <- onToolButtonClicked replaceAllButton performReplaceAll

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

    _ <- onWidgetKeyPressEvent entry $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
            Just "Return" | ModifierTypeShiftMask `elem`  mods ->
                                doSearch Backward ideR >> return True
            Just "Down"   -> doSearch Forward ideR >> return True
            Just "Up"     -> doSearch Backward ideR >> return True
            Just "Escape" -> getOut ideR >> return True
            Just "Tab"    -> do
                re <- getReplaceEntry toolbar'
                widgetGrabFocus re
                --- widgetAc
                return True
            Just name | mapControlCommand ModifierTypeControlMask `elem` mods -> ctrl $ T.toLower name
            _        -> return False

    _ <- onWidgetKeyPressEvent rentry $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
           Just name
             | name == "Tab" || name == "ISO_Left_Tab" -> do
                    fe <- getFindEntry toolbar'
                    widgetGrabFocus fe
                    return True
             | mapControlCommand ModifierTypeControlMask `elem` mods ->
                        ctrl $ T.toLower name
           _ -> return False

    _ <- onIDE afterWidgetFocusInEvent spinL . liftIDE $ inActiveBufContext True $ \_ ebuf _ -> do
        maxLine <- getLineCount ebuf
        spinButtonSetRange spinL 1.0 (fromIntegral maxLine)
        return True

    _ <- onWidgetKeyPressEvent spinL $ \e -> do
        mbName <- getEventKeyKeyval e >>= keyvalName
        mods <- getEventKeyState e
        case mbName of
            Just "Escape" -> getOut ideR >> return True
            Just "Tab"    -> do
                re <- getFindEntry toolbar'
                widgetGrabFocus re
                return True
            Just name | mapControlCommand ModifierTypeControlMask `elem` mods -> ctrl $ T.toLower name
            _ -> return False

    _ <- afterEntryActivate spinL . (`reflectIDE` ideR) $ inActiveBufContext () $ \sv ebuf _ -> do
        line <- spinButtonGetValueAsInt spinL
        iter <- getIterAtLine ebuf (fromIntegral line - 1)
        placeCursor ebuf iter
        scrollToIter sv iter 0.2 Nothing
        getOut ideR
        return ()

    _ <- onToolButtonClicked closeButton $ reflectIDE hideFindbar ideR

    liftIO $ do
        containerChildSetProperty toolbar' spinTool "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' wrapAroundButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' entireWordButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' caseSensitiveButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' regexButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' replaceAllButton "homogeneous" =<< toGValue False
        containerChildSetProperty toolbar' labelTool3 "homogeneous" =<< toGValue False

    modifyIDE_ $ ideGtk . _Just . findbar .~ (False, Just (toolbar', store, entryCssProvider))
    return toolbar'
  where
    getOut ideR = liftIO $ reflectIDE (do
        hideFindbar
        maybeActiveBuf ?>>= makeActive) ideR


doSearch' :: Toolbar -> CssProvider -> SearchHint -> IDERef -> IO ()
doSearch' fb entryCssProvider hint ideR   = do
    entry         <- getFindEntry fb
    search        <- entryGetText entry
    entireWord    <- getEntireWord fb
    caseSensitive <- getCaseSensitive fb
    wrapAround    <- getWrapAround fb
    regex         <- getRegex fb
    regexAndMatchIndex caseSensitive entireWord regex search >>= \case
        Just (_exp, _matchIndex) -> do
            res           <- reflectIDE (editFind entireWord caseSensitive wrapAround regex search "" hint) ideR
            if res || T.null search
                then cssProviderLoadFromData entryCssProvider $ T.encodeUtf8 "entry { }"
                else cssProviderLoadFromData entryCssProvider $ T.encodeUtf8 "entry { background-color: mix(@theme_base_color, red, 0.2); }"
        Nothing ->
            if T.null search
                then cssProviderLoadFromData entryCssProvider $ T.encodeUtf8 "entry { }"
                else cssProviderLoadFromData entryCssProvider $ T.encodeUtf8 "entry { background-color: mix(@theme_base_color, orange, 0.2); }"
    reflectIDE (addToHist search) ideR
    return ()

doGrep :: Toolbar -> WorkspaceAction
doGrep fb   = do
    entry         <- getFindEntry fb
    search        <- entryGetText entry
    entireWord    <- getEntireWord fb
    caseSensitive <- getCaseSensitive fb
    regex         <- getRegex fb
    let (regexString, _) = regexStringAndMatchIndex entireWord regex search
    liftIDE $ workspaceTry $ grepWorkspace regexString caseSensitive

matchFunc :: (Applicative m, MonadIO m) => SeqStore Text -> EntryCompletion -> Text -> TreeIter -> m Bool
matchFunc model _completion str iter = do
  tp <- treeModelGetPath model iter >>= treePathGetIndices
  case tp of
         Just (i:_) -> do row <- seqStoreGetValue model i
                          return (T.isPrefixOf (T.toLower str) (T.toLower row) && T.length str < T.length row)
         _     -> return False

addToHist :: Text -> IDEAction
addToHist str =
    unless (T.null str) $ do
        (_, ls, _) <- needFindbar
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
    replaceText    <- entryGetText rentry
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    wrapAround     <- getWrapAround fb
    regex          <- getRegex fb
    _found <- reflectIDE (editReplace entireWord caseSensitive wrapAround regex search replaceText hint)
                ideR
    return ()

replaceAll :: Toolbar -> SearchHint -> IDERef -> IO ()
replaceAll fb hint ideR   =  do
    entry          <- getFindEntry fb
    search         <- entryGetText entry
    rentry         <- getReplaceEntry fb
    replaceText    <- entryGetText rentry
    entireWord     <- getEntireWord fb
    caseSensitive  <- getCaseSensitive fb
    regex          <- getRegex fb
    _found <- reflectIDE (editReplaceAll entireWord caseSensitive regex search replaceText hint)
                ideR
    return ()

editFind :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editFind entireWord caseSensitive wrapAround regex search dummy hint =
    regexAndMatchIndex caseSensitive entireWord regex search >>= \case
        Nothing -> return False
        Just (regexp, matchIndex) -> editFind' regexp matchIndex wrapAround dummy hint

editFind' :: Regex -> Int -> Bool -> Text -> SearchHint -> IDEM Bool
editFind' regexp matchIndex wrapAround _dummy hint =
    inActiveBufContext False $ \sv ebuf _ -> do
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
                    mbsr <- backSearch ebuf text st3
                    case mbsr of
                        Nothing ->
                            if wrapAround
                                then backSearch ebuf text i2
                                else return Nothing
                        m -> return m
                else do
                    st2 <- if hint == Forward
                        then forwardCharC st1
                        else return st1
                    mbsr <- if hint == Initial
                            then initialSearch ebuf text
                            else forwardSearch' ebuf text st2
                    case mbsr of
                        Nothing ->
                            if wrapAround
                                then forwardSearch' ebuf text i1
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
        backSearch ebuf text iter = do
            offset <- getOffset iter
            findMatch regexp matchIndex ebuf text (<= offset) True

        forwardSearch' ebuf text iter = do
            offset <- getOffset iter
            findMatch regexp matchIndex ebuf text (>= offset) False

        initialSearch ebuf text = findMatch regexp matchIndex ebuf text (>= 0) False

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
                Right regexp -> return $ Just (regexp, index)

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
findMatch regexp matchIndex gtkbuf text offsetPred findLast = do
    let m = (if findLast then reverse else id) (matchAll regexp text)
    case find (offsetPred . fst . (!matchIndex)) m of
        Just matches -> do
            iterStart <- getStartIter gtkbuf
            iter1     <- forwardCharsC iterStart (fst (matches!matchIndex))
            iter2     <- forwardCharsC iter1 (snd (matches!matchIndex))
            return $ Just (iter1, iter2, matches)
        Nothing -> return Nothing

editReplace :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> IDEM Bool
editReplace entireWord caseSensitive wrapAround regex search replaceText hint =
    editReplace' entireWord caseSensitive wrapAround regex search replaceText hint False

editReplace' :: Bool -> Bool -> Bool -> Bool -> Text -> Text -> SearchHint -> Bool -> IDEM Bool
editReplace' entireWord caseSensitive wrapAround regex search replaceText hint fromStart =
    inActiveBufContext False $ \_ ebuf _ -> do
        insertMark <- getInsertMark ebuf
        iter       <- getIterAtMark ebuf insertMark
        offset   <- getOffset iter
        let offset' = if fromStart then 0 else offset
        mbExpAndMatchIndex <- regexAndMatchIndex caseSensitive entireWord regex search
        case mbExpAndMatchIndex of
            Just (regexp, matchIndex) -> do
                iStart <- getStartIter ebuf
                iEnd   <- getEndIter ebuf
                text   <- getText ebuf iStart iEnd True
                findMatch regexp matchIndex ebuf text (== offset') False >>= \case
                    Just (iterStart, iterEnd, matches) -> do
                        replacementText regex text matchIndex matches >>= \case
                            Just t -> do
                                beginUserAction ebuf
                                delete ebuf iterStart iterEnd
                                insert ebuf iterStart t
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
                                    replaceText hint False
                            else return False
            Nothing -> return False
    where
        replacementText False _ _ _ = return $ Just replaceText
        replacementText True text matchIndex matches =
            case compileRegex caseSensitive search of
                Left err -> do
                    sysMessage Normal $ T.pack err
                    return Nothing
                Right _regexp -> return . Just . T.pack $ regexReplacement text matchIndex matches (T.unpack replaceText)

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
editReplaceAll' fromStart entireWord caseSensitive regex search replaceText hint = do
    res <- editReplace' entireWord caseSensitive False regex search replaceText hint fromStart
    if res
        then editReplaceAll' False entireWord caseSensitive regex search replaceText hint
        else return False

compileRegex :: Bool -> Text -> Either String Regex
compileRegex caseSense searchString =
    let compOption = defaultCompOpt {
                            Regex.caseSensitive = caseSense
                        ,   multiline = True } in
    compile compOption defaultExecOpt searchString

needFindbar :: IDEM (Toolbar,SeqStore Text,CssProvider)
needFindbar =
    readIDE (pre $ ideGtk . _Just . findbar . _2) >>= \case
        Nothing -> throwIDE "Find>>needFindbar: No GTK"
        Just Nothing -> throwIDE "Find>>needFindbar: Findbar not initialized"
        Just (Just p) -> return p

-- | Find action. If SearchHint==Initial then we'll use the selected text or current identifier to populate the find text box
editFindInc :: SearchHint -> IDEAction
editFindInc hint = do
    (fb,_,entryCssProvider) <- needFindbar
    case hint of
        Initial -> do
               mbtext <- snd <$> selectedText -- if no text selected, search for the last query
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
            Forward  -> doSearch' fb entryCssProvider Forward ideR
            Backward -> doSearch' fb entryCssProvider Backward ideR
            _        -> return ()

editGotoLine :: IDEAction
editGotoLine = do
    showFindbar
    (fb,_,_) <- needFindbar
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
        [w] -> liftIO (unsafeCastTo Bin w) >>= (fmap fromJust . binGetChild)
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
