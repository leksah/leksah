{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.SourceBuffer
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The source editor part of Leksah
--
-----------------------------------------------------------------------------------

module IDE.Pane.SourceBuffer (
    IDEBuffer(..)
,   BufferState(..)

,   allBuffers
,   maybeActiveBuf
,   selectSourceBuf
,   goToSourceDefinition
,   goToSourceDefinition'
,   goToDefinition
,   insertInBuffer
,   replaceHLintSource

,   fileNew
,   fileOpenThis
,   fileOpen
,   filePrint
,   fileRevert
,   fileClose
,   fileCloseAll
,   fileCloseAllButPackage
,   fileCloseAllButWorkspace
,   fileSave
,   fileSaveAll
,   fileSaveBuffer
,   fileCheckAll
,   editUndo
,   editRedo
,   editCut
,   editCopy
,   editPaste
,   editDelete
,   editSelectAll

,   editComment
,   editUncomment
,   editShiftRight
,   editShiftLeft

,   editToCandy
,   editFromCandy
,   editKeystrokeCandy
,   editCandy

,   markRefInSourceBuf
,   inBufContext
,   inActiveBufContext

,   align
,   startComplete

,   selectedText
,   selectedTextOrCurrentLine
,   insertTextAfterSelection
,   selectedModuleName
,   selectedLocation
,   recentSourceBuffers
,   newTextBuffer
,   belongsToPackages
,   belongsToWorkspace
,   getIdentifierUnderCursorFromIter

) where

import Prelude hiding(getChar, getLine)
import Control.Applicative
import System.FilePath
import System.Directory
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List hiding(insert, delete)
import Data.Maybe
import Data.Char
import Data.Typeable

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.Utils.FileUtils
import IDE.SourceCandy
import IDE.SymbolNavigation
import IDE.Completion as Completion (complete,cancel)
import IDE.TextEditor
import Data.IORef (writeIORef,readIORef,newIORef)
import Control.Event (triggerEvent)
import IDE.Metainfo.Provider (getSystemInfo, getWorkspaceInfo)
import Graphics.UI.Gtk
       (Notebook, clipboardGet, selectionClipboard, dialogAddButton, widgetDestroy,
        fileChooserGetFilename, widgetShow, fileChooserDialogNew,
        notebookGetNthPage, notebookPageNum, widgetHide, dialogRun,
        messageDialogNew, scrolledWindowSetShadowType,
        scrolledWindowSetPolicy, dialogSetDefaultResponse,
        fileChooserSetCurrentFolder, fileChooserSelectFilename,
        TextSearchFlags(..))
import System.Glib.MainLoop (priorityDefaultIdle, idleAdd)
import qualified Graphics.UI.Gtk as Gtk hiding (eventKeyName)
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), PolicyType(..))
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..))
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
import Graphics.UI.Gtk
       (eventModifier, eventKeyName, eventKeyVal, Underline(..))
import Graphics.UI.Gtk.Selectors.FileChooser
       (FileChooserAction(..))
import System.Glib.Attributes (AttrOp(..), set)

import IDE.BufferMode
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM, filterM, unless, when, liftM)
import Control.Exception as E (catch, SomeException)

import qualified IDE.Command.Print as Print
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Log.Logger (debugM)
import Data.Text (Text)
import qualified Data.Text as T
       (length, findIndex, replicate, lines, dropWhileEnd, unlines, strip,
        null, pack, unpack)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (writeFile, readFile)

allBuffers :: MonadIDE m => m [IDEBuffer]
allBuffers = liftIDE getPanes

instance RecoverablePane IDEBuffer BufferState IDEM where
    saveState (p@IDEBuffer {sourceView=v}) = do
                            buf    <- getBuffer v
                            ins    <- getInsertMark buf
                            iter   <- getIterAtMark buf ins
                            offset <- getOffset iter
                            case fileName p of
                                Nothing ->  do
                                    ct      <-  readIDE candy
                                    text    <-  getCandylessText ct buf
                                    return (Just (BufferStateTrans (bufferName p) text offset))
                                Just fn ->  return (Just (BufferState fn offset))
    recoverState pp (BufferState n i) =   do
        mbbuf    <-  newTextBuffer pp (T.pack $ takeFileName n) (Just n)
        case mbbuf of
            Just (IDEBuffer {sourceView=v}) -> do
                ideR    <- ask
                liftIO . (`idleAdd` priorityDefaultIdle) . (`reflectIDE` ideR) $ do
                    liftIO $ debugM "leksah" "SourceBuffer recoverState idle callback"
                    gtkBuf  <- getBuffer v
                    iter    <- getIterAtOffset gtkBuf i
                    placeCursor gtkBuf iter
                    mark    <- getInsertMark gtkBuf
                    scrollToMark v mark 0.0 (Just (0.3,0.3))
                    liftIO $ debugM "leksah" "SourceBuffer recoverState done"
                    return False
                return mbbuf
            Nothing -> return Nothing
    recoverState pp (BufferStateTrans bn text i) =   do
        mbbuf    <-  newTextBuffer pp bn Nothing
        case mbbuf of
            Just (buf@IDEBuffer {sourceView=v}) -> do
                ideR    <- ask
                liftIO . (`idleAdd` priorityDefaultIdle) . (`reflectIDE` ideR) $ do
                    liftIO $ debugM "leksah" "SourceBuffer recoverState idle callback"
                    useCandy <- useCandyFor buf
                    gtkBuf   <-  getBuffer v
                    setText gtkBuf text
                    when useCandy $ modeTransformToCandy (mode buf)
                                        (modeEditInCommentOrString (mode buf)) gtkBuf
                    iter     <-  getIterAtOffset gtkBuf i
                    placeCursor gtkBuf iter
                    mark     <-  getInsertMark gtkBuf
                    scrollToMark v mark 0.0 (Just (0.3,0.3))
                    liftIO $ debugM "leksah" "SourceBuffer recoverState done"
                    return False
                return (Just buf)
            Nothing -> return Nothing
    makeActive (actbuf@IDEBuffer {sourceView=sv}) = do
        ideR    <-  ask
        eBuf    <- getBuffer sv
        writeCursorPositionInStatusbar sv
        writeOverwriteInStatusbar sv
        ids1 <- eBuf `afterModifiedChanged` markActiveLabelAsChanged
        ids2 <- sv `afterMoveCursor` writeCursorPositionInStatusbar sv
        -- ids3 <- sv `onLookupInfo` selectInfo sv       -- obsolete by hyperlinks
        ids4 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv
        activateThisPane actbuf $ concat [ids1, ids2, ids4]
        triggerEventIDE (Sensitivity [(SensitivityEditor, True)])
        grabFocus sv
        checkModTime actbuf
        return ()
    closePane pane = do makeActive pane
                        fileClose
    buildPane panePath notebook builder = return Nothing
    builder pp nb w =    return (Nothing,[])

startComplete :: IDEAction
startComplete = do
    mbBuf <- maybeActiveBuf
    currentState' <- readIDE currentState
    case mbBuf of
        Nothing     -> return ()
        Just (IDEBuffer {sourceView=v}) -> complete v True

selectSourceBuf :: FilePath -> IDEM (Maybe IDEBuffer)
selectSourceBuf fp = do
    fpc <- liftIO $ myCanonicalizePath fp
    buffers <- allBuffers
    let buf = filter (\b -> case fileName b of
                                Just fn -> equalFilePath fn fpc
                                Nothing -> False) buffers
    case buf of
        hdb:tl -> do
            makeActive hdb
            return (Just hdb)
        otherwise -> do
            fe <- liftIO $ doesFileExist fpc
            if fe
                then do
                    prefs <- readIDE prefs
                    pp      <- getBestPathForId  "*Buffer"
                    liftIO $ debugM "lekash" "selectSourceBuf calling newTextBuffer"
                    nbuf <- newTextBuffer pp (T.pack $ takeFileName fpc) (Just fpc)
                    liftIO $ debugM "lekash" "selectSourceBuf newTextBuffer returned"
                    return nbuf
                else do
                    ideMessage Normal ((__ "File path not found ") <> T.pack fpc)
                    return Nothing

goToDefinition :: Descr -> IDEAction
goToDefinition idDescr  = do

    mbWorkspaceInfo     <-  getWorkspaceInfo
    mbSystemInfo        <-  getSystemInfo
    let mbPackagePath = (mbWorkspaceInfo >>= (packagePathFromScope . fst))
                        <|> (mbSystemInfo >>= packagePathFromScope)
        mbSourcePath = (mbWorkspaceInfo  >>= (sourcePathFromScope . fst))
                        <|> (mbSystemInfo >>= sourcePathFromScope)

    liftIO . debugM "leksah" $ show (mbPackagePath, dscMbLocation idDescr, mbSourcePath)
    case (mbPackagePath, dscMbLocation idDescr, mbSourcePath) of
        (Just packagePath, Just loc, _) -> goToSourceDefinition (dropFileName packagePath) loc >> return ()
        (_, Just loc, Just sourcePath)  -> goToSourceDefinition' sourcePath loc >> return ()
        (_, _, Just sp) -> selectSourceBuf sp >> return ()
        _  -> return ()
  where
    packagePathFromScope :: GenScope -> Maybe FilePath
    packagePathFromScope (GenScopeC (PackScope l _)) =
        case dscMbModu idDescr of
            Just mod -> case (pack mod) `Map.lookup` l of
                            Just pack -> pdMbSourcePath pack
                            Nothing   -> Nothing
            Nothing -> Nothing

    sourcePathFromScope :: GenScope -> Maybe FilePath
    sourcePathFromScope (GenScopeC (PackScope l _)) =
        case dscMbModu idDescr of
            Just mod -> case (pack mod) `Map.lookup` l of
                            Just pack ->
                                case filter (\md -> mdModuleId md == fromJust (dscMbModu idDescr))
                                                    (pdModules pack) of
                                    (mod : tl) ->  mdMbSourcePath mod
                                    []         -> Nothing
                            Nothing -> Nothing
            Nothing -> Nothing

goToSourceDefinition :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition packagePath loc = do
    goToSourceDefinition' (packagePath </> locationFile loc) loc

goToSourceDefinition' :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition' sourcePath Location{..} = do
    mbBuf     <- selectSourceBuf sourcePath
    case mbBuf of
        Just buf -> do
            inActiveBufContext () $ \_ sv ebuf _ _ -> do
                liftIO $ debugM "lekash" "goToSourceDefinition calculating range"
                lines           <-  getLineCount ebuf
                iterTemp        <-  getIterAtLine ebuf (max 0 (min (lines-1)
                                        (locationSLine -1)))
                chars           <-  getCharsInLine iterTemp
                iter <- atLineOffset iterTemp (max 0 (min (chars-1) (locationSCol -1)))
                iter2Temp       <-  getIterAtLine ebuf (max 0 (min (lines-1) (locationELine -1)))
                chars2          <-  getCharsInLine iter2Temp
                iter2 <- atLineOffset iter2Temp (max 0 (min (chars2-1) locationECol))
                -- ### we had a problem before using this idleAdd thing
                ideR <- ask
                liftIO . (`idleAdd` priorityDefaultIdle) . (`reflectIDE` ideR) $ do
                    liftIO $ debugM "lekash" "goToSourceDefinition triggered selectRange"
                    selectRange ebuf iter iter2
                    liftIO $ debugM "lekash" "goToSourceDefinition triggered scrollToIter"
                    scrollToIter sv iter 0.0 (Just (0.3,0.3))
                    return False
                return ()
        Nothing -> return ()
    return mbBuf

replaceHLintSource :: FilePath -> Int -> Int -> Int -> Int -> Text -> IDEAction
replaceHLintSource file lineS columnS lineE columnE to = do
    mbBuf     <- selectSourceBuf file
    case mbBuf of
        Just buf -> inActiveBufContext () $ \_ sv ebuf _ _ -> do
            useCandy    <- useCandyFor buf
            candy'      <- readIDE candy
            realString <-  if useCandy then stringToCandy candy' to else return to
            (lineS', columnS', lineE', columnE') <- if useCandy
                    then do
                        (_,e1) <- positionToCandy candy' ebuf (lineS,columnS - 1)
                        (_,e2) <- positionToCandy candy' ebuf (lineE,columnE - 1)
                        return (lineS-1,e1,lineE-1,e2)
                    else return (lineS-1, columnS-1, lineE-1, columnE-1)
            i1  <- getIterAtLine ebuf (lineS')
            i1' <- forwardCharsC i1 (columnS')
            i2  <- getIterAtLine ebuf (lineE')
            i2' <- forwardCharsC i2 (columnE')
            delete ebuf i1' i2'
            editInsertCode ebuf i1' realString
            fileSave False
            setModified ebuf True
        _ -> return ()

insertInBuffer :: Descr -> IDEAction
insertInBuffer idDescr = do
    mbPaneName <- lastActiveBufferPane
    case mbPaneName of
        Nothing  -> return ()
        Just name -> do
            PaneC p <- paneFromName name
            let mbBuf = cast p
            case mbBuf of
                Nothing -> return ()
                Just buf ->
                    inBufContext () buf $ \_ _ ebuf buf _ -> do
                        mark <- getInsertMark ebuf
                        iter <- getIterAtMark ebuf mark
                        insert ebuf iter (dscName idDescr)

markRefInSourceBuf :: Int -> IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf index buf logRef scrollTo = do
    useCandy    <- useCandyFor buf
    candy'      <- readIDE candy
    contextRefs <- readIDE contextRefs
    prefs       <- readIDE prefs
    inBufContext () buf $ \_ sv ebuf buf _ -> do
        let tagName = T.pack $ show (logRefType logRef) ++ show index
        liftIO . debugM "lekash" . T.unpack $ "markRefInSourceBuf getting or creating tag " <> tagName
        tagTable <- getTagTable ebuf
        mbTag <- lookupTag tagTable tagName
        case mbTag of
            Just existingTag -> do
                removeTagByName ebuf tagName
            Nothing -> do
                errtag <- newTag tagTable tagName
                case logRefType logRef of
                    ErrorRef -> do
                        underline errtag UnderlineError
                    WarningRef -> do
                        underline errtag UnderlineError
                    BreakpointRef -> do
                        background errtag $ breakpointBackground prefs
                    ContextRef -> do
                        background errtag $ contextBackground prefs

        liftIO $ debugM "lekash" "markRefInSourceBuf calculating range"
        let start' = (srcSpanStartLine (logRefSrcSpan logRef),
                        srcSpanStartColumn (logRefSrcSpan logRef))
        let end'   = (srcSpanEndLine (logRefSrcSpan logRef),
                        srcSpanEndColumn (logRefSrcSpan logRef))
        start <- if useCandy
                    then positionToCandy candy' ebuf start'
                    else return start'
        end   <- if useCandy
                    then positionToCandy candy' ebuf end'
                    else return end'
        lines   <-  getLineCount ebuf
        iterTmp <-  getIterAtLine ebuf (max 0 (min (lines-1) ((fst start)-1)))
        chars   <-  getCharsInLine iterTmp
        iter    <- atLineOffset iterTmp (max 0 (min (chars-1) (snd start)))

        iter2 <- if start == end
            then do
                maybeWE <- forwardWordEndC iter
                case maybeWE of
                    Nothing -> atEnd iter
                    Just we -> return we
            else do
                newTmp  <- getIterAtLine ebuf (max 0 (min (lines-1) ((fst end)-1)))
                chars   <- getCharsInLine newTmp
                new     <- atLineOffset newTmp (max 0 (min (chars-1) (snd end)))
                forwardCharC new

        let latest = if null contextRefs then Nothing else Just $ last contextRefs
        let isOldContext = case (logRefType logRef, latest) of
                                (ContextRef, Just ctx) | ctx /= logRef -> True
                                _ -> False
        unless isOldContext $ do
            liftIO $ debugM "lekash" "markRefInSourceBuf calling applyTagByName"
            applyTagByName ebuf tagName iter iter2
        when scrollTo $ do
            ideR    <- ask
            liftIO . (`idleAdd` priorityDefaultIdle) . (`reflectIDE` ideR) $ do
                liftIO $ debugM "lekash" "markRefInSourceBuf triggered placeCursor"
                placeCursor ebuf iter
                mark <- getInsertMark ebuf
                liftIO $ debugM "lekash" "markRefInSourceBuf trigged scrollToMark"
                scrollToMark sv mark 0.3 Nothing
                when isOldContext $ selectRange ebuf iter iter2
                return False
            return ()

newTextBuffer :: PanePath -> Text -> Maybe FilePath -> IDEM (Maybe IDEBuffer)
newTextBuffer panePath bn mbfn = do
    cont <- case mbfn of
                Nothing -> return True
                Just fn -> liftIO $ doesFileExist fn
    if cont
        then do
            nb      <-  getNotebook panePath
            prefs   <-  readIDE prefs
            bs      <-  getCandyState
            ct      <-  readIDE candy
            (ind,rbn) <- figureOutPaneName bn 0
            buildThisPane panePath nb (builder' bs mbfn ind bn rbn ct prefs)
        else do
            ideMessage Normal ((__ "File does not exist ") <> (T.pack $ fromJust mbfn))
            return Nothing

data CharacterCategory = IdentifierCharacter | SpaceCharacter | SyntaxCharacter
    deriving (Eq)
getCharacterCategory :: Maybe Char -> CharacterCategory
getCharacterCategory Nothing = SpaceCharacter
getCharacterCategory (Just c)
    | isAlphaNum c || c == '\'' || c == '_' = IdentifierCharacter
    | isSpace c = SpaceCharacter
    | otherwise = SyntaxCharacter

builder' :: Bool ->
    Maybe FilePath ->
    Int ->
    Text ->
    Text ->
    CandyTable ->
    Prefs ->
    PanePath ->
    Gtk.Notebook ->
    Gtk.Window ->
    IDEM (Maybe IDEBuffer,Connections)
builder' bs mbfn ind bn rbn ct prefs pp nb windows = do
    -- load up and display a file
    (fileContents,modTime) <- case mbfn of
        Just fn -> do
            fc <- liftIO $ T.readFile fn
            mt <- liftIO $ getModificationTime fn
            return (fc,Just mt)
        Nothing -> return ("\n",Nothing)

    case textEditorType prefs of
        "GtkSourceView" -> newGtkBuffer mbfn fileContents >>= makeBuffer modTime
        "Yi"            -> newYiBuffer mbfn fileContents >>= makeBuffer modTime
        "CodeMirror"    -> newCMBuffer mbfn fileContents >>= makeBuffer modTime
        _               -> newDefaultBuffer mbfn fileContents >>= makeBuffer modTime

  where
    makeBuffer modTime buffer = do
        ideR <- ask

        tagTable <- getTagTable buffer
        foundTag <- newTag tagTable "found"
        background foundTag $ foundBackground prefs

        beginNotUndoableAction buffer
        let mod = modFromFileName mbfn
        when (bs && isHaskellMode mod) $ modeTransformToCandy mod
                                            (modeEditInCommentOrString mod) buffer
        endNotUndoableAction buffer
        setModified buffer False
        siter <- getStartIter buffer
        placeCursor buffer siter
        iter <- getEndIter buffer

        -- create a new SourceView Widget
        sv <- newView buffer (textviewFont prefs)
        setShowLineNumbers sv $ showLineNumbers prefs
        setRightMargin sv $ case rightMargin prefs of
                                (False,_) -> Nothing
                                (True,v) -> Just v
        setIndentWidth sv $ tabWidth prefs
        setTabWidth sv $ 8 -- GHC treats tabs as 8 we should display them that way
        drawTabs sv
        preferDark <- getDarkState
        setStyle preferDark buffer $ case sourceStyle prefs of
                            (False,_) -> Nothing
                            (True,v) -> Just v

        -- put it in a scrolled window
        sw <- getScrolledWindow sv
        if wrapLines prefs
            then liftIO $ scrolledWindowSetPolicy sw PolicyNever PolicyAutomatic
            else liftIO $ scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        liftIO $ scrolledWindowSetShadowType sw ShadowIn
        modTimeRef <- liftIO $ newIORef modTime
        let buf = IDEBuffer {
            fileName =  mbfn,
            bufferName = bn,
            addedIndex = ind,
            sourceView =sv,
            scrolledWindow = sw,
            modTime = modTimeRef,
            mode = mod}
        -- events
        ids1 <- sv `afterFocusIn` makeActive buf
        ids2 <- onCompletion sv (do
                Completion.complete sv False) $ do
                    Completion.cancel
        ids3 <- onButtonPress sv $ do
                click <- lift $ Gtk.eventClick
                liftIDE $
                    case click of
                        Gtk.DoubleClick -> do
                            (start, end) <- getIdentifierUnderCursor buffer
                            selectRange buffer start end
                            return True
                        _ -> return False

        (GetTextPopup mbTpm) <- triggerEvent ideR (GetTextPopup Nothing)
        ids4 <- case mbTpm of
            Just tpm    -> sv `onPopulatePopup` \menu -> liftIO $ (tpm ideR menu)
            Nothing     -> do
                sysMessage Normal "SourceBuffer>> no text popup"
                return []

        --TODO refactor this
        ids5 <- onMotionNotifyEvent sv $ do
            liftIDE $ do
                candy' <- readIDE candy
                inActiveBufContext () $ \_ _ ebuf currentBuffer _ -> do
                    let tagName = "match"
                    tagTable <- getTagTable ebuf
                    mbTag <- lookupTag tagTable tagName
                    case mbTag of
                        Just existingTag -> do
                            removeTagByName ebuf tagName
                        Nothing -> do
                            tag <- newTag tagTable tagName
                            background tag $ matchBackground prefs

                    hasSelection <- hasSelection ebuf
                    when hasSelection $ do
                            (si1,si2)   <- getSelectionBounds ebuf

                            sTxt      <- getCandylessPart candy' ebuf si1 si2
                            let strippedSTxt = T.strip sTxt
                            if T.null strippedSTxt
                                then return ()
                                else do
                                    bi1 <- getStartIter ebuf
                                    bi2 <- getEndIter ebuf
                                    forwardApplying bi1 strippedSTxt (Just si1) tagName ebuf
                                    forwardApplying si2 strippedSTxt (Just bi2) tagName ebuf
            return False

        ids6 <- onKeyPress sv $ do
            keyval      <- lift eventKeyVal
            name        <- lift eventKeyName
            modifier    <- lift eventModifier
            liftIDE $ do
                let moveToNextWord iterOp sel  = do
                    sel' <- iterOp sel
                    rs <- isRangeStart sel'
                    if rs then return sel' else moveToNextWord iterOp sel'
                let calculateNewPosition iterOp = getInsertIter buffer >>= moveToNextWord iterOp
                let continueSelection keepSelBound nsel = do
                        if keepSelBound
                            then do
                                sb <- getSelectionBoundMark buffer >>= getIterAtMark buffer
                                selectRange buffer nsel sb
                            else
                                placeCursor buffer nsel
                        scrollToIter sv nsel 0 Nothing
                case (name, map mapControlCommand modifier, keyval) of
                    ("Left",[Gtk.Control],_) -> do
                        calculateNewPosition backwardCharC >>= continueSelection False
                        return True
                    ("Left",[Gtk.Shift, Gtk.Control],_) -> do
                        calculateNewPosition backwardCharC >>= continueSelection True
                        return True
                    ("Right",[Gtk.Control],_) -> do
                        calculateNewPosition forwardCharC >>= continueSelection False --placeCursor buffer
                        return True
                    ("Right",[Gtk.Shift, Gtk.Control],_) -> do
                        calculateNewPosition forwardCharC >>= continueSelection True
                        return True
                    ("BackSpace",[Gtk.Control],_) -> do              -- delete word
                        here <- getInsertIter buffer
                        there <- calculateNewPosition backwardCharC
                        delete buffer here there
                        return True
                    ("underscore",[Gtk.Shift, Gtk.Control],_) -> do
                        (start, end) <- getIdentifierUnderCursor buffer
                        slice <- getSlice buffer start end True
                        triggerEventIDE (SelectInfo slice False)
                        return True
                        -- Redundant should become a go to definition directly
                    ("minus",[Gtk.Control],_) -> do
                        (start, end) <- getIdentifierUnderCursor buffer
                        slice <- getSlice buffer start end True
                        triggerEventIDE (SelectInfo slice True)
                        return True
                    _ -> do
                        -- liftIO $ print ("sourcebuffer key:",name,modifier,keyval)
                        return False
        ids7 <- do
            ideR <- ask
            sw <- getScrolledWindow sv
            createHyperLinkSupport sv sw (\ctrl shift iter -> do
                (beg, en) <- getIdentifierUnderCursorFromIter (iter, iter)
                return (beg, if ctrl then en else beg)) (\_ shift' slice -> do
                            unless (T.null slice) $ do
                                -- liftIO$ print ("slice",slice)
                                triggerEventIDE (SelectInfo slice shift')
                                return ()
                            )
        return (Just buf,concat [ids1, ids2, ids3, ids4, ids5, ids6, ids7])

    forwardApplying :: TextEditor editor
                    => EditorIter editor
                    -> Text   -- txt
                    -> Maybe (EditorIter editor)
                    -> Text   -- tagname
                    -> EditorBuffer editor
                    -> IDEM ()
    forwardApplying tI txt mbTi tagName ebuf = do
        mbFTxt <- forwardSearch tI txt [TextSearchVisibleOnly, TextSearchTextOnly] mbTi
        case mbFTxt of
            Just (start, end) -> do
                startsWord <- startsWord start
                endsWord <- endsWord end
                if (startsWord && endsWord)
                    then applyTagByName ebuf tagName start end
                    else return ()
                forwardApplying end txt mbTi tagName ebuf
            Nothing -> return()

isIdent a = isAlphaNum a || a == '\'' || a == '_'       -- parts of haskell identifiers

isRangeStart sel = do                                   -- if char and previous char are of different char categories
    currentChar <- getChar sel
    let mbStartCharCat = getCharacterCategory currentChar
    mbPrevCharCat <- getCharacterCategory <$> (backwardCharC sel >>= getChar)
    return $ currentChar == Nothing || currentChar == Just '\n' || mbStartCharCat /= mbPrevCharCat && (mbStartCharCat == SyntaxCharacter || mbStartCharCat == IdentifierCharacter)

getIdentifierUnderCursor :: forall editor. TextEditor editor => EditorBuffer editor -> IDEM (EditorIter editor, EditorIter editor)
getIdentifierUnderCursor buffer = do
    (startSel, endSel) <- getSelectionBounds buffer
    getIdentifierUnderCursorFromIter (startSel, endSel)

getIdentifierUnderCursorFromIter :: TextEditor editor => (EditorIter editor, EditorIter editor) -> IDEM (EditorIter editor, EditorIter editor)
getIdentifierUnderCursorFromIter (startSel, endSel) = do
    let isIdent a = isAlphaNum a || a == '\'' || a == '_'
    let isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                 || a == '!'  || a == '@' || a == '%' || a == '&' || a == '?'
    mbStartChar <- getChar startSel
    mbEndChar <- getChar endSel
    let isSelectChar =
            case mbStartChar of
                Just startChar | isIdent startChar -> isIdent
                Just startChar | isOp    startChar -> isOp
                _                                  -> const False
    start <- case mbStartChar of
        Just startChar | isSelectChar startChar -> do
            maybeIter <- backwardFindCharC startSel (not.isSelectChar) Nothing
            case maybeIter of
                Just iter -> forwardCharC iter
                Nothing   -> return startSel
        _ -> return startSel
    end <- case mbEndChar of
        Just endChar | isSelectChar endChar -> do
            maybeIter <- forwardFindCharC endSel (not.isSelectChar) Nothing
            case maybeIter of
                Just iter -> return iter
                Nothing   -> return endSel
        _ -> return endSel
    return (start, end)

checkModTime :: MonadIDE m => IDEBuffer -> m (Bool, Bool)
checkModTime buf = do
    currentState' <- readIDE currentState
    case  currentState' of
        IsShuttingDown -> return (False, False)
        _              -> do
            let name = paneName buf
            case fileName buf of
                Just fn -> do
                    exists <- liftIO $ doesFileExist fn
                    if exists
                        then do
                            nmt <- liftIO $ getModificationTime fn
                            modTime' <- liftIO $ readIORef (modTime buf)
                            case modTime' of
                                Nothing ->  error $"checkModTime: time not set " ++ show (fileName buf)
                                Just mt ->
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                                    if False
#else
                                    if nmt /= mt -- Fonts get messed up under windows when adding this line.
                                                  -- Praises to whoever finds out what happens and how to fix this
#endif
                                    then do
                                        load <- readIDE (autoLoad . prefs)
                                        if load
                                            then do
                                                ideMessage Normal $ (__ "Auto Loading ") <> T.pack fn
                                                revert buf
                                                return (False, True)
                                            else do
                                                window <- liftIDE getMainWindow
                                                resp <- liftIO $ do
                                                    md <- messageDialogNew
                                                            (Just window) []
                                                            MessageQuestion
                                                            ButtonsNone
                                                            ((__"File \"") <> name <> (__ "\" has changed on disk."))
                                                    dialogAddButton md (__ "_Load From Disk") (ResponseUser 1)
                                                    dialogAddButton md (__ "_Always Load From Disk") (ResponseUser 2)
                                                    dialogAddButton md (__ "_Don't Load") (ResponseUser 3)
                                                    dialogSetDefaultResponse md (ResponseUser 1)
                                                    set md [ windowWindowPosition := WinPosCenterOnParent ]
                                                    resp <- dialogRun md
                                                    widgetDestroy md
                                                    return resp
                                                case resp of
                                                    ResponseUser 1 -> do
                                                        revert buf
                                                        return (False, True)
                                                    ResponseUser 2 -> do
                                                        revert buf
                                                        modifyIDE_ $ \ide -> ide{prefs = (prefs ide) {autoLoad = True}}
                                                        return (False, True)
                                                    ResponseUser 3 -> do
                                                        nmt2 <- liftIO $ getModificationTime fn
                                                        liftIO $ writeIORef (modTime buf) (Just nmt2)
                                                        return (True, True)
                                                    _           ->  do return (False, False)
                                    else return (False, False)
                        else return (False, False)
                Nothing -> return (False, False)

setModTime :: IDEBuffer -> IDEAction
setModTime buf = do
    let name = paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> liftIO $ E.catch
            (do
                nmt <- getModificationTime fn
                writeIORef (modTime buf) (Just nmt))
            (\(e:: SomeException) -> do
                sysMessage Normal (T.pack $ show e)
                return ())

fileRevert :: IDEAction
fileRevert = inActiveBufContext () $ \ _ _ _ currentBuffer _ -> do
    revert currentBuffer

revert :: MonadIDE m => IDEBuffer -> m ()
revert (buf@IDEBuffer{sourceView = sv}) = do
    useCandy    <-  useCandyFor buf
    ct          <-  readIDE candy
    let name    =   paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> liftIDE $ do
            buffer <- getBuffer sv
            fc <- liftIO $ readFile fn
            mt <- liftIO $ getModificationTime fn
            beginNotUndoableAction buffer
            setText buffer $ T.pack fc
            if useCandy
                then modeTransformToCandy (mode buf)
                         (modeEditInCommentOrString (mode buf)) buffer
                else return ()
            endNotUndoableAction buffer
            setModified buffer False
            return mt
            liftIO $ writeIORef (modTime buf) (Just mt)

writeCursorPositionInStatusbar :: TextEditor editor => EditorView editor -> IDEAction
writeCursorPositionInStatusbar sv = do
    buf  <- getBuffer sv
    mark <- getInsertMark buf
    iter <- getIterAtMark buf mark
    line <- getLine iter
    col  <- getLineOffset iter
    triggerEventIDE (StatusbarChanged [CompartmentBufferPos (line,col)])
    return ()

writeOverwriteInStatusbar :: TextEditor editor => EditorView editor -> IDEAction
writeOverwriteInStatusbar sv = do
    mode <- getOverwrite sv
    triggerEventIDE (StatusbarChanged [CompartmentOverlay mode])
    return ()

selectInfo :: TextEditor editor => EditorView editor -> IDEAction
selectInfo sv = do
    ideR    <- ask
    buf     <- getBuffer sv
    (l,r)   <- getIdentifierUnderCursor buf
    symbol  <- getText buf l r True
    triggerEvent ideR (SelectInfo symbol False)
    return ()

markActiveLabelAsChanged :: IDEAction
markActiveLabelAsChanged = do
    mbPath <- getActivePanePath
    case mbPath of
        Nothing -> return ()
        Just path -> do
          nb <- getNotebook path
          mbBS <- maybeActiveBuf
          case mbBS of
              Nothing -> return ()
              Just buf -> markLabelAsChanged nb buf

markLabelAsChanged :: Notebook -> IDEBuffer -> IDEAction
markLabelAsChanged nb (buf@IDEBuffer{sourceView = sv}) = do
    ebuf   <- getBuffer sv
    modified <- getModified ebuf
    liftIO $ markLabel nb (getTopWidget buf) modified

fileSaveBuffer :: MonadIDE m => TextEditor editor => Bool -> Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int -> m Bool
fileSaveBuffer query nb _ ebuf (ideBuf@IDEBuffer{sourceView = sv}) i = liftIDE $ do
    ideR    <- ask
    window  <- getMainWindow
    prefs   <- readIDE prefs
    useCandy <- useCandyFor ideBuf
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    mbpage <- liftIO $ notebookGetNthPage nb i
    case mbpage of
        Nothing     -> throwIDE (__ "fileSave: Page not found")
        Just page   ->
            if isJust mbfn && query == False
                then do (modifiedOnDiskNotLoaded, modifiedOnDisk) <- checkModTime ideBuf -- The user is given option to reload
                        modifiedInBuffer <- getModified ebuf
                        if (modifiedOnDiskNotLoaded || modifiedInBuffer)
                            then do
                                fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf
                                    useCandy candy $fromJust mbfn
                                setModTime ideBuf
                                return True
                            else return modifiedOnDisk
                else reifyIDE $ \ideR   ->  do
                    dialog <- fileChooserDialogNew
                                    (Just $ (__ "Save File"))
                                    (Just window)
                                FileChooserActionSave
                                [("gtk-cancel"     --buttons to display
                                ,ResponseCancel)  --you can use stock buttons
                                ,("gtk-save"
                                , ResponseAccept)]
                    case mbfn of
                        Just fn -> fileChooserSelectFilename dialog fn >> return ()
                        Nothing -> return ()
                    widgetShow dialog
                    response <- dialogRun dialog
                    mbFileName <- case response of
                            ResponseAccept ->       fileChooserGetFilename dialog
                            ResponseCancel ->       return Nothing
                            ResponseDeleteEvent->   return Nothing
                            _               ->      return Nothing
                    widgetDestroy dialog
                    case mbFileName of
                        Nothing -> return False
                        Just fn -> do
                            dfe <- doesFileExist fn
                            resp <- if dfe
                                then do md <- messageDialogNew (Just window) []
                                                MessageQuestion
                                                ButtonsCancel
                                                (__ "File already exist.")
                                        dialogAddButton md (__ "_Overwrite") ResponseYes
                                        dialogSetDefaultResponse md ResponseCancel
                                        set md [ windowWindowPosition := WinPosCenterOnParent ]
                                        resp <- dialogRun md
                                        widgetHide md
                                        return resp
                                else return ResponseYes
                            case resp of
                                ResponseYes -> do
                                    reflectIDE (do
                                        fileSave' (forceLineEnds prefs) (removeTBlanks prefs)
                                            nb ideBuf useCandy candy fn
                                        closePane ideBuf
                                        cfn <- liftIO $ myCanonicalizePath fn
                                        newTextBuffer panePath (T.pack $ takeFileName cfn) (Just cfn)
                                        ) ideR
                                    return True
                                _          -> return False
    where
        fileSave' :: Bool -> Bool -> Notebook -> IDEBuffer -> Bool -> CandyTable -> FilePath -> IDEAction
        fileSave' forceLineEnds removeTBlanks nb ideBuf useCandy candyTable fn = do
            buf     <-   getBuffer $ sv
            text    <-   getCandylessText candyTable buf
            let text' = if removeTBlanks
                            then T.unlines $ map (T.dropWhileEnd $ \c -> c == ' ') $ T.lines text
                            else text
            succ <- liftIO $ E.catch (do T.writeFile fn text'; return True)
                (\(e :: SomeException) -> do
                    sysMessage Normal . T.pack $ show e
                    return False)
            setModified buf (not succ)
            markLabelAsChanged nb ideBuf

fileSave :: Bool -> IDEM Bool
fileSave query = inActiveBufContext False $ fileSaveBuffer query

fileSaveAll :: MonadIDE m => (IDEBuffer -> m Bool) -> m Bool
fileSaveAll filterFunc = do
    bufs     <- allBuffers
    filtered <- filterM filterFunc bufs
    results  <- forM filtered (\buf -> inBufContext False buf (fileSaveBuffer False))
    return $ True `elem` results

fileCheckBuffer :: (MonadIDE m, TextEditor editor) => Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int -> m Bool
fileCheckBuffer nb _ ebuf ideBuf i = do
    let mbfn = fileName ideBuf
    if isJust mbfn
        then do (_, modifiedOnDisk) <- checkModTime ideBuf -- The user is given option to reload
                modifiedInBuffer    <- liftIDE $ getModified ebuf
                return (modifiedOnDisk || modifiedInBuffer)
        else return False

fileCheckAll :: MonadIDE m => (IDEBuffer -> m [alpha]) -> m [alpha]
fileCheckAll filterFunc = do
    bufs     <- allBuffers
    liftM concat . forM bufs $ \ buf -> do
        ps <- filterFunc buf
        case ps of
            [] -> return []
            _  -> do
                    modified <- inBufContext False buf fileCheckBuffer
                    if modified
                        then return ps
                        else return []

fileNew :: IDEAction
fileNew = do
    prefs   <- readIDE prefs
    pp      <- getBestPathForId  "*Buffer"
    newTextBuffer pp (__ "Unnamed") Nothing
    return ()

fileClose :: IDEM Bool
fileClose = inActiveBufContext True $ fileClose'

fileClose' :: TextEditor editor => Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int  -> IDEM Bool
fileClose' nb _ ebuf currentBuffer i = do
    window  <- getMainWindow
    modified <- getModified ebuf
    cancel <- reifyIDE $ \ideR   ->  do
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsCancel
                                            ((__ "Save changes to document: ")
                                                <> paneName currentBuffer
                                                <> "?")
                dialogAddButton md (__ "_Save") ResponseYes
                dialogAddButton md (__ "_Don't Save") ResponseNo
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                case resp of
                    ResponseYes ->   do
                        reflectIDE (fileSave False) ideR
                        return False
                    ResponseCancel  ->   return True
                    ResponseNo      ->   return False
                    _               ->   return False
            else return False
    if cancel
        then return False
        else do
            closeThisPane currentBuffer
            when (isJust $ fileName currentBuffer)
                (addRecentlyUsedFile (fromJust $ fileName currentBuffer))
            return True

fileCloseAll :: (IDEBuffer -> IDEM Bool)  -> IDEM Bool
fileCloseAll filterFunc = do
    bufs    <- allBuffers
    filtered <- filterM filterFunc bufs
    if null filtered
        then return True
        else do
            makeActive (head filtered)
            r <- fileClose
            if r
                then fileCloseAll filterFunc
                else return False

fileCloseAllButPackage :: IDEAction
fileCloseAllButPackage = do
    mbActivePath    <-  (fmap ipdBuildDir) <$> readIDE activePack
    bufs            <-  allBuffers
    case mbActivePath of
        Just p -> mapM_ (close' p) bufs
        Nothing -> return ()
    where
        close' dir (buf@IDEBuffer {sourceView = sv}) = do
            (pane,_)    <-  guiPropertiesFromName (paneName buf)
            nb          <-  getNotebook pane
            mbI         <-  liftIO $notebookPageNum nb (scrolledWindow buf)
            case mbI of
                Nothing ->  throwIDE (__ "notebook page not found: unexpected")
                Just i  ->  do
                    ebuf <- getBuffer sv
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb sv ebuf buf i; return ()

fileCloseAllButWorkspace :: IDEAction
fileCloseAllButWorkspace = do
    mbWorkspace     <-  readIDE workspace
    bufs            <-  allBuffers
    when (not (null bufs) && isJust mbWorkspace) $ do
        mapM_ (close' (fromJust mbWorkspace)) bufs
    where
        close' workspace (buf@IDEBuffer {sourceView = sv}) = do
            (pane,_)    <-  guiPropertiesFromName (paneName buf)
            nb          <-  getNotebook pane
            mbI         <-  liftIO $notebookPageNum nb (scrolledWindow buf)
            case mbI of
                Nothing ->  throwIDE (__ "notebook page not found: unexpected")
                Just i  ->  do
                    ebuf <- getBuffer sv
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPathOfAny workspace (fromJust (fileName buf))))
                            $ do fileClose' nb sv ebuf buf i; return ()
        isSubPathOfAny workspace fileName =
            let paths = wsPackages workspace >>= ipdAllDirs
            in  or (map (\dir -> isSubPath dir fileName) paths)


fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbBuf <- maybeActiveBuf
    mbFileName <- liftIO $ do
        dialog <- fileChooserDialogNew
                        (Just $ (__ "Open File"))
                        (Just window)
                    FileChooserActionOpen
                    [("gtk-cancel"
                    ,ResponseCancel)
                    ,("gtk-open"
                    ,ResponseAccept)]
        case mbBuf >>= fileName of
            Just fn -> fileChooserSetCurrentFolder dialog (dropFileName $ fn) >> return ()
            Nothing -> return ()
        widgetShow dialog
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do
                f <- fileChooserGetFilename dialog
                widgetDestroy dialog
                return f
            ResponseCancel -> do
                widgetDestroy dialog
                return Nothing
            ResponseDeleteEvent-> do
                widgetDestroy dialog
                return Nothing
            _ -> return Nothing
    case mbFileName of
        Nothing -> return ()
        Just fp -> fileOpenThis fp


fileOpenThis :: FilePath -> IDEAction
fileOpenThis fp =  do
    liftIO . debugM "leksah" $ "fileOpenThis " ++ fp
    prefs <- readIDE prefs
    fpc <-  liftIO $ myCanonicalizePath fp
    buffers <- allBuffers
    let buf = filter (\b -> case fileName b of
                        Just fn -> equalFilePath fn fpc
                        Nothing -> False) buffers
    case buf of
        hdb:tl -> do
            window <- getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew
                        (Just window) []
                        MessageQuestion
                        ButtonsNone
                        (__ "Buffer already open.")
                dialogAddButton md (__ "Make _Active") (ResponseUser 1)
                dialogAddButton md (__ "_Open Second") (ResponseUser 2)
                dialogSetDefaultResponse md (ResponseUser 1)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                return resp
            case resp of
                ResponseUser 2 -> reallyOpen prefs fpc
                _              -> makeActive hdb
        [] -> reallyOpen prefs fpc
    where
        reallyOpen prefs fpc =   do
            pp <-  getBestPathForId "*Buffer"
            newTextBuffer pp (T.pack $ takeFileName fpc) (Just fpc)
            return ()

filePrint :: IDEAction
filePrint = inActiveBufContext () $ filePrint'

filePrint' :: TextEditor editor => Notebook -> EditorView view -> EditorBuffer editor -> IDEBuffer -> Int -> IDEM ()
filePrint' nb _ ebuf currentBuffer _ = do
    let pName = paneName currentBuffer
    window  <- getMainWindow
    print <- reifyIDE $ \ideR ->  do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsNone
                                            (__"Print document: "
                                                <> pName
                                                <> "?")
                dialogAddButton md (__"_Print") ResponseYes
                dialogAddButton md (__"_Don't Print") ResponseNo
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                case resp of
                    ResponseYes ->   do
                        return True
                    ResponseCancel  ->   return False
                    ResponseNo      ->   return False
                    _               ->   return False
    if not print
        then return ()
        else do
            --real code
            modified <- getModified ebuf
            cancel <- reifyIDE $ \ideR ->  do
                if modified
                    then do
                        md <- messageDialogNew (Just window) []
                                                    MessageQuestion
                                                    ButtonsNone
                                                    (__"Save changes to document: "
                                                        <> pName
                                                        <> "?")
                        dialogAddButton md (__"_Save") ResponseYes
                        dialogAddButton md (__"_Don't Save") ResponseNo
                        dialogAddButton md (__"_Cancel Printing") ResponseCancel
                        set md [ windowWindowPosition := WinPosCenterOnParent ]
                        resp <- dialogRun md
                        widgetDestroy md
                        case resp of
                            ResponseYes ->   do
                                reflectIDE (fileSave False) ideR
                                return False
                            ResponseCancel  ->   return True
                            ResponseNo      ->   return False
                            _               ->   return False
                    else
                        return False
            if cancel
                then return ()
                else do
                    case fileName currentBuffer of
                        Just name -> do
                                      status <- liftIO $ Print.print name
                                      case status of
                                        Left error -> liftIO $ showDialog (T.pack $ show error) MessageError
                                        Right _ -> liftIO $ showDialog "Print job has been sent successfully" MessageInfo
                                      return ()
                        Nothing   -> return ()

editUndo :: IDEAction
editUndo = inActiveBufContext () $ \_ _ buf _ _ -> do
    can <- canUndo buf
    when can $ undo buf

editRedo :: IDEAction
editRedo = inActiveBufContext () $ \_ _ buf _ _ -> do
    can <- canRedo buf
    when can $ redo buf

editDelete :: IDEAction
editDelete = inActiveBufContext ()  $ \_ _ ebuf _ _ ->  do
    deleteSelection ebuf
    return ()

editSelectAll :: IDEAction
editSelectAll = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    start <- getStartIter ebuf
    end   <- getEndIter ebuf
    selectRange ebuf start end

editCut :: IDEAction
editCut = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    clip <- liftIO $ clipboardGet selectionClipboard
    cutClipboard ebuf clip True

editCopy :: IDEAction
editCopy = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    clip <- liftIO $ clipboardGet selectionClipboard
    copyClipboard ebuf clip

editPaste :: IDEAction
editPaste = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    mark <- getInsertMark ebuf
    iter <- getIterAtMark ebuf mark
    clip <- liftIO $ clipboardGet selectionClipboard
    pasteClipboard ebuf clip iter True

editShiftLeft :: IDEAction
editShiftLeft = do
    prefs <- readIDE prefs
    let str = T.replicate (tabWidth prefs) " "
    b <- canShiftLeft str prefs
    if b
        then do
            doForSelectedLines [] $ \ebuf lineNr -> do
                sol <- getIterAtLine ebuf lineNr
                sol2 <- forwardCharsC sol (tabWidth prefs)
                delete ebuf sol sol2
            return ()
        else return ()
    where
    canShiftLeft str prefs = do
        boolList <- doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol (tabWidth prefs)
            str1 <- getText ebuf sol sol2 True
            return (str1 == str)
        return (foldl' (&&) True boolList)


editShiftRight :: IDEAction
editShiftRight = do
    prefs <- readIDE prefs
    let str = T.replicate (tabWidth prefs) " "
    doForSelectedLines [] $ \ebuf lineNr -> do
        sol <- getIterAtLine ebuf lineNr
        insert ebuf sol str
    return ()

alignChar :: Char -> IDEAction
alignChar char = do
    positions     <- positionsOfChar
    let alignTo = foldl' max 0 (catMaybes (map snd positions))
    if (alignTo > 0)
        then alignChar (Map.fromList positions) alignTo
        else return ()
    where
    positionsOfChar :: IDEM ([(Int, Maybe Int)])
    positionsOfChar = doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            eol <- forwardToLineEndC sol
            line  <- getText ebuf sol eol True
            return (lineNr, T.findIndex (==char) line)
    alignChar :: Map Int (Maybe Int) -> Int -> IDEM ()
    alignChar positions alignTo = do
            doForSelectedLines [] $ \ebuf lineNr -> do
                case lineNr `Map.lookup` positions of
                    Just (Just n)  ->  do
                        sol       <- getIterAtLine ebuf lineNr
                        insertLoc <- forwardCharsC sol n
                        insert ebuf insertLoc (T.replicate (alignTo - n) " ")
                    _              ->  return ()
            return ()

transChar :: Char -> Char
transChar ':' = toEnum 0x2237 --PROPORTION
transChar '>' = toEnum 0x2192 --RIGHTWARDS ARROW
transChar '<' = toEnum (toEnum 0x2190) --LEFTWARDS ARROW
transChar c   = c

align :: Char -> IDEAction
align = alignChar . transChar

addRecentlyUsedFile :: FilePath -> IDEAction
addRecentlyUsedFile fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        unless (elem fp recentFiles') $
            modifyIDE_ (\ide -> ide{recentFiles = take 12 (fp : recentFiles')})
        triggerEventIDE UpdateRecent
        return ()

removeRecentlyUsedFile :: FilePath -> IDEAction
removeRecentlyUsedFile fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        when (elem fp recentFiles') $
            modifyIDE_ (\ide -> ide{recentFiles = filter (\e -> e /= fp) recentFiles'})
        triggerEventIDE UpdateRecent
        return ()

selectedText :: IDEM (Maybe Text)
selectedText = do
    candy' <- readIDE candy
    inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ -> do
        hasSelection <- hasSelection ebuf
        if hasSelection
            then do
                (i1,i2)   <- getSelectionBounds ebuf
                text      <- getCandylessPart candy' ebuf i1 i2
                return $ Just text
            else return Nothing

selectedTextOrCurrentLine :: IDEM (Maybe Text)
selectedTextOrCurrentLine = do
    candy' <- readIDE candy
    inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ -> do
        hasSelection <- hasSelection ebuf
        (i1, i2) <- if hasSelection
            then getSelectionBounds ebuf
            else do
                (i, _) <- getSelectionBounds ebuf
                line <- getLine i
                iStart <- getIterAtLine ebuf line
                iEnd <- forwardToLineEndC iStart
                return (iStart, iEnd)
        fmap Just $ getCandylessPart candy' ebuf i1 i2

selectedLocation :: IDEM (Maybe (Int, Int))
selectedLocation = do
    candy'      <- readIDE candy
    inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ -> do
        useCandy   <- useCandyFor currentBuffer
        (start, _) <- getSelectionBounds ebuf
        line       <- getLine start
        lineOffset <- getLineOffset start
        res <- if useCandy
            then positionFromCandy candy' ebuf (line, lineOffset)
            else return (line, lineOffset)
        return $ Just res

insertTextAfterSelection :: Text -> IDEAction
insertTextAfterSelection str = do
    candy'       <- readIDE candy
    inActiveBufContext () $ \_ _ ebuf currentBuffer _ -> do
        useCandy     <- useCandyFor currentBuffer
        hasSelection <- hasSelection ebuf
        when hasSelection $ do
            realString <-  if useCandy then stringToCandy candy' str else return str
            (_,i)      <- getSelectionBounds ebuf
            mark       <- createMark ebuf i True
            insert ebuf i realString
            i1         <- getIterAtMark ebuf mark
            i2         <- forwardCharsC i1 (T.length str)
            selectRange ebuf i1 i2

-- | Returns the package, to which this buffer belongs, if possible
belongsToPackages :: MonadIDE m => IDEBuffer -> m [IDEPackage]
belongsToPackages IDEBuffer{fileName = Just fp}= do
    bufferToProject' <-  readIDE bufferProjCache
    ws               <-  readIDE workspace
    case Map.lookup fp bufferToProject' of
        Just p  -> return p
        Nothing -> case ws of
                        Nothing   -> return []
                        Just workspace -> do
--                            mbMn <- liftIO $ moduleNameFromFilePath fp
--                            let mbMn2 = case mbMn of
--                                            Nothing -> Nothing
--                                            Just mn -> simpleParse mn
                            let res = filter (belongsToPackage fp) (wsPackages workspace)
                            modifyIDE_ (\ide -> ide{bufferProjCache = Map.insert fp res bufferToProject'})
                            return res
belongsToPackages _ = return []

-- | Including files in sandbox source dirs
belongsToPackage :: FilePath -> IDEPackage -> Bool
belongsToPackage f = any (`isSubPath` f) . ipdAllDirs

belongsToWorkspace b =  belongsToPackages b >>= return . (not . null)

useCandyFor :: MonadIDE m => IDEBuffer -> m Bool
useCandyFor aBuffer = do
    use <- liftIDE getCandyState
    return (use && isHaskellMode (mode aBuffer))

editCandy = do
    use <- liftIDE getCandyState
    buffers <- allBuffers
    if use
        then mapM_ (\b -> modeEditToCandy (mode b)
            (modeEditInCommentOrString (mode b))) buffers
        else mapM_ (modeEditFromCandy . mode) buffers


