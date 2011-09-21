{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             TypeSynonymInstances, ScopedTypeVariables, RankNTypes #-}
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
,   goToDefinition
,   insertInBuffer

,   fileNew
,   fileOpenThis
,   fileOpen
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
,   belongsToPackage
,   belongsToWorkspace
,   getIdentifierUnderCursorFromIter
,   launchSymbolNavigationDialog

) where

import Prelude hiding(getChar, getLine)
import Control.Monad.Reader
import Control.Applicative
import System.FilePath
import System.Directory
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List hiding(insert, delete)
import Data.Maybe
import Data.Char
import Data.Typeable
import qualified Data.Set as Set

import Graphics.UI.Gtk.Gdk.Events as Gtk
import IDE.Core.State
import IDE.Utils.GUIUtils(getCandyState)
import IDE.Utils.FileUtils
import IDE.SourceCandy
import IDE.SymbolNavigation
import IDE.Completion as Completion (complete,cancel)
import IDE.TextEditor
import qualified System.IO.UTF8 as UTF8
import Data.IORef (writeIORef,readIORef,newIORef)
-- import Data.Char (isAlphaNum, isSymbol)
import Control.Event (triggerEvent)
import IDE.Metainfo.Provider (getSystemInfo, getWorkspaceInfo)
import Distribution.Text (simpleParse)
import Distribution.ModuleName (ModuleName)
--import qualified Data.Set as Set (member)
import Graphics.UI.Gtk
       (Notebook, clipboardGet, selectionClipboard, dialogAddButton, widgetDestroy,
        fileChooserGetFilename, widgetShow, fileChooserDialogNew,
        notebookGetNthPage, notebookPageNum, widgetHide, dialogRun,
        messageDialogNew, postGUIAsync, scrolledWindowSetShadowType,
        scrolledWindowSetPolicy, dialogSetDefaultResponse,
        fileChooserSetCurrentFolder, fileChooserSelectFilename)
import System.Glib.MainLoop (priorityDefaultIdle, idleAdd)
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk (windowWindowPosition, Underline(..))
#else
import Graphics.UI.Gtk.Pango.Types (Underline(..))
#endif
import qualified Graphics.UI.Gtk as Gtk hiding (eventKeyName)
import Graphics.UI.Gtk.General.Enums
       (WindowPosition(..), ShadowType(..), PolicyType(..))
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..))
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
#else
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
#endif
import Graphics.UI.Gtk.Selectors.FileChooser
       (FileChooserAction(..))
import System.Glib.Attributes (AttrOp(..), set)
import qualified Graphics.UI.Gtk.Gdk.Events as GtkOld

import IDE.BufferMode

allBuffers :: IDEM [IDEBuffer]
allBuffers = getPanes

instance RecoverablePane IDEBuffer BufferState IDEM where
    saveState p     =   do  buf    <- getBuffer (sourceView p)
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
        mbbuf    <-  newTextBuffer pp (takeFileName n) (Just n)
        case mbbuf of
            Just buf -> do
                gtkBuf  <- getBuffer (sourceView buf)
                iter    <- getIterAtOffset gtkBuf i
                placeCursor gtkBuf iter
                mark    <- getInsertMark gtkBuf
                ideR    <- ask
                liftIO $ idleAdd (do
                    reflectIDE (scrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))) ideR
                    return False) priorityDefaultIdle
                return (Just buf)
            Nothing -> return Nothing
    recoverState pp (BufferStateTrans bn text i) =   do
        mbbuf    <-  newTextBuffer pp bn Nothing
        case mbbuf of
            Just buf -> do
                useCandy <- useCandyFor buf
                gtkBuf   <-  getBuffer (sourceView buf)
                setText gtkBuf text
                when useCandy $ modeTransformToCandy (mode buf)
                                    (modeEditInCommentOrString (mode buf)) gtkBuf
                iter     <-  getIterAtOffset gtkBuf i
                placeCursor gtkBuf iter
                mark     <-  getInsertMark gtkBuf
                ideR     <- ask
                liftIO $ idleAdd  (do
                    reflectIDE (scrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))) ideR
                    return False) priorityDefaultIdle
                return (Just buf)
            Nothing -> return Nothing
    makeActive actbuf = do
        ideR    <-  ask
        let sv = sourceView actbuf
        eBuf    <- getBuffer sv
        writeCursorPositionInStatusbar sv
        writeOverwriteInStatusbar sv
        ids1 <- eBuf `afterModifiedChanged` markActiveLabelAsChanged
        ids2 <- sv `afterMoveCursor` writeCursorPositionInStatusbar sv
        ids3 <- sv `onLookupInfo` selectInfo sv
        ids4 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv
        activateThisPane actbuf $ concat [ids1, ids2, ids3, ids4]
        triggerEventIDE (Sensitivity [(SensitivityEditor, True)])
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
        Just buf    -> complete (sourceView buf) True

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
                    nbuf <- newTextBuffer pp (takeFileName fpc) (Just fpc)
                    return nbuf
                else do
                    ideMessage Normal ("File path not found " ++ fpc)
                    return Nothing

goToDefinition :: Descr -> IDEAction
goToDefinition idDescr  = do
    mbWorkspaceInfo     <-  getWorkspaceInfo
    mbSystemInfo        <-  getSystemInfo
    let mbSourcePath1   =   case mbWorkspaceInfo of
                                    Nothing -> Nothing
                                    Just (sc, _) -> sourcePathFromScope sc
    let mbSourcePath2   =   case mbSourcePath1 of
                                Just sp -> Just sp
                                Nothing -> case mbSystemInfo of
                                                Just si -> sourcePathFromScope si
                                                Nothing -> Nothing
    when (isJust mbSourcePath2) $
        goToSourceDefinition (fromJust $ mbSourcePath2) (dscMbLocation idDescr)
    return ()
    where
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

goToSourceDefinition :: FilePath -> Maybe Location -> IDEAction
goToSourceDefinition fp dscMbLocation = do
    liftIO $ putStrLn $ "goToSourceDefinition " ++ fp
    mbBuf     <- selectSourceBuf fp
    when (isJust mbBuf && isJust dscMbLocation) $
        inActiveBufContext () $ \_ ebuf buf _ -> do
            let location    =   fromJust dscMbLocation
            lines           <-  getLineCount ebuf
            iterTemp        <-  getIterAtLine ebuf (max 0 (min (lines-1)
                                    ((locationSLine location) -1)))
            chars           <-  getCharsInLine iterTemp
            iter <- atLineOffset iterTemp (max 0 (min (chars-1) (locationSCol location)))
            iter2Temp       <-  getIterAtLine ebuf (max 0 (min (lines-1)
                                    ((locationELine location) -1)))
            chars2          <-  getCharsInLine iter2Temp
            iter2 <- atLineOffset iter2Temp (max 0 (min (chars2-1) (locationECol location)))
            placeCursor ebuf iter
            smark           <-  getSelectionBoundMark ebuf
            moveMark ebuf smark iter2
            -- ### we had a problem before using this idleAdd thing
            ideR <- ask
            liftIO $ idleAdd (do
                reflectIDE (scrollToIter (sourceView buf) iter 0.0 (Just (0.3,0.3))) ideR
                return False) priorityDefaultIdle
            return ()

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
                    inBufContext () buf $ \_ ebuf buf _ -> do
                        mark <- getInsertMark ebuf
                        iter <- getIterAtMark ebuf mark
                        insert ebuf iter (dscName idDescr)

markRefInSourceBuf :: Int -> IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf index buf logRef scrollTo = do
    useCandy    <- useCandyFor buf
    candy'      <- readIDE candy
    contextRefs <- readIDE contextRefs
    prefs       <- readIDE prefs
    inBufContext () buf $ \_ ebuf buf _ -> do
        let tagName = (show $ logRefType logRef) ++ show index
        tagTable <- getTagTable ebuf
        mbTag <- lookupTag tagTable tagName
        case mbTag of
            Just existingTag -> do
                i1 <- getStartIter ebuf
                i2 <- getEndIter ebuf
                removeTagByName ebuf tagName i1 i2
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
        unless isOldContext $ applyTagByName ebuf tagName iter iter2
        when scrollTo $ placeCursor ebuf iter
        mark <- getInsertMark ebuf
        when scrollTo $ do
            ideR <- ask
            liftIO $ idleAdd (do
                reflectIDE (do
                    scrollToMark (sourceView buf) mark 0.3 Nothing
                    when (isOldContext && scrollTo) $ selectRange ebuf iter iter2) ideR
                return False) priorityDefaultIdle
            return ()

newTextBuffer :: PanePath -> String -> Maybe FilePath -> IDEM (Maybe IDEBuffer)
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
            ideMessage Normal ("File does not exist " ++ (fromJust mbfn))
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
    String ->
    String ->
    CandyTable ->
    Prefs ->
    PanePath ->
    Gtk.Notebook ->
    Gtk.Window ->
    IDEM (Maybe IDEBuffer,Connections)
builder' bs mbfn ind bn rbn ct prefs pp nb windows = do
    ideR <- ask
    -- load up and display a file
    (fileContents,modTime) <- case mbfn of
        Just fn -> do
            fc <- liftIO $ UTF8.readFile fn
            mt <- liftIO $ getModificationTime fn
            return (fc,Just mt)
        Nothing -> return ("\n",Nothing)

    buffer <- (if useYi prefs then newYiBuffer else newGtkBuffer) mbfn fileContents
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
    setStyle buffer $ case sourceStyle prefs of
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
    ids2 <- onCompletion sv (Completion.complete sv False) Completion.cancel
    ids3 <- sv `onButtonPress`
        \event -> do
            liftIO $ reflectIDE (do
                case eventClick event of
                    DoubleClick -> do
                        (start, end) <- getIdentifierUnderCursor buffer
                        liftIO $ postGUIAsync $ reflectIDE (selectRange buffer start end) ideR
                        return False
                    _ -> return False) ideR
    (GetTextPopup mbTpm) <- triggerEvent ideR (GetTextPopup Nothing)
    ids4 <- case mbTpm of
        Just tpm    -> sv `onPopulatePopup` \menu -> liftIO $ (tpm ideR menu)
        Nothing     -> do
            sysMessage Normal "SourceBuffer>> no text popup"
            return []
    ids5 <- sv `onKeyPress`
        \name modifier keyval -> do
            liftIO $ reflectIDE (do
                    let moveToNextWord iterOp sel  = do
                        sel' <- iterOp sel
                        rs <- isRangeStart sel'
                        if rs then return sel' else moveToNextWord iterOp sel'
                    let calculateNewPosition iterOp = getInsertIter buffer >>= moveToNextWord iterOp
                    let continueSelection keepSelBound nsel = do
                            im <- getInsertMark buffer
                            moveMark buffer im nsel
                            scrollToIter sv nsel 0 Nothing
                            when (not keepSelBound) $ do
                                sb <- getSelectionBoundMark buffer
                                moveMark buffer sb nsel
                    case (name, map mapControlCommand modifier, keyval) of
                        ("Left",[GtkOld.Control],_) -> do
                            calculateNewPosition backwardCharC >>= continueSelection False
                            return True
                        ("Left",[GtkOld.Shift, GtkOld.Control],_) -> do
                            calculateNewPosition backwardCharC >>= continueSelection True
                            return True
                        ("Right",[GtkOld.Control],_) -> do
                            calculateNewPosition forwardCharC >>= continueSelection False --placeCursor buffer
                            return True
                        ("Right",[GtkOld.Shift, GtkOld.Control],_) -> do
                            calculateNewPosition forwardCharC >>= continueSelection True
                            return True
                        ("BackSpace",[GtkOld.Control],_) -> do              -- delete word
                            here <- getInsertIter buffer
                            there <- calculateNewPosition backwardCharC
                            delete buffer here there
                            return True
                        ("minus",[GtkOld.Control],_) -> do
                            (start, end) <- getIdentifierUnderCursor buffer
                            slice <- getSlice buffer start end True
                            launchSymbolNavigationDialog slice goToDefinition
                            return True
                        _ -> return False
                ) ideR
    ids6 <- case sv of
        GtkEditorView sv -> do
            (liftIO $ createHyperLinkSupport sv sw (\ctrl shift iter -> do
                            (GtkEditorIter beg,GtkEditorIter en) <- reflectIDE (getIdentifierUnderCursorFromIter (GtkEditorIter iter, GtkEditorIter iter)) ideR
                            return (beg, if ctrl then en else beg)) (\_ _ slice -> do
                                        reflectIDE (launchSymbolNavigationDialog slice goToDefinition) ideR
                                        ))

#ifdef LEKSAH_WITH_YI
        _ -> return []
#endif
    return (Just buf,concat [ids1, ids2, ids3, ids4, ids5, ids6])

isIdent a = isAlphaNum a || a == '\'' || a == '_'       -- parts of haskell identifiers

isRangeStart sel = do                                   -- if char and previous char are of different char categories
    currentChar <- getChar sel
    let mbStartCharCat = getCharacterCategory currentChar
    mbPrevCharCat <- getCharacterCategory <$> (backwardCharC sel >>= getChar)
    return $ currentChar == Nothing || currentChar == Just '\n' || mbStartCharCat /= mbPrevCharCat && (mbStartCharCat == SyntaxCharacter || mbStartCharCat == IdentifierCharacter)

getIdentifierUnderCursor :: EditorBuffer -> IDEM (EditorIter, EditorIter)
getIdentifierUnderCursor buffer = do
    (startSel, endSel) <- getSelectionBounds buffer
    getIdentifierUnderCursorFromIter (startSel, endSel)

getIdentifierUnderCursorFromIter :: (EditorIter, EditorIter) -> IDEM (EditorIter, EditorIter)
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

checkModTime :: IDEBuffer -> IDEM (Bool, Bool)
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
                                                ideMessage Normal $ "Auto Loading " ++ fn
                                                revert buf
                                                return (False, True)
                                            else do
                                                window <- getMainWindow
                                                resp <- liftIO $ do
                                                    md <- messageDialogNew
                                                            (Just window) []
                                                            MessageQuestion
                                                            ButtonsNone
                                                            ("File \"" ++ name ++ "\" has changed on disk.")
                                                    dialogAddButton md "_Load From Disk" (ResponseUser 1)
                                                    dialogAddButton md "_Always Load From Disk" (ResponseUser 2)
                                                    dialogAddButton md "_Don't Load" (ResponseUser 3)
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
        Just fn -> liftIO $ do
            nmt <- getModificationTime fn
            writeIORef (modTime buf) (Just nmt)

fileRevert :: IDEAction
fileRevert = inActiveBufContext () $ \ _ _ currentBuffer _ -> do
    revert currentBuffer

revert :: IDEBuffer -> IDEAction
revert buf = do
    useCandy    <-  useCandyFor buf
    ct          <-  readIDE candy
    let name    =   paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> do
            buffer <- getBuffer (sourceView buf)
            fc <- liftIO $ UTF8.readFile fn
            mt <- liftIO $ getModificationTime fn
            beginNotUndoableAction buffer
            setText buffer fc
            if useCandy
                then modeTransformToCandy (mode buf)
                         (modeEditInCommentOrString (mode buf)) buffer
                else return ()
            endNotUndoableAction buffer
            setModified buffer False
            return mt
            liftIO $ writeIORef (modTime buf) (Just mt)

writeCursorPositionInStatusbar :: EditorView -> IDEAction
writeCursorPositionInStatusbar sv = do
    buf  <- getBuffer sv
    mark <- getInsertMark buf
    iter <- getIterAtMark buf mark
    line <- getLine iter
    col  <- getLineOffset iter
    triggerEventIDE (StatusbarChanged [CompartmentBufferPos (line,col)])
    return ()

writeOverwriteInStatusbar :: EditorView -> IDEAction
writeOverwriteInStatusbar sv = do
    mode <- getOverwrite sv
    triggerEventIDE (StatusbarChanged [CompartmentOverlay mode])
    return ()

selectInfo :: EditorView -> IDEAction
selectInfo sv = do
    ideR    <- ask
    buf     <- getBuffer sv
    (l,r)   <- getIdentifierUnderCursor buf
    symbol  <- getText buf l r True
    triggerEvent ideR (SelectInfo symbol)
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
markLabelAsChanged nb buf = do
    ebuf   <- getBuffer (sourceView buf)
    modified <- getModified ebuf
    liftIO $ markLabel nb (getTopWidget buf) modified

fileSaveBuffer :: Bool -> Notebook -> EditorBuffer -> IDEBuffer -> Int -> IDEM Bool
fileSaveBuffer query nb ebuf ideBuf i = do
    ideR    <- ask
    window  <- getMainWindow
    prefs   <- readIDE prefs
    useCandy <- useCandyFor ideBuf
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    mbpage <- liftIO $ notebookGetNthPage nb i
    case mbpage of
        Nothing     -> throwIDE "fileSave: Page not found"
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
                                    (Just $ "Save File")
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
                                                "File already exist."
                                        dialogAddButton md "_Overwrite" ResponseYes
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
                                        newTextBuffer panePath (takeFileName cfn) (Just cfn)
                                        ) ideR
                                    return True
                                _          -> return False
    where
        fileSave' :: Bool -> Bool -> Notebook -> IDEBuffer -> Bool -> CandyTable -> FilePath -> IDEAction
        fileSave' forceLineEnds removeTBlanks nb ideBuf useCandy candyTable fn = do
            buf     <-   getBuffer $ sourceView ideBuf
            text    <-   getCandylessText candyTable buf
            let text' = if removeTBlanks
                            then unlines $ map removeTrailingBlanks $lines text
                            else text
            succ <- liftIO $ catch (do UTF8.writeFile fn text'; return True)
                (\e -> do
                    sysMessage Normal (show e)
                    return False)
            setModified buf (not succ)
            markLabelAsChanged nb ideBuf
        removeTrailingBlanks :: String -> String
        removeTrailingBlanks = reverse . dropWhile (\c -> c == ' ') . reverse

fileSave :: Bool -> IDEM Bool
fileSave query = inActiveBufContext False $ fileSaveBuffer query

fileSaveAll :: (IDEBuffer -> IDEM Bool) -> IDEM Bool
fileSaveAll filterFunc = do
    bufs     <- allBuffers
    filtered <- filterM filterFunc bufs
    results  <- forM filtered (\buf -> inBufContext False buf (fileSaveBuffer False))
    return $ True `elem` results

fileCheckBuffer :: Notebook -> EditorBuffer -> IDEBuffer -> Int -> IDEM Bool
fileCheckBuffer nb ebuf ideBuf i = do
    let mbfn = fileName ideBuf
    if isJust mbfn
        then do (_, modifiedOnDisk) <- checkModTime ideBuf -- The user is given option to reload
                modifiedInBuffer    <- getModified ebuf
                return (modifiedOnDisk || modifiedInBuffer)
        else return False

fileCheckAll :: (IDEBuffer -> IDEM (Maybe alpha)) -> IDEM [alpha]
fileCheckAll filterFunc = do
    bufs     <- allBuffers
    foldM (\ packs buf -> do
            mbFilt <- filterFunc buf
            case mbFilt of
                Nothing -> return packs
                Just p  -> do
                    modified <- inBufContext False buf fileCheckBuffer
                    if modified
                        then return (p : packs)
                        else return packs) [] bufs

fileNew :: IDEAction
fileNew = do
    prefs   <- readIDE prefs
    pp      <- getBestPathForId  "*Buffer"
    newTextBuffer pp "Unnamed" Nothing
    return ()

fileClose :: IDEM Bool
fileClose = inActiveBufContext True $ fileClose'

fileClose' :: Notebook -> EditorBuffer -> IDEBuffer -> Int  -> IDEM Bool
fileClose' nb ebuf currentBuffer i = do
    window  <- getMainWindow
    modified <- getModified ebuf
    cancel <- reifyIDE $ \ideR   ->  do
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsCancel
                                            ("Save changes to document: "
                                                ++ paneName currentBuffer
                                                ++ "?")
                dialogAddButton md "_Save" ResponseYes
                dialogAddButton md "_Don't Save" ResponseNo
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
    mbActivePack    <-  readIDE activePack
    bufs            <-  allBuffers
    when (not (null bufs) && isJust mbActivePack) $ do
        mapM_ (close' (fromJust mbActivePack)) bufs
    where
        close' activePack buf = do
            (pane,_)    <-  guiPropertiesFromName (paneName buf)
            nb          <-  getNotebook pane
            mbI         <-  liftIO $notebookPageNum nb (scrolledWindow buf)
            case mbI of
                Nothing ->  throwIDE "notebook page not found: unexpected"
                Just i  ->  do
                    ebuf <- getBuffer (sourceView buf)
                    let dir = dropFileName $ ipdCabalFile activePack
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb ebuf buf i; return ()

fileCloseAllButWorkspace :: IDEAction
fileCloseAllButWorkspace = do
    mbWorkspace     <-  readIDE workspace
    bufs            <-  allBuffers
    when (not (null bufs) && isJust mbWorkspace) $ do
        mapM_ (close' (fromJust mbWorkspace)) bufs
    where
        close' workspace buf = do
            (pane,_)    <-  guiPropertiesFromName (paneName buf)
            nb          <-  getNotebook pane
            mbI         <-  liftIO $notebookPageNum nb (scrolledWindow buf)
            case mbI of
                Nothing ->  throwIDE "notebook page not found: unexpected"
                Just i  ->  do
                    ebuf <- getBuffer (sourceView buf)
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPathOfAny workspace (fromJust (fileName buf))))
                            $ do fileClose' nb ebuf buf i; return ()
        isSubPathOfAny workspace fileName =
            let pathes = map (dropFileName . ipdCabalFile) (wsPackages workspace)
            in  or (map (\dir -> isSubPath dir fileName) pathes)


fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbBuf <- maybeActiveBuf
    mbFileName <- liftIO $ do
        dialog <- fileChooserDialogNew
                        (Just $ "Open File")
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
                        "Buffer already open."
                dialogAddButton md "Make _Active" (ResponseUser 1)
                dialogAddButton md "_Open Second" (ResponseUser 2)
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
            newTextBuffer pp (takeFileName fpc) (Just fpc)
            return ()

editUndo :: IDEAction
editUndo = inActiveBufContext () $ \_ buf _ _ -> do
    can <- canUndo buf
    when can $ undo buf

editRedo :: IDEAction
editRedo = inActiveBufContext () $ \_ buf _ _ -> do
    can <- canRedo buf
    when can $ redo buf

editDelete :: IDEAction
editDelete = inActiveBufContext ()  $ \_ ebuf _ _ ->  do
    deleteSelection ebuf True True
    return ()

editSelectAll :: IDEAction
editSelectAll = inActiveBufContext () $ \_ ebuf _ _ -> do
    start <- getStartIter ebuf
    end   <- getEndIter ebuf
    selectRange ebuf start end

editCut :: IDEAction
editCut = inActiveBufContext () $ \_ ebuf _ _ -> do
    clip <- liftIO $ clipboardGet selectionClipboard
    cutClipboard ebuf clip True

editCopy :: IDEAction
editCopy = inActiveBufContext () $ \_ ebuf _ _ -> do
    clip <- liftIO $ clipboardGet selectionClipboard
    copyClipboard ebuf clip

editPaste :: IDEAction
editPaste = inActiveBufContext () $ \_ ebuf _ _ -> do
    mark <- getInsertMark ebuf
    iter <- getIterAtMark ebuf mark
    clip <- liftIO $ clipboardGet selectionClipboard
    pasteClipboard ebuf clip iter True

editShiftLeft :: IDEAction
editShiftLeft = do
    prefs <- readIDE prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
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
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
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
            return (lineNr, elemIndex char line)
    alignChar :: Map Int (Maybe Int) -> Int -> IDEM ()
    alignChar positions alignTo = do
            doForSelectedLines [] $ \ebuf lineNr -> do
                case lineNr `Map.lookup` positions of
                    Just (Just n)  ->  do
                        sol       <- getIterAtLine ebuf lineNr
                        insertLoc <- forwardCharsC sol n
                        insert ebuf insertLoc (replicate (alignTo - n) ' ')
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

selectedText :: IDEM (Maybe String)
selectedText = do
    candy' <- readIDE candy
    inActiveBufContext Nothing $ \_ ebuf currentBuffer _ -> do
        hasSelection <- hasSelection ebuf
        if hasSelection
            then do
                (i1,i2)   <- getSelectionBounds ebuf
                text      <- getCandylessPart candy' ebuf i1 i2
                return $ Just text
            else return Nothing

selectedTextOrCurrentLine :: IDEM (Maybe String)
selectedTextOrCurrentLine = do
    candy' <- readIDE candy
    inActiveBufContext Nothing $ \_ ebuf currentBuffer _ -> do
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
    inActiveBufContext Nothing $ \_ ebuf currentBuffer _ -> do
        useCandy   <- useCandyFor currentBuffer
        (start, _) <- getSelectionBounds ebuf
        line       <- getLine start
        lineOffset <- getLineOffset start
        res <- if useCandy
            then positionFromCandy candy' ebuf (line, lineOffset)
            else return (line, lineOffset)
        return $ Just res

insertTextAfterSelection :: String -> IDEAction
insertTextAfterSelection str = do
    candy'       <- readIDE candy
    inActiveBufContext () $ \_ ebuf currentBuffer _ -> do
        useCandy     <- useCandyFor currentBuffer
        hasSelection <- hasSelection ebuf
        when hasSelection $ do
            realString <-  if useCandy then stringToCandy candy' str else return str
            (_,i)      <- getSelectionBounds ebuf
            mark       <- createMark ebuf i True
            insert ebuf i realString
            i1         <- getIterAtMark ebuf mark
            i2         <- forwardCharsC i1 (length str)
            selectRange ebuf i1 i2

-- | Returns the package, to which this buffer belongs, if possible
belongsToPackage :: IDEBuffer -> IDEM(Maybe IDEPackage)
belongsToPackage ideBuf | fileName ideBuf == Nothing        = return Nothing
                        | not (isHaskellMode (mode ideBuf))  = return Nothing
                        | otherwise                        = do
    bufferToProject' <-  readIDE bufferProjCache
    ws               <-  readIDE workspace
    let fp           =   fromJust (fileName ideBuf)
    case Map.lookup fp bufferToProject' of
        Just p  -> return p
        Nothing -> case ws of
                        Nothing   -> return Nothing
                        Just workspace -> do
                            mbMn <- liftIO $ moduleNameFromFilePath fp
                            let mbMn2 = case mbMn of
                                            Nothing -> Nothing
                                            Just mn -> simpleParse mn
                            let res = foldl (belongsToPackage' fp mbMn2) Nothing (wsPackages workspace)
                            modifyIDE_ (\ide -> ide{bufferProjCache = Map.insert fp res bufferToProject'})
                            return res

belongsToPackage' ::  FilePath -> Maybe ModuleName -> Maybe IDEPackage -> IDEPackage -> Maybe IDEPackage
belongsToPackage' _ _ r@(Just pack) _ = r
belongsToPackage' fp mbModuleName Nothing pack =
    let basePath =  dropFileName $ ipdCabalFile pack
    in if isSubPath basePath fp
        then
            let srcPaths = map (\srcP -> basePath </> srcP) (ipdSrcDirs pack)
                relPaths = map (\p -> makeRelative p fp) srcPaths
            in if or (map (\p -> Set.member p (ipdExtraSrcs pack)) relPaths)
                then Just pack
                else case mbModuleName of
                        Nothing -> Nothing
                        Just mn -> if Set.member mn (ipdModules pack)
                                        then Just pack
                                        else Nothing
        else Nothing

belongsToWorkspace b =  belongsToPackage b >>= return . isJust

useCandyFor :: IDEBuffer -> IDEM Bool
useCandyFor aBuffer = do
    use <- getCandyState
    return (use && isHaskellMode (mode aBuffer))

editCandy = do
    use <- getCandyState
    buffers <- allBuffers
    if use
        then mapM_ (\b -> modeEditToCandy (mode b)
            (modeEditInCommentOrString (mode b))) buffers
        else mapM_ (modeEditFromCandy . mode) buffers
