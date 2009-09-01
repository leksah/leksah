{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC  -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances -XScopedTypeVariables #-}
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

,   fileNew
,   fileOpenThis
,   fileOpen
,   fileRevert
,   fileClose
,   fileCloseAll
,   fileCloseAllButPackage
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
) where

import Prelude hiding(getChar, getLine)
import Control.Monad.Reader
import System.IO hiding(getChar, getLine)
import System.FilePath
import System.Directory
import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List hiding(insert, delete)
import Data.Maybe
import Data.Typeable
import System.Time

import Graphics.UI.Gtk as Gtk
    hiding(afterFocusIn, afterModifiedChanged, afterMoveCursor, afterToggleOverwrite, background, onButtonPress,
    onPopulatePopup)
import Graphics.UI.Gtk.Gdk.Events as Gtk

import IDE.Core.State
import IDE.FileUtils
import IDE.SourceCandy
import IDE.Completion as Completion (complete,cancel)
import IDE.TextEditor
import Debug.Trace (trace)
import qualified System.IO.UTF8 as UTF8
import Data.IORef (writeIORef,readIORef,newIORef,IORef(..))
import Graphics.UI.Frame.Panes (IDEPane(..))
import Data.Char (isAlphaNum)
import Control.Event (triggerEvent)
import SrcLoc
    (srcLocCol, srcLocLine, srcSpanEnd, srcSpanStart)
import IDE.Metainfo.GHCUtils (parseHeader)
import GHC (SrcLoc(..), unLoc, moduleNameString, HsModule(..))

--
-- | A text editor pane description
--
data IDEBuffer      =   IDEBuffer {
    fileName        ::  Maybe FilePath
,   bufferName      ::  String
,   addedIndex      ::  Int
,   sourceView      ::  EditorView
,   scrolledWindow  ::  ScrolledWindow
,   modTime         ::  IORef (Maybe (ClockTime))
} deriving (Typeable)

data BufferState            =   BufferState FilePath Int
                            |   BufferStateTrans String String Int
    deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDEBuffer
instance Pane IDEBuffer IDEM
    where
    primPaneName    =   bufferName
    getAddedIndex   =   addedIndex
    getTopWidget    =   castToWidget . scrolledWindow
    paneId b        =   case fileName b of
                            Just s  -> s
                            Nothing -> "?" ++ bufferName b
    makeActive actbuf = do
      ideR    <-  ask
      sbLC    <-  getStatusbarLC
      sbIO    <-  getStatusbarIO
      infos   <-  readIDE accessibleInfo
      let sv = sourceView actbuf
      eBuf    <- getBuffer sv
      liftIO $ bringPaneToFront actbuf
      writeCursorPositionInStatusbar sv sbLC
      writeOverwriteInStatusbar sv sbIO
      ids1 <- eBuf `afterModifiedChanged` markActiveLabelAsChanged
      ids2 <- sv `afterMoveCursor` writeCursorPositionInStatusbar sv sbLC
      ids3 <- sv `onLookupInfo` showInfo sv
      ids4 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv sbIO
      activatePane actbuf $ concat [ids1, ids2, ids3, ids4]
      triggerEventIDE (Sensitivity [(SensitivityEditor, True)])
      checkModTime actbuf
      return ()
    close pane = do makeActive pane
                    fileClose

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
                return ()
            Nothing -> return ()
    recoverState pp (BufferStateTrans bn text i) =   do
        mbbuf    <-  newTextBuffer pp bn Nothing
        useCandy    <- getCandyState
        case mbbuf of
            Just buf -> do
                candy'  <- readIDE candy
                gtkBuf  <-  getBuffer (sourceView buf)
                setText gtkBuf text

                when useCandy $ transformToCandy candy' gtkBuf
                iter    <-  getIterAtOffset gtkBuf i
                placeCursor gtkBuf iter
                mark    <-  getInsertMark gtkBuf
                ideR    <- ask
                liftIO $ idleAdd  (do
                    reflectIDE (scrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))) ideR
                    return False) priorityDefaultIdle
                return ()
            Nothing -> return ()


startComplete :: IDEAction
startComplete = do
    trace "start complete" return ()
    mbBuf <- maybeActiveBuf
    currentState' <- readIDE currentState
    case mbBuf of
        Nothing     -> return ()
        Just buf    -> complete (sourceView buf) True

selectSourceBuf :: FilePath -> IDEM (Maybe IDEBuffer)
selectSourceBuf fp = do
    fpc <-  liftIO $ canonicalizePath fp
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
                else return Nothing

recentSourceBuffers :: IDEM [PaneName]
recentSourceBuffers = do
    recentPanes' <- readIDE recentPanes
    mbBufs       <- mapM mbPaneFromName recentPanes'
    return $ map paneName ((catMaybes $ map (\ (PaneC p) -> cast p) $ catMaybes mbBufs) :: [IDEBuffer])

lastActiveBufferPane :: IDEM (Maybe PaneName)
lastActiveBufferPane = do
    rs <- recentSourceBuffers
    case rs of
        (hd : _) -> return (Just hd)
        _        -> return Nothing

goToDefinition :: Descr -> IDEAction
goToDefinition idDescr = do
    mbAccesibleInfo      <-  readIDE accessibleInfo
    mbCurrentInfo        <-  readIDE currentInfo
    if isJust mbAccesibleInfo && isJust mbCurrentInfo
        then do
            let packageId       =   pack $ descrModu idDescr
            let mbPack          =   case packageId `Map.lookup` fst
                                            (fromJust mbAccesibleInfo) of
                                        Just it ->  Just it
                                        Nothing ->  packageId `Map.lookup` fst (fst
                                                                 (fromJust mbCurrentInfo))
            case mbPack of
                Just pack       ->  case filter (\md -> moduleIdMD md == descrModu idDescr)
                                                    (exposedModulesPD pack) of
                                        (mod : tl)   ->  if isJust (mbSourcePathMD mod)
                                                        then goToSourceDefinition
                                                                (fromJust $ mbSourcePathMD mod)
                                                                (mbLocation idDescr)
                                                        else return ()
                                        []          -> do ideMessage Normal "no module"
                Nothing         ->  do ideMessage Normal "no package"
        else ideMessage Normal  "no infos"

goToSourceDefinition :: FilePath -> Maybe Location -> IDEAction
goToSourceDefinition fp mbLocation = do
    mbBuf     <- selectSourceBuf fp
    when (isJust mbBuf && isJust mbLocation) $
        inActiveBufContext () $ \_ ebuf buf _ -> do
            let location    =   fromJust mbLocation
            lines           <-  getLineCount ebuf
            iter            <-  getIterAtLine ebuf (max 0 (min (lines-1)
                                    ((locationSLine location) -1)))
            chars           <-  getCharsInLine iter
            setLineOffset iter (max 0 (min (chars-1) (locationSCol location)))
            iter2           <-  getIterAtLine ebuf (max 0 (min (lines-1)
                                    ((locationELine location) -1)))
            chars2          <-  getCharsInLine iter2
            setLineOffset iter2 (max 0 (min (chars2-1) (locationECol location)))
            placeCursor ebuf iter
            smark           <-  getSelectionBoundMark ebuf
            moveMark ebuf smark iter2
            -- ### we had a problem before using this idleAdd thing
            ideR <- ask
            liftIO $ idleAdd (do
                reflectIDE (scrollToIter (sourceView buf) iter 0.0 (Just (0.3,0.3))) ideR
                return False) priorityDefaultIdle
            return ()

markRefInSourceBuf :: Int -> IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf index buf logRef scrollTo = do
    useCandy    <- getCandyState
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

        let start' = srcLocToPair $ srcSpanStart (logRefSrcSpan logRef)
        let end'   = srcLocToPair $ srcSpanEnd   (logRefSrcSpan logRef)
        start <- if useCandy
                    then positionToCandy candy' ebuf start'
                    else return start'
        end   <- if useCandy
                    then positionToCandy candy' ebuf end'
                    else return end'
        lines   <-  getLineCount ebuf
        iter    <-  getIterAtLine ebuf (max 0 (min (lines-1) ((fst start)-1)))
        chars   <-  getCharsInLine iter
        setLineOffset iter (max 0 (min (chars-1) (snd start)))

        iter2 <- if start == end
            then do
                copy <- copyIter iter
                forwardWordEnd copy
                return copy
            else do
                new     <-  getIterAtLine ebuf (max 0 (min (lines-1) ((fst end)-1)))
                chars   <-  getCharsInLine new
                setLineOffset new (max 0 (min (chars-1) (snd end)))
                forwardChar new
                return new

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

srcLocToPair :: SrcLoc -> (Int,Int)
srcLocToPair srcLoc = (srcLocLine srcLoc, srcLocCol srcLoc)

allBuffers :: IDEM [IDEBuffer]
allBuffers = getPanes

maybeActiveBuf :: IDEM (Maybe IDEBuffer)
maybeActiveBuf = do
    mbActivePane <- getActivePane
    mbPane       <- lastActiveBufferPane
    case (mbPane,mbActivePane) of
        (Just paneName1, Just (paneName2,_)) | paneName1 == paneName2 -> do
            (PaneC pane) <- paneFromName paneName1
            let mbActbuf = cast pane
            return mbActbuf
        _ -> return Nothing

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
            pane    <-  newPane panePath nb (builder bs mbfn ind bn rbn ct prefs)
            return (Just pane)
        else do
            ideMessage Normal ("File does not exist " ++ (fromJust mbfn))
            return Nothing

builder :: Bool ->
    Maybe FilePath ->
    Int ->
    String ->
    String ->
    CandyTable ->
    Prefs ->
    PanePath ->
    Gtk.Notebook ->
    Gtk.Window ->
    IDERef ->
    IO (IDEBuffer,Connections)
builder bs mbfn ind bn rbn ct prefs pp nb windows ideR = do
    reflectIDE (do
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
        when bs $ transformToCandy ct buffer
        endNotUndoableAction buffer
        setModified buffer False
        siter <- getStartIter buffer
        placeCursor buffer siter
        iter <- getEndIter buffer
        createMark buffer (Just "end") iter True

        -- create a new SourceView Widget
        sv <- newView buffer
        setFont sv $ textviewFont prefs
        setShowLineNumbers sv $ showLineNumbers prefs
        setRightMargin sv $ rightMargin prefs
        setIndentWidth sv $ tabWidth prefs
        setTabWidth sv $ tabWidth prefs
        setStyle buffer $ sourceStyle prefs

        -- put it in a scrolled window
        sw <- getScrolledWindow sv
        liftIO $ scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        liftIO $ scrolledWindowSetShadowType sw ShadowIn
        modTimeRef <- liftIO $ newIORef modTime
        let buf = IDEBuffer mbfn bn ind sv sw modTimeRef
        -- events
        ids1 <- sv `afterFocusIn` makeActive buf
        ids2 <- onCompletion sv (Completion.complete sv False) Completion.cancel
        ids3 <- sv `onButtonPress`
            \event -> do
                let click = eventClick event
                liftIO $ reflectIDE (do
                    case click of
                        DoubleClick -> do
                            let isSelectChar a = (isAlphaNum a) || (a == '_')
                            (start, end) <- getSelectionBounds buffer
                            mbStartChar <- getChar start
                            mbEndChar <- getChar end
                            case mbStartChar of
                                Just startChar | isSelectChar startChar -> do
                                    found <- backwardFindChar start (not.isSelectChar) Nothing
                                    when found $ do
                                        forwardChar start
                                        return ()
                                _ -> return ()
                            case mbEndChar of
                                Just endChar | isSelectChar endChar -> do
                                    forwardFindChar end (not.isSelectChar) Nothing
                                    return ()
                                _ -> return ()
                            selectRange buffer start end
                            return True
                        _ -> return False) ideR
        (GetTextPopup mbTpm) <- triggerEvent ideR (GetTextPopup Nothing)
        ids4 <- case mbTpm of
            Just tpm    -> sv `onPopulatePopup` \menu -> liftIO $ (tpm ideR menu)
            Nothing     -> do
                sysMessage Normal "SourceBuffer>> no text popup"
                return []
        return (buf,concat [ids1, ids2, ids3, ids4])) ideR


checkModTime :: IDEBuffer -> IDEM Bool
checkModTime buf = do
    currentState' <- readIDE currentState
    case  currentState' of
        IsShuttingDown -> return False
        _              -> reifyIDE (\ideR -> do
            let name = paneName buf
            case fileName buf of
                Just fn -> do
                    exists <- doesFileExist fn
                    if exists
                        then do
                            nmt <- getModificationTime fn
                            modTime' <- readIORef (modTime buf)
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
                                                    md <- messageDialogNew
                                                            Nothing []
                                                            MessageQuestion
                                                            ButtonsYesNo
                                                            ("File \"" ++ name ++ "\" has changed on disk. Load file from disk?")
                                                    resp <- dialogRun md
                                                    case resp of
                                                        ResponseYes ->  do
                                                            reflectIDE (revert buf) ideR
                                                            liftIO $ widgetHide md
                                                            return False
                                                        ResponseNo  -> do
                                                            writeIORef (modTime buf) (Just nmt)
                                                            widgetHide md
                                                            return True
                                                        _           ->  do return False
                                                else return False
                        else return False
                Nothing -> return False)

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
    useCandy    <-  getCandyState
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
                then transformToCandy ct buffer
                else return ()
            endNotUndoableAction buffer
            setModified buffer False
            return mt
            liftIO $ writeIORef (modTime buf) (Just mt)

writeCursorPositionInStatusbar :: EditorView -> Statusbar -> IDEAction
writeCursorPositionInStatusbar sv sb = do
    buf  <- getBuffer sv
    mark <- getInsertMark buf
    iter <- getIterAtMark buf mark
    line <- getLine iter
    col  <- getLineOffset iter
    liftIO $ statusbarPop sb 1
    liftIO $ statusbarPush sb 1 $printf "Ln %4d, Col %3d" (line + 1) (col + 1)
    return ()

writeOverwriteInStatusbar :: EditorView -> Statusbar -> IDEAction
writeOverwriteInStatusbar sv sb = do
    modi <- getOverwrite sv
    liftIO $ statusbarPop sb 1
    liftIO $ statusbarPush sb 1 $ if modi then "OVR" else "INS"
    return ()


showInfo :: EditorView -> IDEAction
showInfo sv = do
    ideR    <- ask
    buf     <- getBuffer sv
    (l,r)   <- getSelectionBounds buf
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

inBufContext :: alpha -> IDEBuffer -> (Notebook -> EditorBuffer -> IDEBuffer -> Int -> IDEM alpha) -> IDEM alpha
inBufContext def ideBuf f = do
    (pane,_)       <-  guiPropertiesFromName (paneName ideBuf)
    nb             <-  getNotebook pane
    mbI            <-  liftIO $notebookPageNum nb (scrolledWindow ideBuf)
    case mbI of
        Nothing ->  liftIO $ do
            sysMessage Normal $ bufferName ideBuf ++ " notebook page not found: unexpected"
            return def
        Just i  ->  do
            ebuf <- getBuffer (sourceView ideBuf)
            f nb ebuf ideBuf i

inActiveBufContext :: alpha -> (Notebook -> EditorBuffer -> IDEBuffer -> Int -> IDEM alpha) -> IDEM alpha
inActiveBufContext def f = do
    mbBuf                  <- maybeActiveBuf
    case mbBuf of
        Nothing         -> return def
        Just ideBuf -> do
            inBufContext def ideBuf f

fileSaveBuffer :: Bool -> Notebook -> EditorBuffer -> IDEBuffer -> Int -> IDEM Bool
fileSaveBuffer query nb ebuf ideBuf i = do
    ideR    <- ask
    window  <- getMainWindow
    prefs   <- readIDE prefs
    bs      <- getCandyState
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    mbpage <- liftIO $ notebookGetNthPage nb i
    case mbpage of
        Nothing     -> throwIDE "fileSave: Page not found"
        Just page   ->
            if isJust mbfn && query == False
                then do modifiedOnDisk <- checkModTime ideBuf -- The user is given option to reload
                        modifiedInBuffer <- getModified ebuf
                        if (modifiedOnDisk || modifiedInBuffer)
                            then do
                                fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf bs candy $fromJust mbfn
                                setModTime ideBuf
                                return True
                            else return False
                else reifyIDE $ \ideR   ->  do
                    dialog <- fileChooserDialogNew
                                    (Just $ "Save File")
                                    (Just window)
                                FileChooserActionSave
                                [("gtk-cancel"     --buttons to display
                                ,ResponseCancel)  --you can use stock buttons
                                ,("gtk-save"
                                , ResponseAccept)]
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
                                                ButtonsYesNo
                                                "File already exist. Overwrite?"
                                        resp <- dialogRun md
                                        widgetHide md
                                        return resp
                                else return ResponseYes
                            case resp of
                                ResponseYes -> do
                                    cfn <- canonicalizePath fn

                                    reflectIDE (do
                                        fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf bs candy cfn
                                        close ideBuf
                                        newTextBuffer panePath (takeFileName fn) (Just cfn)
                                        )   ideR
                                    return True
                                ResponseNo -> return False
                                _          -> return False
    where
        fileSave' :: Bool -> Bool -> Notebook -> IDEBuffer -> Bool -> CandyTable -> FilePath -> IDEAction
        fileSave' forceLineEnds removeTBlanks nb ideBuf bs ct fn = do
            buf     <-   getBuffer $ sourceView ideBuf
            text    <-   getCandylessText ct buf
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
    ideR    <- ask
    window  <- getMainWindow
    prefs   <- readIDE prefs
    bs      <- getCandyState
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    mbpage <- liftIO $ notebookGetNthPage nb i
    case mbpage of
        Nothing     -> throwIDE "fileCheck: Page not found"
        Just page   ->
            if isJust mbfn
                then do modifiedOnDisk <- checkModTime ideBuf -- The user is given option to reload
                        modifiedInBuffer <- getModified ebuf
                        return (modifiedOnDisk || modifiedInBuffer)
                else return False

fileCheckAll :: (IDEBuffer -> IDEM Bool) -> IDEM Bool
fileCheckAll filterFunc = do
    bufs    <- allBuffers
    filtered <- filterM filterFunc bufs
    results <- forM filtered (\buf -> inBufContext False buf fileCheckBuffer)
    return $ True `elem` results

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
                                            ButtonsNone
                                            ("Save changes to document: "
                                                ++ paneName currentBuffer
                                                ++ "?")
                dialogAddButton md "_Save" ResponseYes
                dialogAddButton md "_Don't Save" ResponseNo
                dialogAddButton md "_Cancel" ResponseCancel
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
            closePane currentBuffer
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
                    let dir = dropFileName $ cabalFile activePack
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb ebuf buf i; return ()

fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbFileName <- liftIO $ do
        dialog <- fileChooserDialogNew
                        (Just $ "Open File")
                        (Just window)
                    FileChooserActionOpen
                    [("gtk-cancel"
                    ,ResponseCancel)
                    ,("gtk-open"
                    ,ResponseAccept)]
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
    fpc <-  liftIO $canonicalizePath fp
    buffers <- allBuffers
    let buf = filter (\b -> case fileName b of
                        Just fn -> equalFilePath fn fpc
                        Nothing -> False) buffers
    case buf of
        hdb:tl -> do
            md <- liftIO $messageDialogNew
                    Nothing []
                    MessageQuestion
                    ButtonsYesNo
                    ("Buffer already open. " ++
                     "Make active instead of opening a second time?")
            resp <- liftIO $dialogRun md
            liftIO $ widgetDestroy md
            case resp of
                ResponseNo  ->  reallyOpen prefs fpc
                _           ->  makeActive hdb
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
    cb   <- liftIO $ atomNew "GDK_SELECTION_CLIPBOARD"
    clip <- liftIO $ clipboardGet cb
    cutClipboard ebuf clip True

editCopy :: IDEAction
editCopy = inActiveBufContext () $ \_ ebuf _ _ -> do
    cb   <- liftIO $ atomNew "GDK_SELECTION_CLIPBOARD"
    clip <- liftIO $ clipboardGet cb
    copyClipboard ebuf clip

editPaste :: IDEAction
editPaste = inActiveBufContext () $ \_ ebuf _ _ -> do
    cb   <- liftIO $ atomNew "GDK_SELECTION_CLIPBOARD"
    mark <- getInsertMark ebuf
    iter <- getIterAtMark ebuf mark
    clip <- liftIO $ clipboardGet cb
    pasteClipboard ebuf clip iter True

getStartAndEndLineOfSelection :: EditorBuffer -> IDEM (Int,Int)
getStartAndEndLineOfSelection ebuf = do
    startMark   <- getInsertMark ebuf
    endMark     <- getSelectionBoundMark ebuf
    startIter   <- getIterAtMark ebuf startMark
    endIter     <- getIterAtMark ebuf endMark
    startLine   <- getLine startIter
    endLine     <- getLine endIter
    let (startLine',endLine',endIter') = if endLine >=  startLine
            then (startLine,endLine,endIter)
            else (endLine,startLine,startIter)
    b           <- startsLine endIter'
    let endLineReal = if b then endLine' - 1 else endLine'
    return (startLine',endLineReal)

doForSelectedLines :: [a] -> (EditorBuffer -> EditorIter -> Int -> IDEM a) -> IDEM [a]
doForSelectedLines d f = inActiveBufContext d $ \_ ebuf currentBuffer _ -> do
    (start,end) <- getStartAndEndLineOfSelection ebuf
    iter        <- getStartIter ebuf
    mapM (f ebuf iter) [start .. end]

editComment :: IDEAction
editComment = do
    doForSelectedLines [] $ \ebuf iter lineNr -> do
        setLine iter lineNr
        insert ebuf iter "--"
    return ()

editUncomment :: IDEAction
editUncomment = do
    doForSelectedLines [] $ \ebuf iter lineNr -> do
        setLine iter lineNr
        iter2 <- copyIter iter
        forwardChars iter 2
        str   <- getText ebuf iter iter2 True
        if str == "--"
            then do delete ebuf iter iter2
            else return ()
    return ()

editShiftLeft :: IDEAction
editShiftLeft = do
    prefs <- readIDE prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
    b <- canShiftLeft str prefs
    if b
        then do
            doForSelectedLines [] $ \ebuf iter lineNr -> do
                setLine iter lineNr
                iter2 <- copyIter iter
                forwardChars iter (tabWidth prefs)
                delete ebuf iter iter2
            return ()
        else return ()
    where
    canShiftLeft str prefs = do
        boolList <- doForSelectedLines [] $ \ebuf iter lineNr -> do
            setLine iter lineNr
            iter2 <- copyIter iter
            forwardChars iter (tabWidth prefs)
            str1 <- getText ebuf iter iter2 True
            return (str1 == str)
        return (foldl' (&&) True boolList)


editShiftRight :: IDEAction
editShiftRight = do
    prefs <- readIDE prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
    doForSelectedLines [] $ \ebuf iter lineNr -> do
        setLine iter lineNr
        insert ebuf iter str
    return ()

editToCandy :: IDEAction
editToCandy = do
    ct <- readIDE candy
    inActiveBufContext () $ \_ ebuf _ _ -> do
        transformToCandy ct ebuf

editFromCandy :: IDEAction
editFromCandy = do
    ct      <-  readIDE candy
    inActiveBufContext () $ \_ ebuf _ _ -> do
        transformFromCandy ct ebuf

editKeystrokeCandy :: Maybe Char -> IDEAction
editKeystrokeCandy c = do
    ct <- readIDE candy
    inActiveBufContext () $ \_ ebuf _ _ -> do
        keystrokeCandy ct c ebuf

editCandy :: IDEAction
editCandy = do
    ct      <- readIDE candy
    buffers <- allBuffers
    gtkbufs <- mapM (\ b -> getBuffer (sourceView b)) buffers
    bs <- getCandyState
    if bs
        then mapM_ (transformToCandy ct) gtkbufs
        else mapM_ (transformFromCandy ct) gtkbufs

alignChar :: Char -> IDEAction
alignChar char = do
    positions     <- positionsOfChar
    let alignTo = foldl' max 0 (catMaybes (map snd positions))
    if (alignTo > 0)
        then alignChar (Map.fromList positions) alignTo
        else return ()
    where
    positionsOfChar :: IDEM ([(Int, Maybe Int)])
    positionsOfChar = doForSelectedLines [] $ \ebuf iter lineNr -> do
            setLine iter lineNr
            iter2 <- copyIter iter
            forwardToLineEnd iter2
            line  <- getText ebuf iter iter2 True
            return (lineNr, elemIndex char line)
    alignChar :: Map Int (Maybe Int) -> Int -> IDEM ()
    alignChar positions alignTo = do
            doForSelectedLines [] $ \ebuf iter lineNr -> do
                case lineNr `Map.lookup` positions of
                    Just (Just n)  ->  do
                        setLine iter lineNr
                        forwardChars iter n
                        insert ebuf iter (replicate (alignTo - n) ' ')
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
                iEnd <- copyIter iStart
                forwardToLineEnd iEnd
                return (iStart, iEnd)
        fmap Just $ getCandylessPart candy' ebuf i1 i2

selectedLocation :: IDEM (Maybe (Int, Int))
selectedLocation = do
    useCandy    <- getCandyState
    candy'      <- readIDE candy
    inActiveBufContext Nothing $ \_ ebuf currentBuffer _ -> do
        (start, _) <- getSelectionBounds ebuf
        line       <- getLine start
        lineOffset <- getLineOffset start
        res <- if useCandy
            then positionFromCandy candy' ebuf (line, lineOffset)
            else return (line, lineOffset)
        return $ Just res

-- " ++ ++ ++ alpha

insertTextAfterSelection :: String -> IDEAction
insertTextAfterSelection str = do
        candy'       <- readIDE candy
        useCandy     <- getCandyState
        inActiveBufContext () $ \_ ebuf currentBuffer _ -> do
        hasSelection <- hasSelection ebuf
        when hasSelection $ do
            realString <-  if useCandy then stringToCandy candy' str else return str
            (_,i)      <- getSelectionBounds ebuf
            mark       <- createMark ebuf Nothing i True
            insert ebuf i realString
            i1         <- getIterAtMark ebuf mark
            i2         <- copyIter i1
            forwardChars i2 (length str)
            selectRange ebuf i1 i2

selectedModuleName :: IDEM (Maybe String)
selectedModuleName = do
    candy' <- readIDE candy
    inActiveBufContext Nothing $ \_ ebuf currentBuffer _ -> do
        case fileName currentBuffer of
            Just filePath -> do
                text <- getCandylessText candy' ebuf
                parseResult <- parseHeader filePath text
                case parseResult of
                     Just HsModule{ hsmodName = Just name }
                        -> return $ Just $ moduleNameString (unLoc name)
                     _  -> return Nothing
            Nothing -> return Nothing

