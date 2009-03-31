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
,   standardSourcePanePath
,   selectSourceBuf
,   goToSourceDefinition
,   goToDefinition

,   newTextBuffer

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

,   markErrorInSourceBuf
,   inBufContext'
,   inBufContext
,   inActiveBufContext'
,   inActiveBufContext

,   align
,   startComplete
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.General.Enums	(Click(..))
import Graphics.UI.Gtk.Gdk.Events (eventClick)
import Control.Monad.Reader
--import Data.IORef
import System.IO
import System.FilePath
import System.Directory
import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Typeable
import System.Time

import IDE.Core.State
import IDE.FileUtils
import IDE.SourceCandy
import IDE.Completion as Completion (complete,cancel)
import Debug.Trace (trace)
import qualified System.IO.UTF8 as UTF8
import Data.IORef (writeIORef,readIORef,newIORef,IORef(..))
import Graphics.UI.Frame.Panes (IDEPane(..))
import Data.Char (isAlphaNum)
import Control.Event (triggerEvent)

--
-- | A text editor pane description
--
data IDEBuffer      =   IDEBuffer {
    fileName        ::  Maybe FilePath
,   bufferName      ::  String
,   addedIndex      ::  Int
,   sourceView      ::  SourceView
,   scrolledWindow  ::  ScrolledWindow
,   modTime         ::  IORef (Maybe (ClockTime))
,   language        ::  Maybe String
} deriving (Typeable)

data BufferState            =   BufferState FilePath Int
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
      (cids) <- reifyIDE $ \ideR   -> do
          gtkBuf  <- textViewGetBuffer sv
          bringPaneToFront actbuf
          writeCursorPositionInStatusbar sv sbLC
          writeOverwriteInStatusbar sv sbIO
          id1 <- gtkBuf `afterModifiedChanged` reflectIDE (markActiveLabelAsChanged) ideR
          id2 <- sv `afterMoveCursor`
              (\_ _ _ -> writeCursorPositionInStatusbar sv sbLC)
          id3 <- gtkBuf `afterEndUserAction`  writeCursorPositionInStatusbar sv sbLC
          sv `widgetAddEvents` [ButtonReleaseMask]
          id5 <- sv `onButtonRelease`
            (\ e -> do
              writeCursorPositionInStatusbar sv sbLC
              when (controlIsPressed e) $ showInfo sv ideR
              return False)
          id6 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv sbIO
          return [ConnectC id2,ConnectC id6,ConnectC id1,ConnectC id3]
      activatePane actbuf cids
      liftIO $
        idleAdd (do
            widgetQueueDraw sv -- Patch for problem on one machine ##
            return False) priorityDefaultIdle
      triggerEvent ideR (Sensitivity [(SensitivityEditor, True)])
      checkModTime actbuf
      return ()
    close pane = do makeActive pane
                    fileClose
                    return ()

instance RecoverablePane IDEBuffer BufferState IDEM where
    saveState p     =   do  buf     <-  liftIO $ textViewGetBuffer (sourceView p)
                            ins     <-  liftIO $ textBufferGetInsert buf
                            iter    <-  liftIO $ textBufferGetIterAtMark buf ins
                            offset  <-  liftIO $ textIterGetOffset iter
                            case fileName p of
                                Nothing ->  return Nothing
                                Just fn ->  return (Just (BufferState fn offset))
    recoverState pp (BufferState n i) =   do
        exists <- liftIO $doesFileExist n
        when exists $ do
            buf     <-  newTextBuffer pp (takeFileName n) (Just n)
            liftIO $ do
                gtkBuf  <-  textViewGetBuffer (sourceView buf)
                iter    <-  textBufferGetIterAtOffset gtkBuf i
                textBufferPlaceCursor gtkBuf iter
                mark    <-  textBufferGetInsert gtkBuf
                idleAdd  (do
                    textViewScrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))
                    return False) priorityDefaultIdle
                return ()

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
                    path <- standardSourcePanePath
                    nbuf <- newTextBuffer path (takeFileName fpc) (Just fpc)
                    return (Just nbuf)
                else return Nothing

lastActiveBufferPane :: IDEM (Maybe PaneName)
lastActiveBufferPane = do
    recentPanes' <- readIDE recentPanes
    mbBufs       <- mapM mbPaneFromName recentPanes'
    case (catMaybes $ map (\ (PaneC p) -> cast p) $ catMaybes mbBufs) :: [IDEBuffer] of
        (hd : _) -> return (Just (paneName hd))
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
        inActiveBufContext () $ \_ gtkbuf buf _ -> do
            let location    =   fromJust mbLocation
            lines           <-  textBufferGetLineCount gtkbuf
            iter            <-  textBufferGetIterAtLine gtkbuf (max 0 (min (lines-1)
                                    ((locationSLine location) -1)))
            chars           <-  textIterGetCharsInLine iter
            textIterSetLineOffset iter (max 0 (min (chars-1) (locationSCol location)))
            iter2           <-  textBufferGetIterAtLine gtkbuf (max 0 (min (lines-1)
                                    ((locationELine location) -1)))
            chars2          <-  textIterGetCharsInLine iter2
            textIterSetLineOffset iter2 (max 0 (min (chars2-1) (locationECol location)))
            textBufferPlaceCursor gtkbuf iter
            smark           <-  textBufferGetSelectionBound gtkbuf
            textBufferMoveMark gtkbuf smark iter2
            -- ### we had a problem before using this idleAdd thing
            idleAdd  (do
                textViewScrollToIter (sourceView buf) iter 0.0 (Just (0.3,0.3))
                return False) priorityDefaultIdle
            return ()


markErrorInSourceBuf :: Int -> IDEBuffer -> Int -> Int -> String -> Bool -> IDEAction
markErrorInSourceBuf index buf line column string scrollTo =
    inBufContext () buf $ \_ gtkbuf buf _ -> do
        tagTable <- textBufferGetTagTable gtkbuf
        mbTag <- textTagTableLookup tagTable ("Err" ++ show index)
        case mbTag of
            Just tag -> do
                i1 <- textBufferGetStartIter gtkbuf
                i2 <- textBufferGetEndIter gtkbuf
                textBufferRemoveTagByName gtkbuf ("Err" ++ show index) i1 i2
            Nothing -> do
                errtag <- textTagNew (Just $ "Err" ++ show index)
                set errtag[textTagUnderline := UnderlineError]
                textTagTableAdd tagTable errtag

        lines   <-  textBufferGetLineCount gtkbuf
        iter    <-  textBufferGetIterAtLine gtkbuf (max 0 (min (lines-1) (line-1)))
        chars   <-  textIterGetCharsInLine iter
        textIterSetLineOffset iter (max 0 (min (chars-1) column))
        iter2 <- textIterCopy iter
        textIterForwardWordEnd iter2
        textBufferApplyTagByName gtkbuf ("Err" ++ show index) iter iter2
        when scrollTo $ textBufferPlaceCursor gtkbuf iter
        mark <- textBufferGetInsert gtkbuf
        when scrollTo $ do
            idleAdd (do
                textViewScrollToMark (sourceView buf) mark 0.3 Nothing
                return False) priorityDefaultIdle
            return ()

allBuffers :: IDEM [IDEBuffer]
allBuffers = getPanes

maybeActiveBuf :: IDEM (Maybe IDEBuffer)
maybeActiveBuf = do
    mbActivePane <-  readIDE activePane
    mbPane       <- lastActiveBufferPane
    case (mbPane,mbActivePane) of
        (Just paneName1, Just (paneName2,_)) | paneName1 == paneName2 -> do
            (PaneC pane) <- paneFromName paneName1
            let mbActbuf = cast pane
            return mbActbuf
        _ -> return Nothing

standardSourcePanePath :: IDEM PanePath
standardSourcePanePath = do
    layout  <-  readIDE layout
    prefs   <-  readIDE prefs
    return (getStandardPanePath (sourcePanePath prefs) layout)

newTextBuffer :: PanePath -> String -> Maybe FilePath -> IDEM IDEBuffer
newTextBuffer panePath bn mbfn = do
    -- create the appropriate language
    nb      <-  getNotebook panePath
    panes   <-  readIDE panes
    paneMap <-  readIDE paneMap
    prefs   <-  readIDE prefs
    bs      <-  getCandyState
    ct      <-  readIDE candy
    let (ind,rbn) = figureOutPaneName panes bn 0
    (buf,cids)   <- reifyIDE $ \ideR   -> do
        lm       <- sourceLanguageManagerNew
        (mbLanguage, mbSLang)  <- sourceLanguageForFilename lm mbfn

        -- create a new SourceBuffer object
        buffer   <- case mbSLang of
                        Just sLang -> sourceBufferNewWithLanguage sLang
                        Nothing -> sourceBufferNew Nothing
        sourceBufferSetMaxUndoLevels buffer (-1)
        tagTable <- textBufferGetTagTable buffer
        foundTag <- textTagNew (Just "found")
        set foundTag [textTagBackground := "yellow"]
        textTagTableAdd tagTable foundTag

        -- load up and display a file
        (fileContents,modTime) <- case mbfn of
            Just fn -> do
                fc <- UTF8.readFile fn
                mt <- getModificationTime fn
                return (fc,Just mt)
            Nothing -> return ("\n\n\n\n\n",Nothing)
        sourceBufferBeginNotUndoableAction buffer
        textBufferSetText buffer fileContents
        when bs $ transformToCandy ct (castToTextBuffer buffer)
        sourceBufferEndNotUndoableAction buffer
        textBufferSetModified buffer False
        siter <- textBufferGetStartIter buffer
        textBufferPlaceCursor buffer siter
        iter <- textBufferGetEndIter buffer
        textBufferCreateMark buffer (Just "end") iter True

        -- create a new SourceView Widget
        sv <- sourceViewNewWithBuffer buffer
        fd <- case textviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing -> do
                f <- fontDescriptionNew
                fontDescriptionSetFamily f "Monospace"
                return f
        widgetModifyFont sv (Just fd)
        set sv [sourceViewHighlightCurrentLine := True]
        sourceViewSetShowLineNumbers sv (showLineNumbers prefs)
        case rightMargin prefs of
            Just n -> do
                set sv [sourceViewShowRightMargin := True]
                sourceViewSetRightMarginPosition sv (fromIntegral n)
            Nothing -> set sv [sourceViewShowRightMargin := False]
        sourceViewSetInsertSpacesInsteadOfTabs sv True
        sourceViewSetIndentWidth sv (tabWidth prefs)
        sourceViewSetTabWidth sv (tabWidth prefs)
        sourceViewSetIndentOnTab sv True
        sourceViewSetAutoIndent sv True
        sourceViewSetSmartHomeEnd sv SourceSmartHomeEndBefore
        case sourceStyle prefs of
            Nothing  -> return ()
            Just str -> do
                styleManager <- sourceStyleSchemeManagerNew
                ids <- sourceStyleSchemeManagerGetSchemeIds styleManager
                when (elem str ids) $ do
                    scheme <- sourceStyleSchemeManagerGetScheme styleManager str
                    sourceBufferSetStyleScheme buffer scheme

        -- put it in a scrolled window
        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw sv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn
        modTimeRef <- newIORef modTime
        let buf = IDEBuffer mbfn bn ind sv sw modTimeRef mbLanguage
        notebookInsertOrdered nb sw rbn Nothing
        -- events
        cid <- sv `afterFocusIn`
            (\_ -> do reflectIDE (makeActive buf) ideR  ; return False)
        buffer `afterBufferInsertText`
            (\iter text -> do
                case text of
                    [c] | ((isAlphaNum c) || (c == '.') || (c == '_')) -> do
                        reflectIDE (Completion.complete sv False) ideR
                    _ -> return ()
            )
        sv `onMoveCursor`
            (\step n select -> do reflectIDE Completion.cancel ideR)
        sv `onButtonPress`
            \event -> do
                let click = eventClick event
                liftIO $ do
                    reflectIDE Completion.cancel ideR
                    case click of
                        DoubleClick -> do
                            let isSelectChar a = (isAlphaNum a) || (a == '_')
                            (start, end) <- textBufferGetSelectionBounds buffer
                            mbStartChar <- textIterGetChar start
                            mbEndChar <- textIterGetChar end
                            case mbStartChar of
                                Just startChar | isSelectChar startChar -> do
                                    found <- textIterBackwardFindChar start (not.isSelectChar) Nothing
                                    when found $ do
                                        textIterForwardChar start
                                        return ()
                                _ -> return ()
                            case mbEndChar of
                                Just endChar | isSelectChar endChar -> do
                                    textIterForwardFindChar end (not.isSelectChar) Nothing
                                    return ()
                                _ -> return ()
                            textBufferSelectRange buffer start end
                            return True
                        _ -> return False
        return (buf,[cid])
    addPaneAdmin buf (map ConnectC cids) panePath
    liftIO $do
        widgetShowAll (scrolledWindow buf)
        widgetGrabFocus (sourceView buf)
    when (isJust mbfn) $ removeRecentlyUsedFile (fromJust mbfn)
    return buf

checkModTime :: IDEBuffer -> IDEM Bool
checkModTime buf = do
    currentState' <- readIDE currentState
    case  currentState' of
        IsShuttingDown -> return False
        _              -> do
            panes <- readIDE panes
            let name = paneName buf
            case fileName buf of
                Just fn -> do
                    exists <- liftIO $doesFileExist fn
                    if exists
                        then do
                            nmt <- liftIO $ getModificationTime fn
                            modTime' <- liftIO $ readIORef (modTime buf)
                            case modTime' of
                                Nothing ->  throwIDE $"checkModTime: time not set " ++ show (fileName buf)
                                Just mt -> do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                                    -- For some reason evaluating nmt /= mt causes corrupt fonts ?!?
                                    return False
#else
                                    --message $"checkModTime " ++ name ++ " " ++ show mt ++ " " ++ show nmt
                                    if nmt /= mt
                                        then do
                                            md <- liftIO $ messageDialogNew
                                                    Nothing []
                                                    MessageQuestion
                                                    ButtonsYesNo
                                                    ("File has changed on disk " ++ name ++ " Revert?")
                                            resp <- liftIO $ dialogRun md
                                            case resp of
                                                ResponseYes ->  do
                                                    revert buf
                                                    liftIO $ widgetHide md
                                                    return False
                                                ResponseNo  -> liftIO $ do
                                                    writeIORef (modTime buf) (Just nmt)
                                                    widgetHide md
                                                    return True
                                                _           ->  do return False
                                        else return False
#endif
                        else return False
                Nothing -> return False

setModTime :: IDEBuffer -> IDEAction
setModTime buf = do
    panes <- readIDE panes
    let name = paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> liftIO $ do
            nmt <- getModificationTime fn
            writeIORef (modTime buf) (Just nmt)

fileRevert :: IDEAction
fileRevert = inActiveBufContext' () $ \ _ _ currentBuffer _ -> do
    revert currentBuffer

revert :: IDEBuffer -> IDEAction
revert buf = do
    useCandy    <-  getCandyState
    ct          <-  readIDE candy
    panes       <-  readIDE panes
    let name    =   paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> liftIO $do
            buffer' <- textViewGetBuffer (sourceView buf)
            let buffer = castToSourceBuffer buffer'
            fc <- UTF8.readFile fn
            mt <- getModificationTime fn
            sourceBufferBeginNotUndoableAction buffer
            textBufferSetText buffer fc
            if useCandy
                then transformToCandy ct (castToTextBuffer buffer)
                else return ()
            sourceBufferEndNotUndoableAction buffer
            textBufferSetModified buffer False
            return mt
            writeIORef (modTime buf) (Just mt)

writeCursorPositionInStatusbar :: SourceView -> Statusbar -> IO()
writeCursorPositionInStatusbar sv sb = do
    buf  <- textViewGetBuffer sv
    mark <- textBufferGetInsert buf
    iter <- textBufferGetIterAtMark buf mark
    line <- textIterGetLine iter
    col  <- textIterGetLineOffset iter
    statusbarPop sb 1
    statusbarPush sb 1 $printf "Ln %4d, Col %3d" (line + 1) (col + 1)
    return ()

writeOverwriteInStatusbar :: SourceView -> Statusbar -> IO()
writeOverwriteInStatusbar sv sb = do
    modi <- textViewGetOverwrite sv
    statusbarPop sb 1
    statusbarPush sb 1 $ if modi then "OVR" else "INS"
    return ()


showInfo :: SourceView -> IDERef -> IO ()
showInfo sv ideR = do
    buf     <-  textViewGetBuffer sv
    (l,r)   <- textBufferGetSelectionBounds buf
    symbol  <- textBufferGetText buf l r True
    reflectIDE (triggerEvent ideR (SelectInfo symbol)) ideR
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
              Just buf -> liftIO $ markLabelAsChanged nb buf

markLabelAsChanged :: Notebook -> IDEBuffer -> IO ()
markLabelAsChanged nb buf = do
                  gtkbuf   <- textViewGetBuffer (sourceView buf)
                  modified <- textBufferGetModified gtkbuf
                  mbText   <- notebookGetTabLabelText nb (scrolledWindow buf)
                  label    <- labelNew Nothing
                  labelSetUseMarkup label True
                  case mbText of
                    Nothing   -> return ()
                    Just text -> labelSetMarkup label
                                    (if modified
                                          then "<span foreground=\"red\">" ++ text ++ "</span>"
                                          else text)
                  notebookSetTabLabel nb (scrolledWindow buf) label

inBufContext' :: alpha -> IDEBuffer -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IDEM alpha) -> IDEM alpha
inBufContext' def ideBuf f = do
    (pane,_)       <-  guiPropertiesFromName (paneName ideBuf)
    nb             <-  getNotebook pane
    mbI            <-  liftIO $notebookPageNum nb (scrolledWindow ideBuf)
    case mbI of
        Nothing ->  liftIO $ do
            sysMessage Normal $ bufferName ideBuf ++ " notebook page not found: unexpected"
            return def
        Just i  ->  do
            gtkbuf <- liftIO $ textViewGetBuffer (sourceView ideBuf)
            f nb gtkbuf ideBuf i

inBufContext :: alpha -> IDEBuffer -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IO alpha) -> IDEM alpha
inBufContext def ideBuff f = inBufContext' def ideBuff (\ a b c d -> liftIO $ f a b c d)

inActiveBufContext' :: alpha -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IDEM alpha) -> IDEM alpha
inActiveBufContext' def f = do
    mbBuf                  <- maybeActiveBuf
    case mbBuf of
        Nothing         -> return def
        Just ideBuf -> do
            inBufContext' def ideBuf f

inActiveBufContext :: alpha -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IO alpha) -> IDEM alpha
inActiveBufContext def f = inActiveBufContext' def (\ a b c d -> liftIO $ f a b c d)

fileSaveBuffer :: Bool -> Notebook -> TextBuffer -> IDEBuffer -> Int -> IDEM Bool
fileSaveBuffer query nb gtkbuf ideBuf i = do
    ideR    <- ask
    window  <- readIDE window
    bufs    <- readIDE panes
    prefs   <- readIDE prefs
    paneMap <- readIDE paneMap
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
                        modifiedInBuffer <- liftIO $ textBufferGetModified gtkbuf
                        if (modifiedOnDisk || modifiedInBuffer)
                            then do
                                liftIO $fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf bs candy $fromJust mbfn
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
                                    fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf bs candy cfn

                                    reflectIDE (do
                                        close ideBuf
                                        newTextBuffer panePath (takeFileName fn) (Just cfn)
                                        )   ideR
                                    return True
                                ResponseNo -> return False
                                _          -> return False
    where
        fileSave' :: Bool -> Bool -> Notebook -> IDEBuffer -> Bool -> CandyTable -> FilePath -> IO()
        fileSave' forceLineEnds removeTBlanks nb ideBuf bs ct fn = do
            buf     <-   textViewGetBuffer $ sourceView ideBuf
            text    <-   getCandylessText ct buf
            let text' = if removeTBlanks
                            then unlines $map removeTrailingBlanks $lines text
                            else text
            succ <- catch (do UTF8.writeFile fn text'; return True)
                (\e -> do
                    sysMessage Normal (show e)
                    return False)
            textBufferSetModified buf (not succ)
            markLabelAsChanged nb ideBuf
        removeTrailingBlanks :: String -> String
        removeTrailingBlanks = reverse . dropWhile (\c -> c == ' ') . reverse

fileSave :: Bool -> IDEM Bool
fileSave query = inActiveBufContext' False $ fileSaveBuffer query

fileSaveAll :: IDEM Bool
fileSaveAll = do
    bufs    <- allBuffers
    results <- forM bufs (\buf -> inBufContext' False buf (fileSaveBuffer False))
    return $ True `elem` results

fileCheckBuffer :: Bool -> Notebook -> TextBuffer -> IDEBuffer -> Int -> IDEM Bool
fileCheckBuffer query nb gtkbuf ideBuf i = do
    ideR    <- ask
    window  <- readIDE window
    bufs    <- readIDE panes
    prefs   <- readIDE prefs
    paneMap <- readIDE paneMap
    bs      <- getCandyState
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    mbpage <- liftIO $ notebookGetNthPage nb i
    case mbpage of
        Nothing     -> throwIDE "fileCheck: Page not found"
        Just page   ->
            if isJust mbfn && query == False
                then do modifiedOnDisk <- checkModTime ideBuf -- The user is given option to reload
                        modifiedInBuffer <- liftIO $ textBufferGetModified gtkbuf
                        return (modifiedOnDisk || modifiedInBuffer)
                else return False

fileCheckAll :: IDEM Bool
fileCheckAll = do
    bufs    <- allBuffers
    results <- forM bufs (\buf -> inBufContext' False buf (fileCheckBuffer False))
    return $ True `elem` results

fileNew :: IDEAction
fileNew = do
    prefs   <- readIDE prefs
    pp      <- getActivePanePathOrStandard (sourcePanePath prefs)
    newTextBuffer pp "Unnamed" Nothing
    return ()

fileClose :: IDEM Bool
fileClose = inActiveBufContext' True $ fileClose'

fileClose' :: Notebook -> TextBuffer -> IDEBuffer -> Int  -> IDEM Bool
fileClose' nb gtkbuf currentBuffer i = do
    window  <- readIDE window
    bufs    <- readIDE panes
    paneMap <- readIDE paneMap
    cancel <- reifyIDE $ \ideR   ->  do
        modified <- textBufferGetModified gtkbuf
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


fileCloseAll :: IDEM Bool
fileCloseAll = do
    bufs    <- allBuffers
    if null bufs
        then return True
        else do
            makeActive (head bufs)
            r <- fileClose
            if r
                then fileCloseAll
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
                    gtkbuf <- liftIO $ textViewGetBuffer (sourceView buf)
                    let dir = dropFileName $ cabalFile activePack
                    when (isJust (fileName buf)) $ do
                        modified <- liftIO $ textBufferGetModified gtkbuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb gtkbuf buf i; return ()

fileOpen :: IDEAction
fileOpen = do
    window <- readIDE window
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
            pp <-  standardSourcePanePath
            newTextBuffer pp (takeFileName fpc) (Just fpc)
            return ()

editUndo :: IDEAction
editUndo = inActiveBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canUndo <- sourceBufferGetCanUndo sb
        if canUndo
            then sourceBufferUndo sb
            else return ()

editRedo :: IDEAction
editRedo = inActiveBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canRedo <- sourceBufferGetCanRedo sb
        if canRedo
            then sourceBufferRedo sb
            else return ()

editDelete :: IDEAction
editDelete = inActiveBufContext ()  $ \_ gtkbuf _ _ ->  do
    textBufferDeleteSelection gtkbuf True True
    return ()

editSelectAll :: IDEAction
editSelectAll = inActiveBufContext () $ \_ gtkbuf _ _ -> do
    start <- textBufferGetStartIter gtkbuf
    end   <- textBufferGetEndIter gtkbuf
    textBufferSelectRange gtkbuf start end

editCut :: IDEAction
editCut = inActiveBufContext () $ \_ gtkbuf _ _ -> do
    cb   <- atomNew "GDK_SELECTION_CLIPBOARD"
    clip <- clipboardGet cb
    textBufferCutClipboard gtkbuf clip True

editCopy :: IDEAction
editCopy = inActiveBufContext () $ \_ gtkbuf _ _ -> do
    cb   <- atomNew "GDK_SELECTION_CLIPBOARD"
    clip <- clipboardGet cb
    textBufferCopyClipboard gtkbuf clip

editPaste :: IDEAction
editPaste = inActiveBufContext () $ \_ gtkbuf _ _ -> do
    cb   <- atomNew "GDK_SELECTION_CLIPBOARD"
    mark <- textBufferGetInsert gtkbuf
    iter <- textBufferGetIterAtMark gtkbuf mark
    clip <- clipboardGet cb
    textBufferPasteClipboard gtkbuf clip iter True

getStartAndEndLineOfSelection :: TextBuffer -> IO (Int,Int)
getStartAndEndLineOfSelection gtkbuf = do
    startMark   <- textBufferGetInsert gtkbuf
    endMark     <- textBufferGetSelectionBound gtkbuf
    startIter   <- textBufferGetIterAtMark gtkbuf startMark
    endIter     <- textBufferGetIterAtMark gtkbuf endMark
    startLine   <- textIterGetLine startIter
    endLine     <- textIterGetLine endIter
    let (startLine',endLine',endIter') = if endLine >=  startLine
            then (startLine,endLine,endIter)
            else (endLine,startLine,startIter)
    b           <- textIterStartsLine endIter'
    let endLineReal = if b then endLine' - 1 else endLine'
    return (startLine',endLineReal)

doForSelectedLines :: [a] -> (TextBuffer -> TextIter -> Int -> IO a) -> IDEM [a]
doForSelectedLines d f = inActiveBufContext' d $ \_ gtkbuf currentBuffer _ -> liftIO $do
    (start,end) <- getStartAndEndLineOfSelection gtkbuf
    iter        <- textBufferGetStartIter gtkbuf
    mapM (f gtkbuf iter) [start .. end]

editComment :: IDEAction
editComment = do
    doForSelectedLines [] $ \gtkbuf iter lineNr -> do
        textIterSetLine iter lineNr
        textBufferInsert gtkbuf iter "--"
    return ()

editUncomment :: IDEAction
editUncomment = do
    doForSelectedLines [] $ \gtkbuf iter lineNr -> do
        textIterSetLine iter lineNr
        iter2 <- textIterCopy iter
        textIterForwardChars iter 2
        str   <- textIterGetText iter iter2
        if str == "--"
            then do textBufferDelete gtkbuf iter iter2
            else return ()
    return ()

editShiftLeft :: IDEAction
editShiftLeft = do
    prefs <- readIDE prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
    b <- canShiftLeft str prefs
    if b
        then do
            doForSelectedLines [] $ \gtkbuf iter lineNr -> do
                textIterSetLine iter lineNr
                iter2 <- textIterCopy iter
                textIterForwardChars iter (tabWidth prefs)
                textBufferDelete gtkbuf iter iter2
            return ()
        else return ()
    where
    canShiftLeft str prefs = do
        boolList <- doForSelectedLines [] $ \gtkbuf iter lineNr -> do
            textIterSetLine iter lineNr
            iter2 <- textIterCopy iter
            textIterForwardChars iter (tabWidth prefs)
            str1 <- textIterGetText iter iter2
            return (str1 == str)
        return (foldl' (&&) True boolList)


editShiftRight :: IDEAction
editShiftRight = do
    prefs <- readIDE prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
    doForSelectedLines [] $ \gtkbuf iter lineNr -> do
        textIterSetLine iter lineNr
        textBufferInsert gtkbuf iter str
    return ()

editToCandy :: IDEAction
editToCandy = do
    ct <- readIDE candy
    inActiveBufContext () $ \_ gtkbuf _ _ -> do
        transformToCandy ct gtkbuf

editFromCandy :: IDEAction
editFromCandy = do
    ct      <-  readIDE candy
    inActiveBufContext () $ \_ gtkbuf _ _ -> do
        transformFromCandy ct gtkbuf

editKeystrokeCandy :: Maybe Char -> IDEAction
editKeystrokeCandy c = do
    ct <- readIDE candy
    inActiveBufContext () $ \_ gtkbuf _ _ -> do
        keystrokeCandy ct c gtkbuf

editCandy :: IDEAction
editCandy = do
    ct      <- readIDE candy
    buffers <- allBuffers
    gtkbufs <- liftIO $mapM (\ b -> textViewGetBuffer (sourceView b)) buffers
    bs <- getCandyState
    if bs
        then liftIO $mapM_ (transformToCandy ct) gtkbufs
        else liftIO $mapM_ (transformFromCandy ct) gtkbufs

alignChar :: Char -> IDEAction
alignChar char = do
    positions     <- positionsOfChar
    let alignTo = foldl' max 0 (catMaybes (map snd positions))
    if (alignTo > 0)
        then alignChar (Map.fromList positions) alignTo
        else return ()
    where
    positionsOfChar :: IDEM ([(Int, Maybe Int)])
    positionsOfChar = doForSelectedLines [] $ \gtkbuf iter lineNr -> do
            textIterSetLine iter lineNr
            iter2 <- textIterCopy iter
            textIterForwardToLineEnd iter2
            line  <- textIterGetText iter iter2
            return (lineNr, elemIndex char line)
    alignChar :: Map Int (Maybe Int) -> Int -> IDEM ()
    alignChar positions alignTo = do
            doForSelectedLines [] $ \gtkbuf iter lineNr -> do
                case lineNr `Map.lookup` positions of
                    Just (Just n)  ->  do
                        textIterSetLine iter lineNr
                        textIterForwardChars iter n
                        textBufferInsert gtkbuf iter (replicate (alignTo - n) ' ')
                    _              ->  return ()
            return ()

transChar :: Char -> Char
transChar ':' = toEnum 0x2237 --PROPORTION
transChar '>' = toEnum 0x2192 --RIGHTWARDS ARROW
transChar '<' = toEnum (toEnum 0x2190) --LEFTWARDS ARROW
transChar c   = c

align :: Char -> IDEAction
align = alignChar . transChar

sourceLanguageForFilename :: SourceLanguageManager -> Maybe String -> IO (Maybe String, Maybe SourceLanguage)
sourceLanguageForFilename lm Nothing         = do
    mbLang <- sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
    case mbLang of
        Nothing -> return (Nothing,Nothing)
        Just lang -> do
            name <- sourceLanguageGetName lang
            return (Just name, Just lang)

sourceLanguageForFilename lm (Just filename) = do
    mbLang <- sourceLanguageManagerGuessLanguage lm (Just filename) Nothing
    case mbLang of
        Nothing -> return (Nothing,Nothing)
        Just lang -> do
            name <- sourceLanguageGetName lang
            return (Just name, Just lang)

addRecentlyUsedFile :: FilePath -> IDEAction
addRecentlyUsedFile fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        unless (elem fp recentFiles') $
            modifyIDE_ (\ide -> return ide{recentFiles = take 12 (fp : recentFiles')})
        ask >>= \ideR -> triggerEvent ideR UpdateRecent
        return ()

removeRecentlyUsedFile :: FilePath -> IDEAction
removeRecentlyUsedFile fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        when (elem fp recentFiles') $
            modifyIDE_ (\ide -> return ide{recentFiles = filter (\e -> e /= fp) recentFiles'})
        ask >>= \ideR -> triggerEvent ideR UpdateRecent
        return ()



