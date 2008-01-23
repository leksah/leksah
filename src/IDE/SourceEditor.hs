{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SourceEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The source editor part of Leksah
--
-----------------------------------------------------------------------------------

module IDE.SourceEditor (
    allBuffers
,   maybeActiveBuf
,   standardSourcePanePath
,   selectSourceBuf
,   goToSourceDefinition
,   goToDefinition

,   newTextBuffer

,   fileNew
,   fileOpen
,   fileRevert
,   fileClose
,   fileCloseAll
,   fileCloseAllButPackage
,   fileSave
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
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.IORef
import System.IO
import System.FilePath
import System.Directory
import Text.Printf
import qualified Data.Map as Map
import Data.List
import Data.Maybe

import IDE.Core.State
import IDE.Utils.File
import IDE.Framework.ViewFrame
import IDE.SourceCandy
import IDE.Metainfo.Info
import {-# SOURCE #-} IDE.InfoPane

instance IDEObject IDEBuffer
instance Pane IDEBuffer
    where
    primPaneName    =   bufferName
    getAddedIndex   =   addedIndex
    getTopWidget    =   castToWidget . scrolledWindow
    paneId b        =   case fileName b of
                            Just s  -> s
                            Nothing -> "?" ++ bufferName b
    makeActive buf = do
        pane    <-  paneFromName (paneName buf)
        if isIt BufferCasting pane
            then do
              let actbuf =   fromJust $ downCast BufferCasting pane
              ideR    <-  ask
              sbLC    <-  getStatusbarLC
              sbIO    <-  getStatusbarIO
              infos   <-  readIDE accessibleInfo
              let sv = sourceView actbuf
              (tl,tm,tr) <- lift $do
                  gtkBuf  <- textViewGetBuffer sv
                  bringPaneToFront actbuf
                  writeCursorPositionInStatusbar sv sbLC
                  writeOverwriteInStatusbar sv sbIO
                  id1 <- gtkBuf `afterModifiedChanged` runReaderT (markLabelAsChanged) ideR
                  id2 <- sv `afterMoveCursor`
                      (\_ _ _ -> writeCursorPositionInStatusbar sv sbLC)
                  id3 <- gtkBuf `afterEndUserAction`  writeCursorPositionInStatusbar sv sbLC
                  sv `widgetAddEvents` [ButtonReleaseMask]
                  id4 <- sv `onButtonRelease` (\ _ -> do
                                                  writeCursorPositionInStatusbar sv sbLC
                                                  return False)
                  id5 <- sv `onButtonRelease` (\ e -> do
                                                  showInfo sv ideR
                                                  return False)
                  id6 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv sbIO
                  return ([id2,id4,id6],[id1,id3],[])
              activatePane actbuf (BufConnections tl tm tr)
              checkModTime actbuf
            else return ()
    close pane = do makeActive pane
                    fileClose
                    return ()

instance RecoverablePane IDEBuffer BufferState where
    saveState p     =   do  buf     <-  lift $ textViewGetBuffer (sourceView p)
                            ins     <-  lift $ textBufferGetInsert buf
                            iter    <-  lift $ textBufferGetIterAtMark buf ins
                            offset  <-  lift $ textIterGetOffset iter
                            case fileName p of
                                Nothing ->  return Nothing
                                Just fn ->  return (Just (StateC (BufferState fn offset)))
    recoverState pp (BufferState n i) =   do
        exists <- lift $doesFileExist n
        when exists $ do
            buf     <-  newTextBuffer pp (takeFileName n) (Just n)
            lift $ do
                gtkBuf  <-  textViewGetBuffer (sourceView buf)
                iter    <-  textBufferGetIterAtOffset gtkBuf i
                textBufferPlaceCursor gtkBuf iter
                mark    <-  textBufferGetInsert gtkBuf
                textViewScrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))
                return ()

selectSourceBuf :: FilePath -> IDEM Bool
selectSourceBuf fp = do
    fpc <-  lift $canonicalizePath fp
    buffers <- allBuffers
    let buf = filter (\b -> case fileName b of
                                Just fn -> equalFilePath fn fpc
                                Nothing -> False) buffers
    case buf of
        hdb:tl -> do
            makeActive hdb
            return True
        otherwise -> do
            fe <- lift $doesFileExist fpc
            if fe
                then do
                    path <- standardSourcePanePath
                    newTextBuffer path (takeFileName fpc) (Just fpc)
                    return True
                else return False

goToDefinition :: IdentifierDescr -> IDEAction
goToDefinition idDescr = do
    mbAccesibleInfo      <-  readIDE accessibleInfo
    mbCurrentInfo        <-  readIDE currentInfo
    if isJust mbAccesibleInfo && isJust mbCurrentInfo
        then do
            let packageId       =   pack $ moduleIdID idDescr
            let mbPack          =   case packageId `Map.lookup` fst
                                            (fromJust mbAccesibleInfo) of
                                        Just it ->  Just it
                                        Nothing ->  packageId `Map.lookup` fst (fst
                                                                 (fromJust mbCurrentInfo))
            case mbPack of
                Just pack       ->  case filter (\md -> moduleIdMD md == moduleIdID idDescr)
                                                    (exposedModulesPD pack) of
                                        [mod]   ->  if isJust (mbSourcePathMD mod)
                                                        then goToSourceDefinition
                                                                (fromJust $ mbSourcePathMD mod)
                                                                (mbLocation idDescr)
                                                        else return ()
                                        _       ->  ideMessage Normal "not just one module"
                Nothing         ->  do ideMessage Normal "no package"
        else ideMessage Normal  "no infos"

goToSourceDefinition :: FilePath -> Maybe Location -> IDEAction
goToSourceDefinition fp mbLocation = do
    success     <- selectSourceBuf fp
    when (success && isJust mbLocation) $
        inBufContext () $ \_ gtkbuf buf _ -> do
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

        imark           <-  textBufferGetInsert gtkbuf
        -- ### we have a problem here that this does not work for a frsh opened buffer
        textViewScrollToMark (sourceView buf) imark 0.0 (Just (0.3,0.3))


markErrorInSourceBuf ::  Int -> Int -> String -> IDEAction
markErrorInSourceBuf line column string =
    inBufContext () $ \_ gtkbuf buf _ -> do
        i1 <- textBufferGetStartIter gtkbuf
        i2 <- textBufferGetEndIter gtkbuf
        textBufferRemoveTagByName gtkbuf "activeErr" i1 i2

        lines   <-  textBufferGetLineCount gtkbuf
        iter    <-  textBufferGetIterAtLine gtkbuf (max 0 (min (lines-1) (line-1)))
        chars   <-  textIterGetCharsInLine iter
        textIterSetLineOffset iter (max 0 (min (chars-1) column))
        iter2 <- textIterCopy iter
        textIterForwardWordEnd iter2
        textBufferApplyTagByName gtkbuf "activeErr" iter iter2
        textBufferPlaceCursor gtkbuf iter
        mark <- textBufferGetInsert gtkbuf
        textViewScrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))


allBuffers :: IDEM [IDEBuffer]
allBuffers = do
    panesST <- readIDE panes
    return (catMaybes $ map (downCast BufferCasting) $ Map.elems panesST)

maybeActiveBuf :: IDEM (Maybe (IDEBuffer,Connections))
maybeActiveBuf = do
    mbPane   <- readIDE activePane
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,signals) -> do
            pane <- paneFromName paneName
            if isIt BufferCasting pane
                then return (Just (fromJust $ downCast BufferCasting pane,signals))
                else return Nothing

standardSourcePanePath :: IDEM PanePath
standardSourcePanePath = do
    layout  <-  readIDE layout
    prefs   <-  readIDE prefs
    return (getStandardPanePath (sourcePanePath prefs) layout)

newTextBuffer :: PanePath -> String -> Maybe FilePath -> IDEM IDEBuffer
newTextBuffer panePath bn mbfn = do
    -- create the appropriate language
    ideR    <-  ask
    nb      <-  getNotebook panePath
    panes   <-  readIDE panes
    paneMap <-  readIDE paneMap
    prefs   <-  readIDE prefs
    bs      <-  getCandyState
    ct      <-  readIDE candy
    let (ind,rbn) = figureOutPaneName panes bn 0
    (buf,cids) <- lift $ do
        lm      <-  sourceLanguagesManagerNew
        langM   <-  sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
        lang    <-  case langM of
                        (Just lang) -> return lang
                        Nothing -> do
                            langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
                            throwIDE ("please copy haskell.lang to one of the following"
                                  ++ "directories:\n"
                                ++ unlines langDirs)

        -- create a new SourceBuffer object
        buffer <- sourceBufferNewWithLanguage lang
        tagTable <- textBufferGetTagTable buffer
        foundTag <- textTagNew (Just "found")
        set foundTag [textTagBackground := "yellow"]
        textTagTableAdd tagTable foundTag
        activeErrtag <- textTagNew (Just "activeErr")
        set activeErrtag[textTagUnderline := UnderlineError]
        textTagTableAdd tagTable activeErrtag

        -- load up and display a file
        (fileContents,modTime) <- case mbfn of
            Just fn -> do
                fc <- readFile fn
                mt <- getModificationTime fn
                return (fc,Just mt)
            Nothing -> return ("\n\n\n\n\n",Nothing)
        sourceBufferBeginNotUndoableAction buffer
        textBufferSetText buffer fileContents
        if bs
            then transformToCandy ct (castToTextBuffer buffer)
            else return ()
        sourceBufferEndNotUndoableAction buffer
        textBufferSetModified buffer False
        siter <- textBufferGetStartIter buffer
        textBufferPlaceCursor buffer siter
        sourceBufferSetHighlight buffer True
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
        sourceViewSetShowLineNumbers sv (showLineNumbers prefs)
        case rightMargin prefs of
            Just n -> do
                sourceViewSetMargin sv n
                sourceViewSetShowMargin sv True
            Nothing -> sourceViewSetShowMargin sv True
        sourceViewSetInsertSpacesInsteadOfTabs sv True
        sourceViewSetTabsWidth sv (tabWidth prefs)
        sourceViewSetSmartHomeEnd sv True
        --sourceViewSetShowLineMarkers sv True

        -- put it in a scrolled window
        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw sv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        let buf = IDEBuffer mbfn bn ind sv sw modTime
        notebookInsertOrdered nb sw rbn
        -- events
        cid <- sv `afterFocusIn`
            (\_ -> do runReaderT (makeActive buf) ideR; return True)
        return (buf,[cid])
    addPaneAdmin buf (BufConnections cids [] []) panePath
    lift $widgetShowAll (scrolledWindow buf)
    lift $widgetGrabFocus (sourceView buf)
    return buf

checkModTime :: IDEBuffer -> IDEAction
checkModTime buf = do
    panes <- readIDE panes
    let name = paneName buf
    case fileName buf of
        Just fn -> do
            exists <- lift $doesFileExist fn
            if exists
                then do
                    nmt <- lift $getModificationTime fn
                    case modTime buf of
                        Nothing ->  throwIDE $"checkModTime: time not set " ++ show (fileName buf)
                        Just mt -> do
                            --message $"checkModTime " ++ name ++ " " ++ show mt ++ " " ++ show nmt
                            if nmt /= mt
                                then do
                                    md <- lift $messageDialogNew
                                            Nothing []
                                            MessageQuestion
                                            ButtonsYesNo
                                            ("File has changed on disk " ++ name ++ " Revert?")
                                    resp <- lift $dialogRun md
                                    case resp of
                                        ResponseYes ->  do
                                            revert buf
                                            lift $widgetHide md
                                        ResponseNo  ->  do
                                            let newPanes = Map.adjust (\b ->
                                                    if isIt BufferCasting b
                                                        then PaneC ((fromJust $
                                                                downCast BufferCasting b)
                                                                        {modTime = (Just nmt)})
                                                        else b) name panes
                                            modifyIDE_ (\ide -> return (ide{panes = newPanes}))
                                            lift $widgetHide md
                                        _           ->  do return ()
                                else return ()
                else return ()
        Nothing -> return ()

setModTime :: IDEBuffer -> IDEAction
setModTime buf = do
    panes <- readIDE panes
    let name = paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> do
            nmt <- lift $getModificationTime fn
            let newPanes = Map.adjust (
                                \b -> if isIt BufferCasting b
                                        then
                                            PaneC ((fromJust $ downCast BufferCasting b)
                                                    {modTime = (Just nmt)})
                                        else b) name panes
            modifyIDE_ (\ide -> return (ide{panes = newPanes}))

fileRevert :: IDEAction
fileRevert = inBufContext' () $ \ _ _ currentBuffer _ -> do
    revert currentBuffer

revert :: IDEBuffer -> IDEAction
revert buf = do
    useCandy    <-  getCandyState
    ct          <-  readIDE candy
    panes       <-  readIDE panes
    let name    =   paneName buf
    case fileName buf of
        Nothing -> return ()
        Just fn -> do
            mt <- lift $do
                buffer' <- textViewGetBuffer (sourceView buf)
                let buffer = castToSourceBuffer buffer'
                fc <- readFile fn
                mt <- getModificationTime fn
                sourceBufferBeginNotUndoableAction buffer
                textBufferSetText buffer fc
                if useCandy
                    then transformToCandy ct (castToTextBuffer buffer)
                    else return ()
                sourceBufferEndNotUndoableAction buffer
                textBufferSetModified buffer False
                return mt
            let newPanes = Map.adjust (\b -> if isIt BufferCasting b
                                                then
                                                    PaneC ((fromJust $ downCast BufferCasting b)
                                                                {modTime = (Just mt)})
                                                else b) name panes
            modifyIDE_ (\ide -> return (ide{panes = newPanes}))

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
    buf  <-  textViewGetBuffer sv
    (l,r) <- textBufferGetSelectionBounds buf
    symbol <- textBufferGetText buf l r True
    ide <- readIORef ideR
    case currentInfo ide of
        Nothing -> return ()
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            case getIdentifierDescr symbol symbolTable1 symbolTable2 of
                [] -> return ()
                a -> runReaderT (setInfos a) ideR

markLabelAsChanged :: IDEAction
markLabelAsChanged = do
    mbPath <- getActivePanePath
    case mbPath of
        Nothing -> return ()
        Just path -> do
          nb <- getNotebook path
          mbBS <- maybeActiveBuf
          case mbBS of
              Nothing -> return ()
              Just (buf,_) -> lift $do
                  gtkbuf  <- textViewGetBuffer (sourceView buf)
                  modified <- textBufferGetModified gtkbuf
                  (Just text) <- notebookGetTabLabelText nb (scrolledWindow buf)
                  label <- labelNew Nothing
                  labelSetUseMarkup label True
                  labelSetMarkup label
                      (if modified
                          then "<span foreground=\"red\">" ++ text ++ "</span>"
                          else text)
                  notebookSetTabLabel nb (scrolledWindow buf) label

inBufContext' :: alpha -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IDEM alpha ) -> IDEM alpha
inBufContext' def f = do
    mbBuf <- maybeActiveBuf
    case mbBuf of
        Nothing -> return def
        Just (ideBuf,_) -> do
            (pane,_)    <-  guiPropertiesFromName (paneName ideBuf)
            nb          <-  getNotebook pane
            mbI         <-  lift $notebookPageNum nb (scrolledWindow ideBuf)
            case mbI of
                Nothing ->  lift $ do
                                sysMessage Normal "notebook page not found: unexpected"
                                return def
                Just i  ->  do
                                gtkbuf <- lift $ textViewGetBuffer (sourceView ideBuf)
                                f nb gtkbuf ideBuf i

inBufContext :: alpha -> (Notebook -> TextBuffer -> IDEBuffer -> Int -> IO alpha ) -> IDEM alpha
inBufContext def f = inBufContext' def (\ a b c d -> lift $ f a b c d)

fileSave :: Bool -> IDEAction
fileSave query = inBufContext' () $ \ nb _ currentBuffer i -> do
    ideR    <- ask
    window  <- readIDE window
    bufs    <- readIDE panes
    prefs   <- readIDE prefs
    paneMap <- readIDE paneMap
    bs      <- getCandyState
    candy   <- readIDE candy
    (panePath,BufConnections s1 s2 s3)
            <- guiPropertiesFromName (paneName currentBuffer)
    mbnbufsPm <- do
        let mbfn = fileName currentBuffer
        mbpage <- lift $notebookGetNthPage nb i
        case mbpage of
            Nothing     -> throwIDE "fileSave: Page not found"
            Just page   ->
                if isJust mbfn && query == False
                    then do checkModTime currentBuffer
                            lift $fileSave' (forceLineEnds prefs) currentBuffer bs candy $fromJust mbfn
                            setModTime currentBuffer
                            return Nothing
                    else lift $do
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
                            Nothing -> return Nothing
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
                                        fileSave' (forceLineEnds prefs) currentBuffer bs candy fn
                                        modT <- getModificationTime fn
                                        let bn = takeFileName fn
                                        let bufs1 =  Map.delete (paneName currentBuffer) bufs
                                        let (ind,rbn) =  figureOutPaneName bufs1 bn 0
                                        cfn <- canonicalizePath fn
                                        let newBuffer =  currentBuffer {fileName = Just cfn,
                                                        bufferName = bn, addedIndex = ind, modTime = Just modT}
                                        let newBufs   =  Map.insert rbn (PaneC newBuffer) bufs1
                                        mapM_ signalDisconnect s1
                                        mapM_ signalDisconnect s2
                                        mapM_ signalDisconnect s3
                                        cid1 <- (sourceView currentBuffer) `afterFocusIn`
                                            (\_ -> do runReaderT (makeActive newBuffer) ideR
                                                      return True)
                                        let paneMap1  =  Map.delete rbn paneMap
                                        let newPaneMap =  Map.insert rbn
                                                            (panePath,BufConnections [cid1] [] [])  paneMap
                                        label <- labelNew (Just rbn)
                                        notebookSetTabLabel nb page label
                                        return (Just (newBufs,newPaneMap))
                                    ResponseNo -> return Nothing
                                    _           -> return Nothing
    case mbnbufsPm of
        Just (nbufs,pm) -> modifyIDE_
            (\ide -> return (ide{panes = nbufs, paneMap = pm}))
        Nothing -> return ()
    where
        fileSave' :: Bool -> IDEBuffer -> Bool -> CandyTable -> FilePath -> IO()
        fileSave' forceLineEnds ideBuf bs ct fn = do
            buf     <-   textViewGetBuffer $ sourceView ideBuf
            text    <-   getCandylessText ct buf
            let text' = unlines $map removeTrailingBlanks $lines text
            if forceLineEnds
                then do
                    file <- openBinaryFile fn WriteMode
                    hPutStr file text'
                    hClose file
                else
                    writeFile fn text'
            textBufferSetModified buf False
        removeTrailingBlanks :: String -> String
        removeTrailingBlanks = reverse . dropWhile (\c -> c == ' ') . reverse

fileNew :: IDEAction
fileNew = do
    prefs   <- readIDE prefs
    pp      <- getActivePanePathOrStandard (sourcePanePath prefs)
    newTextBuffer pp "Unnamed" Nothing
    return ()

fileClose :: IDEM Bool
fileClose = inBufContext' True $ fileClose'

fileClose' :: Notebook -> TextBuffer -> IDEBuffer -> Int  -> IDEM Bool
fileClose' nb gtkbuf currentBuffer i = do
    ideRef  <- ask
    window  <- readIDE window
    bufs    <- readIDE panes
    paneMap <- readIDE paneMap
    cancel <- lift $ do
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
                        runReaderT (fileSave False) ideRef
                        return False
                    ResponseCancel  ->   return True
                    ResponseNo      ->   return False
                    _               ->   return False
            else return False
    if cancel
        then return False
        else do
            deactivatePane
            lift $notebookRemovePage nb i
            removePaneAdmin currentBuffer
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
            mbI         <-  lift $notebookPageNum nb (scrolledWindow buf)
            case mbI of
                Nothing ->  throwIDE "notebook page not found: unexpected"
                Just i  ->  do
                    gtkbuf <- lift $ textViewGetBuffer (sourceView buf)
                    let dir = dropFileName $ cabalFile activePack
                    when (isJust (fileName buf)) $ do
                        modified <- lift $ textBufferGetModified gtkbuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb gtkbuf buf i; return ()

fileOpen :: IDEAction
fileOpen = do
    window <- readIDE window
    prefs <- readIDE prefs
    mbFileName <- lift $ do
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
        Just fp -> do
            fpc <-  lift $canonicalizePath fp
            buffers <- allBuffers
            let buf = filter (\b -> case fileName b of
                                Just fn -> equalFilePath fn fpc
                                Nothing -> False) buffers
            case buf of
                hdb:tl -> do
                    md <- lift $messageDialogNew
                            Nothing []
                            MessageQuestion
                            ButtonsYesNo
                            ("Buffer already open. " ++
                             "Make active instead of opening a second time?")
                    resp <- lift $dialogRun md
                    lift $ widgetDestroy md
                    case resp of
                        ResponseNo  ->  reallyOpen prefs fpc
                        _           ->  makeActive hdb
                [] -> reallyOpen prefs fpc
    where
        reallyOpen prefs fpc =   do
            pp <-  getActivePanePathOrStandard (sourcePanePath prefs)
            newTextBuffer pp (takeFileName fpc) (Just fpc)
            return ()

editUndo :: IDEAction
editUndo = inBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canUndo <- sourceBufferCanUndo sb
        if canUndo
            then sourceBufferUndo sb
            else return ()

editRedo :: IDEAction
editRedo = inBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canRedo <- sourceBufferCanUndo sb
        if canRedo
            then sourceBufferRedo sb
            else return ()

editDelete :: IDEAction
editDelete = inBufContext ()  $ \_ gtkbuf _ _ ->  do
    textBufferDeleteSelection gtkbuf True True
    return ()

editSelectAll :: IDEAction
editSelectAll = inBufContext () $ \_ gtkbuf _ _ -> do
    start <- textBufferGetStartIter gtkbuf
    end   <- textBufferGetEndIter gtkbuf
    textBufferSelectRange gtkbuf start end

editCut :: IDEAction
#ifdef _Newgtk
editCut = inBufContext () $ \_ gtkbuf _ _ -> do
  clip <- clipboardGet ClipClipboard
  textBufferCutClipboard gtkbuf clip True
#else
editCut = return ()
#endif

editCopy :: IDEAction
#ifdef _Newgtk
editCopy = inBufContext () $ \_ gtkbuf _ _ -> do
  clip <- clipboardGet ClipClipboard
  textBufferCopyClipboard gtkbuf clip
#else
editCopy = return ()
#endif

editPaste :: IDEAction
#ifdef _Newgtk
editPaste = inBufContext () $ \_ gtkbuf _ _ -> do
  mark <- textBufferGetInsert gtkbuf
  iter <- textBufferGetIterAtMark gtkbuf mark
  clip <- clipboardGet ClipClipboard
  textBufferPasteClipboard gtkbuf clip iter True
#else
editPaste = return ()
#endif


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
    b <- textIterStartsLine endIter'
    let endLineReal = if b then endLine' - 1 else endLine'
    return (startLine',endLineReal)

doForSelectedLines :: [a] -> (TextBuffer -> TextIter -> Int -> IO a) -> IDEM [a]
doForSelectedLines d f = inBufContext' d $ \_ gtkbuf currentBuffer _ -> lift $do
    (start,end) <- getStartAndEndLineOfSelection gtkbuf
    iter <- textBufferGetStartIter gtkbuf
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
        str <- textIterGetText iter iter2
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
        return (foldl (&&) True boolList)


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
    inBufContext () $ \_ gtkbuf _ _ -> do
        transformToCandy ct gtkbuf

editFromCandy :: IDEAction
editFromCandy = do
    ct      <-  readIDE candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        transformFromCandy ct gtkbuf

editKeystrokeCandy :: Maybe Char -> IDEAction
editKeystrokeCandy c = do
    ct <- readIDE candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        keystrokeCandy ct c gtkbuf

editCandy = do
    ct      <- readIDE candy
    buffers <- allBuffers
    gtkbufs <- lift $mapM (\ b -> textViewGetBuffer (sourceView b)) buffers
    bs <- getCandyState
    if bs
        then lift $mapM_ (transformToCandy ct) gtkbufs
        else lift $mapM_ (transformFromCandy ct) gtkbufs








