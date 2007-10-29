-----------------------------------------------------------------------------
--
-- Module      :  Ghf.SourceEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The editor part of GHF
--
-----------------------------------------------------------------------------------

module Ghf.SourceEditor (
    isBuffer
,   allBuffers
,   maybeActiveBuf
,   standardSourcePanePath

,   newTextBuffer

,   fileNew
,   fileOpen
,   fileRevert
,   fileClose
,   fileCloseAll
,   fileSave
,   editUndo
,   editRedo
,   editCut
,   editCopy
,   editPaste
,   editDelete
,   editSelectAll

,   SearchHint(..)
,   editFindInc
,   editFindKey
,   editReplace
,   editReplaceAll

,   editGotoLine
,   editGotoLineEnd
,   editGotoLineKey

,   editComment
,   editUncomment
,   editShiftRight
,   editShiftLeft

,   editToCandy
,   editFromCandy
,   editKeystrokeCandy
,   editCandy

,   replaceDialog
,   makeBufferActive

) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
--import Graphics.UI.Gtk.General.Clipboard
import Control.Monad.Reader
import Data.IORef
import System.IO
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust)
import Text.Printf
import Data.Char(toUpper)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List


import Ghf.Core
import Ghf.ViewFrame
import Ghf.SourceCandy
import Ghf.PropertyEditor
import Ghf.Log
import Ghf.Info
import Ghf.InfoPane


isBuffer :: GhfPane -> Bool
isBuffer (BufPane _) = True
isBuffer _           = False

allBuffers :: GhfM [GhfBuffer]
allBuffers = do
    panesST <- readGhf panes
    return (map (\ (BufPane b) -> b) $filter isBuffer $Map.elems panesST)

maybeActiveBuf :: GhfM (Maybe (GhfBuffer,Connections))
maybeActiveBuf = do
    mbPane   <- readGhf activePane
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,signals) -> do
            pane <- paneFromUniqueName paneName
            case pane of
                BufPane buf -> return (Just (buf,signals))
                otherwise   -> return Nothing

standardSourcePanePath :: GhfM PanePath
standardSourcePanePath = do
    layout  <-  readGhf layout
    prefs   <-  readGhf prefs
    return (getStandardPanePath (sourcePanePath prefs) layout)

newTextBuffer :: PanePath -> String -> Maybe FileName -> GhfAction
newTextBuffer panePath bn mbfn = do
    -- create the appropriate language
    ghfR <- ask
    nb <- getNotebook panePath
    panes <- readGhf panes
    paneMap <- readGhf paneMap
    prefs <- readGhf prefs
    bs <- getCandyState
    (from,_) <- readGhf candy
    let (ind,rbn) = figureOutPaneName panes bn 0
    (buf,cids) <- lift $ do
        lm      <-  sourceLanguagesManagerNew
        langM   <-  sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
        lang    <-  case langM of
                        (Just lang) -> return lang
                        Nothing -> do
                            langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
                            error ("please copy haskell.lang to one of the following"
                                  ++ "directories:\n"
                                ++ unlines langDirs)

        -- create a new SourceBuffer object
        buffer <- sourceBufferNewWithLanguage lang
        tagTable <- textBufferGetTagTable buffer
        foundTag <- textTagNew (Just "found")
        set foundTag [textTagBackground := "yellow"]
        textTagTableAdd tagTable foundTag
        activeErrtag <- textTagNew (Just "activeErr")
        set activeErrtag[textTagBackground := "yellow"]
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
            then transformToCandy from (castToTextBuffer buffer)
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

        let buf = GhfBuffer mbfn bn ind sv sw modTime
        notebookPrependPage nb sw rbn
        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        -- events
        cid <- sv `afterFocusIn`
            (\_ -> do runReaderT (makeBufferActive rbn) ghfR; return True)
        return (buf,[cid])
    let newPaneMap  =   Map.insert rbn (panePath,BufConnections cids [] []) paneMap
    let newPanes    =   Map.insert rbn (BufPane buf) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))
    lift $widgetShowAll (scrolledWindow buf)
    lift $widgetGrabFocus (sourceView buf)


makeBufferActive :: PaneName -> GhfAction
makeBufferActive pn = do
    pane    <-  paneFromUniqueName pn
    case pane of
        BufPane buf -> do
            ghfR    <-  ask
            sbLC    <-  getStatusbarLC
            sbIO    <-  getStatusbarIO
            infos   <-  readGhf accessibleInfo
            let sv = sourceView buf
            (tl,tm,tr) <- lift $do
                gtkBuf  <- textViewGetBuffer sv
                bringPaneToFront (BufPane buf)
                writeCursorPositionInStatusbar sv sbLC
                writeOverwriteInStatusbar sv sbIO
                id1 <- gtkBuf `afterModifiedChanged` runReaderT (markLabelAsChanged) ghfR
                id2 <- sv `afterMoveCursor`
                    (\_ _ _ -> writeCursorPositionInStatusbar sv sbLC)
                id3 <- gtkBuf `afterEndUserAction`  writeCursorPositionInStatusbar sv sbLC
                sv `widgetAddEvents` [ButtonReleaseMask]
                id4 <- sv `onButtonRelease` (\ _ -> do
                                                writeCursorPositionInStatusbar sv sbLC
                                                return False)
                id5 <- sv `onButtonRelease` (\ e -> do
                                                showType sv ghfR
                                                return False)
                id6 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv sbIO
                return ([id2,id4,id6],[id1,id3],[])
            activatePane (BufPane buf) (BufConnections tl tm tr)
            checkModTime buf
        otherwise -> return ()


checkModTime :: GhfBuffer -> GhfAction
checkModTime buf = do
    panes <- readGhf panes
    let name = uniquePaneName (BufPane buf)
    case fileName buf of
        Nothing -> return ()
        Just fn -> do
            exists <- lift $doesFileExist fn
            if exists
                then do
                    nmt <- lift $getModificationTime fn
                    case modTime buf of
                        Nothing ->  error $"checkModTime: time not set " ++ show (fileName buf)
                        Just mt -> do
                            message $"checkModTime " ++ name ++ " " ++ show mt ++ " " ++ show nmt
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
                                            let newPanes = Map.adjust (\b -> case b of
                                                                                BufPane  b -> BufPane (b{modTime = (Just nmt)})
                                                                                it         -> it) name panes
                                            modifyGhf_ (\ghf -> return (ghf{panes = newPanes}))
                                            lift $widgetHide md
                                else return ()
                else return ()

setModTime :: GhfBuffer -> GhfAction
setModTime buf = do
    panes <- readGhf panes
    let name = uniquePaneName (BufPane buf)
    case fileName buf of
        Nothing -> return ()
        Just fn -> do
            nmt <- lift $getModificationTime fn
            let newPanes = Map.adjust (\b -> case b of
                                                BufPane  b -> BufPane (b{modTime = (Just nmt)})
                                                it         -> it) name panes
            modifyGhf_ (\ghf -> return (ghf{panes = newPanes}))


fileRevert :: GhfAction
fileRevert = inBufContext' () $ \ _ _ currentBuffer _ -> do
    revert currentBuffer

revert :: GhfBuffer -> GhfAction
revert buf = do
    useCandy <- getCandyState
    (fromCandy,_) <- readGhf candy
    panes <- readGhf panes
    let name = uniquePaneName (BufPane buf)
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
                    then transformToCandy fromCandy (castToTextBuffer buffer)
                    else return ()
                sourceBufferEndNotUndoableAction buffer
                textBufferSetModified buffer False
                return mt
            let newPanes = Map.adjust (\b -> case b of
                                                    BufPane  b -> BufPane (b{modTime = (Just mt)})
                                                    it         -> it) name panes
            modifyGhf_ (\ghf -> return (ghf{panes = newPanes}))

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


showType :: SourceView -> GhfRef -> IO ()
showType sv ghfR = do
    buf  <-  textViewGetBuffer sv
    (l,r) <- textBufferGetSelectionBounds buf
    symbol <- textBufferGetText buf l r True
    ghf <- readIORef ghfR
    case currentInfo ghf of
        Nothing -> return ()
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            case getIdentifierDescr symbol symbolTable1 symbolTable2 of
                [] -> return ()
                a -> runReaderT (setInfo (head a)) ghfR

markLabelAsChanged :: GhfAction
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

inBufContext' :: alpha -> (Notebook -> TextBuffer -> GhfBuffer -> Int -> GhfM alpha ) -> GhfM alpha
inBufContext' def f = do
    mbBuf <- maybeActiveBuf
    case mbBuf of
        Nothing -> return def
        Just (ghfBuf,_) -> do
            (pane,_)    <-  guiPropertiesFromName (uniquePaneName (BufPane ghfBuf))
            nb          <-  getNotebook pane
            mbI         <-  lift $notebookPageNum nb (scrolledWindow ghfBuf)
            case mbI of
                Nothing ->  lift $ do
                                putStrLn "notebook page not found: unexpected"
                                return def
                Just i  ->  do
                                gtkbuf <- lift $ textViewGetBuffer (sourceView ghfBuf)
                                f nb gtkbuf ghfBuf i

inBufContext :: alpha -> (Notebook -> TextBuffer -> GhfBuffer -> Int -> IO alpha ) -> GhfM alpha
inBufContext def f = inBufContext' def (\ a b c d -> lift $ f a b c d)

fileSave :: Bool -> GhfAction
fileSave query = inBufContext' () $ \ nb _ currentBuffer i -> do
    ghfR    <- ask
    window  <- readGhf window
    bufs    <- readGhf panes
    prefs   <- readGhf prefs
    paneMap <- readGhf paneMap
    bs      <- getCandyState
    candy   <- readGhf candy
    (panePath,BufConnections s1 s2 s3)
            <- guiPropertiesFromName (uniquePaneName (BufPane currentBuffer))
    mbnbufsPm <- do
        let mbfn = fileName currentBuffer
        mbpage <- lift $notebookGetNthPage nb i
        case mbpage of
            Nothing     -> error "fileSave: Page not found"
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
                                        let bufs1 =  Map.delete (uniquePaneName (BufPane currentBuffer)) bufs
                                        let (ind,rbn) =  figureOutPaneName bufs1 bn 0
                                        cfn <- canonicalizePath fn
                                        let newBuffer =  currentBuffer {fileName = Just cfn,
                                                        bufferName = bn, addedIndex = ind, modTime = Just modT}
                                        let newBufs   =  Map.insert rbn (BufPane newBuffer) bufs1
                                        mapM_ signalDisconnect s1
                                        mapM_ signalDisconnect s2
                                        mapM_ signalDisconnect s3
                                        cid1 <- (sourceView currentBuffer) `afterFocusIn`
                                            (\_ -> do runReaderT (makeBufferActive rbn) ghfR
                                                      return True)
                                        let paneMap1  =  Map.delete rbn paneMap
                                        let newPaneMap =  Map.insert rbn
                                                            (panePath,BufConnections [cid1] [] [])  paneMap
                                        label <- labelNew (Just rbn)
                                        notebookSetTabLabel nb page label
                                        return (Just (newBufs,newPaneMap))
                                    ResponseNo -> return Nothing
    case mbnbufsPm of
        Just (nbufs,pm) -> modifyGhf_
            (\ghf -> return (ghf{panes = nbufs, paneMap = pm}))
        Nothing -> return ()
    where
        fileSave' :: Bool -> GhfBuffer -> Bool -> CandyTables -> FileName -> IO()
        fileSave' forceLineEnds ghfBuf bs (to,from) fn = do
            buf     <-   textViewGetBuffer $ sourceView ghfBuf
            text    <-   getCandylessText from buf
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

fileNew :: GhfAction
fileNew = do
    prefs   <- readGhf prefs
    pp      <- getActivePanePathOrStandard (sourcePanePath prefs)
    newTextBuffer pp "Unnamed" Nothing

fileClose :: GhfM Bool
fileClose = inBufContext' True $ \nb gtkbuf currentBuffer i -> do
    ghfRef  <- ask
    window  <- readGhf window
    bufs    <- readGhf panes
    paneMap <- readGhf paneMap
    cancel <- lift $ do
        modified <- textBufferGetModified gtkbuf
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsNone
                                            ("Save changes to document: "
                                                ++ uniquePaneName (BufPane currentBuffer)
                                                ++ "?")
                dialogAddButton md "_Save" ResponseYes
                dialogAddButton md "_Don't Save" ResponseNo
                dialogAddButton md "_Cancel" ResponseCancel
                resp <- dialogRun md
                widgetHide md
                case resp of
                    ResponseYes ->   do
                        runReaderT (fileSave False) ghfRef
                        return False
                    ResponseCancel  ->   return True
                    ResponseNo      ->   return False
            else return False
    if cancel
        then return False
        else do
            deactivatePane
            lift $notebookRemovePage nb i
            let newPanes = Map.delete (uniquePaneName (BufPane currentBuffer)) bufs
            let newPaneMap = Map.delete (uniquePaneName (BufPane currentBuffer)) paneMap
            modifyGhf_ (\ghf -> return (ghf{panes = newPanes, paneMap = newPaneMap}))
            return True

fileCloseAll :: GhfM Bool
fileCloseAll = do
    bufs    <- allBuffers
    if null bufs
        then return True
        else do
            makeBufferActive (uniquePaneName (BufPane (head bufs)))
            r <- fileClose
            if r
                then fileCloseAll
                else return False

fileOpen :: GhfAction
fileOpen = do
    window <- readGhf window
    prefs <- readGhf prefs
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
    case mbFileName of
        Nothing -> return ()
        Just fn -> do
            pp <-  getActivePanePathOrStandard (sourcePanePath prefs)
            cfn <- lift $canonicalizePath fn
            newTextBuffer pp (takeFileName fn) (Just cfn)

editUndo :: GhfAction
editUndo = inBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canUndo <- sourceBufferCanUndo sb
        if canUndo
            then sourceBufferUndo sb
            else return ()

editRedo :: GhfAction
editRedo = inBufContext () $ \_ gtkbuf _ _ ->
    let sb = castToSourceBuffer gtkbuf in
    do  canRedo <- sourceBufferCanUndo sb
        if canRedo
            then sourceBufferRedo sb
            else return ()

editDelete :: GhfAction
editDelete = inBufContext ()  $ \_ gtkbuf _ _ ->  do
    textBufferDeleteSelection gtkbuf True True
    return ()

editSelectAll :: GhfAction
editSelectAll = inBufContext () $ \_ gtkbuf _ _ -> do
    start <- textBufferGetStartIter gtkbuf
    end   <- textBufferGetEndIter gtkbuf
    textBufferSelectRange gtkbuf start end

editCut :: GhfAction
#ifdef _Newgtk
editCut = inBufContext () $ \_ gtkbuf _ _ -> do
  clip <- clipboardGet ClipClipboard
  textBufferCutClipboard gtkbuf clip True
#else
editCut = return ()
#endif

editCopy :: GhfAction
#ifdef _Newgtk
editCopy = inBufContext () $ \_ gtkbuf _ _ -> do
  clip <- clipboardGet ClipClipboard
  textBufferCopyClipboard gtkbuf clip
#else
editCopy = return ()
#endif

editPaste :: GhfAction
#ifdef _Newgtk
editPaste = inBufContext () $ \_ gtkbuf _ _ -> do
  mark <- textBufferGetInsert gtkbuf
  iter <- textBufferGetIterAtMark gtkbuf mark
  clip <- clipboardGet ClipClipboard
  textBufferPasteClipboard gtkbuf clip iter True
#else
editPaste = return ()
#endif

red = Color 640000 10000 10000
white = Color 64000 64000 64000
black = Color 0 0 0

-- | Keys for searching
editFindKey :: Event -> GhfAction
editFindKey k@(Key _ _ _ _ _ _ _ _ _ _)
    | eventKeyName k == "Down" =
        editFindInc Forward
    | eventKeyName k == "Up" =
        editFindInc Backward
    | eventKeyName k == "Escape" = do
        entry   <- getTBFindEntry
        inBufContext' () $ \_ gtkbuf currentBuffer _ -> lift $ do
            i1 <- textBufferGetStartIter gtkbuf
            i2 <- textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "found" i1 i2
            startMark <- textBufferGetInsert gtkbuf
            st1 <- textBufferGetIterAtMark gtkbuf startMark
            textBufferPlaceCursor gtkbuf st1
            widgetGrabFocus $ sourceView currentBuffer
    | otherwise = return ()

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

{-- can't be used currently becuase of an export error
  toEnum 1 = SourceSearchVisibleOnly
  toEnum 2 = SourceSearchTextOnly
  toEnum 4 = SourceSearchCaseInsensitive
--}

editFindInc :: SearchHint -> GhfAction
editFindInc hint = do
    entry   <- getTBFindEntry
    lift $widgetGrabFocus entry
    when (hint == Initial)
        (lift $ editableSelectRegion entry 0 (-1))
    search  <- lift $entryGetText entry
    if null search
        then return ()
        else do
            caseSensitiveW <- getTBCaseSensitive
            caseSensitive <- lift $toggleToolButtonGetActive caseSensitiveW
            entireWButton <- getTBEntireWord
            entireW <- lift $toggleToolButtonGetActive entireWButton
            wrapAroundButton <- getTBWrapAround
            wrapAround <- lift $toggleToolButtonGetActive wrapAroundButton
            res <- editFind entireW caseSensitive wrapAround search "" hint
            if res || null search
                then lift $do
                    widgetModifyBase entry StateNormal white
                    widgetModifyText entry StateNormal black
                else lift $do
                    widgetModifyBase entry StateNormal red
                    widgetModifyText entry StateNormal white
            lift $do
                widgetGrabFocus entry
                editableSelectRegion entry 0 (-1)


editFind :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> GhfM Bool
editFind entireWord caseSensitive wrapAround search dummy hint =
    let searchflags = (if caseSensitive then [] else [toEnum 4]) ++ [toEnum 1,toEnum 2] in
    if null search
        then return False
        else inBufContext' False $ \_ gtkbuf currentBuffer _ -> lift $ do
            i1 <- textBufferGetStartIter gtkbuf
            i2 <- textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "found" i1 i2
            startMark <- textBufferGetInsert gtkbuf
            st1 <- textBufferGetIterAtMark gtkbuf startMark
            mbsr2 <-
                if hint == Backward
                    then do
                        textIterBackwardChar st1
                        textIterBackwardChar st1
                        mbsr <- backSearch st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do backSearch i2 search searchflags entireWord searchflags
                                    else return Nothing
                            Just (start,end) -> return (Just (start,end))
                    else do
                        if hint == Forward
                            then textIterForwardChar st1
                            else return True
                        mbsr <- forwardSearch st1 search searchflags entireWord searchflags
                        case mbsr of
                            Nothing ->
                                if wrapAround
                                    then do forwardSearch i1 search searchflags entireWord searchflags
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
        backSearch iter string flags entireWord searchflags = do
            mbsr <- sourceIterBackwardSearch iter search searchflags Nothing
            case mbsr of
                Nothing -> return Nothing
                Just (iter1,iter2) ->
                    if entireWord
                        then do
                            b1 <- textIterStartsWord iter1
                            b2 <- textIterEndsWord iter2
                            if b1 && b2 then return $Just (iter1,iter2) else return Nothing
                        else return (Just (iter1,iter2))
        forwardSearch iter string flags entireWord searchflags = do
            mbsr <- sourceIterForwardSearch iter search searchflags Nothing
            case mbsr of
                Nothing -> return Nothing
                Just (iter1,iter2) ->
                    if entireWord
                        then do
                            b1 <- textIterStartsWord iter1
                            b2 <- textIterEndsWord iter2
                            if b1 && b2 then return $Just (iter1,iter2) else return Nothing
                        else return $Just (iter1,iter2)


editReplace :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> GhfM Bool
editReplace entireWord caseSensitive wrapAround search replace hint =
    editReplace' entireWord caseSensitive wrapAround search replace hint True

editReplace' :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> Bool -> GhfM Bool
editReplace' entireWord caseSensitive wrapAround search replace hint mayRepeat =
    inBufContext' False $ \_ gtkbuf currentBuffer _ -> do
        startMark <- lift $textBufferGetInsert gtkbuf
        iter <- lift $textBufferGetIterAtMark gtkbuf startMark
        iter2 <- lift $textIterCopy iter
        lift $textIterForwardChars iter2 (length search)
        str1 <- lift $textIterGetText iter iter2
        if compare str1 search caseSensitive
            then do
                lift $textBufferDelete gtkbuf iter iter2
                lift $textBufferInsert gtkbuf iter replace
                editFind entireWord caseSensitive wrapAround search "" hint
            else do
                r <- editFind entireWord caseSensitive wrapAround search "" hint
                if r
                    then editReplace' entireWord caseSensitive wrapAround search
                            replace hint False
                    else return False
    where
        compare s1 s2 True = s1 == s2
        compare s1 s2 False = map toUpper s1 == map toUpper s2

editReplaceAll :: Bool -> Bool -> Bool -> String -> String -> SearchHint -> GhfM Bool
editReplaceAll entireWord caseSensitive wrapAround search replace hint = do
    res <- editReplace' entireWord caseSensitive wrapAround search replace hint True
    if res
        then editReplaceAll entireWord caseSensitive wrapAround search replace hint
        else return False


editGotoLine :: GhfAction
editGotoLine = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    spin <- getTBGotoLineSpin
    lift $do
        max <- textBufferGetLineCount gtkbuf
        spinButtonSetRange spin 1.0 (fromIntegral max)
        widgetGrabFocus spin

editGotoLineKey :: Event -> GhfAction
editGotoLineKey k@(Key _ _ _ _ _ _ _ _ _ _)
    | eventKeyName k == "Escape"  =
        inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
            spin <- getTBGotoLineSpin
            lift $ do
                widgetGrabFocus $ sourceView currentBuffer
    | otherwise = return ()

editGotoLineEnd :: GhfAction
editGotoLineEnd = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    spin <- getTBGotoLineSpin
    lift $ do
        line <- spinButtonGetValueAsInt spin
        iter <- textBufferGetStartIter gtkbuf
        textIterSetLine iter (line - 1)
        textBufferPlaceCursor gtkbuf iter
        textViewScrollToIter (sourceView currentBuffer) iter 0.2 Nothing
        widgetGrabFocus $ sourceView currentBuffer


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

doForSelectedLines :: [a] -> (TextBuffer -> TextIter -> Int -> IO a) -> GhfM [a]
doForSelectedLines d f = inBufContext' d $ \_ gtkbuf currentBuffer _ -> lift $do
    (start,end) <- getStartAndEndLineOfSelection gtkbuf
    iter <- textBufferGetStartIter gtkbuf
    mapM (f gtkbuf iter) [start .. end]

editComment :: GhfAction
editComment = do
    doForSelectedLines [] $ \gtkbuf iter lineNr -> do
        textIterSetLine iter lineNr
        textBufferInsert gtkbuf iter "--"
    return ()

editUncomment :: GhfAction
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

editShiftLeft :: GhfAction
editShiftLeft = do
    prefs <- readGhf prefs
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


editShiftRight :: GhfAction
editShiftRight = do
    prefs <- readGhf prefs
    let str = map (\_->' ') [1 .. (tabWidth prefs)]
    doForSelectedLines [] $ \gtkbuf iter lineNr -> do
        textIterSetLine iter lineNr
        textBufferInsert gtkbuf iter str
    return ()

editToCandy :: GhfAction
editToCandy = do
    (to,_) <- readGhf candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        transformToCandy to gtkbuf

editFromCandy :: GhfAction
editFromCandy = do
    (_,from) <- readGhf candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        transformFromCandy from gtkbuf

editKeystrokeCandy :: Maybe Char -> GhfAction
editKeystrokeCandy c = do
    (to,_) <- readGhf candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        keystrokeCandy c to gtkbuf

editCandy = do
    (to,from) <- readGhf candy
    buffers <- allBuffers
    gtkbufs <- lift $mapM (\ b -> textViewGetBuffer (sourceView b)) buffers
    bs <- getCandyState
    if bs
        then lift $mapM_ (transformToCandy to) gtkbufs
        else lift $mapM_ (transformFromCandy from) gtkbufs



--Properties of a replace (get rid and do everythink in the statusbar)
data ReplaceState = ReplaceState{
    searchFor       ::   String
,   replaceWith     ::   String
,   matchCase       ::   Bool
,   matchEntire     ::   Bool
,   searchBackwards ::   Bool}

emptyReplaceState = ReplaceState "" "" False False False

replaceDescription :: [FieldDescriptionE ReplaceState]
replaceDescription = [
        mkFieldE (emptyParams
            {   paraName = Just "Search for"})
            searchFor
            (\ b a -> a{searchFor = b})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName = Just "Replace with"})
            replaceWith
            (\ b a -> a{replaceWith = b})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName = Just "Match case"})
            matchCase
            (\ b a -> a{matchCase = b})
            boolEditor
    ,   mkFieldE (emptyParams
            {   paraName = Just "Entire word"})
            matchEntire
            (\ b a -> a{matchEntire = b})
            boolEditor
    ,   mkFieldE (emptyParams
            {   paraName = Just "Search backwards"})
            searchBackwards
            (\ b a -> a{searchBackwards = b})
            boolEditor]

replaceDialog :: GhfAction
replaceDialog = do
    ghfR <- ask
    lift $replaceDialog' emptyReplaceState replaceDescription ghfR


replaceDialog' :: ReplaceState -> [FieldDescriptionE ReplaceState] -> GhfRef -> IO ()
replaceDialog' replace replaceDesc ghfR  = do
    dialog  <- windowNew
    vb      <- vBoxNew False 0
    bb      <- hButtonBoxNew
    close   <- buttonNewFromStock "gtk-close"
    replAll <- buttonNewWithMnemonic "Replace _all"
    replB   <- buttonNewWithMnemonic "_Replace"
    find    <- buttonNewWithMnemonic "_Find"
    boxPackStart bb close PackNatural 0
    boxPackStart bb replAll PackNatural 0
    boxPackStart bb replB PackNatural 0
    boxPackStart bb find PackNatural 0
    resList <- mapM (\ fd -> (fieldEditor fd) replace) replaceDesc
    let (widgetsP, setInjsP, getExtsP, notifiersP) = unzip4 resList
    mapM_ (\ w -> boxPackStart vb w PackNatural 0) widgetsP
    let fieldNames = map (\fd -> case paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed") replaceDesc
    find `onClicked` do
        findOrSearch editFind getExtsP fieldNames replB replAll
    replB `onClicked` do
        findOrSearch editReplace getExtsP fieldNames replB replAll
    replAll `onClicked` do
        findOrSearch editReplaceAll getExtsP fieldNames replB replAll
    close `onClicked` do
        widgetDestroy dialog
        mainQuit
    dialog `onDelete` (\_ -> do
        widgetDestroy dialog
        mainQuit
        return True)
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetShowAll dialog
    mainGUI
    where
        findOrSearch :: (Bool -> Bool -> Bool -> String -> String -> SearchHint -> GhfM Bool)
            -> [ReplaceState -> Extractor ReplaceState] -> [String] -> Button -> Button -> IO()
        findOrSearch f getExtsP fieldNames replB replAll =  do
            mbReplaceState <- extractAndValidate replace getExtsP fieldNames
            case mbReplaceState of
                Nothing -> return ()
                Just rs -> do
                    let hint = if searchBackwards rs then Backward else Forward
                    found <- runReaderT (f (matchEntire rs) (matchCase rs) False
                                            (searchFor rs) (replaceWith rs) hint) ghfR
                    widgetSetSensitivity replB found
                    widgetSetSensitivity replAll found
                    return ()






