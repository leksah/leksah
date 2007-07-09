module Ghf.Editor.SourceEditor (
    newTextBuffer

,   fileNew
,   fileOpen
,   fileClose
,   fileSave

,   editUndo
,   editRedo
,   editCut
,   editCopy
,   editPaste
,   editDelete
,   editSelectAll

,   SearchHint(..)
,   editFindShow
,   editFindHide
,   editFindInc
,   editFind
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

) where 

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.Glade
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust)
import Text.Printf
import Char(toUpper)
import qualified Data.Map as Map
import Data.Map (Map,(!))


  
import Ghf.Core
import Ghf.GUI.ViewFrame
import Ghf.GUI.SourceCandy

newTextBuffer :: String -> Maybe FileName -> GhfAction
newTextBuffer bn mbfn = do
    -- create the appropriate language
    ghfR <- ask
    panePath <- getActivePanePathOrTop
--    lift $putStrLn $show panePath
    nb <- getActiveOrTopNotebook
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
        foundTag <- textTagNew (Just "found")
        set foundTag [textTagBackground := "yellow"]
        tagTable <- textBufferGetTagTable buffer
        textTagTableAdd tagTable foundTag

        -- load up and display a file
        fileContents <- case mbfn of
            Just fn -> readFile fn
            Nothing -> return "\n\n\n\n\n"
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

        -- create a new SourceView Widget
        sv <- sourceViewNewWithBuffer buffer
        f <- fontDescriptionNew
        fontDescriptionSetFamily f "Monospace"
        widgetModifyFont sv (Just f)
        sourceViewSetShowLineNumbers sv (showLineNumbers prefs)
        case rightMargin prefs of
            Just n -> do
                sourceViewSetMargin sv n
                sourceViewSetShowMargin sv True
            Nothing -> sourceViewSetShowMargin sv True
        sourceViewSetInsertSpacesInsteadOfTabs sv True
        sourceViewSetTabsWidth sv (tabWidth prefs)
        sourceViewSetSmartHomeEnd sv True

        -- put it in a scrolled window
        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw sv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        let buf = GhfBuffer mbfn bn ind sv sw
        notebookPrependPage nb sw rbn
        widgetShowAll (scrolledWindow buf)
        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        -- events
        cid <- (castToWidget sv) `afterFocusIn`
            (\_ -> do runReaderT (makeBufferActive buf) ghfR; return True)
        cid2 <- (castToWidget sv) `afterFocusOut`
            (\_ -> do runReaderT (makeBufferInactive) ghfR; return True)
        return (buf,[cid,cid2])
    let newPaneMap  =  Map.insert (PaneBuf buf) (panePath,cids) paneMap
    let newPanes = Map.insert rbn (PaneBuf buf) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))    
    lift $widgetGrabFocus (sourceView buf)


makeBufferActive :: GhfBuffer -> GhfAction
makeBufferActive buf = do
    ghfR   <- ask
    sbLC   <- getStatusbarLC
    sbIO   <- getStatusbarIO
    modifyGhf_ $ \ghf -> do
      let sv = sourceView buf
      gtkBuf  <- textViewGetBuffer sv
      writeCursorPositionInStatusbar sv sbLC
      writeOverwriteInStatusbar sv sbIO
      id1 <- gtkBuf `afterModifiedChanged` runReaderT (markLabelAsChanged) ghfR
      id2 <- sv `afterMoveCursor`
          (\_ _ _ -> writeCursorPositionInStatusbar sv sbLC)
      id3 <- gtkBuf `afterEndUserAction`  writeCursorPositionInStatusbar sv sbLC
      sv `widgetAddEvents` [ButtonReleaseMask]
      id4 <- sv `onButtonRelease`(\ _ -> do writeCursorPositionInStatusbar sv sbLC; return False)
      id5 <- sv `afterToggleOverwrite`  writeOverwriteInStatusbar sv sbIO
      return (ghf{activePane = Just (PaneBuf buf,BufConnections[id2,id4,id5] [id1,id3])})

makeBufferInactive :: GhfAction
makeBufferInactive = do
    mbBS <-  maybeActiveBuf
    modifyGhf_ $ \ghf -> do
      case mbBS of 
          Just (buf,BufConnections signals signals2) -> do
              mapM_ signalDisconnect signals
              mapM_ signalDisconnect signals2
              return ghf{activePane = Just (PaneBuf buf,BufConnections[][])}
          Nothing -> return ghf

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
    statusbarPush sb 1 $if modi then "OVR" else "INS"
    return ()

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
    paneMap <- readGhf paneMap
    case mbBuf of
        Nothing -> return def
        Just (ghfBuf,_) -> do
            let (pane,_) = paneMap ! PaneBuf ghfBuf
            nb <- getNotebook pane
            mbI  <- lift $notebookPageNum nb (scrolledWindow ghfBuf)
            case mbI of
                Nothing -> lift $ do
                        putStrLn "notebook page not found: unexpected" 
                        return def
                Just i -> do
                        gtkbuf <- lift $ textViewGetBuffer (sourceView ghfBuf)
                        f nb gtkbuf ghfBuf i

inBufContext :: alpha -> (Notebook -> TextBuffer -> GhfBuffer -> Int -> IO alpha ) -> GhfM alpha 
inBufContext def f = inBufContext' def (\ a b c d -> lift $ f a b c d)

fileSave :: Bool -> GhfAction
fileSave query = inBufContext' () $ \ nb _ currentBuffer i -> do
    ghfR    <- ask
    window  <- readGhf window
    bufs    <- readGhf panes
    paneMap <- readGhf paneMap
    bs      <- getCandyState
    candy   <- readGhf candy
    mbnbufsPm <- lift $ do
        let mbfn = fileName currentBuffer
        mbpage <- notebookGetNthPage nb i
        case mbpage of
            Nothing     -> error "fileSave: Page not found"
            Just page   -> 
                if isJust mbfn && query == False
                    then do fileSave' currentBuffer bs candy $fromJust mbfn
                            return Nothing
                    else do
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
                                        fileSave' currentBuffer bs candy fn
                                        let bn = takeFileName fn
                                        let bufs1 =  Map.delete (realPaneName (PaneBuf currentBuffer)) bufs
                                        let (ind,rbn) =  figureOutPaneName bufs1 bn 0
                                        let newBuffer =  currentBuffer {fileName = Just fn,
                                                        bufferName = bn, addedIndex = ind}
                                        let newBufs   =  Map.insert rbn (PaneBuf newBuffer) bufs1
                                        let (pane,cids)=  paneMap ! (PaneBuf currentBuffer)
                                        mapM_ signalDisconnect cids
                                        cid1 <- (castToWidget (sourceView currentBuffer)) `afterFocusIn`
                                            (\_ -> do runReaderT (makeBufferActive newBuffer) ghfR
                                                      return True)
                                        cid2 <- (castToWidget (sourceView currentBuffer)) `afterFocusOut`
                                            (\_ -> do runReaderT (makeBufferInactive) ghfR;
                                                      return True)
                                        let paneMap1  =  Map.delete (PaneBuf currentBuffer) paneMap
                                        let newPaneMap =  Map.insert (PaneBuf newBuffer)
                                                            (pane,[cid1,cid2])  paneMap
                                        label <- labelNew (Just rbn)
                                        notebookSetTabLabel nb page label
                                        return (Just (newBufs,newPaneMap))
                                    ResponseNo -> return Nothing
    case mbnbufsPm of
        Just (nbufs,pm) ->modifyGhf_
            (\ghf -> return (ghf{panes = nbufs, paneMap = pm}))
        Nothing -> return ()
    where
        fileSave' :: GhfBuffer -> Bool -> CandyTables -> FileName -> IO()
        fileSave' ghfBuf bs (to,from) fn = do
            buf     <- textViewGetBuffer $ sourceView ghfBuf
            sourceBufferBeginNotUndoableAction (castToSourceBuffer buf)
            if bs 
                then transformFromCandy from buf
                else return ()
            start   <- textBufferGetStartIter buf
            end     <- textBufferGetEndIter buf
            text    <- textBufferGetText buf start end True
            if bs 
                then transformToCandy to buf
                else return ()
            writeFile fn text
            sourceBufferEndNotUndoableAction (castToSourceBuffer buf)
            textBufferSetModified buf False

fileNew :: GhfAction
fileNew = newTextBuffer "Unnamed" Nothing

fileClose :: GhfM Bool
fileClose = inBufContext' False $ \nb gtkbuf currentBuffer i -> do
    ghfRef  <- ask
    window  <- readGhf window
    bufs    <- readGhf panes
    paneMap <- readGhf paneMap
    mbbuf <- lift $ do
        modified <- textBufferGetModified gtkbuf
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsNone
                                            ("Save changes to document: "
                                                ++ realPaneName (PaneBuf currentBuffer)
                                                ++ "?")
                dialogAddButton md "_Save" ResponseYes
                dialogAddButton md "_Don't Save" ResponseNo
                dialogAddButton md "_Cancel" ResponseCancel
                resp <- dialogRun md
                widgetHide md
                case resp of
                    ResponseYes -> do   runReaderT (fileSave False) ghfRef 
                                        notebookRemovePage nb i
                                        return (Just currentBuffer)
                    ResponseCancel ->   return Nothing
                    ResponseNo -> do    notebookRemovePage nb i
                                        return (Just currentBuffer)
            else do
                notebookRemovePage nb i
                return (Just currentBuffer)
    case mbbuf of
        Just buf -> do
            let newBuffers = Map.delete (realPaneName (PaneBuf currentBuffer)) bufs
            let newPaneMap = Map.delete (PaneBuf currentBuffer) paneMap
            modifyGhf_ (\ghf -> return (ghf{panes = newBuffers, paneMap = newPaneMap}))
            guessNewActiveBuffer nb
            return True
        Nothing -> return False

fileOpen :: GhfAction
fileOpen = do
    window <- readGhf window
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
        Just fn -> newTextBuffer (takeFileName fn) (Just fn)

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

--Unfortunately the current impossible ones
editCut :: GhfAction
editCut = return ()
editCopy :: GhfAction
editCopy = return ()
editPaste :: GhfAction
editPaste = return ()

            
red = Color 640000 10000 10000
white = Color 64000 64000 64000
black = Color 0 0 0

-- | Show the find bar
editFindShow :: GhfAction
editFindShow = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    entry   <-  getFindEntry
    findBar <-  getFindBar
    lift $do
        widgetShow findBar
        widgetGrabFocus entry 

-- | Hides the find bar
editFindHide :: GhfAction
editFindHide = inBufContext' () $ \nb gtkbuf currentBuffer _ -> do
    findBar <-  getFindBar
    lift $do
        widgetHide findBar
        i1 <- textBufferGetStartIter gtkbuf
        i2 <- textBufferGetEndIter gtkbuf       
        textBufferRemoveTagByName gtkbuf "found" i1 i2  
        widgetGrabFocus $ sourceView currentBuffer 

-- | Keys for searching
editFindKey :: Event -> GhfAction
editFindKey k@(Key _ _ _ _ _ _ _ _ _ _) 
    | eventKeyName k == "Down" =
        editFindInc Forward        
    | eventKeyName k == "Up" =
        editFindInc Backward        
    | eventKeyName k == "Escape" = do
        editFindHide  
    | otherwise = return ()

data SearchHint = Forward | Backward | Insert | Delete
    deriving (Eq)

{-- can't be used currently becuase of an export error
  toEnum 1 = SourceSearchVisibleOnly
  toEnum 2 = SourceSearchTextOnly
  toEnum 4 = SourceSearchCaseInsensitive
--}

editFindInc :: SearchHint -> GhfAction 
editFindInc hint = do
    entry   <- getFindEntry
    search  <- lift $entryGetText entry
    caseSensitiveW <- getCaseSensitive
    caseSensitive <- lift $toggleButtonGetActive caseSensitiveW
    entireWButton <- getEntireWord
    entireW <- lift $toggleButtonGetActive entireWButton
    wrapAroundButton <- getWrapAround
    wrapAround <- lift $toggleButtonGetActive wrapAroundButton
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
        editableSelectRegion entry (length search) (length search)        


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
                        else do
                            textIterBackwardChar st1
                            textIterBackwardChar st1
                    mbsr <- forwardSearch st1 search searchflags entireWord searchflags
                    case mbsr of
                        Nothing ->
                            if wrapAround
                                then do forwardSearch i1 search searchflags entireWord searchflags
                                else return Nothing
                        Just (start,end) -> return (Just (start,end))
            case mbsr2 of
                Just (start,end) -> do --found
                    textViewScrollToIter (sourceView currentBuffer) start 0.2 Nothing
                    textBufferApplyTagByName gtkbuf "found" start end
                    textBufferSelectRange gtkbuf start end
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
    spin <- getGotoLineSpin
    lift $do
        max <- textBufferGetLineCount gtkbuf
        spinButtonSetRange spin 1.0 (fromIntegral max) 
        widgetShow spin
        widgetGrabFocus spin

editGotoLineKey :: Event -> GhfAction
editGotoLineKey k@(Key _ _ _ _ _ _ _ _ _ _) 
    | eventKeyName k == "Escape"  =
        inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
            spin <- getGotoLineSpin
            lift $ do 
                widgetHide spin
                widgetGrabFocus $ sourceView currentBuffer 
    | otherwise = return ()
{--editGotoLineKey k@(Focus _ _ ) 
    | eventInFocus k == False =
        inBufContext' () $ \gtkbuf currentBuffer _ -> do
        spin <- getGotoLineSpin
        lift $ do
            widgetHide spin
    | otherwise = return ()--}

editGotoLineEnd :: GhfAction
editGotoLineEnd = inBufContext' () $ \_ gtkbuf currentBuffer _ -> do
    spin <- getGotoLineSpin
    lift $ do 
        line <- spinButtonGetValueAsInt spin
        iter <- textBufferGetStartIter gtkbuf
        textIterSetLine iter (line - 1) 
        textBufferPlaceCursor gtkbuf iter
        textViewScrollToIter (sourceView currentBuffer) iter 0.2 Nothing
        widgetHide spin
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

editKeystrokeCandy = do
    (to,_) <- readGhf candy
    inBufContext () $ \_ gtkbuf _ _ -> do
        keystrokeCandy to gtkbuf

editCandy = do
    (to,from) <- readGhf candy
    buffers <- allBuffers
    gtkbufs <- lift $mapM (\ b -> textViewGetBuffer (sourceView b)) buffers
    bs <- getCandyState
    if bs
        then lift $mapM_ (transformToCandy to) gtkbufs
        else lift $mapM_ (transformFromCandy from) gtkbufs

--Properties of a replace (get rid and do everythink in the statusbar)

replaceDialog :: GhfAction
replaceDialog = do
    ghfR <- ask    
    lift $ do
        dialogXmlM <- xmlNew "dialogs/ghf-replace-dialog.glade"
        let dialogXml = case dialogXmlM of
                            (Just dialogXml) -> dialogXml	       
                            Nothing -> error "can't find the glade file \"ghf-replace-dialog.glade\""
        window <- xmlGetWidget dialogXml castToWindow "dialog"
        closeButton <- xmlGetWidget dialogXml castToButton "close_button"
        replaceAllButton <- xmlGetWidget dialogXml castToButton "replace_all_button"
        replaceButton <- xmlGetWidget dialogXml castToButton "replace_button"
        findButton <- xmlGetWidget dialogXml castToButton "find_button"
        matchCaseCheckbutton <- xmlGetWidget dialogXml castToCheckButton "match_case_checkbutton"      
        entireWordCheckbutton <- xmlGetWidget dialogXml castToCheckButton "entire_word_checkbutton"      
        searchBackwardsCheckbutton <- xmlGetWidget dialogXml castToCheckButton "search_backwards_checkbutton"      
        wrapAroundCheckbutton <- xmlGetWidget dialogXml castToCheckButton "wrap_around_checkbutton"      
        searchForEntry <- xmlGetWidget dialogXml castToEntry "search_for_entry"      
        replaceWithEntry <- xmlGetWidget dialogXml castToEntry "replace_with_entry"      
    
        let findOrSearch = \f -> do
            wrapAround <- toggleButtonGetActive wrapAroundCheckbutton 
            entireWord <- toggleButtonGetActive entireWordCheckbutton
            matchCase  <- toggleButtonGetActive matchCaseCheckbutton
            backwards  <- toggleButtonGetActive searchBackwardsCheckbutton
            let hint = if backwards then Backward else Forward
            searchString <- entryGetText searchForEntry
            replaceString <- entryGetText replaceWithEntry    
            found <- runReaderT (f entireWord matchCase wrapAround searchString replaceString hint) ghfR
            widgetSetSensitivity replaceButton found
            widgetSetSensitivity replaceAllButton found
            return ()

        findButton `onClicked` do 
            putStrLn "find"
            findOrSearch editFind
        replaceButton `onClicked` do
            putStrLn "replace"            
            findOrSearch editReplace
        replaceAllButton `onClicked` do
            putStrLn "replaceAll"
            findOrSearch editReplaceAll
        closeButton `onClicked` (widgetDestroy window) 

        widgetShowAll window
    




    

