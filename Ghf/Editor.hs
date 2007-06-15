module Ghf.Editor (
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


,   SearchHint(..)

) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust )
import Text.Printf
import Char(toUpper)
import qualified Data.Map as Map
import Data.Map (Map)
  
import Ghf.Core
import Ghf.CoreGui

tabWidth = 4

newTextBuffer :: String -> Maybe FileName -> GhfAction
newTextBuffer bn mbfn = do
    -- create the appropriate language
    (p,nb) <- getActiveOrDefaultPNotebook
    bufs <- readGhf buffers
    statLC <- getStatusbarLC
    statIO <- getStatusbarIO
    let (ind,rbn) = figureOutBufferName bufs bn 0
    buf <- lift $ do
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
        textBufferSetModified buffer False
        sourceBufferEndNotUndoableAction buffer
        siter <- textBufferGetStartIter buffer
        textBufferPlaceCursor buffer siter
        sourceBufferSetHighlight buffer True

        -- create a new SourceView Widget
        sv <- sourceViewNewWithBuffer buffer
        f <- fontDescriptionNew
        fontDescriptionSetFamily f "Monospace"
        widgetModifyFont sv (Just f)
        sourceViewSetShowLineNumbers sv True
        sourceViewSetMargin sv 90
        sourceViewSetShowMargin sv True
        sourceViewSetInsertSpacesInsteadOfTabs sv True
        sourceViewSetTabsWidth sv tabWidth
        sourceViewSetSmartHomeEnd sv True

        -- put it in a scrolled window
        sw <- scrolledWindowNew Nothing Nothing
        sw `containerAdd` sv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        sw `scrolledWindowSetShadowType` ShadowIn

        notebookPrependPage nb sw rbn
        mbPn <- notebookPageNum nb sw
        widgetShowAll nb
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"

        -- statusbars  
        statusbarPush statLC 1 ""
        
{--        writeCursorPositionInStatusbar buffer statLC

        afterMoveCursor sv (\_ _ _ -> writeCursorPositionInStatusbar buffer statLC)
        afterEndUserAction buffer (writeCursorPositionInStatusbar buffer statLC)
        afterSwitchPage nb (\pn1 -> do  pn2 <- notebookPageNum nb sw;
                                        if isJust pn2 && pn1 == fromJust pn2
                                            then writeCursorPositionInStatusbar buffer statLC
                                            else return ())
        widgetAddEvents sv [ButtonReleaseMask]
        onButtonRelease sv (\ _ -> do  writeCursorPositionInStatusbar buffer statLC;
                                       return False)
        afterModifiedChanged buffer (markLabelAsChanged buffer sw)

        statusbarPush statIO 1 "INS"
        afterToggleOverwrite sv (writeOverwriteInStatusbar sv sw statIO)
        afterSwitchPage nb (\ _ -> writeOverwriteInStatusbar sv sw statIO)--}
        return (GhfBuffer mbfn bn ind sv sw p)
    modifyGhf_ (\ghf -> return (ghf{buffers = Map.insert rbn buf bufs,
                                    mbActiveBuf = Just buf}))

{--
writeCursorPositionInStatusbar :: SourceBuffer -> Statusbar -> IO()
writeCursorPositionInStatusbar buf stat = do
    modi <- textBufferGetModified buf
    mark <- textBufferGetInsert buf
    iter <- textBufferGetIterAtMark buf mark
    line <- textIterGetLine iter
    col  <- textIterGetLineOffset iter
    statusbarPop stat 1
    statusbarPush stat 1 $printf "Ln %4d, Col %3d" (line + 1) (col + 1)
    return ()

writeOverwriteInStatusbar :: SourceView -> Notebook -> ScrolledWindow -> Statusbar -> IO()
writeOverwriteInStatusbar sv nb sw stat = do
    i <- notebookGetCurrentPage nb
    i2 <- notebookPageNum nb sw
    if isJust i2 && i == fromJust i2
        then do
            modi <- textViewGetOverwrite sv
            statusbarPop stat 1
            statusbarPush stat 1 $if modi then "OVR" else "INS"
            return () 
        else return ()

markLabelAsChanged buf sw = do
    
    modified <- textBufferGetModified buf
    (Just text) <- notebookGetTabLabelText nb sw
    label <- labelNew Nothing
    labelSetUseMarkup label True
    labelSetMarkup label   
        (if modified 
            then "<span foreground=\"red\">" ++ text ++ "</span>" 
            else text) 
    notebookSetTabLabel nb sw label  
--}

inBufContext' :: a -> (Notebook -> TextBuffer -> GhfBuffer -> Int -> GhfM a) -> GhfM a
inBufContext' def f = do
    mbBuf <- readGhf mbActiveBuf
    case mbBuf of
        Nothing -> return def
        Just ghfBuf -> do
            nb <- getNotebook (pane ghfBuf)
            mbI  <- lift $notebookPageNum nb (scrolledWindow ghfBuf)
            case mbI of
                Nothing -> lift $ do
                        putStrLn "notebook page not found: unexpected"
                        return def
                Just i -> do
                        gtkbuf <- lift $ textViewGetBuffer (sourceView ghfBuf)
                        f nb gtkbuf ghfBuf i

inBufContext :: a -> (Notebook -> TextBuffer -> GhfBuffer -> Int -> IO a) -> GhfM a
inBufContext def f = inBufContext' def (\a b c d-> lift $ f a b c d)

fileSave :: Bool -> GhfAction
fileSave query = inBufContext' () $ \nb _ currentBuffer i -> do
    window  <- readGhf window
    bufs    <- readGhf buffers
    mbnbufs <- lift $ do
        let mbfn = fileName currentBuffer
        mbpage <- notebookGetNthPage nb i
        case mbpage of
            Nothing -> error "fileSave: Page not found"
            Just page -> 
                if isJust mbfn && query == False
                    then do fileSave' currentBuffer $fromJust mbfn
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
                        widgetHide dialog
                        mbFileName <- case response of
                                ResponseAccept ->       fileChooserGetFilename dialog
                                ResponseCancel ->       return Nothing
                                ResponseDeleteEvent->   return Nothing
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
                                        fileSave' currentBuffer fn
                                        let bn = takeFileName fn
                                        let bufs1 =  Map.delete (realBufferName currentBuffer) bufs
                                        let (ind,rbn) =  figureOutBufferName bufs1 bn 0
                                        let newBuffer =  currentBuffer {fileName = Just fn,
                                                        bufferName = bn, addedIndex = ind}
                                        let newBufs   =  Map.insert rbn newBuffer bufs1
                                        label <- labelNew (Just rbn)
                                        notebookSetTabLabel nb page label
                                        return (Just newBufs)
                                    ResponseNo -> return Nothing
    case mbnbufs of
        Just nbufs ->modifyGhf_ (\ghf -> return (ghf{buffers = nbufs}))
        Nothing -> return ()
    where
        fileSave' :: GhfBuffer -> FileName -> IO()
        fileSave' ghfBuf fn = do
            buf     <- textViewGetBuffer $ sourceView ghfBuf
            start   <- textBufferGetStartIter buf
            end     <- textBufferGetEndIter buf
            text    <- textBufferGetText buf start end True
            writeFile fn text
            textBufferSetModified buf False

fileNew :: GhfAction
fileNew = newTextBuffer "Unnamed" Nothing

fileClose :: GhfM Bool
fileClose = inBufContext' False $ \nb gtkbuf currentBuffer i -> do
    ghfRef  <- ask
    window  <- readGhf window
    bufs    <- readGhf buffers
    mbbuf <- lift $ do
        modified <- textBufferGetModified gtkbuf
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsNone
                                            ("Save changes to document: "
                                                ++ realBufferName currentBuffer
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
            modifyGhf_ (\ghf -> return (ghf{buffers = Map.delete (realBufferName currentBuffer) bufs}))
            setNewActiveBuffer nb
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
        widgetHide dialog
        case response of
            ResponseAccept ->       fileChooserGetFilename dialog
            ResponseCancel ->       return Nothing
            ResponseDeleteEvent->   return Nothing
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
editShiftLeft = 
    let str = map (\_->' ') [1 ..tabWidth] in
    do  b <- canShiftLeft str 
        if b
            then do
                doForSelectedLines [] $ \gtkbuf iter lineNr -> do
                    textIterSetLine iter lineNr
                    iter2 <- textIterCopy iter
                    textIterForwardChars iter tabWidth         
                    textBufferDelete gtkbuf iter iter2
                return ()                
            else return ()
        where
        canShiftLeft str = do 
            boolList <- doForSelectedLines [] $ \gtkbuf iter lineNr -> do
                textIterSetLine iter lineNr
                iter2 <- textIterCopy iter
                textIterForwardChars iter tabWidth         
                str1 <- textIterGetText iter iter2
                return (str1 == str)
            return (foldl (&&) True boolList)
            
            
editShiftRight :: GhfAction
editShiftRight = 
    let str = map (\_->' ') [1 ..tabWidth] in do
        doForSelectedLines [] $ \gtkbuf iter lineNr -> do
            textIterSetLine iter lineNr
            textBufferInsert gtkbuf iter str
        return ()
