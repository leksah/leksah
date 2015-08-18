{-# LANGUAGE ViewPatterns #-}
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

,   updateStyle
,   updateStyle'
,   addLogRef
,   removeLogRefs
,   removeBuildLogRefs
,   removeTestLogRefs
,   removeLintLogRefs
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
,   belongsToPackage
,   belongsToWorkspace
,   getIdentifierUnderCursorFromIter
,   useCandyFor

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
import qualified Graphics.UI.Gtk as Gtk hiding (eventKeyName)
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), PolicyType(..))
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..))
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
import Graphics.UI.Gtk.Selectors.FileChooser
       (FileChooserAction(..))
import System.Glib.Attributes (AttrOp(..), set)

import IDE.BufferMode
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (filterM, void, unless, when, liftM)
import Control.Exception as E (catch, SomeException)

import qualified IDE.Command.Print as Print
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Log.Logger (debugM)
import Data.Text (Text)
import qualified Data.Text as T
       (length, findIndex, replicate, lines,
        dropWhileEnd, unlines, strip, null, pack, unpack)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (writeFile, readFile)
import Data.Time (UTCTime(..))
import Graphics.UI.Gtk.Gdk.EventM
       (eventModifier, eventKeyName, eventKeyVal)
import qualified Data.Foldable as F (Foldable(..), forM_, toList)
import Data.Traversable (forM)
import Language.Haskell.HLint3 (Idea(..))
-- import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewR(..), (|>))

--time :: MonadIO m => String -> m a -> m a
--time name action = do
--  liftIO . debugM "leksah" $ name <> " start"
--  start <- liftIO $ realToFrac <$> getPOSIXTime
--  result <- action
--  end <- liftIO $ realToFrac <$> getPOSIXTime
--  liftIO . debugM "leksah" $ name <> " took " <> show ((end - start) * 1000000) <> "us"
--  return result

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
                postAsyncIDEIdle $ do
                    liftIO $ debugM "leksah" "SourceBuffer recoverState idle callback"
                    gtkBuf  <- getBuffer v
                    iter    <- getIterAtOffset gtkBuf i
                    placeCursor gtkBuf iter
                    mark    <- getInsertMark gtkBuf
                    scrollToMark v mark 0.0 (Just (0.3,0.3))
                    liftIO $ debugM "leksah" "SourceBuffer recoverState done"
                return mbbuf
            Nothing -> return Nothing
    recoverState pp (BufferStateTrans bn text i) =   do
        mbbuf    <-  newTextBuffer pp bn Nothing
        case mbbuf of
            Just (buf@IDEBuffer {sourceView=v}) -> do
                postAsyncIDEIdle $ do
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

-- startComplete :: IDEAction
startComplete = do
    mbBuf <- maybeActiveBuf
    currentState' <- readIDE currentState
    case mbBuf of
        Nothing     -> return ()
        Just (IDEBuffer {sourceView=v}) -> complete v True

-- selectSourceBuf :: FilePath -> IDEM (Maybe IDEBuffer)
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
                    ideMessage Normal (__ "File path not found " <> T.pack fpc)
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
        (Just packagePath, Just loc, _) -> void (goToSourceDefinition (dropFileName packagePath) loc)
        (_, Just loc, Just sourcePath)  -> void (goToSourceDefinition' sourcePath loc)
        (_, _, Just sp) -> void (selectSourceBuf sp)
        _  -> return ()
  where
    packagePathFromScope :: GenScope -> Maybe FilePath
    packagePathFromScope (GenScopeC (PackScope l _)) =
        case dscMbModu idDescr of
            Just mod -> case pack mod `Map.lookup` l of
                            Just pack -> pdMbSourcePath pack
                            Nothing   -> Nothing
            Nothing -> Nothing

    sourcePathFromScope :: GenScope -> Maybe FilePath
    sourcePathFromScope (GenScopeC (PackScope l _)) =
        case dscMbModu idDescr of
            Just mod -> case pack mod `Map.lookup` l of
                            Just pack ->
                                case filter (\md -> mdModuleId md == fromJust (dscMbModu idDescr))
                                                    (pdModules pack) of
                                    (mod : tl) ->  mdMbSourcePath mod
                                    []         -> Nothing
                            Nothing -> Nothing
            Nothing -> Nothing

goToSourceDefinition :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition packagePath loc =
    goToSourceDefinition' (packagePath </> locationFile loc) loc

goToSourceDefinition' :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition' sourcePath Location{..} = do
    mbBuf     <- selectSourceBuf sourcePath
    case mbBuf of
        Just buf ->
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
                -- ### we had a problem before using postAsyncIDEIdle
                postAsyncIDEIdle $ do
                    liftIO $ debugM "lekash" "goToSourceDefinition triggered selectRange"
                    selectRange ebuf iter iter2
                    liftIO $ debugM "lekash" "goToSourceDefinition triggered scrollToIter"
                    scrollToIter sv iter 0.0 (Just (0.3,0.3))
                return ()
        Nothing -> return ()
    return mbBuf

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

updateStyle' :: IDEBuffer -> IDEAction
updateStyle' IDEBuffer {sourceView = sv} = getBuffer sv >>= updateStyle

removeLogRefs :: (FilePath -> FilePath -> Bool) -> [LogRefType] -> IDEAction
removeLogRefs toRemove' types = do
    (remove, keep) <- Seq.partition toRemove <$> readIDE allLogRefs
    let removeDetails = Map.fromListWith (<>) . nub $ map (\ref ->
                            (logRefRootPath ref </> logRefFilePath ref,
                            [logRefType ref])) $ F.toList remove
    modifyIDE_ (\ide -> ide{allLogRefs = keep})

    buffers <- allBuffers
    let matchingBufs = filter (maybe False (`Map.member` removeDetails) . fileName) buffers
    F.forM_ matchingBufs $ \ (IDEBuffer {..}) -> do
        buf <- getBuffer sourceView
        F.forM_ (maybe [] (fromMaybe [] . (`Map.lookup` removeDetails)) fileName) $
            removeTagByName buf . T.pack . show

    triggerEventIDE (ErrorChanged False)
    return ()
  where
    toRemove ref = toRemove' (logRefRootPath ref) (logRefFilePath ref)
                && logRefType ref `elem` types

removeFileLogRefs :: FilePath -> FilePath -> [LogRefType] -> IDEAction
removeFileLogRefs root file types = do
    liftIO . debugM "leksah" $ "removeFileLogRefs " <> root <> " " <> file <> " " <> show types
    removeLogRefs (\r f -> r == root && f == file) types

removePackageLogRefs :: FilePath -> [LogRefType] -> IDEAction
removePackageLogRefs root types = do
    liftIO . debugM "leksah" $ "removePackageLogRefs " <> root <> " " <> show types
    removeLogRefs (\r _ -> r == root) types

removeBuildLogRefs :: FilePath -> FilePath -> IDEAction
removeBuildLogRefs root file = removeFileLogRefs root file [ErrorRef, WarningRef]

removeTestLogRefs :: FilePath -> IDEAction
removeTestLogRefs root = removePackageLogRefs root [TestFailureRef]

removeLintLogRefs :: FilePath -> FilePath -> IDEAction
removeLintLogRefs root file = removeFileLogRefs root file [LintRef]

canResolve :: LogRef -> Bool
canResolve LogRef { logRefIdea = Just (_, Idea{..}) }
    = ideaHint /= "Reduce duplication" && isJust ideaTo
canResolve _ = False

addLogRef :: Bool -> Bool -> LogRef -> IDEAction
addLogRef hlintFileScope backgroundBuild ref = do
    liftIO . debugM "leksah" $ "addLogRef " <> show hlintFileScope <> " " <> show (logRefType ref) <> " " <> logRefFullFilePath ref
    -- Put most important errors first.
    -- If the importance of two errors is the same then
    -- then the older one might be stale (unless it is in the same file)
    allLogRefs   <- readIDE allLogRefs
    currentError <- readIDE currentError
    let (moreImportant, rest) =
           Seq.spanl (\old ->
                let samePackage = logRefRootPath old     == logRefRootPath ref
                    sameFile    = logRefFullFilePath old == logRefFullFilePath ref in
                -- Work out when the old ref is more important than the new
                case (logRefType ref, logRefType old) of
                    (ErrorRef      , ErrorRef      ) -> sameFile
                    (ErrorRef      , _             ) -> False
                    (WarningRef    , ErrorRef      ) -> samePackage
                    (WarningRef    , WarningRef    ) -> samePackage
                    (WarningRef    , _             ) -> False
                    (TestFailureRef, ErrorRef      ) -> samePackage  -- Probably should never be True
                    (TestFailureRef, TestFailureRef) -> samePackage
                    (TestFailureRef, _             ) -> False
                    (LintRef       , LintRef       ) -> (if hlintFileScope then sameFile else samePackage)
                                                            && (canResolve old
                                                               || not (canResolve ref))
                    (LintRef       , _             ) -> samePackage
                    (ContextRef    , _             ) -> False
                    (BreakpointRef , _             ) -> False) allLogRefs
        currErr = if currentError `elem` map Just (F.toList moreImportant)
                        then currentError
                        else Nothing
    modifyIDE_ $ \ ide ->
        ide{ allLogRefs = (moreImportant |> ref) <> rest
           , currentEBC = (currErr, currentBreak ide, currentContext ide)
           }

    buffers <- allBuffers
    let matchingBufs = filter (maybe False (equalFilePath (logRefFullFilePath ref)) . fileName) buffers
    F.forM_ matchingBufs $ \ buf -> markRefInSourceBuf buf ref False

    triggerEventIDE $ ErrorAdded
        (not backgroundBuild && Seq.null moreImportant) (Seq.length moreImportant) ref
    return ()

markRefInSourceBuf :: IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf buf logRef scrollTo = do
    useCandy    <- useCandyFor buf
    candy'      <- readIDE candy
    contextRefs <- readIDE contextRefs
    prefs       <- readIDE prefs
    inBufContext () buf $ \_ sv ebuf buf _ -> do
        let tagName = T.pack $ show (logRefType logRef)
        liftIO . debugM "lekash" . T.unpack $ "markRefInSourceBuf getting or creating tag " <> tagName

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
        iterTmp <-  getIterAtLine ebuf (max 0 (min (lines-1) (fst start - 1)))
        chars   <-  getCharsInLine iterTmp
        iter    <- atLineOffset iterTmp (max 0 (min (chars-1) (snd start)))

        iter2 <- if start == end
            then do
                maybeWE <- forwardWordEndC iter
                case maybeWE of
                    Nothing -> atEnd iter
                    Just we -> return we
            else do
                newTmp  <- getIterAtLine ebuf (max 0 (min (lines-1) (fst end - 1)))
                chars   <- getCharsInLine newTmp
                new     <- atLineOffset newTmp (max 0 (min (chars-1) (snd end)))
                forwardCharC new

        let last (Seq.viewr -> EmptyR)  = Nothing
            last (Seq.viewr -> xs :> x) = Just x
            last _                      = Nothing
            latest = last contextRefs
            isOldContext = case (logRefType logRef, latest) of
                                (ContextRef, Just ctx) | ctx /= logRef -> True
                                _ -> False
        unless isOldContext $ do
            liftIO $ debugM "lekash" "markRefInSourceBuf calling applyTagByName"
            lineStart <- backwardToLineStartC iter
            createMark sv (logRefType logRef) lineStart $ refDescription logRef
            applyTagByName ebuf tagName iter iter2
        when scrollTo . postAsyncIDE $ do
            liftIO $ debugM "lekash" "markRefInSourceBuf triggered placeCursor"
            placeCursor ebuf iter
            mark <- getInsertMark ebuf
            liftIO $ debugM "lekash" "markRefInSourceBuf trigged scrollToMark"
            scrollToMark sv mark 0.3 Nothing
            when isOldContext $ selectRange ebuf iter iter2

-- | Tries to create a new text buffer, fails when the given filepath
-- does not exist or when it is not a text file.
newTextBuffer :: PanePath -> Text -> Maybe FilePath -> IDEM (Maybe IDEBuffer)
newTextBuffer panePath bn mbfn =
     case mbfn of
            Nothing -> buildPane "" Nothing
            Just fn ->
                do eErrorContents <- liftIO $
                                         catch (Right <$> T.readFile fn)
                                               (\e -> return $ Left (show (e :: IOError)))
                   case eErrorContents of
                       Right contents -> do
                           modTime  <- liftIO $ getModificationTime fn
                           buildPane contents (Just modTime)
                       Left err       -> do
                           ideMessage Normal (__ "Error reading file " <> T.pack err)
                           return Nothing

    where buildPane contents mModTime = do
            nb      <-  getNotebook panePath
            prefs   <-  readIDE prefs
            bs      <-  getCandyState
            ct      <-  readIDE candy
            (ind,rbn) <- figureOutPaneName bn 0
            buildThisPane panePath nb (builder' bs mbfn ind bn rbn ct prefs contents mModTime)

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
    Text  ->
    Maybe UTCTime ->
    PanePath ->
    Gtk.Notebook ->
    Gtk.Window ->
    IDEM (Maybe IDEBuffer,Connections)
builder' bs mbfn ind bn rbn ct prefs fileContents modTime pp nb windows =
    -- display a file
    case textEditorType prefs of
        "GtkSourceView" -> newGtkBuffer mbfn fileContents >>= makeBuffer modTime
        "Yi"            -> newYiBuffer mbfn fileContents >>= makeBuffer modTime
        "CodeMirror"    -> newCMBuffer mbfn fileContents >>= makeBuffer modTime
        _               -> newDefaultBuffer mbfn fileContents >>= makeBuffer modTime

  where
    makeBuffer :: TextEditor editor => Maybe UTCTime -> EditorBuffer editor -> IDEM (Maybe IDEBuffer,Connections)
    makeBuffer modTime buffer = do
        ideR <- ask

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
        setTabWidth sv 8 -- GHC treats tabs as 8 we should display them that way
        drawTabs sv
        updateStyle buffer

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
        ids3 <- onButtonPress sv $ do
                click <- lift Gtk.eventClick
                liftIDE $
                    case click of
                        Gtk.DoubleClick -> do
                            (start, end) <- getIdentifierUnderCursor buffer
                            selectRange buffer start end
                            return True
                        _ -> return False

        (GetTextPopup mbTpm) <- triggerEvent ideR (GetTextPopup Nothing)
        ids4 <- case mbTpm of
            Just tpm    -> sv `onPopulatePopup` \menu -> liftIO $ tpm ideR menu
            Nothing     -> do
                sysMessage Normal "SourceBuffer>> no text popup"
                return []

        hasMatch <- liftIO $ newIORef False
        ids5 <- onSelectionChanged buffer $ do
            (iStart, iEnd) <- getSelectionBounds buffer
            lStart <- (+1) <$> getLine iStart
            cStart <- getLineOffset iStart
            lEnd <- (+1) <$> getLine iEnd
            cEnd <- getLineOffset iEnd
            triggerEventIDE_ . SelectSrcSpan $
                case mbfn of
                    Just fn -> Just (SrcSpan fn lStart cStart lEnd cEnd)
                    Nothing -> Nothing
            let tagName = "match"
            hasSelection <- hasSelection buffer
            m <- liftIO $ readIORef hasMatch
            when m $ removeTagByName buffer tagName
            r <- if hasSelection
                    then do
                        candy'    <- readIDE candy
                        sTxt      <- getCandylessPart candy' buffer iStart iEnd
                        let strippedSTxt = T.strip sTxt
                        if T.null strippedSTxt
                            then return False
                            else do
                                bi1 <- getStartIter buffer
                                bi2 <- getEndIter buffer
                                r1 <- forwardApplying bi1 strippedSTxt (Just iStart) tagName buffer
                                r2 <- forwardApplying iEnd strippedSTxt (Just bi2) tagName buffer
                                return (r1 || r2)
                    else return False
            liftIO $ writeIORef hasMatch r
            return ()

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
                    _ ->
                        -- liftIO $ print ("sourcebuffer key:",name,modifier,keyval)
                        return False
        ids7 <- do
            ideR <- ask
            sw <- getScrolledWindow sv
            createHyperLinkSupport sv sw (\ctrl shift iter -> do
                (beg, en) <- getIdentifierUnderCursorFromIter (iter, iter)
                return (beg, if ctrl then en else beg)) (\_ shift' slice ->
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
                    -> IDEM Bool
    forwardApplying tI txt mbTi tagName ebuf = do
        mbFTxt <- forwardSearch tI txt [TextSearchVisibleOnly, TextSearchTextOnly] mbTi
        case mbFTxt of
            Just (start, end) -> do
                startsWord <- startsWord start
                endsWord <- endsWord end
                when (startsWord && endsWord) $
                    applyTagByName ebuf tagName start end
                (|| (startsWord && endsWord)) <$> forwardApplying end txt mbTi tagName ebuf
            Nothing -> return False

isIdent a = isAlphaNum a || a == '\'' || a == '_'       -- parts of haskell identifiers

isRangeStart sel = do                                   -- if char and previous char are of different char categories
    currentChar <- getChar sel
    let mbStartCharCat = getCharacterCategory currentChar
    mbPrevCharCat <- getCharacterCategory <$> (backwardCharC sel >>= getChar)
    return $ isNothing currentChar || currentChar == Just '\n' || mbStartCharCat /= mbPrevCharCat && (mbStartCharCat == SyntaxCharacter || mbStartCharCat == IdentifierCharacter)

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
                                    if nmt /= mt -- Fonts get messed up under windows when adding this line.
                                                  -- Praises to whoever finds out what happens and how to fix this
                                    then do
                                        load <- readIDE (autoLoad . prefs)
                                        if load
                                            then do
                                                ideMessage Normal $ __ "Auto Loading " <> T.pack fn
                                                revert buf
                                                return (False, True)
                                            else do
                                                window <- liftIDE getMainWindow
                                                resp <- liftIO $ do
                                                    md <- messageDialogNew
                                                            (Just window) []
                                                            MessageQuestion
                                                            ButtonsNone
                                                            (__ "File \"" <> name <> __ "\" has changed on disk.")
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
                                                    _              -> return (False, False)
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
fileRevert = inActiveBufContext () $ \ _ _ _ currentBuffer _ ->
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
            when useCandy $
                modeTransformToCandy (mode buf)
                    (modeEditInCommentOrString (mode buf))
                    buffer
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
          F.forM_ mbBS (markLabelAsChanged nb)

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
            if isJust mbfn && not query
                then do (modifiedOnDiskNotLoaded, modifiedOnDisk) <- checkModTime ideBuf -- The user is given option to reload
                        modifiedInBuffer <- getModified ebuf
                        if modifiedOnDiskNotLoaded || modifiedInBuffer
                            then do
                                fileSave' (forceLineEnds prefs) (removeTBlanks prefs) nb ideBuf
                                    useCandy candy $fromJust mbfn
                                setModTime ideBuf
                                return True
                            else return modifiedOnDisk
                else reifyIDE $ \ideR   ->  do
                    dialog <- fileChooserDialogNew
                                    (Just $ __ "Save File")
                                    (Just window)
                                FileChooserActionSave
                                [("gtk-cancel"     --buttons to display
                                ,ResponseCancel)  --you can use stock buttons
                                ,("gtk-save"
                                , ResponseAccept)]
                    case mbfn of
                        Just fn -> void (fileChooserSelectFilename dialog fn)
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
            buf     <-   getBuffer sv
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
            triggerEventIDE_ $ SavedFile fn

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
fileClose = inActiveBufContext True fileClose'

fileClose' :: TextEditor editor => Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int  -> IDEM Bool
fileClose' nb _ ebuf currentBuffer i = do
    window  <- getMainWindow
    modified <- getModified ebuf
    cancel <- reifyIDE $ \ideR   ->
        if modified
            then do
                md <- messageDialogNew (Just window) []
                                            MessageQuestion
                                            ButtonsCancel
                                            (__ "Save changes to document: "
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
            F.forM_ (fileName currentBuffer) addRecentlyUsedFile
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
    mbActivePath    <-  fmap ipdBuildDir <$> readIDE activePack
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
    when (not (null bufs) && isJust mbWorkspace) $
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
            in  any (`isSubPath` fileName) paths


fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbBuf <- maybeActiveBuf
    mbFileName <- liftIO $ do
        dialog <- fileChooserDialogNew
                        (Just $ __ "Open File")
                        (Just window)
                    FileChooserActionOpen
                    [("gtk-cancel"
                    ,ResponseCancel)
                    ,("gtk-open"
                    ,ResponseAccept)]
        case mbBuf >>= fileName of
            Just fn -> void (fileChooserSetCurrentFolder dialog (dropFileName fn))
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
    F.forM_ mbFileName fileOpenThis


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
filePrint = inActiveBufContext () filePrint'

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
            ResponseYes     ->   return True
            ResponseCancel  ->   return False
            ResponseNo      ->   return False
            _               ->   return False
    when print $ do
        --real code
        modified <- getModified ebuf
        cancel <- reifyIDE $ \ideR ->
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
        unless cancel $
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
    when b $ do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol (tabWidth prefs)
            delete ebuf sol sol2
        return ()
    where
    canShiftLeft str prefs = do
        boolList <- doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol (tabWidth prefs)
            str1 <- getText ebuf sol sol2 True
            return (str1 == str)
        return (F.foldl' (&&) True boolList)


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
    let alignTo = F.foldl' max 0 (mapMaybe snd positions)
    when (alignTo > 0) $ alignChar (Map.fromList positions) alignTo
    where
    positionsOfChar :: IDEM [(Int, Maybe Int)]
    positionsOfChar = doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            eol <- forwardToLineEndC sol
            line  <- getText ebuf sol eol True
            return (lineNr, T.findIndex (==char) line)
    alignChar :: Map Int (Maybe Int) -> Int -> IDEM ()
    alignChar positions alignTo = do
            doForSelectedLines [] $ \ebuf lineNr ->
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
    unless (isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        unless (fp `elem` recentFiles') $
            modifyIDE_ (\ide -> ide{recentFiles = take 12 (fp : recentFiles')})
        triggerEventIDE UpdateRecent
        return ()

removeRecentlyUsedFile :: FilePath -> IDEAction
removeRecentlyUsedFile fp = do
    state <- readIDE currentState
    unless (isStartingOrClosing state) $ do
        recentFiles' <- readIDE recentFiles
        when (fp `elem` recentFiles') $
            modifyIDE_ (\ide -> ide{recentFiles = filter (/= fp) recentFiles'})
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
        Just <$> getCandylessPart candy' ebuf i1 i2

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
            insert ebuf i realString
            (_,i1)     <- getSelectionBounds ebuf
            i2         <- forwardCharsC i1 (T.length realString)
            selectRange ebuf i1 i2

-- | Returns the packages to which this buffer belongs
--   uses the 'bufferProjCache' and might extend it
belongsToPackages :: MonadIDE m => IDEBuffer -> m [IDEPackage]
belongsToPackages IDEBuffer{fileName = Just fp}= do
    bufferToProject' <-  readIDE bufferProjCache
    ws               <-  readIDE workspace
    case Map.lookup fp bufferToProject' of
        Just p  -> return p
        Nothing -> case ws of
                        Nothing   -> return []
                        Just workspace -> do
                            let res = filter (belongsToPackage fp) (wsPackages workspace)
                            modifyIDE_ (\ide -> ide{bufferProjCache = Map.insert fp res bufferToProject'})
                            return res
belongsToPackages _ = return []

-- | Checks whether a file belongs to a package (includes files in 
-- sandbox source dirs)
belongsToPackage :: FilePath -> IDEPackage -> Bool
belongsToPackage f = any (`isSubPath` f) . ipdAllDirs

-- | Checks whether a file belongs to the workspace
belongsToWorkspace :: MonadIDE m => IDEBuffer -> m Bool
belongsToWorkspace b =  liftM (not . null) (belongsToPackages b)

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


