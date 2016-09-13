{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
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
,   switchBuffersCandy

,   updateStyle
,   updateStyle'
,   addLogRef
,   removeLogRefs
,   removeBuildLogRefs
,   removeFileExtLogRefs
,   removeTestLogRefs
,   removeLintLogRefs
,   markRefInSourceBuf
,   inBufContext
,   inActiveBufContext

,   align
,   startComplete

,   selectedText
,   selectedTextOrCurrentLine
,   selectedTextOrCurrentIdentifier
,   insertTextAfterSelection
,   selectedModuleName
,   selectedLocation
,   recentSourceBuffers
,   newTextBuffer
,   belongsToPackages
,   belongsToPackages'
,   belongsToPackage
,   belongsToWorkspace
,   belongsToWorkspace'
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
import IDE.Utils.DirectoryUtils
import IDE.SourceCandy
import IDE.SymbolNavigation
import IDE.Completion as Completion (complete,cancel)
import IDE.TextEditor
import Data.IORef (writeIORef,readIORef,newIORef)
import Control.Event (triggerEvent)
import IDE.Metainfo.Provider (getSystemInfo, getWorkspaceInfo)
import IDE.BufferMode
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (filterM, void, unless, when, liftM, forM_)
import Control.Exception as E (catch, SomeException)

import qualified IDE.Command.Print as Print
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Log.Logger (errorM, warningM, debugM)
import Data.Text (Text)
import qualified Data.Text as T
       (length, findIndex, replicate, lines,
        dropWhileEnd, unlines, strip, null, pack, unpack)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (writeFile, readFile)
import Data.Time (UTCTime(..))
import qualified Data.Foldable as F (Foldable(..), forM_, toList)
import Data.Traversable (forM)
import Language.Haskell.HLint3 (Idea(..))
-- import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewR(..), (|>))
import Data.Time.Clock (addUTCTime, diffUTCTime)
import qualified GI.Gtk.Objects.Notebook as Gtk (Notebook(..))
import qualified GI.Gtk.Objects.Window as Gtk (Window(..))
import GI.Gtk.Objects.ScrolledWindow
       (setScrolledWindowShadowType, scrolledWindowSetPolicy)
import GI.Gtk.Enums
       (FileChooserAction(..), WindowPosition(..), ResponseType(..),
        ButtonsType(..), MessageType(..), ShadowType(..), PolicyType(..))
import GI.Gdk.Structs.EventKey
       (getEventKeyState, getEventKeyKeyval)
import GI.Gdk.Functions (keyvalName)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gtk.Flags (TextSearchFlags(..))
import GI.Gtk.Objects.MessageDialog
       (setMessageDialogText, constructMessageDialogButtons, setMessageDialogMessageType,
        MessageDialog(..))
import GI.Gtk.Objects.Dialog
       (dialogRun, constructDialogUseHeaderBar)
import GI.Gtk.Objects.Window
       (setWindowTitle, setWindowWindowPosition, windowSetTransientFor)
import Data.GI.Base (nullToNothing, new')
import GI.Gtk.Objects.Widget
       (widgetHide, widgetShow, widgetDestroy)
import GI.Gtk.Objects.Notebook
       (notebookPageNum, notebookGetNthPage, Notebook(..))
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, fileChooserSelectFilename,
        fileChooserSetAction)
import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', dialogAddButton')
import GI.Gtk.Objects.Clipboard (clipboardGet)
import GI.Gdk.Structs.Atom (atomIntern)
import GI.Gdk.Structs.EventButton (getEventButtonType)
import GI.Gdk.Enums (EventType(..))
import GI.Gtk
       (boxPackStart, vBoxNew, Container(..),
        containerAdd, infoBarGetContentArea,
        labelNew, infoBarNew)
import Data.GI.Base.ManagedPtr (unsafeCastTo)

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
                    scrollToMark v mark 0.0 (Just (1.0,0.3))
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
                    scrollToMark v mark 0.0 (Just (1.0,0.3))
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
                    scrollToIter sv iter 0.0 (Just (1.0,0.3))
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

removeFileExtLogRefs :: FilePath -> String -> [LogRefType] -> IDEAction
removeFileExtLogRefs root fileExt types = do
    liftIO . debugM "leksah" $ "removeFileTypeLogRefs " <> root <> " " <> fileExt <> " " <> show types
    removeLogRefs (\r f -> r == root && takeExtension f == fileExt) types

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
        when scrollTo $ do
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
            let useCandy = candyState prefs
            ct      <-  readIDE candy
            (ind,rbn) <- figureOutPaneName bn 0
            buildThisPane panePath nb (builder' useCandy mbfn ind bn rbn ct prefs contents mModTime)

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
builder' useCandy mbfn ind bn rbn ct prefs fileContents modTime pp nb windows =
    case textEditorType prefs of
        "GtkSourceView" -> newGtkBuffer mbfn fileContents >>= makeBuffer modTime
        "Yi"            -> newYiBuffer mbfn fileContents >>= makeBuffer modTime
        "CodeMirror"    -> newCMBuffer mbfn fileContents >>= makeBuffer modTime
        _               -> newDefaultBuffer mbfn fileContents >>= makeBuffer modTime

  where
    makeBuffer :: TextEditor editor => Maybe UTCTime -> EditorBuffer editor -> IDEM (Maybe IDEBuffer,Connections)
    makeBuffer modTime buffer = do
        liftIO $ debugM "lekash" "makeBuffer"
        ideR <- ask

        beginNotUndoableAction buffer
        let mode = modeFromFileName mbfn
        when (useCandy && isHaskellMode mode) $ modeTransformToCandy mode
                                                    (modeEditInCommentOrString mode) buffer
        endNotUndoableAction buffer
        setModified buffer False
        siter <- getStartIter buffer
        placeCursor buffer siter
        iter <- getEndIter buffer

        -- create a new SourceView Widget
        sv <- newView buffer (textviewFont prefs)

        -- Files opened from the unpackDirectory are meant for documentation
        -- and are not actually a source dependency, they should not be editable.
        let isEditable = fromMaybe True $ do
                            dir  <- unpackDirectory prefs
                            file <- mbfn
                            return (not $ (splitDirectories dir) `isPrefixOf` (splitDirectories file))
        setEditable sv isEditable
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
            then scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
            else scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
        liftIO $ debugM "lekash" "makeBuffer setScrolledWindowShadowType"
        setScrolledWindowShadowType sw ShadowTypeIn
        liftIO $ debugM "lekash" "makeBuffer setScrolledWindowShadowType done"


        box <- vBoxNew False 0
        when (not isEditable) $ liftIO $ do
            bar <- infoBarNew
            lab <- labelNew (Just "This file is opened in read-only mode because it comes from a non-local package")
            area <- infoBarGetContentArea bar >>= unsafeCastTo Container
            containerAdd area lab
            -- infoBarAddButton bar "Enable editing" (fromIntegral . fromEnum $ ResponseTypeReject)
            -- infoBarSetShowCloseButton bar True
            boxPackStart box bar False False 0
            widgetShow bar

        boxPackStart box sw True True 0

        modTimeRef <- liftIO $ newIORef modTime
        let buf = IDEBuffer {
            fileName =  mbfn,
            bufferName = bn,
            addedIndex = ind,
            sourceView =sv,
            vBox = box,
            modTime = modTimeRef,
            mode = mode}
        -- events
        ids1 <- afterFocusIn sv $ makeActive buf
        ids2 <- onCompletion sv (Completion.complete sv False) Completion.cancel
        ids3 <- onButtonPress sv $ do
                e <- lift ask
                click <- getEventButtonType e
                liftIDE $
                    case click of
                        EventType2buttonPress -> do
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

            let tagName = "selection-match"
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
            e        <- lift ask
            keyval   <- getEventKeyKeyval e
            name     <- keyvalName keyval
            modifier <- getEventKeyState e
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
                    (Just "Left",[ModifierTypeControlMask],_) -> do
                        calculateNewPosition backwardCharC >>= continueSelection False
                        return True
                    (Just "Left",[ModifierTypeShiftMask, ModifierTypeControlMask],_) -> do
                        calculateNewPosition backwardCharC >>= continueSelection True
                        return True
                    (Just "Right",[ModifierTypeControlMask],_) -> do
                        calculateNewPosition forwardCharC >>= continueSelection False --placeCursor buffer
                        return True
                    (Just "Right",[ModifierTypeControlMask, ModifierTypeControlMask],_) -> do
                        calculateNewPosition forwardCharC >>= continueSelection True
                        return True
                    (Just "BackSpace",[ModifierTypeControlMask],_) -> do              -- delete word
                        here <- getInsertIter buffer
                        there <- calculateNewPosition backwardCharC
                        delete buffer here there
                        return True
                    (Just "underscore",[ModifierTypeControlMask, ModifierTypeControlMask],_) -> do
                        (start, end) <- getIdentifierUnderCursor buffer
                        slice <- getSlice buffer start end True
                        triggerEventIDE (SelectInfo slice False)
                        return True
                        -- Redundant should become a go to definition directly
                    (Just "minus",[ModifierTypeControlMask],_) -> do
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
        mbFTxt <- forwardSearch tI txt [TextSearchFlagsVisibleOnly, TextSearchFlagsTextOnly] mbTi
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

-- | Get an iterator pair (start,end) delimiting the identifier currently under the cursor
getIdentifierUnderCursor :: forall editor. TextEditor editor => EditorBuffer editor -> IDEM (EditorIter editor, EditorIter editor)
getIdentifierUnderCursor buffer = do
    (startSel, endSel) <- getSelectionBounds buffer
    getIdentifierUnderCursorFromIter (startSel, endSel)

-- | Get an iterator pair (start,end) delimiting the identifier currently contained inside the provided iterator pair
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
                                                md <- new' MessageDialog [
                                                    constructDialogUseHeaderBar 0,
                                                    constructMessageDialogButtons ButtonsTypeNone]
                                                setMessageDialogMessageType md MessageTypeQuestion
                                                setMessageDialogText md (__ "File \"" <> name <> __ "\" has changed on disk.")
                                                windowSetTransientFor md (Just window)
                                                dialogAddButton' md (__ "_Load From Disk") (AnotherResponseType 1)
                                                dialogAddButton' md (__ "_Always Load From Disk") (AnotherResponseType 2)
                                                dialogAddButton' md (__ "_Don't Load") (AnotherResponseType 3)
                                                dialogSetDefaultResponse' md (AnotherResponseType 1)
                                                setWindowWindowPosition md WindowPositionCenterOnParent
                                                resp <- dialogRun' md
                                                widgetDestroy md
                                                case resp of
                                                    AnotherResponseType 1 -> do
                                                        revert buf
                                                        return (False, True)
                                                    AnotherResponseType 2 -> do
                                                        revert buf
                                                        modifyIDE_ $ \ide -> ide{prefs = (prefs ide) {autoLoad = True}}
                                                        return (False, True)
                                                    AnotherResponseType 3 -> dontLoad fn
                                                    ResponseTypeDeleteEvent -> dontLoad fn
                                                    _              -> return (False, False)

                                    else return (False, False)
                        else return (False, False)
                Nothing -> return (False, False)

    where
        dontLoad fn = do
            nmt2 <- liftIO $ getModificationTime fn
            liftIO $ writeIORef (modTime buf) (Just nmt2)
            return (True, True)
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
    liftIO $ debugM "leksah" "markLabelAsChanged"
    ebuf   <- getBuffer sv
    modified <- getModified ebuf
    w <- getTopWidget buf
    markLabel nb w modified

fileSaveBuffer :: MonadIDE m => TextEditor editor => Bool -> Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int -> m Bool
fileSaveBuffer query nb _ ebuf (ideBuf@IDEBuffer{sourceView = sv}) i = liftIDE $ do
    ideR    <- ask
    window  <- getMainWindow
    prefs   <- readIDE prefs
    useCandy <- useCandyFor ideBuf
    candy   <- readIDE candy
    (panePath,connects) <- guiPropertiesFromName (paneName ideBuf)
    let mbfn = fileName ideBuf
    page <- liftIO $ notebookGetNthPage nb (fromIntegral i)
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
            dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
            setWindowTitle dialog (__ "Save File")
            windowSetTransientFor dialog $ Just window
            fileChooserSetAction dialog FileChooserActionSave
            dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
            dialogAddButton' dialog "gtk-save" ResponseTypeAccept
            case mbfn of
                Just fn -> void (fileChooserSelectFilename dialog fn)
                Nothing -> return ()
            widgetShow dialog
            response <- dialogRun' dialog
            mbFileName <- case response of
                    ResponseTypeAccept      -> nullToNothing $ fileChooserGetFilename dialog
                    ResponseTypeCancel      -> return Nothing
                    ResponseTypeDeleteEvent -> return Nothing
                    _                       -> return Nothing
            widgetDestroy dialog
            case mbFileName of
                Nothing -> return False
                Just fn -> do
                    dfe <- doesFileExist fn
                    resp <- if dfe
                        then do md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 0,
                                    constructMessageDialogButtons ButtonsTypeCancel]
                                setMessageDialogMessageType md MessageTypeQuestion
                                setMessageDialogText md $ __ "File already exist."
                                windowSetTransientFor md (Just window)
                                dialogAddButton' md (__ "_Overwrite") ResponseTypeYes
                                dialogSetDefaultResponse' md ResponseTypeCancel
                                setWindowWindowPosition md WindowPositionCenterOnParent
                                resp <- toEnum . fromIntegral <$> dialogRun md
                                widgetHide md
                                return resp
                        else return ResponseTypeYes
                    case resp of
                        ResponseTypeYes -> do
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
            alreadyExists <- liftIO $ doesFileExist fn
            mbModTimeBefore <- if alreadyExists
                then liftIO $ Just <$> getModificationTime fn
                else return Nothing
            succ <- liftIO $ E.catch (do T.writeFile fn text'; return True)
                (\(e :: SomeException) -> do
                    sysMessage Normal . T.pack $ show e
                    return False)

            -- Truely horrible hack to work around HFS+ only having 1sec resolution
            -- and ghc ignoring files unless the modifiction time has moved forward.
            -- The limitation means we can do at most 1 reload a second, but
            -- this hack allows us to take an advance of up to 30 reloads (by
            -- moving the modidification time up to 30s into the future).
            modTimeChanged <- liftIO $ case mbModTimeBefore of
                Nothing -> return True
                Just modTime -> do
                    newModTime <- getModificationTime fn
                    let diff = diffUTCTime modTime newModTime
                    if
                        | (newModTime > modTime) -> return True -- All good mode time has moved on
                        | diff < 30 -> do
                             setModificationTimeOnOSX fn (addUTCTime 1 modTime)
                             updatedModTime <- getModificationTime fn
                             return (updatedModTime > modTime)
                        | diff < 32 -> do
                             -- Reached our limit of how far in the future we want to set the modifiction time.
                             -- Using 32 instead of 31 in case NTP or something is adjusting the clock back.
                             warningM "leksah" $ "Modification time for " <> fn
                                <> " was already " <> show (diffUTCTime modTime newModTime)
                                <> " in the future"
                             -- We still want to keep the modification time the same though.
                             -- If it went back the future date ghc has might cause it to
                             -- continue to ignore the file.
                             setModificationTimeOnOSX fn modTime
                             return False
                        | otherwise -> do
                             -- This should never happen unless something else is messing
                             -- with the modification time or the clock.
                             -- If it does happen we will leave the modifiction time alone.
                             errorM "leksah" $ "Modification time for " <> fn
                                <> " was already " <> show (diffUTCTime modTime newModTime)
                                <> " in the future"
                             return True

            -- Only consider the file saved if the modification time changed
            -- otherwise another save is really needed to trigger ghc.
            when modTimeChanged $ do
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
                md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeCancel]
                setMessageDialogMessageType md MessageTypeQuestion
                setMessageDialogText md $ __ "Save changes to document: "
                                                <> paneName currentBuffer
                                                <> "?"
                windowSetTransientFor md (Just window)
                dialogAddButton' md (__ "_Save") ResponseTypeYes
                dialogAddButton' md (__ "_Don't Save") ResponseTypeNo
                dialogSetDefaultResponse' md ResponseTypeYes
                setWindowWindowPosition md WindowPositionCenterOnParent
                resp <- dialogRun' md
                widgetDestroy md
                case resp of
                    ResponseTypeYes -> do
                        reflectIDE (fileSave False) ideR
                        return False
                    ResponseTypeCancel -> return True
                    ResponseTypeNo     -> return False
                    _                  -> return False
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
    mbActivePath    <-  fmap ipdPackageDir <$> readIDE activePack
    bufs            <-  allBuffers
    case mbActivePath of
        Just p -> mapM_ (close' p) bufs
        Nothing -> return ()
    where
        close' dir (buf@IDEBuffer {sourceView = sv}) = do
            (pane,_)    <-  guiPropertiesFromName (paneName buf)
            nb          <-  getNotebook pane
            i           <-  notebookPageNum nb (vBox buf)
            if i < 0
                then throwIDE (__ "notebook page not found: unexpected")
                else do
                    ebuf <- getBuffer sv
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPath dir (fromJust (fileName buf))))
                            $ do fileClose' nb sv ebuf buf (fromIntegral i); return ()

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
            i           <-  notebookPageNum nb (vBox buf)
            if i < 0
                then throwIDE (__ "notebook page not found: unexpected")
                else do
                    ebuf <- getBuffer sv
                    when (isJust (fileName buf)) $ do
                        modified <- getModified ebuf
                        when (not modified && not (isSubPathOfAny workspace (fromJust (fileName buf))))
                            $ do fileClose' nb sv ebuf buf (fromIntegral i); return ()
        isSubPathOfAny workspace fileName =
            let paths = wsPackages workspace >>= ipdAllDirs
            in  any (`isSubPath` fileName) paths


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
        hdb:_ -> do
            window <- getMainWindow
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeNone]
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md $ __ "Buffer already open."
            windowSetTransientFor md (Just window)
            dialogAddButton' md (__ "Make _Active") (AnotherResponseType 1)
            dialogAddButton' md (__ "_Open Second") (AnotherResponseType 2)
            dialogSetDefaultResponse' md (AnotherResponseType 1)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetDestroy md
            case resp of
                AnotherResponseType 2 -> reallyOpen prefs fpc
                _                     -> makeActive hdb
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
        md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeNone]
        setMessageDialogMessageType md MessageTypeQuestion
        setMessageDialogText md $ __"Print document: "
                                                <> pName
                                                <> "?"
        windowSetTransientFor md (Just window)
        dialogAddButton' md (__"_Print") ResponseTypeYes
        dialogSetDefaultResponse' md ResponseTypeYes
        dialogAddButton' md (__"_Don't Print") ResponseTypeNo
        setWindowWindowPosition md WindowPositionCenterOnParent
        resp <- dialogRun' md
        widgetDestroy md
        case resp of
            ResponseTypeYes     ->   return True
            ResponseTypeCancel  ->   return False
            ResponseTypeNo      ->   return False
            _                   ->   return False
    when print $ do
        --real code
        modified <- getModified ebuf
        cancel <- reifyIDE $ \ideR ->
            if modified
                then do
                    md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeNone]
                    setMessageDialogMessageType md MessageTypeQuestion
                    setMessageDialogText md $ __"Save changes to document: "
                                                    <> pName
                                                    <> "?"
                    windowSetTransientFor md (Just window)
                    dialogAddButton' md (__"_Save") ResponseTypeYes
                    dialogSetDefaultResponse' md ResponseTypeYes
                    dialogAddButton' md (__"_Don't Save") ResponseTypeNo
                    dialogAddButton' md (__"_Cancel Printing") ResponseTypeCancel
                    setWindowWindowPosition md WindowPositionCenterOnParent
                    resp <- dialogRun' md
                    widgetDestroy md
                    case resp of
                        ResponseTypeYes ->   do
                            reflectIDE (fileSave False) ideR
                            return False
                        ResponseTypeCancel  ->   return True
                        ResponseTypeNo      ->   return False
                        _               ->   return False
                else
                    return False
        unless cancel $
            case fileName currentBuffer of
                Just name -> do
                              status <- liftIO $ Print.print name
                              case status of
                                Left error -> liftIO $ showDialog (T.pack $ show error) MessageTypeError
                                Right _ -> liftIO $ showDialog "Print job has been sent successfully" MessageTypeInfo
                              return ()
                Nothing   -> return ()

editUndo :: IDEAction
editUndo = inActiveBufContext () $ \_ view buf _ _ -> do
    can <- canUndo buf
    when can $ do
        undo buf
        scrollToCursor view

editRedo :: IDEAction
editRedo = inActiveBufContext () $ \_ view buf _ _ -> do
    can <- canRedo buf
    when can $ redo buf
    scrollToCursor view

editDelete :: IDEAction
editDelete = inActiveBufContext ()  $ \_ view ebuf _ _ ->  do
    deleteSelection ebuf
    scrollToCursor view

editSelectAll :: IDEAction
editSelectAll = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    start <- getStartIter ebuf
    end   <- getEndIter ebuf
    selectRange ebuf start end

editCut :: IDEAction
editCut = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    clip <- clipboardGet =<< atomIntern "CLIPBOARD" False
    cutClipboard ebuf clip True

editCopy :: IDEAction
editCopy = inActiveBufContext () $ \_ view ebuf _ _ -> do
    clip <- clipboardGet =<< atomIntern "CLIPBOARD" False
    copyClipboard ebuf clip
    scrollToCursor view

editPaste :: IDEAction
editPaste = inActiveBufContext () $ \_ _ ebuf _ _ -> do
    mark <- getInsertMark ebuf
    iter <- getIterAtMark ebuf mark
    clip <- clipboardGet =<< atomIntern "CLIPBOARD" False
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

-- | Get the currently selected text or Nothing is no text is selected
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

-- | Get the currently selected text, or, if none, the current line text
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

-- | Get the currently selected text, or, if none, tries to selected the current identifier (the one under the cursor)
selectedTextOrCurrentIdentifier :: IDEM (Maybe Text)
selectedTextOrCurrentIdentifier = do
    st <- selectedText
    case st of
        Just t -> return $ Just t
        Nothing -> do
            candy' <- readIDE candy
            inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ -> do
                        (l,r)   <- getIdentifierUnderCursor ebuf
                        t <- getCandylessPart candy' ebuf l r
                        return $ if T.null t
                                                then Nothing
                                                else Just t

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

-- | Returns the packages to which this file belongs
--   uses the 'bufferProjCache' and might extend it
belongsToPackages :: MonadIDE m => FilePath -> m [IDEPackage]
belongsToPackages fp = do
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

-- | Returns the packages to which this buffer belongs
--   uses the 'bufferProjCache' and might extend it
belongsToPackages' :: MonadIDE m => IDEBuffer -> m [IDEPackage]
belongsToPackages' = maybe (return []) belongsToPackages . fileName

-- | Checks whether a file belongs to a package (includes files in
-- sandbox source dirs)
belongsToPackage :: FilePath -> IDEPackage -> Bool
belongsToPackage f = any (`isSubPath` f) . ipdAllDirs

-- | Checks whether a file belongs to the workspace
belongsToWorkspace :: MonadIDE m => FilePath -> m Bool
belongsToWorkspace fp = liftM (not . null) (belongsToPackages fp)

-- | Checks whether a file belongs to the workspace
belongsToWorkspace' :: MonadIDE m => IDEBuffer -> m Bool
belongsToWorkspace' = maybe (return False) belongsToWorkspace . fileName

useCandyFor :: MonadIDE m => IDEBuffer -> m Bool
useCandyFor aBuffer = do
    prefs <- readIDE prefs
    return (candyState prefs && isHaskellMode (mode aBuffer))

switchBuffersCandy :: IDEAction
switchBuffersCandy = do
    prefs <- readIDE prefs
    buffers <- allBuffers
    forM_ buffers $ \b@IDEBuffer{sourceView=sv} -> do
        buf <- getBuffer sv
        if candyState prefs
            then modeTransformToCandy (mode b) (modeEditInCommentOrString (mode b)) buf
            else modeTransformFromCandy (mode b) buf


