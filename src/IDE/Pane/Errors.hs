{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
   MultiParamTypeClasses, DeriveDataTypeable, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Errors
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | A pane which displays a list of errors
--
-----------------------------------------------------------------------------

module IDE.Pane.Errors (
    IDEErrors
,   ErrorsState
,   fillErrorList
,   addErrorToList
,   selectError
,   getErrors
,   selectMatchingErrors
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import IDE.Core.State
import IDE.ImportTool
       (addResolveMenuItems, resolveErrors)
import Data.List (elemIndex)
import IDE.LogRef (showSourceSpan)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (getDarkState, treeViewContextMenu, __)
import Data.Text (Text)
import Control.Monad (foldM_, unless, void, when, forM_)
import qualified Data.Text as T
       (intercalate, lines, takeWhile, length, drop)
import Data.IORef (writeIORef, readIORef, newIORef, IORef)
import Data.Maybe (isNothing)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as Seq (null, elemIndexL)

-- | A breakpoints pane description
--
data IDEErrors      =   IDEErrors {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   errorStore      ::   ListStore ErrColumn
,   autoClose       ::   IORef Bool -- If the pane was only displayed to show current error
} deriving Typeable

data ErrColumn = ErrColumn {
    logRef     :: LogRef,
    background :: Text}

data ErrorsState    =   ErrorsState {
}   deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEErrors IDEM
    where
    primPaneName _  =   __ "Errors"
    getTopWidget    =   castToWidget . scrolledView
    paneId _b       =   "*Errors"

instance RecoverablePane IDEErrors ErrorsState IDEM where
    saveState _p     =   return (Just ErrorsState)
    recoverState pp ErrorsState = do nb <- getNotebook pp
                                     buildPane pp nb builder
    builder = builder'

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDEErrors, Connections)
builder' _pp _nb _windows = reifyIDE $ \ ideR -> do
    errorStore   <- listStoreNew []
    treeView     <- treeViewNew
    treeViewSetModel treeView errorStore

    rendererA    <- cellRendererTextNew
    colA         <- treeViewColumnNew
    treeViewColumnSetTitle colA (__ "Description")
    treeViewColumnSetSizing colA TreeViewColumnAutosize
    treeViewColumnSetResizable colA True
    treeViewColumnSetReorderable colA True
    treeViewAppendColumn treeView colA
    cellLayoutPackStart colA rendererA False
    cellLayoutSetAttributes colA rendererA errorStore
        $ \row -> [cellText := removeIndentation (refDescription (logRef row)),
                   cellTextBackground := background row ]
    rendererB    <- cellRendererTextNew
    colB         <- treeViewColumnNew
    treeViewColumnSetTitle colB (__ "Location")
    treeViewColumnSetSizing colB TreeViewColumnAutosize
    treeViewColumnSetResizable colB True
    treeViewColumnSetReorderable colB True
    treeViewAppendColumn treeView colB
    cellLayoutPackStart colB rendererB False
    cellLayoutSetAttributes colB rendererB errorStore
        $ \row -> [ cellText := showSourceSpan (logRef row),
                   cellTextBackground := background row ]
    treeViewSetHeadersVisible treeView False
    selB <- treeViewGetSelection treeView
    treeSelectionSetMode selB SelectionMultiple
    scrolledView <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType scrolledView ShadowIn
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
    autoClose <- newIORef False
    let pane = IDEErrors {..}
    cid1 <- after treeView focusInEvent $ do
        liftIO $ reflectIDE (makeActive pane) ideR
        return True
    (cid2, cid3) <- treeViewContextMenu treeView $ errorsContextMenu ideR errorStore treeView
    cid4 <- treeView `on` rowActivated $ errorsSelect ideR errorStore
    reflectIDE (fillErrorList' pane) ideR
    return (Just pane, map ConnectC [cid1, cid2, cid3, cid4])

removeIndentation t = T.intercalate "\n" $ map (T.drop minIndent) l
  where
    l = T.lines t
    minIndent = minimum $ map (T.length . T.takeWhile (== ' ')) l

getErrors :: Maybe PanePath -> IDEM IDEErrors
getErrors Nothing    = forceGetPane (Right "*Errors")
getErrors (Just pp)  = forceGetPane (Left pp)

fillErrorList :: Bool -> IDEAction
fillErrorList False = getPane >>= maybe (return ()) fillErrorList'
fillErrorList True = getErrors Nothing  >>= \ p -> fillErrorList' p >> displayPane p False

fillErrorList' :: IDEErrors -> IDEAction
fillErrorList' pane = do
    refs <- readIDE errorRefs
    ac   <- liftIO $ readIORef (autoClose pane)
    when (Seq.null refs && ac) . void $ closePane pane
    isDark <- getDarkState
    liftIO $ do
        let store = errorStore pane
        listStoreClear store
        forM_ (zip (F.toList refs) [0..]) $ \ (lr, index) ->
            listStoreInsert store index $ ErrColumn lr (
                (if even index then fst else snd) $
                (if isDark then fst else snd) $
                    case logRefType lr of
                        WarningRef -> (("#282000", "#201900"), ("#FFF1DE", "#FFF5E8"))
                        LintRef    -> (("#003000", "#002800"), ("#DBFFDB", "#EDFFED"))
                        _          -> (("#380000", "#280000"), ("#FFDEDE", "#FFEBEB")))

addErrorToList :: Bool -> Int -> LogRef -> IDEAction
addErrorToList False index lr = getPane >>= maybe (return ()) (addErrorToList' index lr)
addErrorToList True  index lr = getErrors Nothing  >>= \ p -> addErrorToList' index lr p >> displayPane p False

addErrorToList' :: Int -> LogRef -> IDEErrors -> IDEAction
addErrorToList' index lr pane = do
--    refs <- readIDE errorRefs
    ac   <- liftIO $ readIORef (autoClose pane)
--    when (null refs && ac) . void $ closePane pane
    isDark <- getDarkState
    liftIO $ do
        let store = errorStore pane
--        listStoreClear store
--        forM_ (zip (toList refs) [0..]) $ \ (lr, index) ->
        listStoreInsert store index $ ErrColumn lr (
                (if even index then fst else snd) $
                (if isDark then fst else snd) $
                    case logRefType lr of
                        WarningRef -> (("#282000", "#201900"), ("#FFF1DE", "#FFF5E8"))
                        LintRef    -> (("#003000", "#002800"), ("#DBFFDB", "#EDFFED"))
                        _          -> (("#380000", "#280000"), ("#FFDEDE", "#FFEBEB")))

getSelectedError ::  TreeView
    -> ListStore ErrColumn
    -> IO (Maybe LogRef)
getSelectedError treeView store = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        [a]:r ->  do
            val     <-  listStoreGetValue store a
            return (Just (logRef val))
        _  ->  return Nothing

selectError :: Maybe LogRef -> IDEAction
selectError mbLogRef = do
    (mbPane :: Maybe IDEErrors) <- getPane
    errorRefs' <- readIDE errorRefs
    errors     <- getErrors Nothing
    when (isNothing mbPane) $ do
        liftIO $ writeIORef (autoClose errors) True
        displayPane errors False
    liftIO $ do
        selection <- treeViewGetSelection (treeView errors)
        case mbLogRef of
            Nothing -> do
                size <- listStoreGetSize (errorStore errors)
                unless (size == 0) $
                    treeViewScrollToCell (treeView errors) (Just [0]) Nothing Nothing
                treeSelectionUnselectAll selection
            Just lr -> case lr `Seq.elemIndexL` errorRefs' of
                        Nothing  -> return ()
                        Just ind -> do
                            treeViewScrollToCell (treeView errors) (Just [ind]) Nothing Nothing
                            treeSelectionSelectPath selection [ind]

errorsContextMenu :: IDERef
                  -> ListStore ErrColumn
                  -> TreeView
                  -> Menu
                  -> IO ()
errorsContextMenu ideR store treeView theMenu = do
    mbSel           <-  getSelectedError treeView store
    item0           <-  menuItemNewWithLabel (__ "Resolve Errors")
    item0 `on` menuItemActivate $ reflectIDE resolveErrors ideR
    menuShellAppend theMenu item0
    case mbSel of
        Just sel -> addResolveMenuItems ideR theMenu sel
        Nothing -> return ()

errorsSelect :: IDERef
                -> ListStore ErrColumn
                -> TreePath
                -> TreeViewColumn
                -> IO ()
errorsSelect ideR store [index] _ = do
    ref <- listStoreGetValue store index
    reflectIDE (setCurrentError (Just (logRef ref))) ideR
errorsSelect _ _ _ _ = return ()

selectMatchingErrors :: Maybe SrcSpan -> IDEM ()
selectMatchingErrors mbSpan = do
    mbErrors <- getPane
    case mbErrors of
        Nothing -> return ()
        Just pane  ->
            liftIO $ do
                treeSel <- treeViewGetSelection (treeView pane)
                case mbSpan of
                    Nothing -> treeSelectionUnselectAll treeSel
                    Just (SrcSpan file lStart cStart lEnd cEnd) -> do
                        size <- listStoreGetSize (errorStore pane)
                        foldM_ (\ haveScrolled n -> do
                            mbIter <- treeModelGetIter (errorStore pane) [n]
                            case mbIter of
                                Nothing -> return False
                                Just iter -> do
                                    ErrColumn {logRef = ref@LogRef{..}} <- listStoreGetValue (errorStore pane) n
                                    isSelected <- treeSelectionIterIsSelected treeSel iter
                                    let shouldBeSel = file == logRefFullFilePath ref
                                                     && (lStart, cStart) <= (srcSpanEndLine     logRefSrcSpan,
                                                                               srcSpanEndColumn   logRefSrcSpan)
                                                     && (lEnd, cEnd)     >= (srcSpanStartLine   logRefSrcSpan,
                                                                               srcSpanStartColumn logRefSrcSpan)
                                    when (isSelected && not shouldBeSel) $ treeSelectionUnselectIter treeSel iter
                                    when (not isSelected && shouldBeSel) $ do
                                        unless haveScrolled $ treeViewScrollToCell (treeView pane) (Just [n]) Nothing Nothing
                                        treeSelectionSelectIter treeSel iter
                                    return $ haveScrolled || shouldBeSel)
                            False (take size [0..])


