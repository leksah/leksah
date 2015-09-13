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
    ErrorsPane
,   ErrorsState
,   fillErrorList
,   getErrors
,   selectMatchingErrors
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable)
import IDE.Core.State
import IDE.ImportTool
       (resolveErrors, resolveMenuItems)
import Data.List (groupBy, sortBy, elemIndex)
import IDE.LogRef (showSourceSpan)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils
       (treeViewContextMenu', treeViewContextMenu, __, treeViewToggleRow,
        treeStoreGetForest)
import Data.Text (dropWhileEnd, Text)
import Control.Monad (filterM, foldM_, unless, void, when, forM_)
import qualified Data.Text as T
       (unlines, dropWhileEnd, unpack, pack, intercalate, lines,
        takeWhile, length, drop)
import Data.IORef (writeIORef, readIORef, newIORef, IORef)
import Data.Maybe (isJust, isNothing)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as Seq (null, elemIndexL)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import Data.Char (isSpace)
import Data.Tree (Forest, Tree(..), Tree)
import Control.Applicative (Alternative(..))


-- | The representation of the Errors pane
data ErrorsPane      =   ErrorsPane {
    vbox              :: VBox
,   scrolledView      :: ScrolledWindow
,   treeView          :: TreeView
,   errorStore        :: TreeStore ErrorRecord
,   autoClose         :: IORef Bool -- ^ If the pane was only displayed to show current error
,   errorsButton      :: ToggleButton
,   warningsButton    :: ToggleButton
,   suggestionsButton :: ToggleButton
} deriving Typeable


-- | The data for a single row in the Errors pane
data ErrorRecord = ERLogRef LogRef
                 | ERPackage IDEPackage Text
                 | ERIDE Text
                 | ERFile IDEPackage FilePath -- used for parent node for all errors in a file
    deriving (Eq)

-- | The additional state used when recovering the pane
data ErrorsState = ErrorsState
    {
      showErrors :: Bool
    , showWarnings :: Bool
    , showSuggestions :: Bool
    }
   deriving (Eq,Ord,Read,Show,Typeable)


instance Pane ErrorsPane IDEM
    where
    primPaneName _  =   __ "Errors"
    getTopWidget    =   castToWidget . vbox
    paneId _b       =   "*Errors"


instance RecoverablePane ErrorsPane ErrorsState IDEM where
    saveState ErrorsPane{..} = liftIO $ do
        showErrors      <- get errorsButton toggleButtonActive
        showWarnings    <- get warningsButton toggleButtonActive
        showSuggestions <- get suggestionsButton toggleButtonActive
        return (Just ErrorsState{..})

    recoverState pp ErrorsState{..} = do
        nb <- getNotebook pp
        mbErrors <- buildPane pp nb builder
        forM_ mbErrors $ \ErrorsPane{..} -> liftIO $ do
            set errorsButton [toggleButtonActive := showErrors]
            set warningsButton [toggleButtonActive := showWarnings]
            set suggestionsButton [toggleButtonActive := showSuggestions]
        return mbErrors


    builder = builder'

-- | Builds an 'ErrorsPane' pane together with a list of
--   event 'Connections'
builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe ErrorsPane, Connections)
builder' _pp _nb _windows = reifyIDE $ \ ideR -> do
    errorStore   <- treeStoreNew []

    vbox         <- vBoxNew False 0

    -- Top box with buttons
    hbox <- hBoxNew False 0
    boxPackStart vbox hbox PackNatural 0

    errorsButton <- toggleButtonNewWithLabel (__ "Errors")
    warningsButton <- toggleButtonNewWithLabel (__ "Warnings")
    suggestionsButton <- toggleButtonNewWithLabel (__ "Suggestions")
    set suggestionsButton [toggleButtonActive := False]

    forM_ [errorsButton, warningsButton, suggestionsButton] $ \b -> do
        set b [toggleButtonActive := True]
        boxPackStart hbox b PackNatural 3
        b `on` toggled $ reflectIDE (fillErrorList False) ideR


    boxPackStart vbox hbox PackNatural 0


    -- TreeView for bottom part of vbox

    treeView     <- treeViewNew
    treeViewSetModel treeView errorStore
    set treeView
        [ treeViewLevelIndentation := 20
        , treeViewRulesHint := True
        , treeViewHeadersVisible := False]

    column       <- treeViewColumnNew
    iconRenderer <- cellRendererPixbufNew

    cellLayoutPackStart column iconRenderer False
    cellLayoutSetAttributes column iconRenderer errorStore
                $ \row -> [ newAttrFromMaybeStringProperty "icon-name" := toIcon row]


    treeViewColumnSetSizing column TreeViewColumnAutosize

    renderer <- cellRendererTextNew
    cellLayoutPackStart column renderer False
    cellLayoutSetAttributes column renderer errorStore
        $ \row -> [cellText := toDescription row ]

    treeViewAppendColumn treeView column

    selB <- treeViewGetSelection treeView
    treeSelectionSetMode selB SelectionMultiple
    scrolledView <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType scrolledView ShadowIn
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
    boxPackStart vbox scrolledView PackGrow 0

    autoClose <- newIORef False

    let pane = ErrorsPane {..}
    cid1 <- after treeView focusInEvent $ do
        liftIO $ reflectIDE (makeActive pane) ideR
        return True
    (cid2, cid3) <- flip reflectIDE ideR $
        treeViewContextMenu' treeView errorStore contextMenuItems
    cid4 <- treeView `on` rowActivated $ \path col -> do
        record <- treeStoreGetValue errorStore path
        case record of
            ERLogRef logRef -> errorsSelect ideR errorStore path col
            ERFile _ _ -> void $ treeViewToggleRow treeView path
            _        -> return ()

    reflectIDE (fillErrorList' pane) ideR
    return (Just pane, map ConnectC [cid1, cid2, cid3, cid4])


toIcon :: ErrorRecord -> Maybe Text
toIcon (ERLogRef logRef) =
    case logRefType logRef of
        ErrorRef   -> Just "ide_error"
        WarningRef -> Just "ide_warning"
        LintRef    -> Just "ide_suggestion"
        _          -> Nothing
toIcon (ERPackage _ _) = Just "dialog-error"
toIcon (ERIDE _) = Just "dialog-error"
toIcon (ERFile _ _) = Just "ide_source"

toDescription :: ErrorRecord -> Text
toDescription errorRec =  T.intercalate " " . map (removeTrailingWhiteSpace . removeIndentation) . T.lines $
    case errorRec of
        (ERLogRef logRef) -> refDescription logRef
        (ERPackage pkg msg) -> packageIdentifierToString (ipdPackageId pkg) <> ": " <> msg
        (ERIDE msg) -> msg
        (ERFile pkg fp) -> T.pack fp <> " ("  <> packageIdentifierToString (ipdPackageId pkg) <> ")"


-- | Removes the unnecessary indentation
removeIndentation :: Text -> Text
removeIndentation t = T.intercalate "\n" $ map (T.drop minIndent) l
  where
    l = T.lines t
    minIndent = minimum $ map (T.length . T.takeWhile (== ' ')) l

removeTrailingWhiteSpace :: Text -> Text
removeTrailingWhiteSpace = T.dropWhileEnd isSpace

cutOffAt :: Int -> Text -> Text
cutOffAt n t | T.length t < n = t
             | otherwise      = T.pack (take n (T.unpack t)) <> "..."

-- | Get the Errors pane
getErrors :: Maybe PanePath -> IDEM ErrorsPane
getErrors Nothing    = forceGetPane (Right "*Errors")
getErrors (Just pp)  = forceGetPane (Left pp)


-- | Repopulates the Errors pane
fillErrorList :: Bool -- ^ Whether to display the Errors pane
              -> IDEAction
fillErrorList False = getPane >>= maybe (return ()) fillErrorList'
fillErrorList True = getErrors Nothing  >>= \ p -> fillErrorList' p >> displayPane p False


-- | Fills the pane with the error list from the IDE state
fillErrorList' :: ErrorsPane -> IDEAction
fillErrorList' pane = do
    refs <- F.toList <$> readIDE errorRefs
    visibleRefs <- liftIO $ filterM visible refs
    let eqFile ref1 ref2 = let file = (srcSpanFilename . logRefSrcSpan) in file ref1 == file ref2
        fileRefs :: [(IDEPackage, FilePath, [LogRef])]
        fileRefs =
            map (\(x:xs) -> (logRefPackage x, srcSpanFilename (logRefSrcSpan x), x:xs))
            . map (sortBy (comparing logRefType))
            . groupBy eqFile
            . sortBy (comparing (srcSpanFilename . logRefSrcSpan))
            $ visibleRefs

    ac   <- liftIO $ readIORef (autoClose pane)
    when (null refs && ac) . void $ closePane pane

    updateButtons pane
    liftIO $ do
        let store = errorStore pane
        let view  = treeView pane
        treeStoreClear store
        forM_ (zip fileRefs [0..]) $ \ ((pkg, path, refs), n) -> do
            treeStoreInsert store [] n (ERFile pkg path)
            forM_ (zip refs [0..]) $ \(ref, m) ->
                treeStoreInsert store [n] m (ERLogRef ref)
            treeViewExpandRow view [n] False
    where
        visible ref =
            case logRefType ref of
                ErrorRef   -> toggleButtonGetActive (errorsButton pane)
                WarningRef -> toggleButtonGetActive (warningsButton pane)
                LintRef    -> toggleButtonGetActive (suggestionsButton pane)
                _          -> return False

updateButtons :: ErrorsPane -> IDEAction
updateButtons pane = do
    let numRefs refType = length . filter ((== refType) . logRefType) . F.toList <$> readIDE errorRefs
    let setLabel name amount button = buttonSetLabel button (name <> " (" <> T.pack (show amount) <> ")" )

    numErrors      <- numRefs ErrorRef
    numWarnings    <- numRefs WarningRef
    numSuggestions <- numRefs LintRef

    liftIO $ do
        setLabel "Errors"      numErrors      (errorsButton   pane)
        setLabel "Warnings"    numWarnings    (warningsButton pane)
        setLabel "Suggestions" numSuggestions (suggestionsButton pane)
        widgetShowAll (vbox pane)


-- | Get the currently selected error
getSelectedError ::  TreeView
    -> TreeStore ErrorRecord
    -> IO (Maybe LogRef)
getSelectedError treeView store = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        path:_ ->  do
            val     <-  treeStoreGetValue store path
            case val of
                ERLogRef logRef -> return (Just logRef)
                _ -> return Nothing
        _  ->  return Nothing

-- | Select a 'LogRef' in the Errors pane if it is visible
selectError :: Maybe LogRef -- ^ When @Nothing@, the first row in the list is selected
            -> IDEAction
selectError mbLogRef = do
    (mbPane :: Maybe ErrorsPane) <- getPane
    errors     <- getErrors Nothing
    when (isNothing mbPane) $ do
        liftIO $ writeIORef (autoClose errors) True
        displayPane errors False
    reifyIDE $ \ideR -> do
        selection <- treeViewGetSelection (treeView errors)
        case mbLogRef of
            Nothing -> do
                empty <- null <$> treeStoreGetTree (errorStore errors) []
                unless empty $
                    treeViewScrollToCell (treeView errors) (Just [0]) Nothing Nothing
                treeSelectionUnselectAll selection
            Just lr -> do
                let store = errorStore errors
                empty <- isNothing <$> treeModelGetIterFirst store
                unless empty $ do
                    forest <- treeStoreGetForest store
                    let mbPath = forestFind forest (ERLogRef lr)
                    forM_ mbPath $ \path -> do
                        treeViewScrollToCell (treeView errors) (Just path) Nothing Nothing
                        treeSelectionSelectPath selection path

    where
        forestFind :: Eq a => Forest a -> a -> Maybe TreePath
        forestFind = forestFind' [0]
            where
                forestFind' path [] _ = Nothing
                forestFind' path (Node x trees : forest) y
                    | x == y    = Just path
                    | otherwise = forestFind' (path ++ [0]) trees y
                                      <|> forestFind' (sibling path) forest y

                sibling [n] = [n+1]
                sibling (x:xs) = x:sibling xs

contextMenuItems :: ErrorRecord -> TreePath -> TreeStore ErrorRecord -> IDEM [[(Text, IDEAction)]]
contextMenuItems record path store = return $
    [[("Resolve Errors", resolveErrors)]
        ++ case record of
               ERLogRef logRef -> resolveMenuItems logRef ++ [clipboardItem (refDescription logRef)]
               ERIDE msg       -> [clipboardItem msg]
               ERPackage _ msg -> [clipboardItem msg]
               _               -> []
    ]


    where clipboardItem str = ("Copy message to clipboard", liftIO $ clipboardGet selectionClipboard >>= flip clipboardSetText str)


-- | Highlight an error refered to by the 'TreePath' in the given 'TreeViewColumn'
errorsSelect :: IDERef
                -> TreeStore ErrorRecord
                -> TreePath
                -> TreeViewColumn
                -> IO ()
errorsSelect ideR store path _ = do
    record <- treeStoreGetValue store path
    case record of
        ERLogRef logRef -> reflectIDE (setCurrentError (Just logRef )) ideR
        _ -> return ()


-- | Select the matching errors for a 'SrcSpan' in the Errors
--   pane, or none at all
selectMatchingErrors :: Maybe SrcSpan -- ^ When @Nothing@, unselects any errors in the pane
                     -> IDEAction
selectMatchingErrors mbSpan = do
    mbErrors <- getPane
    forM_ mbErrors $ \pane -> do
        treeSel <- liftIO $ treeViewGetSelection (treeView pane)
        liftIO $ treeSelectionUnselectAll treeSel
        forM_ mbSpan $ \span -> do
            spans <- map logRefSrcSpan . F.toList <$> readIDE errorRefs
            matches <- matchingRefs span . F.toList <$> readIDE errorRefs
            forM_ matches $ \ref ->
                selectError (Just ref)

matchingRefs :: SrcSpan -> [LogRef] -> [LogRef]
matchingRefs span refs =
    -- the path of the SrcSpan in the LogRef absolute, so comparison with the given SrcSpan goes right
    let toAbsolute ref =  ref {logRefSrcSpan = (logRefSrcSpan ref) {srcSpanFilename = logRefFullFilePath ref}}
    in filter (\ref -> filesMatch (logRefSrcSpan (toAbsolute ref)) span && span `insideOf` (logRefSrcSpan (toAbsolute ref))) refs
    where
        filesMatch span span' = srcSpanFilename span == srcSpanFilename span'

        -- Test whether the first span is inside of the second
        insideOf (SrcSpan _ lStart cStart lEnd cEnd) (SrcSpan _ lStart' cStart' lEnd' cEnd')
            =  (lStart, cStart) <= (lEnd', cEnd')
            && (lEnd, cEnd)     >= (lStart', cStart')
