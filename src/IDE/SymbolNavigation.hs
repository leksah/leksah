module IDE.SymbolNavigation (launchAutoCompleteDialog)

where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Monoid
import IDE.Core.Types
import IDE.Core.CTypes
import IDE.Core.State
import IDE.Metainfo.Provider
import Graphics.UI.Gtk.Gdk.Events as Gdk
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Frame.ViewFrame
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Applicative
import Distribution.ModuleName
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

data Locality = LocalityPackage  | LocalityWorkspace | LocalitySystem  -- in which category symbol is located
    deriving (Ord,Eq,Show)


launchAutoCompleteDialog :: String -> (Descr -> IDEM ()) -> IDEM ()
launchAutoCompleteDialog txt act = do
    dia                        <-   liftIO $ dialogNew
    win <- getMainWindow
    ideR    <- ask
    wi <- getSystemInfo
    wiW <- getWorkspaceInfo
    wiP <- getPackageInfo
    case (wi,wiW,wiP) of
        (Just (GenScopeC (PackScope _ syms)),
                Just (GenScopeC (PackScope _ symsW), GenScopeC (PackScope _ _ )),
                Just (GenScopeC (PackScope _ symsP), GenScopeC (PackScope _ _ ))) -> do
            let symbolsT = map T.pack $ Set.toList $ (symbols syms `Set.union` symbols symsP)
            liftIO $ do
                print "============================"
                print $ symLookup "launchAutoCompleteDialog" symsP
                windowSetTransientFor dia win
                windowSetTitle dia "Go to Symbol"
                upper                      <-   dialogGetUpper dia
                lower                      <-   dialogGetActionArea dia
                vb <- vBoxNew False 0
                boxPackStart upper vb PackNatural 7
                en <- entryNew
                store <- listStoreNew []
                tv <- treeViewNewWithModel store
                treeViewSetFixedHeightMode tv True
                treeViewSetHoverExpand tv True
                mapM_ (\(i,s) -> do
                    col <- treeViewColumnNew
                    renderer <- cellRendererTextNew
                    treeViewColumnSetSizing col TreeViewColumnFixed
                    treeViewColumnSetResizable col True
                    treeViewColumnSetFixedWidth col ([200,300,200] !! i)
                    treeViewAppendColumn tv col
                    cellLayoutPackStart col renderer True

                    cellLayoutSetAttributes col renderer store
                        $ \(locality, _, ss) -> [cellText := replaceCR $ ss !! i] ++
                                    case (i, locality) of
                                        (2, _) -> [cellTextScale := 0.8, cellTextScaleSet := True]
                                        (0, LocalityWorkspace) ->
                                            [cellTextStyle := StyleItalic, cellTextStyleSet := True, cellTextWeightSet := False, cellTextScaleSet := False]
                                        (0, LocalityPackage) ->
                                            [cellTextWeight := 1000, cellTextWeightSet := True, cellTextStyleSet := False, cellTextScaleSet := False]
                                        _ -> [cellTextStyleSet := False, cellTextWeightSet := False, cellTextScaleSet := False]

                    treeViewColumnSetTitle col s) $ zip [0..] ["symbol","module","type/kind"]

                boxPackEnd vb tv PackNatural 7
                boxPackEnd vb en PackNatural 0
                widgetSetSizeRequest tv 900 600

                bb      <-  hButtonBoxNew
                closeB  <-  buttonNewFromStock "gtk-cancel"
                okB    <-  buttonNewFromStock "gtk-ok"
                okB `onClicked` do
                    dialogResponse dia ResponseOk
                    widgetHideAll dia
                closeB `onClicked` do
                    dialogResponse dia ResponseCancel
                    widgetHideAll dia
                boxPackEnd bb closeB PackNatural 0
                boxPackEnd bb okB PackNatural 0
                boxPackStart lower bb PackNatural 7
                set okB [widgetCanDefault := True]
                buttonSetLabel okB "Goto"
                widgetGrabDefault okB
                widgetShowAll dia
                entrySetText en txt
                let getSymbolLocality symbolName = case (symLookup symbolName symsP, symLookup symbolName symsW) of
                                                        ((_:_),_) -> LocalityPackage
                                                        (_,(_:_)) -> LocalityWorkspace
                                                        _ -> LocalitySystem
                let
                    compareLocalityThenLength (Real d1 ) (Real d2) =
                        let
                            desc1 = dscName' d1
                            desc2 = dscName' d2
                            l1 = getSymbolLocality $ desc1
                            l2 = getSymbolLocality $ desc2
                            lcomp = compare l1 l2
                        in if lcomp == EQ
                            then compare (length desc1) (length desc2)
                            else lcomp
                    compareLocalityThenLength _ _ = error "compareLocalityThenLength: not real desciptions"
                let updateList = do
                    txt <- T.pack <$> entryGetText en
                    let ttxt = T.toLower txt
                    let symz_ = take 50
                                    $ sortBy (comparing getSymbolLocality)
                                    $ map (T.unpack)
                                    $ filter (matchCamelCase (txt :: T.Text) (ttxt:: T.Text)) (symbolsT ::[T.Text])
                    let symz = sortBy compareLocalityThenLength
                                    $ filter (not . isReexported)
                                    $ concatMap (\sym -> symLookup sym syms `mappend` symLookup sym symsP) symz_
                    listStoreClear store
                    mapM (\descr_ -> do
                        case descr_ of
                            Real descr -> do
                                let modn = case dscMbModu' descr of
                                                Just mn -> intercalate"." $ components $ modu mn
                                                Nothing -> "?"
                                let symbolName = dscName' descr
                                let typeDesc = map BS.w2c $ BS.unpack $ fromMaybe BS.empty $ dscMbTypeStr' descr
                                listStoreAppend store (getSymbolLocality symbolName, descr_, [symbolName,modn,take 80 typeDesc])
--                                treeVie
                                return ()
                            _ -> return ()
                            ) symz
                    when (length symz > 0) $ do
                        treeViewSetCursor tv [0] Nothing
                        return ()
                    print ("symz found: ",length symz)
                    return ()
                let gotoSelectedSymbol = do
                    cursor <- treeViewGetCursor tv
                    case cursor of
                        ([pos],_) -> do
                            (_,descr,_) <- listStoreGetValue store pos
                            dialogResponse dia ResponseCancel
                            widgetHideAll dia
                            liftIO $ reflectIDE (act descr) ideR
                            return ()
                        _ -> return ()

                updateList
                let modifyTreeViewCursor :: (Int -> Int) -> IO ()
                    modifyTreeViewCursor op = do
                        cur <- treeViewGetCursor tv
                        case cur of
                            ([pos],_) -> do
                                let npos = op pos
                                storeSize <- listStoreGetSize store
                                let npos' = if npos < 0 then 0 else if npos >= storeSize then storeSize-1 else npos
                                when (npos < storeSize) $ do
                                    treeViewSetCursor tv [npos'] Nothing
                                return ()
                            _ -> return ()
                onEditableChanged en $ do
                    updateList
                    return ()
                en `onKeyPress` \e -> do
                    case Gdk.eventKeyName e of
                        "Escape" -> do
                            dialogResponse dia ResponseCancel
                            widgetHideAll dia
                            return True
                        "Up" -> do
                            modifyTreeViewCursor (\x -> x-1)
                            return True
                        "Down" -> do
                            modifyTreeViewCursor (\x -> x+1)
                            return True
                        "Return" -> do
                            gotoSelectedSymbol
                            return True
                        nm -> do
                            print nm
                            return False
                resp  <- dialogRun dia
                return ()
        _ -> return ()
    return ()


matchCamelCase :: T.Text -> T.Text -> T.Text -> Bool
matchCamelCase search lsearch item =
    search `T.isInfixOf` item || (lsearch `T.isInfixOf` (T.toLower item))

compareLength  :: [a] -> [a] -> Ordering
compareLength s1 s2 = compare (length s1) (length s2)

compareLocality  :: Locality -> Locality -> Ordering
compareLocality s1 s2 = compare s1 s2

replaceCR [] = []
replaceCR ('\n':ss) = ' ':(replaceCR ss)
replaceCR ('\r':ss) = ' ':(replaceCR ss)
replaceCR (s:ss) = s:(replaceCR ss)
