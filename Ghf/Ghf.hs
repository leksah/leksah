module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Control.Monad.Reader
import Data.IORef


type FileName   =   String

data Ghf    =   Ghf {
    window :: Window
,   notebook1 :: Notebook
,   buffers :: [GhfBuffer]
}
data GhfBuffer  =   GhfBuffer { 
    fileName :: FileName 
,   sourceView :: SourceView 
,   scrolledWindow :: ScrolledWindow
}

type GhfRef = IORef Ghf
type GhfM = ReaderT (GhfRef) IO
type GhfAction = GhfM ()

readGhf :: (Ghf -> b) -> GhfM b
readGhf f = do
    e <- ask
    lift $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyGhf_ :: (Ghf -> IO Ghf) -> GhfM ()
modifyGhf_ f = do
    e <- ask
    e' <- lift $ (f =<< readIORef e)
    lift $ writeIORef e e'  

-- | Variation on modifyGhf_ that lets you return a value
modifyGhf :: (Ghf -> IO (Ghf,b)) -> GhfM b
modifyGhf f = do
    e <- ask
    (e',result) <- lift (f =<< readIORef e)
    lift $ writeIORef e e'
    return result

withGhf :: (Ghf -> IO a) -> GhfM a
withGhf f = do
    e <- ask
    lift $ f =<< readIORef e

main = startGhf

startGhf = do
    initGUI
    win <- windowNew
    nb <- notebookNew
    let ghf = Ghf win nb []
    ghfR <- newIORef ghf
    mb <- runReaderT (makeMenuBar [fileMenu]) ghfR
    vb <- vBoxNew False 1  -- Top-level vbox

    boxPackStart vb mb PackNatural 0
    boxPackStart vb nb PackGrow 0

    win `onDestroy` mainQuit
    win `containerAdd` vb

      -- show the widget and run the main loop
    windowSetDefaultSize win 400 500
    widgetShowAll win
    mainGUI


newTextBuffer :: FileName -> GhfAction
newTextBuffer fn = do
    -- create the appropriate language
    nb <- readGhf notebook1
    lift $ do
        lm <-  sourceLanguagesManagerNew
        langM <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
        lang <- case langM of
            (Just lang) -> return lang
            Nothing -> do
                langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
                error ("please copy haskell.lang to one of the following directories:\n"
                    ++unlines langDirs)

        -- create a new SourceBuffer object
        buffer <- sourceBufferNewWithLanguage lang

        -- load up and display a file
        fileContents <- readFile fn
        textBufferSetText buffer fileContents
        textBufferSetModified buffer False
        sourceBufferSetHighlight buffer True

        -- create a new SourceView Widget
        sv <- sourceViewNewWithBuffer buffer

        -- put it in a scrolled window
        sw <- scrolledWindowNew Nothing Nothing
        sw `containerAdd` sv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        sw `scrolledWindowSetShadowType` ShadowIn
        notebookPrependPage nb sw fn
        widgetShowAll nb
        return ()

type ItemDesc   =   (String, GhfAction)
type SubDesc    =   (String, [ItemDesc])
type MenuDesc   =   [SubDesc]

makeMenuBar :: MenuDesc -> GhfM MenuBar
makeMenuBar menuDesc = do
    mb <- lift menuBarNew
    mapM_ (buildSubmenu mb) menuDesc
    return mb

buildSubmenu :: MenuBar -> SubDesc -> GhfAction
buildSubmenu mb (title,items) = do
    mu <- lift menuNew
    mapM_ (buildItems mu) items
    lift $ do
      item <- menuItemNewWithMnemonic title
      menuItemSetSubmenu item mu
      menuShellAppend mb item

buildItems :: Menu -> ItemDesc -> GhfAction
buildItems menu (itemName,func) = do
    ghfR <- ask
    lift $ do
        item <- menuItemNewWithMnemonic itemName
        menuShellAppend menu item
        onActivateLeaf item (runReaderT func ghfR)
    return ()

fileMenu :: SubDesc
fileMenu =  ("_File",[("_Open",fileOpen),("_Save",fileSave),("_Quit",quit)])

fileSave :: GhfAction
fileSave = return ()

quit :: GhfAction
quit = return ()


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
        Just fn -> newTextBuffer fn 



{--
main :: IO ()
main = do
    initGUI


    f <- fontDescriptionNew
    fontDescriptionSetFamily f "Monospace"

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.

    v <- sourceViewNew 
    widgetModifyFont v (Just f)


    scroll <- scrolledWindowNew Nothing Nothing
    set scroll [scrolledWindowPlacement := CornerTopRight,
                scrolledWindowVscrollbarPolicy := PolicyAlways,
                scrolledWindowHscrollbarPolicy := PolicyAutomatic,
                containerChild := v]

    vb <- vBoxNew False 1
    set vb [ containerChild := scroll, 
             containerChild := ml, 
             boxChildPacking ml := PackNatural] 
    return (castToBox vb)

    vb <- vBoxNew False 1  -- Top-level vbox
    vb' <- vBoxNew False 1

    win <- windowNew
    set win [ containerChild := vb ]
    onDestroy win mainQuit
  
    cmd <- labelNew Nothing
    set cmd [ miscXalign := 0.01 ]
    widgetModifyFont cmd (Just f)

    set vb [ containerChild := vb', 
             containerChild := cmd, 
             boxChildPacking cmd := PackNatural] 

    widgetShowAll win
    return ()
--}     
