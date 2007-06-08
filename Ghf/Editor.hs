module Ghf.Editor where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust )

import Ghf.Core

realBufferName :: [GhfBuffer] -> String -> Int -> (Int,String)
realBufferName bufs bn ind =
    let ind = foldr (\buf ind -> if bufferName buf == bn
                    then max ind (addedIndex buf + 1)
                    else ind) 0 bufs in
    if ind == 0 then (0,bn) else (ind,bn ++ "(" ++ show ind ++ ")")


newTextBuffer :: String -> Maybe FileName -> GhfAction
newTextBuffer bn mbfn = do
    -- create the appropriate language
    nb <- readGhf notebook1
    bufs <- readGhf buffers
    let (ind,rbn) = realBufferName bufs bn 0
    buf <- lift $ do
        lm      <-  sourceLanguagesManagerNew
        langM   <-  sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
        lang    <-  case langM of
                        (Just lang) -> return lang
                        Nothing -> do
                            langDirs <- sourceLanguagesManagerGetLangFilesDirs lm
                            error ("please copy haskell.lang to one of the following directories:\n"
                                ++ unlines langDirs)

        -- create a new SourceBuffer object
        buffer <- sourceBufferNewWithLanguage lang

        -- load up and display a file
        fileContents <- case mbfn of
            Just fn -> readFile fn
            Nothing -> return "\n\n\n\n\n"
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
        notebookPrependPage nb sw rbn
        widgetShowAll nb

        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        return (GhfBuffer mbfn bn ind sv sw)
    modifyGhf_ (\ghf -> return (ghf{buffers = buf : bufs}))

fileSave :: Bool -> GhfAction
fileSave query = do
    window  <- readGhf window
    nb      <- readGhf notebook1
    bufs    <- readGhf buffers 
    mbnbufs <- lift $ do
        i   <- notebookGetCurrentPage nb
        mbp <- notebookGetNthPage nb i
        let currentBuffer = case i of
                -1 -> error "No page selected"
                n  -> bufs !! i
        let mbfn = fileName currentBuffer
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
                                        let (ind,rbn) = realBufferName bufs bn 0
                                        label <- labelNew (Just rbn)
                                        notebookSetTabLabel nb (fromJust mbp) label
                                        return (Just (map (bufRename currentBuffer fn bn ind) bufs))
                                    ResponseNo -> return Nothing
    case mbnbufs of
        Just nbufs ->modifyGhf_ (\ghf -> return (ghf{buffers = nbufs}))
        Nothing -> return ()
    where
        bufRename cb fn bn ind b  = if b == cb
                            then b{fileName = Just fn, bufferName = bn, addedIndex = ind}
                            else b
        fileSave' :: GhfBuffer -> FileName -> IO()
        fileSave' ghfBuf fn = do
            buf     <- textViewGetBuffer $ sourceView ghfBuf
            start   <- textBufferGetStartIter buf
            end     <- textBufferGetEndIter buf
            text    <- textBufferGetText buf start end True
            writeFile fn text


quit :: GhfAction
quit = lift mainQuit

fileNew :: GhfAction
fileNew = newTextBuffer "Unnamed" Nothing

fileClose :: GhfAction
fileClose = do
    nb      <- readGhf notebook1
    bufs    <- readGhf buffers 
    buf <- lift $ do
        i   <- notebookGetCurrentPage nb
        mbp <- notebookGetNthPage nb i
        let currentBuffer = case i of
                -1 -> error "No page selected"
                n  -> bufs !! i
        notebookRemovePage nb i
        return currentBuffer
    modifyGhf_ (\ghf -> return (ghf{buffers = filter (/= buf) bufs}))

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



