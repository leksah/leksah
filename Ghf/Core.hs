module Ghf.Core (
    Ghf(..)
,   GhfBuffer(..)
,   GhfRef
,   GhfM
,   GhfAction
,   FileName
,   readGhf
,   modifyGhf
,   modifyGhf_
,   withGhf
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust )

type FileName   =   String

data Ghf        =   Ghf {
    window      ::  Window
,   uiManager   ::  UIManager
,   buffers     ::  [GhfBuffer]
} 

data GhfBuffer  =   GhfBuffer {
    fileName    :: Maybe FileName
,   bufferName  :: String
,   addedIndex  :: Int
,   sourceView  :: SourceView 
,   scrolledWindow  :: ScrolledWindow
}

instance Eq GhfBuffer
    where (==) a b = bufferName a == bufferName b && addedIndex a == addedIndex b

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


