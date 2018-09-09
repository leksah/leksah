{-# OPTIONS_GHC -Wall #-}
module IDE.Brittany (
  runBrittany
) where

import Prelude ()
import Prelude.Compat
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens (runIdentity)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import Data.Coerce (coerce)
import Data.Semigroup (Option(..), Last(..))
import Data.Maybe (maybeToList)
import Data.Text (Text)

import System.Directory (setCurrentDirectory)

import Language.Haskell.Brittany
       (parsePrintModule, readConfigsWithUserConfig, staticDefaultConfig,
        forwardOptionsSyntaxExtsEnabled, BrittanyError(..),
        CForwardOptions(..), CConfig(..), CLayoutConfig(..),
        findLocalConfigPath)

import IDE.Core.Types (IDE(prefs), MonadIDE, Prefs(tabWidth))
import IDE.Core.State (readIDE)

runBrittany :: MonadIDE m
            => Maybe FilePath   -- ^ package location
            -> Text             -- ^ text to format
            -> m (Either [BrittanyError] Text)
runBrittany packageDir text = do
  tw <- tabWidth <$> readIDE prefs
  liftIO $ do
    mapM_ setCurrentDirectory packageDir -- Hopefully this is not necessary
    confPath <- maybe (return Nothing)  findLocalConfigPath packageDir
    let cfg = mempty
                { _conf_layout =
                    mempty { _lconfig_indentAmount = opt (coerce tw)
                           }
                , _conf_forward =
                    (mempty :: CForwardOptions Option)
                      { _options_ghc = opt (runIdentity ( _options_ghc forwardOptionsSyntaxExtsEnabled))
                      }
                }

    config <- fromMaybeT (pure staticDefaultConfig) (readConfigsWithUserConfig cfg (maybeToList confPath))
    parsePrintModule config text

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def act = runMaybeT act >>= maybe def return

opt :: a -> Option a
opt = Option . Just

