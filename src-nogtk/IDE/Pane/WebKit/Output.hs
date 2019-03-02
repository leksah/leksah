module IDE.Pane.WebKit.Output where

import Data.Text (Text)
import IDE.Core.Types (IDEAction)

setOutput :: Text -> Text -> IDEAction
setOutput _ _ = return ()

showOutputPane :: IDEAction
showOutputPane = return ()

loadOutputUri :: FilePath -> IDEAction
loadOutputUri _ = return ()

loadOutputHtmlFile :: FilePath -> IDEAction
loadOutputHtmlFile _ = return ()
