module IDE.Pane.WebKit.Documentation where

import Data.Text (Text)
import IDE.Core.Types (IDEAction)

setOutput :: Text -> Text -> IDEAction
setOutput _ _ = return ()

showDocumentationPane :: IDEAction
showDocumentationPane = return ()

loadDoc :: Text -> IDEAction
loadDoc _ = return ()

reloadDoc :: IDEAction
reloadDoc = return ()
