module IDE.Gtk.Package where

import Control.Lens ((%~))
import IDE.Core.State
       (modifyIDE_, prefs, Prefs(..), IDEAction, PackageAction, DebugAction)
import IDE.Package
       (packageRunJavaScript', packageRun', interruptSaveAndRun,
        tryDebug')

packageRun :: PackageAction
packageRun = interruptSaveAndRun $ packageRun' $ Just $
    return False

packageRunJavaScript :: PackageAction
packageRunJavaScript = interruptSaveAndRun $ packageRunJavaScript' $ Just $
    return False

tryDebug :: DebugAction -> PackageAction
tryDebug = tryDebug' (return False)

backgroundBuildToggled :: IDEAction
backgroundBuildToggled =
    modifyIDE_ $ prefs %~ (\p -> p{backgroundBuild = not $ backgroundBuild p})

makeDocsToggled :: IDEAction
makeDocsToggled =
    modifyIDE_ $ prefs %~ (\p -> p{makeDocs = not $ makeDocs p})

runUnitTestsToggled :: IDEAction
runUnitTestsToggled =
    modifyIDE_ $ prefs %~ (\p -> p{runUnitTests = not $ runUnitTests p})

runBenchmarksToggled :: IDEAction
runBenchmarksToggled =
    modifyIDE_ $ prefs %~ (\p -> p{runBenchmarks = not $ runBenchmarks p})

nativeToggled :: IDEAction
nativeToggled =
    modifyIDE_ $ prefs %~ (\p -> p{native = not $ native p})

javaScriptToggled :: IDEAction
javaScriptToggled =
    modifyIDE_ $ prefs %~ (\p -> p{javaScript = not $ javaScript p})

makeModeToggled :: IDEAction
makeModeToggled =
    modifyIDE_ $ prefs %~ (\p -> p{makeMode = not $ makeMode p})
