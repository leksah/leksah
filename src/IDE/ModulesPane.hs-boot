module IDE.ModulesPane (
    showModules
,   selectIdentifier
) where

import IDE.Core.State

selectIdentifier :: IdentifierDescr -> IDEAction

showModules :: IDEAction
