-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.CabalUtils
-- Copyright   :  2007-2016 Juergen Nicklisch-Franken, Hamish Mackenzie, Jacco Krijnen, JP Moresmau
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.CabalUtils (
    writeGenericPackageDescription'
) where

import Distribution.PackageDescription
       (GenericPackageDescription(..))
import Distribution.Simple.Utils (writeUTF8File)
import Distribution.PackageDescription.PrettyPrint
       (showGenericPackageDescription)

-- | Version of writeGenericPackageDescription that drops trailing spaces
writeGenericPackageDescription' :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription' f = writeUTF8File f . dropTrailingSpaces . showGenericPackageDescription
  where
    dropTrailingSpaces = unlines . map (reverse . dropWhile (==' ') . reverse) . lines


