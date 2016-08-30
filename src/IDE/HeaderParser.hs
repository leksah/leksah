{-# LANGUAGE CPP, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.HeaderParser
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.HeaderParser (

    parseTheHeader

) where

import Control.Applicative
import Prelude hiding (readFile)
import IDE.Core.CTypes hiding(SrcSpan(..))
import GHC hiding (ImportDecl)
import FastString(unpackFS)
import IDE.Utils.GHCUtils
import Data.Maybe (mapMaybe)
#if MIN_VERSION_ghc(8,0,0)
import BasicTypes (StringLiteral(..))
#endif
#if MIN_VERSION_ghc(7,4,1)
import Outputable(pprPrefixOcc, ppr)
#else
import Outputable(pprHsVar, ppr)
#endif
#if MIN_VERSION_ghc(7,6,0)
import Outputable(showSDoc)
#else
import qualified Outputable as O
#endif
import IDE.Utils.FileUtils (figureOutHaddockOpts)
import Control.Monad.IO.Class (MonadIO(..))
import System.IO.Strict (readFile)
import qualified Data.Text as T (pack)

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> O.SDoc -> String
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> O.SDoc -> String
showSDocUnqual _ = O.showSDocUnqual
#endif

#if MIN_VERSION_ghc(7,10,0)
unLoc710 :: GenLocated l e -> e
unLoc710 = unLoc
#else
unLoc710 :: a -> a
unLoc710 = id
#endif

showRdrName :: DynFlags -> RdrName -> String
showRdrName dflags r = showSDoc dflags (ppr r)

parseTheHeader :: FilePath -> IO ServerAnswer
parseTheHeader filePath = do
    text        <- readFile filePath
    opts        <- figureOutHaddockOpts
    parseResult <- liftIO $ myParseHeader filePath text (filterOpts opts)
    case parseResult of
        Left str                                      -> return (ServerFailed str)
        Right (_, pr@HsModule{ hsmodImports = []})       -> do
            let i = case hsmodDecls pr of
                        decls@(_hd:_tl) -> foldl (\ a b -> min a (srcSpanStartLine' (getLoc b))) 0 decls - 1
                        [] -> case hsmodExports pr of
                            Just list -> foldl (\ a b -> max a (srcSpanEndLine' (getLoc b))) 0 (unLoc710 list) + 1
                            Nothing -> case hsmodName pr of
                                        Nothing -> 0
                                        Just mn -> srcSpanEndLine' (getLoc mn) + 2
            return (ServerHeader (Right i))
        Right (dflags, _pr@HsModule{ hsmodImports = imports }) -> return (ServerHeader (Left (transformImports dflags imports)))
  where
    filterOpts []    = []
    filterOpts (o:_:r) | o `elem` ["-link-js-lib", "-js-lib-outputdir", "-js-lib-src", "-package-id"] = filterOpts r
    filterOpts (o:r) = o:filterOpts r

transformImports :: DynFlags -> [LImportDecl RdrName] -> [ImportDecl]
transformImports dflags = map (transformImport dflags)

transformImport :: DynFlags -> LImportDecl RdrName -> ImportDecl
transformImport dflags (L srcSpan importDecl) =
    ImportDecl {
        importLoc = srcSpanToLocation srcSpan,
        importModule = T.pack modName,
        importQualified = ideclQualified importDecl,
        importSrc = ideclSource importDecl,
        importPkg = T.pack <$> pkgQual,
        importAs  = T.pack <$> impAs,
        importSpecs = specs}
    where
        modName =  moduleNameString $ unLoc $ ideclName importDecl
        pkgQual =  case ideclPkgQual importDecl of
                        Nothing -> Nothing
#if MIN_VERSION_ghc(8,0,0)
                        Just fs -> Just (sl_st fs)
#else
                        Just fs -> Just (unpackFS fs)
#endif
        impAs   =  case ideclAs importDecl of
                        Nothing -> Nothing
                        Just mn -> Just (moduleNameString mn)
        specs =    case ideclHiding importDecl of
                        Nothing -> Nothing
                        Just (hide, list) -> Just (ImportSpecList hide (mapMaybe (transformEntity dflags) (unLoc710 list)))

transformEntity :: DynFlags -> LIE RdrName -> Maybe ImportSpec
#if MIN_VERSION_ghc(7,2,0)
transformEntity dflags (L _ (IEVar name))              = Just (IVar (T.pack $ showSDoc dflags (pprPrefixOcc $ unLoc710 name)))
#else
transformEntity dflags (L _ (IEVar name))              = Just (IVar (T.pack $ showSDoc dflags (pprHsVar name)))
#endif
transformEntity dflags (L _ (IEThingAbs name))         = Just (IAbs (T.pack . showRdrName dflags $ unLoc710 name))
transformEntity dflags (L _ (IEThingAll name))         = Just (IThingAll (T.pack . showRdrName dflags $ unLoc710 name))
#if MIN_VERSION_ghc(8,0,0)
transformEntity dflags (L _ (IEThingWith name _ list _)) = Just (IThingWith (T.pack . showRdrName dflags $ unLoc710 name)
#else
transformEntity dflags (L _ (IEThingWith name list))   = Just (IThingWith (T.pack . showRdrName dflags $ unLoc710 name)
#endif
                                                        (map (T.pack . showRdrName dflags . unLoc710) list))
transformEntity _ _                              = Nothing

#if MIN_VERSION_ghc(7,2,0)
srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation (RealSrcSpan span')
    =   Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span')
srcSpanToLocation _ = error "srcSpanToLocation: unhelpful span"

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' (RealSrcSpan span') = srcSpanStartLine span'
srcSpanStartLine' _ = error "srcSpanStartLine': unhelpful span"

srcSpanEndLine' :: SrcSpan -> Int
srcSpanEndLine' (RealSrcSpan span') = srcSpanEndLine span'
srcSpanEndLine' _ = error "srcSpanEndLine': unhelpful span"
#else
srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation span' | not (isGoodSrcSpan span')
    =   error "srcSpanToLocation: unhelpful span"
srcSpanToLocation span'
    =   Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span')

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' = srcSpanStartLine

srcSpanEndLine' :: SrcSpan -> Int
srcSpanEndLine' = srcSpanEndLine
#endif
