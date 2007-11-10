-----------------------------------------------------------------------------
--
-- Module      :  Ghf.DescriptionPP
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Description with additional fileds for printing and parsing
--
-----------------------------------------------------------------------------------
module Ghf.DescriptionPP (
    Applicator
,   FieldDescriptionPP(..)
,   mkFieldPP

) where

--import GUI.Ghf.MakeEditor
import Graphics.UI.Gtk hiding (Event)
import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec as P

import Ghf.PrinterParser hiding (fieldParser,parameters)
import GUI.Ghf.Parameters
import GUI.Ghf.EditorBasics
import Ghf.Core

type Applicator alpha = alpha -> GhfAction

data FieldDescriptionPP alpha =  FDPP {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> P.CharParser () alpha
    ,   fieldEditor     ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    ,   applicator      ::  alpha -> alpha -> GhfAction
    }

type MkFieldDescriptionPP alpha beta =
    Parameters      ->
    (Printer beta)     ->
    (Parser beta)      ->
    (Getter alpha beta)    ->
    (Setter alpha beta)    ->
    (Editor beta)      ->
    (Applicator beta)  ->
    FieldDescriptionPP alpha

mkFieldPP :: Eq beta => MkFieldDescriptionPP alpha beta
mkFieldPP parameters printer parser getter setter editor applicator =
    FDPP parameters
        (\ dat -> (PP.text (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case getParameterPrim paraSynopsis parameters of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> P.try (do
            symbol (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str)
            colon
            val <- parser
            return (setter val dat)))
        (\ dat -> do
            (widget, inj,ext,noti) <- editor parameters
            inj (getter dat)
            noti FocusOut (Left (\e -> do
                putStrLn "Handling Focus out"
                v <- ext
                case v of
                    Just _ -> do
                        widgetModifyFg widget StateNormal (Color 0 0 0)
                        return False
                    Nothing -> do
                        widgetModifyFg widget StateNormal (Color 65535 65535 0)
                        return False))
            return (widget,
                    (\a -> inj (getter a)),
                    (\a -> do
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    noti))
        (\ newDat oldDat -> do --appicator
            let newField = getter newDat
            let oldField = getter oldDat
            if newField == oldField
                then return ()
                else applicator newField)


