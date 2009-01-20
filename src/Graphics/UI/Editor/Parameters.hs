-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Parameters
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for parameters for editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Parameters (
    Parameters
,   Parameter(..)
,   paraName
,   paraSynopsis
,   paraDirection
,   paraShadow
,   paraOuterAlignment
,   paraInnerAlignment
,   paraOuterPadding
,   paraInnerPadding
,   paraMinSize
,   paraHorizontal
,   paraStockId
,   paraMultiSel
,   paraPack

,   getParameter
,   getParameterPrim
,   (<<<-)
,   emptyParams
,   Direction(..)
,   HorizontalAlign(..)
) where

import Graphics.UI.Gtk
import Data.Maybe
import qualified Data.List as List

import IDE.Exception

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Show)

data HorizontalAlign =   StartHorizontal | StopHorizontal | Keep
    deriving (Eq,Show)
--
-- | A type for parameters for editors
--
type Parameters     =   [Parameter]

data Parameter      =   ParaName String
                    |   ParaSynopsis String
                    |   ParaDirection Direction
                    |   ParaShadow ShadowType
                    |   ParaOuterAlignment  (Float,Float,Float,Float)
                                               -- | xalign yalign xscale yscale
                    |   ParaOuterPadding    (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    |   ParaInnerAlignment  (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                    |   ParaInnerPadding   (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    |   ParaMinSize         (Int, Int)
                    |   ParaHorizontal      HorizontalAlign
                    |   ParaStockId String
                    |   ParaMultiSel Bool
                    |   ParaPack Packing
    deriving (Eq,Show)


{--        #if MIN_VERSION_gtk(0,9,13)
            -- now defined in gtk
        #else
        instance Show ShadowType
            where show _    =   "Any Shadow"
        #endif
--}

emptyParams         ::   [Parameter]
emptyParams         =   []

paraName                        ::   (Parameter -> (Maybe String))
paraName (ParaName str)         =   Just str
paraName _                      =   Nothing

paraSynopsis                    ::   (Parameter -> (Maybe String))
paraSynopsis (ParaSynopsis str) =   Just str
paraSynopsis _                  =   Nothing

paraDirection                   ::   (Parameter -> (Maybe Direction))
paraDirection (ParaDirection d) =   Just d
paraDirection _                 =   Nothing

paraShadow                      ::   (Parameter -> (Maybe ShadowType))
paraShadow (ParaShadow d)       =   Just d
paraShadow _                    =   Nothing

paraOuterAlignment              ::   (Parameter -> (Maybe (Float,Float,Float,Float)))
paraOuterAlignment (ParaOuterAlignment d) = Just d
paraOuterAlignment _            =   Nothing

paraInnerAlignment              ::   (Parameter -> (Maybe (Float,Float,Float,Float)))
paraInnerAlignment (ParaInnerAlignment d) = Just d
paraInnerAlignment _            =   Nothing

paraOuterPadding                ::   (Parameter -> (Maybe (Int,Int,Int,Int)))
paraOuterPadding (ParaOuterPadding d) = Just d
paraOuterPadding _              =   Nothing

paraInnerPadding                ::   (Parameter -> (Maybe (Int,Int,Int,Int)))
paraInnerPadding (ParaInnerPadding d) = Just d
paraInnerPadding _              =   Nothing

paraMinSize                     ::   (Parameter -> (Maybe (Int, Int)))
paraMinSize (ParaMinSize d)     =   Just d
paraMinSize _                   =   Nothing

paraHorizontal                  ::   (Parameter -> (Maybe (HorizontalAlign)))
paraHorizontal (ParaHorizontal d) =   Just d
paraHorizontal _                =   Nothing

paraStockId                     ::   (Parameter -> (Maybe String))
paraStockId (ParaStockId str)   =   Just str
paraStockId _                   =   Nothing

paraMultiSel                    ::   (Parameter -> (Maybe Bool))
paraMultiSel (ParaMultiSel b)   =   Just b
paraMultiSel _                  =   Nothing

paraPack                        ::   (Parameter -> (Maybe Packing))
paraPack (ParaPack b)           =   Just b
paraPack _                      =   Nothing

--
-- | Convenience method to get a parameter, or if not set the default parameter
--
getParameter :: (Parameter -> (Maybe beta)) -> Parameters -> beta
getParameter selector parameter =
    case getParameterPrim selector parameter of
        Just ele       -> ele
        _              -> case getParameterPrim selector defaultParameters of
                            Just ele       -> ele
                            _              -> throwIDE "default parameter not defined"

getParameterPrim :: (Parameter -> (Maybe beta)) -> Parameters -> Maybe beta
getParameterPrim selector parameter =
    case filter isJust $ map selector parameter of
        (Just ele) : _ -> Just ele
        _              -> Nothing

(<<<-) :: (Parameter -> (Maybe beta)) -> Parameter -> Parameters -> Parameters
(<<<-) selector para  params = para : filter (isNothing . selector) params

defaultParameters :: Parameters
defaultParameters =
    [   ParaName ""
    ,   ParaStockId ""
    ,   ParaSynopsis ""
    ,   ParaDirection Horizontal
    ,   ParaShadow ShadowNone
    ,   ParaOuterAlignment  (0.4, 0.5, 1.0, 0.7)
    ,   ParaOuterPadding    (5, 5, 5, 5)
    ,   ParaInnerAlignment  (0.4, 0.5, 1.0, 0.7)
    ,   ParaInnerPadding    (5, 5, 5, 5)
    ,   ParaMinSize         (-1,-1)
    ,   ParaHorizontal      Keep
    ,   ParaMultiSel True
    ,   ParaPack PackNatural
    ]

