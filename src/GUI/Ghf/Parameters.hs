-----------------------------------------------------------------------------
--
-- Module      :  GUI.Ghf.Parameters
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for parameters for editors
--
-----------------------------------------------------------------------------------

module GUI.Ghf.Parameters (
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

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Show)

data HorizontalAlign =   StartHorizontal | StopHorizontal | Keep

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

--
-- | Convenience method to get a parameter, or if not set the default parameter
--
getParameter :: (Parameter -> (Maybe beta)) -> Parameters -> beta
getParameter selector parameters =
    case getParameterPrim selector parameters of
        Just ele       -> ele
        _              -> case getParameterPrim selector defaultParameters of
                            Just ele       -> ele
                            _              -> error "default parameter not defined"

getParameterPrim :: (Parameter -> (Maybe beta)) -> Parameters -> Maybe beta
getParameterPrim selector parameters =
    case filter isJust $ map selector parameters of
        (Just ele) : _ -> Just ele
        _              -> Nothing

(<<<-) :: (Parameter -> (Maybe beta)) -> Parameter -> Parameters -> Parameters
(<<<-) selector para  = \params -> para : filter (isJust . selector) params


defaultParameters =
    [   ParaName ""
    ,   ParaSynopsis ""
    ,   ParaDirection Horizontal
    ,   ParaShadow ShadowNone
    ,   ParaOuterAlignment  (0.5, 0.5, 0.95, 0.95)
    ,   ParaOuterPadding    (2, 5, 3, 3)
    ,   ParaInnerAlignment  (0.5, 0.5, 0.95, 0.95)
    ,   ParaInnerPadding    (2, 5, 3, 3)
    ,   ParaMinSize         (-1,-1)
    ,   ParaHorizontal      Keep]
