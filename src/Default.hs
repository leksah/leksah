
-----------------------------------------------------------------------------
--
-- Module      :  Default
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for default values of a data type
--
------------------------------------------------------------------------------

module Default (
    Default(..)
) where

--
-- | A class for providing default values for certain types of editors
--
class Default alpha where
    getDefault      ::   alpha

instance Default Int where
    getDefault      =   1

instance Default alpha  => Default (Either alpha beta) where
        getDefault  =   Left getDefault

instance (Default alpha, Default beta) => Default (alpha, beta) where
        getDefault  =   (getDefault,getDefault)

instance (Default alpha, Default beta, Default gamma) => Default (alpha, beta, gamma) where
        getDefault  =   (getDefault,getDefault,getDefault)

instance Default [alpha] where
        getDefault  =   []

instance Default (Maybe alpha) where
    getDefault      =   Nothing

instance Default Bool where
    getDefault      =   True
