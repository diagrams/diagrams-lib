{-# LANGUAGE PackageImports, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  walck@lvc.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Transformations specific to two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Transform
       ( rotation
       , rotate
       , horizontalScale
       , verticalScale
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types

import Control.Arrow (first, second)

-- Do we want to rotate things in arbitrary dimensions?

rotation :: Angle -> Projective P2
rotation theta = fromLinear $ rot theta <-> rot (-theta)
  where
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)

rotate :: (TSpace t ~ P2, Transformable t) => Angle -> t -> t
rotate = transform . rotation

horizontalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
horizontalScale c = transform . fromLinear $ first (*c) <-> first (/c)

verticalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
verticalScale c = transform . fromLinear $ second (*c) <-> second (/c)
