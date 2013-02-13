{-# LANGUAGE FlexibleContexts
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Ellipse
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional ellipses (and, as a special case, circles).
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Ellipse
    (
      -- * Ellipse and circle diagrams
      unitCircle
    , circle
    , ellipse
    , ellipseXY
    ) where

import Data.VectorSpace (Scalar(..))
import Data.Basis       (HasBasis(..), Basis(..))
import Data.MemoTrie    (HasTrie(..))

import Diagrams.Core

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Arc

import Diagrams.Path
import Diagrams.Util

-- | A circle of radius 1, with center at the origin.
unitCircle :: forall a p. ( Ord a
                          , Floating a
                          , RealFrac a
                          , HasBasis a
                          , HasTrie (Basis a)
                          , a ~ Scalar a
                          , PathLike p
                          , V p ~ V2 a
                          ) => p
unitCircle = pathLike (p2 (1,0)) True $ trailSegments (arcT 0 (tau::Rad a))

-- | A circle of the given radius, centered at the origin.  As a path,
--   it begins at (r,0).
circle :: ( Ord a
          , Floating a
          , RealFrac a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , PathLike p
          , V p ~ V2 a
          , Transformable p
          ) => a -> p
circle d = unitCircle # scale d

-- | @ellipse e@ constructs an ellipse with eccentricity @e@ by
--   scaling the unit circle in the X direction.  The eccentricity must
--   be within the interval [0,1).
ellipse :: ( Ord a
           , Floating a
           , RealFrac a
           , HasBasis a
           , HasTrie (Basis a)
           , a ~ Scalar a
           , PathLike p
           , V p ~ V2 a
           , Transformable p
           ) => a -> p
ellipse e
    | e >= 0 && e < 1  = scaleX (sqrt (1 - e*e)) unitCircle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | @ellipseXY x y@ creates an axis-aligned ellipse, centered at the
--   origin, with radius @x@ along the x-axis and radius @y@ along the
--   y-axis.
ellipseXY :: ( Ord a
             , Floating a
             , RealFrac a
             , HasBasis a
             , HasTrie (Basis a)
             , a ~ Scalar a
             , PathLike p
             , V p ~ V2 a
             , Transformable p
             ) => a -> a -> p
ellipseXY x y = unitCircle # scaleX x # scaleY y
