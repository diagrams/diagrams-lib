{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- XXX comment me
--
-----------------------------------------------------------------------------

module Diagrams.Path where

import Graphics.Rendering.Diagrams

import Data.VectorSpace

import Data.Monoid

------------------------------------------------
-- Paths

-- Segments are *translationally invariant.*

data Segment v = Linear v
               | Cubic v v v
  deriving Show

-- Note, this Transformable instance is a bit strange, since it
-- ignores translational components.

instance (Transformable v, AdditiveGroup v) => Transformable (Segment v) where
  type TSpace (Segment v) = TSpace v
  transform t (Linear v) = Linear (transform t v ^-^ transform t zeroV)
  transform t (Cubic v1 v2 v3) = Cubic (transform t v1 ^-^ z)
                                       (transform t v2 ^-^ z)
                                       (transform t v3 ^-^ z)
    where z = transform t zeroV

straight :: (VectorSpace v, Fractional (Scalar v)) => v -> Segment v
straight v = Linear v
     -- Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v

bezier3 :: v -> v -> v -> Segment v
bezier3 = Cubic

pointAt :: (VectorSpace v, Num (Scalar v)) => Scalar v -> Segment v -> v
pointAt t (Linear x)       = t *^ x
pointAt t (Cubic c1 c2 x2) = (3 * (1-t)^2 * t) *^ c1
                          ^+^ (3 * (1-t) * t^2) *^ c2
                          ^+^ t^3 *^ x2

{- (1-t)^2 t c1 + (1-t) t^2 c2 + t^3 x2

   Can we compute the projection of B(t) onto a given vector v?

   u.v = |u||v| cos th

   |proj_v u| = cos th * |u|
              = (u.v/|v|)

   so B_v(t) = (B(t).v/|v|)

   Then take the derivative of this wrt. t, get a quadratic, solve.

   B_v(t) = (1/|v|) *     -- note this does not affect max/min, can solve for t first
            (1-t)^2 t (c1.v) + (1-t) t^2 (c2.v) + t^3 (x2.v)
          = t^3 ((c1 - c2 + x2).v) + t^2 ((-2c1 + c2).v) + t (c1.v)

   B_v'(t) = t^2 (3(c1 - c2 + x2).v) + t (2(-2c1 + c2).v) + c1.v

   Set equal to zero, use quadratic formula.
-}

quadForm :: (Floating d, Ord d) => d -> d -> d -> [d]
quadForm a b c
  | d < 0     = []
  | d == 0    = [-b/(2*a)]
  | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]
 where d = b*b - 4*a*c

segmentBounds :: (InnerSpace v, Ord (Scalar v), Floating (Scalar v))
              => Segment v -> Bounds v
segmentBounds s@(Linear x1) v      =
  maximum . map (\t -> (pointAt t s <.> v) / magnitude v) $ [0,1]
segmentBounds s@(Cubic c1 c2 x2) v =
  maximum .
  map (\t -> (pointAt t s <.> v) / magnitude v) $
  [0,1] ++
  quadForm (3 * ((c1 ^-^ c2 ^+^ x2) <.> v))
         (2 * (((-2) *^ c1 ^+^ c2) <.> v))
         (c1 <.> v)

-- A *translationally invariant* relative path.
newtype RelPath v = RelPath { getSegments :: [Segment v] }

-- Relative paths form a monoid under path concatenation.

instance Monoid (RelPath v) where
  mempty = RelPath []
  (RelPath b1) `mappend` (RelPath b2) = RelPath (b1 ++ b2)

instance (AdditiveGroup v, Transformable v) => Transformable (RelPath v) where
  type TSpace (RelPath v)  = TSpace v
  transform t = RelPath . map (transform t) . getSegments