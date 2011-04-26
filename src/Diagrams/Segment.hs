{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , DeriveFunctor
           , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Segment
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic functionality for constructing and manipulating linear or
-- cubic Bezier segments.
--
-----------------------------------------------------------------------------

module Diagrams.Segment
       ( -- * Constructing segments

         Segment(..), straight, bezier3

         -- * Computing with segments
       , atParam, segOffset

       ) where

import Graphics.Rendering.Diagrams

import Data.VectorSpace

import Control.Applicative (liftA2)

------------------------------------------------------------
--  Constructing segments  ---------------------------------
------------------------------------------------------------

-- | The atomic constituents of paths are /segments/, which are single
--   straight lines or cubic Bezier curves.  Segments are
--   /translationally invariant/, that is, they have no particular
--   \"location\" and are unaffected by translations.  They are,
--   however, affected by other transformations such as rotations and
--   scales.
data Segment v = Linear v     -- ^ A linear segment with given offset.
               | Cubic v v v  -- ^ A cubic bezier segment specified by
                              --   three offsets from the starting
                              --   point to the first control point,
                              --   second control point, and ending
                              --   point, respectively.
  deriving (Show, Functor, Eq, Ord)

type instance V (Segment v) = v

instance HasLinearMap v => Transformable (Segment v) where
  transform = fmap . apply

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v -> Segment v
straight v = Linear v

-- Note, if we didn't have a Linear constructor we could also create
-- linear segments with @Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v@.  Those
-- would not be precisely the same, however, since we can actually
-- observe how segments are parametrized.

-- | @bezier3 v1 v2 v3@ constructs a translationally invariant cubic
--   Bezier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @v1@, @v2@, and @v3@.
bezier3 :: v -> v -> v -> Segment v
bezier3 = Cubic

-- | 'atParam' yields a parametrized view of segments as continuous
--   functions @[0,1] -> v@, which give the offset from the start of
--   the segment for each value of the parameter between @0@ and @1@.
--   It is designed to be used infix, like @seg `atParam` 0.5@.
atParam :: (VectorSpace v, Num (Scalar v)) => Segment v -> Scalar v -> v
atParam (Linear x) t       = t *^ x
atParam (Cubic c1 c2 x2) t =     (3 * t'*t'*t ) *^ c1
                             ^+^ (3 * t'*t *t ) *^ c2
                             ^+^ (    t *t *t ) *^ x2
  where t' = 1-t

-- | Compute the offset from the start of a segment to the
--   end.  Note that in the case of a Bezier segment this is /not/ the
--   same as the length of the curve itself; for that, see 'arcLength'.
segOffset :: Segment v -> v
segOffset (Linear v)    = v
segOffset (Cubic _ _ v) = v

------------------------------------------------------------
--  Computing segment bounds  ------------------------------
------------------------------------------------------------

{- 3 (1-t)^2 t c1 + 3 (1-t) t^2 c2 + t^3 x2

   Can we compute the projection of B(t) onto a given vector v?

   u.v = |u||v| cos th

   |proj_v u| = cos th * |u|
              = (u.v/|v|)

   so B_v(t) = (B(t).v/|v|)

   Then take the derivative of this wrt. t, get a quadratic, solve.

   B_v(t) = (1/|v|) *     -- note this does not affect max/min, can solve for t first
            3 (1-t)^2 t (c1.v) + 3 (1-t) t^2 (c2.v) + t^3 (x2.v)
          = t^3 ((3c1 - 3c2 + x2).v) + t^2 ((-6c1 + 3c2).v) + t (3c1.v)

   B_v'(t) = t^2 (3(3c1 - 3c2 + x2).v) + t (6(-2c1 + c2).v) + 3c1.v

   Set equal to zero, use quadratic formula.
-}

quadForm :: (Floating d, Ord d) => d -> d -> d -> [d]
quadForm a b c
  | d < 0     = []
  | d == 0    = [-b/(2*a)]
  | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]
 where d = b*b - 4*a*c

-- | The bounding function for a segment is based at the segment's
--   start.
instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Segment v) where

  getBounds (s@(Linear {})) = Bounds $ \v ->
    maximum . map (\t -> ((s `atParam` t) <.> v) / magnitudeSq v) $ [0,1]

  getBounds (s@(Cubic c1 c2 x2)) = Bounds $ \v ->
    maximum .
    map (\t -> ((s `atParam` t) <.> v) / magnitudeSq v) $
    [0,1] ++
    filter (liftA2 (&&) (>0) (<1))
      (quadForm (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ x2) <.> v))
                (6 * (((-2) *^ c1 ^+^ c2) <.> v))
                ((3 *^ c1) <.> v))

{- XXX TODO

   - add a function to compute the arc length of a segment
   - add functions for splitting segments into multiple segments
-}
