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
       , splitAtParam, arcLength

       , arcLengthToParam
       , adjustSegmentToParams

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Solve

import Data.VectorSpace

import Control.Applicative (liftA2, (<$>))

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
straight = Linear

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
--   It is designed to be used infix, like @seg ``atParam`` 0.5@.
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

------------------------------------------------------------
--  Manipulating segments
------------------------------------------------------------

-- | 'splitAtParam' splits a segment @s@ into two new segments @(l,r)@
--   at the parameter @t@ where @l@ corresponds to the portion of
--   @s@ for parameter values from @0@ to @t@ and @r@ for @s@ from @t@ to @1@.
--   The following should hold for splitting:
--
-- > paramSplit s t u
-- >   | u < t     = atParam s u == atParam l (u / t)
-- >   | otherwise = atParam s u == atParam s t ^+^ atParam l ((u - t) / (1.0 - t))
-- >   where (l,r) = splitAtParam s t
--
--   That is to say, the parameterization scales linearly with splitting.
--
--   'splitAtParam' can also be used with parameters outside the range
--   (0,1).  For example, using the parameter @2@ gives two result
--   segments where the first is the original segment extended to the
--   parameter 2, and the second result segment travels /backwards/
--   from the end of the first to the end of the original segment.
splitAtParam :: (VectorSpace v) => Segment v -> Scalar v -> (Segment v, Segment v)
splitAtParam (Linear x1) t = (left, right)
  where left  = Linear p
        right = Linear (x1 ^-^ p)
        p = lerp zeroV x1 t
splitAtParam (Cubic c1 c2 x2) t = (left, right)
  where left  = Cubic a b e
        right = Cubic (c ^-^ e) (d ^-^ e) (x2 ^-^ e)
        p = lerp c1    c2 t
        a = lerp zeroV c1 t
        b = lerp a     p  t
        d = lerp c2    x2 t
        c = lerp p     d  t
        e = lerp b     c  t

-- | 'arcLength' @s m@ approximates the arc length of the segment curve @s@ with
--   accuracy of at least plus or minus @m@.  For a 'Cubic' segment this is computed
--   by subdividing until the arc length of the path through the control points is
--   within @m@ of distance from start to end.
arcLength :: (InnerSpace v, Floating (Scalar v), Ord (Scalar v))
          => Segment v -> Scalar v -> Scalar v
arcLength (Linear x1) _ = magnitude x1
arcLength s@(Cubic c1 c2 x2) m
  | ub - lb < m = (ub + lb) / 2
  | otherwise   = arcLength l m + arcLength r m
 where (l,r) = splitAtParam s 0.5
       ub    = sum (map magnitude [c1, c2 ^-^ c1, x2 ^-^ c2])
       lb    = magnitude x2

-- | @'arcLengthToParam' s l m@ converts the absolute arc length @l@ to
--   a parameter on the segment @s@, with accuracy of at least plus or
--   minus @m@.
arcLengthToParam :: (InnerSpace v, Floating (Scalar v), Ord (Scalar v))
                 => Segment v -> Scalar v -> Scalar v -> Maybe (Scalar v)
arcLengthToParam s len m
  |  len < 0 || len > arcLength s m = Nothing
arcLengthToParam s@(Linear {}) len m = Just $ len / arcLength s m
arcLengthToParam s@(Cubic {})  len m
  | abs (len - ll) < m = Just 0.5
  | len < ll           = (*0.5) <$> arcLengthToParam l len m
  | otherwise          = (+0.5) . (*0.5) <$> arcLengthToParam r (len - ll) m
  where (l,r) = splitAtParam s 0.5
        ll    = arcLength l m

--------------------------------------------------
--  Adjusting segment length
--------------------------------------------------

data AdjustMethod v = ByParam (Scalar v)
                    | ByAbsolute (Scalar v)
                    | ToAbsolute (Scalar v)

data AdjustSide = Start
                | End
                | Both

data AdjustOpts v = ALO { adjMethod :: AdjustMethod v
                        , adjSide   :: AdjustSide
                        }

-- adjustSegment :: Segment v -> AdjustOpts -> Segment v
-- adjustSegment s

adjustSegmentToParams :: (Fractional (Scalar v), VectorSpace v)
                      => Segment v -> Scalar v -> Scalar v -> Segment v
adjustSegmentToParams s p1 p2 = snd (splitAtParam (fst (splitAtParam s p2)) (p1/p2))