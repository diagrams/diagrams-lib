{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
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
-- A /segment/ is a translation-invariant, atomic path.  There are two
-- types: linear (/i.e./ just a straight line to the endpoint) and
-- cubic Bézier curves (/i.e./ a curve to an endpoint with two control
-- points).  This module contains tools for creating and manipulating
-- segments, as well as a definition of segments with a fixed location
-- (useful for backend implementors).
--
-- Generally speaking, casual users of diagrams should not need this
-- module; the higher-level functionality provided by "Diagrams.Path"
-- should usually suffice instead.  However, directly manipulating
-- segments can occasionally be useful.
--
-----------------------------------------------------------------------------

module Diagrams.Segment
       ( -- * Constructing segments

         Segment(..), straight, bezier3

         -- * Fixed (absolutely located) segments
       , FixedSegment(..)
       , mkFixedSeg, fromFixedSeg

       ) where

import Diagrams.Core
import Diagrams.Parametric
import Diagrams.Solve
import Diagrams.Util

import Data.AffineSpace
import Data.VectorSpace

import Control.Applicative (liftA2)
import Data.Semigroup

------------------------------------------------------------
--  Constructing segments  ---------------------------------
------------------------------------------------------------

-- | The atomic constituents of paths are /segments/, which are single
--   straight lines or cubic Bézier curves.  Segments are
--   /translationally invariant/, that is, they have no particular
--   \"location\" and are unaffected by translations.  They are,
--   however, affected by other transformations such as rotations and
--   scales.
data Segment v = Linear v     -- ^ A linear segment with given offset.
               | Cubic v v v  -- ^ A cubic Bézier segment specified by
                              --   three offsets from the starting
                              --   point to the first control point,
                              --   second control point, and ending
                              --   point, respectively.
  deriving (Show, Functor, Eq, Ord)

type instance V (Segment v) = v

instance HasLinearMap v => Transformable (Segment v) where
  transform = fmap . apply

instance HasLinearMap v => Renderable (Segment v) NullBackend where
  render _ _ = mempty

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v -> Segment v
straight = Linear

-- Note, if we didn't have a Linear constructor we could also create
-- linear segments with @Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v@.  Those
-- would not be precisely the same, however, since we can actually
-- observe how segments are parametrized.

-- | @bezier3 v1 v2 v3@ constructs a translationally invariant cubic
--   Bézier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @v1@, @v2@, and @v3@.
bezier3 :: v -> v -> v -> Segment v
bezier3 = Cubic

type instance Codomain (Segment v) = v

-- | 'atParam' yields a parametrized view of segments as continuous
--   functions @[0,1] -> v@, which give the offset from the start of
--   the segment for each value of the parameter between @0@ and @1@.
--   It is designed to be used infix, like @seg ``atParam`` 0.5@.
instance (VectorSpace v, Num (Scalar v)) => Parametric (Segment v) where
  atParam (Linear x) t       = t *^ x
  atParam (Cubic c1 c2 x2) t =     (3 * t'*t'*t ) *^ c1
                               ^+^ (3 * t'*t *t ) *^ c2
                               ^+^ (    t *t *t ) *^ x2
    where t' = 1-t

instance  Num (Scalar v) => DomainBounds (Segment v) where

instance AdditiveGroup v => EndValues (Segment v) where
  atStart = const zeroV
  atEnd (Linear v) = v
  atEnd (Cubic _ _ v) = v

------------------------------------------------------------
--  Computing segment envelope  ------------------------------
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

-- | The envelope for a segment is based at the segment's start.
instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Segment v) where
  getEnvelope (s@(Linear {})) = mkEnvelope $ \v ->
    maximum . map (\t -> ((s `atParam` t) <.> v) / magnitudeSq v) $ [0,1]

  getEnvelope (s@(Cubic c1 c2 x2)) = mkEnvelope $ \v ->
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

instance (VectorSpace v, Fractional (Scalar v)) => Sectionable (Segment v) where
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
  
  reverseDomain (Linear v)       = Linear (negateV v)
  reverseDomain (Cubic c1 c2 x2) = Cubic (c2 ^-^ x2) (c1 ^-^ x2) (negateV x2)

-- | 'arcLength' @s m@ approximates the arc length of the segment curve @s@ with
--   accuracy of at least plus or minus @m@.  For a 'Cubic' segment this is computed
--   by subdividing until the arc length of the path through the control points is
--   within @m@ of distance from start to end.
instance (InnerSpace v, Floating (Scalar v), Ord (Scalar v))
      => ArcLength (Segment v) where
  arcLength (Linear x1) _ = magnitude x1
  arcLength s@(Cubic c1 c2 x2) m
    | ub - lb < m = (ub + lb) / 2
    | otherwise   = arcLength l m + arcLength r m
   where (l,r) = splitAtParam s 0.5
         ub    = sum (map magnitude [c1, c2 ^-^ c1, x2 ^-^ c2])
         lb    = magnitude x2

-- | @'arcLengthToParam' s l m@ converts the absolute arc length @l@,
--   measured from the segment starting point, to a parameter on the
--   segment @s@, with accuracy of at least plus or minus @m@.  Works
--   for /any/ arc length, and may return any parameter value (not
--   just parameters between 0 and 1).
instance (InnerSpace v, Floating (Scalar v), Ord (Scalar v), AdditiveGroup v)
      => ArcLengthToParam (Segment v) where
  arcLengthToParam s _ m | arcLength s m == 0 = 0.5
  arcLengthToParam s@(Linear {}) len m = len / arcLength s m
  arcLengthToParam s@(Cubic {})  len m
    | len == 0             = 0
    | len < 0              = - arcLengthToParam (fst (splitAtParam s (-1))) (-len) m
    | abs (len - slen) < m = 1
    | len > slen           = 2 * arcLengthToParam (fst (splitAtParam s 2)) len m
    | len < ll             = (*0.5) $ arcLengthToParam l len m
    | otherwise            = (+0.5) . (*0.5) $ arcLengthToParam r (len - ll) m
    where (l,r) = splitAtParam s 0.5
          ll    = arcLength l m
          slen  = arcLength s m

-- Note, the above seems to be quite slow since it duplicates a lot of
-- work.  We could trade off some time for space by building a tree of
-- parameter values (up to a certain depth...)

instance (InnerSpace v, OrderedField (Scalar v)) => Adjustable (Segment v) where
  adjust s opts = section s
      (if adjSide opts == End   then 0 else getParam s)
      (if adjSide opts == Start then 0 else 1 - getParam (reverseDomain s))
    where
      getParam seg = case adjMethod opts of
        ByParam p -> -p * bothCoef
        ByAbsolute len -> param (-len * bothCoef)
        ToAbsolute len -> param (absDelta len * bothCoef)
        where
          param l = arcLengthToParam seg l eps
          absDelta len = arcLength s eps - len
      bothCoef = if adjSide opts == Both then 0.5 else 1
      eps = adjEps opts

------------------------------------------------------------
--  Fixed segments
------------------------------------------------------------

-- | @FixedSegment@s are like 'Segment's except that they have
--   absolute locations.
data FixedSegment v = FLinear (Point v) (Point v)
                    | FCubic (Point v) (Point v) (Point v) (Point v)
  deriving Show

type instance V (FixedSegment v) = v

instance HasLinearMap v => Transformable (FixedSegment v) where
  transform t (FLinear p1 p2)
    = FLinear
      (transform t p1)
      (transform t p2)

  transform t (FCubic p1 c1 c2 p2)
    = FCubic
      (transform t p1)
      (transform t c1)
      (transform t c2)
      (transform t p2)

instance VectorSpace v => HasOrigin (FixedSegment v) where
  moveOriginTo o (FLinear p1 p2)
    = FLinear
      (moveOriginTo o p1)
      (moveOriginTo o p2)

  moveOriginTo o (FCubic p1 c1 c2 p2)
    = FCubic
      (moveOriginTo o p1)
      (moveOriginTo o c1)
      (moveOriginTo o c2)
      (moveOriginTo o p2)

instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (FixedSegment v) where
  getEnvelope f = moveTo p (getEnvelope s)
    where (p, s) = fromFixedSeg f

    -- Eventually we might decide it's cleaner/more efficient (?) to
    -- have all the computation in the FixedSegment instance of
    -- Envelope, and implement the Segment instance in terms of it,
    -- instead of the other way around

-- | Create a 'FixedSegment' from a starting point and a 'Segment'.
mkFixedSeg :: AdditiveGroup v => Point v -> Segment v -> FixedSegment v
mkFixedSeg p (Linear v)       = FLinear p (p .+^ v)
mkFixedSeg p (Cubic c1 c2 x2) = FCubic p (p .+^ c1) (p .+^ c2) (p .+^ x2)

-- | Decompose a 'FixedSegment' into a starting point and a 'Segment'.
fromFixedSeg :: AdditiveGroup v => FixedSegment v -> (Point v, Segment v)
fromFixedSeg (FLinear p1 p2)      = (p1, Linear (p2 .-. p1))
fromFixedSeg (FCubic x1 c1 c2 x2) = (x1, Cubic (c1 .-. x1) (c2 .-. x1) (x2 .-. x1))

type instance Codomain (FixedSegment v) = Point v

instance VectorSpace v => Parametric (FixedSegment v) where
  atParam (FLinear p1 p2) t = alerp p1 p2 t
  atParam (FCubic x1 c1 c2 x2) t = p3
    where p11 = alerp x1 c1 t
          p12 = alerp c1 c2 t
          p13 = alerp c2 x2 t

          p21 = alerp p11 p12 t
          p22 = alerp p12 p13 t

          p3  = alerp p21 p22 t