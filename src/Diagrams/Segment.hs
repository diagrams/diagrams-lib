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

         -- * Computing with segments
       , atParam, segOffset
       , reverseSegment
       , splitAtParam, arcLength
       , arcLengthToParam

         -- * Adjusting segments
       , adjustSegment
       , AdjustOpts(..)
       , AdjustMethod(..)
       , AdjustSide(..)

       , adjustSegmentToParams

         -- * Fixed (absolutely located) segments
       , FixedSegment(..)
       , mkFixedSeg
       , fAtParam

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Solve
import Diagrams.Util

import Data.Default
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
--   end.  Note that in the case of a Bézier segment this is /not/ the
--   same as the length of the curve itself; for that, see 'arcLength'.
segOffset :: Segment v -> v
segOffset (Linear v)    = v
segOffset (Cubic _ _ v) = v

-- | Reverse the direction of a segment.
reverseSegment :: AdditiveGroup v => Segment v -> Segment v
reverseSegment (Linear v)       = Linear (negateV v)
reverseSegment (Cubic c1 c2 x2) = Cubic (c2 ^-^ x2) (c1 ^-^ x2) (negateV x2)

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

-- | @'arcLengthToParam' s l m@ converts the absolute arc length @l@,
--   measured from the segment starting point, to a parameter on the
--   segment @s@, with accuracy of at least plus or minus @m@.  Works
--   for /any/ arc length, and may return any parameter value (not
--   just parameters between 0 and 1).
arcLengthToParam :: (InnerSpace v, Floating (Scalar v), Ord (Scalar v), AdditiveGroup v)
                 => Segment v -> Scalar v -> Scalar v -> Scalar v
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

--------------------------------------------------
--  Adjusting segment length
--------------------------------------------------

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod v = ByParam (Scalar v)     -- ^ Extend by the given parameter value
                                             --   (use a negative parameter to shrink)
                    | ByAbsolute (Scalar v)  -- ^ Extend by the given arc length
                                             --   (use a negative length to shrink)
                    | ToAbsolute (Scalar v)  -- ^ Extend or shrink to the given
                                             --   arc length

-- | Which side of a segment, trail, or path should be adjusted?
data AdjustSide = Start  -- ^ Adjust only the beginning
                | End    -- ^ Adjust only the end
                | Both   -- ^ Adjust both sides equally
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | How should a segment, trail, or path be adjusted?
data AdjustOpts v = ALO { adjMethod :: AdjustMethod v
                        , adjSide   :: AdjustSide
                        , adjEps    :: Scalar v
                        , adjOptsvProxy__ :: Proxy v
                        }

instance Fractional (Scalar v) => Default (AdjustMethod v) where
  def = ByParam 0.2

instance Default AdjustSide where
  def = Both

instance Fractional (Scalar v) => Default (AdjustOpts v) where
  def = ALO def def (1/10^(10 :: Integer)) Proxy

-- | Adjust the length of a segment.  The second parameter is an
--   option record which controls how the adjustment should be
--   performed; see 'AdjustOpts'.
adjustSegment :: (InnerSpace v, OrderedField (Scalar v))
              => Segment v -> AdjustOpts v -> Segment v
adjustSegment s opts = adjustSegmentToParams s 
    (if adjSide opts == End   then 0 else getParam s) 
    (if adjSide opts == Start then 0 else 1 - getParam (reverseSegment s))
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

-- | Given a segment and parameters @t1@, @t2@, produce the segment
--   which lies on the (infinitely extended) original segment
--   beginning at @t1@ and ending at @t2@.
adjustSegmentToParams :: (Fractional (Scalar v), VectorSpace v)
                      => Segment v -> Scalar v -> Scalar v -> Segment v
adjustSegmentToParams s t1 t2 = snd (splitAtParam (fst (splitAtParam s t2)) (t1/t2))

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
  transform t (FLinear p1 p2) = FLinear (transform t p1) (transform t p2)
  transform t (FCubic p1 c1 c2 p2) = FCubic (transform t p1)
                                            (transform t c1)
                                            (transform t c2)
                                            (transform t p2)

instance VectorSpace v => HasOrigin (FixedSegment v) where
  moveOriginTo o (FLinear p1 p2) = FLinear (moveOriginTo o p1)
                                           (moveOriginTo o p2)
  moveOriginTo o (FCubic p1 c1 c2 p2) = FCubic (moveOriginTo o p1)
                                               (moveOriginTo o c1)
                                               (moveOriginTo o c2)
                                               (moveOriginTo o p2)

-- instance Enveloped (FixedSegment v) where
  -- XXX write me

-- | Create a 'FixedSegment' from a starting point and a 'Segment'.
mkFixedSeg :: AdditiveGroup v => Point v -> Segment v -> FixedSegment v
mkFixedSeg p (Linear v)       = FLinear p (p .+^ v)
mkFixedSeg p (Cubic c1 c2 x2) = FCubic p (p .+^ c1) (p .+^ c2) (p .+^ x2)

-- | Compute the point on a fixed segment at a given parameter.  A
--   parameter of 0 corresponds to the starting point and 1 corresponds
--   to the ending point.
fAtParam :: VectorSpace v => FixedSegment v -> Scalar v -> Point v
fAtParam (FLinear p1 p2) t = alerp p1 p2 t
fAtParam (FCubic x1 c1 c2 x2) t = p3
  where p11 = alerp x1 c1 t
        p12 = alerp c1 c2 t
        p13 = alerp c2 x2 t

        p21 = alerp p11 p12 t
        p22 = alerp p12 p13 t

        p3  = alerp p21 p22 t