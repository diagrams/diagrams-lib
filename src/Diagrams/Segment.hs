{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Segment
-- Copyright   :  (c) 2011-2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /segment/ is a translation-invariant, atomic path.  Currently,
-- there are two types: linear (/i.e./ just a straight line to the
-- endpoint) and cubic Bézier curves (/i.e./ a curve to an endpoint
-- with two control points).  This module contains tools for creating
-- and manipulating segments, as well as a definition of segments with
-- a fixed location (useful for backend implementors).
--
-- Generally speaking, casual users of diagrams should not need this
-- module; the higher-level functionality provided by
-- "Diagrams.Trail", "Diagrams.TrailLike", and "Diagrams.Path" should
-- usually suffice.  However, directly manipulating segments can
-- occasionally be useful.
--
-----------------------------------------------------------------------------

module Diagrams.Segment
       ( -- * Open/closed tags

         Open, Closed

         -- * Segment offsets

       , Offset(..), segOffset

         -- * Constructing and modifying segments

       , Segment(..), straight, bezier3, bézier3, reverseSegment

         -- * Fixed (absolutely located) segments
       , FixedSegment(..)
       , mkFixedSeg, fromFixedSeg

         -- * Segment measures
         -- $segmeas

       , SegCount(SegCount), getSegCount
       , ArcLength(ArcLength)
       , getArcLength, getArcLengthCached, getArcLengthFun, getArcLengthBounded
       , TotalOffset(..)
       , OffsetEnvelope(..)
       , SegMeasure

       ) where

import           Control.Lens (makeLenses)
import           Control.Applicative (liftA2)
import           Data.AffineSpace
import           Data.FingerTree
import           Data.Monoid.MList
import           Data.Semigroup
import           Data.VectorSpace    hiding (Sum (..))
import           Numeric.Interval    (Interval (..))
import qualified Numeric.Interval    as I

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Solve


------------------------------------------------------------
--  Open/closed type tags  ---------------------------------
------------------------------------------------------------

-- Eventually we should use DataKinds for this, but not until we drop
-- support for GHC 7.4.

-- | Type tag for open segments.
data Open

-- | Type tag for closed segments.
data Closed

------------------------------------------------------------
--  Segment offsets  ---------------------------------------
------------------------------------------------------------

-- | The /offset/ of a segment is the vector from its starting point
--   to its end.  The offset for an /open/ segment is determined by
--   the context, /i.e./ its endpoint is not fixed.  The offset for a
--   /closed/ segment is stored explicitly, /i.e./ its endpoint is at
--   a fixed offset from its start.
data Offset c v where
  OffsetOpen   :: Offset Open v
  OffsetClosed :: v -> Offset Closed v

deriving instance Show v => Show (Offset c v)
deriving instance Eq   v => Eq   (Offset c v)
deriving instance Ord  v => Ord  (Offset c v)

instance Functor (Offset c) where
  fmap _ OffsetOpen       = OffsetOpen
  fmap f (OffsetClosed v) = OffsetClosed (f v)

type instance V (Offset c v) = v

instance HasLinearMap v => Transformable (Offset c v) where
  transform = fmap . apply

------------------------------------------------------------
--  Constructing segments  ---------------------------------
------------------------------------------------------------

-- | The atomic constituents of the concrete representation currently
--   used for trails are /segments/, currently limited to
--   single straight lines or cubic Bézier curves.  Segments are
--   /translationally invariant/, that is, they have no particular
--   \"location\" and are unaffected by translations.  They are,
--   however, affected by other transformations such as rotations and
--   scales.
data Segment c v
    = Linear (Offset c v)
      -- ^ A linear segment with given offset.

    | Cubic v v (Offset c v)
      -- ^ A cubic Bézier segment specified by
      --   three offsets from the starting
      --   point to the first control point,
      --   second control point, and ending
      --   point, respectively.

  deriving (Show, Functor, Eq, Ord)

-- Note, can't yet have Haddock comments on GADT constructors; see
-- http://trac.haskell.org/haddock/ticket/43. For now we don't need
-- Segment to be a GADT but we might in the future. (?)

type instance V (Segment c v) = v

instance HasLinearMap v => Transformable (Segment c v) where
  transform = fmap . apply

instance HasLinearMap v => Renderable (Segment c v) NullBackend where
  render _ _ = mempty

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v -> Segment Closed v
straight = Linear . OffsetClosed

-- Note, if we didn't have a Linear constructor we could also create
-- linear segments with @Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v@.  Those
-- would not be precisely the same, however, since we can actually
-- observe how segments are parametrized.

-- | @bezier3 c1 c2 x@ constructs a translationally invariant cubic
--   Bézier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @c1@, @c2@, and @x@.
bezier3 :: v -> v -> v -> Segment Closed v
bezier3 c1 c2 x = Cubic c1 c2 (OffsetClosed x)

-- | @bézier3@ is the same as @bezier3@, but with more snobbery.
bézier3 :: v -> v -> v -> Segment Closed v
bézier3 = bezier3

type instance Codomain (Segment Closed v) = v

-- | 'atParam' yields a parametrized view of segments as continuous
--   functions @[0,1] -> v@, which give the offset from the start of
--   the segment for each value of the parameter between @0@ and @1@.
--   It is designed to be used infix, like @seg ``atParam`` 0.5@.
instance (VectorSpace v, Num (Scalar v)) => Parametric (Segment Closed v) where
  atParam (Linear (OffsetClosed x)) t       = t *^ x
  atParam (Cubic c1 c2 (OffsetClosed x2)) t =     (3 * t'*t'*t ) *^ c1
                                              ^+^ (3 * t'*t *t ) *^ c2
                                              ^+^ (    t *t *t ) *^ x2
    where t' = 1-t

instance Num (Scalar v) => DomainBounds (Segment Closed v)

instance (VectorSpace v, Num (Scalar v)) => EndValues (Segment Closed v) where
  atStart                            = const zeroV
  atEnd (Linear (OffsetClosed v))    = v
  atEnd (Cubic _ _ (OffsetClosed v)) = v

-- | Compute the offset from the start of a segment to the
--   end.  Note that in the case of a Bézier segment this is /not/ the
--   same as the length of the curve itself; for that, see 'arcLength'.
segOffset :: Segment Closed v -> v
segOffset (Linear (OffsetClosed v))    = v
segOffset (Cubic _ _ (OffsetClosed v)) = v

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
instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Segment Closed v) where

  getEnvelope (s@(Linear {})) = mkEnvelope $ \v ->
    maximum . map (\t -> ((s `atParam` t) <.> v) / magnitudeSq v) $ [0,1]

  getEnvelope (s@(Cubic c1 c2 (OffsetClosed x2))) = mkEnvelope $ \v ->
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

instance (VectorSpace v, Fractional (Scalar v)) => Sectionable (Segment Closed v) where
  splitAtParam (Linear (OffsetClosed x1)) t = (left, right)
    where left  = straight p
          right = straight (x1 ^-^ p)
          p = lerp zeroV x1 t
  splitAtParam (Cubic c1 c2 (OffsetClosed x2)) t = (left, right)
    where left  = bezier3 a b e
          right = bezier3 (c ^-^ e) (d ^-^ e) (x2 ^-^ e)
          p = lerp c1    c2 t
          a = lerp zeroV c1 t
          b = lerp a     p  t
          d = lerp c2    x2 t
          c = lerp p     d  t
          e = lerp b     c  t

  reverseDomain = reverseSegment

-- | Reverse the direction of a segment.
reverseSegment :: AdditiveGroup v => Segment Closed v -> Segment Closed v
reverseSegment (Linear (OffsetClosed v))       = straight (negateV v)
reverseSegment (Cubic c1 c2 (OffsetClosed x2)) = bezier3 (c2 ^-^ x2) (c1 ^-^ x2) (negateV x2)

instance (InnerSpace v, Floating (Scalar v), Ord (Scalar v), AdditiveGroup v)
      => HasArcLength (Segment Closed v) where

  arcLengthBounded _ (Linear (OffsetClosed x1)) = I.singleton $ magnitude x1
  arcLengthBounded m s@(Cubic c1 c2 (OffsetClosed x2))
    | ub - lb < m = I lb ub
    | otherwise   = arcLengthBounded (m/2) l + arcLengthBounded (m/2) r
   where (l,r) = s `splitAtParam` 0.5
         ub    = sum (map magnitude [c1, c2 ^-^ c1, x2 ^-^ c2])
         lb    = magnitude x2

  arcLengthToParam m s _ | arcLength m s == 0 = 0.5
  arcLengthToParam m s@(Linear {}) len = len / arcLength m s
  arcLengthToParam m s@(Cubic {})  len
    | len `I.elem` (I (-m/2) (m/2)) = 0
    | len < 0              = - arcLengthToParam m (fst (splitAtParam s (-1))) (-len)
    | len `I.elem` slen    = 1
    | len > I.sup slen     = 2 * arcLengthToParam m (fst (splitAtParam s 2)) len
    | len < I.sup llen     = (*0.5) $ arcLengthToParam m l len
    | otherwise            = (+0.5) . (*0.5)
                           $ arcLengthToParam (9*m/10) r (len - I.midpoint llen)
    where (l,r) = s `splitAtParam` 0.5
          llen  = arcLengthBounded (m/10) l
          slen  = arcLengthBounded m s

  -- Note, the above seems to be quite slow since it duplicates a lot of
  -- work.  We could trade off some time for space by building a tree of
  -- parameter values (up to a certain depth...)

------------------------------------------------------------
--  Fixed segments
------------------------------------------------------------

-- | @FixedSegment@s are like 'Segment's except that they have
--   absolute locations.  @FixedSegment v@ is isomorphic to @Located
--   (Segment Closed v)@, as witnessed by 'mkFixedSeg' and
--   'fromFixedSeg', but @FixedSegment@ is convenient when one needs
--   the absolute locations of the vertices and control points.
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
    where (p, s) = viewLoc $ fromFixedSeg f

    -- Eventually we might decide it's cleaner/more efficient (?) to
    -- have all the computation in the FixedSegment instance of
    -- Envelope, and implement the Segment instance in terms of it,
    -- instead of the other way around

-- | Create a 'FixedSegment' from a located 'Segment'.
mkFixedSeg :: AdditiveGroup v => Located (Segment Closed v) -> FixedSegment v
mkFixedSeg ls =
  case viewLoc ls of
    (p, Linear (OffsetClosed v))       -> FLinear p (p .+^ v)
    (p, Cubic c1 c2 (OffsetClosed x2)) -> FCubic  p (p .+^ c1) (p .+^ c2) (p .+^ x2)

-- | Convert a 'FixedSegment' back into a located 'Segment'.
fromFixedSeg :: AdditiveGroup v => FixedSegment v -> Located (Segment Closed v)
fromFixedSeg (FLinear p1 p2)      = straight (p2 .-. p1) `at` p1
fromFixedSeg (FCubic x1 c1 c2 x2) = bezier3 (c1 .-. x1) (c2 .-. x1) (x2 .-. x1) `at` x1

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

------------------------------------------------------------
--  Segment measures  --------------------------------------
------------------------------------------------------------

-- $segmeas
-- Trails store a sequence of segments in a fingertree, which can
-- automatically track various monoidal \"measures\" on segments.

-- | A type to track the count of segments in a 'Trail'.
newtype SegCount = SegCount { _getSegCount :: Sum Int }
  deriving (Semigroup, Monoid)

makeLenses ''SegCount

-- | A type to represent the total arc length of a chain of
--   segments. The first component is a \"standard\" arc length,
--   computed to within a tolerance of @10e-6@.  The second component is
--   a generic arc length function taking the tolerance as an
--   argument.
newtype ArcLength v = ArcLength
  { _getArcLength :: (Sum (Interval (Scalar v)), Scalar v -> Sum (Interval (Scalar v))) }


makeLenses ''ArcLength

-- | Project out the cached arc length, stored together with error
--   bounds.
getArcLengthCached :: ArcLength v -> Interval (Scalar v)
getArcLengthCached = getSum . fst . getArcLength

-- | Project out the generic arc length function taking the tolerance as
--   an argument.
getArcLengthFun :: ArcLength v -> Scalar v -> Interval (Scalar v)
getArcLengthFun = fmap getSum . snd . getArcLength

-- | Given a specified tolerance, project out the cached arc length if
--   it is accurate enough; otherwise call the generic arc length
--   function with the given tolerance.
getArcLengthBounded :: (Num (Scalar v), Ord (Scalar v))
                    => Scalar v -> ArcLength v -> Interval (Scalar v)
getArcLengthBounded eps al
  | I.width cached <= eps = cached
  | otherwise             = getArcLengthFun al eps
  where
    cached = getArcLengthCached al
deriving instance (Num (Scalar v), Ord (Scalar v)) => Semigroup (ArcLength v)
deriving instance (Num (Scalar v), Ord (Scalar v)) => Monoid    (ArcLength v)

-- | A type to represent the total cumulative offset of a chain of
--   segments.
newtype TotalOffset v = TotalOffset { getTotalOffset :: v }

instance AdditiveGroup v => Semigroup (TotalOffset v) where
  TotalOffset v1 <> TotalOffset v2 = TotalOffset (v1 ^+^ v2)

instance AdditiveGroup v => Monoid (TotalOffset v) where
  mempty  = TotalOffset zeroV
  mappend = (<>)

-- | A type to represent the offset and envelope of a chain of
--   segments.  They have to be paired into one data structure, since
--   combining the envelopes of two consecutive chains needs to take
--   the offset of the the offset of the first into account.
data OffsetEnvelope v = OffsetEnvelope
  { oeOffset   :: TotalOffset v
  , oeEnvelope :: Envelope v
  }

instance (InnerSpace v, OrderedField (Scalar v)) => Semigroup (OffsetEnvelope v) where
  (OffsetEnvelope o1 e1) <> (OffsetEnvelope o2 e2)
    = OffsetEnvelope
        (o1 <> o2)
        (e1 <> moveOriginBy (negateV . getTotalOffset $ o1) e2)

-- | @SegMeasure@ collects up all the measurements over a chain of
--   segments.
type SegMeasure v = SegCount
                ::: ArcLength v
                ::: OffsetEnvelope v
                ::: ()
  -- unfortunately we can't cache Trace, since there is not a generic
  -- instance Traced (Segment Closed v), only Traced (Segment Closed R2).

instance (InnerSpace v, OrderedField (Scalar v))
    => Measured (SegMeasure v) (SegMeasure v) where
  measure = id

instance (OrderedField (Scalar v), InnerSpace v)
    => Measured (SegMeasure v) (Segment Closed v) where
  measure s = (SegCount . Sum $ 1)

            -- cache arc length with two orders of magnitude more
            -- accuracy than standard, so we have a hope of coming out
            -- with an accurate enough total arc length for
            -- reasonable-length trails
           *: (ArcLength $ ( Sum $ arcLengthBounded (stdTolerance/100) s
                           , Sum . flip arcLengthBounded s
                           )
              )

           *: (OffsetEnvelope
                (TotalOffset . segOffset $ s)
                (getEnvelope s)
              )
           *: ()
