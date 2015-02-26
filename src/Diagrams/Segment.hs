{-# LANGUAGE BangPatterns               #-}
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

       , Offset(..) , segOffset

         -- * Constructing and modifying segments

       , Segment(..), straight, bezier3, bézier3, reverseSegment, mapSegmentVectors

         -- * Fixed (absolutely located) segments
       , FixedSegment(..)
       , mkFixedSeg, fromFixedSeg

         -- * Segment measures
         -- $segmeas

       , SegCount(..)
       , ArcLength(..)
       , getArcLengthCached, getArcLengthFun, getArcLengthBounded
       , TotalOffset(..)
       , OffsetEnvelope(..), oeOffset, oeEnvelope
       , SegMeasure

       ) where

import           Control.Lens              (Each (..), Rewrapped, Wrapped (..),
                                            iso, makeLenses, op, over)
import           Data.FingerTree
import           Data.Monoid.MList
import           Data.Semigroup
import           Numeric.Interval.Kaucher  (Interval (..))
import qualified Numeric.Interval.Kaucher  as I

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

import           Control.Applicative
import           Diagrams.Core             hiding (Measured)
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Solve.Polynomial


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
data Offset c v n where
  OffsetOpen   :: Offset Open v n
  OffsetClosed :: v n -> Offset Closed v n

deriving instance Show (v n) => Show (Offset c v n)
deriving instance Eq   (v n) => Eq   (Offset c v n)
deriving instance Ord  (v n) => Ord  (Offset c v n)

instance Functor v => Functor (Offset c v) where
  fmap _ OffsetOpen       = OffsetOpen
  fmap f (OffsetClosed v) = OffsetClosed (fmap f v)

instance Each (Offset c v n) (Offset c v' n') (v n) (v' n') where
  each f (OffsetClosed v) = OffsetClosed <$> f v
  each _ OffsetOpen       = pure OffsetOpen
  {-# INLINE each #-}

type instance V (Offset c v n) = v
type instance N (Offset c v n) = n

instance Transformable (Offset c v n) where
  transform _ OffsetOpen       = OffsetOpen
  transform t (OffsetClosed v) = OffsetClosed (apply t v)

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
data Segment c v n
    = Linear !(Offset c v n)
      -- ^ A linear segment with given offset.

    | Cubic !(v n) !(v n) !(Offset c v n)
      -- ^ A cubic Bézier segment specified by
      --   three offsets from the starting
      --   point to the first control point,
      --   second control point, and ending
      --   point, respectively.

  deriving (Show, Functor, Eq, Ord)

instance Each (Segment c v n) (Segment c v' n') (v n) (v' n') where
  each f (Linear offset)      = Linear <$> each f offset
  each f (Cubic v1 v2 offset) = Cubic  <$> f v1 <*> f v2 <*> each f offset
  {-# INLINE each #-}

-- | Map over the vectors of each segment.
mapSegmentVectors :: (v n -> v' n') -> Segment c v n -> Segment c v' n'
mapSegmentVectors = over each

-- Note, can't yet have Haddock comments on GADT constructors; see
-- http://trac.haskell.org/haddock/ticket/43. For now we don't need
-- Segment to be a GADT but we might in the future. (?)

type instance V (Segment c v n) = v
type instance N (Segment c v n) = n

instance Transformable (Segment c v n) where
	transform = mapSegmentVectors . apply

instance Renderable (Segment c v n) NullBackend where
  render _ _ = mempty

-- | @'straight' v@ constructs a translationally invariant linear
--   segment with direction and length given by the vector @v@.
straight :: v n -> Segment Closed v n
straight = Linear . OffsetClosed

-- Note, if we didn't have a Linear constructor we could also create
-- linear segments with @Cubic (v ^/ 3) (2 *^ (v ^/ 3)) v@.  Those
-- would not be precisely the same, however, since we can actually
-- observe how segments are parametrized.

-- | @bezier3 c1 c2 x@ constructs a translationally invariant cubic
--   Bézier curve where the offsets from the first endpoint to the
--   first and second control point and endpoint are respectively
--   given by @c1@, @c2@, and @x@.
bezier3 :: v n -> v n -> v n -> Segment Closed v n
bezier3 c1 c2 x = Cubic c1 c2 (OffsetClosed x)

-- | @bézier3@ is the same as @bezier3@, but with more snobbery.
bézier3 :: v n -> v n -> v n -> Segment Closed v n
bézier3 = bezier3

type instance Codomain (Segment Closed v n) = v

-- | 'atParam' yields a parametrized view of segments as continuous
--   functions @[0,1] -> v@, which give the offset from the start of
--   the segment for each value of the parameter between @0@ and @1@.
--   It is designed to be used infix, like @seg ``atParam`` 0.5@.
instance (Additive v, Num n) => Parametric (Segment Closed v n) where
  atParam (Linear (OffsetClosed x)) t       = t *^ x
  atParam (Cubic c1 c2 (OffsetClosed x2)) t =     (3 * t'*t'*t ) *^ c1
                                              ^+^ (3 * t'*t *t ) *^ c2
                                              ^+^ (    t *t *t ) *^ x2
    where t' = 1-t

instance Num n => DomainBounds (Segment Closed v n)

instance (Additive v, Num n) => EndValues (Segment Closed v n) where
  atStart                            = const zero
  atEnd (Linear (OffsetClosed v))    = v
  atEnd (Cubic _ _ (OffsetClosed v)) = v

-- | Compute the offset from the start of a segment to the
--   end.  Note that in the case of a Bézier segment this is /not/ the
--   same as the length of the curve itself; for that, see 'arcLength'.
segOffset :: Segment Closed v n -> v n
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
instance (Metric v, OrderedField n) => Enveloped (Segment Closed v n) where

  getEnvelope (s@(Linear {})) = mkEnvelope $ \v ->
    maximum (map (\t -> (s `atParam` t) `dot` v) [0,1]) / quadrance v

  getEnvelope (s@(Cubic c1 c2 (OffsetClosed x2))) = mkEnvelope $ \v ->
    maximum .
    map (\t -> ((s `atParam` t) `dot` v) / quadrance v) $
    [0,1] ++
    filter (liftA2 (&&) (>0) (<1))
      (quadForm (3 * ((3 *^ c1 ^-^ 3 *^ c2 ^+^ x2) `dot` v))
                (6 * (((-2) *^ c1 ^+^ c2) `dot` v))
                ((3 *^ c1) `dot` v))

------------------------------------------------------------
--  Manipulating segments
------------------------------------------------------------

instance (Additive v, Fractional n) => Sectionable (Segment Closed v n) where
  splitAtParam (Linear (OffsetClosed x1)) t = (left, right)
    where left  = straight p
          right = straight (x1 ^-^ p)
          p = lerp t x1 zero
  splitAtParam (Cubic c1 c2 (OffsetClosed x2)) t = (left, right)
    where left  = bezier3 a b e
          right = bezier3 (c ^-^ e) (d ^-^ e) (x2 ^-^ e)
          p = lerp t c2 c1
          a = lerp t c1 zero
          b = lerp t p a
          d = lerp t x2 c2
          c = lerp t d p
          e = lerp t c b

  reverseDomain = reverseSegment

-- | Reverse the direction of a segment.
reverseSegment :: (Num n, Additive v) => Segment Closed v n -> Segment Closed v n
reverseSegment (Linear (OffsetClosed v))       = straight (negated v)
reverseSegment (Cubic c1 c2 (OffsetClosed x2)) = bezier3 (c2 ^-^ x2) (c1 ^-^ x2) (negated x2)

instance (Metric v, Floating n, Ord n, Additive v)
      => HasArcLength (Segment Closed v n) where

  arcLengthBounded _ (Linear (OffsetClosed x1)) = I.singleton $ norm x1
  arcLengthBounded m s@(Cubic c1 c2 (OffsetClosed x2))
    | ub - lb < m = I lb ub
    | otherwise   = arcLengthBounded (m/2) l + arcLengthBounded (m/2) r
   where (l,r) = s `splitAtParam` 0.5
         ub    = sum (map norm [c1, c2 ^-^ c1, x2 ^-^ c2])
         lb    = norm x2

  arcLengthToParam m s _ | arcLength m s == 0 = 0.5
  arcLengthToParam m s@(Linear {}) len = len / arcLength m s
  arcLengthToParam m s@(Cubic {})  len
    | len `I.elem` I (-m/2) (m/2) = 0
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
data FixedSegment v n = FLinear (Point v n) (Point v n)
                      | FCubic (Point v n) (Point v n) (Point v n) (Point v n)
  deriving Show

type instance V (FixedSegment v n) = v
type instance N (FixedSegment v n) = n

instance Each (FixedSegment v n) (FixedSegment v' n') (Point v n) (Point v' n') where
  each f (FLinear p0 p1)      = FLinear <$> f p0 <*> f p1
  each f (FCubic p0 p1 p2 p3) = FCubic  <$> f p0 <*> f p1 <*> f p2 <*> f p3
  {-# INLINE each #-}

instance (Additive v, Num n) => Transformable (FixedSegment v n) where
  transform t = over each (papply t)

instance (Additive v, Num n) => HasOrigin (FixedSegment v n) where
  moveOriginTo o = over each (moveOriginTo o)

instance (Metric v, OrderedField n) => Enveloped (FixedSegment v n) where
  getEnvelope f = moveTo p (getEnvelope s)
    where (p, s) = viewLoc $ fromFixedSeg f

    -- Eventually we might decide it's cleaner/more efficient (?) to
    -- have all the computation in the FixedSegment instance of
    -- Envelope, and implement the Segment instance in terms of it,
    -- instead of the other way around

-- | Create a 'FixedSegment' from a located 'Segment'.
mkFixedSeg :: (Num n, Additive v) => Located (Segment Closed v n) -> FixedSegment v n
mkFixedSeg ls =
  case viewLoc ls of
    (p, Linear (OffsetClosed v))       -> FLinear p (p .+^ v)
    (p, Cubic c1 c2 (OffsetClosed x2)) -> FCubic  p (p .+^ c1) (p .+^ c2) (p .+^ x2)

-- | Convert a 'FixedSegment' back into a located 'Segment'.
fromFixedSeg :: (Num n, Additive v) => FixedSegment v n -> Located (Segment Closed v n)
fromFixedSeg (FLinear p1 p2)      = straight (p2 .-. p1) `at` p1
fromFixedSeg (FCubic x1 c1 c2 x2) = bezier3 (c1 .-. x1) (c2 .-. x1) (x2 .-. x1) `at` x1

type instance Codomain (FixedSegment v n) = Point v

instance (Additive v, Num n) => Parametric (FixedSegment v n) where
  atParam (FLinear p1 p2) t = lerp t p2 p1
  atParam (FCubic x1 c1 c2 x2) t = p3
    where p11 = lerp t c1 x1
          p12 = lerp t c2 c1
          p13 = lerp t x2 c2

          p21 = lerp t p12 p11
          p22 = lerp t p13 p12

          p3  = lerp t p22 p21

instance Num n => DomainBounds (FixedSegment v n)

instance (Additive v, Num n) => EndValues (FixedSegment v n) where
  atStart (FLinear p0 _)     = p0
  atStart (FCubic  p0 _ _ _) = p0
  atEnd   (FLinear _ p1)     = p1
  atEnd   (FCubic _ _ _ p1 ) = p1

instance (Additive v, Fractional n) => Sectionable (FixedSegment v n) where
  splitAtParam (FLinear p0 p1) t = (left, right)
    where left  = FLinear p0 p
          right = FLinear p  p1
          p = lerp t p1 p0
  splitAtParam (FCubic p0 c1 c2 p1) t = (left, right)
    where left  = FCubic p0 a b cut
          right = FCubic cut c d p1
          -- first round
          a   = lerp t c1 p0
          p   = lerp t c2 c1
          d   = lerp t p1 c2
          -- second round
          b   = lerp t p a
          c   = lerp t d p
          -- final round
          cut = lerp t c b

  reverseDomain (FLinear p0 p1) = FLinear p1 p0
  reverseDomain (FCubic p0 c1 c2 p1) = FCubic p1 c2 c1 p0

------------------------------------------------------------
--  Segment measures  --------------------------------------
------------------------------------------------------------

-- $segmeas
-- Trails store a sequence of segments in a fingertree, which can
-- automatically track various monoidal \"measures\" on segments.

-- | A type to track the count of segments in a 'Trail'.
newtype SegCount = SegCount (Sum Int)
  deriving (Semigroup, Monoid)

instance Wrapped SegCount where
    type Unwrapped SegCount = Sum Int
    _Wrapped' = iso (\(SegCount x) -> x) SegCount

instance Rewrapped SegCount SegCount

-- | A type to represent the total arc length of a chain of
--   segments. The first component is a \"standard\" arc length,
--   computed to within a tolerance of @10e-6@.  The second component is
--   a generic arc length function taking the tolerance as an
--   argument.

newtype ArcLength n
  = ArcLength (Sum (Interval n), n -> Sum (Interval n))

instance Wrapped (ArcLength n) where
  type Unwrapped (ArcLength n) = (Sum (Interval n), n -> Sum (Interval n))
  _Wrapped' = iso (\(ArcLength x) -> x) ArcLength

instance Rewrapped (ArcLength n) (ArcLength n')

-- | Project out the cached arc length, stored together with error
--   bounds.
getArcLengthCached :: ArcLength n -> Interval n
getArcLengthCached = getSum . fst . op ArcLength

-- | Project out the generic arc length function taking the tolerance as
--   an argument.
getArcLengthFun :: ArcLength n -> n -> Interval n
getArcLengthFun = fmap getSum . snd . op ArcLength

-- | Given a specified tolerance, project out the cached arc length if
--   it is accurate enough; otherwise call the generic arc length
--   function with the given tolerance.
getArcLengthBounded :: (Num n, Ord n)
                    => n -> ArcLength n -> Interval n
getArcLengthBounded eps al
  | I.width cached <= eps = cached
  | otherwise             = getArcLengthFun al eps
  where
    cached = getArcLengthCached al
deriving instance (Num n, Ord n) => Semigroup (ArcLength n)
deriving instance (Num n, Ord n) => Monoid    (ArcLength n)

-- | A type to represent the total cumulative offset of a chain of
--   segments.
newtype TotalOffset v n = TotalOffset (v n)

instance Wrapped (TotalOffset v n) where
    type Unwrapped (TotalOffset v n) = v n
    _Wrapped' = iso (\(TotalOffset x) -> x) TotalOffset

instance Rewrapped (TotalOffset v n) (TotalOffset v' n')

instance (Num n, Additive v) => Semigroup (TotalOffset v n) where
  TotalOffset v1 <> TotalOffset v2 = TotalOffset (v1 ^+^ v2)

instance (Num n, Additive v) => Monoid (TotalOffset v n) where
  mempty  = TotalOffset zero
  mappend = (<>)

-- | A type to represent the offset and envelope of a chain of
--   segments.  They have to be paired into one data structure, since
--   combining the envelopes of two consecutive chains needs to take
--   the offset of the the offset of the first into account.
data OffsetEnvelope v n = OffsetEnvelope
  { _oeOffset   :: !(TotalOffset v n)
  , _oeEnvelope :: Envelope v n
  }

makeLenses ''OffsetEnvelope

instance (Metric v, OrderedField n) => Semigroup (OffsetEnvelope v n) where
  (OffsetEnvelope o1 e1) <> (OffsetEnvelope o2 e2)
    = let !negOff = negated . op TotalOffset $ o1
          e2Off = moveOriginBy negOff e2
          !_unused = maybe () (\f -> f `seq` ()) $ appEnvelope e2Off
      in OffsetEnvelope
          (o1 <> o2)
          (e1 <> e2Off)

-- | @SegMeasure@ collects up all the measurements over a chain of
--   segments.
type SegMeasure v n = SegCount
                  ::: ArcLength n
                  ::: OffsetEnvelope v n
                  ::: ()
  -- unfortunately we can't cache Trace, since there is not a generic
  -- instance Traced (Segment Closed v), only Traced (Segment Closed R2).

instance (Metric v, OrderedField n)
    => Measured (SegMeasure v n) (SegMeasure v n) where
  measure = id

instance (OrderedField n, Metric v)
    => Measured (SegMeasure v n) (Segment Closed v n) where
  measure s = (SegCount . Sum) 1

            -- cache arc length with two orders of magnitude more
            -- accuracy than standard, so we have a hope of coming out
            -- with an accurate enough total arc length for
            -- reasonable-length trails
            *: ArcLength ( Sum $ arcLengthBounded (stdTolerance/100) s
                         , Sum . flip arcLengthBounded s               )

           *: OffsetEnvelope (TotalOffset . segOffset $ s)
                             (getEnvelope s)

           *: ()

