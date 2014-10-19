{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- We have an orphan Transformable FingerTree instance here.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Trail
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /trails/, translationally invariant paths
-- through space.  Trails form a central part of the diagrams-lib API,
-- so the documentation for this module merits careful study.
--
-- Related modules include:
--
-- * The 'TrailLike' class ("Diagrams.TrailLike") exposes a generic
--   API for building a wide range of things out of trails.
--
-- * 'Path's ("Diagrams.Path") are collections of 'Located'
--   ("Diagrams.Located") trails.
--
-- * Trails are composed of 'Segment's (see "Diagrams.Segment"),
--   though most users should not need to work with segments directly.
--
-----------------------------------------------------------------------------

module Diagrams.Trail
       (
         -- * Type definitions

         -- ** Lines and loops

         Trail'(..)

       , glueLine
       , closeLine
       , cutLoop

         -- ** Generic trails

       , Trail(..)
       , wrapTrail, wrapLine, wrapLoop
       , onTrail, onLine

       , glueTrail, closeTrail, cutTrail

         -- * Constructing trails

       , emptyLine, emptyTrail
       , lineFromVertices, trailFromVertices
       , lineFromOffsets,  trailFromOffsets
       , lineFromSegments, trailFromSegments

         -- * Eliminating trails

       , withTrail', withTrail, withLine
       , isLineEmpty, isTrailEmpty
       , isLine, isLoop
       , trailSegments, lineSegments, loopSegments
       , onLineSegments
       , trailOffsets, trailOffset
       , lineOffsets, lineOffset, loopOffsets
       , trailPoints, linePoints, loopPoints
       , trailVertices', lineVertices', loopVertices'
       , trailVertices, lineVertices, loopVertices
       , trailLocSegments, fixTrail

         -- * Modifying trails

       , reverseTrail, reverseLocTrail
       , reverseLine, reverseLocLine
       , reverseLoop, reverseLocLoop

         -- * Internals
         -- $internals

         -- ** Type tags

       , Line, Loop

         -- ** Segment trees

       , SegTree(..), trailMeasure, numSegs, offset

         -- ** Extracting segments

       , GetSegment(..), getSegment

       ) where

import           Control.Arrow            ((***))
import           Control.Lens             (AnIso', Rewrapped, Wrapped (..), cloneIso, iso, op, view,
                                           (^.))
import           Data.FingerTree          (FingerTree, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.FingerTree          as FT
import           Data.Fixed
import qualified Data.Foldable            as F
import           Data.Monoid.MList
import           Data.Semigroup
import qualified Numeric.Interval.Kaucher as I

import           Diagrams.Core            hiding ((|>))
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Tangent

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- $internals
--
-- Most users of diagrams should not need to use anything in this
-- section directly, but they are exported on the principle that we
-- can't forsee what uses people might have for them.

------------------------------------------------------------
--  FingerTree instances
------------------------------------------------------------

type instance V (FingerTree m a) = V a
type instance N (FingerTree m a) = N a

instance ( Metric (V a), OrderedField (N a)
         , FT.Measured m a, Transformable a
         )
    => Transformable (FingerTree m a) where
  transform = FT.fmap' . transform

------------------------------------------------------------
--  Segment trees  -----------------------------------------
------------------------------------------------------------

-- | A @SegTree@ represents a sequence of closed segments, stored in a
--   fingertree so we can easily recover various monoidal measures of
--   the segments (number of segments, arc length, envelope...) and
--   also easily slice and dice them according to the measures
--   (/e.g./, split off the smallest number of segments from the
--   beginning which have a combined arc length of at least 5).

newtype SegTree v n = SegTree (FingerTree (SegMeasure v n) (Segment Closed v n))
  deriving (Eq, Ord, Show)

instance Wrapped (SegTree v n) where
    type Unwrapped (SegTree v n) = FingerTree (SegMeasure v n) (Segment Closed v n)
    _Wrapped' = iso (\(SegTree x) -> x) SegTree

instance Rewrapped (SegTree v n) (SegTree v' n')

type instance V (SegTree v n) = v
type instance N (SegTree v n) = n

deriving instance (OrderedField n, Metric v)
  => Monoid (SegTree v n)
deriving instance (OrderedField n, Metric v)
  => FT.Measured (SegMeasure v n) (SegTree v n)
deriving instance (Metric v, OrderedField n)
  => Transformable (SegTree v n)

type instance Codomain (SegTree v n) = v

instance (Metric v, OrderedField n, Real n)
    => Parametric (SegTree v n) where
  atParam t p = offset . fst $ splitAtParam t p

instance Num n => DomainBounds (SegTree v n)

instance (Metric v, OrderedField n, Real n)
    => EndValues (SegTree v n)

type SplitResult v n = ((SegTree v n, n -> n), (SegTree v n, n -> n))

splitAtParam' :: (Metric v, OrderedField n, Real n) => SegTree v n -> n -> SplitResult v n
splitAtParam' tree@(SegTree t) p
    | p < 0     = case FT.viewl t of
                    EmptyL    -> emptySplit
                    seg :< t' ->
                      case seg `splitAtParam` (p * tSegs) of
                        (seg1, seg2) -> ( (SegTree $ FT.singleton seg1, \u -> u * p)
                                        , (SegTree $ seg2 <| t', \u -> 1 - (1 - u) * tSegs / (tSegs + 1))
                                        )
    | p >= 1    = case FT.viewr t of
                    EmptyR    -> emptySplit
                    t' :> seg ->
                      case seg `splitAtParam` (1 - (1 - p)*tSegs) of
                        (seg1, seg2) -> ( (SegTree $ t' |> seg1, \u -> u * tSegs / (tSegs + 1))
                                        , (SegTree $ FT.singleton seg2, \u -> (u - p) / (1 - p))
                                        )
    | otherwise = case FT.viewl after of
                    EmptyL    -> emptySplit
                    seg :< after' ->
                      let (n, p') = propFrac $ p * tSegs
                          f p n u | u * tSegs < n = u * tSegs / (n + 1)
                                  | otherwise     = (n + (u * tSegs - n) / (p * tSegs - n)) / (n+1)
                      in case seg `splitAtParam` p' of
                           (seg1, seg2) -> ( ( SegTree $ before |> seg1  , f p n )
                                           , ( SegTree $ seg2   <| after'
                                             , \v -> 1 - f (1 - p) (tSegs - n - 1) (1 - v)
                                             )
                                           )
 where
      newSegs p' = (1+) . fst . propFrac $ p' * tSegs
      (before, after) = FT.split ((p * tSegs <) . numSegs) t
      tSegs           = numSegs t
      emptySplit      = let t' = (tree, id) in (t',t')

      propFrac x = let m = signum x * mod1 x in (x - m, m)

instance (Metric v, OrderedField n, Real n) => Sectionable (SegTree v n) where
  splitAtParam tree p = let ((a,_),(b,_)) = splitAtParam' tree p in (a,b)

  reverseDomain (SegTree t) = SegTree $ FT.reverse t'
    where t' = FT.fmap' reverseSegment t

  section x t1 t2 = let ((a,fa),_) = splitAtParam' x t2
                    in  snd $ splitAtParam a (fa t1)

  -- XXX seems like it should be possible to collapse some of the
  -- above cases into one?

instance (Metric v, OrderedField n, Real n)
    => HasArcLength (SegTree v n) where
  arcLengthBounded eps t
    -- Use the cached value if it is accurate enough; otherwise fall
    -- back to recomputing a more accurate value
    | I.width i <= eps = i
    | otherwise        = fun (eps / numSegs t)
    where
      i   = trailMeasure (I.singleton 0)
              getArcLengthCached
              t
      fun = trailMeasure (const 0)
              getArcLengthFun
              t

  arcLengthToParam eps st@(SegTree t) l
    | l < 0        = case FT.viewl t of
                       EmptyL   -> 0
                       seg :< _ -> arcLengthToParam eps seg l / tSegs
    | l >= totalAL = case FT.viewr t of
                       EmptyR    -> 0
                       t' :> seg ->
                         let p = arcLengthToParam (eps/2) seg
                                   (l - arcLength (eps/2) (SegTree t'))
                         in  (p - 1)/tSegs + 1
    | otherwise    = case FT.viewl after of
                       EmptyL    -> 0
                       seg :< _  ->
                         let p = arcLengthToParam (eps/2) seg
                                   (l - arcLength (eps/2) (SegTree before))
                         in  (numSegs before + p) / tSegs
    where
      totalAL         = arcLength eps st
      tSegs           = numSegs t
      before, after :: FingerTree (SegMeasure v n) (Segment Closed v n)
      (before, after) =
        FT.split ((>= l)
                 . trailMeasure
                 0
                 (I.midpoint . getArcLengthBounded eps))
                 t

-- | Given a default result (to be used in the case of an empty
--   trail), and a function to map a single measure to a result,
--   extract the given measure for a trail and use it to compute a
--   result.  Put another way, lift a function on a single measure
--   (along with a default value) to a function on an entire trail.
trailMeasure :: ( Metric v, OrderedField n
                , SegMeasure v n :>: m, FT.Measured (SegMeasure v n) t
                )
             => a -> (m -> a) -> t -> a
trailMeasure d f = option d f . get . FT.measure

-- | Compute the number of segments of anything measured by
--   'SegMeasure' (/e.g./ @SegMeasure@ itself, @Segment@, @SegTree@,
--   @Trail@s...)
numSegs :: ( OrderedField n, Num c, Metric v,
             FT.Measured (SegMeasure v n) a
           )
        => a -> c
numSegs = fromIntegral . trailMeasure 0 (getSum . op SegCount)

-- | Compute the total offset of anything measured by 'SegMeasure'.
offset :: ( OrderedField n, Metric v,
            FT.Measured (SegMeasure v n) t
          )
       => t -> v n
offset = trailMeasure zero (op TotalOffset . view oeOffset)

------------------------------------------------------------
--  Trails  ------------------------------------------------
------------------------------------------------------------

-- Eventually we should use DataKinds for this, but not until we drop
-- support for GHC 7.4.

-- | Type tag for trails with distinct endpoints.
data Line

-- | Type tag for \"loopy\" trails which return to their starting
--   point.
data Loop

--------------------------------------------------
-- The Trail' type

-- | Intuitively, a trail is a single, continuous path through space.
--   However, a trail has no fixed starting point; it merely specifies
--   /how/ to move through space, not /where/.  For example, \"take
--   three steps forward, then turn right twenty degrees and take two
--   more steps\" is an intuitive analog of a trail; these
--   instructions specify a path through space from any given starting
--   location.  To be precise, trails are /translation-invariant/;
--   applying a translation to a trail has no effect.
--
--   A @'Located' Trail@, on the other hand, is a trail paired with
--   some concrete starting location (\"start at the big tree on the
--   corner, then take three steps forward, ...\").  See the
--   "Diagrams.Located" module for help working with 'Located' values.
--
--   Formally, the semantics of a trail is a continuous (though not
--   necessarily differentiable) function from the real interval [0,1]
--   to vectors in some vector space.  (In contrast, a 'Located' trail
--   is a continuous function from [0,1] to /points/ in some /affine/
--   space.)
--
--   There are two types of trails:
--
--   * A \"line\" (think of the \"train\", \"subway\", or \"bus\"
--     variety, rather than the \"straight\" variety...) is a trail
--     with two distinct endpoints.  Actually, a line can have the
--     same start and end points, but it is still /drawn/ as if it had
--     distinct endpoints: the two endpoints will have the appropriate
--     end caps, and the trail will not be filled.  Lines have a
--     @Monoid@ instance where @mappend@ corresponds to concatenation,
--     /i.e./ chaining one line after the other.
--
--   * A \"loop\" is required to end in the same place it starts (that
--     is, t(0) = t(1)).  Loops are filled and are drawn as one
--     continuous loop, with the appropriate join at the
--     start/endpoint rather than end caps.  Loops do not have a
--     @Monoid@ instance.
--
--   To convert between lines and loops, see 'glueLine',
--   'closeLine', and 'cutLoop'.
--
--   To construct trails, see 'emptyTrail', 'trailFromSegments',
--   'trailFromVertices', 'trailFromOffsets', and friends.  You can
--   also get any type of trail from any function which returns a
--   'TrailLike' (/e.g./ functions in "Diagrams.TwoD.Shapes", and many
--   others; see "Diagrams.TrailLike").
--
--   To extract information from trails, see 'withLine', 'isLoop',
--   'trailSegments', 'trailOffsets', 'trailVertices', and friends.

data Trail' l v n where
  Line :: SegTree v n                     -> Trail' Line v n
  Loop :: SegTree v n -> Segment Open v n -> Trail' Loop v n

-- | A generic eliminator for 'Trail'', taking functions specifying
--   what to do in the case of a line or a loop.
withTrail' :: (Trail' Line v n -> r) -> (Trail' Loop v n -> r) -> Trail' l v n -> r
withTrail' line _    t@(Line{}) = line t
withTrail' _    loop t@(Loop{}) = loop t

deriving instance Show (v n) => Show (Trail' l v n)
deriving instance Eq   (v n) => Eq   (Trail' l v n)
deriving instance Ord  (v n) => Ord  (Trail' l v n)

type instance V (Trail' l v n) = v
type instance N (Trail' l v n) = n

type instance Codomain (Trail' l v n) = v

instance (OrderedField n, Metric v) => Semigroup (Trail' Line v n) where
  (Line t1) <> (Line t2) = Line (t1 `mappend` t2)

-- | The empty trail is constantly the zero vector.  Trails are
--   composed via concatenation.  Note that only lines have a monoid
--   instance (and not loops).
instance (Metric v, OrderedField n) => Monoid (Trail' Line v n) where
  mempty  = emptyLine
  mappend = (<>)

instance (HasLinearMap v, Metric v, OrderedField n)
    => Transformable (Trail' l v n) where
  transform tr (Line t  ) = Line (transform tr t)
  transform tr (Loop t s) = Loop (transform tr t) (transform tr s)

-- | The envelope for a trail is based at the trail's start.
instance (Metric v, OrderedField n) => Enveloped (Trail' l v n) where
  getEnvelope = withTrail' ftEnv (ftEnv . cutLoop)
    where
      ftEnv :: Trail' Line v n -> Envelope v n
      ftEnv (Line t) = trailMeasure mempty (view oeEnvelope) t

instance (HasLinearMap v, Metric v, OrderedField n)
    => Renderable (Trail' o v n) NullBackend where
  render _ _ = mempty

instance (Metric v, OrderedField n, Real n)
    => Parametric (Trail' l v n) where
  atParam t p = withTrail'
                  (\(Line segT) -> segT `atParam` p)
                  (\l -> cutLoop l `atParam` mod1 p)
                  t

type instance Codomain (Tangent (Trail' c v n)) = Codomain (Trail' c v n)

instance (Parametric (GetSegment (Trail' c v n)), Additive v, Num n)
    => Parametric (Tangent (Trail' c v n)) where
  Tangent tr `atParam` p =
    case GetSegment tr `atParam` p of
      GetSegmentCodomain Nothing                  -> zero
      GetSegmentCodomain (Just (_, seg, reparam)) -> Tangent seg `atParam` (p ^. cloneIso reparam)

instance ( Parametric (GetSegment (Trail' c v n))
         , EndValues (GetSegment (Trail' c v n))
         , Additive v
         , Num n
         )
    => EndValues (Tangent (Trail' c v n)) where
  atStart (Tangent tr) =
    case atStart (GetSegment tr) of
      GetSegmentCodomain Nothing            -> zero
      GetSegmentCodomain (Just (_, seg, _)) -> atStart (Tangent seg)
  atEnd (Tangent tr) =
    case atEnd (GetSegment tr) of
      GetSegmentCodomain Nothing            -> zero
      GetSegmentCodomain (Just (_, seg, _)) -> atEnd (Tangent seg)

type instance Codomain (Tangent (Trail v n)) = Codomain (Trail v n)

instance (Metric v , OrderedField n, Real n)
    => Parametric (Tangent (Trail v n)) where
  Tangent tr `atParam` p
    = withTrail
        ((`atParam` p) . Tangent)
        ((`atParam` p) . Tangent)
        tr

instance (Metric v, OrderedField n, Real n)
    => EndValues (Tangent (Trail v n)) where
  atStart (Tangent tr) = withTrail (atStart . Tangent) (atStart . Tangent) tr
  atEnd   (Tangent tr) = withTrail (atEnd   . Tangent) (atEnd   . Tangent) tr

-- | Compute the remainder mod 1.  Convenient for constructing loop
--   parameterizations that wrap around.
mod1 :: Real a => a -> a
mod1 = (`mod'` 1)

instance Num n => DomainBounds (Trail' l v n)

instance (Metric v, OrderedField n, Real n)
  => EndValues (Trail' l v n)

instance (Metric v, OrderedField n, Real n)
    => Sectionable (Trail' Line v n) where
  splitAtParam (Line t) p = (Line t1, Line t2)
    where
      (t1, t2) = splitAtParam t p

  reverseDomain = reverseLine

instance (Metric v, OrderedField n, Real n)
    => HasArcLength (Trail' l v n) where
  arcLengthBounded eps =
    withTrail'
      (\(Line t) -> arcLengthBounded eps t)
      (arcLengthBounded eps . cutLoop)

  arcLengthToParam eps tr l =
    withTrail'
      (\(Line t) -> arcLengthToParam eps t l)
      (\lp -> arcLengthToParam eps (cutLoop lp) l)
      tr

--------------------------------------------------
-- Extracting segments

-- | A newtype wrapper around trails which exists solely for its
--   'Parametric', 'DomainBounds' and 'EndValues' instances.  The idea
--   is that if @tr@ is a trail, you can write, /e.g./
--
--   @
--   getSegment tr `atParam` 0.6
--   @
--
--   or
--
--   @
--   atStart (getSegment tr)
--   @
--
--   to get the segment at parameter 0.6 or the first segment in the
--   trail, respectively.
--
--   The codomain for 'GetSegment', /i.e./ the result you get from
--   calling 'atParam', 'atStart', or 'atEnd', is @Maybe (v, Segment
--   Closed v, AnIso' n n)@.  @Nothing@ results if
--   the trail is empty; otherwise, you get:
--
--   * the offset from the start of the trail to the beginning of the
--     segment,
--
--   * the segment itself, and
--
--   * a reparameterization isomorphism: in the forward direction, it
--     translates from parameters on the whole trail to a parameters
--     on the segment.  Note that for technical reasons you have to
--     call 'cloneIso' on the @AnIso'@ value to get a real isomorphism
--     you can use.
newtype GetSegment t = GetSegment t

newtype GetSegmentCodomain v n =
  GetSegmentCodomain
    (Maybe ( v n                -- offset from trail start to segment start
           , Segment Closed v n -- the segment
           , AnIso' n n         -- reparameterization, trail <-> segment
           ))

-- | Create a 'GetSegment' wrapper around a trail, after which you can
--   call 'atParam', 'atStart', or 'atEnd' to extract a segment.
getSegment :: t -> GetSegment t
getSegment = GetSegment

type instance V (GetSegment t) = V t
type instance N (GetSegment t) = N t

type instance Codomain (GetSegment t) = GetSegmentCodomain (V t)

-- | Parameters less than 0 yield the first segment; parameters
--   greater than 1 yield the last.  A parameter exactly at the
--   junction of two segments yields the second segment (/i.e./ the
--   one with higher parameter values).
instance (Metric v, OrderedField n) => Parametric (GetSegment (Trail' Line v n)) where
  atParam (GetSegment (Line (SegTree ft))) p
    | p <= 0 = case FT.viewl ft of
        EmptyL   -> GetSegmentCodomain Nothing
        seg :< _ -> GetSegmentCodomain $ Just (zero, seg, reparam 0)

    | p >= 1 = case FT.viewr ft of
        EmptyR     -> GetSegmentCodomain Nothing
        ft' :> seg -> GetSegmentCodomain $ Just (offset ft', seg, reparam (n-1))

    | otherwise
    = let (before, after) = FT.split ((p*n <) . numSegs) ft
      in  case FT.viewl after of
            EmptyL   -> GetSegmentCodomain Nothing
            seg :< _ -> GetSegmentCodomain $ Just (offset before, seg, reparam (numSegs before))
    where
      n = numSegs ft
      reparam k = iso (subtract k . (*n))
                      ((/n) . (+ k))

-- | The parameterization for loops wraps around, /i.e./ parameters
--   are first reduced \"mod 1\".
instance (Metric v, OrderedField n, Real n) => Parametric (GetSegment (Trail' Loop v n)) where
  atParam (GetSegment l) p = atParam (GetSegment (cutLoop l)) (mod1 p)

instance (Metric v, OrderedField n, Real n)
    => Parametric (GetSegment (Trail v n)) where
  atParam (GetSegment t) p
    = withTrail
      ((`atParam` p) . GetSegment)
      ((`atParam` p) . GetSegment)
      t

instance DomainBounds t => DomainBounds (GetSegment t) where
  domainLower (GetSegment t) = domainLower t
  domainUpper (GetSegment t) = domainUpper t

instance (Metric v, OrderedField n, Real n)
    => EndValues (GetSegment (Trail' Line v n)) where
  atStart (GetSegment (Line (SegTree ft)))
    = case FT.viewl ft of
        EmptyL   -> GetSegmentCodomain Nothing
        seg :< _ ->
          let n = numSegs ft
          in  GetSegmentCodomain $ Just (zero, seg, iso (*n) (/n))

  atEnd (GetSegment (Line (SegTree ft)))
    = case FT.viewr ft of
        EmptyR     -> GetSegmentCodomain Nothing
        ft' :> seg ->
          let n = numSegs ft
          in  GetSegmentCodomain $
                Just (offset ft', seg, iso (subtract (n-1) . (*n))
                                         ((/n) . (+ (n-1)))
                     )

instance (Metric v, OrderedField n, Real n)
    => EndValues (GetSegment (Trail' Loop v n)) where
  atStart (GetSegment l) = atStart (GetSegment (cutLoop l))
  atEnd   (GetSegment l) = atEnd   (GetSegment (cutLoop l))

instance (Metric v, OrderedField n, Real n)
    => EndValues (GetSegment (Trail v n)) where
  atStart (GetSegment t)
    = withTrail
      (atStart . GetSegment)
      (atStart . GetSegment)
      t
  atEnd (GetSegment t)
    = withTrail
      (atEnd . GetSegment)
      (atEnd . GetSegment)
      t

--------------------------------------------------
-- The Trail type

-- | @Trail@ is a wrapper around @Trail'@, hiding whether the
--   underlying @Trail'@ is a line or loop (though which it is can be
--   recovered; see /e.g./ 'withTrail').
data Trail v n where
  Trail :: Trail' l v n -> Trail v n

deriving instance Show (v n) => Show (Trail v n)

instance Eq (v n) => Eq (Trail v n) where
  t1 == t2 =
    withTrail
      (\ln1 -> withTrail (\ln2 -> ln1 == ln2) (const False) t2)
      (\lp1 -> withTrail (const False) (\lp2 -> lp1 == lp2) t2)
      t1

instance Ord (v n) => Ord (Trail v n) where
  compare t1 t2 =
    withTrail
      (\ln1 -> withTrail (compare ln1) (const LT) t2)
      (\lp1 -> withTrail (const GT) (compare lp1) t2)
      t1

-- | Two @Trail@s are combined by first ensuring they are both lines
--   (using 'cutTrail' on loops) and then concatenating them.  The
--   result, in general, is a line.  However, there is a special case
--   for the empty line, which acts as the identity (so combining the
--   empty line with a loop results in a loop).
instance (OrderedField n, Metric v) => Semigroup (Trail v n) where
  (Trail (Line (SegTree ft))) <> t2 | FT.null ft = t2
  t1 <> (Trail (Line (SegTree ft))) | FT.null ft = t1
  t1 <> t2 = flip withLine t1 $ \l1 ->
             flip withLine t2 $ \l2 ->
             wrapLine (l1 <> l2)

-- | @Trail@s are combined as described in the 'Semigroup' instance;
--   the empty line is the identity element, with special cases so
--   that combining the empty line with a loop results in the
--   unchanged loop (in all other cases loops will be cut).  Note that
--   this does, in fact, satisfy the monoid laws, though it is a bit
--   strange.  Mostly it is provided for convenience, so one can work
--   directly with @Trail@s instead of working with @Trail' Line@s and
--   then wrapping.
instance (OrderedField n, Metric v) => Monoid (Trail v n) where
  mempty  = wrapLine emptyLine
  mappend = (<>)

type instance V (Trail v n) = v
type instance N (Trail v n) = n

type instance Codomain (Trail v n) = v

instance (HasLinearMap v, Metric v, OrderedField n)
    => Transformable (Trail v n) where
  transform t = onTrail (transform t) (transform t)

instance (Metric v, OrderedField n) => Enveloped (Trail v n) where
  getEnvelope = withTrail getEnvelope getEnvelope

instance (Metric v, OrderedField n, Real n)
    => Parametric (Trail v n) where
  atParam t p = withTrail (`atParam` p) (`atParam` p) t

instance Num n => DomainBounds (Trail v n)

instance (Metric v, OrderedField n, Real n) => EndValues (Trail v n)

-- | Note that there is no @Sectionable@ instance for @Trail' Loop@,
--   because it does not make sense (splitting a loop at a parameter
--   results in a single line, not two loops).  However, it's
--   convenient to have a @Sectionable@ instance for @Trail@; if the
--   @Trail@ contains a loop the loop will first be cut and then
--   @splitAtParam@ called on the resulting line.  This is
--   semantically a bit silly, so please don't rely on it. (*E.g.* if
--   this is really the behavior you want, consider first calling
--   'cutLoop' yourself.)
instance (Metric v, OrderedField n, Real n) => Sectionable (Trail v n) where
  splitAtParam t p = withLine ((wrapLine *** wrapLine) . (`splitAtParam` p)) t

  reverseDomain = reverseTrail

instance (Metric v, OrderedField n, Real n)
    => HasArcLength (Trail v n) where
  arcLengthBounded = withLine . arcLengthBounded
  arcLengthToParam eps tr al = withLine (\ln -> arcLengthToParam eps ln al) tr

--------------------------------------------------
-- Constructors and eliminators for Trail

-- | A generic eliminator for 'Trail', taking functions specifying
--   what to do in the case of a line or a loop.
withTrail :: (Trail' Line v n -> r) -> (Trail' Loop v n -> r) -> Trail v n -> r
withTrail line loop (Trail t) = withTrail' line loop t

-- | Modify a @Trail@, specifying two separate transformations for the
--   cases of a line or a loop.
onTrail :: (Trail' Line v n -> Trail' l1 v n) -> (Trail' Loop v n -> Trail' l2 v n)
        -> Trail v n -> Trail v n
onTrail o c = withTrail (wrapTrail . o) (wrapTrail . c)

-- | An eliminator for @Trail@ based on eliminating lines: if the
--   trail is a line, the given function is applied; if it is a loop, it
--   is first converted to a line with 'cutLoop'.  That is,
--
-- @
-- withLine f === 'withTrail' f (f . 'cutLoop')
-- @
withLine :: (Metric v, OrderedField n)
              => (Trail' Line v n -> r) -> Trail v n -> r
withLine f = withTrail f (f . cutLoop)

-- | Modify a @Trail@ by specifying a transformation on lines.  If the
--   trail is a line, the transformation will be applied directly.  If
--   it is a loop, it will first be cut using 'cutLoop', the
--   transformation applied, and then glued back into a loop with
--   'glueLine'.  That is,
--
--   @
--   onLine f === onTrail f (glueLine . f . cutLoop)
--   @
--
--   Note that there is no corresponding @onLoop@ function, because
--   there is no nice way in general to convert a line into a loop,
--   operate on it, and then convert back.
onLine :: (Metric v, OrderedField n)
       => (Trail' Line v n -> Trail' Line v n) -> Trail v n -> Trail v n
onLine f = onTrail f (glueLine . f . cutLoop)

-- | Convert a 'Trail'' into a 'Trail', hiding the type-level
--   distinction between lines and loops.
wrapTrail :: Trail' l v n -> Trail v n
wrapTrail = Trail

-- | Convert a line into a 'Trail'.  This is the same as 'wrapTrail',
--   but with a more specific type, which can occasionally be
--   convenient for fixing the type of a polymorphic expression.
wrapLine :: Trail' Line v n -> Trail v n
wrapLine = wrapTrail

-- | Convert a loop into a 'Trail'.  This is the same as 'wrapTrail',
--   but with a more specific type, which can occasionally be
--   convenient for fixing the type of a polymorphic expression.
wrapLoop :: Trail' Loop v n -> Trail v n
wrapLoop = wrapTrail

------------------------------------------------------------
--  Constructing trails  -----------------------------------
------------------------------------------------------------

-- | The empty line, which is the identity for concatenation of lines.
emptyLine :: (Metric v, OrderedField n) => Trail' Line v n
emptyLine = Line mempty

-- | A wrapped variant of 'emptyLine'.
emptyTrail :: (Metric v, OrderedField n) => Trail v n
emptyTrail = wrapLine emptyLine

-- | Construct a line from a list of closed segments.
lineFromSegments :: (Metric v, OrderedField n)
                   => [Segment Closed v n] -> Trail' Line v n
lineFromSegments = Line . SegTree . FT.fromList

-- | @trailFromSegments === 'wrapTrail' . 'lineFromSegments'@, for
--   conveniently constructing a @Trail@ instead of a @Trail'@.
trailFromSegments :: (Metric v, OrderedField n)
                  => [Segment Closed v n] -> Trail v n
trailFromSegments = wrapTrail . lineFromSegments

-- | Construct a line containing only linear segments from a list of
--   vectors, where each vector represents the offset from one vertex
--   to the next.  See also 'fromOffsets'.
--
--   <<diagrams/src_Diagrams_Trail_lineFromOffsetsEx.svg#diagram=lineFromOffsetsEx&width=300>>
--
--   > import Diagrams.Coordinates
--   > lineFromOffsetsEx = strokeLine $ lineFromOffsets [ 2 ^& 1, 2 ^& (-1), 2 ^& 0.5 ]
lineFromOffsets :: (Metric v, OrderedField n) => [v n] -> Trail' Line v n
lineFromOffsets = lineFromSegments . map straight

-- | @trailFromOffsets === 'wrapTrail' . 'lineFromOffsets'@, for
--   conveniently constructing a @Trail@ instead of a @Trail' Line@.
trailFromOffsets :: (Metric v, OrderedField n) => [v n] -> Trail v n
trailFromOffsets = wrapTrail . lineFromOffsets

-- | Construct a line containing only linear segments from a list of
--   vertices.  Note that only the relative offsets between the
--   vertices matters; the information about their absolute position
--   will be discarded.  That is, for all vectors @v@,
--
-- @
-- lineFromVertices === lineFromVertices . 'translate' v
-- @
--
--   If you want to retain the position information, you should
--   instead use the more general 'fromVertices' function to
--   construct, say, a @'Located' ('Trail'' 'Line' v)@ or a @'Located'
--   ('Trail' v)@.
--
--   <<diagrams/src_Diagrams_Trail_lineFromVerticesEx.svg#diagram=lineFromVerticesEx&width=300>>
--
--   > import Diagrams.Coordinates
--   > lineFromVerticesEx = pad 1.1 . centerXY . strokeLine
--   >   $ lineFromVertices [origin, 0 ^& 1, 1 ^& 2, 5 ^& 1]
lineFromVertices :: (Metric v, OrderedField n)
                   => [Point v n] -> Trail' Line v n
lineFromVertices []  = emptyLine
lineFromVertices [_] = emptyLine
lineFromVertices ps  = lineFromSegments . map straight $ zipWith (.-.) (tail ps) ps


-- | @trailFromVertices === 'wrapTrail' . 'lineFromVertices'@, for
--   conveniently constructing a @Trail@ instead of a @Trail' Line@.
trailFromVertices :: (Metric v, OrderedField n)
                  => [Point v n] -> Trail v n
trailFromVertices = wrapTrail . lineFromVertices

------------------------------------------------------------
--  Converting between lines and loops  --------------------
------------------------------------------------------------

-- | Make a line into a loop by \"gluing\" the endpoint to the
--   starting point.  In particular, the offset of the final segment
--   is modified so that it ends at the starting point of the entire
--   trail.  Typically, you would first construct a line which you
--   know happens to end where it starts, and then call 'glueLine' to
--   turn it into a loop.
--
--   <<diagrams/src_Diagrams_Trail_glueLineEx.svg#diagram=glueLineEx&width=500>>
--
--   > glueLineEx = pad 1.1 . hcat' (with & sep .~ 1)
--   >   $ [almostClosed # strokeLine, almostClosed # glueLine # strokeLoop]
--   >
--   > almostClosed :: Trail' Line R2
--   > almostClosed = fromOffsets $ map r2 [(2, -1), (-3, -0.5), (-2, 1), (1, 0.5)]
--
--   @glueLine@ is left inverse to 'cutLoop', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
glueLine :: (Metric v, OrderedField n) => Trail' Line v n -> Trail' Loop v n
glueLine (Line (SegTree t)) =
  case FT.viewr t of
    FT.EmptyR           -> Loop mempty (Linear OffsetOpen)
    t' :> Linear _      -> Loop (SegTree t') (Linear OffsetOpen)
    t' :> Cubic c1 c2 _ -> Loop (SegTree t') (Cubic c1 c2 OffsetOpen)

-- | @glueTrail@ is a variant of 'glueLine' which works on 'Trail's.
--   It performs 'glueLine' on lines and is the identity on loops.
glueTrail :: (Metric v, OrderedField n) => Trail v n -> Trail v n
glueTrail = onTrail glueLine id

-- | Make a line into a loop by adding a new linear segment from the
--   line's end to its start.
--
--   @closeLine@ does not have any particularly nice theoretical
--   properties, but can be useful /e.g./ when you want to make a
--   closed polygon out of a list of points where the initial point is
--   not repeated at the end.  To use 'glueLine', one would first have
--   to duplicate the initial vertex, like
--
-- @
-- 'glueLine' . 'lineFromVertices' $ ps ++ [head ps]
-- @
--
--   Using @closeLine@, however, one can simply
--
-- @
-- closeLine . lineFromVertices $ ps
-- @
--
--   <<diagrams/src_Diagrams_Trail_closeLineEx.svg#diagram=closeLineEx&width=500>>
--
--   > closeLineEx = pad 1.1 . centerXY . hcat' (with & sep .~ 1)
--   >   $ [almostClosed # strokeLine, almostClosed # closeLine # strokeLoop]
closeLine :: Trail' Line v n -> Trail' Loop v n
closeLine (Line t) = Loop t (Linear OffsetOpen)

-- | @closeTrail@ is a variant of 'closeLine' for 'Trail', which
--   performs 'closeLine' on lines and is the identity on loops.
closeTrail :: Trail v n -> Trail v n
closeTrail = onTrail closeLine id

-- | Turn a loop into a line by \"cutting\" it at the common start/end
--   point, resulting in a line which just happens to start and end at
--   the same place.
--
--   @cutLoop@ is right inverse to 'glueLine', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
cutLoop :: forall v n. (Metric v, OrderedField n)
         => Trail' Loop v n -> Trail' Line v n
cutLoop (Loop (SegTree t) c) =
  case (FT.null t, c) of
    (True, Linear OffsetOpen)      -> emptyLine
    (_   , Linear OffsetOpen)      -> Line (SegTree (t |> Linear off))
    (_   , Cubic c1 c2 OffsetOpen) -> Line (SegTree (t |> Cubic c1 c2 off))
  where
    offV :: v n
    offV = negated . trailMeasure zero (op TotalOffset .view oeOffset) $ t
    off = OffsetClosed offV

-- | @cutTrail@ is a variant of 'cutLoop' for 'Trail'; it is the is
--   the identity on lines and performs 'cutLoop' on loops.
cutTrail :: (Metric v, OrderedField n)
         => Trail v n -> Trail v n
cutTrail = onTrail id cutLoop

------------------------------------------------------------
--  Eliminating trails  ------------------------------------
------------------------------------------------------------

-- | Test whether a line is empty.
isLineEmpty :: (Metric v, OrderedField n) => Trail' Line v n -> Bool
isLineEmpty (Line (SegTree t)) = FT.null t

-- | Test whether a trail is empty.  Note that loops are never empty.
isTrailEmpty :: (Metric v, OrderedField n) => Trail v n -> Bool
isTrailEmpty = withTrail isLineEmpty (const False)

-- | Determine whether a trail is a line.
isLine :: Trail v n -> Bool
isLine = not . isLoop

-- | Determine whether a trail is a loop.
isLoop :: Trail v n -> Bool
isLoop = withTrail (const False) (const True)

-- | Extract the segments comprising a line.
lineSegments :: Trail' Line v n -> [Segment Closed v n]
lineSegments (Line (SegTree t)) = F.toList t

-- | Modify a line by applying a function to its list of segments.
onLineSegments
  :: (Metric v, OrderedField n)
  => ([Segment Closed v n] -> [Segment Closed v n])
  -> Trail' Line v n -> Trail' Line v n
onLineSegments f = lineFromSegments . f . lineSegments

-- | Extract the segments comprising a loop: a list of closed
--   segments, and one final open segment.
loopSegments :: Trail' Loop v n -> ([Segment Closed v n], Segment Open v n)
loopSegments (Loop (SegTree t) c) = (F.toList t, c)

-- | Extract the segments of a trail.  If the trail is a loop it will
--   first have 'cutLoop' applied.
trailSegments :: (Metric v, OrderedField n)
              => Trail v n -> [Segment Closed v n]
trailSegments = withLine lineSegments

-- | Extract the offsets of the segments of a trail.
trailOffsets :: (Metric v, OrderedField n) => Trail v n -> [v n]
trailOffsets = withLine lineOffsets

-- | Compute the offset from the start of a trail to the end.  Satisfies
--
--   @
--   trailOffset === sumV . trailOffsets
--   @
--
--   but is more efficient.
--
--   <<diagrams/src_Diagrams_Trail_trailOffsetEx.svg#diagram=trailOffsetEx&width=300>>
--
--   > trailOffsetEx = (strokeLine almostClosed <> showOffset) # centerXY # pad 1.1
--   >   where showOffset = fromOffsets [trailOffset (wrapLine almostClosed)]
--   >                    # stroke # lc red
trailOffset :: (Metric v, OrderedField n) => Trail v n -> v n
trailOffset = withLine lineOffset

-- | Extract the offsets of the segments of a line.
lineOffsets :: (Metric v, OrderedField n) => Trail' Line v n -> [v n]
lineOffsets = map segOffset . lineSegments

-- | Extract the offsets of the segments of a loop.
loopOffsets :: (Metric v, OrderedField n) => Trail' Loop v n -> [v n]
loopOffsets = lineOffsets . cutLoop

-- | Compute the offset from the start of a line to the end.  (Note,
--   there is no corresponding @loopOffset@ function because by
--   definition it would be constantly zero.)
lineOffset :: (Metric v, OrderedField n) => Trail' Line v n -> v n
lineOffset (Line t) = trailMeasure zero (op TotalOffset . view oeOffset) t

-- | Extract the points of a concretely located trail.  That is the points
--   where one segment ends and the next begings. Note that
--   for loops, the starting point will /not/ be repeated at the end.
--   If you want this behavior, you can use 'cutTrail' to make the
--   loop into a line first, which happens to repeat the same point
--   at the start and end, /e.g./ with @trailPoints . mapLoc
--   cutTrail@.
--
--   Note that it does not make sense to ask for the points of a
--   'Trail' by itself; if you want the points of a trail
--   with the first point at, say, the origin, you can use
--   @trailPoints . (\`at\` origin)@.
trailPoints :: (Metric v, OrderedField n)
              => Located (Trail v n) -> [Point v n]
trailPoints (viewLoc -> (p,t))
  = withTrail (linePoints . (`at` p)) (loopPoints . (`at` p)) t

-- | Extract the vertices of a concretely located line.  See
--   'trailPoints' for more information.
linePoints :: (Metric v, OrderedField n)
             => Located (Trail' Line v n) -> [Point v n]
linePoints (viewLoc -> (p,t))
  = segmentPoints p . lineSegments $ t

-- | Extract the vertices of a concretely located loop.  Note that the
--   initial vertex is not repeated at the end.  See 'trailPoints' for
--   more information.
loopPoints :: (Metric v, OrderedField n)
             => Located (Trail' Loop v n) -> [Point v n]
loopPoints (viewLoc -> (p,t))
  = segmentPoints p . fst . loopSegments $ t

segmentPoints :: (Additive v, Num n) => Point v n -> [Segment Closed v n] -> [Point v n]
segmentPoints p = scanl (.+^) p . map segOffset

tolerance :: OrderedField a => a
tolerance = 10e-16

-- | Extract the vertices of a concretely located trail.  Note that
--   for loops, the starting vertex will /not/ be repeated at the end.
--   If you want this behavior, you can use 'cutTrail' to make the
--   loop into a line first, which happens to repeat the same vertex
--   at the start and end, /e.g./ with @trailVertices . mapLoc
--   cutTrail@.
--
--   Note that it does not make sense to ask for the vertices of a
--   'Trail' by itself; if you want the vertices of a trail
--   with the first vertex at, say, the origin, you can use
--   @trailVertices . (\`at\` origin)@.
trailVertices' :: (Metric v, OrderedField n)
              => n ->  Located (Trail v n) -> [Point v n]
trailVertices' toler (viewLoc -> (p,t))
  = withTrail (lineVertices' toler . (`at` p)) (loopVertices' toler . (`at` p)) t

-- : Like trailVertices' but the tolerance is set to tolerance
trailVertices :: (Metric v, OrderedField n)
              => Located (Trail v n) -> [Point v n]
trailVertices = trailVertices' tolerance

-- | Extract the vertices of a concretely located line.  See
--   'trailVertices' for more information.
lineVertices' :: (Metric v, OrderedField n)
             => n -> Located (Trail' Line v n) -> [Point v n]
lineVertices' toler (viewLoc -> (p,t))
  = segmentVertices' toler p . lineSegments $ t

-- | Like lineVertices' with tolerance set to tolerance.
lineVertices :: (Metric v, OrderedField n)
             => Located (Trail' Line v n) -> [Point v n]
lineVertices = lineVertices' tolerance

-- | Extract the vertices of a concretely located loop.  Note that the
--   initial vertex is not repeated at the end.  See 'trailVertices' for
--   more information.
loopVertices' :: (Metric v, OrderedField n)
             => n -> Located (Trail' Loop v n) -> [Point v n]
loopVertices' toler (viewLoc -> (p,t))
  | length segs > 1 = if far > toler  then init ps else init . drop 1 $ ps
  | otherwise       = ps
  where
    far = quadrance ((signorm . tangentAtStart . head $ segs) ^-^
                       (signorm . tangentAtEnd   . last $ segs))
    segs = lineSegments . cutLoop $ t
    ps = segmentVertices' toler p segs

-- | Same as loopVertices' with tolerance set to tolerance.
loopVertices :: (Metric v, OrderedField n)
             => Located (Trail' Loop v n) -> [Point v n]
loopVertices = loopVertices' tolerance

-- The vertices of a list of segments laid end to end.
-- The start and end points are always included in the list of vertices.
-- The other points connecting segments are included if the slope at the
-- end of a segment is not equal to the slope at the beginning of the next.
-- The 'toler' parameter is used to control how close the slopes need to
-- be in order to declatre them equal.
segmentVertices' :: (Metric v, OrderedField n)
             => n -> Point v n -> [Segment Closed v n] -> [Point v n]
segmentVertices' toler p ts  =
  case ps of
    (x:_:_) -> x : select (drop 1 ps) ds ++ [last ps]
    _       -> ps
    where
      ds = zipWith far tans (drop 1 tans)
      tans = [(signorm . tangentAtStart $ s
              ,signorm . tangentAtEnd   $ s) | s <- ts]
      ps = scanl (.+^) p . map segOffset $ ts
      far p2 q2 = quadrance (snd p2 ^-^ fst q2) > toler

select :: [a] -> [Bool] -> [a]
select xs bs = map fst $ filter snd (zip xs bs)

-- | Convert a concretely located trail into a list of fixed segments.
fixTrail :: (Metric v, OrderedField n)
         => Located (Trail v n) -> [FixedSegment v n]
fixTrail t = map mkFixedSeg (trailLocSegments t)

-- | Convert a concretely located trail into a list of located segments.
trailLocSegments :: (Metric v, OrderedField n)
                  => Located (Trail v n) -> [Located (Segment Closed v n)]
trailLocSegments t = zipWith at (trailSegments (unLoc t)) (trailPoints t)

------------------------------------------------------------
--  Modifying trails  --------------------------------------
------------------------------------------------------------

-- | Reverse a trail.  Semantically, if a trail given by a function t
--   from [0,1] to vectors, then the reverse of t is given by t'(s) =
--   t(1-s).  @reverseTrail@ is an involution, that is,
--
--   @
--   reverseTrail . reverseTrail === id
--   @
reverseTrail :: (Metric v, OrderedField n) => Trail v n -> Trail v n
reverseTrail = onTrail reverseLine reverseLoop

-- | Reverse a concretely located trail.  The endpoint of the original
--   trail becomes the starting point of the reversed trail, so the
--   original and reversed trails comprise exactly the same set of
--   points.  @reverseLocTrail@ is an involution, /i.e./
--
--   @
--   reverseLocTrail . reverseLocTrail === id
--   @
reverseLocTrail :: (Metric v, OrderedField n)
                => Located (Trail v n) -> Located (Trail v n)
reverseLocTrail (viewLoc -> (p, t)) = reverseTrail t `at` (p .+^ trailOffset t)

-- | Reverse a line.  See 'reverseTrail'.
reverseLine :: (Metric v, OrderedField n)
            => Trail' Line v n -> Trail' Line v n
reverseLine = onLineSegments (reverse . map reverseSegment)

-- | Reverse a concretely located line.  See 'reverseLocTrail'.
reverseLocLine :: (Metric v, OrderedField n)
               => Located (Trail' Line v n) -> Located (Trail' Line v n)
reverseLocLine (viewLoc -> (p,l)) = reverseLine l `at` (p .+^ lineOffset l)

-- | Reverse a loop.  See 'reverseTrail'.
reverseLoop :: (Metric v, OrderedField n)
            => Trail' Loop v n -> Trail' Loop v n
reverseLoop = glueLine . reverseLine . cutLoop

-- | Reverse a concretely located loop.  See 'reverseLocTrail'.  Note
--   that this is guaranteed to preserve the location.
reverseLocLoop :: (Metric v, OrderedField n)
               => Located (Trail' Loop v n) -> Located (Trail' Loop v n)
reverseLocLoop = mapLoc reverseLoop

