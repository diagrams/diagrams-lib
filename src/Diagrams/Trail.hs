{-# LANGUAGE DeriveFunctor              #-}
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
       , trailVertices, lineVertices, loopVertices
       , fixTrail

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

       ) where

import           Control.Arrow       ((***))
import           Data.AffineSpace
import           Data.FingerTree     (FingerTree, ViewL (..), ViewR (..), (<|),
                                      (|>))
import qualified Data.FingerTree     as FT
import qualified Data.Foldable       as F
import           Data.Monoid.MList
import           Data.Semigroup
import           Data.VectorSpace    hiding (Sum (..))
import qualified Numeric.Interval    as I

import           Diagrams.Core       hiding ((|>))
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment

-- $internals
--
-- Most users of diagrams should not need to use anything in this
-- section directly, but they are exported on the principle that we
-- can't forsee what uses people might have for them.

------------------------------------------------------------
--  FingerTree instances
------------------------------------------------------------

type instance V (FingerTree m a) = V a

instance ( HasLinearMap (V a), InnerSpace (V a), OrderedField (Scalar (V a))
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
newtype SegTree v = SegTree
                  { getSegTree :: FingerTree (SegMeasure v) (Segment Closed v) }
  deriving (Eq, Ord, Show)

type instance V (SegTree v) = v

deriving instance (OrderedField (Scalar v), InnerSpace v)
  => Monoid (SegTree v)
deriving instance (OrderedField (Scalar v), InnerSpace v)
  => FT.Measured (SegMeasure v) (SegTree v)
deriving instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
  => Transformable (SegTree v)

type instance Codomain (SegTree v) = v

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => Parametric (SegTree v) where
  atParam t p = offset . fst $ splitAtParam t p

instance Num (Scalar v) => DomainBounds (SegTree v)

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v), Num (Scalar v))
    => EndValues (SegTree v)

instance (InnerSpace v, RealFrac (Scalar v), Floating (Scalar v))
    => Sectionable (SegTree v) where
  splitAtParam (SegTree t) p
    | p < 0     = case FT.viewl t of
                    EmptyL    -> emptySplit
                    seg :< t' ->
                      case seg `splitAtParam` (p * tSegs) of
                        (seg1, seg2) -> ( SegTree $ FT.singleton seg1
                                        , SegTree $ seg2 <| t'
                                        )
    | p >= 1    = case FT.viewr t of
                    EmptyR    -> emptySplit
                    t' :> seg ->
                      case seg `splitAtParam` (1 - (1 - p)*tSegs) of
                        (seg1, seg2) -> ( SegTree $ t' |> seg1
                                        , SegTree $ FT.singleton seg2
                                        )
    | otherwise = case FT.viewl after of
                    EmptyL    -> emptySplit
                    seg :< after' ->
                      case seg `splitAtParam` (snd . properFraction $ p * tSegs) of
                        (seg1, seg2) -> ( SegTree $ before |> seg1
                                        , SegTree $ seg2   <| after'
                                        )
    where
      (before, after) = FT.split ((p * tSegs <) . numSegs) t
      tSegs           = numSegs t
      emptySplit      = (SegTree t, SegTree t)

  reverseDomain (SegTree t) = SegTree $ FT.reverse t'
    where t' = FT.fmap' reverseSegment t

  -- XXX seems like it should be possible to collapse some of the
  -- above cases into one?

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => HasArcLength (SegTree v) where
  arcLengthBounded eps t
    -- Use the cached value if it is accurate enough; otherwise fall
    -- back to recomputing a more accurate value
    | I.width i <= eps = i
    | otherwise        = fun (eps / numSegs t)
    where
      i   = trailMeasure (I.singleton 0)
              (getArcLengthCached :: ArcLength v -> I.Interval (Scalar v))
              t
      fun = trailMeasure (const 0)
              (getArcLengthFun :: ArcLength v -> Scalar v -> I.Interval (Scalar v))
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
                       seg :< after' ->
                         let p = arcLengthToParam (eps/2) seg
                                   (l - arcLength (eps/2) (SegTree before))
                         in  (numSegs before + p) / tSegs
    where
      totalAL         = arcLength eps st
      tSegs           = numSegs t
      before, after :: FingerTree (SegMeasure v) (Segment Closed v)
      (before, after) = FT.split ((>= l) . trailMeasure 0 (I.midpoint . (getArcLengthBounded eps :: ArcLength v -> I.Interval (Scalar v)))) t

-- | Given a default result (to be used in the case of an empty
--   trail), and a function to map a single measure to a result,
--   extract the given measure for a trail and use it to compute a
--   result.  Put another way, lift a function on a single measure
--   (along with a default value) to a function on an entire trail.
trailMeasure :: ( InnerSpace v, OrderedField (Scalar v)
                , SegMeasure v :>: m, FT.Measured (SegMeasure v) t
                )
             => a -> (m -> a) -> t -> a
trailMeasure d f = option d f . get . FT.measure

-- | Compute the number of segments of anything measured by
--   'SegMeasure' (/e.g./ @SegMeasure@ itself, @Segment@, @SegTree@,
--   @Trail@s...)
numSegs :: ( Floating (Scalar v), Num c, Ord (Scalar v), InnerSpace v,
             FT.Measured (SegMeasure v) a
           )
        => a -> c
numSegs = fromIntegral . trailMeasure 0 (getSum . getSegCount)

-- | Compute the total offset of anything measured by 'SegMeasure'.
offset :: ( Floating (Scalar v), Ord (Scalar v), InnerSpace v,
            FT.Measured (SegMeasure v) t
          )
       => t -> v
offset = trailMeasure zeroV (getTotalOffset . oeOffset)

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

data Trail' l v where
  Line :: SegTree v                   -> Trail' Line v
  Loop :: SegTree v -> Segment Open v -> Trail' Loop v

-- | A generic eliminator for 'Trail'', taking functions specifying
--   what to do in the case of a line or a loop.
withTrail' :: (Trail' Line v -> r) -> (Trail' Loop v -> r) -> Trail' l v -> r
withTrail' line loop t@(Line{}) = line t
withTrail' line loop t@(Loop{}) = loop t

deriving instance Show v => Show (Trail' l v)
deriving instance Eq   v => Eq   (Trail' l v)
deriving instance Ord  v => Ord  (Trail' l v)

type instance V (Trail' l v) = v

type instance Codomain (Trail' l v) = v

instance (OrderedField (Scalar v), InnerSpace v) => Semigroup (Trail' Line v) where
  (Line t1) <> (Line t2) = Line (t1 `mappend` t2)

-- | The empty trail is constantly the zero vector.  Trails are
--   composed via concatenation.  Note that only lines have a monoid
--   instance (and not loops).
instance (OrderedField (Scalar v), InnerSpace v) => Monoid (Trail' Line v) where
  mempty  = emptyLine
  mappend = (<>)

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
    => Transformable (Trail' l v) where
  transform tr (Line t  ) = Line (transform tr t)
  transform tr (Loop t s) = Loop (transform tr t) (transform tr s)

-- | The envelope for a trail is based at the trail's start.
instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Trail' l v) where
  getEnvelope = withTrail' ftEnv (ftEnv . cutLoop)
    where
      ftEnv :: Trail' Line v -> Envelope v
      ftEnv (Line t) = trailMeasure mempty oeEnvelope $ t

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
    => Renderable (Trail' o v) NullBackend where
  render _ _ = mempty

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => Parametric (Trail' l v) where
  atParam t p = withTrail'
                  (\(Line segT) -> segT `atParam` p)
                  (\l -> cutLoop l `atParam` p')
                  t
    where
      pf = snd . properFraction $ p
      p' | p >= 0    = pf
         | otherwise = 1 + pf

instance Num (Scalar v) => DomainBounds (Trail' l v)

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
  => EndValues (Trail' l v)

instance (InnerSpace v, RealFrac (Scalar v), Floating (Scalar v))
    => Sectionable (Trail' Line v) where
  splitAtParam (Line t) p = (Line t1, Line t2)
    where
      (t1, t2) = splitAtParam t p

  reverseDomain = reverseLine

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => HasArcLength (Trail' l v) where
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
-- The Trail type

-- | @Trail@ is a wrapper around @Trail'@, hiding whether the
--   underlying @Trail'@ is a line or loop (though which it is can be
--   recovered; see /e.g./ 'withTrail').
data Trail v where
  Trail :: Trail' l v -> Trail v

deriving instance Show v => Show (Trail v)

instance Eq v => Eq (Trail v) where
  t1 == t2 =
    withTrail
      (\ln1 -> withTrail (\ln2 -> ln1 == ln2) (const False) t2)
      (\lp1 -> withTrail (const False) (\lp2 -> lp1 == lp2) t2)
      t1

instance Ord v => Ord (Trail v) where
  compare t1 t2 =
    withTrail
      (\ln1 -> withTrail (\ln2 -> compare ln1 ln2) (const LT) t2)
      (\lp1 -> withTrail (const GT) (\lp2 -> compare lp1 lp2) t2)
      t1

-- | Two @Trail@s are combined by first ensuring they are both lines
--   (using 'cutTrail' on loops) and then concatenating them.  The
--   result, in general, is a line.  However, there is a special case
--   for the empty line, which acts as the identity (so combining the
--   empty line with a loop results in a loop).
instance (OrderedField (Scalar v), InnerSpace v) => Semigroup (Trail v) where
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
instance (OrderedField (Scalar v), InnerSpace v) => Monoid (Trail v) where
  mempty  = wrapLine emptyLine
  mappend = (<>)

type instance V (Trail v) = v

type instance Codomain (Trail v) = v

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
    => Transformable (Trail v) where
  transform t = onTrail (transform t) (transform t)

instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Trail v) where
  getEnvelope = withTrail getEnvelope getEnvelope

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => Parametric (Trail v) where
  atParam t p = withTrail (`atParam` p) (`atParam` p) t

instance Num (Scalar v) => DomainBounds (Trail v)

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
  => EndValues (Trail v)

-- | Note that there is no @Sectionable@ instance for @Trail' Loop@,
--   because it does not make sense (splitting a loop at a parameter
--   results in a single line, not two loops).  However, it's
--   convenient to have a @Sectionable@ instance for @Trail@; if the
--   @Trail@ contains a loop the loop will first be cut and then
--   @splitAtParam@ called on the resulting line.  This is
--   semantically a bit silly, so please don't rely on it. (*E.g.* if
--   this is really the behavior you want, consider first calling
--   'cutLoop' yourself.)
instance (InnerSpace v, RealFrac (Scalar v), Floating (Scalar v))
    => Sectionable (Trail v) where
  splitAtParam t p = withLine ((wrapLine *** wrapLine) . (`splitAtParam` p)) t

  reverseDomain = reverseTrail

instance (InnerSpace v, OrderedField (Scalar v), RealFrac (Scalar v))
    => HasArcLength (Trail v) where
  arcLengthBounded = withLine . arcLengthBounded
  arcLengthToParam eps tr al = withLine (\ln -> arcLengthToParam eps ln al) tr

--------------------------------------------------
-- Constructors and eliminators for Trail

-- | A generic eliminator for 'Trail', taking functions specifying
--   what to do in the case of a line or a loop.
withTrail :: (Trail' Line v -> r) -> (Trail' Loop v -> r) -> Trail v -> r
withTrail line loop (Trail t) = withTrail' line loop t

-- | Modify a @Trail@, specifying two separate transformations for the
--   cases of a line or a loop.
onTrail :: (Trail' Line v -> Trail' l1 v) -> (Trail' Loop v -> Trail' l2 v)
        -> (Trail v -> Trail v)
onTrail o c = withTrail (wrapTrail . o) (wrapTrail . c)

-- | An eliminator for @Trail@ based on eliminating lines: if the
--   trail is a line, the given function is applied; if it is a loop, it
--   is first converted to a line with 'cutLoop'.  That is,
--
-- @
-- withLine f === 'withTrail' f (f . 'cutLoop')
-- @
withLine :: (InnerSpace v, OrderedField (Scalar v))
              => (Trail' Line v -> r) -> Trail v -> r
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
onLine :: (InnerSpace v, OrderedField (Scalar v))
            => (Trail' Line v -> Trail' Line v) -> Trail v -> Trail v
onLine f = onTrail f (glueLine . f . cutLoop)

-- | Convert a 'Trail'' into a 'Trail', hiding the type-level
--   distinction between lines and loops.
wrapTrail :: Trail' l v -> Trail v
wrapTrail = Trail

-- | Convert a line into a 'Trail'.  This is the same as 'wrapTrail',
--   but with a more specific type, which can occasionally be
--   convenient for fixing the type of a polymorphic expression.
wrapLine :: Trail' Line v -> Trail v
wrapLine = wrapTrail

-- | Convert a loop into a 'Trail'.  This is the same as 'wrapTrail',
--   but with a more specific type, which can occasionally be
--   convenient for fixing the type of a polymorphic expression.
wrapLoop :: Trail' Loop v -> Trail v
wrapLoop = wrapTrail

------------------------------------------------------------
--  Constructing trails  -----------------------------------
------------------------------------------------------------

-- | The empty line, which is the identity for concatenation of lines.
emptyLine :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Line v
emptyLine = Line mempty

-- | A wrapped variant of 'emptyLine'.
emptyTrail :: (InnerSpace v, OrderedField (Scalar v)) => Trail v
emptyTrail = wrapLine emptyLine

-- | Construct a line from a list of closed segments.
lineFromSegments :: (InnerSpace v, OrderedField (Scalar v))
                   => [Segment Closed v] -> Trail' Line v
lineFromSegments = Line . SegTree . FT.fromList

-- | @trailFromSegments === 'wrapTrail' . 'lineFromSegments'@, for
--   conveniently constructing a @Trail@ instead of a @Trail'@.
trailFromSegments :: (InnerSpace v, OrderedField (Scalar v))
                  => [Segment Closed v] -> Trail v
trailFromSegments = wrapTrail . lineFromSegments

-- | Construct a line containing only linear segments from a list of
--   vectors, where each vector represents the offset from one vertex
--   to the next.  See also 'fromOffsets'.
--
--   <<diagrams/lineFromOffsetsEx.svg#diagram=lineFromOffsetsEx&width=300>>
--
--   > import Diagrams.Coordinates
--   > lineFromOffsetsEx = strokeLine $ lineFromOffsets [ 2 & 1, 2 & (-1), 2 & 0.5 ]
lineFromOffsets :: (InnerSpace v, OrderedField (Scalar v))
                  => [v] -> Trail' Line v
lineFromOffsets = lineFromSegments . map straight

-- | @trailFromOffsets === 'wrapTrail' . 'lineFromOffsets'@, for
--   conveniently constructing a @Trail@ instead of a @Trail' Line@.
trailFromOffsets :: (InnerSpace v, OrderedField (Scalar v))
                 => [v] -> Trail v
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
--   <<diagrams/lineFromVerticesEx.svg#diagram=lineFromVerticesEx&width=300>>
--
--   > import Diagrams.Coordinates
--   > lineFromVerticesEx = pad 1.1 . centerXY . strokeLine
--   >   $ lineFromVertices [origin, 0 & 1, 1 & 2, 5 & 1]
lineFromVertices :: (InnerSpace v, OrderedField (Scalar v))
                   => [Point v] -> Trail' Line v
lineFromVertices []  = emptyLine
lineFromVertices [_] = emptyLine
lineFromVertices ps  = lineFromSegments . map straight $ zipWith (.-.) (tail ps) ps


-- | @trailFromVertices === 'wrapTrail' . 'lineFromVertices'@, for
--   conveniently constructing a @Trail@ instead of a @Trail' Line@.
trailFromVertices :: (InnerSpace v, OrderedField (Scalar v))
                  => [Point v] -> Trail v
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
--   <<diagrams/glueLineEx.svg#diagram=glueLineEx&width=500>>
--
--   > import Diagrams.Coordinates
--   > glueLineEx = pad 1.1 . hcat' with {sep = 1}
--   >   $ [almostClosed # strokeLine, almostClosed # glueLine # strokeLoop]
--   >
--   > almostClosed :: Trail' Line R2
--   > almostClosed = fromOffsets [2 & (-1), (-3) & (-0.5), (-2) & 1, 1 & 0.5]
--
--   @glueLine@ is left inverse to 'cutLoop', that is,
--
--   @
--   glueLine . cutLoop === id
--   @
glueLine :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Line v -> Trail' Loop v
glueLine (Line (SegTree t)) =
  case FT.viewr t of
    FT.EmptyR             -> Loop mempty (Linear OffsetOpen)
    t' :> (Linear _)      -> Loop (SegTree t') (Linear OffsetOpen)
    t' :> (Cubic c1 c2 _) -> Loop (SegTree t') (Cubic c1 c2 OffsetOpen)

-- | @glueTrail@ is a variant of 'glueLine' which works on 'Trail's.
--   It performs 'glueLine' on lines and is the identity on loops.
glueTrail :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> Trail v
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
--   <<diagrams/closeLineEx.svg#diagram=closeLineEx&width=500>>
--
--   > closeLineEx = pad 1.1 . centerXY . hcat' with {sep = 1}
--   >   $ [almostClosed # strokeLine, almostClosed # closeLine # strokeLoop]
closeLine :: Trail' Line v -> Trail' Loop v
closeLine (Line t) = Loop t (Linear OffsetOpen)

-- | @closeTrail@ is a variant of 'closeLine' for 'Trail', which
--   performs 'closeLine' on lines and is the identity on loops.
closeTrail :: Trail v -> Trail v
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
cutLoop :: forall v. (InnerSpace v, OrderedField (Scalar v))
         => Trail' Loop v -> Trail' Line v
cutLoop (Loop (SegTree t) c) =
  case (FT.null t, c) of
    (True, Linear OffsetOpen)      -> emptyLine
    (_   , Linear OffsetOpen)      -> Line (SegTree (t |> Linear off))
    (_   , Cubic c1 c2 OffsetOpen) -> Line (SegTree (t |> Cubic c1 c2 off))
  where
    offV :: v
    offV = negateV . trailMeasure zeroV (getTotalOffset . oeOffset) $ t
    off = OffsetClosed offV

-- | @cutTrail@ is a variant of 'cutLoop' for 'Trail'; it is the is
--   the identity on lines and performs 'cutLoop' on loops.
cutTrail :: (InnerSpace v, OrderedField (Scalar v))
         => Trail v -> Trail v
cutTrail = onTrail id cutLoop

------------------------------------------------------------
--  Eliminating trails  ------------------------------------
------------------------------------------------------------

-- | Test whether a line is empty.
isLineEmpty :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Line v -> Bool
isLineEmpty (Line (SegTree t)) = FT.null t

-- | Test whether a trail is empty.  Note that loops are never empty.
isTrailEmpty :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> Bool
isTrailEmpty = withTrail isLineEmpty (const False)

-- | Determine whether a trail is a line.
isLine :: Trail v -> Bool
isLine = not . isLoop

-- | Determine whether a trail is a loop.
isLoop :: Trail v -> Bool
isLoop = withTrail (const False) (const True)

-- | Extract the segments comprising a line.
lineSegments :: Trail' Line v -> [Segment Closed v]
lineSegments (Line (SegTree t)) = F.toList t

-- | Modify a line by applying a function to its list of segments.
onLineSegments
  :: (InnerSpace v, OrderedField (Scalar v))
  => ([Segment Closed v] -> [Segment Closed v])
  -> Trail' Line v -> Trail' Line v
onLineSegments f = lineFromSegments . f . lineSegments

-- | Extract the segments comprising a loop: a list of closed
--   segments, and one final open segment.
loopSegments :: Trail' Loop v -> ([Segment Closed v], Segment Open v)
loopSegments (Loop (SegTree t) c) = (F.toList t, c)

-- | Extract the segments of a trail.  If the trail is a loop it will
--   first have 'cutLoop' applied.
trailSegments :: (InnerSpace v, OrderedField (Scalar v))
              => Trail v -> [Segment Closed v]
trailSegments = withLine lineSegments

-- | Extract the offsets of the segments of a trail.
trailOffsets :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> [v]
trailOffsets = withLine lineOffsets

-- | Compute the offset from the start of a trail to the end.  Satisfies
--
--   @
--   trailOffset === sumV . trailOffsets
--   @
--
--   but is more efficient.
--
--   <<diagrams/trailOffsetEx.svg#diagram=trailOffsetEx&width=300>>
--
--   > trailOffsetEx = (strokeLine almostClosed <> showOffset) # centerXY # pad 1.1
--   >   where showOffset = fromOffsets [trailOffset (wrapLine almostClosed)]
--   >                    # stroke # lc red # lw 0.05
trailOffset :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> v
trailOffset = withLine lineOffset

-- | Extract the offsets of the segments of a line.
lineOffsets :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Line v -> [v]
lineOffsets = map segOffset . lineSegments

-- | Extract the offsets of the segments of a loop.
loopOffsets :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Loop v -> [v]
loopOffsets = lineOffsets . cutLoop

-- | Compute the offset from the start of a line to the end.  (Note,
--   there is no corresponding @loopOffset@ function because by
--   definition it would be constantly zero.)
lineOffset :: (InnerSpace v, OrderedField (Scalar v)) => Trail' Line v -> v
lineOffset (Line t) = trailMeasure zeroV (getTotalOffset . oeOffset) t

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
--   @trailVertices . (`at` origin)@.
trailVertices :: (InnerSpace v, OrderedField (Scalar v))
              => Located (Trail v) -> [Point v]
trailVertices (viewLoc -> (p,t))
  = withTrail (lineVertices . (`at` p)) (loopVertices . (`at` p)) t

-- | Extract the vertices of a concretely located line.  See
--   'trailVertices' for more information.
lineVertices :: (InnerSpace v, OrderedField (Scalar v))
             => Located (Trail' Line v) -> [Point v]
lineVertices (viewLoc -> (p,t))
  = segmentVertices p . lineSegments $ t

-- | Extract the vertices of a concretely located loop.  Note that the
--   initial vertex is not repeated at the end.  See 'trailVertices' for
--   more information.
loopVertices :: (InnerSpace v, OrderedField (Scalar v))
             => Located (Trail' Loop v) -> [Point v]
loopVertices (viewLoc -> (p,t))
  = segmentVertices p . fst . loopSegments $ t

segmentVertices :: AdditiveGroup v => Point v -> [Segment Closed v] -> [Point v]
segmentVertices p = scanl (.+^) p . map segOffset

-- | Convert a concretely located trail into a list of fixed segments.
fixTrail :: (InnerSpace v, OrderedField (Scalar v))
         => Located (Trail v) -> [FixedSegment v]
fixTrail t = zipWith ((mkFixedSeg .) . at)
               (trailSegments (unLoc t)) (trailVertices t)

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
reverseTrail :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> Trail v
reverseTrail = onTrail reverseLine reverseLoop

-- | Reverse a concretely located trail.  The endpoint of the original
--   trail becomes the starting point of the reversed trail, so the
--   original and reversed trails comprise exactly the same set of
--   points.  @reverseLocTrail@ is an involution, /i.e./
--
--   @
--   reverseLocTrail . reverseLocTrail === id
--   @
reverseLocTrail :: (InnerSpace v, OrderedField (Scalar v))
                => Located (Trail v) -> Located (Trail v)
reverseLocTrail (viewLoc -> (p, t)) = reverseTrail t `at` (p .+^ trailOffset t)

-- | Reverse a line.  See 'reverseTrail'.
reverseLine :: (InnerSpace v, OrderedField (Scalar v))
            => Trail' Line v -> Trail' Line v
reverseLine = onLineSegments (reverse . map reverseSegment)

-- | Reverse a concretely located line.  See 'reverseLocTrail'.
reverseLocLine :: (InnerSpace v, OrderedField (Scalar v))
               => Located (Trail' Line v) -> Located (Trail' Line v)
reverseLocLine (viewLoc -> (p,l)) = reverseLine l `at` (p .+^ lineOffset l)

-- | Reverse a loop.  See 'reverseTrail'.
reverseLoop :: (InnerSpace v, OrderedField (Scalar v))
            => Trail' Loop v -> Trail' Loop v
reverseLoop = glueLine . reverseLine . cutLoop

-- | Reverse a concretely located loop.  See 'reverseLocTrail'.  Note
--   that this is guaranteed to preserve the location.
reverseLocLoop :: (InnerSpace v, OrderedField (Scalar v))
               => Located (Trail' Loop v) -> Located (Trail' Loop v)
reverseLocLoop = mapLoc reverseLoop
