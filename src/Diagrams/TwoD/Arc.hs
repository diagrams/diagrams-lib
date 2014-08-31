{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arc
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional arcs, approximated by cubic bezier curves.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arc
    ( arc
    , arc'
    , arcT
    , bezierFromSweep

    , wedge
    , arcBetween
    , annularWedge
    ) where

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Direction
import           Diagrams.Located        (at)
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (unitX, unitY, unit_Y)
import           Diagrams.Util           (( # ))

import           Control.Lens            ((&), (<>~), (^.))
import           Data.Semigroup          ((<>))

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- For details of this approximation see:
--   http://www.tinaja.com/glib/bezcirc2.pdf

-- | @bezierFromSweepQ1 s@ constructs a 'Cubic' segment that starts in
--  the positive y direction and sweeps counterclockwise through an
--  angle @s@.  The approximation is only valid for angles in the
--  first quadrant.
bezierFromSweepQ1 :: Floating n => Angle n -> Segment Closed V2 n
bezierFromSweepQ1 s = mapSegmentVectors (^-^ v) . rotate (s ^/ 2) $ bezier3 c2 c1 p0
  where p0@(V2 x y) = rotate (s ^/ 2) v
        c1          = V2 ((4-x)/3) ((1-x)*(3-x)/(3*y))
        c2          = reflectY c1
        v           = unitX

-- | @bezierFromSweep s@ constructs a series of 'Cubic' segments that
--   start in the positive y direction and sweep counter clockwise
--   through the angle @s@.  If @s@ is negative, it will start in the
--   negative y direction and sweep clockwise.  When @s@ is less than
--   0.0001 the empty list results.  If the sweep is greater than @fullTurn@
--   later segments will overlap earlier segments.
bezierFromSweep :: OrderedField n => Angle n -> [Segment Closed V2 n]
bezierFromSweep s
  | s < zero          = fmap reflectY . bezierFromSweep $ negated s
  | s < 0.0001 @@ rad = []
  | s < fullTurn^/4   = [bezierFromSweepQ1 s]
  | otherwise         = bezierFromSweepQ1 (fullTurn^/4)
          : map (rotateBy (1/4)) (bezierFromSweep (max (s ^-^ fullTurn^/4) zero))

{-
~~~~ Note [segment spacing]

There are a few obvious options for segment spacing:
   A. Evenly space segments each with sweep less than or equal
      to one quarter of a circle.  This has the benefit of a better approximation
      (at least I think it is better).
   B. Use as much of the sweep in quarter-circle sized segments and one for
      the remainder.  This potentially gives more opportunities for
      consistency (though not as much as option C) as the error in
      approximation would more often match the error from another arc
      in the diagram.
   C. Like option B but fixing the orientation and having a remnant at
      the beginning and the end.

Option B is implemented and this note is for posterity if anyone comes
across a situation with large enough arcs that they can actually see
the approximation error.
-}

-- | Given a start direction @d@ and a sweep angle @s@, @'arcT' d s@
--   is the 'Trail' of a radius one arc starting at @d@ and sweeping out
--   the angle @s@ counterclockwise (for positive s).  The resulting
--   @Trail@ is allowed to wrap around and overlap itself.
arcT :: RealFloat n => Direction V2 n -> Angle n -> Trail V2 n
arcT start sweep = trailFromSegments bs
  where
    bs = map (rotate $ start ^. _theta) . bezierFromSweep $ sweep

-- | Given a start direction @d@ and a sweep angle @s@, @'arc' d s@ is the
--   path of a radius one arc starting at @d@ and sweeping out the angle
--   @s@ counterclockwise (for positive s).  The resulting
--   @Trail@ is allowed to wrap around and overlap itself.
arc :: (TrailLike t, Vn t ~ V2 n, RealFloat n) => Direction V2 n -> Angle n -> t
arc start sweep = trailLike $ arcT start sweep `at` rotate (start ^. _theta) (p2 (1,0))

-- | Given a radus @r@, a start direction @d@ and an angle @s@,
--   @'arc'' r d s@ is the path of a radius @(abs r)@ arc starting at
--   @d@ and sweeping out the angle @s@ counterclockwise (for positive
--   s).  The origin of the arc is its center.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_arc'Ex.svg#diagram=arc'Ex&width=300>>
--
--   > arc'Ex = mconcat [ arc' r (0 @@ turn) (1/4 @@ turn) | r <- [0.5,-1,1.5] ]
--   >        # centerXY # pad 1.1
arc' :: (TrailLike t, Vn t ~ V2 n, RealFloat n) => n -> Direction V2 n -> Angle n -> t
arc' r start sweep = trailLike $ scale (abs r) ts `at` rotate (start ^. _theta) (p2 (abs r,0))
  where ts = arcT start sweep

-- | Create a circular wedge of the given radius, beginning at the
--   given direction and extending through the given angle.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_wedgeEx.svg#diagram=wedgeEx&width=400>>
--
--   > wedgeEx = hcat' (with & sep .~ 0.5)
--   >   [ wedge 1 xDir (1/4 \@\@ turn)
--   >   , wedge 1 (rotate (7/30 \@\@ turn) xDir) (4/30 \@\@ turn)
--   >   , wedge 1 (rotate (1/8 \@\@ turn) xDir) (3/4 \@\@ turn)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
wedge :: (TrailLike t, Vn t ~ V2 n, RealFloat n) => n -> Direction V2 n -> Angle n -> t
wedge r d s = trailLike . (`at` origin) . glueTrail . wrapLine
              $ fromOffsets [r *^ fromDirection d]
                <> arc d s # scale r
                <> fromOffsets [r *^ negated (rotate s $ fromDirection d)]

-- | @arcBetween p q height@ creates an arc beginning at @p@ and
--   ending at @q@, with its midpoint at a distance of @abs height@
--   away from the straight line from @p@ to @q@.  A positive value of
--   @height@ results in an arc to the left of the line from @p@ to
--   @q@; a negative value yields one to the right.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_arcBetweenEx.svg#diagram=arcBetweenEx&width=300>>
--
--   > arcBetweenEx = mconcat
--   >   [ arcBetween origin (p2 (2,1)) ht | ht <- [-0.2, -0.1 .. 0.2] ]
--   >   # centerXY # pad 1.1
arcBetween :: (TrailLike t, Vn t ~ V2 n, RealFloat n) => Point V2 n -> Point V2 n -> n -> t
arcBetween p q ht = trailLike (a # rotate (v^._theta) # moveTo p)
  where
    h = abs ht
    isStraight = h < 0.00001
    v = q .-. p
    d = norm (q .-. p)
    th  = acosA ((d*d - 4*h*h)/(d*d + 4*h*h))
    r = d/(2*sinA th)
    mid | ht >= 0    = direction unitY
        | otherwise  = direction unit_Y
    st  = mid & _theta <>~ negated th
    a | isStraight
      = fromOffsets [d *^ unitX]
      | otherwise
      = arc st (2 *^ th)
        # scale r
        # translateY ((if ht > 0 then negate else id) (r - h))
        # translateX (d/2)
        # (if ht > 0 then reverseLocTrail else id)

-- | Create an annular wedge of the given radii, beginning at the
--   first direction and extending through the given sweep angle.
--   The radius of the outer circle is given first.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_annularWedgeEx.svg#diagram=annularWedgeEx&width=400>>
--
--   > annularWedgeEx = hcat' (with & sep .~ 0.50)
--   >   [ annularWedge 1 0.5 xDir (1/4 @@ turn)
--   >   , annularWedge 1 0.3 (rotate (7/30 @@ turn) xDir)n (4/30 @@ turn)
--   >   , annularWedge 1 0.7 (rotate (1/8 @@ turn) xDir) (3/4 @@ turn)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
annularWedge :: (TrailLike t, Vn t ~ V2 n, RealFloat n) =>
                n -> n -> Direction V2 n -> Angle n -> t
annularWedge r1' r2' d1 s = trailLike . (`at` o) . glueTrail . wrapLine
              $ fromOffsets [(r1' - r2') *^ fromDirection d1]
                <> arc d1 s # scale r1'
                <> fromOffsets [(r1' - r2') *^ negated (fromDirection d2)]
                <> arc d2 (negated s) # scale r2'
  where o = origin # translate (r2' *^ fromDirection d1)
        d2 = d1 & _theta <>~ s

