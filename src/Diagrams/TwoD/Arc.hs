{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
    , arcCW
    , arcT
    , bezierFromSweep

    , wedge
    , arcBetween
    , annularWedge
    ) where

import           Diagrams.Core
import           Diagrams.Located        (at)
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (direction, e, unitX)
import           Diagrams.Util           (( # ))

import           Control.Lens            ((^.))
import           Data.AffineSpace        ((.-.))
import           Data.Semigroup          ((<>))
import           Data.VectorSpace        (magnitude, negateV, (*^), (^-^))
import           Diagrams.Coordinates

-- For details of this approximation see:
--   http://www.tinaja.com/glib/bezcirc2.pdf

-- | @bezierFromSweepQ1 s@ constructs a 'Cubic' segment that starts in
--   the positive y direction and sweeps counterclockwise through @s@
--   radians.  The approximation is only valid for angles in the first
--   quadrant.
bezierFromSweepQ1 :: Angle -> Segment Closed R2
bezierFromSweepQ1 s = fmap (^-^ v) . rotate (s/2) $ bezier3 c2 c1 p0
  where p0@(coords -> x :& y) = rotate (s/2) v
        c1                    = ((4-x)/3)  ^&  ((1-x)*(3-x)/(3*y))
        c2                    = reflectY c1
        v                     = unitX

-- | @bezierFromSweep s@ constructs a series of 'Cubic' segments that
--   start in the positive y direction and sweep counter clockwise
--   through the angle @s@.  If @s@ is negative, it will start in the
--   negative y direction and sweep clockwise.  When @s@ is less than
--   0.0001 the empty list results.  If the sweep is greater than tau
--   radians then it is truncated to one full revolution.
bezierFromSweep :: Angle -> [Segment Closed R2]
bezierFromSweep s
  | s > fullTurn = bezierFromSweep fullTurn
  | s < 0          = fmap reflectY . bezierFromSweep $ (-s)
  | s < 0.0001     = []
  | s < fullTurn/4      = [bezierFromSweepQ1 s]
  | otherwise      = bezierFromSweepQ1 (fullTurn/4)
          : map (rotateBy (1/4)) (bezierFromSweep (max (s - fullTurn/4) 0))

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

-- | Given a start angle @s@ and an end angle @e@, @'arcT' s e@ is the
--   'Trail' of a radius one arc counterclockwise between the two angles.
arcT :: Angle -> Angle -> Trail R2
arcT start end
    | end' < start' = arcT start (end + fromIntegral d)
    | otherwise     = (if sweep >= fullTurn then glueTrail else id)
                    $ trailFromSegments bs
  where sweep = end - start
        bs    = map (rotate start) . bezierFromSweep $ sweep

        -- We want to compare the start and the end and in case
        -- there isn't some law about 'Angle' ordering, we use a
        -- known 'Angle' for that.
        start' = start^.turn
        end'   = end^.turn
        d      = ceiling (start' - end') :: Integer

-- | Given a start angle @s@ and an end angle @e@, @'arc' s e@ is the
--   path of a radius one arc counterclockwise between the two angles.
--   The origin of the arc is its center.
arc :: (TrailLike t, V t ~ R2) => Angle -> Angle -> t
arc start end = trailLike $ arcT start end `at` (rotate start $ p2 (1,0))

-- | Like 'arc' but clockwise.
arcCW :: (TrailLike t, V t ~ R2) => Angle -> Angle -> t
arcCW start end = trailLike $
                            -- flipped arguments to get the path we want
                            -- then reverse the trail to get the cw direction.
                            (reverseTrail $ arcT end start)
                            `at`
                            (rotate start $ p2 (1,0))
                   -- We could just have `arcCW = reversePath . flip arc`
                   -- but that wouldn't be `TrailLike`.

-- | Given a radus @r@, a start angle @s@ and an end angle @e@,
--   @'arc'' r s e@ is the path of a radius @(abs r)@ arc between
--   the two angles.  If a negative radius is given, the arc will
--   be clockwise, otherwise it will be counterclockwise. The origin
--   of the arc is its center.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_arc'Ex.svg#diagram=arc'Ex&width=300>>
--
--   > arc'Ex = mconcat [ arc' r 0 (1/4 \@\@ turn) | r <- [0.5,-1,1.5] ]
--   >        # centerXY # pad 1.1
arc' :: (TrailLike p, V p ~ R2) => Double -> Angle -> Angle -> p
arc' r start end = trailLike $ scale (abs r) ts `at` (rotate start $ p2 (abs r,0))
  where ts | r < 0     = reverseTrail $ arcT end start
           | otherwise = arcT start end

-- | Create a circular wedge of the given radius, beginning at the
--   first angle and extending counterclockwise to the second.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_wedgeEx.svg#diagram=wedgeEx&width=400>>
--
--   > wedgeEx = hcat' (with & sep .~ 0.5)
--   >   [ wedge 1 (0 \@\@ turn) (1/4)
--   >   , wedge 1 (7/30 \@\@ turn) (11/30)
--   >   , wedge 1 (1/8 \@\@ turn) (7/8)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
wedge :: (TrailLike p, V p ~ R2) => Double -> Angle -> Angle -> p
wedge r a1 a2 = trailLike . (`at` origin) . glueTrail . wrapLine
              $ fromOffsets [r *^ e a1]
                <> arc a1 a2 # scale r
                <> fromOffsets [r *^ negateV (e a2)]

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
arcBetween :: (TrailLike t, V t ~ R2) => P2 -> P2 -> Double -> t
arcBetween p q ht = trailLike (a # rotate (direction v) # moveTo p)
  where
    h = abs ht
    isStraight = h < 0.00001
    v = q .-. p
    d = magnitude (q .-. p)
    th  = acos ((d*d - 4*h*h)/(d*d + 4*h*h))
    r = d/(2*sin th)
    mid | ht >= 0    = fullTurn/4
        | otherwise = 3*fullTurn/4
    st  = mid - (th @@ rad)
    end = mid + (th @@ rad)
    a | isStraight
      = fromOffsets [d *^ unitX]
      | otherwise
      = arc st end
        # scale r
        # translateY ((if ht > 0 then negate else id) (r-h))
        # translateX (d/2)
        # (if ht > 0 then reverseLocTrail else id)

-- | Create an annular wedge of the given radii, beginning at the
--   first angle and extending counterclockwise to the second.
--   The radius of the outer circle is given first.
--
--   <<diagrams/src_Diagrams_TwoD_Arc_annularWedgeEx.svg#diagram=annularWedgeEx&width=400>>
--
--   > annularWedgeEx = hcat' (with & sep .~ 0.50)
--   >   [ annularWedge 1 0.5 (0 \@\@ turn) (1/4)
--   >   , annularWedge 1 0.3 (7/30 \@\@ turn) (11/30)
--   >   , annularWedge 1 0.7 (1/8 \@\@ turn) (7/8)
--   >   ]
--   >   # fc blue
--   >   # centerXY # pad 1.1
annularWedge :: (TrailLike p, V p ~ R2) => Double -> Double -> Angle -> Angle -> p
annularWedge r1' r2' a1 a2 = trailLike . (`at` o) . glueTrail . wrapLine
              $ fromOffsets [(r1'-r2') *^ e a1]
                <> arc a1 a2 # scale r1'
                <> fromOffsets [(r1'-r2') *^ negateV (e a2)]
                <> arcCW a2 a1 # scale r2'
  where o = origin # translate (r2' *^ e a1)
