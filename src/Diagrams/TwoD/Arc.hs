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
    ( -- * Creating arcs

      arc
    , arcCW
    , arc'

    , arcBetween
    , ArcParam(..)

      -- * Creating arc-ish things
    , wedge

      -- * Internals
    , arcT
    , bezierFromSweep
    ) where

import           Diagrams.Core

import           Diagrams.Coordinates
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (direction, e, unitX)
import           Diagrams.Util           (tau, ( # ))

import           Data.AffineSpace        ((.+^), (.-.))
import           Data.Semigroup          ((<>))
import           Data.VectorSpace        (magnitude, negateV, normalized, (*^),
                                          (^-^))

-- For details of this approximation see:
--   http://www.tinaja.com/glib/bezcirc2.pdf

-- | @bezierFromSweepQ1 s@ constructs a 'Cubic' segment that starts in
--   the positive y direction and sweeps counterclockwise through @s@
--   radians.  The approximation is only valid for angles in the first
--   quadrant.
bezierFromSweepQ1 :: Rad -> Segment R2
bezierFromSweepQ1 s = fmap (^-^ v) . rotate (s/2) $ Cubic c2 c1 p0
  where p0@(coords -> x :& y) = rotate (s/2) v
        c1                    = ((4-x)/3)  &  ((1-x)*(3-x)/(3*y))
        c2                    = reflectY c1
        v                     = unitX

-- | @bezierFromSweep s@ constructs a series of 'Cubic' segments that
--   start in the positive y direction and sweep counter clockwise
--   through @s@ radians.  If @s@ is negative, it will start in the
--   negative y direction and sweep clockwise.  When @s@ is less than
--   0.0001 the empty list results.  If the sweep is greater than tau
--   then it is truncated to tau.
bezierFromSweep :: Rad -> [Segment R2]
bezierFromSweep s
  | s > tau    = bezierFromSweep tau
  | s < 0      = fmap reflectY . bezierFromSweep $ (-s)
  | s < 0.0001 = []
  | s < tau/4  = [bezierFromSweepQ1 s]
  | otherwise  = bezierFromSweepQ1 (tau/4)
          : map (rotateBy (1/4)) (bezierFromSweep (max (s - tau/4) 0))

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
arcT :: Angle a => a -> a -> Trail R2
arcT start end
    | e < s     = arcT s (e + fromIntegral d)
    | otherwise = Trail bs (sweep >= tau)
  where sweep = convertAngle $ end - start
        bs    = map (rotate start) . bezierFromSweep $ sweep

        -- We want to compare the start and the end and in case
        -- there isn't some law about 'Angle' ordering, we use a
        -- known 'Angle' for that.
        s = convertAngle start :: CircleFrac
        e = convertAngle end
        d = ceiling (s - e) :: Integer

-- | Given a start angle @s@ and an end angle @e@, @'arc' s e@ is the
--   path of a radius one arc counterclockwise between the two angles.
--   The origin of the arc is its center.
arc :: (Angle a, PathLike p, V p ~ R2) => a -> a -> p
arc start end = pathLike (rotate start $ p2 (1,0))
                         False
                         (trailSegments $ arcT start end)

-- | Like 'arc' but clockwise.
arcCW :: (Angle a, PathLike p, V p ~ R2) => a -> a -> p
arcCW start end = pathLike (rotate start $ p2 (1,0))
                           False
                           -- flipped arguments to get the path we want
                           -- then reverse the trail to get the cw direction.
                           (trailSegments . reverseTrail $ arcT end start)
                   -- We could just have `arcCW = reversePath . flip arc`
                   -- but that wouldn't be `PathLike`.

-- | Given a radus @r@, a start angle @s@ and an end angle @e@,
--   @'arc'' r s e@ is the path of a radius @(abs r)@ arc between
--   the two angles.  If a negative radius is given, the arc will
--   be clockwise, otherwise it will be counterclockwise. The origin
--   of the arc is its center.
arc' :: (Angle a, PathLike p, V p ~ R2) => Double -> a -> a -> p
arc' r start end = pathLike (rotate start $ p2 (abs r,0))
                   False
                   (trailSegments . scale (abs r) $ ts)
  where ts | r < 0     = reverseTrail $ arcT end start
           | otherwise = arcT start end

-- | Create a circular wedge of the given radius, beginning at the
--   first angle and extending counterclockwise to the second.
wedge :: (Angle a, PathLike p, V p ~ R2) => Double -> a -> a -> p
wedge r a1 a2 = pathLikeFromTrail $ fromOffsets [r *^ e a1]
                                 <> arc a1 a2 # scale r
                                 <> fromOffsets [r *^ negateV (e a2)]

-- | Parameters controlling an arc drawn between two points.
data ArcParam = ArcRadius
                  Double
                  Bool      -- ^ The radius of the arc, and a Boolean
                            --   value where @True@ means the /longer/
                            --   of the two possible arcs should be
                            --   used, /i.e./ the one subtending an
                            --   obtuse angle.
                            --
                            -- > arcRadFalse = arcBetween (ArcRadius 2 False) origin (1&0)
                            --
                            -- <<dia#diagram=arcRadFalse&width=200>>
                            --
                            -- > arcRadTrue = arcBetween (ArcRadius 2 True) origin (1&0)
                            --
                            -- <<dia#diagram=arcRadTrue&width=200>>
              | ArcOffset
                  Double    -- ^ The distance of the arc midpoint from
                            --   the straight line between the points.
                            --
                            -- > arcOffs = mconcat $ [arcBetween (ArcOffset h) origin (1&0) | h <- [1, 0.8 .. (-1)] ]
                            --
                            -- <<dia#diagram=arcOffs&width=200>>

-- | Generate an arc from the first point to the second, according to
--   the given parameters.  Note that the result is wrapped in
--   @Maybe@; it will fail to produce an arc precisely when the radius
--   specified with @ArcRadius@ is smaller than half the distance
--   between the points.
arcBetween :: (Transformable p, HasOrigin p, PathLike p, V p ~ R2)
           => ArcParam -> P2 -> P2 -> Maybe p
arcBetween (ArcOffset offs) p q
  | abs offs < 0.00001 = Just (p ~~ q)    -- XXX how to deal with this properly?
arcBetween (ArcOffset offs) p q
  = arcBetween (ArcRadius r (h > abs r)) p q
  where
    d    = magnitude (p .-. q)
    h    = abs offs
    phi2 = acos ((d*d - 4*h*h) / (d*d + 4*h*h))
    r    = signum offs * d / (2 * sin phi2)
arcBetween (ArcRadius r obtuse) p q
  | d > 2*r'  = Nothing
  | otherwise = Just $ arc' (if obtuse then -r else r) 0 phi
                     # rotate psi
                     # moveTo c
                     # if obtuse then reflectAbout p (q .-. p) else id
  where
    r'    = abs r
    d     = magnitude (p .-. q)
    theta = Rad $ acos (d / (2*r')) * signum r
    phi   = 2 * (tau/4 - abs theta) * (Rad $ signum r)
    psi   = direction (p .-. c) :: Rad
    c     = p .+^ (normalized (q .-. p) # scale r' # rotate theta)
