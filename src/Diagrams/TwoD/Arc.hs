{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arc
-- Copyright   :  (c) Ryan Yates 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  fryguybob@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Two-dimensional arcs approximated by beziers.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arc
    ( arc, arcT
    , bezierFromSweep
    ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform

import Diagrams.Path
import Diagrams.Segment

import Data.VectorSpace((^-^))

-- For details of this approximation see:
--   http://www.tinaja.com/glib/bezcirc2.pdf

-- | Construct a 'Cubic' segment that starts in positive y direction
--   and sweeps counter clockwise 's' radians.  The approximation is
--   only valid for angles in the first quarter.
bezierFromSweepQ1 :: Angle -> Segment R2
bezierFromSweepQ1 s = fmap (^-^ v) . rotate (s/2) $ Cubic p2 p1 p0
  where p0@(x,y) = rotate (s/2) v
        p1       = ((4-x)/3, (1-x)*(3-x)/(3*y))
        p2       = reflectY p1
        v        = (1,0)

-- | Construct a series of 'Cubic' segments that start in positive
--   y direction and sweep counter clockwise 's' radians.  If 's' is
--   negative it will start in the negative y direction and sweep
--   clockwise.  When 's' is less than 0.0001 the result is '[]'.  If
--   the sweep is greater than two pi then it is truncated to two pi.
--   See Note [segment spacing]
bezierFromSweep :: Angle -> [Segment R2]
bezierFromSweep s
  | s > 2 * pi = bezierFromSweep (2*pi)
  | s < 0      = fmap reflectY . bezierFromSweep $ (-s)
  | s < pi/2   = [bezierFromSweepQ1 s]
  | s < 0.0001 = []
  | otherwise  = bezierFromSweepQ1 (pi/2)
          : map (rotate (pi/2)) (bezierFromSweep (max (s-pi/2) 0))

{-
~~~~ Note [segment spacing]

There are a few obvious options for segment spacing:
   A. Evenly space segments each with sweep less than or equal
      to half pi.  This has the benefit of a better approximation
      (at least I think it is better).
   B. Use as much of the sweep in half pi sized segments and one for
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

-- | A version of 'arc' that produces a 'Trail' instead of a 'Path'.
arcT :: Angle -> Angle -> Trail R2
arcT start end = Trail bs (sweep >= pi*2)
  where sweep = end - start
        bs    = map (rotate start) . bezierFromSweep $ sweep

-- | Given a 'start' angle and an 'end' angle, 'arc' is the path of
--   a radius one arc counter clockwise between the two angles.
arc :: Angle -> Angle -> Path R2
arc start end = pathFromTrailAt (arcT start end) (rotate start $ P (1,0))
