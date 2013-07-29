{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Shapes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various two-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Shapes
       (
         -- * Miscellaneous
         hrule, vrule

         -- * Regular polygons

       , regPoly
       , triangle
       , eqTriangle
       , square
       , pentagon
       , hexagon
       , septagon
       , octagon
       , nonagon
       , decagon
       , hendecagon
       , dodecagon

         -- * Other special polygons
       , unitSquare
       , rect

         -- * Other shapes

       , roundedRect
       , RoundedRectOpts(..)
       , roundedRect'
       ) where

import           Diagrams.Core

import           Diagrams.Coordinates
import           Diagrams.Located        (at)
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types

import           Diagrams.Util

import           Data.Default.Class
import           Data.Semigroup

-- | Create a centered horizontal (L-R) line of the given length.
hrule :: (TrailLike t, V t ~ R2) => Double -> t
hrule d = trailLike $ trailFromSegments [straight (d & 0)] `at` (p2 (-d/2,0))

-- | Create a centered vertical (T-B) line of the given length.
vrule :: (TrailLike t, V t ~ R2) => Double -> t
vrule d = trailLike $ trailFromSegments [straight (0 & (-d))] `at` (p2 (0,d/2))

-- | A square with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
unitSquare :: (TrailLike t, V t ~ R2) => t
unitSquare = polygon with { polyType   = PolyRegular 4 (sqrt 2 / 2)
                          , polyOrient = OrientH }

-- | A square with its center at the origin and sides of the given
--   length, oriented parallel to the axes.
square :: (TrailLike t, Transformable t, V t ~ R2) => Double -> t
square d = rect d d

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
rect :: (TrailLike t, Transformable t, V t ~ R2) => Double -> Double -> t
rect w h = trailLike . head . pathTrails $ unitSquare # scaleX w # scaleY h

    -- The above may seem a bit roundabout.  In fact, we used to have
    --
    --   rect w h = unitSquare # scaleX w # scaleY h
    --
    -- since unitSquare can produce any TrailLike.  The current code
    -- instead uses (unitSquare # scaleX w # scaleY h) to specifically
    -- produce a Path, which is then deconstructed and passed back into
    -- 'trailLike' to create any TrailLike.
    --
    -- The difference is that while scaling by zero works fine for
    -- Path it does not work very well for, say, Diagrams (leading to
    -- NaNs or worse).  This way, we force the scaling to happen on a
    -- Path, where we know it will behave properly, and then use the
    -- resulting geometry to construct an arbitrary TrailLike.
    --
    -- See https://github.com/diagrams/diagrams-lib/issues/43 .

------------------------------------------------------------
--  Regular polygons
------------------------------------------------------------

-- | Create a regular polygon. The first argument is the number of
--   sides, and the second is the /length/ of the sides. (Compare to the
--   'polygon' function with a 'PolyRegular' option, which produces
--   polygons of a given /radius/).
--
--   The polygon will be oriented with one edge parallel to the x-axis.
regPoly :: (TrailLike t, V t ~ R2) => Int -> Double -> t
regPoly n l = polygon with { polyType =
                               PolySides
                                 (repeat (1/ fromIntegral n :: Turn))
                                 (replicate (n-1) l)
                           , polyOrient = OrientH
                           }

-- | A synonym for 'triangle', provided for backwards compatibility.
eqTriangle :: (TrailLike t, V t ~ R2) => Double -> t
eqTriangle = triangle

-- | An equilateral triangle, with sides of the given length and base
--   parallel to the x-axis.
triangle :: (TrailLike t, V t ~ R2) => Double -> t
triangle = regPoly 3

-- | A regular pentagon, with sides of the given length and base
--   parallel to the x-axis.
pentagon :: (TrailLike t, V t ~ R2) => Double -> t
pentagon = regPoly 5

-- | A regular hexagon, with sides of the given length and base
--   parallel to the x-axis.
hexagon :: (TrailLike t, V t ~ R2) => Double -> t
hexagon = regPoly 6

-- | A regular septagon, with sides of the given length and base
--   parallel to the x-axis.
septagon :: (TrailLike t, V t ~ R2) => Double -> t
septagon = regPoly 7

-- | A regular octagon, with sides of the given length and base
--   parallel to the x-axis.
octagon :: (TrailLike t, V t ~ R2) => Double -> t
octagon = regPoly 8

-- | A regular nonagon, with sides of the given length and base
--   parallel to the x-axis.
nonagon :: (TrailLike t, V t ~ R2) => Double -> t
nonagon = regPoly 9

-- | A regular decagon, with sides of the given length and base
--   parallel to the x-axis.
decagon :: (TrailLike t, V t ~ R2) => Double -> t
decagon = regPoly 10

-- | A regular hendecagon, with sides of the given length and base
--   parallel to the x-axis.
hendecagon :: (TrailLike t, V t ~ R2) => Double -> t
hendecagon = regPoly 11

-- | A regular dodecagon, with sides of the given length and base
--   parallel to the x-axis.
dodecagon :: (TrailLike t, V t ~ R2) => Double -> t
dodecagon = regPoly 12

------------------------------------------------------------
--  Other shapes  ------------------------------------------
------------------------------------------------------------

-- | @roundedRect w h r@ generates a closed trail, or closed path
--   centered at the origin, of an axis-aligned rectangle with width
--   @w@, height @h@, and circular rounded corners of radius @r@.  If
--   @r@ is negative the corner will be cut out in a reverse arc. If
--   the size of @r@ is larger than half the smaller dimension of @w@
--   and @h@, then it will be reduced to fit in that range, to prevent
--   the corners from overlapping.  The trail or path begins with the
--   right edge and proceeds counterclockwise.  If you need to specify
--   a different radius for each corner individually, use
--   @roundedRect'@ instead.
roundedRect :: (TrailLike t, V t ~ R2) => Double -> Double -> Double -> t
roundedRect w h r = roundedRect' w h (with { radiusTL = r,
                                             radiusBR = r,
                                             radiusTR = r,
                                             radiusBL = r})

-- | @roundedRect'@ works like @roundedRect@ but allows you to set the radius of
--   each corner indivually, using @RoundedRectOpts@. The default corner radius is 0.
--   Each radius can also be negative, which results in the curves being reversed
--   to be inward instead of outward.
roundedRect' :: (TrailLike t, V t ~ R2) => Double -> Double -> RoundedRectOpts -> t
roundedRect' w h opts
   = trailLike
   . (`at` (p2 (w/2, abs rBR - h/2)))
   . wrapTrail
   . glueLine
   $ seg (0, h - abs rTR - abs rBR)
   <> mkCorner 0 rTR
   <> seg (abs rTR + abs rTL - w, 0)
   <> mkCorner 1 rTL
   <> seg (0, abs rTL + abs rBL - h)
   <> mkCorner 2 rBL
   <> seg (w - abs rBL - abs rBR, 0)
   <> mkCorner 3 rBR
  where seg   = lineFromOffsets . (:[]) . r2
        diag  = sqrt (w * w + h * h)
        -- to clamp corner radius, need to compare with other corners that share an
        -- edge. If the corners overlap then reduce the largest corner first, as far
        -- as 50% of the edge in question.
        rTL                 = clampCnr radiusTR radiusBL radiusBR radiusTL
        rBL                 = clampCnr radiusBR radiusTL radiusTR radiusBL
        rTR                 = clampCnr radiusTL radiusBR radiusBL radiusTR
        rBR                 = clampCnr radiusBL radiusTR radiusTL radiusBR
        clampCnr rx ry ro r = let (rx',ry',ro',r') = (rx opts, ry opts, ro opts, r opts)
                                in clampDiag ro' . clampAdj h ry' . clampAdj w rx' $ r'
        -- prevent curves of adjacent corners from overlapping
        clampAdj len adj r  = if abs r > len/2
                                then sign r * max (len/2) (min (len - abs adj) (abs r))
                                else r
        -- prevent inward curves of diagonally opposite corners from intersecting
        clampDiag opp r     = if r < 0 && opp < 0 && abs r > diag / 2
                                then sign r * max (diag / 2) (min (abs r) (diag + opp))
                                else r
        sign n = if n < 0 then -1 else 1
        mkCorner k r | r == 0    = mempty
                     | r < 0     = doArc 3 2
                     | otherwise = doArc 0 1
                     where doArc d d' = arc' r ((k+d)/4) ((k+d')/4:: Turn)

data RoundedRectOpts = RoundedRectOpts { radiusTL :: Double
                                       , radiusTR :: Double
                                       , radiusBL :: Double
                                       , radiusBR :: Double
                                       }
instance Default RoundedRectOpts where
  def = RoundedRectOpts 0 0 0 0
