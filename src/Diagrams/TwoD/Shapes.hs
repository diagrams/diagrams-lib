{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}

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

         -- * Special polygons
       , unitSquare
       , square
       , rect

         -- * Other shapes

       , roundedRect
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Path
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Polygons

import Diagrams.Util

import Data.Monoid
import Data.VectorSpace

-- | Create a centered horizontal (L-R) line of the given length.
hrule :: (PathLike p, V p ~ R2) => Double -> p
hrule d = pathLike (P (-d/2,0)) False [Linear (d,0)]

-- | Create a centered vertical (T-B) line of the given length.
vrule :: (PathLike p, V p ~ R2) => Double -> p
vrule d = pathLike (P (0,d/2)) False [Linear (0,-d)]

-- | A sqaure with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
unitSquare :: (Transformable p, PathLike p, V p ~ R2) => p
unitSquare = polygon with { polyType   = PolyRegular 4 (sqrt 2 / 2)
                          , polyOrient = OrientH }

-- | A sqaure with its center at the origin and sides of the given
--   length, oriented parallel to the axes.
square :: (PathLike p, Transformable p, V p ~ R2) => Double -> p
square d = unitSquare # scale d

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
rect :: (PathLike p, Transformable p, V p ~ R2) => Double -> Double -> p
rect w h = unitSquare # scaleX w # scaleY h

{-
-- | An equilateral triangle, with radius 1 and base parallel to the
--   x-axis.
eqTriangle :: (PathLike p, Transformable p, V p ~ R2) => p
eqTriangle = polygon with {sides = 3, orientation = OrientToX}

pentagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
pentagon = writeMe "pentagon"

hexagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
hexagon = writeMe "hexagon"

septagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
septagon = writeMe "septagon"

octagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
octagon = writeMe "octagon"

nonagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
nonagon = writeMe "nonagon"

decagon :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
decagon = writeMe "decagon"

-- | Construct a triangle from three side lengths, if possible.  The
--   longest side will be parallel to the x-axis.
triangleFromSides :: (Backend b R2, Renderable (Path R2) b)
                  => Double -> Double -> Double -> Maybe (Diagram b R2)
triangleFromSides = writeMe "triangleFromSides"
-}

------------------------------------------------------------
--  Other shapes  ------------------------------------------
------------------------------------------------------------

-- | @roundedRect v r@ generates a closed trail, or closed path
-- centered at the origin, of an axis-aligned rectangle with diagonal
-- @v@ and circular rounded corners of radius @r@.  @r@ must be
-- between @0@ and half the smaller dimension of @v@, inclusive; smaller or
-- larger values of @r@ will be treated as @0@ or half the smaller
-- dimension of @v@, respectively.  The trail or path begins with the
-- right edge and proceeds counterclockwise.
roundedRect :: (PathLike p, V p ~ R2) => R2 -> Double -> p
roundedRect v r = pathLike (P (xOff/2 + r', -yOff/2)) True
                . trailSegments
                $ seg (0,yOff)
                <> mkCorner 0
                <> seg (-xOff,0)
                <> mkCorner 1
                <> seg (0, -yOff)
                <> mkCorner 2
                <> seg (xOff,0)
                <> mkCorner 3
  where seg = fromOffsets  . (:[])
        r'   = clamp r 0 maxR
        maxR = uncurry min v / 2
        (xOff,yOff) = v ^-^ (2*r', 2*r')
        mkCorner k | r' == 0   = mempty
                   | otherwise = arc (k/4) ((k+1)/4::CircleFrac) # scale r'

-- | @clamp x lo hi@ clamps @x@ to lie between @lo@ and @hi@
--   inclusive.  That is, if @lo <= x <= hi@ it returns @x@; if @x < lo@
--   it returns @lo@, and if @hi < x@ it returns @hi@.
clamp :: Ord a => a -> a -> a -> a
clamp x lo hi | x < lo    = lo
              | x > hi    = hi
              | otherwise = x
