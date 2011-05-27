{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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

         -- * General polygons
       , polygon, polygonPath, polygonVertices
       , PolygonOpts(..), PolygonOrientation(..)

         -- * Special polygons
       , square
       , rect
       , starPolygon

       , eqTriangle
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Path
import Diagrams.TwoD.Path
import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Align

import Diagrams.Util

import Data.Default

-- | Create a centered horizontal line of the given length.
hrule :: (Backend b R2, Renderable (Path R2) b) => Double -> Diagram b R2
hrule d = centerX . stroke $ fromOffsets [(d,0)]

-- | Create a centered vertical line of the given length.
vrule :: (Backend b R2, Renderable (Path R2) b) => Double -> Diagram b R2
vrule d = centerY . stroke $ fromOffsets [(0,d)]

-- | Determine how a polygon should be oriented.
data PolygonOrientation = NoOrient  -- ^ No special orientation; one
                                    --   vertex will be at (1,0).
                                    --   This is the default.
                        | OrientToX -- ^ Orient so the botommost edge
                                    --   is parallel to the x-axis.
                        | OrientToY -- ^ Orient so the leftmost edge
                                    --   is parallel to the y-axis.
  deriving (Eq, Ord, Show, Read)

data PolygonOpts = PolygonOpts {
    sides       :: Int    -- ^ Number of sides; the default is 5.
  , edgeSkip    :: Int    -- ^ Create star polygons by setting the
                          --   edge skip to some number other than 1
                          --   (the default).  With an edge skip of n,
                          --   edges will connect every nth vertex.
  , orientation :: PolygonOrientation
                          -- ^ Determine how the polygon should be
                          --   oriented.
  }
  deriving (Eq, Ord, Show, Read)

instance Default PolygonOpts where
  def = PolygonOpts { sides = 5, edgeSkip = 1, orientation = NoOrient }

-- | Create a regular polygon from the given options.
polygon :: (Backend b R2, Renderable (Path R2) b) => PolygonOpts -> Diagram b R2
polygon = stroke . polygonPath

-- | Create a closed regular polygonal path from the given options.
polygonPath :: (PathLike p, V p ~ R2) => PolygonOpts -> p
polygonPath = close . fromVertices . polygonVertices

-- | Generate the vertices of a regular polygon from the given
--   options.
polygonVertices :: PolygonOpts -> [P2]
polygonVertices opts = orient . take n . iterate (rotateBy turn) $ start
  where start  = translateX 1 origin
        turn   = fromIntegral (edgeSkip opts) / fromIntegral n
        n      = sides opts
        orient  | orientation opts == OrientToX = orientX
                | orientation opts == OrientToY = orientY
                | otherwise                     = id
        orientX | odd n          = rotateBy (1/4)
                | n `mod` 4 == 0 = rotateBy (turn/2)
                | otherwise      = id
        orientY | even n         = rotateBy (turn/2)
                | otherwise      = id

-- | A sqaure with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
square ::  (Backend b R2, Renderable (Path R2) b) => Diagram b R2
square = scale (1/sqrt 2) $ polygon def { sides = 4, orientation = OrientToX }

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
rect :: (Backend b R2, Renderable (Path R2) b) => Double -> Double -> Diagram b R2
rect w h = square # scaleX w # scaleY h

-- | @starPolygon p q@ creates a star polygon, where @p@ indicates the
--   number of vertices, and an edge connects every @q@th vertex.
starPolygon :: (Backend b R2, Renderable (Path R2) b) => Int -> Int -> Diagram b R2
starPolygon p q = polygon def { sides = p, edgeSkip = q }

-- | An equilateral triangle, with radius 1 and base parallel to the
--   x-axis.
eqTriangle :: (Backend b R2, Renderable (Path R2) b) => Diagram b R2
eqTriangle = polygon with {sides = 3, orientation = OrientToX}

{-
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
