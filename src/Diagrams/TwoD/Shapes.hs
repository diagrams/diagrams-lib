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
       ( PolygonOrientation(..), PolygonOpts(..)
       , polygon, polygonPath, polygonVertices
       , square
       , starPolygon
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Path
import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform

import Diagrams.Util

import qualified Data.Map as M

import Data.Monoid (Any(..))
import Data.Default

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
polygon :: (BSpace b ~ R2, Renderable (Path R2) b) => PolygonOpts -> Diagram b
polygon = stroke . polygonPath

-- | Create a closed regular polygonal path from the given options.
polygonPath :: PolygonOpts -> Path R2
polygonPath = close . pathFromVertices . polygonVertices

-- | Generate the vertices of a regular polygon from the given
--   options.
polygonVertices :: PolygonOpts -> [P2]
polygonVertices opts = orient . take n . iterate (rotate angle) $ start
  where start  = translateX 1 origin
        angle  = (fromIntegral $ edgeSkip opts) * 2*pi / fromIntegral n
        n      = sides opts
        orient  | orientation opts == OrientToX = orientX
                | orientation opts == OrientToY = orientY
                | otherwise                     = id
        orientX | odd n          = rotateBy (1/4)
                | n `mod` 4 == 0 = rotate (angle/2)
                | otherwise      = id
        orientY | even n    = rotate (angle/2)
                | otherwise = id

-- | A sqaure with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
square ::  (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
square = scale (1/sqrt 2) $ polygon def { sides = 4, orientation = OrientToX }

-- | @starPolygon p q@ creates a star polygon, where @p@ indicates the
--   number of vertices, and an edge connects every @q@th vertex.
starPolygon :: (BSpace b ~ R2, Renderable (Path R2) b) => Int -> Int -> Diagram b
starPolygon p q = polygon def { sides = p, edgeSkip = q }

-- | An equilateral triangle, with radius 1 and base parallel to the
--   x-axis.
eqTriangle :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
eqTriangle = polygon with {sides = 3, orientation = OrientToX}

pentagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
pentagon = writeMe "pentagon"

hexagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
hexagon = writeMe "hexagon"

septagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
septagon = writeMe "septagon"

octagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
octagon = writeMe "octagon"

nonagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
nonagon = writeMe "nonagon"

decagon :: (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
decagon = writeMe "decagon"

-- | Construct a triangle from three side lengths, if possible.  The
--   longest side will be parallel to the x-axis.
triangleFromSides :: (BSpace b ~ R2, Renderable (Path R2) b)
                  => Double -> Double -> Double -> Maybe (Diagram b)
triangleFromSides = writeMe "triangleFromSides"

