{-# LANGUAGE TypeFamilies #-}
module Diagrams.TwoD.Util
       ( width, height, size2D
       , extentX, extentY, center2D
       ) where

import Graphics.Rendering.Diagrams
import Diagrams.TwoD.Types

import Control.Arrow ((***), (&&&))

-- | Compute the width of a diagram in R2.
width :: (BSpace b ~ R2) => AnnDiagram b a -> Double
width = negate . uncurry (-) . extentX

-- | Compute the height of a diagram in R2.
height :: (BSpace b ~ R2) => AnnDiagram b a -> Double
height = negate . uncurry (-) . extentY

-- | Compute the width and height of a diagram in R2.
size2D :: (BSpace b ~ R2) => AnnDiagram b a -> (Double, Double)
size2D = width &&& height

-- | Compute the absolute x-coordinate range of a diagram in R2, in
--   the form (lo,hi).
extentX :: (BSpace b ~ R2) => AnnDiagram b a -> (Double, Double)
extentX d = (-f (-1,0), f (1,0))
  where f = getBoundFunc $ bounds d

-- | Compute the absolute y-coordinate range of a diagram in R2, in
--   the form (lo,hi).
extentY :: (BSpace b ~ R2) => AnnDiagram b a -> (Double, Double)
extentY d = (-f (0,-1), f (0,1))
  where f = getBoundFunc $ bounds d

-- | Compute the center of a diagram in R2.
center2D :: (BSpace b ~ R2) => AnnDiagram b a -> P2
center2D = P . (mid *** mid) . (extentX &&& extentY)
  where mid = (/2) . uncurry (+)