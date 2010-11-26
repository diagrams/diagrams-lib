{-# LANGUAGE PackageImports, TypeFamilies #-}
module Diagrams.TwoD.Util
       ( width, height, size2D ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Diagrams.TwoD.Types

-- | Compute the width of a diagram in R2.
width :: (BSpace b ~ R2) => AnnDiagram b a -> Double
width (Diagram {bounds = Bounds b})  = b (1,0) + b (-1,0)

-- | Compute the height of a diagram in R2.
height :: (BSpace b ~ R2) => AnnDiagram b a -> Double
height (Diagram {bounds = Bounds b}) = b (0,1) + b (0,-1)

-- | Compute the width and height of a diagram in R2.
size2D :: (BSpace b ~ R2) => AnnDiagram b a -> (Double, Double)
size2D d = (width d, height d)