{-# LANGUAGE PackageImports, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Shapes
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Various two-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Shapes
       ( Box(..)
       , box
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Basics (prim)
import Graphics.Rendering.Diagrams.Expressions

import Diagrams.TwoD.Types

import qualified Data.Map as M

import Data.Monoid (Any(..))

data Box = Box P2 P2 P2 P2
           deriving (Show)

instance Transformable Box where
  type TSpace Box = P2
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

box :: (BSpace b ~ P2, Renderable Box b) => Diagram b
box = Diagram (prim $ Box (-1,-1) (1,-1) (1,1) (-1,1))
              (Bounds boxBounds)
              (fromNames [ ("LL", (-1,-1))
                         , ("LR", ( 1,-1))
                         , ("UR", ( 1, 1))
                         , ("UL", (-1, 1)) ])
              (\(x,y) -> Any (inBox x && inBox y))
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]
        inBox x = x >= (-1) && x <= 1

