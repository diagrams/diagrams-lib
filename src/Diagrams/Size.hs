{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Size
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for computing sizes of diagrams and other boundable objects.
--
-----------------------------------------------------------------------------
module Diagrams.Size
       (
         breadth
       ) where

import Graphics.Rendering.Diagrams

import Data.VectorSpace (negateV, Scalar, normalized)

-- | Compute the distance from one side of an object to the other
--   along the given direction.
breadth :: Boundable a => V a -> a -> Scalar (V a)
breadth v a = f v' + f (negateV v')
  where f  = appBounds $ getBounds a
        v' = normalized v
