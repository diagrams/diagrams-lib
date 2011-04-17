{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagram combinators in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Combinators
    (
      (===), (|||)

    , hcat, hcat'
    , vcat, vcat'

    , strutX, strutY
    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Combinators

import Data.VectorSpace

import Data.Monoid
import Data.Default

-- | Place two diagrams (or other boundable objects) vertically
--   adjacent to one another.
(===) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
a1 === a2 = beside unitY a2 a1

-- | Place two diagrams (or other boundable objects) horizontally
--   adjacent to one another.
(|||) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
a1 ||| a2 = beside unitX a1 a2

-- | XXX comment me
hcat :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => [a] -> a
hcat = hcat' def

hcat' :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
hcat' = cat' unitX

-- | XXX comment me
vcat :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => [a] -> a
vcat = vcat' def

vcat' :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
vcat' = cat' (negateV unitY)

-- | @strutX d@ is an empty diagram with width @d@ and height 0.
strutX :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutX d = strut (d,0)

-- | @strutY d@ is an empty diagram with height @d@ and width 0.
strutY :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutY d = strut (0,d)