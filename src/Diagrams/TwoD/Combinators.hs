{-# LANGUAGE FlexibleContexts
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

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.TwoD.Align
import Diagrams.Combinators

import Data.VectorSpace

import Data.Monoid
import Data.Default

-- | Place two diagrams (or other boundable objects) vertically
--   adjacent to one another.
(===) :: (HasOrigin a R2, Boundable a R2, Monoid a) => a -> a -> a
a1 === a2 = beside unitY a2 a1

-- | Place two diagrams (or other boundable objects) horizontally
--   adjacent to one another.
(|||) :: (HasOrigin a R2, Boundable a R2, Monoid a) => a -> a -> a
a1 ||| a2 = beside unitX a1 a2

-- | XXX comment me
hcat :: (HasOrigin a R2, Boundable a R2, Monoid a) => [a] -> a
hcat = hcat' def

hcat' :: (HasOrigin a R2, Boundable a R2, Monoid a) => CatOpts R2 -> [a] -> a
hcat' = cat' unitX

-- | XXX comment me
vcat :: (HasOrigin a R2, Boundable a R2, Monoid a) => [a] -> a
vcat = vcat' def

vcat' :: (HasOrigin a R2, Boundable a R2, Monoid a) => CatOpts R2 -> [a] -> a
vcat' = cat' (negateV unitY)