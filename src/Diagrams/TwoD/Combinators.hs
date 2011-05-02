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
-- Diagram combinators specialized to two dimensions. For more general
-- combinators, see "Diagrams.Combinators".
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Combinators
    (
      -- * Binary combinators

      (===), (|||)

      -- * n-ary combinators
    , hcat, hcat'
    , vcat, vcat'

      -- * Struts
    , strutX, strutY
    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Combinators

import Data.VectorSpace

import Data.Monoid
import Data.Default

infixl 6 ===
infixl 6 |||

-- | Place two diagrams (or other boundable objects) vertically
--   adjacent to one another, with the first diagram above the second.
--   Since Haskell ignores whitespace in expressions, one can thus write
--
--   >    c
--   >   ===
--   >    d
--
--   to place @c@ above @d@.
(===) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
(===) = beside (negateV unitY)

-- | Place two diagrams (or other boundable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.
(|||) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
(|||) = beside unitX

-- | Lay out a list of boundable objects in a row from left to right,
--   so that their local origins lie along a single horizontal line,
--   with successive bounding regions tangent to one another.
--
--   * For more control over the spacing, see 'hcat''.
--
--   * To align the diagrams vertically (or otherwise), use alignment
--   combinators (such as 'alignTop' or 'alignBottom') from
--   "Diagrams.TwoD.Align" before applying 'hcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
hcat :: (HasOrigin a, Boundable a, Qualifiable a, V a ~ R2, Monoid a) => [a] -> a
hcat = hcat' def

-- | A variant of 'hcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities.
hcat' :: (HasOrigin a, Boundable a, Qualifiable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
hcat' = cat' unitX

-- | Lay out a list of boundable objects in a column from top to bottom,
--   so that their local origins lie along a single vertical line,
--   with successive bounding regions tangent to one another.
--
--   * For more control over the spacing, see 'vcat''.
--
--   * To align the diagrams horizontally (or otherwise), use alignment
--   combinators (such as 'alignLeft' or 'alignRight') from
--   "Diagrams.TwoD.Align" before applying 'vcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
vcat :: (HasOrigin a, Boundable a, Qualifiable a, V a ~ R2, Monoid a) => [a] -> a
vcat = vcat' def

-- | A variant of 'vcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of the
--   possibilities.
vcat' :: (HasOrigin a, Boundable a, Qualifiable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
vcat' = cat' (negateV unitY)

-- | @strutX d@ is an empty diagram with width @d@ and height 0.
strutX :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutX d = strut (d,0)

-- | @strutY d@ is an empty diagram with height @d@ and width 0.
strutY :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutY d = strut (0,d)