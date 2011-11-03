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

      -- * Spacing
    , strutX, strutY
    , padX, padY
    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Transform (scaleX, scaleY)
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector (unitX, unitY)

import Diagrams.Util ((#))
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
--   to place @c@ above @d@.  The local origin of the resulting
--   combined diagram is the same as the local origin of the first.
--   @(===)@ is associative and has 'mempty' as a right (but not left)
--   identity.  See the documentation of 'beside' for more information.
(===) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
(===) = beside (negateV unitY)

-- | Place two diagrams (or other boundable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.  The local origin of the resulting
--   combined diagram is the same as the local origin of the first.
--   @(===)@ is associative and has 'mempty' as a right (but not left)
--   identity.  See the documentation of 'beside' for more information.
(|||) :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => a -> a -> a
(|||) = beside unitX

-- | Lay out a list of boundable objects in a row from left to right,
--   so that their local origins lie along a single horizontal line,
--   with successive bounding regions tangent to one another.
--
--   * For more control over the spacing, see 'hcat''.
--
--   * To align the diagrams vertically (or otherwise), use alignment
--     combinators (such as 'alignT' or 'alignB') from
--     "Diagrams.TwoD.Align" before applying 'hcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
hcat :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => [a] -> a
hcat = hcat' def

-- | A variant of 'hcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities.
hcat' :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
hcat' = cat' unitX

-- | Lay out a list of boundable objects in a column from top to bottom,
--   so that their local origins lie along a single vertical line,
--   with successive bounding regions tangent to one another.
--
--   * For more control over the spacing, see 'vcat''.
--
--   * To align the diagrams horizontally (or otherwise), use alignment
--     combinators (such as 'alignL' or 'alignR') from
--     "Diagrams.TwoD.Align" before applying 'vcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
vcat :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => [a] -> a
vcat = vcat' def

-- | A variant of 'vcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of the
--   possibilities.
vcat' :: (HasOrigin a, Boundable a, V a ~ R2, Monoid a) => CatOpts R2 -> [a] -> a
vcat' = cat' (negateV unitY)

-- | @strutX d@ is an empty diagram with width @d@, height 0, and a
--   centered local origin.  Note that @strutX (-w)@ behaves the same as
--   @strutX w@.
strutX :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutX d = strut (d,0)

-- | @strutY d@ is an empty diagram with height @d@, width 0, and a
--   centered local origin. Note that @strutX (-w)@ behaves the same as
--   @strutX w@.
strutY :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutY d = strut (0,d)

-- | @padX s@ \"pads\" a diagram in the x-direction, expanding its
--   bounding region horizontally by a factor of @s@ (factors between
--   0 and 1 can be used to shrink the bounding region).  Note that
--   the bounding region will expand with respect to the local origin,
--   so if the origin is not centered horizontally the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using 'centerX') before applying @padX@.
padX :: ( Backend b R2, Monoid m )
     => Double -> AnnDiagram b R2 m -> AnnDiagram b R2 m
padX s d = withBounds (d # scaleX s) d

-- | @padY s@ \"pads\" a diagram in the y-direction, expanding its
--   bounding region vertically by a factor of @s@ (factors between
--   0 and 1 can be used to shrink the bounding region).  Note that
--   the bounding region will expand with respect to the local origin,
--   so if the origin is not centered vertically the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using 'centerY') before applying @padY@.
padY :: ( Backend b R2, Monoid m )
     => Double -> AnnDiagram b R2 m -> AnnDiagram b R2 m
padY s d = withBounds (d # scaleY s) d
