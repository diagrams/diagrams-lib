{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , ViewPatterns
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

      (===), (|||), atAngle

      -- * n-ary combinators
    , hcat, hcat'
    , vcat, vcat'

      -- * Spacing/envelopes
    , strutR2
    , strutX, strutY
    , padX, padY

    , view

    , boundingRect, bg

    ) where

import Data.AffineSpace
import Data.Colour
import Data.Default
import Data.Semigroup
import Data.VectorSpace

import Diagrams.Core

import Diagrams.Attributes (lw, fc)
import Diagrams.BoundingBox
import Diagrams.Combinators
import Diagrams.Coordinates
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD.Align
import Diagrams.TwoD.Path ()   -- for PathLike (D R2) instance
import Diagrams.TwoD.Segment
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Transform (scaleX, scaleY)
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector (unitX, unitY, fromDirection)
import Diagrams.Util ((#))


infixl 6 ===
infixl 6 |||

-- | Place two diagrams (or other objects) vertically
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
(===) :: (Juxtaposable a, V a ~ R2, Semigroup a) => a -> a -> a
(===) = beside (negateV unitY)

-- | Place two diagrams (or other juxtaposable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.  The local origin of the resulting
--   combined diagram is the same as the local origin of the first.
--   @(===)@ is associative and has 'mempty' as a right (but not left)
--   identity.  See the documentation of 'beside' for more information.
(|||) :: (Juxtaposable a, V a ~ R2, Semigroup a) => a -> a -> a
(|||) = beside unitX

-- | Place two diagrams (or other juxtaposable objects) adjacent to one
--   another, with the second diagram placed along a line at angle
--   'th' from the first.  The local origin of the resulting combined
--   diagram is the same as the local origin of the first.
--   See the documentation of 'beside' for more information.
atAngle :: (Juxtaposable a, V a ~ R2, Semigroup a, Angle b) => b -> a -> a -> a
atAngle th = beside (fromDirection th)

-- | Lay out a list of juxtaposable objects in a row from left to right,
--   so that their local origins lie along a single horizontal line,
--   with successive envelopes tangent to one another.
--
--   * For more control over the spacing, see 'hcat''.
--
--   * To align the diagrams vertically (or otherwise), use alignment
--     combinators (such as 'alignT' or 'alignB') from
--     "Diagrams.TwoD.Align" before applying 'hcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
hcat :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2)
     => [a] -> a
hcat = hcat' def

-- | A variant of 'hcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities.
hcat' :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2)
      => CatOpts R2 -> [a] -> a
hcat' = cat' unitX

-- | Lay out a list of juxtaposable objects in a column from top to
--   bottom, so that their local origins lie along a single vertical
--   line, with successive envelopes tangent to one another.
--
--   * For more control over the spacing, see 'vcat''.
--
--   * To align the diagrams horizontally (or otherwise), use alignment
--     combinators (such as 'alignL' or 'alignR') from
--     "Diagrams.TwoD.Align" before applying 'vcat'.
--
--   * For non-axis-aligned layout, see 'cat'.
vcat :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2)
     => [a] -> a
vcat = vcat' def

-- | A variant of 'vcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of the
--   possibilities.
vcat' :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2)
      => CatOpts R2 -> [a] -> a
vcat' = cat' (negateV unitY)

-- | @strutR2 v@ is a two-dimensional diagram which produces no
--   output, but with respect to alignment, envelope, /and trace/ acts
--   like a 1-dimensional segment oriented along the vector @v@, with
--   local origin at its center.  If you don't care about the trace
--   then there's no difference between @strutR2@ and the more general
--   'strut'.
strutR2 :: (Backend b R2, Monoid' m) => R2 -> QDiagram b R2 m
strutR2 v = phantom seg
  where
    seg = FLinear (origin .+^ 0.5 *^ v) (origin .+^ (-0.5) *^ v)

-- | @strutX d@ is an empty diagram with width @d@, height 0, and a
--   centered local origin.  Note that @strutX (-w)@ behaves the same as
--   @strutX w@.
strutX :: (Backend b R2, Monoid' m) => Double -> QDiagram b R2 m
strutX d = strut (d & 0)

-- | @strutY d@ is an empty diagram with height @d@, width 0, and a
--   centered local origin. Note that @strutY (-h)@ behaves the same as
--   @strutY h@.
strutY :: (Backend b R2, Monoid' m) => Double -> QDiagram b R2 m
strutY d = strut (0 & d)

-- | @padX s@ \"pads\" a diagram in the x-direction, expanding its
--   envelope horizontally by a factor of @s@ (factors between 0 and 1
--   can be used to shrink the envelope).  Note that the envelope will
--   expand with respect to the local origin, so if the origin is not
--   centered horizontally the padding may appear \"uneven\".  If this
--   is not desired, the origin can be centered (using 'centerX')
--   before applying @padX@.
padX :: ( Backend b R2, Monoid' m )
     => Double -> QDiagram b R2 m -> QDiagram b R2 m
padX s d = withEnvelope (d # scaleX s) d

-- | @padY s@ \"pads\" a diagram in the y-direction, expanding its
--   envelope vertically by a factor of @s@ (factors between
--   0 and 1 can be used to shrink the envelope).  Note that
--   the envelope will expand with respect to the local origin,
--   so if the origin is not centered vertically the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using 'centerY') before applying @padY@.
padY :: ( Backend b R2, Monoid' m )
     => Double -> QDiagram b R2 m -> QDiagram b R2 m
padY s d = withEnvelope (d # scaleY s) d

-- | @view p v@ sets the envelope of a diagram to a rectangle whose
--   lower-left corner is at @p@ and whose upper-right corner is at @p
--   .+^ v@.  Useful for selecting the rectangular portion of a
--   diagram which should actually be \"viewed\" in the final render,
--   if you don't want to see the entire diagram.
view :: ( Backend b R2, Monoid' m )
     => P2 -> R2 -> QDiagram b R2 m -> QDiagram b R2 m
view p (coords -> w :& h) = withEnvelope (rect w h # alignBL # moveTo p :: D R2)

-- | Construct a bounding rectangle for an enveloped object, that is,
--   the smallest axis-aligned rectangle which encloses the object.
boundingRect :: ( Enveloped p, Transformable p, PathLike p, V p ~ R2
                , Enveloped a, V a ~ R2
                )
             => a -> p
boundingRect = (`boxFit` rect 1 1) . boundingBox

-- | \"Set the background color\" of a diagram.  That is, place a
--   diagram atop a bounding rectangle of the given color.
bg :: (Renderable (Path R2) b) => Colour Double -> Diagram b R2 -> Diagram b R2
bg c d = d <> boundingRect d # lw 0 # fc c
