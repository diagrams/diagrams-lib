{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup

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
    , hcat, hcat', hsep
    , vcat, vcat', vsep

      -- * Spacing/envelopes
    , strutR2
    , strutX, strutY
    , padX, padY

    , extrudeLeft, extrudeRight, extrudeBottom, extrudeTop

    , rectEnvelope, crop

    , boundingRect, bg, bgFrame

    ) where

import           Control.Lens             ((&), (.~))
import           Data.Colour
import           Data.Default
import           Data.Semigroup

import           Diagrams.Core

import           Diagrams.Attributes      (lwO)
import           Diagrams.BoundingBox
import           Diagrams.Combinators
import           Diagrams.Path
import           Diagrams.Query           (value)
import           Diagrams.Segment
import           Diagrams.TrailLike
import           Diagrams.TwoD.Align
import           Diagrams.TwoD.Attributes (fc)
import           Diagrams.TwoD.Path       ()
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Transform  (scaleX, scaleY)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector
import           Diagrams.Util            (( # ))

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

infixl 6 ===
infixl 6 |||

-- | Place two diagrams (or other objects) vertically adjacent to one
--   another, with the first diagram above the second.  Since Haskell
--   ignores whitespace in expressions, one can thus write
--
--   @
--       c
--      ===
--       d
--   @
--
--   to place @c@ above @d@.  The local origin of the resulting
--   combined diagram is the same as the local origin of the first.
--   @(===)@ is associative and has 'mempty' as an identity.  See the
--   documentation of 'beside' for more information.
(===) :: (InSpace V2 n a, Juxtaposable a, Semigroup a) => a -> a -> a
(===) = beside unit_Y

-- | Place two diagrams (or other juxtaposable objects) horizontally
--   adjacent to one another, with the first diagram to the left of
--   the second.  The local origin of the resulting combined diagram
--   is the same as the local origin of the first.  @(|||)@ is
--   associative and has 'mempty' as an identity.  See the
--   documentation of 'beside' for more information.
(|||) :: (InSpace V2 n a, Juxtaposable a, Semigroup a) => a -> a -> a
(|||) = beside unitX

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
hcat :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => [a] -> a
hcat = hcat' def

-- | A variant of 'hcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities. For the common case of setting just a
--   separation amount, see 'hsep'.
hcat' :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
      => CatOpts n -> [a] -> a
hcat' = cat' unitX

-- | A convenient synonym for horizontal concatenation with
--   separation: @hsep s === hcat' (with & sep .~ s)@.
hsep :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => n -> [a] -> a
hsep s = hcat' (def & sep .~ s)

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
vcat :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => [a] -> a
vcat = vcat' def

-- | A variant of 'vcat' taking an extra 'CatOpts' record to control
--   the spacing.  See the 'cat'' documentation for a description of
--   the possibilities.  For the common case of setting just a
--   separation amount, see 'vsep'.
vcat' :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
      => CatOpts n -> [a] -> a
vcat' = cat' unit_Y

-- | A convenient synonym for vertical concatenation with
--   separation: @vsep s === vcat' (with & sep .~ s)@.
vsep :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
     => n -> [a] -> a
vsep s = vcat' (def & sep .~ s)

-- | @strutR2 v@ is a two-dimensional diagram which produces no
--   output, but with respect to alignment, envelope, /and trace/ acts
--   like a 1-dimensional segment oriented along the vector @v@, with
--   local origin at its center.  If you don't care about the trace
--   then there's no difference between @strutR2@ and the more general
--   'strut'.
strutR2 :: (RealFloat n, Monoid' m) => V2 n -> QDiagram b V2 n m
strutR2 v = phantom seg
  where
    seg = FLinear (origin .+^ 0.5 *^ v) (origin .+^ (-0.5) *^ v)

-- | @strutX w@ is an empty diagram with width @w@, height 0, and a
--   centered local origin.  Note that @strutX (-w)@ behaves the same as
--   @strutX w@.
strutX :: (Metric v, R1 v, OrderedField n) => n -> QDiagram b v n m
strutX d = strut (zero & _x .~ d)

-- | @strutY h@ is an empty diagram with height @h@, width 0, and a
--   centered local origin. Note that @strutY (-h)@ behaves the same as
--   @strutY h@.
strutY :: (Metric v, R2 v, OrderedField n) => n -> QDiagram b v n m
strutY d = strut (zero & _y .~ d)

-- | @padX s@ \"pads\" a diagram in the x-direction, expanding its
--   envelope horizontally by a factor of @s@ (factors between 0 and 1
--   can be used to shrink the envelope).  Note that the envelope will
--   expand with respect to the local origin, so if the origin is not
--   centered horizontally the padding may appear \"uneven\".  If this
--   is not desired, the origin can be centered (using 'centerX')
--   before applying @padX@.
padX :: (Metric v, R2 v, OrderedField n, Monoid' m)
     => n -> QDiagram b v n m -> QDiagram b v n m
padX s d = withEnvelope (d # scaleX s) d

-- | @padY s@ \"pads\" a diagram in the y-direction, expanding its
--   envelope vertically by a factor of @s@ (factors between
--   0 and 1 can be used to shrink the envelope).  Note that
--   the envelope will expand with respect to the local origin,
--   so if the origin is not centered vertically the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using 'centerY') before applying @padY@.
padY :: (Metric v, R2 v, Monoid' m, OrderedField n)
     => n -> QDiagram b v n m -> QDiagram b v n m
padY s d = withEnvelope (d # scaleY s) d

-- | @extrudeLeft s@ \"extrudes\" a diagram in the negative x-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeLeft :: (OrderedField n, Monoid' m) => n -> QDiagram b V2 n m -> QDiagram b V2 n m
extrudeLeft s
  | s >= 0    = extrudeEnvelope $ unitX ^* negate s
  | otherwise = intrudeEnvelope $ unitX ^* negate s

-- | @extrudeRight s@ \"extrudes\" a diagram in the positive x-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeRight :: (OrderedField n, Monoid' m) => n -> QDiagram b V2 n m -> QDiagram b V2 n m
extrudeRight s
  | s >= 0    = extrudeEnvelope $ unitX ^* s
  | otherwise = intrudeEnvelope $ unitX ^* s

-- | @extrudeBottom s@ \"extrudes\" a diagram in the negative y-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeBottom :: (OrderedField n, Monoid' m) => n -> QDiagram b V2 n m -> QDiagram b V2 n m
extrudeBottom s
  | s >= 0    = extrudeEnvelope $ unitY ^* negate s
  | otherwise = intrudeEnvelope $ unitY ^* negate s

-- | @extrudeTop s@ \"extrudes\" a diagram in the positive y-direction,
--   offsetting its envelope by the provided distance. When @ s < 0 @,
--   the envelope is inset instead.
--
--   See the documentation for 'extrudeEnvelope' for more information.
extrudeTop :: (OrderedField n, Monoid' m) => n -> QDiagram b V2 n m -> QDiagram b V2 n m
extrudeTop s
  | s >= 0    = extrudeEnvelope $ unitY ^* s
  | otherwise = intrudeEnvelope $ unitY ^* s

-- | @rectEnvelope p v@ sets the envelope of a diagram to a rectangle whose
--   lower-left corner is at @p@ and whose upper-right corner is at @p
--   .+^ v@.  Useful for selecting the rectangular portion of a
--   diagram which should actually be \"viewed\" in the final render,
--   if you don't want to see the entire diagram.
rectEnvelope :: forall b n m. (OrderedField n, Monoid' m)
     => Point V2 n -> V2 n -> QDiagram b V2 n m -> QDiagram b V2 n m
rectEnvelope p (V2 w h) = withEnvelope (rect w h # alignBL # moveTo p :: Path V2 n)

-- | A synonym for 'rectEnvelope'.
crop :: forall b n m. (OrderedField n, Monoid' m)
     => Point V2 n -> V2 n -> QDiagram b V2 n m -> QDiagram b V2 n m
crop = rectEnvelope

-- | Construct a bounding rectangle for an enveloped object, that is,
--   the smallest axis-aligned rectangle which encloses the object.
boundingRect :: ( InSpace V2 n a, SameSpace a t
                , Enveloped t, Transformable t, TrailLike t, Monoid t
                , Enveloped a)
             => a -> t
boundingRect = (`boxFit` rect 1 1) . boundingBox

-- | \"Set the background color\" of a diagram.  That is, place a
--   diagram atop a bounding rectangle of the given color.
--   The background does not change the result of queries.
bg :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' q)
    => Colour Double -> QDiagram b V2 n q -> QDiagram b V2 n q
bg c d = d <> boundingRect d # lwO 0 # fc c # value mempty

-- | Similar to 'bg' but makes the colored background rectangle larger than
--   the diagram. The first parameter is used to set how far the background
--   extends beyond the diagram.
--   The background does not change the result of queries.
bgFrame :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' q)
    => n -> Colour Double -> QDiagram b V2 n q -> QDiagram b V2 n q
bgFrame f c d = d <> boundingRect (frame f d) # lwO 0 # fc c # value mempty
