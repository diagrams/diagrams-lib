{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Attributes
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered. This module defines /Textures/ (Gradients and Colors) in two
-- dimensions. Like the attriubtes defined in the Diagrams.Attributes module,
-- all attributes defined here use the 'Last' or 'Recommend' /semigroup/ structure.
-- 'FillColor' and 'LineColor' attributes are provided so that backends that
-- don't support gradients need not be concerned with using textures. Backends
-- should only implement color attributes or textures attributes, not both.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Attributes (
  -- ** Width
      LineWidth, getLineWidth, lineWidth, lineWidthA, lw

  -- * Measure conversion
    , toOutput

    ) where

import           Data.Default.Class
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace      (magnitude)

import           Diagrams.Core
import           Diagrams.Core.Compile (mapRTreeStyle)
import           Diagrams.Core.Style   (setAttr)
import           Diagrams.Core.Types   (RTree)
import           Diagrams.TwoD.Types   (R2)
import           Diagrams.TwoD.Vector  (unitX, unitY)

------------------------------------------------------------
--  Line Width  -------------------------------------------------
------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last (Measure Double))
  deriving (Typeable, Semigroup)
instance AttributeClass LineWidth

type instance V LineWidth = R2

-- Estimate the line width scaling based on the geometric mean of the x and y
-- scaling of the transformation
geometricScale :: Transformation R2 -> Double -> Double
geometricScale t w = w * sqrt (x*y)
  where
    x = magnitude $ transform t unitX
    y = magnitude $ transform t unitY

instance Transformable LineWidth where
  transform t (LineWidth (Last (Local w))) =
    LineWidth (Last (Local (geometricScale t w)))
  transform _ l = l

instance Default LineWidth where
    def = LineWidth (Last (Output 1))

getLineWidth :: LineWidth -> (Measure Double)
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: (HasStyle a, V a ~ R2) => (Measure Double) -> a -> a
lineWidth = applyTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  (HasStyle a, V a ~ R2) => LineWidth -> a -> a
lineWidthA = applyTAttr

-- | A convenient synonym for 'lineWidth'.
lw :: (HasStyle a, V a ~ R2) => (Measure Double) -> a -> a
lw = lineWidth

-- | Convert all of the @LineWidth@ attributes in an @RTree@ to output
--   units. `w` and `h` are the width and height of the final diagram.
--   The scaling factor is the geometric mean of `h` and `w`.
toOutput :: Double -> Double -> RTree b v () -> RTree b v ()
toOutput w h tr = mapRTreeStyle f tr
  where
    f sty = case getAttr sty of
              Just (LineWidth (Last (Output t))) -> out t sty
              Just (LineWidth (Last (Normalized t))) -> out (s*t) sty
              Just (LineWidth (Last (Local t))) -> out t sty
              Nothing -> sty
    out z st = setAttr (LineWidth (Last (Output z))) st
    s = sqrt (w * h)
