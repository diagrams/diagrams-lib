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
      LineWidth, getLineWidth, lineWidth, lineWidthA, setLineWidth
    , lw, lwN, lwO, lwL
    , ultraThin, veryThin, thin, medium, thick, veryThick

    ) where

import           Data.Default.Class
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Core
import           Diagrams.Core.Style     (setAttr)
import           Diagrams.TwoD.Transform (avgScale)
import           Diagrams.TwoD.Types     (R2)

------------------------------------------------------------
--  Line Width  -------------------------------------------------
------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last (Measure Double))
  deriving (Typeable, Semigroup)
instance AttributeClass LineWidth

type instance V LineWidth = R2

instance Transformable LineWidth where
  transform t (LineWidth (Last (Local w))) =
    LineWidth (Last (Local (avgScale t * w)))
  transform _ l = l

instance Default LineWidth where
    def = LineWidth (Last (Output 1))

getLineWidth :: LineWidth -> (Measure Double)
getLineWidth (LineWidth (Last w)) = w

setLineWidth :: (Measure Double) -> Style R2 -> Style R2
setLineWidth = setAttr . LineWidth . Last

-- | Set the line (stroke) width.
lineWidth :: (HasStyle a, V a ~ R2) => (Measure Double) -> a -> a
lineWidth = applyTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  (HasStyle a, V a ~ R2) => LineWidth -> a -> a
lineWidthA = applyTAttr

-- | A convenient synonym for 'lineWidth (Global w)'.
lw :: (HasStyle a, V a ~ R2) => Double -> a -> a
lw w = lineWidth (Global w)

-- | A convenient synonym for 'lineWidth (Normalized w)'.
lwN :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwN w = lineWidth (Normalized w)

-- | A convenient synonym for 'lineWidth (Output w)'.
lwO :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwO w = lineWidth (Output w)

-- | A convenient sysnonym for 'lineWidth (Local w)'.
lwL :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwL w = lineWidth (Local w)

-- | Standard line widths.
ultraThin, veryThin, thin, medium, thick, veryThick
  :: (HasStyle a, V a ~ R2) => a -> a
ultraThin = lwO 0.25
veryThin  = lwO 0.5
thin      = lwO 1
medium    = lwO 2
thick     = lwO 4
veryThick = lwO 5
