{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

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
      LineWidth, getLineWidth, lineWidth, lineWidthA
    , lw, lwN, lwO, lwL
    , ultraThin, veryThin, thin, medium, thick, veryThick

    -- ** Dashing
  , Dashing(..), DashingA, getDashing, setDashing
  , dashing, dashingN, dashingO, dashingL

    ) where

import           Data.Data
import           Data.Default.Class
import           Data.Semigroup

import           Diagrams.Core
import           Diagrams.TwoD.Types (R2)

-----------------------------------------------------------------

type instance V (Measure Double) = R2

instance Transformable (Measure Double) where
  transform tr (Local x) = Local (avgScale tr * x)
  transform _ y = y

-----------------------------------------------------------------
--  Line Width  -------------------------------------------------
-----------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last (Measure Double))
  deriving (Typeable, Data, Semigroup)
instance AttributeClass LineWidth

type instance V LineWidth = R2

instance Transformable LineWidth where
  transform t (LineWidth (Last w)) =
    LineWidth (Last (transform t w))

instance Default LineWidth where
    def = LineWidth (Last (Output 1))

getLineWidth :: LineWidth -> Measure Double
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: (HasStyle a, V a ~ R2) => (Measure Double) -> a -> a
lineWidth = applyGTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  (HasStyle a, V a ~ R2) => LineWidth -> a -> a
lineWidthA = applyGTAttr

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

-----------------------------------------------------------------
--  Dashing  ----------------------------------------------------
-----------------------------------------------------------------

-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Measure Double] (Measure Double)
  deriving (Typeable, Data, Eq)

newtype DashingA = DashingA (Last Dashing)
  deriving (Typeable, Data, Semigroup, Eq)
instance AttributeClass DashingA

type instance V DashingA = R2

instance Transformable DashingA where
  transform t (DashingA (Last (Dashing w v))) =
    DashingA (Last (Dashing r s))
    where
      r = map (transform t) w
      s = transform t v

getDashing :: DashingA -> Dashing
getDashing (DashingA (Last d)) = d

-- | Set the line dashing style.
setDashing :: (HasStyle a, V a ~ R2) =>
           [Measure Double]  -- ^ A list specifying alternate lengths of on
                     --   and off portions of the stroke.  The empty
                     --   list indicates no dashing.
        -> Measure Double    -- ^ An offset into the dash pattern at which the
                     --   stroke should start.
        -> a -> a
setDashing ds offs = applyGTAttr (DashingA (Last (Dashing ds offs)))

-- | A convenient synonym for 'setDashing (Global w)'.
dashing :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashing w v = setDashing (map Global w) (Global v)

-- | A convenient synonym for 'setDashing (Normalized w)'.
dashingN :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingN w v = setDashing (map Normalized w) (Normalized v)

-- | A convenient synonym for 'setDashing (Output w)'.
dashingO :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingO w v = setDashing (map Output w) (Output v)

-- | A convenient sysnonym for 'setDashing (Local w)'.
dashingL :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingL w v = setDashing (map Local w) (Local v)
