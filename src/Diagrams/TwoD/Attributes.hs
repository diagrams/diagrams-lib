{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
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
      LineWidth, getLineWidth, lineWidth, lineWidthA
    , lw, lwN, lwO, lwL, lwG
    , ultraThin, veryThin, thin, medium, thick, veryThick, none
    , tiny, verySmall, small, normal, large, veryLarge, huge

    -- ** Dashing
  , Dashing(..), DashingA, getDashing
  , dashing, dashingN, dashingO, dashingL, dashingG

    ) where

import           Data.Data
import           Data.Default.Class
import           Data.Semigroup

import           Diagrams.Core
import           Diagrams.TwoD.Types

-- | Standard 'Measures'.
none, ultraThin, veryThin, thin, medium, thick, veryThick,
  tiny, verySmall, small, normal, large, veryLarge, huge :: Measure R2
none      = Output 0
ultraThin = Normalized 0.0005 `atLeast` Output 0.5
veryThin  = Normalized 0.001  `atLeast` Output 0.5
thin      = Normalized 0.002  `atLeast` Output 0.5
medium    = Normalized 0.004  `atLeast` Output 0.5
thick     = Normalized 0.0075 `atLeast` Output 0.5
veryThick = Normalized 0.01   `atLeast` Output 0.5
tiny      = Normalized 0.01   `atLeast` Output 1
verySmall = Normalized 0.02   `atLeast` Output 1
small     = Normalized 0.0375 `atLeast` Output 1
normal    = Normalized 0.05   `atLeast` Output 1
large     = Normalized 0.075  `atLeast` Output 1
veryLarge = Normalized 0.125  `atLeast` Output 1
huge      = Normalized 0.2    `atLeast` Output 1

-----------------------------------------------------------------
--  Line Width  -------------------------------------------------
-----------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last (Measure R2))
  deriving (Typeable, Data, Semigroup)
instance AttributeClass LineWidth

type instance V LineWidth = R2

instance Transformable LineWidth where
  transform t (LineWidth (Last w)) =
    LineWidth (Last (transform t w))

instance Default LineWidth where
    def = LineWidth (Last (Output 1))

getLineWidth :: LineWidth -> Measure R2
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: (HasStyle a, V a ~ R2) => Measure R2 -> a -> a
lineWidth = applyGTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  (HasStyle a, V a ~ R2) => LineWidth -> a -> a
lineWidthA = applyGTAttr

-- | Default for 'lineWidth'.
lw :: (HasStyle a, V a ~ R2) => Measure R2 -> a -> a
lw = lineWidth

-- | A convenient synonym for 'lineWidth (Global w)'.
lwG :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwG w = lineWidth (Global w)

-- | A convenient synonym for 'lineWidth (Normalized w)'.
lwN :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwN w = lineWidth (Normalized w)

-- | A convenient synonym for 'lineWidth (Output w)'.
lwO :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwO w = lineWidth (Output w)

-- | A convenient sysnonym for 'lineWidth (Local w)'.
lwL :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwL w = lineWidth (Local w)

-----------------------------------------------------------------
--  Dashing  ----------------------------------------------------
-----------------------------------------------------------------

-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Measure R2] (Measure R2)
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
dashing :: (HasStyle a, V a ~ R2) =>
           [Measure R2]  -- ^ A list specifying alternate lengths of on
                         --   and off portions of the stroke.  The empty
                         --   list indicates no dashing.
        -> Measure R2    -- ^ An offset into the dash pattern at which the
                         --   stroke should start.
        -> a -> a
dashing ds offs = applyGTAttr (DashingA (Last (Dashing ds offs)))

-- | A convenient synonym for 'dashing (Global w)'.
dashingG :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingG w v = dashing (map Global w) (Global v)

-- | A convenient synonym for 'dashing (Normalized w)'.
dashingN :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingN w v = dashing (map Normalized w) (Normalized v)

-- | A convenient synonym for 'dashing (Output w)'.
dashingO :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingO w v = dashing (map Output w) (Output v)

-- | A convenient sysnonym for 'dashing (Local w)'.
dashingL :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingL w v = dashing (map Local w) (Local v)
