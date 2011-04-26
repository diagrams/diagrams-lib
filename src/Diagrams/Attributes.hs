{-# LANGUAGE DeriveDataTypeable
           , ExistentialQuantification
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered.  This module defines some common attributes; particular
-- backends may also define more backend-specific attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
  -- * Color
  -- $color

    Color(..), SomeColor(..)

  -- ** Line color
  , LineColor(..), lineColor, lc, lcA

  -- ** Fill color
  , FillColor(..), fillColor, fc, fcA

  -- * Lines
  , LineWidth(..), lineWidth, lw
  , LineCap(..), lineCap
  , LineJoin(..), lineJoin
  , Dashing(..), dashing

  ) where

import Graphics.Rendering.Diagrams

import Data.Colour
import qualified Data.Colour.SRGB as RGB

import Data.Typeable

------------------------------------------------------------
--  Color  -------------------------------------------------
------------------------------------------------------------

-- $color
-- Diagrams outsources all things color-related to Russell O\'Connor\'s
-- very nice colour package
-- (<http://hackage.haskell.org/package/colour>).  For starters, it
-- provides a large collection of standard color names.  However, it
-- also provides a rich set of combinators for combining and
-- manipulating colors; see its documentation for more information.

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library.  Instances are provided for
--   both the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types
--   from the "Data.Colour" library.
class Color c where
  -- | Convert a color to red, green, blue, and alpha channels in the
  --   range [0,1].
  colorToRGBA :: c -> (Double,Double,Double,Double)

-- | An existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable

-- | The color with which lines (strokes) are drawn.
newtype LineColor = LineColor SomeColor
  deriving Typeable
instance AttributeClass LineColor

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (Color c, HasStyle a) => c -> a -> a
lineColor = applyAttr . LineColor . SomeColor

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
lc :: HasStyle a => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
lcA :: HasStyle a => AlphaColour Double -> a -> a
lcA = lineColor

-- | The color with which shapes are filled.
newtype FillColor = FillColor SomeColor
  deriving Typeable
instance AttributeClass FillColor

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (Color c, HasStyle a) => c -> a -> a
fillColor = applyAttr . FillColor . SomeColor

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
fc :: HasStyle a => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
fcA :: HasStyle a => AlphaColour Double -> a -> a
fcA = fillColor

instance (Floating a, Real a) => Color (Colour a) where
  colorToRGBA col = (r,g,b,1)
    where c' = RGB.toSRGB . colourConvert $ col
          r  = RGB.channelRed c'
          g  = RGB.channelGreen c'
          b  = RGB.channelBlue c'

instance (Floating a, Real a) => Color (AlphaColour a) where
  colorToRGBA col = (r,g,b,a)
    where col' = alphaColourConvert col
          a  = alphaChannel col'
          c' = RGB.toSRGB . alphaToColour $ col'
          r  = RGB.channelRed c'
          g  = RGB.channelGreen c'
          b  = RGB.channelBlue c'

instance Color SomeColor where
  colorToRGBA (SomeColor c) = colorToRGBA c

instance Color LineColor where
  colorToRGBA (LineColor c) = colorToRGBA c

instance Color FillColor where
  colorToRGBA (FillColor c) = colorToRGBA c

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)


------------------------------------------------------------
--  Lines and stuff    -------------------------------------
------------------------------------------------------------

-- | The width of lines.  By default, the line width is measured with
--   respect to the /final/ coordinate system of a rendered diagram,
--   as opposed to the local coordinate systems in effect at the time
--   the line width was set for various subdiagrams.  This is so that
--   it is easy to combine a variety of shapes (some created by
--   scaling) and have them all drawn using a consistent line width.
--   However, sometimes it is desirable for scaling to affect line
--   width; the 'freeze' operation is provided for this purpose.  The
--   line width of frozen diagrams is affected by transformations.
newtype LineWidth = LineWidth Double
  deriving Typeable
instance AttributeClass LineWidth

-- | Set the line (stroke) width.
lineWidth :: HasStyle a => Double -> a -> a
lineWidth = applyAttr . LineWidth

-- | A convenient synonym for 'lineWidth'.
lw :: HasStyle a => Double -> a -> a
lw = lineWidth


-- | What sort of shape should be placed at the endpoints of lines?
data LineCap = LineCapButt   -- ^ Lines end precisely at their endpoints.
             | LineCapRound  -- ^ Lines are capped with semicircles
                             --   centered on endpoints.
             | LineCapSquare -- ^ Lines are capped with a squares
                             --   centered on endpoints.
  deriving (Eq,Show,Typeable)
instance AttributeClass LineCap

-- | Set the line end cap attribute.
lineCap :: HasStyle a => LineCap -> a -> a
lineCap = applyAttr


-- | How should the join points between line segments be drawn?
data LineJoin = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
              | LineJoinRound    -- ^ Use rounded join points.
              | LineJoinBevel    -- ^ Use a \"bevel\" shape (whatever
                                 --   that is).  Are these...
                                 --   carpentry terms?
  deriving (Eq,Show,Typeable)
instance AttributeClass LineJoin

-- | Set the segment join style.
lineJoin :: HasStyle a => LineJoin -> a -> a
lineJoin = applyAttr


-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Double] Double
  deriving Typeable
instance AttributeClass Dashing

-- | Set the line dashing style.
dashing :: HasStyle a =>
           [Double]  -- ^ A list specifying alternate lengths of on
                     --   and off portions of the stroke.  The empty
                     --   list indicates no dashing.
        -> Double    -- ^ An offset into the dash pattern at which the
                     --   stroke should start.
        -> a -> a
dashing ds offs = applyAttr (Dashing ds offs)
