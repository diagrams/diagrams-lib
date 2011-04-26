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
-- Some common attributes.  Particular backends may also define more
-- backend-specific attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
  -- * Color

    Color(..), SomeColor(..)

  , LineColor(..), lineColor, lc, lcA
  , FillColor(..), fillColor, fc, fcA
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

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library; that is, every function in
--   the Diagrams library which expects a color can take any type
--   which is an instance of 'Color'.  Instances are provided for both
--   the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types from
--   the "Data.Colour" library.
class Color c where
  colorToRGBA :: c -> (Double,Double,Double,Double)

-- | Existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable

-- | Line/stroke color attribute.
newtype LineColor = LineColor SomeColor
  deriving Typeable
instance AttributeClass LineColor

-- | Set the line (stroke) color of a diagram.  This function is
--   polymorphic in the color type (so it can be used with either
--   'Colour' or 'AlphaColour'), but this can sometimes create
--   problems for type inference, so the 'lc' and 'lcA' variants are
--   provided with more concrete types.
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

-- | Fill color attribute.
newtype FillColor = FillColor SomeColor
  deriving Typeable
instance AttributeClass FillColor

-- | Set the fill color of a diagram.  This function is polymorphic in
--   the color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'fc' and 'fcA' variants are provided with more
--   concrete types.
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
--  Other attributes  -------------------------------------
------------------------------------------------------------

-- | Line/stroke width attribute.
newtype LineWidth = LineWidth Double
  deriving Typeable
instance AttributeClass LineWidth

-- | Set the line (stroke) width of a diagram.
lineWidth :: HasStyle a => Double -> a -> a
lineWidth = applyAttr . LineWidth

-- | A convenient synonym for 'lineWidth'.
lw :: HasStyle a => Double -> a -> a
lw = lineWidth


-- | Line/stroke end cap attribute.
data LineCap = LineCapButt | LineCapRound | LineCapSquare
  deriving (Eq,Show,Typeable)
instance AttributeClass LineCap

-- | Set the line (stroke) end cap of a diagram.
lineCap :: HasStyle a => LineCap -> a -> a
lineCap = applyAttr


-- | Line/stroke join attribute.
data LineJoin = LineJoinMiter | LineJoinRound | LineJoinBevel
  deriving (Eq,Show,Typeable)
instance AttributeClass LineJoin

-- | Set the line (stroke) join of a diagram.
lineJoin :: HasStyle a => LineJoin -> a -> a
lineJoin = applyAttr


-- | Line/stroke dashing attribute.
data Dashing = Dashing [Double] Double
  deriving Typeable
instance AttributeClass Dashing

-- | Set the line (stroke) dashing of a diagram.
dashing :: HasStyle a =>
           [Double]  -- ^ a list specifying alternate lengths of on
                     --   and off portions of the stroke.  The empty
                     --   list indicates no dashing.
        -> Double    -- ^ an offset into the dash pattern at which the
                     --   stroke should start
        -> a -> a
dashing ds offs = applyAttr (Dashing ds offs)
