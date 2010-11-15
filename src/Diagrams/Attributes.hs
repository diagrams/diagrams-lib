{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Some common attributes.  Particular backends may also define more
-- backend-specific attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
  -- * Color

    Color(..), SomeColor(..)

  , LineColor(..), lineColor, lc
  , FillColor(..), fillColor, fc
  , LineWidth(..), lineWidth, lw
  , LineCap(..), lineCap
  , LineJoin(..), lineJoin

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

-- | Set the line (stroke) color of a diagram.
lineColor :: Color c => c -> Diagram b -> Diagram b
lineColor = applyAttr . LineColor . SomeColor

-- | A convenient synonym for 'lineColor'.
lc :: Color c => c -> Diagram b -> Diagram b
lc = lineColor

-- | Fill color attribute.
newtype FillColor = FillColor SomeColor
  deriving Typeable
instance AttributeClass FillColor

-- | Set the fill color of a diagram.
fillColor :: Color c => c -> Diagram b -> Diagram b
fillColor = applyAttr . FillColor . SomeColor

-- | A convenient synonym for 'fillColor'.
fc :: Color c => c -> Diagram b -> Diagram b
fc = fillColor

-- Note: we would like to just be able to say 'instance Color (Colour
-- Double)' and so on, but the problem is that the named color
-- constants in Data.Colour.Names are polymorphic with type (Floating
-- a, Ord a) => Colour a, so trying to pass one of these constants to
-- a function like 'lc' gives an error that there is no instance for
-- Color (Colour a).  Adding a type annotation like 'lc (black ::
-- Colour Double)' works, but this is a pain for the user.  The
-- (admittedly hackish) solution is to make general instances which
-- require Floating and Real (so that we can convert to Double with
-- fromRational . toRational), and let type defaulting figure out that
-- in the expression 'lc black', black should have type Colour Double.

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
lineWidth :: Double -> Diagram b -> Diagram b
lineWidth = applyAttr . LineWidth

-- | A convenient synonym for 'lineWidth'.
lw :: Double -> Diagram b -> Diagram b
lw = lineWidth


-- | Line/stroke end cap attribute.
data LineCap = LineCapButt | LineCapRound | LineCapSquare
  deriving (Eq,Show,Typeable)
instance AttributeClass LineCap

-- | Set the line (stroke) end cap of a diagram.
lineCap :: LineCap -> Diagram b -> Diagram b
lineCap = applyAttr


-- | Line/stroke join attribute.
data LineJoin = LineJoinMiter | LineJoinRound | LineJoinBevel
  deriving (Eq,Show,Typeable)
instance AttributeClass LineJoin

-- | Set the line (stroke) join of a diagram.
lineJoin :: LineJoin -> Diagram b -> Diagram b
lineJoin = applyAttr
