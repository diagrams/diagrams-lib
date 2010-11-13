
module Diagrams.Attributes (
  -- * Color

    Color(..), SomeColor(..)

  , lc

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

instance AttributeClass SomeColor

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
  colorToRGBA (SomeColor col) = colorToRGBA col

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)

lc :: Color c => c -> Diagram b -> Diagram b
lc c = applyAttr (SomeColor c)