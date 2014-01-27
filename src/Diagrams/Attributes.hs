{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- Every attribute type must have a /semigroup/ structure, that is, an
-- associative binary operation for combining two attributes into one.
-- Unless otherwise noted, all the attributes defined here use the
-- 'Last' structure, that is, combining two attributes simply keeps
-- the second one and throws away the first.  This means that child
-- attributes always override parent attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
  -- * Color
  -- $color

    Color(..), SomeColor(..), black, transparent, SRGBA(..), SRGB(..)

  -- ** Line color
  , LineColor, getLineColor, mkLineColor, styleLineColor, lineColor, lineColorA, lc, lcA

  -- ** Fill color
  , FillColor, getFillColor, mkFillColor, styleFillColor, recommendFillColor, fillColor, fc, fcA

  -- ** Opacity
  , Opacity, getOpacity, opacity

  -- ** Converting colors
  , colorToSRGBA, colorToRGBA

  -- * Lines
  -- ** Width
  , LineWidth, getLineWidth, lineWidth, lineWidthA, lw

  -- ** Cap style
  , LineCap(..), LineCapA, getLineCap, lineCap

  -- ** Join style
  , LineJoin(..), LineJoinA, getLineJoin, lineJoin

  -- ** Miter limit
  , LineMiterLimit(..), getLineMiterLimit, lineMiterLimit, lineMiterLimitA

  -- ** Dashing
  , Dashing(..), DashingA, getDashing, dashing

  ) where

import           Control.Lens          (Setter, sets)
import           Data.Colour           hiding (black, transparent)
import qualified Data.Colour as Colour (black)
import           Data.Colour.RGBSpace  (RGB(..))
import           Data.Colour.SRGB      (toSRGB, sRGB)
import           Data.Default.Class
import           Data.Maybe            (fromMaybe)
import           Data.Monoid.Recommend
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Core
import           Diagrams.Core.Style   (setAttr)

------------------------------------------------------------
--  Color  -------------------------------------------------
------------------------------------------------------------

data SRGBA = SRGBA !Double !Double !Double !Double

transparent :: SRGBA
transparent = SRGBA 0 0 0 0

data SRGB = SRGB !Double !Double !Double

black :: SRGB
black = SRGB 0 0 0

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library.  Instances are provided for
--   both the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types
--   from the "Data.Colour" library.
class Color c where
  -- | Convert a color to its standard representation sRGBA.
  toSRGBA :: c -> SRGBA

  -- | Convert from a SRGBA.
  fromSRGBA :: SRGBA -> c

-- | An existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable


someToSRGBA :: SomeColor -> SRGBA
someToSRGBA (SomeColor c) = toSRGBA c

--someToAlpha :: SomeColor -> AlphaColour Double
--someToAlpha (SomeColor c) = toAlphaColour c

-- | The color with which lines (strokes) are drawn.  Note that child
--   colors always override parent colors; that is, @'lineColor' c1
--   . 'lineColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on line color attributes
--   is that of 'Last'.
newtype LineColor = LineColor (Last SomeColor)
  deriving (Typeable, Semigroup)
instance AttributeClass LineColor

instance Default LineColor where
    def = LineColor (Last (SomeColor black))

getLineColor :: LineColor -> SomeColor
getLineColor (LineColor (Last c)) = c

mkLineColor :: Color c => c -> LineColor
mkLineColor = LineColor . Last . SomeColor

styleLineColor :: (Color c, Color c') => Setter (Style v) (Style v) c c'
styleLineColor = sets modifyLineColor
  where
    modifyLineColor f s
      = flip setAttr s
      . mkLineColor
      . f
      . fromSRGBA . someToSRGBA
      . getLineColor
      . fromMaybe def . getAttr
      $ s

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type , but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (Color c, HasStyle a) => c -> a -> a
lineColor = applyAttr . mkLineColor

-- | Apply a 'lineColor' attribute.
lineColorA :: HasStyle a => LineColor -> a -> a
lineColorA = applyAttr

-- | A synonym for 'lineColor', specialized to @'SRGB'@
--   (i.e. opaque colors).
lc :: HasStyle a => SRGB -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'SRGBA'@
--   (i.e. colors with transparency).
lcA :: HasStyle a => SRGBA -> a -> a
lcA = lineColor

-- | The color with which shapes are filled. Note that child
--   colors always override parent colors; that is, @'fillColor' c1
--   . 'fillColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on fill color attributes
--   is that of 'Last'.
newtype FillColor = FillColor (Recommend (Last SomeColor))
  deriving (Typeable, Semigroup)
instance AttributeClass FillColor

instance Default FillColor where
  def = FillColor (Recommend (Last (SomeColor transparent)))

mkFillColor :: Color c => c -> FillColor
mkFillColor = FillColor . Commit . Last . SomeColor

styleFillColor :: (Color c, Color c') => Setter (Style v) (Style v) c c'
styleFillColor = sets modifyFillColor
  where
    modifyFillColor f s
      = flip setAttr s
      . mkFillColor
      . f
      . fromSRGBA . someToSRGBA
      . getFillColor
      . fromMaybe def . getAttr
      $ s

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (Color c, HasStyle a) => c -> a -> a
fillColor = applyAttr . mkFillColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
recommendFillColor :: (Color c, HasStyle a) => c -> a -> a
recommendFillColor = applyAttr . FillColor . Recommend . Last . SomeColor

getFillColor :: FillColor -> SomeColor
getFillColor (FillColor c) = getLast . getRecommend $ c

-- | A synonym for 'fillColor', specialized to @'SRGB'@
--   (i.e. opaque colors).
fc :: HasStyle a => SRGB -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'SRGBA'@
--   (i.e. colors with transparency).
fcA :: HasStyle a => SRGBA -> a -> a
fcA = fillColor

instance Color SRGBA where
  toSRGBA = id
  fromSRGBA = id

instance Color SRGB where
  toSRGBA (SRGB r g b) = SRGBA r g b 1
  fromSRGBA (SRGBA r g b _) = SRGB r g b

instance Color (Colour Double) where
  toSRGBA col = SRGBA r g b 1
    where RGB r g b = toSRGB col
  fromSRGBA (SRGBA r g b _) = sRGB r g b

instance Color (AlphaColour Double) where
  toSRGBA col = SRGBA r g b a
    where
      c' = alphaColourConvert col
      c = alphaToColour c'
      a = alphaChannel c'
      RGB r g b = toSRGB c
  fromSRGBA (SRGBA r g b a) = sRGB r g b `withOpacity` a

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` Colour.black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` Colour.black)

instance Color SomeColor where
  toSRGBA (SomeColor c) = toSRGBA c
  fromSRGBA c = SomeColor c

instance Color LineColor where
  toSRGBA (LineColor c) = toSRGBA . getLast $ c
  fromSRGBA = LineColor . Last . fromSRGBA

instance Color FillColor where
  toSRGBA (FillColor c) = toSRGBA . getLast . getRecommend $ c
  fromSRGBA = FillColor . Commit . Last . fromSRGBA

-- | Convert to SRGBA.
colorToSRGBA, colorToRGBA :: Color c => c -> (Double, Double, Double, Double)
colorToSRGBA col = (r, g, b, a)
  where SRGBA r g b a = toSRGBA col

colorToRGBA = colorToSRGBA
{-# DEPRECATED colorToRGBA "Renamed to colorToSRGBA." #-}



------------------------------------------------------------
-- Opacity

-- | Although the individual colors in a diagram can have
--   transparency, the opacity/transparency of a diagram as a whole
--   can be specified with the @Opacity@ attribute.  The opacity is a
--   value between 1 (completely opaque, the default) and 0
--   (completely transparent).  Opacity is multiplicative, that is,
--   @'opacity' o1 . 'opacity' o2 === 'opacity' (o1 * o2)@.  In other
--   words, for example, @opacity 0.8@ means \"decrease this diagram's
--   opacity to 80% of its previous opacity\".
newtype Opacity = Opacity (Product Double)
  deriving (Typeable, Semigroup)
instance AttributeClass Opacity

getOpacity :: Opacity -> Double
getOpacity (Opacity (Product d)) = d

-- | Multiply the opacity (see 'Opacity') by the given value.  For
--   example, @opacity 0.8@ means \"decrease this diagram's opacity to
--   80% of its previous opacity\".
opacity :: HasStyle a => Double -> a -> a
opacity = applyAttr . Opacity . Product

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
--
--   Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass LineWidth

instance Default LineWidth where
    def = LineWidth (Last 0.01)

getLineWidth :: LineWidth -> Double
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: HasStyle a => Double -> a -> a
lineWidth = applyAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  HasStyle a => LineWidth -> a -> a
lineWidthA = applyAttr

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

newtype LineCapA = LineCapA (Last LineCap)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass LineCapA

instance Default LineCap where
    def = LineCapButt

getLineCap :: LineCapA -> LineCap
getLineCap (LineCapA (Last c)) = c

-- | Set the line end cap attribute.
lineCap :: HasStyle a => LineCap -> a -> a
lineCap = applyAttr . LineCapA . Last


-- | How should the join points between line segments be drawn?
data LineJoin = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
              | LineJoinRound    -- ^ Use rounded join points.
              | LineJoinBevel    -- ^ Use a \"bevel\" shape (whatever
                                 --   that is).  Are these...
                                 --   carpentry terms?
  deriving (Eq,Show,Typeable)

newtype LineJoinA = LineJoinA (Last LineJoin)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass LineJoinA

instance Default LineJoin where
    def = LineJoinMiter

getLineJoin :: LineJoinA -> LineJoin
getLineJoin (LineJoinA (Last j)) = j

-- | Set the segment join style.
lineJoin :: HasStyle a => LineJoin -> a -> a
lineJoin = applyAttr . LineJoinA . Last


-- | Miter limit attribute affecting the 'LineJoinMiter' joins.
--   For some backends this value may have additional effects.
newtype LineMiterLimit = LineMiterLimit (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass LineMiterLimit

instance Default LineMiterLimit where
    def = LineMiterLimit (Last 10)

getLineMiterLimit :: LineMiterLimit -> Double
getLineMiterLimit (LineMiterLimit (Last l)) = l

-- | Set the miter limit for joins with 'LineJoinMiter'.
lineMiterLimit :: HasStyle a => Double -> a -> a
lineMiterLimit = applyAttr . LineMiterLimit . Last

-- | Apply a 'LineMiterLimit' attribute.
lineMiterLimitA :: HasStyle a => LineMiterLimit -> a -> a
lineMiterLimitA = applyAttr

-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Double] Double
  deriving (Typeable, Eq)

newtype DashingA = DashingA (Last Dashing)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass DashingA

getDashing :: DashingA -> Dashing
getDashing (DashingA (Last d)) = d

-- | Set the line dashing style.
dashing :: HasStyle a =>
           [Double]  -- ^ A list specifying alternate lengths of on
                     --   and off portions of the stroke.  The empty
                     --   list indicates no dashing.
        -> Double    -- ^ An offset into the dash pattern at which the
                     --   stroke should start.
        -> a -> a
dashing ds offs = applyAttr (DashingA (Last (Dashing ds offs)))
