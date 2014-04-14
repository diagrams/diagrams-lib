{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
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

    Color(..), SomeColor(..), someToAlpha

<<<<<<< HEAD
=======
  -- ** Line color
  , LineColor, getLineColor, mkLineColor, styleLineColor, lineColor, lineColorA, lc, lcA

  -- ** Fill color
  , FillColor, getFillColor, mkFillColor, styleFillColor, recommendFillColor, fillColor, fc, fcA

>>>>>>> master
  -- ** Opacity
  , Opacity, getOpacity, opacity

  -- ** Converting colors
  , colorToSRGBA, colorToRGBA

  -- * Line stuff
  -- ** Cap style
  , LineCap(..), LineCapA, getLineCap, lineCap

  -- ** Join style
  , LineJoin(..), LineJoinA, getLineJoin, lineJoin

  -- ** Miter limit
  , LineMiterLimit(..), getLineMiterLimit, lineMiterLimit, lineMiterLimitA

  -- * Compilation utilities
  , splitFills

  ) where

import           Control.Lens                (Setter, sets)
import           Data.Colour
import           Data.Colour.RGBSpace        (RGB (..))
import           Data.Colour.SRGB            (toSRGB)
import           Data.Default.Class
<<<<<<< HEAD

import           Data.Typeable

=======
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Recommend
>>>>>>> master
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Attributes.Compile
import           Diagrams.Core
import           Diagrams.Core.Style         (setAttr)
import           Diagrams.Core.Types         (RTree)
import           Diagrams.Located            (unLoc)
import           Diagrams.Path               (Path, pathTrails)
import           Diagrams.Trail              (isLoop)

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
  -- | Convert a color to its standard representation, AlphaColour.
  toAlphaColour :: c -> AlphaColour Double

  -- | Convert from an AlphaColour Double.  Note that this direction
  --   may lose some information. For example, the instance for
  --   'Colour' drops the alpha channel.
  fromAlphaColour :: AlphaColour Double -> c

-- | An existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable

<<<<<<< HEAD
=======
someToAlpha :: SomeColor -> AlphaColour Double
someToAlpha (SomeColor c) = toAlphaColour c

-- | The color with which lines (strokes) are drawn.  Note that child
--   colors always override parent colors; that is, @'lineColor' c1
--   . 'lineColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on line color attributes
--   is that of 'Last'.
newtype LineColor = LineColor (Last SomeColor)
  deriving (Typeable, Semigroup)
instance AttributeClass LineColor

instance Default LineColor where
    def = LineColor (Last (SomeColor (black :: Colour Double)))

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
      . fromAlphaColour . someToAlpha
      . getLineColor
      . fromMaybe def . getAttr
      $ s

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (Color c, HasStyle a) => c -> a -> a
lineColor = applyAttr . mkLineColor

-- | Apply a 'lineColor' attribute.
lineColorA :: HasStyle a => LineColor -> a -> a
lineColorA = applyAttr

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
lc :: HasStyle a => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
lcA :: HasStyle a => AlphaColour Double -> a -> a
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
  def = FillColor (Recommend (Last (SomeColor (transparent :: AlphaColour Double))))

mkFillColor :: Color c => c -> FillColor
mkFillColor = FillColor . Commit . Last . SomeColor

styleFillColor :: (Color c, Color c') => Setter (Style v) (Style v) c c'
styleFillColor = sets modifyFillColor
  where
    modifyFillColor f s
      = flip setAttr s
      . mkFillColor
      . f
      . fromAlphaColour . someToAlpha
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

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
fc :: HasStyle a => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
fcA :: HasStyle a => AlphaColour Double -> a -> a
fcA = fillColor

>>>>>>> master
instance (Floating a, Real a) => Color (Colour a) where
  toAlphaColour   = opaque . colourConvert
  fromAlphaColour = colourConvert . (`over` black)

instance (Floating a, Real a) => Color (AlphaColour a) where
  toAlphaColour   = alphaColourConvert
  fromAlphaColour = alphaColourConvert

instance Color SomeColor where
  toAlphaColour (SomeColor c) = toAlphaColour c
  fromAlphaColour c = SomeColor c

<<<<<<< HEAD
-- | Convert to an RGB space while preserving the alpha channel.
toRGBAUsingSpace :: Color c => RGBSpace Double -> c
                            -> (Double, Double, Double, Double)
toRGBAUsingSpace s col = (r,g,b,a)
  where c' = toAlphaColour col
        c  = toRGBUsingSpace s (alphaToColour c')
        a  = alphaChannel  c'
        r  = channelRed   c
        g  = channelGreen c
        b  = channelBlue  c
=======
instance Color LineColor where
  toAlphaColour (LineColor c) = toAlphaColour . getLast $ c
  fromAlphaColour = LineColor . Last . fromAlphaColour

instance Color FillColor where
  toAlphaColour (FillColor c) = toAlphaColour . getLast . getRecommend $ c
  fromAlphaColour = FillColor . Commit . Last . fromAlphaColour
>>>>>>> master

-- | Convert to sRGBA.
colorToSRGBA, colorToRGBA :: Color c => c -> (Double, Double, Double, Double)
colorToSRGBA col = (r, g, b, a)
  where
    c' = toAlphaColour col
    c = alphaToColour c'
    a = alphaChannel c'
    RGB r g b = toSRGB c

colorToRGBA = colorToSRGBA
{-# DEPRECATED colorToRGBA "Renamed to colorToSRGBA." #-}

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)

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
--  Line stuff    -------------------------------------
------------------------------------------------------------

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
------------------------------------------------------------

data FillLoops v = FillLoops

instance Typeable v => SplitAttribute (FillLoops v) where
  type AttrType (FillLoops v) = FillColor
  type PrimType (FillLoops v) = Path v

  primOK _ = all (isLoop . unLoc) . pathTrails

-- | Push fill attributes down until they are at the root of subtrees
--   containing only loops. This makes life much easier for backends,
--   which typically have a semantics where fill attributes are
--   applied to lines/non-closed paths as well as loops/closed paths,
--   whereas in the semantics of diagrams, fill attributes only apply
--   to loops.
splitFills :: forall b v a. Typeable v => RTree b v a -> RTree b v a
splitFills = splitAttr (FillLoops :: FillLoops v)
