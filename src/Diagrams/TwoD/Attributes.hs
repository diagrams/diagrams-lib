{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
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
  -- * Textures
    Texture(..), _SC, _LG, _RG, defaultLG, defaultRG
  , GradientStop(..), stopColor, stopFraction, mkStops
  , SpreadMethod(..), lineLGradient, lineRGradient


  -- ** Linear Gradients
  , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
  , lGradSpreadMethod, mkLinearGradient

  -- ** Radial Gradients
  , RGradient(..), rGradStops, rGradTrans, rGradRadius, rGradCenter, rGradFocus
  , rGradSpreadMethod, mkRadialGradient

  -- ** Line texture
  ,  LineTexture(..), getLineTexture, lineTexture

  -- ** Line color
  , LineColor, lineColor, getLineColor, lc, lcA, lineColorA

  -- ** Fill texture
  , FillTexture(..), getFillTexture, fillTexture

  -- ** Fill color
  , FillColor, fillColor, getFillColor, fc, fcA, recommendFillColor

  ) where

import           Diagrams.Core
import           Diagrams.Attributes (Color(..), SomeColor(..))
import           Diagrams.TwoD.Types (T2, R2, P2, mkP2)

import           Control.Lens ( makeLensesWith, generateSignatures, lensRules
                              , makePrisms, Lens', (&), (%~), (.~), makeLenses)

import           Data.Colour hiding (AffineSpace)
import           Data.Default.Class
import           Data.Typeable

import           Data.Monoid.Recommend
import           Data.Semigroup

-- | A gradient stop contains a color and fraction (usually between 0 and 1)
data GradientStop = GradientStop
     { _stopColor    :: SomeColor
     , _stopFraction :: Double}

makeLensesWith (lensRules & generateSignatures .~ False) ''GradientStop

-- | A color for the stop.
stopColor :: Lens' GradientStop SomeColor

-- | The fraction for stop.
stopFraction :: Lens' GradientStop Double

-- | The 'SpreadMethod' determines what happens before 'lGradStart' and after
--   'lGradEnd'. 'GradPad' fills the space before the start of the gradient
--   with the color of the first stop and the color after end of the gradient
--   with the color of the last stop. 'GradRepeat' restarts the gradient and
--   'GradReflect' restarts the gradient with the stops in reverse order.
data SpreadMethod = GradPad | GradReflect | GradRepeat

-- | Linear Gradient
data LGradient = LGradient
    { _lGradStops        :: [GradientStop]
    , _lGradStart        :: P2
    , _lGradEnd          :: P2
    , _lGradTrans        :: T2
    , _lGradSpreadMethod :: SpreadMethod }

makeLensesWith (lensRules & generateSignatures .~ False) ''LGradient

-- | A list of stops (colors and fractions).
lGradStops :: Lens' LGradient [GradientStop]

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
lGradTrans :: Lens' LGradient T2

-- | The starting point for the first gradient stop. Values for the coordinates
--   between 0 and 1 are inside of the object to which the gradient is applied.
--   The default is (0,0).
lGradStart :: Lens' LGradient P2

-- | The ending point for the last gradient stop. Values for the coordinates
--   between 0 and 1 are inside of the object to which the gradient is applied.
--   The default is (1,0), that is the gradient runs from left to right.
lGradEnd :: Lens' LGradient P2

-- | For setting the spread method.
lGradSpreadMethod :: Lens' LGradient SpreadMethod

-- | Radial Gradient
data RGradient = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradRadius       :: Double
    , _rGradCenter       :: P2
    , _rGradFocus        :: P2
    , _rGradTrans        :: T2
    , _rGradSpreadMethod :: SpreadMethod }

makeLensesWith (lensRules & generateSignatures .~ False) ''RGradient

-- | A list of stops (colors and fractions).
rGradStops :: Lens' RGradient [GradientStop]

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
rGradTrans :: Lens' RGradient T2

-- | The radius and center determine where the gradient ends.
rGradRadius :: Lens' RGradient Double

-- | The radius and center determine where the gradient ends.
rGradCenter :: Lens' RGradient P2

-- | The focal point of the radial gradient. The point to which the 0 gradient
--   stop is mapped.
rGradFocus :: Lens' RGradient P2

-- | For setting the spread method.
rGradSpreadMethod :: Lens' RGradient SpreadMethod

-- | A Texture is either a color 'SC', linear gradient 'LG', or radial gradient 'RG'.
--   An object can have only one texture which is determined by the 'Last'
--   semigroup structure. The prisms '_SC', '_LG', '_RG' can be used as setters
--   for a texture.
data Texture = SC SomeColor | LG LGradient | RG RGradient
  deriving (Typeable)

makePrisms ''Texture

-- | A default is provided so that linear gradients can easily be created using
--   lenses. For example, @lg = defaultLG & lGradStart .~ (0.25 ^& 0.33)@. Note that
--   no default value is provided for @lGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultLG :: Texture
defaultLG = LG (LGradient
    { _lGradStops        = []
    , _lGradStart        = mkP2 0 0
    , _lGradEnd          = mkP2 1 0
    , _lGradTrans        = mempty
    , _lGradSpreadMethod = GradPad
    })

-- | A default is provided so that radial gradients can easily be created using
--   lenses. For example, @rg = defaultRG & rGradRadius .~ 0.25@. Note that
--   no default value is provided for @rGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultRG :: Texture
defaultRG = RG (RGradient
    { _rGradStops        = []
    , _rGradRadius       = 0.5
    , _rGradCenter       = mkP2 0 0
    , _rGradFocus        = mkP2 0 0
    , _rGradTrans        = mempty
    , _rGradSpreadMethod = GradPad
    })

-- | A convenient function for making gradient stops from a list of triples.
--   (An opaque color, a stop fraction, an opacity).
mkStops :: [(Colour Double, Double, Double)] -> [GradientStop]
mkStops s = map (\(x, y, z) -> GradientStop (SomeColor (withOpacity x z)) y) s

-- | Make a linear gradient texture from a stop list, start point, end point,
--   and 'SpreadMethod'. The 'lGradTrans' field is set to the identity
--   transfrom, to change it use the 'lGradTrans' lens.
mkLinearGradient :: [GradientStop]  -> P2 -> P2 -> SpreadMethod -> Texture
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end (scaling 1) spreadMethod)

-- | Make a radial gradient texture from a stop list, radius, start point,
--   end point, and 'SpreadMethod'. The 'lGradTrans' field is set to the identity
--   transfrom, to change it use the 'rGradTrans' lens.
mkRadialGradient :: [GradientStop] -> Double -> P2 -> P2 -> SpreadMethod -> Texture
mkRadialGradient stops r center focus spreadMethod
  = RG (RGradient stops r center focus mempty spreadMethod)

-- | The texture which lines are drawn.  Note that child
--   textures always override parent textures.
--   More precisely, the semigroup structure on line texture attributes
--   is that of 'Last'.
newtype LineTexture = LineTexture (Last Texture)
  deriving (Typeable, Semigroup)
instance AttributeClass LineTexture

type instance V LineTexture = R2

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Transformable LineTexture where
  transform t (LineTexture (Last texture)) = LineTexture (Last tx)
    where
      tx = texture & lgt . rgt
      lgt = _LG . lGradTrans %~ f
      rgt = _RG . rGradTrans %~ f
      f = transform t

instance Default LineTexture where
    def = LineTexture (Last (SC (SomeColor (black :: Colour Double))))

getLineTexture :: LineTexture -> Texture
getLineTexture (LineTexture (Last t)) = t

lineTexture :: (HasStyle a, V a ~ R2) => Texture-> a -> a
lineTexture = applyTAttr . LineTexture . Last

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

instance Color LineColor where
  toAlphaColour (LineColor (Last c)) = toAlphaColour c

getLineColor :: LineColor -> SomeColor
getLineColor (LineColor (Last c)) = c

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types. 'lineColor' adds both a texture attribute and a
--   color attribute to the style so that backends that don't support gradients
--   don't need to worry about them. It is important that backends only implement
--   one or the other.
lineColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
lineColor c = (lTx c) . (lCl c)
  where
    lTx x = lineTexture (SC (SomeColor x))
    lCl = applyAttr . LineColor . Last . SomeColor

-- | Apply a 'lineColor' attribute. See comment in 'lineColor' about backends.
lineColorA :: HasStyle a => LineColor -> a -> a
lineColorA = applyAttr

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).  See comment in 'lineColor' about backends.
lc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).  See comment in 'lineColor'
--   about backends.
lcA :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
lcA = lineColor

-- | Apply a linear gradient.
lineLGradient :: (HasStyle a, V a ~ R2) => LGradient -> a -> a
lineLGradient g = lineTexture (LG g)

-- | Apply a radial gradient.
lineRGradient :: (HasStyle a, V a ~ R2) => RGradient -> a -> a
lineRGradient g = lineTexture (RG g)

-- | The texture which objects are filled.
--   The semigroup structure on fill texture attributes
--   is that of 'Recommed . Last'.
newtype FillTexture = FillTexture (Recommend (Last Texture))
  deriving (Typeable, Semigroup)

instance AttributeClass FillTexture

type instance V FillTexture = R2

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance Transformable FillTexture where
  transform _ tx@(FillTexture (Recommend _)) = tx
  transform t (FillTexture (Commit (Last texture))) = FillTexture (Commit (Last tx))
    where
      tx = texture & lgt . rgt
      lgt = _LG . lGradTrans %~ f
      rgt = _RG . rGradTrans %~ f
      f = transform t

getFillTexture :: FillTexture -> Texture
getFillTexture (FillTexture tx) = getLast . getRecommend $ tx

fillTexture :: (HasStyle a, V a ~ R2) => Texture -> a -> a
fillTexture = applyTAttr . FillTexture . Commit . Last

-- | The color with which shapes are filled. Note that child
--   colors always override parent colors; that is, @'fillColor' c1
--   . 'fillColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on fill color attributes
--   is that of 'Last'.
newtype FillColor = FillColor (Recommend (Last SomeColor))
  deriving (Typeable, Semigroup)
instance AttributeClass FillColor

instance Color FillColor where
  toAlphaColour (FillColor c) = toAlphaColour . getLast . getRecommend $ c

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
--   'fillColor' adds both a texture attribute and a
--   color attribute to the style so that backends that don't support gradients
--   don't need to worry about them. It is important that backends only implement
--   one or the other.
fillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
fillColor c = (fTx c) . (fCl c)
  where
    fTx x = fillTexture (SC (SomeColor x))
    fCl  = applyAttr . FillColor . Commit . Last . SomeColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
--   See comment after 'fillColor' about backends.
recommendFillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
recommendFillColor c = (fT c) . (fC c)
  where
    fT = applyTAttr . FillTexture . Recommend . Last . SC . SomeColor
    fC = applyAttr . FillColor . Recommend . Last . SomeColor

getFillColor :: FillColor -> SomeColor
getFillColor (FillColor c) = getLast . getRecommend $ c

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors). See comment after 'fillColor' about backends.
fc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency). See comment after 'fillColor' about backends.
fcA :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
fcA = fillColor