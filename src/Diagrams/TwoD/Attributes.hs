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
-- rendered. This module defines Gradients and Colors (Textures) in two
-- dimensions. Some of these functions are carbon copies of funtions defined
-- in Diagrams.Attributes, provided for backward compatability. Functions
-- ending in T like /fcT/ have counterparts without the T, e.g. /fc/.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Attributes (
  -- * Gradients
    Texture(..), _SC, _LG, _RG, defaultLG, defaultRG, mkStops, idTransform
  , GradientStop(..), SpreadMethod(..), lineLGradient, lineRGradient
  , stopColor, stopFraction

  -- ** Linear Gradients
  , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
  , lGradSpreadMethod, mkLinearGradient

  -- ** Radial Gradients
  , RGradient(..), rGradStops, rGradTrans, rGradRadius, rGradCenter, rGradFocus
  , rGradSpreadMethod, mkRadialGradient

  -- * Line texture
  ,  LineTexture(..), getLineTexture, lineTexture

  -- * Line color
  , LineColor, lineColor, getLineColor, lc, lcA, lineColorA

  -- * Fill texture
  , FillTexture(..), getFillTexture, fillTexture

  -- * Fill color
  , FillColor, fillColor, getFillColor, fc, fcA, recommendFillColor

  ) where

import           Diagrams.Core
import           Diagrams.Attributes (Color(..), SomeColor(..))
import           Diagrams.TwoD.Types (T2, R2, P2, mkP2)

import           Control.Lens (makeLenses, makePrisms, (&), (%~))

import           Data.Colour hiding (AffineSpace)
import           Data.Default.Class
import           Data.Typeable

import           Data.Monoid.Recommend
import           Data.Semigroup

-- | A stop is (color, proportion, opacity)
--type GradientStop = (SomeColor, Double, Double)
data GradientStop = GradientStop
     { _stopColor    :: SomeColor
     , _stopFraction :: Double}

makeLenses ''GradientStop

data SpreadMethod = GradPad | GradReflect | GradRepeat

-- | Linear Gradient
data LGradient = LGradient
    { _lGradStops        :: [GradientStop]
    , _lGradStart        :: P2
    , _lGradEnd          :: P2
    , _lGradTrans        :: T2
    , _lGradSpreadMethod :: SpreadMethod }

makeLenses ''LGradient

-- | Radial Gradient
data RGradient = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradRadius       :: Double
    , _rGradCenter       :: P2
    , _rGradFocus        :: P2
    , _rGradTrans        :: T2
    , _rGradSpreadMethod :: SpreadMethod }

makeLenses ''RGradient

data Texture = SC SomeColor | LG LGradient | RG RGradient
  deriving (Typeable)

makePrisms ''Texture

-- XXX replace with a general version of identity transform in core.
idTransform :: Transformation R2
idTransform = scaling 1

defaultLG :: Texture
defaultLG = LG (LGradient
    { _lGradStops        = []
    , _lGradStart        = mkP2 0 0
    , _lGradEnd          = mkP2 1 0
    , _lGradTrans        = scaling 1
    , _lGradSpreadMethod = GradPad
    })

defaultRG :: Texture
defaultRG = RG (RGradient
    { _rGradStops        = []
    , _rGradRadius       = 0.5
    , _rGradCenter       = mkP2 0 0
    , _rGradFocus        = mkP2 0 0
    , _rGradTrans        = scaling 1
    , _rGradSpreadMethod = GradPad
    })

mkStops :: [(Colour Double, Double, Double)] -> [GradientStop]
mkStops s = map (\(x, y, z) -> GradientStop (SomeColor (withOpacity x z)) y) s

mkLinearGradient :: [GradientStop]  -> P2 -> P2 -> SpreadMethod -> Texture
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end (scaling 1) spreadMethod)

mkRadialGradient :: [GradientStop] -> Double -> P2 -> P2 -> SpreadMethod -> Texture
mkRadialGradient stops r center focus spreadMethod
  = RG (RGradient stops r center focus (scaling 1) spreadMethod)

newtype LineTexture = LineTexture (Last Texture)
  deriving (Typeable, Semigroup)
instance AttributeClass LineTexture

type instance V LineTexture = R2

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
--   concrete types.
lineColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
lineColor c = (lTx c) . (lCl c)
  where
    lTx x = lineTexture (SC (SomeColor x))
    lCl = applyAttr . LineColor . Last . SomeColor

-- | Apply a 'lineColor' attribute.
lineColorA :: HasStyle a => LineColor -> a -> a
lineColorA = applyAttr

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
lc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
lcA :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
lcA = lineColor

lineLGradient :: (HasStyle a, V a ~ R2) => LGradient -> a -> a
lineLGradient g = lineTexture (LG g)

lineRGradient :: (HasStyle a, V a ~ R2) => RGradient -> a -> a
lineRGradient g = lineTexture (RG g)

newtype FillTexture = FillTexture (Recommend (Last Texture))
  deriving (Typeable, Semigroup)

instance AttributeClass FillTexture

type instance V FillTexture = R2

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
fillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
fillColor c = (fTx c) . (fCl c)
  where
    fTx x = fillTexture (SC (SomeColor x))
    fCl  = applyAttr . FillColor . Commit . Last . SomeColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
recommendFillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
recommendFillColor c = (fT c) . (fC c)
  where
    fT = applyTAttr . FillTexture . Recommend . Last . SC . SomeColor
    fC = applyAttr . FillColor . Recommend . Last . SomeColor

getFillColor :: FillColor -> SomeColor
getFillColor (FillColor c) = getLast . getRecommend $ c

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
fc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
fcA :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
fcA = fillColor