{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes
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
  , GradientStop, SpreadMethod(..), lineLGradient

  -- ** Linear Gradients
  , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
  , lGradSpreadMethod, mkLinearGradient

  -- ** Radial Gradients
  , RGradient(..), rGradStops, rGradTrans, rGradRadius, rGradCenter, rGradFocus
  , rGradSpreadMethod, mkRadialGradient

  -- * Line texture
  ,  LineTexture(..), getLineTexture, lineTexture

  -- * Line color
  , lineColorT, lcT, lcAT

  -- * Fill texture
  , FillTexture(..), getFillTexture, fillTexture

  -- * Fill color
  , fillColorT, fcT, fcAT, recommendFillColorT

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
type GradientStop = (SomeColor, Double, Double)

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

mkStops :: Color c => [(c, Double, Double)] -> [GradientStop]
mkStops s = map (\(x, y, z) -> (SomeColor x, y, z)) s

mkLinearGradient :: [GradientStop]  -> P2 -> P2 -> SpreadMethod -> Texture
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end (scaling 1) spreadMethod)

mkRadialGradient :: [GradientStop] -> Double -> P2 -> P2 -> SpreadMethod -> Texture
mkRadialGradient stops radius center focus spreadMethod
  = RG (RGradient stops radius center focus (scaling 1) spreadMethod)

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

lineColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
lineColorT c = lineTexture (SC (SomeColor c))

lcT :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
lcT = lineColorT

lcAT :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
lcAT = lineColorT

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

fillColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
fillColorT c = fillTexture (SC (SomeColor c))

recommendFillColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
recommendFillColorT = applyTAttr . FillTexture . Recommend . Last . SC . SomeColor

fcT :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
fcT = fillColorT

fcAT :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
fcAT = fillColorT