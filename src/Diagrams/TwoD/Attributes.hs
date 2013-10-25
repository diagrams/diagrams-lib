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
-- rendered.  This module defines Gradients and Colors in TwoD.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Attributes (

  -- * Gradient
    GradientStop, SpreadMethod(..)
  , LGradient(..), lGradStops, lGradStart, lGradEnd, lGradSpreadMethod
  , RGradient(..), rGradStops, rGradRadius, rGradCenter, rGradFocus, rGradSpreadMethod
  , lineLGradient, lineRGradient

  -- * Texture
  ,  Texture(..), _SC, _LG, _RG

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
import           Diagrams.TwoD.Types (R2)

import           Control.Lens (makeLenses, makePrisms, (&), (%~))

import           Data.Colour hiding (AffineSpace)

import           Data.Default.Class

import           Data.Typeable

import           Data.Monoid.Recommend
import           Data.Semigroup

type GradientStop = (SomeColor, Double)

data SpreadMethod = GradPad | GradReflect | GradRepeat

-- | Linear Gradient
data LGradient = LGradient
    { _lGradStops        :: [GradientStop]
    , _lGradStart        :: R2
    , _lGradEnd          :: R2
    , _lGradSpreadMethod :: SpreadMethod }

makeLenses ''LGradient

-- | Radial Gradient
data RGradient = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradRadius       :: Double
    , _rGradCenter       :: R2
    , _rGradFocus        :: R2
    , _rGradSpreadMethod :: SpreadMethod }

makeLenses ''RGradient

data Texture = SC SomeColor | LG LGradient | RG RGradient
  deriving (Typeable)

makePrisms ''Texture

-- | The texture with which lines (strokes) are drawn.  Note that child
--   textures always override parent textures; that is, @'lineTexture' t1
--   . 'lineTexture' t2 $ d@ is equivalent to @'lineTexture' t2 $ d@.
--   More precisely, the semigroup structure on line texture attributes
--   is that of 'Last'.
newtype LineTexture = LineTexture (Last Texture)
  deriving (Typeable, Semigroup)
instance AttributeClass LineTexture

type instance V LineTexture = R2

instance Transformable LineTexture where
  transform t (LineTexture (Last texture)) = LineTexture (Last tx)
    where
      tx = texture & lgS . lgE . rgC . rgF
      lgS = _LG . lGradStart  %~ f
      lgE = _LG . lGradEnd    %~ f
      rgC = _RG . rGradCenter %~ f
      rgF = _RG . rGradFocus  %~ f
      f   = transform t

instance Default LineTexture where
    def = LineTexture (Last (SC (SomeColor (black :: Colour Double))))

getLineTexture :: LineTexture -> Texture
getLineTexture (LineTexture (Last t)) = t

lineTexture :: (HasStyle a, V a ~ R2) => Texture-> a -> a
lineTexture = applyTAttr . LineTexture . Last

lineColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
lineColorT c = lineTexture (SC (SomeColor c))

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
lcT :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
lcT = lineColorT

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
lcAT :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
lcAT = lineColorT

lineLGradient :: (HasStyle a, V a ~ R2) => LGradient -> a -> a
lineLGradient g = lineTexture (LG g)

lineRGradient :: (HasStyle a, V a ~ R2) => RGradient -> a -> a
lineRGradient g = lineTexture (RG g)

-- | The texture with which shapes are filled. Note that child
--   textures always override parent colors; that is, @'fillTexture' t1
--   . 'fillTexture' t2 $ d@ is equivalent to @'fillTexture' t2 $ d@.
--   More precisely, the semigroup structure on fill textureå attributes
--   is that of 'Last'.
newtype FillTexture = FillTexture (Recommend (Last Texture))
  deriving (Typeable, Semigroup)
instance AttributeClass FillTexture

type instance V FillTexture = R2

-- Recommended textures are assumed to colors and are therefore not transformed.
-- For committed textures the transform is applied to the R2 elements of the
-- gradient records.
instance Transformable FillTexture where
  transform _ tx@(FillTexture (Recommend _)) = tx
  transform t (FillTexture (Commit (Last texture))) = FillTexture (Commit (Last tx))
    where
      tx = texture & lgS . lgE . rgC . rgF
      lgS = _LG . lGradStart  %~ f
      lgE = _LG . lGradEnd    %~ f
      rgC = _RG . rGradCenter %~ f
      rgF = _RG . rGradFocus  %~ f
      f   = transform t

getFillTexture :: FillTexture -> Texture
getFillTexture (FillTexture tx) = getLast . getRecommend $ tx

fillTexture :: (HasStyle a, V a ~ R2) => Texture -> a -> a
fillTexture = applyTAttr . FillTexture . Commit . Last

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fcT' and 'fcAT' variants are provided with more concrete types.
fillColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
fillColorT c = fillTexture (SC (SomeColor c))

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColorT' (or 'fcT', or 'fcAT') are used.
recommendFillColorT :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
recommendFillColorT = applyTAttr . FillTexture . Recommend . Last . SC . SomeColor

-- | A synonym for 'fillColorT', specialized to @'Colour' Double@
--   (i.e. opaque colors).
fcT :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
fcT= fillColorT

-- | A synonym for 'fillColorT', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
fcAT :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
fcAT = fillColorT