{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Attributes
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered.  This module defines some common attributes relevant in
-- 3D; particular backends may also define more backend-specific
-- attributes.
--
-- Every attribute type must have a /semigroup/ structure, that is, an
-- associative binary operation for combining two attributes into one.
-- Unless otherwise noted, all the attributes defined here use the
-- 'Last' structure, that is, combining two attributes simply keeps
-- the second one and throws away the first.  This means that child
-- attributes always override parent attributes.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Attributes where

import           Control.Lens
import           Data.Semigroup
import           Data.Typeable

import           Data.Colour

import           Diagrams.Core

-- | @SurfaceColor@ is the inherent pigment of an object, assumed to
-- be opaque.
newtype SurfaceColor = SurfaceColor (Last (Colour Double))
  deriving (Typeable, Semigroup, Show)

instance AttributeClass SurfaceColor

_SurfaceColor :: Iso' SurfaceColor (Colour Double)
_SurfaceColor = iso (\(SurfaceColor (Last c)) -> c) (SurfaceColor . Last)

-- | Set the surface color.
sc :: HasStyle d => Colour Double -> d -> d
sc = applyAttr . review _SurfaceColor

-- | Lens onto the surface colour of a style.
_sc :: Lens' (Style v n) (Maybe (Colour Double))
_sc = atAttr . mapping _SurfaceColor

-- | @Diffuse@ is the fraction of incident light reflected diffusely,
-- that is, in all directions.  The actual light reflected is the
-- product of this value, the incident light, and the @SurfaceColor@
-- Attribute.  For physical reasonableness, @Diffuse@ should have a
-- value between 0 and 1; this is not checked.
newtype Diffuse = Diffuse (Last Double)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Diffuse

-- | Isomorphism between 'Diffuse' and 'Double'
_Diffuse :: Iso' Diffuse Double
_Diffuse = iso (\(Diffuse (Last d)) -> d) (Diffuse . Last)

-- | Set the diffuse reflectance.
diffuse :: HasStyle d => Double -> d -> d
diffuse = applyAttr . review _Diffuse

-- | Lens onto the possible diffuse reflectance in a style.
_diffuse :: Lens' (Style v n) (Maybe Double)
_diffuse = atAttr . mapping _Diffuse

-- | @Ambient@ is an ad-hoc representation of indirect lighting.  The
-- product of @Ambient@ and @SurfaceColor@ is added to the light
-- leaving an object due to diffuse and specular terms.  @Ambient@ can
-- be set per-object, and can be loosely thought of as the product of
-- indirect lighting incident on that object and the diffuse
-- reflectance.
newtype Ambient = Ambient (Last Double)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Ambient

_Ambient :: Iso' Ambient Double
_Ambient = iso (\(Ambient (Last d)) -> d) (Ambient . Last)

-- | Set the emittance due to ambient light.
ambient :: HasStyle d => Double -> d -> d
ambient = applyAttr . review _Ambient

-- | Lens onto the possible ambience in a style.
_ambient :: Lens' (Style v n) (Maybe Double)
_ambient = atAttr . mapping _Ambient

-- | A specular highlight has two terms, the intensity, between 0 and
-- 1, and the size.  The highlight size is assumed to be the exponent
-- in a Phong shading model (though Backends are free to use a
-- different shading model).  In this model, reasonable values are
-- between 1 and 50 or so, with higher values for shinier objects.
-- Physically, the intensity and the value of @Diffuse@ must add up to
-- less than 1; this is not enforced.
data Specular = Specular
  { _specularIntensity :: Double
  , _specularSize      :: Double
  } deriving Show

makeLenses ''Specular

newtype Highlight = Highlight (Last Specular)
  deriving (Typeable, Semigroup, Show)

instance AttributeClass Highlight

_Highlight :: Iso' Highlight Specular
_Highlight = iso (\(Highlight (Last s)) -> s) (Highlight . Last)

-- | Set the specular highlight.
highlight :: HasStyle d => Specular -> d -> d
highlight = applyAttr . review _Highlight

-- | Lens onto the possible specular highlight in a style
_highlight :: Lens' (Style v n) (Maybe Specular)
_highlight = atAttr . mapping _Highlight

-- | Traversal over the highlight intensity of a style. If the style has
--   no 'Specular', setting this will do nothing.
highlightIntensity :: Traversal' (Style v n) Double
highlightIntensity = _highlight . _Just . specularSize

-- | Traversal over the highlight size in a style. If the style has no
--   'Specular', setting this will do nothing.
highlightSize :: Traversal' (Style v n) Double
highlightSize = _highlight . _Just . specularSize
