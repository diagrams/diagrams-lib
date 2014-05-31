{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

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
    -- ** Width
    LineWidth, getLineWidth, lineWidth, lineWidthA
  , lw, lwN, lwO, lwL, lwG
  , ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick, none
  , tiny, verySmall, small, normal, large, veryLarge, huge

    -- ** Dashing
  , Dashing(..), DashingA, getDashing
  , dashing, dashingN, dashingO, dashingL, dashingG

  -- * Textures
  , Texture(..), solid, _SC, _LG, _RG, defaultLG, defaultRG
  , GradientStop(..), stopColor, stopFraction, mkStops
  , SpreadMethod(..), lineLGradient, lineRGradient

  -- ** Linear Gradients
  , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
  , lGradSpreadMethod, mkLinearGradient

  -- ** Radial Gradients
  , RGradient(..), rGradStops, rGradTrans
  , rGradCenter0, rGradRadius0, rGradCenter1, rGradRadius1
  , rGradSpreadMethod, mkRadialGradient

  -- ** Line texture
  ,  LineTexture(..), getLineTexture, lineTexture, lineTextureA
  ,  mkLineTexture, styleLineTexture

  -- ** Line color
  , lineColor, lc, lcA

  -- ** Fill texture
  , FillTexture(..), getFillTexture, fillTexture
  , mkFillTexture, styleFillTexture

  -- ** Fill color
  , fillColor, fc, fcA, recommendFillColor

  -- * Compilation utilities
  , splitTextureFills

  ) where

import           Diagrams.Core
import           Diagrams.Core.Style         (setAttr)
import           Diagrams.Attributes
import           Diagrams.Attributes.Compile
import           Diagrams.TwoD.Types

import           Diagrams.Core.Types         (RTree)
import           Diagrams.Located            (unLoc)
import           Diagrams.Path               (Path, pathTrails)
import           Diagrams.Trail              (isLoop)

import           Control.Lens ( makeLensesWith, generateSignatures, lensRules
                              , makePrisms, Lens', (&), (%~), (.~), Setter', sets)

import           Data.Colour hiding (AffineSpace)
import           Data.Data
import           Data.Default.Class
import           Data.Maybe                  (fromMaybe)

import           Data.Monoid.Recommend
import           Data.Semigroup
import           Data.VectorSpace

-- | Standard 'Measures'.
none, ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick,
  tiny, verySmall, small, normal, large, veryLarge, huge :: (Floating d) => MeasureX d
none       = Output 0
ultraThin  = Normalized 0.0005 `atLeast` Output 0.5
veryThin   = Normalized 0.001  `atLeast` Output 0.5
thin       = Normalized 0.002  `atLeast` Output 0.5
medium     = Normalized 0.004  `atLeast` Output 0.5
thick      = Normalized 0.0075 `atLeast` Output 0.5
veryThick  = Normalized 0.01   `atLeast` Output 0.5
ultraThick = Normalized 0.02   `atLeast` Output 0.5

tiny      = Normalized 0.01
verySmall = Normalized 0.015
small     = Normalized 0.023
normal    = Normalized 0.035
large     = Normalized 0.05
veryLarge = Normalized 0.07
huge      = Normalized 0.10

-----------------------------------------------------------------
--  Line Width  -------------------------------------------------
-----------------------------------------------------------------

-- | Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth v = LineWidth (Last (Measure v))
 deriving (Typeable, Semigroup)

deriving instance (Data (Scalar v), Data v) => Data (LineWidth v)
instance (Typeable v) => AttributeClass (LineWidth v)

type instance V (LineWidth v) = v

instance (R2Ish v) => Transformable (LineWidth v) where
  transform t (LineWidth (Last w)) =
    LineWidth (Last (transform (scaling (avgScale t)) w))

instance (R2Ish v) => Default (LineWidth v) where
    def = LineWidth (Last medium)

getLineWidth :: (R2Ish v) => LineWidth v -> Measure v
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: (R2Ish v, HasStyle a, V a ~ v) => Measure v -> a -> a
lineWidth = applyGTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA :: (R2Ish v, HasStyle a, V a ~ v) => LineWidth v -> a -> a
lineWidthA = applyGTAttr

-- | Default for 'lineWidth'.
lw :: (R2Ish v, HasStyle a, V a ~ v) => Measure v -> a -> a
lw = lineWidth

-- | A convenient synonym for 'lineWidth (Global w)'.
lwG :: (R2Ish v, HasStyle a, V a ~ v) => Scalar v -> a -> a
lwG w = lineWidth (Global w)

-- | A convenient synonym for 'lineWidth (Normalized w)'.
lwN :: (R2Ish v, HasStyle a, V a ~ v) => Scalar v -> a -> a
lwN w = lineWidth (Normalized w)

-- | A convenient synonym for 'lineWidth (Output w)'.
lwO :: (R2Ish v, HasStyle a, V a ~ v) => Scalar v -> a -> a
lwO w = lineWidth (Output w)

-- | A convenient sysnonym for 'lineWidth (Local w)'.
lwL :: (R2Ish v, HasStyle a, V a ~ v) => Scalar v -> a -> a
lwL w = lineWidth (Local w)

-----------------------------------------------------------------
--  Dashing  ----------------------------------------------------
-----------------------------------------------------------------

-- | Create lines that are dashing... er, dashed.
data Dashing v = Dashing [Measure v] (Measure v)
  deriving (Typeable)

deriving instance (Data (Scalar v), Data v) => Data (Dashing v)
deriving instance Eq (Scalar v) => Eq (Dashing v)

newtype DashingA v = DashingA (Last (Dashing v))
  deriving (Typeable, Semigroup)

deriving instance (Data (Scalar v), Data v) => Data (DashingA v)
deriving instance Eq (Scalar v) => Eq (DashingA v)

instance (Typeable v) => AttributeClass (DashingA v)

type instance V (DashingA v) = v

instance (R2Ish v) => Transformable (DashingA v) where
  transform t (DashingA (Last (Dashing w v))) =
    DashingA (Last (Dashing r s))
    where
      t' = scaling (avgScale t)
      r = map (transform t') w
      s = transform t' v

getDashing :: (R2Ish v) => DashingA v -> Dashing v
getDashing (DashingA (Last d)) = d

-- | Set the line dashing style.
dashing :: (R2Ish v, HasStyle a, V a ~ v) =>
           [Measure v]  -- ^ A list specifying alternate lengths of on
                         --   and off portions of the stroke.  The empty
                         --   list indicates no dashing.
        -> Measure v    -- ^ An offset into the dash pattern at which the
                         --   stroke should start.
        -> a -> a
dashing ds offs = applyGTAttr (DashingA (Last (Dashing ds offs)))

-- | A convenient synonym for 'dashing (Global w)'.
dashingG :: (R2Ish v, HasStyle a, V a ~ v) => [Scalar v] -> Scalar v -> a -> a
dashingG w v = dashing (map Global w) (Global v)

-- | A convenient synonym for 'dashing (Normalized w)'.
dashingN :: (R2Ish v, HasStyle a, V a ~ v) => [Scalar v] -> Scalar v -> a -> a
dashingN w v = dashing (map Normalized w) (Normalized v)

-- | A convenient synonym for 'dashing (Output w)'.
dashingO :: (R2Ish v, HasStyle a, V a ~ v) => [Scalar v] -> Scalar v -> a -> a
dashingO w v = dashing (map Output w) (Output v)

-- | A convenient sysnonym for 'dashing (Local w)'.
dashingL :: (R2Ish v, HasStyle a, V a ~ v) => [Scalar v] -> Scalar v -> a -> a
dashingL w v = dashing (map Local w) (Local v)

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
data LGradient v = LGradient
    { _lGradStops        :: [GradientStop]
    , _lGradStart        :: Point v
    , _lGradEnd          :: Point v
    , _lGradTrans        :: Transformation v
    , _lGradSpreadMethod :: SpreadMethod }

makeLensesWith (lensRules & generateSignatures .~ False) ''LGradient

-- | A list of stops (colors and fractions).
lGradStops :: (R2Ish v) => Lens' (LGradient v) [GradientStop]

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
lGradTrans :: (R2Ish v) => Lens' (LGradient v) (Transformation v)

-- | The starting point for the first gradient stop. The coordinates are in
--   'Local' units and the default is (-0.5, 0).
lGradStart :: (R2Ish v) => Lens' (LGradient v) (Point v)

-- | The ending point for the last gradient stop.The coordinates are in
--   'Local' units and the default is (0.5, 0).
lGradEnd :: (R2Ish v) => Lens' (LGradient v) (Point v)

-- | For setting the spread method.
lGradSpreadMethod :: (R2Ish v) => Lens' (LGradient v) SpreadMethod

-- | Radial Gradient
data RGradient v = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradCenter0      :: Point v
    , _rGradRadius0      :: Scalar v
    , _rGradCenter1      :: Point v
    , _rGradRadius1      :: Scalar v
    , _rGradTrans        :: Transformation v
    , _rGradSpreadMethod :: SpreadMethod }

makeLensesWith (lensRules & generateSignatures .~ False) ''RGradient

-- | A list of stops (colors and fractions).
rGradStops :: (R2Ish v) => Lens' (RGradient v) [GradientStop]

-- | The center point of the inner circle.
rGradCenter0 :: (R2Ish v) => Lens' (RGradient v) (Point v)

-- | The radius of the inner cirlce in 'Local' coordinates.
rGradRadius0 :: (R2Ish v) => Lens' (RGradient v) (Scalar v)

-- | The center of the outer circle.
rGradCenter1  :: (R2Ish v) => Lens' (RGradient v) (Point v)

-- | The radius of the outer circle in 'Local' coordinates.
rGradRadius1 :: (R2Ish v) => Lens' (RGradient v) (Scalar v)

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
rGradTrans :: (R2Ish v) => Lens' (RGradient v) (Transformation v)

-- | For setting the spread method.
rGradSpreadMethod :: (R2Ish v) => Lens' (RGradient v) SpreadMethod

-- | A Texture is either a color 'SC', linear gradient 'LG', or radial gradient 'RG'.
--   An object can have only one texture which is determined by the 'Last'
--   semigroup structure.
data Texture v = SC SomeColor | LG (LGradient v) | RG (RGradient v)
  deriving (Typeable)

makePrisms ''Texture

-- | Convert a solid colour into a texture.
solid :: (R2Ish v) => Color a => a -> Texture v
solid = SC . SomeColor

-- | A default is provided so that linear gradients can easily be created using
--   lenses. For example, @lg = defaultLG & lGradStart .~ (0.25 ^& 0.33)@. Note that
--   no default value is provided for @lGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultLG :: (R2Ish v) => Texture v
defaultLG = LG (LGradient
    { _lGradStops        = []
    , _lGradStart        = mkP2 (-0.5) 0
    , _lGradEnd          = mkP2 (0.5)  0
    , _lGradTrans        = mempty
    , _lGradSpreadMethod = GradPad
    })

-- | A default is provided so that radial gradients can easily be created using
--   lenses. For example, @rg = defaultRG & rGradRadius1 .~ 0.25@. Note that
--   no default value is provided for @rGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultRG :: (R2Ish v) => Texture v
defaultRG = RG (RGradient
    { _rGradStops        = []
    , _rGradCenter0      = mkP2 0 0
    , _rGradRadius0      = 0.0
    , _rGradCenter1      = mkP2 0 0
    , _rGradRadius1      = 0.5
    , _rGradTrans        = mempty
    , _rGradSpreadMethod = GradPad
    })

-- | A convenient function for making gradient stops from a list of triples.
--   (An opaque color, a stop fraction, an opacity).
mkStops :: [(Colour Double, Double, Double)] -> [GradientStop]
mkStops = map (\(x, y, z) -> GradientStop (SomeColor (withOpacity x z)) y)

-- | Make a linear gradient texture from a stop list, start point, end point,
--   and 'SpreadMethod'. The 'lGradTrans' field is set to the identity
--   transfrom, to change it use the 'lGradTrans' lens.
mkLinearGradient :: (R2Ish v) => [GradientStop]  -> Point v -> Point v -> SpreadMethod -> Texture v
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end mempty spreadMethod)

-- | Make a radial gradient texture from a stop list, radius, start point,
--   end point, and 'SpreadMethod'. The 'rGradTrans' field is set to the identity
--   transfrom, to change it use the 'rGradTrans' lens.
mkRadialGradient :: (R2Ish v) => [GradientStop] -> Point v -> Scalar v
                  -> Point v -> Scalar v -> SpreadMethod -> Texture v
mkRadialGradient stops c0 r0 c1 r1 spreadMethod
  = RG (RGradient stops c0 r0 c1 r1 mempty spreadMethod)

-- | The texture with which lines are drawn.  Note that child
--   textures always override parent textures.
--   More precisely, the semigroup structure on line texture attributes
--   is that of 'Last'.
newtype LineTexture v = LineTexture (Last (Texture v))
  deriving (Typeable, Semigroup)
instance (Typeable v) => AttributeClass (LineTexture v)

type instance V (LineTexture v) = v

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance (R2Ish v) => Transformable (LineTexture v) where
  transform t (LineTexture (Last texture)) = LineTexture (Last tx)
    where
      tx = texture & lgt . rgt
      lgt = _LG . lGradTrans %~ f
      rgt = _RG . rGradTrans %~ f
      f = transform t

instance Default (LineTexture v) where
    def = LineTexture (Last (SC (SomeColor (black :: Colour Double))))

getLineTexture :: (R2Ish v) => LineTexture v -> Texture v
getLineTexture (LineTexture (Last t)) = t

lineTexture :: (R2Ish v, HasStyle a, V a ~ v) => Texture v -> a -> a
lineTexture = applyTAttr . LineTexture . Last

lineTextureA :: (R2Ish v, HasStyle a, V a ~ v) => LineTexture v -> a -> a
lineTextureA = applyTAttr

mkLineTexture :: (R2Ish v) => Texture v -> LineTexture v
mkLineTexture = LineTexture . Last

styleLineTexture :: (R2Ish v) => Setter' (Style v) (Texture v)
styleLineTexture = sets modifyLineTexture
  where
    modifyLineTexture f s
      = flip setAttr s
      . mkLineTexture
      . f
      . getLineTexture
      . fromMaybe def . getAttr
      $ s

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (R2Ish v, Color c, HasStyle a, V a ~ v) => c -> a -> a
lineColor = lineTexture . SC . SomeColor

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).  See comment in 'lineColor' about backends.
lc :: (R2Ish v, HasStyle a, V a ~ v) => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).  See comment in 'lineColor'
--   about backends.
lcA :: (R2Ish v, HasStyle a, V a ~ v) => AlphaColour Double -> a -> a
lcA = lineColor

-- | Apply a linear gradient.
lineLGradient :: (R2Ish v, HasStyle a, V a ~ v) => LGradient v -> a -> a
lineLGradient g = lineTexture (LG g)

-- | Apply a radial gradient.
lineRGradient :: (R2Ish v, HasStyle a, V a ~ v) => RGradient v -> a -> a
lineRGradient g = lineTexture (RG g)

-- | The texture with which objects are filled.
--   The semigroup structure on fill texture attributes
--   is that of 'Recommed . Last'.
newtype FillTexture v = FillTexture (Recommend (Last (Texture v)))
  deriving (Typeable, Semigroup)

instance (Typeable v) => AttributeClass (FillTexture v)

type instance V (FillTexture v) = v

-- Only gradients get transformed. The transform is applied to the gradients
-- transform field. Colors are left unchanged.
instance (R2Ish v) => Transformable (FillTexture v) where
  transform _ tx@(FillTexture (Recommend _)) = tx
  transform t (FillTexture (Commit (Last texture))) = FillTexture (Commit (Last tx))
    where
      tx = texture & lgt . rgt
      lgt = _LG . lGradTrans %~ f
      rgt = _RG . rGradTrans %~ f
      f = transform t

instance (R2Ish v) => Default (FillTexture v) where
    def = FillTexture (Recommend (Last (SC
                      (SomeColor (transparent :: AlphaColour Double)))))

getFillTexture :: (R2Ish v) => FillTexture v -> Texture v
getFillTexture (FillTexture tx) = getLast . getRecommend $ tx

fillTexture :: (R2Ish v, HasStyle a, V a ~ v) => Texture v -> a -> a
fillTexture = applyTAttr . FillTexture . Commit . Last

mkFillTexture :: (R2Ish v) => Texture v  -> FillTexture v
mkFillTexture = FillTexture . Commit . Last

styleFillTexture :: (R2Ish v) => Setter' (Style v) (Texture v)
styleFillTexture = sets modifyFillTexture
  where
    modifyFillTexture f s
      = flip setAttr s
      . mkFillTexture
      . f
      . getFillTexture
      . fromMaybe def . getAttr
      $ s

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (R2Ish v, Color c, HasStyle a, V a ~ v) => c -> a -> a
fillColor = fillTexture . SC . SomeColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
--   See comment after 'fillColor' about backends.
recommendFillColor :: (R2Ish v, Color c, HasStyle a, V a ~ v) => c -> a -> a
recommendFillColor =
  applyTAttr . FillTexture . Recommend . Last . SC . SomeColor

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors). See comment after 'fillColor' about backends.
fc :: (R2Ish v, HasStyle a, V a ~ v) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency). See comment after 'fillColor' about backends.
fcA :: (R2Ish v, HasStyle a, V a ~ v) => AlphaColour Double -> a -> a
fcA = fillColor
------------------------------------------------------------

data FillTextureLoops v = FillTextureLoops

instance Typeable v => SplitAttribute (FillTextureLoops v) where
  type AttrType (FillTextureLoops v) = FillTexture v
  type PrimType (FillTextureLoops v) = Path v

  primOK _ = all (isLoop . unLoc) . pathTrails

-- | Push fill attributes down until they are at the root of subtrees
--   containing only loops. This makes life much easier for backends,
--   which typically have a semantics where fill attributes are
--   applied to lines/non-closed paths as well as loops/closed paths,
--   whereas in the semantics of diagrams, fill attributes only apply
--   to loops.
splitTextureFills :: forall b v a. Typeable v => RTree b v a -> RTree b v a
splitTextureFills = splitAttr (FillTextureLoops :: FillTextureLoops v)
