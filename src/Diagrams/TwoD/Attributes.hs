{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
                              , makePrisms, Lens', (&), (%~), (.~), Setter', sets
                              , Wrapped(..), iso)

import           Data.Colour hiding (AffineSpace)
import           Data.Data
import           Data.Default.Class
import           Data.Maybe                  (fromMaybe)

import           Data.Monoid.Recommend
import           Data.Semigroup

-- | Standard 'Measures'.
none, ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick,
  tiny, verySmall, small, normal, large, veryLarge, huge :: Measure R2
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
newtype LineWidth = LineWidth (Last (Measure R2))
  deriving (Typeable, Data, Semigroup)
instance AttributeClass LineWidth

type instance V LineWidth = R2

instance Transformable LineWidth where
  transform t (LineWidth (Last w)) =
    LineWidth (Last (transform t w))

instance Default LineWidth where
    def = LineWidth (Last medium)

getLineWidth :: LineWidth -> Measure R2
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: (HasStyle a, V a ~ R2) => Measure R2 -> a -> a
lineWidth = applyGTAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  (HasStyle a, V a ~ R2) => LineWidth -> a -> a
lineWidthA = applyGTAttr

-- | Default for 'lineWidth'.
lw :: (HasStyle a, V a ~ R2) => Measure R2 -> a -> a
lw = lineWidth

-- | A convenient synonym for 'lineWidth (Global w)'.
lwG :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwG w = lineWidth (Global w)

-- | A convenient synonym for 'lineWidth (Normalized w)'.
lwN :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwN w = lineWidth (Normalized w)

-- | A convenient synonym for 'lineWidth (Output w)'.
lwO :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwO w = lineWidth (Output w)

-- | A convenient sysnonym for 'lineWidth (Local w)'.
lwL :: (HasStyle a, V a ~ R2) => Double -> a -> a
lwL w = lineWidth (Local w)

-----------------------------------------------------------------
--  Dashing  ----------------------------------------------------
-----------------------------------------------------------------

-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Measure R2] (Measure R2)
  deriving (Typeable, Data, Eq)

newtype DashingA = DashingA (Last Dashing)
  deriving (Typeable, Data, Semigroup, Eq)
instance AttributeClass DashingA

type instance V DashingA = R2

instance Transformable DashingA where
  transform t (DashingA (Last (Dashing w v))) =
    DashingA (Last (Dashing r s))
    where
      r = map (transform t) w
      s = transform t v

getDashing :: DashingA -> Dashing
getDashing (DashingA (Last d)) = d

-- | Set the line dashing style.
dashing :: (HasStyle a, V a ~ R2) =>
           [Measure R2]  -- ^ A list specifying alternate lengths of on
                         --   and off portions of the stroke.  The empty
                         --   list indicates no dashing.
        -> Measure R2    -- ^ An offset into the dash pattern at which the
                         --   stroke should start.
        -> a -> a
dashing ds offs = applyGTAttr (DashingA (Last (Dashing ds offs)))

-- | A convenient synonym for 'dashing (Global w)'.
dashingG :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingG w v = dashing (map Global w) (Global v)

-- | A convenient synonym for 'dashing (Normalized w)'.
dashingN :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingN w v = dashing (map Normalized w) (Normalized v)

-- | A convenient synonym for 'dashing (Output w)'.
dashingO :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
dashingO w v = dashing (map Output w) (Output v)

-- | A convenient sysnonym for 'dashing (Local w)'.
dashingL :: (HasStyle a, V a ~ R2) => [Double] -> Double -> a -> a
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

-- | The starting point for the first gradient stop. The coordinates are in
--   'Local' units and the default is (-0.5, 0).
lGradStart :: Lens' LGradient P2

-- | The ending point for the last gradient stop.The coordinates are in
--   'Local' units and the default is (0.5, 0).
lGradEnd :: Lens' LGradient P2

-- | For setting the spread method.
lGradSpreadMethod :: Lens' LGradient SpreadMethod

-- | Radial Gradient
data RGradient = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradCenter0      :: P2
    , _rGradRadius0      :: Double
    , _rGradCenter1      :: P2
    , _rGradRadius1      :: Double
    , _rGradTrans        :: T2
    , _rGradSpreadMethod :: SpreadMethod }

makeLensesWith (lensRules & generateSignatures .~ False) ''RGradient

-- | A list of stops (colors and fractions).
rGradStops :: Lens' RGradient [GradientStop]

-- | The center point of the inner circle.
rGradCenter0 :: Lens' RGradient P2

-- | The radius of the inner cirlce in 'Local' coordinates.
rGradRadius0 :: Lens' RGradient Double

-- | The center of the outer circle.
rGradCenter1  :: Lens' RGradient P2

-- | The radius of the outer circle in 'Local' coordinates.
rGradRadius1 :: Lens' RGradient Double

-- | A transformation to be applied to the gradient. Usually this field will
--   start as the identity transform and capture the transforms that are applied
--   to the gradient.
rGradTrans :: Lens' RGradient T2

-- | For setting the spread method.
rGradSpreadMethod :: Lens' RGradient SpreadMethod

-- | A Texture is either a color 'SC', linear gradient 'LG', or radial gradient 'RG'.
--   An object can have only one texture which is determined by the 'Last'
--   semigroup structure.
data Texture = SC SomeColor | LG LGradient | RG RGradient
  deriving (Typeable)

makePrisms ''Texture

-- | Convert a solid colour into a texture.
solid :: Color a => a -> Texture
solid = SC . SomeColor

-- | A default is provided so that linear gradients can easily be created using
--   lenses. For example, @lg = defaultLG & lGradStart .~ (0.25 ^& 0.33)@. Note that
--   no default value is provided for @lGradStops@, this must be set before
--   the gradient value is used, otherwise the object will appear transparent.
defaultLG :: Texture
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
defaultRG :: Texture
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
mkLinearGradient :: [GradientStop]  -> P2 -> P2 -> SpreadMethod -> Texture
mkLinearGradient stops  start end spreadMethod
  = LG (LGradient stops start end mempty spreadMethod)

-- | Make a radial gradient texture from a stop list, radius, start point,
--   end point, and 'SpreadMethod'. The 'rGradTrans' field is set to the identity
--   transfrom, to change it use the 'rGradTrans' lens.
mkRadialGradient :: [GradientStop] -> P2 -> Double
                  -> P2 -> Double -> SpreadMethod -> Texture
mkRadialGradient stops c0 r0 c1 r1 spreadMethod
  = RG (RGradient stops c0 r0 c1 r1 mempty spreadMethod)

-- | The texture with which lines are drawn.  Note that child
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

instance Wrapped LineTexture where
  type Unwrapped LineTexture = Texture
  _Wrapped' = iso getLineTexture mkLineTexture

getLineTexture :: LineTexture -> Texture
getLineTexture (LineTexture (Last t)) = t

lineTexture :: (HasStyle a, V a ~ R2) => Texture -> a -> a
lineTexture = applyTAttr . LineTexture . Last

lineTextureA :: (HasStyle a, V a ~ R2) => LineTexture -> a -> a
lineTextureA = applyTAttr

mkLineTexture :: Texture  -> LineTexture
mkLineTexture = LineTexture . Last

styleLineTexture :: Setter' (Style v) Texture
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
lineColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
lineColor = lineTexture . SC . SomeColor

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

-- | The texture with which objects are filled.
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

instance Default FillTexture where
    def = FillTexture (Recommend (Last (SC
                      (SomeColor (transparent :: AlphaColour Double)))))

getFillTexture :: FillTexture -> Texture
getFillTexture (FillTexture tx) = getLast . getRecommend $ tx

fillTexture :: (HasStyle a, V a ~ R2) => Texture -> a -> a
fillTexture = applyTAttr . FillTexture . Commit . Last

mkFillTexture :: Texture  -> FillTexture
mkFillTexture = FillTexture . Commit . Last

styleFillTexture :: Setter' (Style v) Texture
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
fillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
fillColor = fillTexture . SC . SomeColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
--   See comment after 'fillColor' about backends.
recommendFillColor :: (Color c, HasStyle a, V a ~ R2) => c -> a -> a
recommendFillColor =
  applyTAttr . FillTexture . Recommend . Last . SC . SomeColor

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors). See comment after 'fillColor' about backends.
fc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency). See comment after 'fillColor' about backends.
fcA :: (HasStyle a, V a ~ R2) => AlphaColour Double -> a -> a
fcA = fillColor
------------------------------------------------------------

data FillTextureLoops v = FillTextureLoops

instance Typeable v => SplitAttribute (FillTextureLoops v) where
  type AttrType (FillTextureLoops v) = FillTexture
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
