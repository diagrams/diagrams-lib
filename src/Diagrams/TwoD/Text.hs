{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Text
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Very basic text primitives along with associated attributes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Text (
  -- * Creating text diagrams
    Text(..), TextAlignment(..)
  , text, topLeftText, alignedText, baselineText

  -- * Text attributes
  -- ** Font family
  , Font(..), getFont, font
  -- ** Font size
  , FontSize(..), getFontSize, fontSizeM, fontSize
  , fontSizeN, fontSizeO, fontSizeL, fontSizeG
  -- ** Font slant
  , FontSlant(..), FontSlantA, getFontSlant, fontSlant, italic, oblique
  -- ** Font weight
  , FontWeight(..), FontWeightA, getFontWeight, fontWeight, bold
  ) where

import           Diagrams.Core
import           Diagrams.Core.Envelope   (pointEnvelope)
import           Diagrams.TwoD.Attributes (recommendFillColor)
import           Diagrams.TwoD.Types

import           Data.Colour
import           Data.Functor
import           Data.Typeable
import           Data.Default.Class
import           Data.Semigroup
import           Data.Monoid.Recommend

import           Linear.Affine

------------------------------------------------------------
-- Text diagrams
------------------------------------------------------------

-- | A text primitive consists of the string contents and alignment
--   specification, along with two transformations: the first
--   accumulates all transformations which have been applied to the
--   text; the second accumulates normalized, "anti-scaled" versions
--   of the transformations which have had their average scaling
--   component removed.
data Text n = Text (Transformation V2 n) (TextAlignment n) String
  deriving Typeable

type instance V (Text n) = V2
type instance N (Text n) = n

instance Floating n => Transformable (Text n) where
  transform t (Text tt a s) = Text (t <> tt <> t') a s
    where t' = scaling (1 / avgScale t)

instance Floating n => HasOrigin (Text n) where
  moveOriginTo p = translate (origin .-. p)

instance Floating n => Renderable (Text n) NullBackend where
  render _ _ = mempty

-- | @TextAlignment@ specifies the alignment of the text's origin.
data TextAlignment n = BaselineText | BoxAlignedText n n

mkText :: (TypeableFloat n, Renderable (Text n) b)
  => TextAlignment n -> String -> Diagram b V2 n
mkText a t = recommendFillColor (black :: Colour Double)
             -- See Note [recommendFillColor]
           . recommendFontSize (local 1)
             -- See Note [recommendFontSize]

           $ mkQD (Prim $ Text mempty a t)
                       (pointEnvelope origin)
                       mempty
                       mempty
                       mempty

-- ~~~~ Note [recommendFillColor]

-- The reason we "recommend" a fill color of black instead of setting
-- it directly (or instead of simply not specifying a fill color at
-- all) was originally to support the SVG backend, though it is
-- actually in some sense the "right thing" to do, and other backends
-- we add later may conceivably need it as well.  The cairo backend
-- defaults happen to be to use a transparent fill for paths and a
-- black fill for text.  The SVG standard, however, specifies a
-- default fill of black for everything (both text and paths).  In
-- order to correctly render paths with no fill set, the SVG backend
-- must therefore explicitly set the fill to transparent -- but this
-- meant that it was also drawing text with a transparent fill.  The
-- solution is that we now explicitly inform all backends that the
-- *default* ("recommended") fill color for text should be black; an
-- absence of fill specification now consistently means to use a
-- "transparent" fill no matter what the primitive.  The reason we
-- need the special recommend/commit distinction is because if the
-- user explicitly sets a fill color later it should override this
-- recommendation; normally, the innermost occurrence of an attribute
-- would override all outer occurrences.
--
-- The reason we "recommend" a fill color of black instead of setting
-- it directly (or instead of simply not specifying a fill color at

-- | Create a primitive text diagram from the given string, with center
--   alignment, equivalent to @'alignedText' 0.5 0.5@.
--
--   Note that it /takes up no space/, as text size information is not
--   available.
text :: TypeableFloat n => (Renderable (Text n) b) => String -> Diagram b V2 n
text = alignedText 0.5 0.5

-- | Create a primitive text diagram from the given string, origin at
--   the top left corner of the text's bounding box, equivalent to
--   @'alignedText' 0 1@.
--
--   Note that it /takes up no space/.
topLeftText :: TypeableFloat n => (Renderable (Text n) b) => String -> Diagram b V2 n
topLeftText = alignedText 0 1

-- | Create a primitive text diagram from the given string, with the
--   origin set to a point interpolated within the bounding box.  The
--   first parameter varies from 0 (left) to 1 (right), and the second
--   parameter from 0 (bottom) to 1 (top).
--
--   The height of this box is determined by the font's potential ascent
--   and descent, rather than the height of the particular string.
--
--   Note that it /takes up no space/.
alignedText :: (TypeableFloat n, Renderable (Text n) b)
  => n -> n -> String -> Diagram b V2 n
alignedText w h = mkText (BoxAlignedText w h)

-- | Create a primitive text diagram from the given string, with the
--   origin set to be on the baseline, at the beginning (although not
--   bounding).  This is the reference point of showText in the Cairo
--   graphics library.
--
--   Note that it /takes up no space/.
baselineText :: (TypeableFloat n, Renderable (Text n) b)
  => String -> Diagram b V2 n
baselineText = mkText BaselineText

------------------------------------------------------------
-- Text attributes
------------------------------------------------------------

--------------------------------------------------
-- Font family

-- | The @Font@ attribute specifies the name of a font family.  Inner
--   @Font@ attributes override outer ones.
newtype Font = Font (Last String)
  deriving (Typeable, Semigroup, Eq)

instance AttributeClass Font

-- | Extract the font family name from a @Font@ attribute.
getFont :: Font -> String
getFont (Font (Last f)) = f

-- | Specify a font family to be used for all text within a diagram.
font :: HasStyle a => String -> a -> a
font = applyAttr . Font . Last

--------------------------------------------------
-- Font size

-- | The @FontSize@ attribute specifies the size of a font's
--   em-square.  Inner @FontSize@ attributes override outer ones.
newtype FontSize n = FontSize (Recommend (Last n))
  deriving (Typeable, Semigroup)

-- not sure why this can't be derived
instance Functor FontSize where
  fmap f (FontSize (Recommend (Last a))) = FontSize (Recommend (Last (f a)))
  fmap f (FontSize (Commit (Last a)))    = FontSize (Commit (Last (f a)))

-- (Recommend (Last (Texture n)))

type FontSizeM n = Measured n (FontSize n)

instance Typeable n => AttributeClass (FontSize n)

instance Num n => Default (FontSizeM n) where
  def = FontSize . Recommend . Last <$> local 1

-- | Extract the size from a @FontSize@ attribute.
getFontSize :: FontSize n -> n
getFontSize (FontSize (Recommend (Last s))) = s
getFontSize (FontSize (Commit (Last s)))    = s

-- | Set the font size, that is, the size of the font's em-square as
--   measured within the current local vector space.  The default size
--   is @1@.
fontSize :: (N a ~ n, Typeable n, HasStyle a) => Measure n -> a -> a
fontSize = applyMAttr . fmap (FontSize . Commit . Last)

-- | A convenient synonym for 'fontSize (Global w)'.
fontSizeG :: (N a ~ n, Typeable n, Num n, HasStyle a) => n -> a -> a
fontSizeG = fontSize . global

-- | A convenient synonym for 'fontSize (Normalized w)'.
fontSizeN :: (N a ~ n, Typeable n, Num n, HasStyle a) => n -> a -> a
fontSizeN = fontSize . normalized

-- | A convenient synonym for 'fontSize (Output w)'.
fontSizeO :: (N a ~ n, Typeable n, Num n, HasStyle a) => n -> a -> a
fontSizeO = fontSize . output

-- | A convenient sysnonym for 'fontSize (Local w)'.
fontSizeL :: (N a ~ n, Typeable n, Num n, HasStyle a) => n -> a -> a
fontSizeL = fontSize . local

-- | Apply a 'FontSize' attribute.
fontSizeM :: (N a ~ n, Typeable n, Num n, HasStyle a) => FontSizeM n -> a -> a
fontSizeM = applyMAttr

recommendFontSize :: (N a ~ n, Typeable n, HasStyle a) => Measure n -> a -> a
recommendFontSize = applyMAttr . fmap (FontSize . Recommend . Last)

--------------------------------------------------
-- Font slant

data FontSlant = FontSlantNormal
               | FontSlantItalic
               | FontSlantOblique
    deriving (Eq, Show)

-- | The @FontSlantA@ attribute specifies the slant (normal, italic,
--   or oblique) that should be used for all text within a diagram.
--   Inner @FontSlantA@ attributes override outer ones.
newtype FontSlantA = FontSlantA (Last FontSlant)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass FontSlantA

-- | Extract the font slant from a 'FontSlantA' attribute.
getFontSlant :: FontSlantA -> FontSlant
getFontSlant (FontSlantA (Last s)) = s

-- | Specify the slant (normal, italic, or oblique) that should be
--   used for all text within a diagram.  See also 'italic' and
--   'oblique' for useful special cases.
fontSlant :: HasStyle a => FontSlant -> a -> a
fontSlant = applyAttr . FontSlantA . Last

-- | Set all text in italics.
italic :: HasStyle a => a -> a
italic = fontSlant FontSlantItalic

-- | Set all text using an oblique slant.
oblique :: HasStyle a => a -> a
oblique = fontSlant FontSlantOblique

--------------------------------------------------
-- Font weight

data FontWeight = FontWeightNormal
                | FontWeightBold
    deriving (Eq, Show)

-- | The @FontWeightA@ attribute specifies the weight (normal or bold)
--   that should be used for all text within a diagram.  Inner
--   @FontWeightA@ attributes override outer ones.
newtype FontWeightA = FontWeightA (Last FontWeight)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass FontWeightA

-- | Extract the font weight from a 'FontWeightA' attribute.
getFontWeight :: FontWeightA -> FontWeight
getFontWeight (FontWeightA (Last w)) = w

-- | Specify the weight (normal or bold) that should be
--   used for all text within a diagram.  See also 'bold'
--   for a useful special case.
fontWeight :: HasStyle a => FontWeight -> a -> a
fontWeight = applyAttr . FontWeightA . Last

-- | Set all text using a bold font weight.
bold :: HasStyle a => a -> a
bold = fontWeight FontWeightBold
