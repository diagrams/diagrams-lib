{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
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
  , FontSize(..), getFontSize, setFontSize, fontSizeA
  , fontSize, fontSizeN, fontSizeO, fontSizeL, fontSizeG
  -- ** Font slant
  , FontSlant(..), FontSlantA, getFontSlant, fontSlant, italic, oblique
  -- ** Font weight
  , FontWeight(..), FontWeightA, getFontWeight, fontWeight, bold
  ) where

import           Diagrams.Attributes
import           Diagrams.Core
import           Diagrams.Core.Envelope (pointEnvelope)
import           Diagrams.TwoD.Types

import           Data.AffineSpace       ((.-.))
import           Data.Colour
import           Data.Data
import           Data.Default.Class
import           Data.Semigroup

------------------------------------------------------------
-- Text diagrams
------------------------------------------------------------

-- | A text primitive consists of the string contents and alignment
--   specification, along with a transformation mapping from the local
--   vector space of the text to the vector space in which it is
--   embedded.
data Text = Text T2 TextAlignment String
  deriving Typeable

type instance V Text = R2

instance Transformable Text where
  transform t (Text tt a s) = Text (t <> tt <> t') a s
    where
      t' = scaling (1 / avgScale t)

instance HasOrigin Text where
  moveOriginTo p = translate (origin .-. p)

instance Renderable Text NullBackend where
  render _ _ = mempty

-- | @TextAlignment@ specifies the alignment of the text's origin.
data TextAlignment = BaselineText | BoxAlignedText Double Double

mkText :: Renderable Text b => TextAlignment -> String -> Diagram b R2
mkText a t = recommendFillColor (black :: Colour Double)
             -- See Note [recommendFillColor]

           $ mkQD (Prim (Text mempty a t))
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

-- | Create a primitive text diagram from the given string, with center
--   alignment, equivalent to @'alignedText' 0.5 0.5@.
--
--   Note that it /takes up no space/, as text size information is not
--   available.
text :: Renderable Text b => String -> Diagram b R2
text = alignedText 0.5 0.5

-- | Create a primitive text diagram from the given string, origin at
--   the top left corner of the text's bounding box, equivalent to
--   @'alignedText' 0 1@.
--
--   Note that it /takes up no space/.
topLeftText :: Renderable Text b => String -> Diagram b R2
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
alignedText :: Renderable Text b => Double -> Double -> String -> Diagram b R2
alignedText w h = mkText (BoxAlignedText w h)

-- | Create a primitive text diagram from the given string, with the
--   origin set to be on the baseline, at the beginning (although not
--   bounding).  This is the reference point of showText in the Cairo
--   graphics library.
--
--   Note that it /takes up no space/.
baselineText :: Renderable Text b => String -> Diagram b R2
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
newtype FontSize = FontSize (Last (Measure R2))
  deriving (Typeable, Data, Semigroup)
instance AttributeClass FontSize

type instance V FontSize = R2

instance Default FontSize where
    def = FontSize (Last (Output 1))

instance Transformable FontSize where
  transform t (FontSize (Last s)) =
    FontSize (Last (transform t s))

-- | Extract the size from a @FontSize@ attribute.
getFontSize :: FontSize -> Measure R2
getFontSize (FontSize (Last s)) = s

-- | Set the font size, that is, the size of the font's em-square as
--   measured within the current local vector space.  The default size
--   is @1@.
setFontSize :: (HasStyle a, V a ~ R2) => Measure R2 -> a -> a
setFontSize = applyGTAttr . FontSize . Last

-- | Default for 'FontSize'.
fontSize :: (HasStyle a, V a ~ R2) => Double -> a -> a
fontSize  = fontSizeN

-- | A convenient synonym for 'setFontSize (Global w)'.
fontSizeG :: (HasStyle a, V a ~ R2) => Double -> a -> a
fontSizeG w = setFontSize (Global w)

-- | A convenient synonym for 'setFontSize (Normalized w)'.
fontSizeN :: (HasStyle a, V a ~ R2) => Double -> a -> a
fontSizeN w = setFontSize (Normalized w)

-- | A convenient synonym for 'setFontSize (Output w)'.
fontSizeO :: (HasStyle a, V a ~ R2) => Double -> a -> a
fontSizeO w = setFontSize (Output w)

-- | A convenient sysnonym for 'setFontSize (Local w)'.
fontSizeL :: (HasStyle a, V a ~ R2) => Double -> a -> a
fontSizeL w = setFontSize (Local w)

-- | Apply a 'FontSize' attribute.
fontSizeA :: (HasStyle a, V a ~ R2) => FontSize -> a -> a
fontSizeA = applyGTAttr

--------------------------------------------------
-- Font slant

data FontSlant = FontSlantNormal
               | FontSlantItalic
               | FontSlantOblique
    deriving (Eq)

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
    deriving (Eq)

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
