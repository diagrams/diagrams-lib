{-# LANGUAGE DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , FlexibleContexts
           , TypeFamilies
  #-}
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
    Text(..)
  , text

  -- * Text attributes
  -- ** Font family
  , Font(..), getFont, font
  -- ** Font size
  , FontSize(..), getFontSize, fontSize
  -- ** Font slant
  , FontSlant(..), FontSlantA, getFontSlant, fontSlant, italic, oblique
  -- ** Font weight
  , FontWeight(..), FontWeightA, getFontWeight, fontWeight, bold
  ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.Util

import Data.Monoid (mempty)
import Data.Semigroup (Semigroup, Last(..))

import Data.Typeable

------------------------------------------------------------
-- Text diagrams
------------------------------------------------------------

-- | A text primitive consists of the string contents along with a
--   transformation mapping from the local vector space of the text to
--   the vector space in which it is embedded.
data Text = Text T2 String

type instance V Text = R2

instance Transformable Text where
  transform t (Text tt s) = Text (t <> tt) s

-- | Create a primitive text diagram from the given string, which
--   /takes up no space/.  By default, the text is centered with
--   respect to its local origin (see 'alignText').
text :: Renderable Text b => String -> Diagram b R2
text t = mkAD (Prim (Text mempty t))
              mempty
              mempty
              mempty

------------------------------------------------------------
-- Text attributes
------------------------------------------------------------

{-
--------------------------------------------------
-- Alignment

-- | The @TextAlignment@ attribute specifies what alignment should be
--   applied to text.  Inner @TextAlignment@ attributes override outer
--   ones.
newtype TextAlignment = TextAlignment (Last (Alignment R2))
  deriving (Typeable, Semigroup)
instance AttributeClass TextAlignment

-- | Extract an alignment from a @TextAlignment@ attribute.
getTextAlignment :: TextAlignment -> Alignment R2
getTextAlignment (TextAlignment (Last a)) = a

-- | The default alignment for text is centered.
centeredText :: TextAlignment
centeredText = TextAlignment (Last (asAlignment id))

-- | @alignText f@ aligns text by applying the alignment function @f@
--   (any transformation of boundable things with origins may be used;
--   for example, 'alignTL' and friends).
alignText :: HasStyle a => (Alignment R2 -> Alignment R2) -> a -> a
alignText = applyAttr . TextAlignment . Last . asAlignment
-}

--------------------------------------------------
-- Font family

-- | The @Font@ attribute specifies the name of a font family.  Inner
--   @Font@ attributes override outer ones.
newtype Font = Font (Last String)
  deriving (Typeable, Semigroup)
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
--   em-square, measured with respect to the current local vector space.
--   Inner @FontSize@ attributes override outer ones.
newtype FontSize = FontSize (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass FontSize

-- | Extract the size from a @FontSize@ attribute.
getFontSize :: FontSize -> Double
getFontSize (FontSize (Last s)) = s

-- | Set the font size, that is, the size of the font's em-square as
--   measured within the current local vector space.  The default size
--   is @1@.
fontSize :: HasStyle a => Double -> a -> a
fontSize = applyAttr . FontSize . Last

--------------------------------------------------
-- Font slant

data FontSlant = FontSlantNormal
               | FontSlantItalic
               | FontSlantOblique

-- | The @FontSlantA@ attribute specifies the slant (normal, italic,
--   or oblique) that should be used for all text within a diagram.
--   Inner @FontSlantA@ attributes override outer ones.
newtype FontSlantA = FontSlantA (Last (FontSlant))
  deriving (Typeable, Semigroup)
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

-- | The @FontWeightA@ attribute specifies the weight (normal or bold)
--   that should be used for all text within a diagram.  Inner
--   @FontWeightA@ attributes override outer ones.
newtype FontWeightA = FontWeightA (Last (FontWeight))
  deriving (Typeable, Semigroup)
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