{-# LANGUAGE DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , FlexibleContexts
           , TypeFamilies
           , MultiParamTypeClasses
  #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , FontSize(..), getFontSize, fontSize
  -- ** Font slant
  , FontSlant(..), FontSlantA, getFontSlant, fontSlant, italic, oblique
  -- ** Font weight
  , FontWeight(..), FontWeightA, getFontWeight, fontWeight, bold
  ) where

import Diagrams.Attributes
import Diagrams.Core
import Diagrams.TwoD.Types

import Data.AffineSpace ((.-.))
import Data.VectorSpace (Scalar, InnerSpace)
import Data.Basis (Basis, HasBasis)
import Data.MemoTrie (HasTrie)
import Data.AdditiveGroup (AdditiveGroup)
import Data.Semigroup

import Data.Colour

import Data.Typeable

------------------------------------------------------------
-- Text diagrams
------------------------------------------------------------

-- | A text primitive consists of the string contents and alignment
--   specification, along with a transformation mapping from the local
--   vector space of the text to the vector space in which it is
--   embedded.
data Text a = Text (T2 a) (TextAlignment a) String

type instance V (Text a) = V2 a

instance ( Num a
         , HasBasis a
         , HasTrie (Basis a)
         , a ~ Scalar a
         ) => Transformable (Text a) where
  transform t (Text tt a s) = Text (t <> tt) a s

instance ( Num a
         , AdditiveGroup a
         , HasBasis a
         , HasTrie (Basis a)
         , a ~ Scalar a
         ) => HasOrigin (Text a) where
  moveOriginTo p = translate (origin .-. p)

instance ( Num a
         , HasBasis a
         , HasTrie (Basis a)
         , a ~ Scalar a) => Renderable (Text a) NullBackend where
  render _ _ = mempty

-- | @TextAlignment@ specifies the alignment of the text's origin.
data TextAlignment a = BaselineText | BoxAlignedText a a

mkText :: ( Ord a
          , Floating a
          , InnerSpace a
          , a ~ Scalar a
          , Renderable (Text a) b
          ) => TextAlignment a -> String -> Diagram b (V2 a)
mkText a t = recommendFillColor black
           $ mkQD (Prim (Text mempty a t))
                       mempty
                       mempty
                       mempty
                       mempty

-- | Create a primitive text diagram from the given string, with center
--   alignment, equivalent to @'alignedText' 0.5 0.5@.
--
--   Note that it /takes up no space/, as text size information is not
--   available.
text :: ( Fractional a
        , Floating a
        , Ord a
        , a ~ Scalar a
        , InnerSpace a
        , Renderable (Text a) b
        ) => String -> Diagram b (V2 a)
text = alignedText 0.5 0.5

-- | Create a primitive text diagram from the given string, origin at
--   the top left corner of the text's bounding box, equivalent to
--   @'alignedText' 0 1@.
--
--   Note that it /takes up no space/.
topLeftText :: ( Num a
               , Ord a
               , Floating a
               , a ~ Scalar a
               , InnerSpace a
               , Renderable (Text a) b
               ) => String -> Diagram b (V2 a)
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
alignedText :: ( Ord a
               , Floating a
               , a ~ Scalar a
               , InnerSpace a
               , Renderable (Text a) b
               ) => a -> a -> String -> Diagram b (V2 a)
alignedText w h = mkText (BoxAlignedText w h)

-- | Create a primitive text diagram from the given string, with the
--   origin set to be on the baseline, at the beginning (although not
--   bounding).  This is the reference point of showText in the Cairo
--   graphics library.
--
--   Note that it /takes up no space/.
baselineText :: ( Ord a
                , Floating a
                , a ~ Scalar a
                , InnerSpace a
                , Renderable (Text a) b
                ) => String -> Diagram b (V2 a)
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
--   em-square, measured with respect to the current local vector space.
--   Inner @FontSize@ attributes override outer ones.
newtype FontSize = FontSize (Last Double)
  deriving (Typeable, Semigroup, Eq)
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
