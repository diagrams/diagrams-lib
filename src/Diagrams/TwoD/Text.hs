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
    Text(..)
  , text

  , Font(..), getFont, font
  , FontSize(..), getFontSize, fontSize
  ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.Util

import Data.Monoid (mempty)
import Data.Semigroup (Semigroup, Last(..))

import Data.Typeable

-- | A text primitive consists of the string contents along with a
--   transformation mapping from the local vector space of the text to
--   the vector space in which it is embedded.  In its local vector
--   space, the text is positioned ???
data Text = Text T2 String

type instance V Text = R2

instance Transformable Text where
  transform t (Text tt s) = Text (t <> tt) s

text :: Renderable Text b => String -> Diagram b R2
text t = mkAD (Prim (Text mempty t))
              mempty
              mempty
              mempty

newtype Font = Font (Last String)
  deriving (Typeable, Semigroup)
instance AttributeClass Font

getFont :: Font -> String
getFont (Font (Last f)) = f

font :: HasStyle a => String -> a -> a
font = applyAttr . Font . Last

newtype FontSize = FontSize (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass FontSize

getFontSize :: FontSize -> Double
getFontSize (FontSize (Last s)) = s

fontSize :: HasStyle a => Double -> a -> a
fontSize = applyAttr . FontSize . Last
