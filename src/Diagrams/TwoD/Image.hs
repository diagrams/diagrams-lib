{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Image
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.
-- Usage: To create a diagram from an embedded image with width 1 and height
--   set according to the aspect ratio: 'image img # scaleUToX 1`
--   where 'img' is a 'DImage v Embedded'
-----------------------------------------------------------------------------

module Diagrams.TwoD.Image
    (
      DImage(..), ImageData(..)
    , Embedded, External, Native
    , image
    , loadImageEmb
    , loadImageExt
    , uncheckedImageRef
    , raster
    , rasterDia
    ) where


import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)

import           Data.Colour          (AlphaColour)
import           Data.Typeable        (Typeable)

import           Diagrams.Core

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.TwoD.Path   (isInsideEvenOdd)
import           Diagrams.TwoD.Shapes (rect)
import           Diagrams.TwoD.Types  (TwoD)

import           Data.AffineSpace     ((.-.))
import           Data.Semigroup

data Embedded deriving Typeable
data External deriving Typeable
data Native (t :: *) deriving Typeable

-- | 'ImageData' is either a JuicyPixels @DynamicImage@ tagged as 'Embedded' or
--   a reference tagged as 'External'. Additionally 'Native' is provided for
--   external libraries to hook into.
data ImageData :: * -> * where
  ImageRaster :: DynamicImage -> ImageData Embedded
  ImageRef :: FilePath -> ImageData External
  ImageNative :: t -> ImageData (Native t)

-------------------------------------------------------------------------------
-- | An image primitive, the two ints are width followed by height.
--   Will typically be created by @loadImageEmb@ or @loadImageExt@ which,
--   will handle setting the width and heigh to the actual width and height
--   of the image.
data DImage :: * -> * -> * where
  DImage :: ImageData t -> Int -> Int -> Transformation v -> DImage v t
  deriving Typeable

type instance V (DImage v a) = v

instance (TwoD v) => Transformable (DImage v a) where
  transform t1 (DImage iD w h t2) = DImage iD w h (t1 <> t2)

instance (TwoD v) => HasOrigin (DImage v a) where
  moveOriginTo p = translate (origin .-. p)

-- | Make a 'DImage' into a 'Diagram'.
image :: (Typeable a, Renderable (DImage v a) b, TwoD v) => DImage v a -> Diagram b v
image img = mkQD (Prim (img)) (getEnvelope r) (getTrace r) mempty
                  (Query $ \p -> Any (isInsideEvenOdd p r))
  where
    -- r :: Path v
    r = rect (fromIntegral w) (fromIntegral h)
    DImage _ w h _ = img

-- | Use JuicyPixels to read an image in any format and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
loadImageEmb :: (TwoD v) => FilePath -> IO (Either String (DImage v Embedded))
loadImageEmb path = do
    dImg <- readImage path
    return $ case dImg of
      Left msg  -> Left msg
      Right img -> Right (DImage (ImageRaster img) w h mempty)
        where
          w = dynamicMap imageWidth img
          h = dynamicMap imageHeight img

-- | Check that a file exists, and use JuicyPixels to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: (TwoD v) => FilePath -> IO (Either String (DImage v External))
loadImageExt path = do
  dImg <- readImage path
  return $ case dImg of
    Left msg  -> Left msg
    Right img -> Right $ DImage (ImageRef path) w h mempty
      where
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

-- | Make an "unchecked" image reference; have to specify a
--   width and height. Unless the aspect ratio of the external
--   image is the w :: h, then the image will be distorted.
uncheckedImageRef :: (TwoD v) => FilePath -> Int -> Int -> DImage v External
uncheckedImageRef path w h = DImage (ImageRef path) w h mempty

-- | Crate a diagram from raw raster data.
rasterDia :: (Renderable (DImage v Embedded) b, TwoD v)
          => (Int -> Int -> AlphaColour Double) -> Int -> Int -> Diagram b v
rasterDia f w h = image $ raster f w h

-- | Create an image "from scratch" by specifying the pixel data
raster :: (TwoD v) => (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage v Embedded
raster f w h = DImage (ImageRaster (ImageRGBA8 img)) w h mempty
  where
    img = generateImage g w h
    g x y = fromAlphaColour $ f x y

fromAlphaColour :: AlphaColour Double -> PixelRGBA8
fromAlphaColour c = PixelRGBA8 r g b a
  where
    (r, g, b, a) = (int r', int g', int b', int a')
    (r', g', b', a') = colorToSRGBA c
    int x = round (255 * x)

instance (TwoD v) => Renderable (DImage v a) NullBackend where
  render _ _ = mempty
