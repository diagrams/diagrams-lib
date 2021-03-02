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
-- Importing external images into diagrams.  Usage example: To create
-- a diagram from an embedded image with width 1 and height set
-- according to the aspect ratio, use @image img # scaleUToX 1@, where
-- @img@ is a value of type @DImage n e@, created with a function like
-- 'loadImageEmb', 'loadImageExt', or 'raster'.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Image
    (
      DImage(..), ImageData(..)
    , Embedded, External, Native
    , image
    , embeddedImage
    , loadImageEmb
    , loadImageEmbBS
    , loadImageExt
    , uncheckedImageRef
    , raster
    , rasterDia
    ) where

import           Codec.Picture

import           Data.Colour          (AlphaColour)
import           Data.Kind            (Type)
import           Data.Semigroup
import           Data.Typeable        (Typeable)

import           Diagrams.Core

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.Path        (Path)
import           Diagrams.Query
import           Diagrams.TwoD.Path   (isInsideEvenOdd)
import           Diagrams.TwoD.Shapes (rect)
import           Diagrams.TwoD.Types

import           Data.ByteString

import           Linear.Affine

data Embedded deriving Typeable
data External deriving Typeable
data Native (t :: Type) deriving Typeable

-- | 'ImageData' is either a JuicyPixels @DynamicImage@ tagged as 'Embedded' or
--   a reference tagged as 'External'. Additionally 'Native' is provided for
--   external libraries to hook into.
data ImageData :: Type -> Type where
  ImageRaster :: DynamicImage -> ImageData Embedded
  ImageRef    :: FilePath -> ImageData External
  ImageNative :: t -> ImageData (Native t)

-------------------------------------------------------------------------------
-- | An image primitive, the two ints are width followed by height.
--   Will typically be created by @loadImageEmb@ or @loadImageExt@ which,
--   will handle setting the width and height to the actual width and height
--   of the image.
data DImage :: Type -> Type -> Type where
  DImage :: ImageData t -> Int -> Int -> Transformation V2 n -> DImage n t
  deriving Typeable

type instance V (DImage n a) = V2
type instance N (DImage n a) = n

instance RealFloat n => HasQuery (DImage n a) Any where
  getQuery (DImage _ w h _) = -- transform t $
    Query $ \p -> Any (isInsideEvenOdd r p)
    where
    r = rectPath (fromIntegral w) (fromIntegral h)

instance Fractional n => Transformable (DImage n a) where
  transform t1 (DImage iD w h t2) = DImage iD w h (t1 <> t2)

instance Fractional n => HasOrigin (DImage n a) where
  moveOriginTo p = translate (origin .-. p)

-- | Make a 'DImage' into a 'Diagram'.
image :: (TypeableFloat n, Typeable a, Renderable (DImage n a) b)
      => DImage n a -> QDiagram b V2 n Any
image img
  = mkQD (Prim img)
         (getEnvelope r)
         (getTrace r)
         mempty
         (Query $ \p -> Any (isInsideEvenOdd r p))
  where
    r = rectPath (fromIntegral w) (fromIntegral h)
    -- should we use the transform here?
    DImage _ w h _ = img

rectPath :: RealFloat n => n -> n -> Path V2 n
rectPath = rect

-- | Read a JuicyPixels @DynamicImage@ and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
embeddedImage :: Num n => DynamicImage -> DImage n Embedded
embeddedImage img = DImage (ImageRaster img) w h mempty
  where
    w = dynamicMap imageWidth img
    h = dynamicMap imageHeight img

-- | Use JuicyPixels to read a file in any format and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
loadImageEmb :: Num n => FilePath -> IO (Either String (DImage n Embedded))
loadImageEmb path = fmap embeddedImage `fmap` readImage path

-- | A pure variant of 'loadImageEmb'
loadImageEmbBS :: Num n => ByteString -> Either String (DImage n Embedded)
loadImageEmbBS bs = embeddedImage `fmap` decodeImage bs

-- | Check that a file exists, and use JuicyPixels to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: Num n => FilePath -> IO (Either String (DImage n External))
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
uncheckedImageRef :: Num n => FilePath -> Int -> Int -> DImage n External
uncheckedImageRef path w h = DImage (ImageRef path) w h mempty

-- | Crate a diagram from raw raster data.
rasterDia :: (TypeableFloat n, Renderable (DImage n Embedded) b)
          => (Int -> Int -> AlphaColour Double) -> Int -> Int -> QDiagram b V2 n Any
rasterDia f w h = image $ raster f w h

-- | Create an image "from scratch" by specifying the pixel data
raster :: Num n => (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage n Embedded
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

instance Fractional n => (Renderable (DImage n a) NullBackend) where
  render _ _ = mempty

