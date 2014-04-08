{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Image
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Image
    (
      DImage(..), ImageData(..)
    , Embedded, External
    , image
    , imageRef
    , mkImageRaster
    , mkImageRef
    , uncheckedImageRef
    , loadImage
    , raster
    ) where


import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)

import           Data.Typeable
import           Data.Colour          (AlphaColour)

import           Diagrams.Core

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.Path
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Types

import           Data.AffineSpace     ((.-.))
import           Data.Semigroup

data Embedded deriving Typeable
data External deriving Typeable

-- | 'ImageData' is either a JuicyPixels @DynamicImage@ tagged as 'Embedded' or
--   a reference tagged as 'External'.
data ImageData :: * -> * where
  ImageRaster :: DynamicImage -> ImageData Embedded
  ImageRef :: FilePath -> ImageData External

-------------------------------------------------------------------------------
-- | An image primitive, the two ints are width followed by height.
data DImage :: * -> * where
  DImage :: ImageData t -> Int -> Int -> T2 -> DImage t
  deriving Typeable

type instance V (DImage a) = R2

instance Transformable (DImage a) where
  transform t1 (DImage iD w h t2) = DImage iD w h (t1 <> t2)

instance HasOrigin (DImage a) where
  moveOriginTo p = translate (origin .-. p)

-- | Use JuicyPixels to read an image in any format.
loadImage :: FilePath -> IO (Either String (DImage Embedded))
loadImage path = do
    dImg <- readImage path
    return $ case dImg of
      Left msg  -> Left msg
      Right img -> Right (DImage (ImageRaster img) 1 1 mempty)

-- | Make a 'DImage' into a 'Diagram'.
image :: (Typeable a, Renderable (DImage a) b) => DImage a -> Diagram b R2
image img = mkQD (Prim (img)) (getEnvelope r) (getTrace r) mempty
                  (Query $ \p -> Any (isInsideEvenOdd p r))
  where
    r :: Path R2
    r = rect (fromIntegral w) (fromIntegral h)
    DImage _ w h _ = img
-- See Note [Image size specification]

-- | Make a@DynamicImage@ into a 'Diagram', i.e a primitive of type 'DImage Embedded'.
mkImageRaster :: Renderable (DImage Embedded) b
              => DynamicImage -> Int -> Int -> Diagram b R2
mkImageRaster dImg w h = image $ DImage (ImageRaster dImg) w h mempty

-- | Make a file path into a 'Diagram', i.e a primitive of type 'DImage External'.
--   This function calls @uncheckedImageRef@ and provides no guarantee that
--   the image file exists.
mkImageRef :: Renderable (DImage External) b
              => FilePath -> Int -> Int -> Diagram b R2
mkImageRef path w h = image $ uncheckedImageRef path w h

-- | Check that a file exists, and use JuicyPixels to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
imageRef :: FilePath -> IO (Either String (DImage External))
imageRef path = do
  dImg <- readImage path
  return $ case dImg of
    Left msg  -> Left msg
    Right img -> Right $ DImage (ImageRef path) w h mempty
      where
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

-- | Make an "unchecked" image reference; have to specify a
--   width and height.
uncheckedImageRef :: FilePath -> Int -> Int -> DImage External
uncheckedImageRef path w h = DImage (ImageRef path) w h mempty

-- | Create an image "from scratch" by specifying the pixel data
raster :: (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage Embedded
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

instance Renderable (DImage a) NullBackend where
  render _ _ = mempty

{- ~~~~ Note [Image size specification]

It's tempting to make 'image' take a SizeSpec2D instead of two
Doubles.  For example, if I know I want the image to be x units wide
but I don't know the original aspect ratio of the image, I'd like to
be able to just say "make it x units wide".  The problem is that
diagrams would then not know how tall the image is until rendering
time (at least, not without unsafePerformIO yuckiness).  A more
general solution will have to wait until we can specify constraints
and solve them later.

-}
