{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}
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
      Image(..)
    , image
    ) where

import Graphics.Rendering.Diagrams

import Diagrams.Path
import Diagrams.TwoD.Types
import Diagrams.TwoD.Align
import Diagrams.TwoD.Path
import Diagrams.TwoD.Util (SizeSpec2D(..))
import Diagrams.Util

import Data.AffineSpace ((.-.))

import Data.Monoid

-- XXX comment me
data Image = Image { imgFile   :: FilePath
                   , imgSize   :: SizeSpec2D
                   , imgTransf :: T2
                   }

type instance V Image = R2

instance Transformable Image where
  transform t1 (Image file sz t2) = Image file sz (t1 <> t2)

instance HasOrigin Image where
  moveOriginTo p = translate (origin .-. p)

-- See Note [Image size specification]

-- | Take an external image from the specified file and turn it into a
--   diagram with the specified width and height, centered at the
--   origin.  Note that the image's aspect ratio will be preserved; if
--   the specified width and height have a different ratio than the
--   image's aspect ratio, there will be extra space in one dimension.
image :: (Renderable Image b) => FilePath -> Double -> Double -> Diagram b R2
image file w h = mkAD (Prim (Image file (Dims w h) mempty))
                      (getBounds r)
                      mempty
                      (Query $ \p -> Any (isInsideEvenOdd p r))
  where r :: Path R2
        r = fromOffsets [(w,0), (0,h), (-w,0), (0,-h)]
            # centerXY
            -- XXX use rectPath ?

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