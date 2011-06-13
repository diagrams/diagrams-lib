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
import Diagrams.Util

import Data.AffineSpace ((.-.))

import Data.Monoid

-- XXX comment me
data Image = Image { imgFile   :: FilePath
                     -- add SizeSpec parameter (also make option to cairo backend?)
                   , imgTransf :: T2
                   }

type instance V Image = R2

instance Transformable Image where
  transform t1 (Image file t2) = Image file (t1 <> t2)

instance HasOrigin Image where
  moveOriginTo p = translate (origin .-. p)

-- XXX comment me
image :: (Renderable Image b) => FilePath -> Double -> Double -> Diagram b R2
image file w h = mkAD (Prim (Image file mempty))
                      (getBounds r)
                      mempty
                      (Query $ \p -> Any (isInsideEvenOdd p r))
  where r :: Path R2
        r = fromOffsets [(w,0), (0,h), (-w,0), (0,-h)]
            # centerXY
            -- XXX use rectPath ?


