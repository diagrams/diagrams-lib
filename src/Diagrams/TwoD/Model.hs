{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for visualizing diagrams' internal model: local origins,
-- envelopes, /etc./
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Model
       ( -- * Showing the local origin
         showOrigin
       , showOrigin'
       , OriginOpts(..)
       , showLabels
       ) where

import Diagrams.Core
import Diagrams.Core.Names

import Diagrams.Path

import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Size    (size2D)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path
import Diagrams.Attributes
import Diagrams.Util

import Control.Arrow (second)
import Data.Semigroup
import Data.Default
import Data.AffineSpace ((.-.))
import Data.VectorSpace ((^*), Scalar, InnerSpace)
import Data.AdditiveGroup (AdditiveGroup)
import Data.Basis (HasBasis, Basis)
import Data.MemoTrie (HasTrie)

import qualified Data.Map as M

import Data.Colour.Names
import Data.Colour (Colour)

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: ( Ord a
              , RealFloat a
              , AdditiveGroup a
              , HasBasis a
              , HasTrie (Basis a)
              , a ~ Scalar a
              , InnerSpace a
              , Renderable (Path (D2 a)) b
              , Backend b (D2 a), Monoid' m
              ) => QDiagram b (D2 a) m -> QDiagram b (D2 a) m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: ( Ord a
               , RealFloat a
               , AdditiveGroup a
               , HasBasis a
               , HasTrie (Basis a)
               , a ~ Scalar a
               , InnerSpace a
               , Renderable (Path (D2 a)) b
               , Backend b (D2 a)
               , Monoid' m
               ) => OriginOpts a -> QDiagram b (D2 a) m -> QDiagram b (D2 a) m
showOrigin' oo d = o <> d
  where o     = stroke (circle sz)
                # fc (oColor oo)
                # lw 0
                # fmap (const mempty)
        (w,h) = size2D d ^* oScale oo
        sz = maximum [w, h, oMinSize oo]

data OriginOpts a = OriginOpts { oColor :: Colour Double
                               , oScale :: a
                               , oMinSize :: a
                               }

instance (Fractional a) => Default (OriginOpts a) where
  def = OriginOpts red (1/50) 0.001


------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: ( Ord a
              , Floating a
              , a ~ Scalar a
              , InnerSpace a
              , Renderable (Text a) b
              , Backend b (D2 a)
              ) => QDiagram b (D2 a) m -> QDiagram b (D2 a) Any
showLabels d =
             ( mconcat
             . map (\(n,p) -> text (show n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) location
             . M.assocs
             $ m
             ) <>
             fmap (const (Any False)) d
  where
    SubMap m = subMap d
