{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Adjust
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A default diagram-adjustment implementation for two-dimensional
-- diagrams, useful for backend implementors.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Adjust
    (
      setDefault2DAttributes
    , adjustDiaSize2D
    , adjustDia2D
    , adjustSize     -- for backwards compatibility
    , requiredScale  -- re-exported for backwards compatibility
    ) where

import Diagrams.Core

import Diagrams.Attributes  (lw, lc)
import Diagrams.Util        ((#))

import Diagrams.TwoD.Types  (D2, R2, p2)
import Diagrams.TwoD.Size   ( size2D, center2D, SizeSpec2D(..)
                            , requiredScaleT, requiredScale
                            )
import Diagrams.TwoD.Text   (fontSize)

import Data.AffineSpace     ((.-.))
import Data.Semigroup
import Data.VectorSpace     (Scalar, InnerSpace)
import Data.Basis           (Basis, HasBasis)
import Data.MemoTrie        (HasTrie)
import Data.AdditiveGroup   (AdditiveGroup)

import Data.Colour.Names    (black)

-- | Set default attributes of a 2D diagram (in case they have not
--   been set):
--
--       * Line width 0.01
--
--       * Line color black
--
--       * Font size 1
setDefault2DAttributes :: ( Ord a
                          , Floating a
                          , HasBasis a
                          , InnerSpace a
                          , HasTrie (Basis a)
                          , a ~ Scalar a
                          , Semigroup m
                          ) => QDiagram b (D2 a) m -> QDiagram b (D2 a) m
setDefault2DAttributes d = d # lw 0.01 # lc black # fontSize 1

-- | Adjust the size and position of a 2D diagram to fit within the
--   requested size. The first two arguments specify a method for
--   extracting the requested output size from the rendering options,
--   and a way of updating the rendering options with a new (more
--   specific) size.
adjustDiaSize2D :: ( Eq a
                   , RealFloat a
                   , InnerSpace a
                   , a ~ Scalar a
                   , Transformable a
                   , V a ~ D2 a
                   , Monoid' m
                   ) => (Options b (D2 a) -> SizeSpec2D a)
                     -> (SizeSpec2D a -> Options b (D2 a) -> Options b (D2 a))
                     -> b -> Options b (D2 a) -> QDiagram b (D2 a) m
                     -> (Options b (D2 a), QDiagram b (D2 a) m)
adjustDiaSize2D getSize setSize _ opts d =
  ( case spec of
       Dims _ _ -> opts
       _        -> setSize (uncurry Dims . scale s $ size) opts

  , d # scale s
      # translate tr
  )
  where spec = getSize opts
        size = size2D d
        s    = requiredScale spec size
        finalSz = case spec of
                    Dims w h -> (w,h)
                    _        -> scale s size
        tr = (0.5 *. p2 finalSz) .-. (s *. center2D d)

-- | @adjustDia2D@ provides a useful default implementation of
--   the 'adjustDia' method from the 'Backend' type class.
--
--   As its first two arguments it requires a method for extracting
--   the requested output size from the rendering options, and a way
--   of updating the rendering options with a new (more specific) size.
--
--   It then performs the following adjustments:
--
--   * Set default attributes (see 'setDefault2DAttributes')
--
--   * Freeze the diagram in its final form
--
--   * Scale and translate the diagram to fit within the requested
--     size (see 'adjustDiaSize2D')
--
--   * Also return the actual adjusted size of the diagram.
adjustDia2D :: ( Ord a
               , RealFloat a
               , AdditiveGroup a
               , HasBasis a
               , InnerSpace a
               , HasTrie (Basis a)
               , a ~ Scalar a
               , Transformable a
               , V a ~ D2 a
               , Monoid' m
               ) => (Options b (D2 a) -> SizeSpec2D a)
                 -> (SizeSpec2D a -> Options b (D2 a) -> Options b (D2 a))
                 -> b -> Options b (D2 a) -> QDiagram b (D2 a) m
                 -> (Options b (D2 a), QDiagram b (D2 a) m)
adjustDia2D getSize setSize b opts d
  = adjustDiaSize2D getSize setSize b opts (d # setDefault2DAttributes # freeze)

{-# DEPRECATED adjustSize "Use Diagrams.TwoD.Size.requiredScaleT instead." #-}
-- | Re-export 'requiredScaleT' with the name 'adjustSize' for
--   backwards compatibility.
adjustSize :: ( RealFloat a
              , HasBasis a
              , HasTrie (Basis a)
              , a ~ Scalar a
              ) => SizeSpec2D a -> (a, a) -> Transformation (D2 a)
adjustSize = requiredScaleT