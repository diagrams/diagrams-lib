{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Show
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple Show-based diagrams backend, for testing purposes.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Show where

import Diagrams.Prelude

import Graphics.Rendering.Diagrams.Transform
import Diagrams.TwoD.Ellipse

import Data.Basis
import Data.VectorSpace

import Text.PrettyPrint (Doc, empty, ($+$), vcat, parens, hsep, text, nest)
import qualified Text.PrettyPrint as PP

import Data.List (sortBy, transpose)
import Data.Ord (comparing)

-- | Token for identifying this backend.
data ShowBackend v = ShowBackend

instance Monoid Doc where
  mempty  = empty
  mappend = ($+$)

instance HasLinearMap v => Backend (ShowBackend v) where
  type BSpace (ShowBackend v) = v
  type Render (ShowBackend v) = Doc
  type Result (ShowBackend v) = String
  data Options (ShowBackend v) = SBOpt

  withStyle _ s r = r -- XXX FIXME

  doRender _ _ r = PP.render r

  adjustDia _ _ d = d

renderTransf :: forall v. (Num (Scalar v), HasLinearMap v)
             => Transformation v -> Doc
renderTransf t = renderMat mat
  where tr :: v
        tr    = transl t
        basis :: [Basis v]
        basis = map fst (decompose tr)
        es :: [v]
        es    = map basisValue basis
        vmat :: [v]
        vmat = map (apply t) es
        mat :: [[Scalar v]]
        mat = map decompV vmat
        mat' :: [[Scalar v]]
        mat'  = map (++[0]) mat ++ [decompV tr ++ [1]]
        decompV = map snd . decompose

renderMat :: Show a => [[a]] -> Doc
renderMat = vcat . map renderRow . transpose
  where renderRow = parens . hsep . map (text . show)

instance Renderable Ellipse (ShowBackend R2) where
  render b (Ellipse t) = text "Ellipse (" $+$
                           (nest 2 (renderTransf t)) $+$
                         text ")"

instance (Show v, HasLinearMap v) => Renderable (Segment v) (ShowBackend v) where
  render _ s = text (show s)

instance (Show v, HasLinearMap v) => Renderable (Trail v) (ShowBackend v) where
  render _ t = text (show t)

instance (Ord v, Show v, HasLinearMap v) => Renderable (Path v) (ShowBackend v) where
  render _ p = text (show p)