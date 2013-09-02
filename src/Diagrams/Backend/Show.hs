{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Show
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A simple Show-based diagrams backend, for testing purposes.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Show where

import           Diagrams.Core.Transform (onBasis)
import           Diagrams.Prelude

import           Data.Basis

import           Text.PrettyPrint        (Doc, empty, hsep, parens, ($+$))
import qualified Text.PrettyPrint        as PP

import           Data.List               (transpose)

-- | Token for identifying this backend.
data ShowBackend = ShowBackend

instance HasLinearMap v => Backend ShowBackend v where
  data Render  ShowBackend v = SR Doc
  type Result  ShowBackend v = String
  data Options ShowBackend v = SBOpt

  withStyle _ _ _ r = r -- XXX FIXME

  doRender _ _ (SR r) = PP.render r

instance Monoid (Render ShowBackend v) where
  mempty = SR empty
  (SR d1) `mappend` (SR d2) = SR (d1 $+$ d2)

renderTransf :: forall v. (Num (Scalar v), HasLinearMap v, Show (Scalar v))
             => Transformation v -> Doc
renderTransf t = renderMat mat
  where vmat :: [v]
        (vmat, _) = onBasis t
        mat :: [[Scalar v]]
        mat = map decompV vmat
--        mat' :: [[Scalar v]]
--        mat'  = map (++[0]) mat ++ [decompV tr ++ [1]]
        decompV = map snd . decompose

renderMat :: Show a => [[a]] -> Doc
renderMat = PP.vcat . map renderRow . transpose
  where renderRow = parens . hsep . map (PP.text . show)

instance (Show v, HasLinearMap v) => Renderable (Segment o v) ShowBackend where
  render _ s = SR $ PP.text (show s)

instance (Show v, OrderedField (Scalar v), InnerSpace v, HasLinearMap v) => Renderable (Trail v) ShowBackend where
  render _ t = SR $ PP.text (show t)

instance (Show v, OrderedField (Scalar v), InnerSpace v, HasLinearMap v) => Renderable (Path v) ShowBackend where
  render _ p = SR $ PP.text (show p)
