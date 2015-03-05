-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Transform.Matrix
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Functions for converting between 'Transformation's and matricies.
--
-----------------------------------------------------------------------------

module Diagrams.Transform.Matrix where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Rep

import           Diagrams.Core.Transform as D
import           Diagrams.ThreeD.Types
import           Diagrams.TwoD.Types

import           Linear.Epsilon
import           Linear.Matrix
import           Linear.Vector

-- | Build a matrix from a 'Transformation', ignoring the translation.
mkMat :: (HasBasis v, Num n) => Transformation v n -> v (v n)
mkMat t = distribute . tabulate $ apply t . unit . el

-- | Build a transformation matrix from a 'Transformation'.
mkMatHomo :: Num n => Transformation V3 n -> M44 n
mkMatHomo t = mkTransformationMat (mkMat t) (transl t)

fromMat22 :: (Epsilon n, Floating n) => M22 n -> V2 n -> Maybe (T2 n)
fromMat22 m v = flip (fromMatWithInv m) v <$> inv22 m

fromMat33 :: (Epsilon n, Floating n) => M33 n -> V3 n -> Maybe (T3 n)
fromMat33 m v = flip (fromMatWithInv m) v <$> inv33 m

-- | Build a transform with a maxtrix along with its inverse.
fromMatWithInv :: (Additive v, Distributive v, Foldable v, Num n)
  => v (v n) -- ^ matrix
  -> v (v n) -- ^ inverse
  -> v n     -- ^ translation
  -> Transformation v n
fromMatWithInv m m_ v =
  Transformation ((*! m)            <-> (*! m_))
                 ((*! distribute m) <-> (*! distribute m_))
                 v

-- are these useful?
mat22 :: (Epsilon n, Floating n) => Prism' (M22 n, V2 n) (T2 n)
mat22 = prism' (mkMat &&& transl) (uncurry fromMat22)

mat33 :: (Epsilon n, Floating n) => Prism' (M33 n, V3 n) (T3 n)
mat33 = prism' (mkMat &&& transl) (uncurry fromMat33)

