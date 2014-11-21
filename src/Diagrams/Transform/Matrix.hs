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

module Diagrams.Trasform.Matrix where

import           Control.Applicative
import           Control.Lens
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Rep

import           Diagrams.Core.Transform

import           Linear.Epsilon
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.Vector

mat22 :: (Epsilon n, Floating n) => Prism' (M22 n) (Transformation V2 n)
mat22 = prism' mkMat fromMat
  where
    fromMat m = fromMatWithInv m <$> inv22 m

mat33 :: (Epsilon n, Floating n) => Prism' (M33 n) (Transformation V3 n)
mat33 = prism' mkMat fromMat
  where
    fromMat m = fromMatWithInv m <$> inv33 m

-- | Build a matrix from a 'Transformation', ignoring the translation.
mkMat :: (HasBasis v, Num n) => Transformation v n -> v (v n)
mkMat t = tabulate $ apply t . unit . el

-- | Build a transformation matrix from a 'Transformation'.
mkMatHomo :: Num n => Transformation V3 n -> M44 n
mkMatHomo t = mkTransformationMat (mkMat t) (transl t)

-- | Build a transform with a maxtrix along with its inverse.
fromMatWithInv :: (Additive v, Distributive v, Foldable v, Num n)
  => v (v n) -- ^ matrix
  -> v (v n) -- ^ inverse
  -> Transformation v n
fromMatWithInv m m_ =
  Transformation ((*! m)            <-> (*! m_))
                 ((*! distribute m) <-> (*! distribute m_))
                 zero

