{-# LANGUAGE CPP #-}

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

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Arrow           ((&&&))
import           Control.Lens
import           Data.Distributive
import qualified Data.Foldable           as F
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

-- | Build a 3D transformation matrix in homogeneous coordinates from
--   a 'Transformation V3'.
mkMatHomo :: Num n => Transformation V3 n -> M44 n
mkMatHomo t = mkTransformationMat (mkMat t) (transl t)

-- | Make a 2D transformation from a 2x2 transform matrix and a
--   translation vector. If the matrix is not invertible, 'Nothing' is
--   returned.
fromMat22 :: (Epsilon n, Floating n) => M22 n -> V2 n -> Maybe (T2 n)
fromMat22 m v = flip (fromMatWithInv m) v <$> inv22 m

-- | Make a 3D transformation from a 3x3 transform matrix and a
--   translation vector. If the matrix is not invertible, 'Nothing' is
--   returned.
fromMat33 :: (Epsilon n, Floating n) => M33 n -> V3 n -> Maybe (T3 n)
fromMat33 m v = flip (fromMatWithInv m) v <$> inv33 m

-- | Build a transform with a maxtrix along with its inverse.
fromMatWithInv :: (Additive v, Distributive v, F.Foldable v, Num n)
  => v (v n) -- ^ matrix
  -> v (v n) -- ^ inverse
  -> v n     -- ^ translation
  -> Transformation v n
fromMatWithInv m m_ v =
  Transformation ((*! m)            <-> (*! m_))
                 ((*! distribute m) <-> (*! distribute m_))
                 v

-- | Prism onto a 2D transformation from a 2x2 transform matrix and
--   translation vector.
mat22 :: (Epsilon n, Floating n) => Prism' (M22 n, V2 n) (T2 n)
mat22 = prism' (mkMat &&& transl) (uncurry fromMat22)

-- | Prism onto a 2D transformation from a 2x2 transform matrix and
--   translation vector.
mat33 :: (Epsilon n, Floating n) => Prism' (M33 n, V3 n) (T3 n)
mat33 = prism' (mkMat &&& transl) (uncurry fromMat33)
