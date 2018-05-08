{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE MonoLocalBinds  #-}
{-# LANGUAGE TypeFamilies    #-}

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

import           Control.Arrow           ((&&&))
import           Control.Lens
import           Data.Distributive
import qualified Data.Foldable           as F
import           Data.Functor.Rep

import           Diagrams.Core.Transform as D
import           Diagrams.ThreeD.Types
import           Diagrams.TwoD.Types

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
--   translation vector. Does not check if the matrix is not invertible
--   (in which case the 'T2' will be invalid).
fromMat22 :: Floating n => M22 n -> V2 n -> T2 n
fromMat22 m v = fromMatWithInv m (inv22 m) v

-- | Make a 3D transformation from a 3x3 transform matrix and a
--   translation vector. Does not check if the matrix is not invertible
--   (in which case the 'T3' will be invalid).
fromMat33 :: Floating n => M33 n -> V3 n -> T3 n
fromMat33 m v = fromMatWithInv m (inv33 m) v

-- | Build a transform with a maxtrix along with its inverse.
fromMatWithInv :: (Additive v, Distributive v, F.Foldable v, Num n)
  => v (v n) -- ^ matrix
  -> v (v n) -- ^ inverse
  -> v n     -- ^ translation
  -> Transformation v n
fromMatWithInv m m_ v =
  Transformation ((m !*)            <-> (m_ !*))
                 ((distribute m !*) <-> (distribute m_ !*))
                 v

-- | Prism onto a 2D transformation from a 2x2 transform matrix and
--   translation vector. Does not check if the matrix is invertible (in
--   which case the 'T2' will be invalid).
mat22 :: Floating n => Iso' (M22 n, V2 n) (T2 n)
mat22 = iso (uncurry fromMat22) (mkMat &&& transl)

-- | Prism onto a 3D transformation from a 3x3 transform matrix and
--   translation vector. Does not check if the matrix is invertible
--   (in which case the 'T3' will be invalid).
mat33 :: Floating n => Iso' (M33 n, V3 n) (T3 n)
mat33 = iso (uncurry fromMat33) (mkMat &&& transl)
