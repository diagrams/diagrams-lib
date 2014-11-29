{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}

module Diagrams.ThreeD.Projection where

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Projections
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 3D projections are a way of viewing a three-dimensional objects on a
-- two-dimensional plane.
--
--
-----------------------------------------------------------------------------

-- import           Control.Applicative
import           Control.Lens              hiding (transform)
-- import           Data.Foldable
-- import           Data.Typeable

-- import           Data.Semigroup
-- import           Diagrams.Angle
import           Diagrams.Core
import           Linear as L
-- import           Diagrams.Core.HasOrigin
-- import           Diagrams.ThreeD.Camera
-- import           Diagrams.ThreeD.Transform
-- import           Diagrams.ThreeD.Types
-- import           Diagrams.TwoD             hiding (view)
-- -- import Diagrams.Core
import           Diagrams.Direction
import           Diagrams.LinearMap
import Diagrams.Path
import Diagrams.TrailLike
import Data.Functor.Rep
import Linear.Affine
import Diagrams.Deform


-- * Parallel projections

-- ** Orthographic projections
-- Orthographic projections are a form of parallel projections where are
-- projection lines are orthogonal to the projection plane.

-- Parallel projections

-- facingX :: Num n => AffineMap V3 V2 n
-- facingX =

-- facingY :: Num n => AffineMap V3 V2 n
-- facingY =

-- facingZ :: Num n => AffineMap V3 V2 n
-- facingZ =

-- ** Axonometric projection
-- Axonometric projections are a type of orthographic projection where
-- the object is rotated along one or more of its axes relative to the
-- plane of projection.

-- *** Common axonometric projections

isometricProj :: (InSpace V3 n a, InSpace V2 n b, AffineMappable a b, Floating n, Epsilon n)
              => Direction V3 n -> a -> b
isometricProj up = amap (isometric up)

isometric :: (Floating n, Epsilon n) => Direction V3 n -> AffineMap V3 V2 n
isometric up = affineFromHomo m
  where
    m = lookAt (V3 1 1 1) zero (fromDirection up)

orthoProj :: (InSpace V3 n a, InSpace V2 n b, AffineMappable a b)
          => M44 n -> a -> b
orthoProj = amap . affineFromHomo

affineFromHomo :: Num n => M44 n -> AffineMap V3 V2 n
affineFromHomo m = AffineMap (LinearMap f) (f v)
  where
    f  = view _xy . (m' !*)
    m' = m ^. linearTransform
    v  = m ^. L.translation

linearTransform :: (Representable u, R3 v, R3 u) => Lens' (u (v n)) (M33 n)
linearTransform = column _xyz . _xyz

-- * Perspective projections
-- Perspective projections are when closer objects appear bigger.

m44Deformation :: M44 Double -> Deformation V3 V2 Double
m44Deformation m = Deformation
  -- no idea what I'm doing
  $ \(P v) -> P (fmap sane . view _xz $ normalizePoint (m !* point v))
  -- debugging purposes
  where
  sane x
    | x > (-200) && x < 200 = x
    | otherwise             = 0

-- used for testing
house :: OrderedField n => Path V3 n
house = Path $ map fromVertices ps
  where ps = [[P (V3 1 (-0.2) (-1)),P (V3 1 (-0.2) (-0.4)),P (V3 1 0.2
             (-0.4)),P (V3 1 0.2 (-1))],[P (V3 1 1 1),P (V3 1 0 1.4)],[P
             (V3 1 (-1) 1),P (V3 1 0 1.4)],[P (V3 1 0 1.4),P (V3 (-1) 0
             1.4)],[P (V3 (-1) 1 1),P (V3 (-1) 0 1.4)],[P (V3 (-1) (-1)
             1),P (V3 (-1) 0 1.4)],[P (V3 (-1) (-1) (-1)),P (V3 (-1)
             (-1) 1)],[P (V3 (-1) (-1) 1),P (V3 (-1) 1 1)],[P (V3 (-1) 1
             (-1)),P (V3 (-1) 1 1)],[P (V3 (-1) (-1) (-1)),P (V3 (-1) 1
             (-1))],[P (V3 1 (-1) (-1)),P (V3 1 1 (-1))],[P (V3 1 (-1)
             1),P (V3 1 (-1) (-1))],[P (V3 1 1 (-1)),P (V3 1 1 1)],[P
                  (V3 1 1 1),P (V3 1 (-1) 1)],[P (V3 (-1) (-1) (-1)),P
                  (V3 1 (-1) (-1))],[P (V3 (-1) (-1) 1),P (V3 1 (-1)
                  1)],[P (V3 1 1 (-1)),P (V3 (-1) 1 (-1))],[P (V3 (-1) 1
                  1),P (V3 1 1 1)]]

