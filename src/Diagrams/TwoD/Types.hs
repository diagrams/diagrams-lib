{-# LANGUAGE PackageImports, TypeFamilies, TypeSynonymInstances #-}
module Diagrams.TwoD.Types
       ( P2
       , Angle
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams

type P2 = (Double, Double)

instance Transformable P2 where
  type TSpace P2 = P2
  transform = apply

type Angle = Double  -- in radians
