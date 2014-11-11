{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Render
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify lighting for 3D rendering.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Light where

import           Data.Colour
import           Data.Monoid
import           Data.Typeable

import           Diagrams.Core
import           Diagrams.Direction
import           Diagrams.ThreeD.Types

-- | A @PointLight@ radiates uniformly in all directions from a given
-- point.
data PointLight n = PointLight (Point V3 n) (Colour Double)
  deriving Typeable

type instance V (PointLight n) = V3
type instance N (PointLight n) = n

-- | A @ParallelLight@ casts parallel rays in the specified direction,
-- from some distant location outside the scene.
data ParallelLight n = ParallelLight (V3 n) (Colour Double)
  deriving Typeable

type instance V (ParallelLight n) = V3
type instance N (ParallelLight n) = n

instance Fractional n => Transformable (PointLight n) where
  transform t (PointLight p c) = PointLight (transform t p) c

instance Fractional n => Transformable (ParallelLight n) where
  transform t (ParallelLight v c) = ParallelLight (transform t v) c

-- | Construct a Diagram with a single PointLight at the origin, which
-- takes up no space.
pointLight :: (Typeable n, Num n, Ord n, Renderable (PointLight n) b)
              => Colour Double -- ^ The color of the light
              -> QDiagram b V3 n Any
pointLight c = mkQD (Prim $ PointLight origin c) mempty mempty 
               (Query . const . Any $ False)

-- | Construct a Diagram with a single ParallelLight, which takes up no space.
parallelLight :: (Typeable n, OrderedField n, Renderable (ParallelLight n) b)
                 => Direction V3 n -- ^ The direction in which the light travels.
                 -> Colour Double  -- ^ The color of the light.
                 -> QDiagram b V3 n Any
parallelLight d c = mkQD (Prim $ ParallelLight (fromDirection d) c)
                    mempty mempty (Query . const . Any $ False)
