{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances          #-}

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

data PointLight v = PointLight (Point v) (Colour Double)
  deriving Typeable

data ParallelLight v = ParallelLight v (Colour Double)
  deriving Typeable

type instance V (PointLight v) = v
type instance V (ParallelLight v) = v

instance (R3Ish v) => Transformable (PointLight v) where
    transform t (PointLight p c) = PointLight (transform t p) c

instance (R3Ish v) => Transformable (ParallelLight v) where
    transform t (ParallelLight v c) = ParallelLight (transform t v) c

-- | Construct a Diagram with a single PointLight at the origin, which
-- takes up no space.
pointLight :: (Backend b v, Renderable (PointLight v) b, R3Ish v)
              => Colour Double -- ^ The color of the light
              -> Diagram b v
pointLight c = mkQD (Prim $ PointLight origin c) mempty mempty mempty
               (Query . const . Any $ False)

-- | Construct a Diagram with a single ParallelLight, which takes up no space.
parallelLight :: (Backend b v, Renderable (ParallelLight v) b, R3Ish v)
                 => Direction v -- ^ The direction in which the light travels.
                 -> Colour Double -- ^ The color of the light.
                 -> Diagram b v
parallelLight d c = mkQD (Prim $ ParallelLight (fromDirection d) c)
                    mempty mempty mempty (Query . const . Any $ False)
