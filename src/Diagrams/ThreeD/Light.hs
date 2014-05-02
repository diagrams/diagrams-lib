{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

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
import           Diagrams.ThreeD.Types

data PointLight = PointLight P3 (Colour Double)
  deriving Typeable

data ParallelLight = ParallelLight R3 (Colour Double)
  deriving Typeable

type instance V PointLight = R3
type instance V ParallelLight = R3

instance Transformable PointLight where
    transform t (PointLight p c) = PointLight (transform t p) c

instance Transformable ParallelLight where
    transform t (ParallelLight v c) = ParallelLight (transform t v) c

-- | Construct a Diagram with a single PointLight at the origin, which
-- takes up no space.
pointLight :: (Backend b R3)
              => Colour Double -- ^ The color of the light
              -> Diagram R3
pointLight c = mkQD (Prim $ PointLight origin c) mempty mempty mempty
               (Query . const . Any $ False)

-- | Construct a Diagram with a single ParallelLight, which takes up no space.
parallelLight :: (Backend b R3)
                 => Direction -- ^ The direction in which the light travels.
                 -> Colour Double -- ^ The color of the light.
                 -> Diagram R3
parallelLight d c = mkQD (Prim $ ParallelLight (fromDirection d) c)
                    mempty mempty mempty (Query . const . Any $ False)
