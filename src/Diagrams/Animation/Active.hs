{-# LANGUAGE TypeFamilies
  #-}

module Diagrams.Animation.Active where

import Graphics.Rendering.Diagrams
import Control.Applicative (pure)

import Diagrams.Path

import Data.Active

type instance V (Active a) = V a

-- XXX all these instances should go with their classes

instance HasOrigin a => HasOrigin (Active a) where
  moveOriginTo = fmap . moveOriginTo

instance Transformable a => Transformable (Active a) where
  transform = fmap . transform
  
instance HasStyle a => HasStyle (Active a) where
  applyStyle = fmap . applyStyle
  
instance PathLike p => PathLike (Active p) where
  pathLike st cl segs = pure (pathLike st cl segs)
  
-- XXX Juxtaposable