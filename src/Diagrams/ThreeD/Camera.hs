{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Camera
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Types to specify viewpoint for 3D rendering.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Camera
       ( Camera  -- do not export constructor
        -- These are safe to construct manually
       , PerspectiveLens(..), OrthoLens(..)
       , horizontalFieldOfView, verticalFieldOfView
       , orthoWidth, orthoHeight
       , camLoc, camForward, camUp, camRight, camLens
       , facing_ZCamera, mm50Camera
       , mm50, mm50Wide, mm50Narrow
       , aspect, camAspect
       )
       where

import           Control.Lens           (makeLenses)
import           Data.Cross
import           Data.Monoid
import           Data.Typeable

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

-- Parameterize Camera on the lens type, so that Backends can express which
-- lenses they handle.
data Camera l = Camera
    { camLoc  :: P3
    , forward :: R3
    , up      :: R3
    , lens    :: l
    }
  deriving Typeable

class Typeable l => CameraLens l where
    -- | The natural aspect ratio of the projection.
    aspect :: l -> Double

-- | A perspective projection
data PerspectiveLens = PerspectiveLens
                     { _horizontalFieldOfView :: Angle -- ^ Horizontal field of view.
                     , _verticalFieldOfView   :: Angle -- ^ Vertical field of view.
                     }
  deriving Typeable

makeLenses ''PerspectiveLens

instance CameraLens PerspectiveLens where
    aspect (PerspectiveLens h v) = angleRatio h v

-- | An orthographic projection
data OrthoLens = OrthoLens
               { _orthoWidth  :: Double -- ^ Width
               , _orthoHeight :: Double -- ^ Height
               }
  deriving Typeable

makeLenses ''OrthoLens

instance CameraLens OrthoLens where
    aspect (OrthoLens h v) = h / v

type instance V (Camera l) = R3

instance Transformable (Camera l) where
  transform t (Camera p f u l) =
      Camera (transform t p)
             (transform t f)
             (transform t u)
             l

-- | A camera at the origin facing along the negative Z axis, with its
-- up-axis coincident with the positive Y axis.  The field of view is
-- chosen to match a 50mm camera on 35mm film. Note that Cameras take
-- up no space in the Diagram.
mm50Camera :: Diagram R3
mm50Camera = facing_ZCamera mm50

-- | 'facing_ZCamera l' is a camera at the origin facing along the
-- negative Z axis, with its up-axis coincident with the positive Y
-- axis, with the projection defined by l.
facing_ZCamera :: (CameraLens l) =>
                  l -> Diagram R3
facing_ZCamera l = mkQD (Prim $ Camera origin unit_Z unitY l)
        mempty mempty mempty (Query . const . Any $ False)

mm50, mm50Wide, mm50Narrow :: PerspectiveLens

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg)

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide = PerspectiveLens (43.2 @@ deg)  (27 @@ deg)

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resulotions.
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg)

camForward :: Camera l -> Direction
camForward = direction . forward

camUp :: Camera l -> Direction
camUp = direction . up

camRight :: Camera l -> Direction
camRight c = direction right where
  right = cross3 (forward c) (up c)

camLens :: Camera l -> l
camLens = lens

camAspect :: CameraLens l => Camera l -> Double
camAspect = aspect . camLens
