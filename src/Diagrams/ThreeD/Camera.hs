{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Data.VectorSpace

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Direction
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

-- Parameterize Camera on the lens type, so that Backends can express which
-- lenses they handle.
data Camera l = Camera
    { camLoc  :: Point (V l)
    , forward :: V l
    , up      :: V l
    , lens    :: l
    }
  deriving Typeable

class (Typeable l, Typeable (V l)) => CameraLens l where
    -- | The natural aspect ratio of the projection.
    aspect :: l -> Scalar (V l)

-- | A perspective projection
data PerspectiveLens v = PerspectiveLens
                     { _horizontalFieldOfView :: Angle (Scalar v) -- ^ Horizontal field of view.
                     , _verticalFieldOfView   :: Angle (Scalar v) -- ^ Vertical field of view.
                     }
  deriving Typeable

makeLenses ''PerspectiveLens

type instance V (PerspectiveLens v) = v

instance (R3Ish v) => CameraLens (PerspectiveLens v) where
    aspect (PerspectiveLens h v) = angleRatio h v

-- | An orthographic projection
data OrthoLens v = OrthoLens
               { _orthoWidth  :: Scalar v -- ^ Width
               , _orthoHeight :: Scalar v -- ^ Height
               }
  deriving Typeable

makeLenses ''OrthoLens

type instance V (OrthoLens v) = v

instance (R3Ish v) => CameraLens (OrthoLens v) where
    aspect (OrthoLens h v) = h / v

type instance V (Camera l) = V l

instance (R3Ish (V l)) => Transformable (Camera l) where
  transform t (Camera p f u l) =
      Camera (transform t p)
             (transform t f)
             (transform t u)
             l

instance (R3Ish (V l)) => Renderable (Camera l) NullBackend where
    render _ _ = mempty

-- | A camera at the origin facing along the negative Z axis, with its
-- up-axis coincident with the positive Y axis.  The field of view is
-- chosen to match a 50mm camera on 35mm film. Note that Cameras take
-- up no space in the Diagram.
mm50Camera :: (R3Ish v, Backend b v, Renderable (Camera (PerspectiveLens v)) b) => Diagram b v
mm50Camera = facing_ZCamera mm50

-- | 'facing_ZCamera l' is a camera at the origin facing along the
-- negative Z axis, with its up-axis coincident with the positive Y
-- axis, with the projection defined by l.
facing_ZCamera :: (R3Ish v, V l ~ v, CameraLens l, Backend b v, Renderable (Camera l) b) =>
                  l -> Diagram b v
facing_ZCamera l = mkQD (Prim $ Camera origin unit_Z unitY l)
        mempty mempty mempty (Query . const . Any $ False)

mm50, mm50Wide, mm50Narrow :: (R3Ish v) => PerspectiveLens v

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg)

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide = PerspectiveLens (43.2 @@ deg)  (27 @@ deg)

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resolutions.
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg)

camForward :: (R3Ish v, v ~ V l) => Camera l -> Direction v
camForward = direction . forward

camUp :: (R3Ish v, v ~ V l) => Camera l -> Direction v
camUp = direction . up

camRight :: (R3Ish v, v ~ V l) => Camera l -> Direction v
camRight c = direction right where
  right = cross3 (forward c) (up c)

camLens :: (R3Ish v, v ~ V l) => Camera l -> l
camLens = lens

camAspect :: (R3Ish v, v ~ V l, CameraLens l) => Camera l -> Scalar v
camAspect = aspect . camLens
