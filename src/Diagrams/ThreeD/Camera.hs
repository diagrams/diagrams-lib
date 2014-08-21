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
import           Data.Monoid
import           Data.Typeable

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Direction
import           Diagrams.ThreeD.Types ()
import           Diagrams.ThreeD.Vector

import Linear.V3

-- Parameterize Camera on the lens type, so that Backends can express which
-- lenses they handle.
data Camera l = Camera
    { camLoc  :: Point (V l) (N l)
    , forward :: VN l
    , up      :: VN l
    , lens    :: l
    }
  deriving Typeable

class (Typeable l, Typeable (VN l)) => CameraLens l where
    -- | The natural aspect ratio of the projection.
    aspect :: l -> N l

-- | A perspective projection
data PerspectiveLens n = PerspectiveLens
  { _horizontalFieldOfView :: Angle n -- ^ Horizontal field of view.
  , _verticalFieldOfView   :: Angle n -- ^ Vertical field of view.
  }
  deriving Typeable

makeLenses ''PerspectiveLens

type instance V (PerspectiveLens n) = V3
type instance N (PerspectiveLens n) = n

instance (Floating n, Typeable n) => CameraLens (PerspectiveLens n) where
  aspect (PerspectiveLens h v) = angleRatio h v

-- | An orthographic projection
data OrthoLens n = OrthoLens
               { _orthoWidth  :: n -- ^ Width
               , _orthoHeight :: n -- ^ Height
               }
  deriving Typeable

makeLenses ''OrthoLens

type instance V (OrthoLens n) = V3
type instance N (OrthoLens n) = n

instance (Typeable n, Fractional n) => CameraLens (OrthoLens n) where
    aspect (OrthoLens h v) = h / v

type instance V (Camera l) = V l
type instance N (Camera l) = N l

instance (VN l ~ V3 n, Num n) => Transformable (Camera l) where
  transform t (Camera p f u l) =
      Camera (transform t p)
             (transform t f)
             (transform t u)
             l

instance (VN l ~ V3 n, Num n) => Renderable (Camera l) NullBackend where
    render _ _ = mempty

-- | A camera at the origin facing along the negative Z axis, with its
-- up-axis coincident with the positive Y axis.  The field of view is
-- chosen to match a 50mm camera on 35mm film. Note that Cameras take
-- up no space in the Diagram.
mm50Camera :: (Typeable n, Floating n, Ord n, Backend b V3 n, Renderable (Camera (PerspectiveLens n)) b)
           => Diagram b V3 n
mm50Camera = facing_ZCamera mm50

-- | 'facing_ZCamera l' is a camera at the origin facing along the
-- negative Z axis, with its up-axis coincident with the positive Y
-- axis, with the projection defined by l.
facing_ZCamera :: (VN l ~ V3 n, Floating n, Ord n, CameraLens l, Backend b V3 n, Renderable (Camera l) b) =>
                  l -> Diagram b V3 n
facing_ZCamera l = mkQD (Prim $ Camera origin unit_Z unitY l)
        mempty mempty mempty (Query . const . Any $ False)

mm50, mm50Wide, mm50Narrow :: Floating n => PerspectiveLens n

-- | mm50 has the field of view of a 50mm lens on standard 35mm film,
-- hence an aspect ratio of 3:2.
mm50 = PerspectiveLens (40.5 @@ deg) (27 @@ deg)

-- | mm50blWide has the same vertical field of view as mm50, but an
-- aspect ratio of 1.6, suitable for wide screen computer monitors.
mm50Wide = PerspectiveLens (43.2 @@ deg)  (27 @@ deg)

-- | mm50Narrow has the same vertical field of view as mm50, but an
-- aspect ratio of 4:3, for VGA and similar computer resolutions.
mm50Narrow = PerspectiveLens (36 @@ deg) (27 @@ deg)

camForward :: (VN l ~ V3 n, Fractional n) => Camera l -> Direction V3 n
camForward = direction . forward

camUp :: (VN l ~ V3 n, Fractional n) => Camera l -> Direction V3 n
camUp = direction . up

camRight :: (VN l ~ V3 n, Fractional n) => Camera l -> Direction V3 n
camRight c = direction right where
  right = cross (forward c) (up c)

camLens :: (V3 ~ V l) => Camera l -> l
camLens = lens

camAspect :: (VN l ~ V3 n, CameraLens l) => Camera l -> n
camAspect = aspect . camLens

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
