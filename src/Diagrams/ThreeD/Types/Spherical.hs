{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Diagrams.ThreeD.Types.Spherical
  ( -- * Data type
    Spherical
  , mkSpherical, spherical, unspherical, sphericalIso, sphericalV3

    -- ** Spherical functions
  , interpSpherical

    -- * Classes
  , Radial (..), Circle (..), Sphere (..)
  , HasX (..), HasY (..), HasZ (..), HasR (..)

    -- * Basis elements
  , er, eθ, etheta, eφ, ephi

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Rep
import           Data.Typeable
import           GHC.Generics              (Generic1)

import           Diagrams.Angle
import           Diagrams.TwoD.Types.Polar

import           Linear.Affine
import           Linear.Metric
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.Vector

-- | Space which has a radial and two angular basis. The inclination is the
--   positive angles from the z-axis.
class Circle t => Sphere t where
  _inclination :: Lens' (t a) (Angle a)
  _spherical   :: Lens' (t a) (Spherical a)

eφ, ephi :: Sphere v => E v
eφ   = E (_spherical . sphericalV3 . _z)
ephi = eφ

newtype Spherical a = Spherical (V3 a)
  deriving (Monad, Functor, Typeable, MonadFix, Applicative, Traversable,
            Generic1, MonadZip, Foldable)

sphericalV3 :: Iso' (Spherical a) (V3 a)
sphericalV3 = iso (\(Spherical v) -> v) Spherical

mkSpherical :: n -> Angle n -> Angle n -> Spherical n
mkSpherical r θ φ = Spherical $ V3 r (θ ^. rad) (φ ^. rad)

spherical :: (n, Angle n, Angle n) -> Spherical n
spherical (n, θ, φ) = mkSpherical n θ φ

unspherical :: Spherical n -> (n, Angle n, Angle n)
unspherical (Spherical (V3 r θ φ)) = (r, θ @@ rad, φ @@ rad)

-- | Linear interpolation between spherical coordinates.
interpSpherical :: Num n => n -> Spherical n -> Spherical n -> Spherical n
interpSpherical t (Spherical a) (Spherical b) = Spherical $ lerp t a b

instance Distributive Spherical where
  distribute f = Spherical $ V3 (fmap (\(Spherical (V3 x _ _)) -> x) f)
                                (fmap (\(Spherical (V3 _ y _)) -> y) f)
                                (fmap (\(Spherical (V3 _ _ z)) -> z) f)

instance Representable Spherical where
  type Rep Spherical = E Spherical
  tabulate f         = Spherical $ V3 (f er) (f eθ) (f eφ)
  index xs (E l)     = view l xs

instance Radial Spherical where
  _radial = sphericalV3 . _x

instance Circle Spherical where
  _azimuth = sphericalV3 . _y . from rad
  _polar   = sphericalV3 . _xy . _Unwrapped'

instance Sphere Spherical where
  _inclination = sphericalV3 . _z . from rad
  _spherical   = id

sphericalIso :: RealFloat n => Iso' (Spherical n) (V3 n)
sphericalIso = iso
  (\(Spherical (V3 r θ φ)) -> V3 (r * cos θ * sin φ) (r * sin θ * sin φ) (r * cos φ))
  (\v@(V3 x y z)           -> let r = norm v
                              in  Spherical $ V3 r (atan2 y x) (acos (z / r)))

-- | Coordinate with at least three dimensions where the x, y and z coordinate can be
--   retreived numerically.
class HasY t => HasZ t where
  z_ :: RealFloat n => Lens' (t n) n
  z_ = xyz_ . _z

  xyz_ :: RealFloat n => Lens' (t n) (V3 n)

instance HasZ v => HasZ (Point v) where
  xyz_ = lensP . xyz_

instance HasZ V3 where xyz_ = id

instance HasX Spherical where x_   = sphericalIso . _x
instance HasY Spherical where xy_  = sphericalIso . _xy
instance HasZ Spherical where xyz_ = sphericalIso

instance HasR     Spherical where _r     = _radial
instance HasTheta Spherical where _theta = _azimuth
instance HasPhi   Spherical where _phi   = _inclination

