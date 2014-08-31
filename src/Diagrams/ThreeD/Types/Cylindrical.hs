{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Diagrams.ThreeD.Types.Cylindrical
  ( -- * Data type
    Cylindrical
  , mkCylindrical, cylindrical, uncylindrical
  , cylindricalV3, cylindricalIso

    -- * Classes
  , Radial (..), Circle (..), Cylinder (..)
  , HasX (..), HasY (..), HasZ (..)

    -- * Basis elements
  , er, eθ, etheta, eh

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Rep
import           Data.Typeable
import           GHC.Generics                    (Generic1)

import           Diagrams.Angle
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Types.Spherical

import           Linear.Vector


-- | Space which has a radial, angular and height basis.
class Circle t => Cylinder t where
  _longitude   :: Lens' (t a) a
  _cylindrical :: Lens' (t a) (Cylindrical a)

eh :: Cylinder v => E v
eh = E (_cylindrical . cylindricalWrapper . _z)

newtype Cylindrical a = Cylindrical (V3 a)
  deriving (Monad, Functor, Typeable, MonadFix, Applicative, Traversable,
            Generic1, MonadZip, Foldable)

cylindricalWrapper :: Iso' (Cylindrical a) (V3 a)
cylindricalWrapper = iso (\(Cylindrical v) -> v) Cylindrical

mkCylindrical :: n -> Angle n -> n -> Cylindrical n
mkCylindrical r θ z = Cylindrical $ V3 r (θ ^. rad) z

cylindrical :: (n, Angle n, n) -> Cylindrical n
cylindrical (r,θ,z) = mkCylindrical r θ z

uncylindrical :: Cylindrical n -> (n, Angle n, n)
uncylindrical (Cylindrical (V3 r θ z)) = (r, θ @@ rad, z)

cylindricalIso :: Iso' (Cylindrical n) (n, Angle n, n)
cylindricalIso = iso uncylindrical cylindrical

instance Distributive Cylindrical where
  distribute f = Cylindrical $ V3 (fmap (\(Cylindrical (V3 x _ _)) -> x) f)
                                  (fmap (\(Cylindrical (V3 _ y _)) -> y) f)
                                  (fmap (\(Cylindrical (V3 _ _ z)) -> z) f)

instance Representable Cylindrical where
  type Rep Cylindrical = E Cylindrical
  tabulate f           = Cylindrical $ V3 (f er) (f eθ) (f eh)
  index xs (E l)       = view l xs

instance Radial Cylindrical where
  _radial = cylindricalWrapper . _x

instance Circle Cylindrical where
  _azimuth = cylindricalWrapper . _y . from rad
  _polar   = cylindricalWrapper . _xy . _Unwrapped'

instance Cylinder Cylindrical where
  _longitude   = cylindricalWrapper . _z
  _cylindrical = id

cylindricalV3 :: RealFloat n => Iso' (Cylindrical n) (V3 n)
cylindricalV3 = iso
  (\(Cylindrical (V3 r θ z)) -> V3 (r*cos θ) (r*sin θ) z)
  (\(V3 x y z)               -> Cylindrical $ V3 (sqrt $ x*x + y*y) (atan2 y x) z)

instance HasX Cylindrical where x_   = cylindricalV3 . _x
instance HasY Cylindrical where xy_  = cylindricalV3 . _xy
instance HasZ Cylindrical where xyz_ = cylindricalV3

instance HasR     Cylindrical where _r     = cylindricalV3 . _r
instance HasTheta Cylindrical where _theta = _azimuth
instance HasPhi   Cylindrical where _phi   = cylindricalV3 . _phi

