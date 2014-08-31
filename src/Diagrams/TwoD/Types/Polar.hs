{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Diagrams.TwoD.Types.Polar
  ( -- * Polar type
    Polar
  , mkPolar, polar, unpolar, polarIso, polarV2

    -- * Polar functions
  , interpPolar

    -- * Classes
  , Radial (..), Circle (..)
  , HasX (..), HasY (..), HasR (..)

    -- * Basis elements
  , er, eθ, etheta,

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Rep
import           Data.Typeable
import           GHC.Generics        (Generic1)

import           Diagrams.Angle
import           Diagrams.TwoD.Types

import           Linear.Affine
import           Linear.Metric
import           Linear.V3
import           Linear.Vector


newtype Polar a = Polar (V2 a)
  deriving (Monad, Functor, Typeable, MonadFix, Applicative, Traversable,
            Generic1, MonadZip, Foldable)

makeWrapped ''Polar

-- can't make reasonable Additive instance

instance Distributive Polar where
  distribute f = Polar $ V2 (fmap (\(Polar (V2 x _)) -> x) f)
                            (fmap (\(Polar (V2 _ y)) -> y) f)

instance Representable Polar where
  type Rep Polar = E Polar
  tabulate f     = Polar $ V2 (f er) (f eθ)
  index xs (E l) = view l xs

instance Circle Polar where
  _azimuth = polarWrapper . _y . from rad
  _polar   = id

-- | Construct a 'Polar' from a magnitude and an 'Angle'.
mkPolar :: n -> Angle n -> Polar n
mkPolar r θ = Polar $ V2 r (θ^.rad)

-- | Construct a 'Polar' from a magnitude and 'Angle' tuple.
polar :: (n, Angle n) -> Polar n
polar = uncurry mkPolar

-- | Turn a 'Polar' back into a magnitude and 'Angle' tuple.
unpolar :: Polar n -> (n, Angle n)
unpolar (Polar (V2 r θ)) = (r, θ @@ rad)

-- | 'Iso'' between 'Polar' and its tuple form.
polarIso :: Iso' (Polar n) (n, Angle n)
polarIso = iso unpolar polar

-- | Numerical 'Iso'' between 'Polar' and 'R2'.
polarV2 :: RealFloat n => Iso' (Polar n) (V2 n)
polarV2 = iso (\(Polar (V2 r θ)) -> V2 (r * cos θ) (r * sin θ))
               (\v@(V2 x y)       -> Polar $ V2 (norm v) (atan2 y x))

-- internal iso for instances
polarWrapper :: Iso' (Polar a) (V2 a)
polarWrapper = iso (\(Polar v) -> v) Polar

-- | Polar interpolation between two polar coordinates.
interpPolar :: Num n => n -> Polar n -> Polar n -> Polar n
interpPolar t (Polar a) (Polar b) = Polar (lerp t a b)


-- | Space which has a radial length basis. For Polar and Cylindrical this is
--   the radius of the circle in the xy-plane. For Spherical this is the
--   distance from the origin.
class Radial t where
  _radial :: Lens' (t a) a

instance Radial Polar where
  _radial = polarWrapper . _x

-- | Space which has a radial and angular basis.
class Radial t => Circle t where
  _azimuth :: Lens' (t a) (Angle a)
  _polar   :: Lens' (t a) (Polar a)

er :: Circle v => E v
er = E _radial

eθ, etheta :: Circle v => E v
eθ     = E (_polar . polarWrapper . _y)
etheta = eθ

-- | Coordinate with at least one dimension where the x coordinate can be
--   retreived numerically. Note this differs slightly from 'R1' which requires
--   a lens for all values. This allows instances for different coordinates
--   such as 'Polar', where the x coordinate can only be retreived numerically.
class HasX t where
  x_ :: RealFloat n => Lens' (t n) n

instance HasX v => HasX (Point v) where
  x_ = lensP . x_

instance HasX V2    where x_ = _x
instance HasX V3    where x_ = _x
instance HasX Polar where x_ = polarV2 . _x

-- | Coordinate with at least two dimensions where the x and y coordinates can be
--   retreived numerically.
class HasX t => HasY t where
  y_ :: RealFloat n => Lens' (t n) n
  y_ = xy_ . _y

  xy_ :: RealFloat n => Lens' (t n) (V2 n)

instance HasY v => HasY (Point v) where
  xy_ = lensP . xy_

instance HasY V2    where xy_ = id
instance HasY V3    where xy_ = _xy
instance HasY Polar where xy_ = polarV2

