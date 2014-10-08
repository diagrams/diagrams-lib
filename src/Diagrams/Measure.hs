{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Diagrams.Measure
  ( Measured (..)
  , Measure
  , fromMeasured
  , output
  , local
  , global
  , normalized
  , scaleLocal
  , atLeast
  , atMost
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Control.Monad.Reader as R
import           Data.Distributive
import           Data.Typeable

import           Linear.Vector

-- (local, global, normalized) -> output
newtype Measured n a = Measured { unMeasure :: (n,n,n) -> a }
  deriving (Typeable, Functor, Applicative, Monad, Additive, R.MonadReader (n,n,n))

type Measure n = Measured n n

-- | @fromMeasured globalScale normalizedScale measure -> a@
fromMeasured :: Num n => n -> n -> Measured n a -> a
fromMeasured g n (Measured m) = m (1,g,n)

output :: Num n => n -> Measure n
output = pure

local :: Num n => n -> Measure n
local x = views _1 (*x)

global :: Num n => n -> Measure n
global x = views _2 (*x)

normalized :: Num n => n -> Measure n
normalized x = views _3 (*x)

scaleLocal :: Num n => n -> Measured n a -> Measured n a
scaleLocal s = R.local (_1 *~ s)

atLeast :: Ord n => Measure n -> Measure n -> Measure n
atLeast = liftA2 min

atMost :: Ord n => Measure n -> Measure n -> Measure n
atMost = liftA2 max

instance Num a => Num (Measured n a) where
  (+) = (^+^)
  (-) = (^-^)
  (*) = liftA2 (*)

  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional a => Fractional (Measured n a) where
  (/)   = liftA2 (/)
  recip = fmap recip

  fromRational = pure . fromRational

instance Floating a => Floating (Measured n a) where
  pi      = pure pi
  exp     = fmap exp
  sqrt    = fmap sqrt
  log     = fmap log
  (**)    = liftA2 (**)
  logBase = liftA2 logBase
  sin     = fmap sin
  tan     = fmap tan
  cos     = fmap cos
  asin    = fmap asin
  atan    = fmap atan
  acos    = fmap acos
  sinh    = fmap sinh
  tanh    = fmap tanh
  cosh    = fmap cosh
  asinh   = fmap asinh
  atanh   = fmap atanh
  acosh   = fmap acosh

-- exotic instances

instance Distributive (Measured n) where
  distribute a = Measured $ \x -> fmap (\(Measured m) -> m x) a

instance Profunctor Measured where
  lmap f (Measured m) = Measured $ \(l,g,n) -> m (f l, f g, f n)
  rmap f (Measured m) = Measured $ f . m

