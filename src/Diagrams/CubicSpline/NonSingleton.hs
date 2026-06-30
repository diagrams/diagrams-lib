{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTSyntax #-}

-- |
-- Module      :  Diagrams.CubicSpline.NonSingleton
-- Copyright   :  (c) 2026 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A simple type representing lists with at least 2 elements, needed
-- as input for the cubic spline solver.
module Diagrams.CubicSpline.NonSingleton (
  -- * Non-singleton lists
  NonSingleton (..),
  toNonEmpty,
  tail,
  zipWith,
  dupFirst,
  dupLast,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Prelude hiding (tail, zipWith)

-- | Lists with at least 2 elements.
data NonSingleton a where
  (:||) :: a -> NonEmpty a -> NonSingleton a
  deriving (Functor)

infixr 5 :||

toNonEmpty :: NonSingleton a -> NonEmpty a
toNonEmpty (x :|| xs) = NE.cons x xs

dupFirst :: NonSingleton a -> NonSingleton a
dupFirst (x :|| xs) = x :|| NE.cons x xs

dupLast :: NonEmpty a -> NonSingleton a
dupLast (x :| []) = x :|| (x :| [])
dupLast (x :| (y : ys)) = x :|| (y :| (ys ++ [NE.last (y :| ys)]))

zipWith :: (a -> b -> c) -> NonSingleton a -> NonSingleton b -> NonSingleton c
zipWith f (x :|| xs) (y :|| ys) = f x y :|| NE.zipWith f xs ys

tail :: NonSingleton a -> NonEmpty a
tail (_ :|| xs) = xs
