{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Coordinates
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Nice syntax for constructing and pattern-matching on literal
-- points and vectors.
--
-----------------------------------------------------------------------------

module Diagrams.Coordinates
    ( (:&)(..), Coordinates(..)

    -- * Lenses for particular axes
    , HasX(..), HasY(..), HasZ(..)
    )
    where

import           Control.Lens          (Lens')

import           Diagrams.Core.Points

-- | A pair of values, with a convenient infix (left-associative)
--   data constructor.
data a :& b = a :& b
  deriving (Eq, Ord, Show)

infixl 7 :&


-- | Types which are instances of the @Coordinates@ class can be
--   constructed using '^&' (for example, a three-dimensional vector
--   could be constructed by @1 ^& 6 ^& 3@), and deconstructed using
--   'coords'.  A common pattern is to use 'coords' in conjunction
--   with the @ViewPatterns@ extension, like so:
--
-- @
-- foo :: Vector3 -> ...
-- foo (coords -> x :& y :& z) = ...
-- @
class Coordinates c where

  -- | The type of the final coordinate.
  type FinalCoord c    :: *

  -- | The type of everything other than the final coordinate.
  type PrevDim c       :: *

  -- | Decomposition of @c@ into applications of ':&'.
  type Decomposition c :: *
    -- Decomposition c = Decomposition (PrevDim c) :& FinalCoord c  (essentially)

  -- | Construct a value of type @c@ by providing something of one
  --   less dimension (which is perhaps itself recursively constructed
  --   using @(^&)@) and a final coordinate.  For example,
  --
  -- @
  -- 2 ^& 3 :: P2
  -- 3 ^& 5 ^& 6 :: R3
  -- @
  --
  --   Note that @^&@ is left-associative.
  (^&)    :: PrevDim c -> FinalCoord c -> c

  -- | Prefix synonym for @^&@. pr stands for pair of @PrevDim@, @FinalCoord@
  pr      :: PrevDim c -> FinalCoord c -> c
  pr = (^&)

  -- | Decompose a value of type @c@ into its constituent coordinates,
  --   stored in a nested @(:&)@ structure.
  coords :: c -> Decomposition c

infixl 7 ^&

-- Some standard instances for plain old tuples

instance Coordinates (a,b) where
  type FinalCoord (a,b)    = b
  type PrevDim (a,b)       = a
  type Decomposition (a,b) = a :& b

  x ^& y                    = (x,y)
  coords (x,y)             = x :& y

instance Coordinates (a,b,c) where
  type FinalCoord (a,b,c)    = c
  type PrevDim (a,b,c)       = (a,b)
  type Decomposition (a,b,c) = Decomposition (a,b) :& c

  (x,y) ^& z                  = (x,y,z)
  coords (x,y,z)             = coords (x,y) :& z

instance Coordinates (a,b,c,d) where
  type FinalCoord (a,b,c,d)    = d
  type PrevDim (a,b,c,d)       = (a,b,c)
  type Decomposition (a,b,c,d) = Decomposition (a,b,c) :& d

  (w,x,y)  ^& z                  = (w,x,y,z)
  coords (w,x,y,z)             = coords (w,x,y) :& z

instance Coordinates v => Coordinates (Point v) where
  type FinalCoord (Point v)    = FinalCoord v
  type PrevDim (Point v)       = PrevDim v
  type Decomposition (Point v) = Decomposition v

  x ^& y        = P (x ^& y)
  coords (P v) = coords v

-- | The class of types with at least one coordinate, called _x.
class HasX t where
    _x :: Lens' t Double

-- | The class of types with at least two coordinates, the second called _y.
class HasY t where
    _y :: Lens' t Double

-- | The class of types with at least three coordinates, the third called _z.
class HasZ t where
    _z :: Lens' t Double
