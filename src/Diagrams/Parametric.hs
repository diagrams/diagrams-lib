{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Parametric
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type classes for things which are parameterized in some way, /e.g./
-- segments and trails.
--
-----------------------------------------------------------------------------
module Diagrams.Parametric
  (
    stdTolerance
  , Codomain, Parametric(..)

  , DomainBounds(..), domainBounds, EndValues(..), Sectionable(..), HasArcLength(..)

  ) where

import           Data.Kind (Type)
import           Diagrams.Core.V
import qualified Numeric.Interval.Kaucher as I

-- | Codomain of parametric classes.  This is usually either @(V p)@, for relative
--   vector results, or @(Point (V p))@, for functions with absolute coordinates.
type family Codomain p :: Type -> Type

-- | Type class for parametric functions.
class Parametric p where

  -- | 'atParam' yields a parameterized view of an object as a
  --   continuous function. It is designed to be used infix, like @path
  --   ``atParam`` 0.5@.
  atParam :: p -> N p -> Codomain p (N p)

-- | Type class for parametric functions with a bounded domain.  The
--   default bounds are @[0,1]@.
--
--   Note that this domain indicates the main \"interesting\" portion of the
--   function.  It must be defined within this range, but for some instances may
--   still have sensible values outside.
class DomainBounds p where
  -- | 'domainLower' defaults to being constantly 0 (for vector spaces with
  --   numeric scalars).
  domainLower :: p -> N p

  default domainLower :: Num (N p) => p -> N p
  domainLower = const 0

  -- | 'domainUpper' defaults to being constantly 1 (for vector spaces
  --   with numeric scalars).
  domainUpper :: p -> N p

  default domainUpper :: Num (N p) => p -> N p
  domainUpper = const 1

-- | Type class for querying the values of a parametric object at the
--   ends of its domain.
class (Parametric p, DomainBounds p) => EndValues p where
  -- | 'atStart' is the value at the start of the domain.  That is,
  --
  --   > atStart x = x `atParam` domainLower x
  --
  --   This is the default implementation, but some representations will
  --   have a more efficient and/or precise implementation.
  atStart :: p -> Codomain p (N p)
  atStart x = x `atParam` domainLower x

  -- | 'atEnd' is the value at the end of the domain. That is,
  --
  --   > atEnd x = x `atParam` domainUpper x
  --
  --   This is the default implementation, but some representations will
  --   have a more efficient and/or precise implementation.
  atEnd :: p -> Codomain p (N p)
  atEnd x = x `atParam` domainUpper x

-- | Return the lower and upper bounds of a parametric domain together
--   as a pair.
domainBounds :: DomainBounds p => p -> (N p, N p)
domainBounds x = (domainLower x, domainUpper x)

-- | Type class for parametric objects which can be split into
--   subobjects.
--
--   Minimal definition: Either 'splitAtParam' or 'section',
--   plus 'reverseDomain'.
class DomainBounds p => Sectionable p where
  -- | 'splitAtParam' splits an object @p@ into two new objects
  --   @(l,r)@ at the parameter @t@, where @l@ corresponds to the
  --   portion of @p@ for parameter values from @0@ to @t@ and @r@ for
  --   to that from @t@ to @1@.  The following property should hold:
  --
  -- @
  --   prop_splitAtParam f t u =
  --     | u < t     = atParam f u == atParam l (u / t)
  --     | otherwise = atParam f u == atParam f t ??? atParam l ((u - t) / (domainUpper f - t))
  --     where (l,r) = splitAtParam f t
  -- @
  --
  --   where @(???) = (^+^)@ if the codomain is a vector type, or
  --   @const flip@ if the codomain is a point type.  Stated more
  --   intuitively, all this is to say that the parameterization
  --   scales linearly with splitting.
  --
  --   'splitAtParam' can also be used with parameters outside the
  --   range of the domain.  For example, using the parameter @2@ with
  --   a path (where the domain is the default @[0,1]@) gives two
  --   result paths where the first is the original path extended to
  --   the parameter 2, and the second result path travels /backwards/
  --   from the end of the first to the end of the original path.
  splitAtParam :: p -> N p -> (p, p)
  splitAtParam x t
    = ( section x (domainLower x) t
      , section x t (domainUpper x))

  -- | Extract a particular section of the domain, linearly
  --   reparameterized to the same domain as the original.  Should
  --   satisfy the property:
  --
  -- > prop_section x l u t =
  -- >   let s = section x l u
  -- >   in     domainBounds x == domainBounds x
  -- >       && (x `atParam` lerp l u t) == (s `atParam` t)
  --
  --   That is, the section should have the same domain as the
  --   original, and the reparameterization should be linear.
  section :: p -> N p -> N p -> p
  default section :: Fractional (N p) => p -> N p -> N p -> p
  section x t1 t2 = snd (splitAtParam (fst (splitAtParam x t2)) (t1/t2))

  -- | Flip the parameterization on the domain.
  reverseDomain :: p -> p

-- | The standard tolerance used by @std...@ functions (like
--   'stdArcLength' and 'stdArcLengthToParam', currently set at
--   @1e-6@.
stdTolerance :: Fractional a => a
stdTolerance = 1e-6

-- | Type class for parametric things with a notion of arc length.
class Parametric p => HasArcLength p where

  -- | @arcLengthBounded eps x@ approximates the arc length of @x@.
  --   The true arc length is guaranteed to lie within the interval
  --   returned, which will have a size of at most @eps@.
  arcLengthBounded :: N p -> p -> I.Interval (N p)

  -- | @arcLength eps s@ approximates the arc length of @x@ up to the
  --   accuracy @eps@ (plus or minus).
  arcLength :: N p -> p -> N p
  default arcLength :: Fractional (N p) => N p -> p -> N p
  arcLength eps = I.midpoint . arcLengthBounded eps

  -- | Approximate the arc length up to a standard accuracy of
  --   'stdTolerance' (@1e-6@).
  stdArcLength :: p -> N p
  default stdArcLength :: Fractional (N p) => p -> N p
  stdArcLength = arcLength stdTolerance

  -- | @'arcLengthToParam' eps s l@ converts the absolute arc length
  --   @l@, measured from the start of the domain, to a parameter on
  --   the object @s@.  The true arc length at the parameter returned
  --   is guaranteed to be within @eps@ of the requested arc length.
  --
  --   This should work for /any/ arc length, and may return any
  --   parameter value (not just parameters in the domain).
  arcLengthToParam :: N p -> p -> N p -> N p

  -- | A simple interface to convert arc length to a parameter,
  --   guaranteed to be accurate within 'stdTolerance', or @1e-6@.
  stdArcLengthToParam :: p -> N p -> N p
  default stdArcLengthToParam :: Fractional (N p) => p -> N p -> N p
  stdArcLengthToParam = arcLengthToParam stdTolerance

