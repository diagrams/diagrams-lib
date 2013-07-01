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
  -- * Parametric functions
    stdTolerance
  , Codomain, Parametric(..), HasArcLength(..)

  , DomainBounds(..), EndValues(..), Sectionable(..)

  -- * Adjusting
  , adjust
  , AdjustOpts(..), AdjustMethod(..), AdjustSide(..)

  ) where

import           Diagrams.Core
import           Diagrams.Util

import           Data.Default.Class
import           Data.VectorSpace
import qualified Numeric.Interval   as I

-- | Codomain of parametric classes.  This is usually either @(V p)@, for relative
--   vector results, or @(Point (V p))@, for functions with absolute coordinates.
type family Codomain p :: *

-- | Type class for parametric functions.
class Parametric p where

  -- | 'atParam' yields a parameterized view of an object as a
  --   continuous function. It is designed to be used infix, like @path
  --   ``atParam`` 0.5@.
  atParam :: p -> Scalar (V p) -> Codomain p

-- | Type class for parametric functions with a bounded domain.  The
--   default bounds are @[0,1]@.
--
--   Note that this domain indicates the main \"interesting\" portion of the
--   function.  It must be defined within this range, but for some instances may
--   still have sensible values outside.
class DomainBounds p where
  -- | 'domainLower' defaults to being constantly 0 (for vector spaces with
  --   numeric scalars).
  domainLower :: p -> Scalar (V p)

  default domainLower :: Num (Scalar (V p)) => p -> Scalar (V p)
  domainLower = const 0

  -- | 'domainUpper' defaults to being constantly 1 (for vector spaces
  --   with numeric scalars).
  domainUpper :: p -> Scalar (V p)

  default domainUpper :: Num (Scalar (V p)) => p -> Scalar (V p)
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
  atStart :: p -> Codomain p
  atStart x = x `atParam` domainLower x

  -- | 'atEnd' is the value at the end of the domain. That is,
  --
  --   > atEnd x = x `atParam` domainUpper x
  --
  --   This is the default implementation, but some representations will
  --   have a more efficient and/or precise implementation.
  atEnd :: p -> Codomain p
  atEnd x = x `atParam` domainUpper x

-- | Return the lower and upper bounds of a parametric domain together
--   as a pair.
domainBounds :: DomainBounds p => p -> (Scalar (V p), Scalar (V p))
domainBounds x = (domainLower x, domainUpper x)

-- | Type class for parametric objects which can be split into
--   subobjects.
--
--   Minimal definition: Either 'splitAtParam' or 'section'.
class DomainBounds p => Sectionable p where
  -- | 'splitAtParam' splits an object @p@ into two new objects
  --   @(l,r)@ at the parameter @t@, where @l@ corresponds to the
  --   portion of @p@ for parameter values from @0@ to @t@ and @r@ for
  --   to that from @t@ to @1@.  The following property should hold:
  --
  -- > prop_splitAtParam f t u =
  -- >   | u < t     = atParam f u == atParam l (u / t)
  -- >   | otherwise = atParam f u == atParam f t ??? atParam l ((u - t) / (domainUpper f - t))
  -- >   where (l,r) = splitAtParam f t
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
  splitAtParam :: p -> Scalar (V p) -> (p, p)
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
  section :: p -> Scalar (V p) -> Scalar (V p) -> p
  default section :: Fractional (Scalar (V p)) => p -> Scalar (V p) -> Scalar (V p) -> p
  section x t1 t2 = snd (splitAtParam (fst (splitAtParam x t2)) (t1/t2))

  -- | Flip the parameterization on the domain.  This has the
  --   following default definition:
  --
  -- > reverse x = section x (domainUpper x) (domainLower x)
  reverseDomain :: p -> p
  reverseDomain x = section x (domainUpper x) (domainLower x)

-- | XXX comment me
stdTolerance :: Fractional a => a
stdTolerance = 1e-6

-- | Type class for parametric things with a notion of arc length.
--   XXX finish me
class Parametric p => HasArcLength p where

  -- | @arcLengthBounded eps x@ approximates the arc length of @x@.
  --   The true arc length is guaranteed to lie within the interval
  --   returned, which will have a size of at most @eps@.
  arcLengthBounded :: Scalar (V p) -> p -> I.Interval (Scalar (V p))

  -- | @arcLength eps s@ approximates the arc length of @x@ up to the
  --   accuracy @eps@ (plus or minus).
  arcLength :: Scalar (V p) -> p -> Scalar (V p)
  default arcLength :: Fractional (Scalar (V p)) => Scalar (V p ) -> p -> Scalar (V p)
  arcLength eps = I.midpoint . arcLengthBounded eps

  -- | XXX comment me
  stdArcLength :: p -> Scalar (V p)
  default stdArcLength :: Fractional (Scalar (V p)) => p -> Scalar (V p)
  stdArcLength = arcLength stdTolerance

  -- | @'arcLengthToParam' eps s l@ converts the absolute arc length
  --   @l@, measured from the start of the domain, to a parameter on
  --   the object @s@.  The true arc length at the parameter returned
  --   is guaranteed to be within @eps@ of the requested arc length.
  --
  --   This should work for /any/ arc length, and may return any
  --   parameter value (not just parameters in the domain).
  arcLengthToParam :: Scalar (V p) -> p -> Scalar (V p) -> Scalar (V p)

  -- | A simple interface to convert arc length to a parameter,
  --   guaranteed to be accurate within the standard tolerance of
  --   @1e-6@.
  stdArcLengthToParam :: p -> Scalar (V p) -> Scalar (V p)
  default stdArcLengthToParam :: Fractional (Scalar (V p))
                              => p -> Scalar (V p) -> Scalar (V p)
  stdArcLengthToParam = arcLengthToParam stdTolerance

--------------------------------------------------
--  Adjusting length
--------------------------------------------------

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod v = ByParam (Scalar v)     -- ^ Extend by the given parameter value
                                             --   (use a negative parameter to shrink)
                    | ByAbsolute (Scalar v)  -- ^ Extend by the given arc length
                                             --   (use a negative length to shrink)
                    | ToAbsolute (Scalar v)  -- ^ Extend or shrink to the given
                                             --   arc length

-- | Which side of a segment, trail, or path should be adjusted?
data AdjustSide = Start  -- ^ Adjust only the beginning
                | End    -- ^ Adjust only the end
                | Both   -- ^ Adjust both sides equally
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | How should a segment, trail, or path be adjusted?
data AdjustOpts v = AO { adjMethod       :: AdjustMethod v
                       , adjSide         :: AdjustSide
                       , adjEps          :: Scalar v
                       , adjOptsvProxy__ :: Proxy v
                       }

instance Fractional (Scalar v) => Default (AdjustMethod v) where
  def = ByParam 0.2

instance Default AdjustSide where
  def = Both

instance Fractional (Scalar v) => Default (AdjustOpts v) where
  def = AO def def stdTolerance Proxy

-- | Adjust the length of a parametric object such as a segment or
--   trail.  The second parameter is an option record which controls how
--   the adjustment should be performed; see 'AdjustOpts'.
adjust :: (DomainBounds a, Sectionable a, HasArcLength a, Fractional (Scalar (V a)))
       => a -> AdjustOpts (V a) -> a
adjust s opts = section s
  (if adjSide opts == End   then domainLower s else getParam s)
  (if adjSide opts == Start then domainUpper s else domainUpper s - getParam (reverseDomain s))
 where
  getParam seg = case adjMethod opts of
    ByParam p -> -p * bothCoef
    ByAbsolute len -> param (-len * bothCoef)
    ToAbsolute len -> param (absDelta len * bothCoef)
   where
    param        = arcLengthToParam eps seg
    absDelta len = arcLength eps s - len
  bothCoef = if adjSide opts == Both then 0.5 else 1
  eps = adjEps opts
