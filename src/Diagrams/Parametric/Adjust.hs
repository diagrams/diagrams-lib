{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Parametric.Adjust
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- XXX
--
-----------------------------------------------------------------------------
module Diagrams.Parametric.Adjust
    ( adjust
    , AdjustOpts(..), AdjustMethod(..), AdjustSide(..)

    ) where

import           Data.Default.Class
import           Data.VectorSpace

import           Diagrams.Core.V
import           Diagrams.Parametric
import           Diagrams.Util

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod v = ByParam (Scalar v)     -- ^ Extend by the given parameter value
                                             --   (use a negative parameter to shrink)
                    | ByAbsolute (Scalar v)  -- ^ Extend by the given arc length
                                             --   (use a negative length to shrink)
                    | ToAbsolute (Scalar v)  -- ^ Extend or shrink to the given
                                             --   arc length
                    | ByDistance (Scalar v)  -- ^ Extend or by the fraction of the
                                             --   distance form the origin to the
                                             --   origin .+^ offset

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

paramDistance x t = undefined

-- | Adjust the length of a parametric object such as a segment or
--   trail.  The second parameter is an option record which controls how
--   the adjustment should be performed; see 'AdjustOpts'.

-- This origingal attempt crashes GHC, Ticket 8227. Deleting the line
-- marked XXX allows it to compile. Hence the more redundant code used
-- below.

-- adjust' :: (DomainBounds a, Sectionable a, HasArcLength a, Fractional (Scalar (V a)))
--        => a -> DomainMap a -> Scalar (V a) -> AdjustOpts (V a) -> a
-- adjust' s f t opts = section' s f
--   (if adjSide opts == End   then domainLower s else t')
--   (if adjSide opts == Start then domainUpper s else reverseParam s t')
--  where
--   t' = if adjSide opts == Both then 0.5 * t else t
--
-- absoluteToParam :: (Parametric a, HasArcLength a) => Scalar (V a) -> DomainMap a
-- absoluteToParam eps seg len  = arcLengthToParam eps (arcLength eps seg - len)
--
-- adjust  :: (DomainBounds a, Sectionable a, HasArcLength a, Fractional (Scalar (V a)))
--         => a -> AdjustOpts (V a) -> a
-- adjust s opts = adjust' s f t opts
--   where
--     (f, t) = case adjMethod opts of
--       ByParam p -> (ident, p)
--       ByAbsolute len -> (arcLengthToParam eps, -len)
-- XXX   ToAbsolute len -> (absoluteToParam eps, len)
--       {- ByDistance dist -> (distanceToParam eps, -dist) -}
--     eps = adjEps opts

makeParams :: (Sectionable p, DomainBounds p, Fractional (Scalar (V p)))
              => AdjustSide -> p -> Scalar (V p) -> (Scalar (V p), Scalar (V p))
makeParams side s t =
    ((if side == End   then domainLower s else t'),
    (if side == Start then domainUpper s else r))
  where
    t' = if side == Both then 0.5 * t else t
    r = domainUpper s - t'

adjustByParam :: (Sectionable p, DomainBounds p, Fractional (Scalar (V p)))
              => p -> Scalar (V p) -> AdjustOpts (V p) -> p
adjustByParam x v opts = section' x id t1 t2
  where (t1, t2) = makeParams (adjSide opts) x v

adjustByAbsolute :: ( Sectionable p, DomainBounds p, Fractional (Scalar (V p))
                    , HasArcLength p) => p -> Scalar (V p) -> AdjustOpts (V p) -> p
adjustByAbsolute x v opts = section' x f t1 t2
  where
    (t1, t2) = makeParams (adjSide opts) x v
    f = arcLengthToParam (adjEps opts) x

adjustToAbsolute :: ( Sectionable p, DomainBounds p, Fractional (Scalar (V p))
                    , HasArcLength p) => p -> Scalar (V p) -> AdjustOpts (V p) -> p
adjustToAbsolute x v opts = section' x f t1 t2
  where
    (t1, t2) = makeParams (adjSide opts) x v
    f s =  arcLengthToParam eps x s
    s = (arcLength eps x - v)
    eps = (adjEps opts)

adjust :: (DomainBounds a, Sectionable a, HasArcLength a, Fractional (Scalar (V a)))
        => a -> AdjustOpts (V a) -> a
adjust s opts =
  case adjMethod opts of
    ByParam t -> adjustByParam s t opts
    ByAbsolute t -> adjustByAbsolute s t opts
    ToAbsolute t -> adjustToAbsolute s t opts