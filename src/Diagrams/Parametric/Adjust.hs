{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Parametric.Adjust
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for adjusting the length of parametric objects such as
-- segments and trails.
--
-----------------------------------------------------------------------------
module Diagrams.Parametric.Adjust
    ( adjust
    , AdjustOpts(_adjMethod, _adjSide, _adjEps)
    , adjMethod, adjSide, adjEps
    , AdjustMethod(..), AdjustSide(..)

    ) where

import           Control.Lens        (Lens', generateSignatures, lensField, lensRules,
                                      makeLensesWith, (&), (.~), (^.))
import           Data.Proxy

import           Data.Default.Class
import           Data.VectorSpace

import           Diagrams.Core.V
import           Diagrams.Parametric

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
data AdjustOpts v = AO { _adjMethod       :: AdjustMethod v
                       , _adjSide         :: AdjustSide
                       , _adjEps          :: Scalar v
                       , _adjOptsvProxy__ :: Proxy v
                       }

makeLensesWith
  ( lensRules
    -- don't make a lens for the proxy field
    & lensField .~ (\label ->
        case label of
          "_adjOptsvProxy__" -> Nothing
          _ -> Just (drop 1 label)
        )
    & generateSignatures .~ False
  )
  ''AdjustOpts

-- | Which method should be used for adjusting?
adjMethod :: Lens' (AdjustOpts v) (AdjustMethod v)

-- | Which end(s) of the object should be adjusted?
adjSide :: Lens' (AdjustOpts v) AdjustSide

-- | Tolerance to use when doing adjustment.
adjEps :: Lens' (AdjustOpts v) (Scalar v)

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
  (if opts^.adjSide == End   then domainLower s else getParam s)
  (if opts^.adjSide == Start then domainUpper s else domainUpper s - getParam (reverseDomain s))
 where
  getParam seg = case opts^.adjMethod of
    ByParam p -> -p * bothCoef
    ByAbsolute len -> param (-len * bothCoef)
    ToAbsolute len -> param (absDelta len * bothCoef)
   where
    param        = arcLengthToParam eps seg
    absDelta len = arcLength eps s - len
  bothCoef = if opts^.adjSide == Both then 0.5 else 1
  eps = opts^.adjEps
