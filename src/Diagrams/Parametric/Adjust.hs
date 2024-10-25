{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
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

import           Control.Lens        (Lens', generateSignatures, lensRules, makeLensesWith, (&),
                                      (.~), (^.))
import           Data.Proxy

import           Data.Default

import           Diagrams.Core.V
import           Diagrams.Parametric

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod n = ByParam n     -- ^ Extend by the given parameter value
                                    --   (use a negative parameter to shrink)
                    | ByAbsolute n  -- ^ Extend by the given arc length
                                    --   (use a negative length to shrink)
                    | ToAbsolute n  -- ^ Extend or shrink to the given
                                    --   arc length

-- | Which side of a segment, trail, or path should be adjusted?
data AdjustSide = Start  -- ^ Adjust only the beginning
                | End    -- ^ Adjust only the end
                | Both   -- ^ Adjust both sides equally
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | How should a segment, trail, or path be adjusted?
data AdjustOpts n = AO { _adjMethod    :: AdjustMethod n
                       , _adjSide      :: AdjustSide
                       , _adjEps       :: n
                       , adjOptsvProxy :: Proxy n
                       }

-- See Diagrams.Combinators for reasoning behind 'Proxy'.

makeLensesWith (lensRules & generateSignatures .~ False) ''AdjustOpts

-- | Which method should be used for adjusting?
adjMethod :: Lens' (AdjustOpts n) (AdjustMethod n)

-- | Which end(s) of the object should be adjusted?
adjSide :: Lens' (AdjustOpts n) AdjustSide

-- | Tolerance to use when doing adjustment.
adjEps :: Lens' (AdjustOpts n) n

instance Fractional n => Default (AdjustMethod n) where
  def = ByParam 0.2

instance Default AdjustSide where
  def = Both

instance Fractional n => Default (AdjustOpts n) where
  def = AO { _adjMethod    = def
           , _adjSide      = def
           , _adjEps       = stdTolerance
           , adjOptsvProxy = Proxy
           }

-- | Adjust the length of a parametric object such as a segment or
--   trail.  The second parameter is an option record which controls how
--   the adjustment should be performed; see 'AdjustOpts'.
adjust :: (N t ~ n, Sectionable t, HasArcLength t, Fractional n)
       => t -> AdjustOpts n -> t
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
