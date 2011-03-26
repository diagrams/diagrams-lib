{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap, moveTo)

import Diagrams.Segment (Segment(..))
import Diagrams.Path

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Monoid
import Data.Default

------------------------------------------------------------
-- Aligning diagrams
------------------------------------------------------------

------------------------------------------------------------
-- Combining two diagrams
------------------------------------------------------------

-- | Place two diagrams next to each other along the given vector.
--   XXX write more. note where origin ends up.
beside :: ( Backend b, v ~ BSpace b, s ~ Scalar v
          , HasLinearMap v, InnerSpace v
          , AdditiveGroup s, Fractional s, Ord s
          , Monoid m
          )
       => v -> AnnDiagram b m -> AnnDiagram b m -> AnnDiagram b m
beside v d1 d2
  = moveOriginTo (P $ getBoundFunc (bounds d1) v *^ v) d1 `atop`
    moveOriginTo (P $ getBoundFunc (bounds d2) (negateV v) *^ negateV v) d2
  -- XXX reimplement in terms of more basic "align" functions


-- XXX this should move to a different module?
-- | @strut v@ is a diagram which produces no output, but for the
--   purposes of alignment and bounding regions acts like a
--   1-dimensional segment oriented along the vector @v@.  Useful for
--   manually creating separation between two diagrams.
strut :: ( BSpace b ~ v, Scalar v ~ s
         , InnerSpace v, Floating s, Ord s, AdditiveGroup s
         , Monoid m
         )
      => v -> AnnDiagram b m
strut v = mempty { bounds_ = bounds (Linear v) }

------------------------------------------------------------
-- Combining multiple diagrams
------------------------------------------------------------

-- | Assign absolute positions to the origins of some diagrams,
--   combining them into one.
position :: ( Backend b, BSpace b ~ v, Scalar v ~ s
            , InnerSpace v, HasLinearMap v
            , AdditiveGroup s, Ord s, Floating s
            , Monoid m
            )
         => [ (Point v, AnnDiagram b m) ] -> AnnDiagram b m
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams by using them to \"decorate\" a trail,
--   placing the local origin of one diagram at each successive vertex.
--   XXX say more
decorateTrail :: ( Backend b, BSpace b ~ v, Scalar v ~ s
                 , InnerSpace v, HasLinearMap v
                 , AdditiveGroup s, Ord s, Floating s
                 , Monoid m
                 )
              => Trail v -> [AnnDiagram b m] -> AnnDiagram b m
decorateTrail t = position . zip (trailVertices origin t)

-- XXX comment me
data Alignment = AlignLeft | AlignRight | AlignCenter

-- XXX comment me
data Positioning = PositionFront | PositionCenter | PositionBack

-- XXX comment me
data CatMethod v = CatSep (Scalar v)
                 | CatRep (Scalar v) Positioning
                 | CatDistrib (Scalar v) Positioning

-- XXX comment me
data CatOpts v = CatOpts
  { catDir      :: v
  , catMethod   :: CatMethod v
  , catAlignDir :: v
  , catAlign    :: Alignment
  }

instance (AdditiveGroup v, AdditiveGroup (Scalar v)) => Default (CatOpts v) where
  def = CatOpts { catDir      = zeroV
                , catMethod   = CatSep (zeroV)
                , catAlignDir = zeroV
                , catAlign    = AlignCenter
                }

-- XXX comment me
cat' :: (Backend b, BSpace b ~ v)
    => CatOpts v -> [AnnDiagram b m] -> AnnDiagram b m
cat' (CatOpts { catDir      = dir
              , catMethod   = meth
              , catAlignDir = aDir
              , catAlign    = algn
              })
     dias
  = undefined

-- cat

-- along

-- at

-- grid