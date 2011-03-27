{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)

import Diagrams.Segment (Segment(..))
import Diagrams.Path
import Diagrams.Align
import Diagrams.Util

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Monoid
import Data.Default

------------------------------------------------------------
-- Combining two diagrams
------------------------------------------------------------

-- | Place two bounded, monoidal objects (i.e. diagrams or paths) next
--   to each other along the given vector.  In particular, place the
--   first object so that the vector points from its local origin to
--   the local origin of the second object, at a distance so that
--   their bounding regions are just tangent.  The local origin of the
--   new, combined object is at the point of tangency, along the line
--   between the old local origins.
beside :: ( HasOrigin a, Boundable a, Monoid a
          , v ~ OriginSpace a, v ~ BoundSpace a
          , VectorSpace v )
       => v -> a -> a -> a
beside v d1 d2
  = align (negateV v) d1 <> align v d2

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

-- | Combine a list of objects (i.e. diagrams or paths) by assigning
--   them absolute positions in the vector space of the combined
--   object.
position :: ( HasOrigin a, Monoid a
            , v ~ OriginSpace a, AdditiveGroup v
            )
         => [ (Point v, a) ] -> a
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
-- \"decorate\" a trail, placing the local origin of one diagram at
-- each successive vertex.  XXX say more
decorateTrail :: ( HasOrigin a, Monoid a
                 , v ~ OriginSpace a, AdditiveGroup v
                 )
              => Trail v -> [a] -> a
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