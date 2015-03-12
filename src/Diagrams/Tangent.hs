{-# LANGUAGE GADTs                #-} -- for ghc < 7.8, TypeFamilies covers GADT patten mathcing in > 7.8
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Tangent
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Computing tangent and normal vectors for segments and trails.
--
-----------------------------------------------------------------------------
module Diagrams.Tangent
    ( -- ** Tangents
      tangentAtParam
    , tangentAtStart
    , tangentAtEnd

      -- ** Normals
    , normalAtParam
    , normalAtStart
    , normalAtEnd

      -- ** Tangent newtype
    , Tangent(..)
    )
    where

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment

import           Linear.Vector
import           Linear.Metric
import           Linear.V2

------------------------------------------------------------
-- Tangent
------------------------------------------------------------

-- | A newtype wrapper used to give different instances of
--   'Parametric' and 'EndValues' that compute tangent vectors.
newtype Tangent t = Tangent t

type instance V (Tangent t) = V t
type instance N (Tangent t) = N t
type instance Codomain (Tangent t) = V t

instance DomainBounds t => DomainBounds (Tangent t) where
  domainLower (Tangent t) = domainLower t
  domainUpper (Tangent t) = domainUpper t

instance Parametric (Tangent t) => Parametric (Tangent (Located t)) where
  Tangent l `atParam` p = Tangent (unLoc l) `atParam` p

instance (DomainBounds t, EndValues (Tangent t))
    => EndValues (Tangent (Located t)) where
  atStart (Tangent l) = atStart (Tangent (unLoc l))
  atEnd   (Tangent l) = atEnd   (Tangent (unLoc l))

-- | Compute the tangent vector to a segment or trail at a particular
--   parameter.
--
--   Examples of more specific types this function can have include
--
--   * @Segment Closed V2 -> Double -> V2 Double@
--
--   * @Trail' Line V2 -> Double -> V2 Double@
--
--   * @Located (Trail V2) -> Double -> V2 Double@
--
--   See the instances listed for the 'Tangent' newtype for more.
tangentAtParam :: Parametric (Tangent t) => t -> N t -> Vn t
tangentAtParam t p = Tangent t `atParam` p

-- | Compute the tangent vector at the start of a segment or trail.
tangentAtStart :: EndValues (Tangent t) => t -> Vn t
tangentAtStart = atStart . Tangent

-- | Compute the tangent vector at the end of a segment or trail.
tangentAtEnd :: EndValues (Tangent t) => t -> Vn t
tangentAtEnd = atEnd . Tangent

--------------------------------------------------
-- Segment

instance (Additive v, Num n)
    => Parametric (Tangent (Segment Closed v n)) where
  Tangent (Linear (OffsetClosed v)) `atParam` _ = v
  Tangent (Cubic c1 c2 (OffsetClosed x2)) `atParam` p
    = (3*(3*p*p-4*p+1))*^c1 ^+^ (3*(2-3*p)*p)*^c2 ^+^ (3*p*p)*^x2

instance (Additive v, Num n)
    => EndValues (Tangent (Segment Closed v n)) where
  atStart (Tangent (Linear (OffsetClosed v)))      = v
  atStart (Tangent (Cubic c1 _ _))                 = c1
  atEnd   (Tangent (Linear (OffsetClosed v)))      = v
  atEnd   (Tangent (Cubic _ c2 (OffsetClosed x2))) = x2 ^-^ c2

instance (Additive v, Num n)
    => Parametric (Tangent (FixedSegment v n)) where
  atParam (Tangent fSeg) = atParam $ Tangent (fromFixedSeg fSeg)

instance (Additive v, Num n)
    => EndValues (Tangent (FixedSegment v n)) where
  atStart (Tangent fSeg) = atStart $ Tangent (fromFixedSeg fSeg)
  atEnd (Tangent fSeg)   = atEnd $ Tangent (fromFixedSeg fSeg)

------------------------------------------------------------
-- Normal
------------------------------------------------------------

-- | Compute the (unit) normal vector to a segment or trail at a
--   particular parameter.
--
--   Examples of more specific types this function can have include
--
--   * @Segment Closed V2 Double -> Double -> V2 Double@
--
--   * @Trail' Line V2 Double -> Double -> V2 Double@
--
--   * @Located (Trail V2 Double) -> Double -> V2 Double@
--
--   See the instances listed for the 'Tangent' newtype for more.
normalAtParam
  :: (InSpace V2 n t, Parametric (Tangent t), Floating n)
  => t -> n -> V2 n
normalAtParam t p = normize (t `tangentAtParam` p)

-- | Compute the normal vector at the start of a segment or trail.
normalAtStart
  :: (InSpace V2 n t, EndValues (Tangent t), Floating n)
  => t -> V2 n
normalAtStart = normize . tangentAtStart

-- | Compute the normal vector at the end of a segment or trail.
normalAtEnd
  :: (InSpace V2 n t, EndValues (Tangent t), Floating n)
  => t -> V2 n
normalAtEnd = normize . tangentAtEnd

-- | Construct a normal vector from a tangent.
normize :: Floating n => V2 n -> V2 n
normize = negated . perp . signorm
