{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
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
    ( tangentAtParam
    , tangentAtStart
    , tangentAtEnd
    -- , normalAtParam
    -- , normalAtStart
    -- , normalAtEnd
    , Tangent(..)
    )
    where

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment

import Linear.Vector

------------------------------------------------------------
-- Tangent
------------------------------------------------------------

-- | A newtype wrapper used to give different instances of
--   'Parametric' and 'EndValues' that compute tangent vectors.
newtype Tangent t = Tangent t

type instance V (Tangent t) = V t
type instance N (Tangent t) = N t

instance DomainBounds t => DomainBounds (Tangent t) where
  domainLower (Tangent t) = domainLower t
  domainUpper (Tangent t) = domainUpper t

type instance Codomain (Tangent (Located t)) n = Codomain (Tangent t) n

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
--   * @Segment Closed R2 -> Double -> R2@
--
--   * @Trail' Line R2 -> Double -> R2@
--
--   * @Located (Trail R2) -> Double -> R2@
--
--   See the instances listed for the 'Tangent' newtype for more.
tangentAtParam :: Parametric (Tangent t) => t -> N t -> Codomain (Tangent t) (N t)
tangentAtParam t p = Tangent t `atParam` p

-- | Compute the tangent vector at the start of a segment or trail.
tangentAtStart :: EndValues (Tangent t) => t -> Codomain (Tangent t) (N t)
tangentAtStart = atStart . Tangent

-- | Compute the tangent vector at the end of a segment or trail.
tangentAtEnd :: EndValues (Tangent t) => t -> Codomain (Tangent t) (N t)
tangentAtEnd = atEnd . Tangent

--------------------------------------------------
-- Segment

type instance Codomain (Tangent (Segment Closed v n)) n = Codomain (Segment Closed v n) n

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

------------------------------------------------------------
-- Normal
------------------------------------------------------------

-- | Compute the (unit) normal vector to a segment or trail at a
--   particular parameter.
--
--   Examples of more specific types this function can have include
--
--   * @Segment Closed R2 -> Double -> R2@
--
--   * @Trail' Line R2 -> Double -> R2@
--
--   * @Located (Trail R2) -> Double -> P2@
--
--   See the instances listed for the 'Tangent' newtype for more.
-- normalAtParam
--   :: (R2Ish (Codomain (Tangent t)), Parametric (Tangent t))
--   => t -> Scalar (V t) -> Codomain (Tangent t)
-- normalAtParam t p = normize (t `tangentAtParam` p)
-- 
-- -- | Compute the normal vector at the start of a segment or trail.
-- normalAtStart
--   :: (R2Ish (Codomain (Tangent t)), EndValues (Tangent t))
--   => t -> Codomain (Tangent t)
-- normalAtStart = normize . tangentAtStart
-- 
-- -- | Compute the normal vector at the end of a segment or trail.
-- normalAtEnd
--   :: (R2Ish (Codomain (Tangent t)), EndValues (Tangent t))
--   => t -> Codomain (Tangent t)
-- normalAtEnd = normize . tangentAtEnd
-- 
-- | Construct a normal vector from a tangent.
-- normize :: (Additive v, Num n) => v n -> v n
-- normize = negated . perp . normalize
