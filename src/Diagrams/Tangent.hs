{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}

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
    , normalAtParam
    , normalAtStart
    , normalAtEnd
    , Tangent(..)
    , MoreLikeR2
    )
    where

import           Control.Lens         (cloneIso, (^.))

import           Data.VectorSpace
import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TwoD.Vector (perp, LikeR2)

------------------------------------------------------------
-- Tangent
------------------------------------------------------------

-- | A newtype wrapper used to give different instances of
--   'Parametric' and 'EndValues' that compute tangent vectors.
newtype Tangent t = Tangent t

type instance V (Tangent t) = V t

instance DomainBounds t => DomainBounds (Tangent t) where
  domainLower (Tangent t) = domainLower t
  domainUpper (Tangent t) = domainUpper t

type instance Codomain (Tangent (Located t)) = Codomain (Tangent t)

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
tangentAtParam :: Parametric (Tangent t) => t -> Scalar (V t) -> Codomain (Tangent t)
tangentAtParam t p = Tangent t `atParam` p

-- | Compute the tangent vector at the start of a segment or trail.
tangentAtStart :: EndValues (Tangent t) => t -> Codomain (Tangent t)
tangentAtStart = atStart . Tangent

-- | Compute the tangent vector at the end of a segment or trail.
tangentAtEnd :: EndValues (Tangent t) => t -> Codomain (Tangent t)
tangentAtEnd = atEnd . Tangent

--------------------------------------------------
-- Segment

type instance Codomain (Tangent (Segment Closed v)) = Codomain (Segment Closed v)

instance (VectorSpace v, Num (Scalar v))
    => Parametric (Tangent (Segment Closed v)) where
  Tangent (Linear (OffsetClosed v)) `atParam` _ = v
  Tangent (Cubic c1 c2 (OffsetClosed x2)) `atParam` p
    = (3*(3*p*p-4*p+1))*^c1 ^+^ (3*(2-3*p)*p)*^c2 ^+^ (3*p*p)*^x2

instance (VectorSpace v, Num (Scalar v))
    => EndValues (Tangent (Segment Closed v)) where
  atStart (Tangent (Linear (OffsetClosed v)))      = v
  atStart (Tangent (Cubic c1 _ _))                 = c1
  atEnd   (Tangent (Linear (OffsetClosed v)))      = v
  atEnd   (Tangent (Cubic _ c2 (OffsetClosed x2))) = x2 ^-^ c2

--------------------------------------------------
-- Trail' and Trail

type instance Codomain (Tangent (Trail' c v)) = Codomain (Trail' c v)

instance ( Parametric (GetSegment (Trail' c v))
         , VectorSpace v
         , Num (Scalar v)
         )
    => Parametric (Tangent (Trail' c v)) where
  Tangent tr `atParam` p =
    case GetSegment tr `atParam` p of
      Nothing                -> zeroV
      Just (_, seg, reparam) -> Tangent seg `atParam` (p ^. cloneIso reparam)

instance ( Parametric (GetSegment (Trail' c v))
         , EndValues (GetSegment (Trail' c v))
         , VectorSpace v
         , Num (Scalar v)
         )
    => EndValues (Tangent (Trail' c v)) where
  atStart (Tangent tr) =
    case atStart (GetSegment tr) of
      Nothing          -> zeroV
      Just (_, seg, _) -> atStart (Tangent seg)
  atEnd (Tangent tr) =
    case atEnd (GetSegment tr) of
      Nothing          -> zeroV
      Just (_, seg, _) -> atEnd (Tangent seg)

type instance Codomain (Tangent (Trail v)) = Codomain (Trail v)

instance ( InnerSpace v
         , OrderedField (Scalar v)
         , RealFrac (Scalar v)
         )
    => Parametric (Tangent (Trail v)) where
  Tangent tr `atParam` p
    = withTrail
        ((`atParam` p) . Tangent)
        ((`atParam` p) . Tangent)
        tr

instance ( InnerSpace v
         , OrderedField (Scalar v)
         , RealFrac (Scalar v)
         )
    => EndValues (Tangent (Trail v)) where
  atStart (Tangent tr) = withTrail (atStart . Tangent) (atStart . Tangent) tr
  atEnd   (Tangent tr) = withTrail (atEnd   . Tangent) (atEnd   . Tangent) tr

------------------------------------------------------------
-- Normal
------------------------------------------------------------

type MoreLikeR2 v = (LikeR2 v, InnerSpace v, Floating (Scalar v))

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
normalAtParam
  :: (MoreLikeR2 (Codomain (Tangent t)), Parametric (Tangent t))
  => t -> Scalar (V t) -> Codomain (Tangent t)
normalAtParam t p = normize (t `tangentAtParam` p)

-- | Compute the normal vector at the start of a segment or trail.
normalAtStart
  :: (MoreLikeR2 (Codomain (Tangent t)), EndValues (Tangent t))
  => t -> Codomain (Tangent t)
normalAtStart = normize . tangentAtStart

-- | Compute the normal vector at the end of a segment or trail.
normalAtEnd
  :: (MoreLikeR2 (Codomain (Tangent t)), EndValues (Tangent t))
  => t -> Codomain (Tangent t)
normalAtEnd = normize . tangentAtEnd

-- | Construct a normal vector from a tangent.
normize :: (MoreLikeR2 v) => v -> v
normize = negateV . perp . normalized
