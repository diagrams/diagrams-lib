{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Anchors
-- Copyright   :  (c) 2016-2016 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- An /anchor/ is a point on an object which can be used for alignment
-- by naming it, offering easier control over alignment compared to the
-- 'Diagrams.Align' module when aligning many objects.
--
-----------------------------------------------------------------------------

module Diagrams.Anchors
       (
         -- * Anchors
         IsAnchor(..)
       , Anchor
         -- * Anchored objects
       , Anchored
       , withAnchors
       , noAnchors
       , addAnchor
       , deleteAnchor
       , getAnchorOffset
       , alignAnchor
       , hasAnchor
       , unanchor
         -- * Positional anchors
       , PositionalAnchor (..)
       , rotateAnchors
       , rotatePosAnchors
         -- * Qualifying anchors and anchored objects
       , (\>/)
       , (\>>/)
         -- * Easily concatenate many anchored objects
       , anchorMany
       , anchorMany_
         -- * Debugging
       , showAnchor
       , showAnchor_)
       where

import           Diagrams.Names
import           Diagrams.Core
import           Diagrams.Path
import           Diagrams.TwoD.Model

import qualified Control.Lens     as Lens
import           Control.Lens     hiding (transform, (.>))
import           Data.List        (foldl')
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust, fromMaybe)
import qualified Data.Set         as Set
import           Data.Typeable    (Typeable)
import           Data.Semigroup

import           Linear.Vector
import           Linear.V2
import           Linear.Affine

--------------------------------------------------------------------------------
--  Anchors
--------------------------------------------------------------------------------

-- | A concrete anchor type.
newtype Anchor = Anchor { getAnchorName :: Name }
               deriving (Eq, Ord, Show)

-- | Class for an object which can be used as an anchor. Can be derived
-- automatically for anything which is an instance of 'IsName'.
class IsAnchor anchor where
  -- | Convert the object into a concrete 'Anchor'.
  toAnchor :: anchor -> Anchor

  default toAnchor :: IsName anchor => anchor -> Anchor
  toAnchor = Anchor . toName

instance IsAnchor Anchor where
  toAnchor = id

instance IsAnchor Int where
instance IsAnchor Integer where
instance IsAnchor Float where
instance IsAnchor Double where
instance IsAnchor Char where
instance IsAnchor Bool where
instance IsAnchor Name where
instance IsAnchor a => IsAnchor [a] where
  toAnchor = Anchor . toName . map (getAnchorName . toAnchor)

--------------------------------------------------------------------------------
--  Anchored objects
--------------------------------------------------------------------------------

-- | An 'Anchored' object which can be aligned to anchor points before
-- concatenating with other 'Anchored' objects. Note that when concatenating,
-- any anchors with the same names in each of the left and right operands will
-- be retained in the left operand, and lost in the right. To avoid this, qualify
-- anchors in each object using '(\>>/)'.
data Anchored t =
  Anchored
  { _currentAnchor :: Maybe Anchor
  , _anchors :: Map Anchor (V t (N t))
  , _anchoredObj :: t
  }

makeLenses ''Anchored

type instance N (Anchored t) = N t
type instance V (Anchored t) = V t

instance (HasOrigin t, Additive (V t), Num (N t)) => HasOrigin (Anchored t) where
  moveOriginTo p@(P v) =
    (anchoredObj %~ moveOriginTo p) .
    (anchors . traverse %~ (^-^ v))

instance (Additive (V t), Num (N t), Transformable t) => Transformable (Anchored t) where
  transform t =
    (anchors . traverse %~ apply t) .
    (anchoredObj %~ transform t)

instance (Additive (V t), Num (N t), HasOrigin t, Semigroup t) => Semigroup (Anchored t) where
  o1 <> o2 =
    let updateObj obj
          | Just anchor <- obj^.currentAnchor
            = moveOriginBy (getAnchorOffset anchor obj)
            . deleteAnchor anchor
            $ obj
          | otherwise = obj

        a1 <+> a2 = Anchored Nothing
                             ((a1 ^. anchors) <> (a2 ^. anchors))
                             ((a1 ^. anchoredObj) <> (a2 ^. anchoredObj))
    in updateObj o1 <+> updateObj o2

instance (Additive (V t), Num (N t), HasOrigin t, Monoid' t) => Monoid (Anchored t) where
  mempty = Anchored Nothing mempty mempty
  mappend = (<>)

instance (Show (N t), Show (V t (N t)), Show t) => Show (Anchored t) where
  showsPrec p anch =
    showsPrec p (anch^.anchors) . (", " ++) . showsPrec p (anch^.anchoredObj)

-- | Add another anchor to an already 'Anchored' object.
addAnchor :: IsAnchor anchor => anchor -> V t (N t) -> Anchored t -> Anchored t
addAnchor anchor val = anchors . Lens.at (toAnchor anchor) .~ Just val

-- | Attach a list of anchors to an object, making it 'Anchored'.
withAnchors :: IsAnchor anchor => [(anchor, V t (N t))] -> t -> Anchored t
withAnchors = Anchored Nothing . Map.fromList . over (each . _1) toAnchor

-- | Turn an object into a trivial 'Anchored' object with no anchors.
noAnchors :: t -> Anchored t
noAnchors = Anchored Nothing mempty

-- | Delete an anchor from an anchored object. Does nothing if the object does
-- not have the specified anchor.
deleteAnchor :: IsAnchor anchor => anchor -> Anchored t -> Anchored t
deleteAnchor anchor = anchors . Lens.at (toAnchor anchor) .~ Nothing

-- | Get the offset from the origin of a particular anchor, or 'zero' if the object
-- does not have the specified anchor.
getAnchorOffset :: (Num (N t), Additive (V t), IsAnchor a) => a -> Anchored t -> V t (N t)
getAnchorOffset anchor = view $ anchors . Lens.at (toAnchor anchor) . to (fromMaybe zero)

-- | Align an anchored object to an anchor. Subsequently concatening with '(<>)'
-- will take this into account.
alignAnchor :: (IsAnchor a) => a -> Anchored t -> Anchored t
alignAnchor anch = currentAnchor .~ Just (toAnchor anch)

-- | Does the given anchored object have the given anchor?
hasAnchor :: (IsAnchor a) => a -> Anchored t -> Bool
hasAnchor anchor = view $ anchors . to (Map.member (toAnchor anchor))

-- | Throw away anchors and get the underlying object.
unanchor
  :: Anchored t -> t
unanchor = view anchoredObj

--------------------------------------------------------------------------------
--  Positional Anchors
--------------------------------------------------------------------------------

-- | A convenient type of positional anchors.
data PositionalAnchor
  = AnchorL
  | AnchorTL
  | AnchorT
  | AnchorTR
  | AnchorR
  | AnchorBR
  | AnchorB
  | AnchorBL
  deriving (Eq, Ord, Show, Typeable, Enum)

instance IsName PositionalAnchor where
instance IsAnchor PositionalAnchor where

{-|
Given an 'Anchored' object containing the given list of anchors, rotate the
order of the given anchors clockwise by the given number of positions.

For example, given a diagram with positional anchors on it in these positions:

@
TL    T    TR

L          R

BL    B    BR
@

using @'rotatePosAnchors' 1 = 'rotateAnchors' (enumFrom AnchorL) 1@ will move
the anchors to these positions:

@
L     TL   T

BL         TR

B     BR   R
@

Using a parameter @n@ is equivalent to using @1@, @n@ times and a negative
number produces an anticlockwise rotation.

If any of the anchors do not exist, this function skips them.
-}
rotateAnchors :: (IsAnchor anchor) => [anchor] -> Int -> Anchored t -> Anchored t
rotateAnchors allAnchorsList n t =
  let allAnchorsSet = Set.fromList . map toAnchor $ allAnchorsList
      allObjAnchors = t ^. anchors
      presentAnchorsSet = Map.keysSet allObjAnchors `Set.intersection` allAnchorsSet
      presentAnchorsList = filter ((`Set.member` presentAnchorsSet) . toAnchor) allAnchorsList
      rotateList k xs = drop k xs ++ take k xs
      rotatedList = rotateList ((-n) `mod` length presentAnchorsList) presentAnchorsList
      findOriginalPairing posAnch = fromJust $ Map.lookup (toAnchor posAnch) allObjAnchors
      originalOffsets = map findOriginalPairing presentAnchorsList
      rotatedOffsets = zip (map toAnchor rotatedList) originalOffsets
      newObjAnchors = Map.fromList rotatedOffsets `Map.union` allObjAnchors
  in t & anchors .~ newObjAnchors

-- | As 'rotateAnchors', but specialised to the list of all 'PositionalAnchor's.
rotatePosAnchors :: Int -> Anchored t -> Anchored t
rotatePosAnchors = rotateAnchors (enumFrom AnchorL)

--------------------------------------------------------------------------------
--  Qualifying Anchors
--------------------------------------------------------------------------------

infixr 5 \>/
infixr 5 \>>/

-- | Qualify an anchor with another anchor.
(\>/)
  :: (IsAnchor anchor1, IsAnchor anchor2) =>
     anchor1 -> anchor2 -> Anchor
a1 \>/ a2
  | Anchor n1 <- toAnchor a1, Anchor n2 <- toAnchor a2 = Anchor (n1 .> n2)

-- | Qualify all anchors within an 'Anchored' object.
(\>>/) :: IsAnchor anchor => anchor -> Anchored t -> Anchored t
(\>>/) anch =
  (currentAnchor._Just %~ (anch \>/)) .
  (anchors %~ Map.mapKeys (anch \>/))

--------------------------------------------------------------------------------
--  Easily concatenate many anchored objects
--------------------------------------------------------------------------------

{-|
Starting from a base anchored object, recursively concatenate more objects to
the structure built up so far. Be sure to qualify anchors in the input so that
names aren't overwritten.

In each @(thatAnchor, thisAnchor, obj)@ triple, @thatAnchor@ refers to the
anchor point in the structure already constructed, and @thisAnchor@ refers to
the anchor point in the new object being added.
-}
anchorMany
  :: (Num (N t), Semigroup t, Additive (V t), HasOrigin t,
      IsAnchor anchor) =>
     Anchored t -> [(anchor, anchor, Anchored t)] -> Anchored t
anchorMany = foldl' go
  where
    go base (thatAnch, thisAnch, obj)
      = alignAnchor thatAnch base <> alignAnchor thisAnch obj

-- | As 'anchorMany', but call 'unanchor' on the result. Convenient when you're
-- not going to be doing any more alignment using anchors with the result.
anchorMany_
  :: (Num (N c), Semigroup c, Additive (V c), HasOrigin c,
      IsAnchor anchor) =>
     Anchored c -> [(anchor, anchor, Anchored c)] -> c
anchorMany_ base = unanchor . anchorMany base

--------------------------------------------------------------------------------
--  Debugging
--------------------------------------------------------------------------------

-- | Show a particular anchor in the 'Anchored' object.
showAnchor
  :: (RealFloat n, Typeable n, Monoid m, Semigroup m,
      Renderable (Path V2 n) b, IsAnchor a) =>
     a -> Anchored (QDiagram b V2 n m) -> Anchored (QDiagram b V2 n m)
showAnchor anch = moveFromAnchor . over anchoredObj showOrigin . moveToAnchor
  where
    moveToAnchor   t = t & anchoredObj %~ moveOriginBy ( getAnchorOffset anch t)
    moveFromAnchor t = t & anchoredObj %~ moveOriginBy (-getAnchorOffset anch t)

-- | Show a particular anchor in the 'Anchored' object, then 'unanchor'.
showAnchor_
  :: (RealFloat n, Typeable n, Monoid m, Semigroup m,
      Renderable (Path V2 n) b, IsAnchor a) =>
     a -> Anchored (QDiagram b V2 n m) -> QDiagram b V2 n m
showAnchor_ anch = unanchor . showAnchor anch
