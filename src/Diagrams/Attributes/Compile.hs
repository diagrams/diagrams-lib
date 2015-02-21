{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes.Compile
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Diagrams.Attributes.Compile (
    SplitAttribute(..), splitAttr
  ) where

import           Data.Typeable

import           Control.Arrow       (second)
import           Control.Lens        ((%~), (&), _Wrapping')
import qualified Data.HashMap.Strict as HM
import           Data.Semigroup      ((<>))
import           Data.Tree           (Tree (..))

import           Diagrams.Core
import           Diagrams.Core.Style (Style (..), attributeToStyle)
import           Diagrams.Core.Types (RNode (..), RTree)

------------------------------------------------------------

-- This is a sort of roundabout, overly-general way to define
-- splitFills; it's done this way to facilitate testing.

class (AttributeClass (AttrType code), Typeable (PrimType code)) => SplitAttribute code where
  type AttrType code :: *
  type PrimType code :: *

  primOK :: code -> PrimType code -> Bool

-- | Push certain attributes down until they are at the roots of trees
--   containing only "safe" nodes.  In particular this is used to push
--   fill attributes down until they are over only loops; see
--   'splitFills'.
splitAttr :: forall code b v n a. SplitAttribute code => code -> RTree b v n a -> RTree b v n a
splitAttr code = fst . splitAttr' Nothing
  where

  -- splitAttr' is where the most interesting logic happens.
  -- Mutually recursive with splitAttr'Forest. rebuildNode and
  -- applyMfc are helper functions.
  --
  -- Input: attribute to apply to "safe" subtrees.
  --
  -- Output: tree with attributes pushed down appropriately, and
  -- a Bool indicating whether the tree contains only "safe" prims (True) or
  -- contains some unsafe ones (False).
  splitAttr' :: Maybe (AttrType code) -> RTree b v n a -> (RTree b v n a, Bool)

  -- RStyle node: Check for the special attribute, and split it out of
  -- the style, combining it with the incoming attribute.  Recurse and
  -- rebuild. The tricky bit is that we use some knot-tying to
  -- determine the right attribute to pass down to the subtrees based
  -- on this computed Bool: if all subtrees are safe, then we will
  -- apply the attribute at the root of this tree, and pass Nothing to
  -- all the subtrees.  Otherwise, we pass the given attribute along.
  -- This works out because the attribute does not need to be
  -- pattern-matched until actually applying it at some root, so the
  -- recursion can proceed and the Bool values be computed with the
  -- actual value of the attributes nodes filled in lazily.
  splitAttr' mattr (Node (RStyle sty) cs) = (t', ok)
    where
      mattr' = mattr <> getAttr sty
      sty' = sty & _Wrapping' Style %~ HM.delete ty
      ty   = typeOf (undefined :: AttrType code)
      (cs', ok) = splitAttr'Forest mattr' cs
      t' | ok        = rebuildNode Nothing ok (RStyle sty) cs'
         | otherwise = rebuildNode mattr ok (RStyle sty') cs'

  -- RPrim node: check whether it
  --   * is some sort of prim not under consideration: don't apply the attribute; return True
  --   * is unsafe: don't apply the attribute; return False
  --   * is safe  :  do   apply the attribute; return True
  splitAttr' mattr (Node rp@(RPrim (Prim prm)) _) =
      case cast prm :: Maybe (PrimType code) of
        Nothing  -> (Node rp [], True)
        Just p ->
          if primOK code p
            then (rebuildNode mattr True rp [], True)
            else (Node rp [], False)

  -- RFrozenTr, RAnnot, REmpty cases: just recurse and rebuild.  Note
  -- we assume that transformations do not affect the attributes.
  splitAttr' mattr (Node nd cs)    = (t', ok)
    where
      (cs', ok) = splitAttr'Forest mattr cs
      t'        = rebuildNode mattr ok nd cs'

  -- Recursively call splitAttr' on all subtrees, returning the
  -- logical AND of the Bool results returned (the whole forest is
  -- safe iff all subtrees are).
  splitAttr'Forest :: Maybe (AttrType code) -> [RTree b v n a] -> ([RTree b v n a], Bool)
  splitAttr'Forest mattr cs = (cs', ok)
    where
      (cs', ok) = second and . unzip . map (splitAttr' mattr) $ cs

  -- Given a fill attribute, a Bool indicating whether the given
  -- subforest contains only loops, a node, and a subforest, rebuild a
  -- tree, applying the fill attribute as appropriate (only if the
  -- Bool is true and the attribute is not Nothing).
  rebuildNode :: Maybe (AttrType code) -> Bool -> RNode b v n a -> [RTree b v n a] -> RTree b v n a
  rebuildNode mattr ok nd cs
    | ok        = applyMattr mattr (Node nd cs)
    | otherwise = Node nd cs

  -- Prepend a new fill color node if Just; the identity function if
  -- Nothing.
  applyMattr :: Maybe (AttrType code) -> RTree b v n a -> RTree b v n a
  applyMattr Nothing  t = t
  applyMattr (Just a) t = Node (RStyle $ attributeToStyle (Attribute a)) [t]
