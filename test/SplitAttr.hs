{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Applicative
import           Control.Lens                ((%~), (&), _Wrapping')
import           Control.Monad
import qualified Data.Map                    as M
import           Data.Semigroup
import           Data.Tree
import           Data.Typeable
import           Diagrams.Attributes.Compile
import           Diagrams.Core
import           Diagrams.Core.Style
import           Diagrams.Core.Types
import           Diagrams.Prelude            (R2)
import           Test.QuickCheck

data FakePath = Ln | Lp
  deriving (Show, Eq, Typeable)

type instance V FakePath = R2

instance Transformable FakePath where
  transform _ = id

instance Renderable FakePath NullBackend where
  render _ _ = undefined

instance IsPrim FakePath where

data A = A
  deriving (Typeable, Show, Eq, Ord)
instance Semigroup A where
  A <> A = A

data B = B
  deriving (Typeable, Show, Eq, Ord)
instance Semigroup B where
  B <> B = B

data FillLoopsTest = FillLoopsTest

instance SplitAttribute FillLoopsTest where
  type AttrType FillLoopsTest = A
  type PrimType FillLoopsTest = FakePath

  primOK _ l = l == Lp

showRTree :: RTree b v a -> String
showRTree = drawTree . fmap show

instance Show (RNode b v a) where
  show (RStyle s) = "<" ++ showAttr A s ++ showAttr B s ++ ">"
  show (RPrim _ (Prim p)) =
    case cast p of
      Nothing               -> "<P>"
      Just (fp :: FakePath) -> "\"" ++ show fp ++ "\""
  show _ = "."

showAttr :: forall a v. (Show a, AttributeClass a) => a -> Style v -> String
showAttr _ s = maybe "" show (getAttr s :: Maybe a)

instance Eq (RNode b v a) where
  r1 == r2 = show r1 == show r2

instance Arbitrary (RNode b v a) where
  arbitrary = oneof [pure REmpty, s $ attrToStyle A, s $ attrToStyle B, s $ attrToStyle A <> attrToStyle B]
    where
      s = pure . RStyle
  shrink REmpty = []
  shrink _ = [REmpty]

arbPrim :: Gen (RNode NullBackend R2 ())
arbPrim = (RPrim mempty . Prim) <$> elements [Ln, Lp]

genTree :: Int -> Gen (Tree (RNode NullBackend R2 ()))
genTree n | n <= 0 = Node <$> arbitrary <*> pure []
genTree n = do
  len <- choose (0,3)
  if len == 0
    then Node <$> arbPrim   <*> pure []
    else Node <$> arbitrary <*> replicateM len (genTree (n - 1))

instance Arbitrary (Tree (RNode NullBackend R2 ())) where
  arbitrary = sized genTree
  shrink (Node r ts)
    =  [ t          | t <- ts          ]
    ++ [ Node r ts' | ts' <- splices ts ]
    ++ [ Node r' ts | r'  <- shrink r  ]
    ++ [ Node r ts' | ts' <- shrink ts ]
    where
      splices :: [Tree a] -> [[Tree a]]
      splices [] = []
      splices (t@(Node _ cs) : ts) = (cs ++ ts) : (map (t :) (splices ts))

splitA = splitAttr FillLoopsTest

newtype PrettyTree = PT (RTree NullBackend R2 ())
  deriving (Arbitrary)

instance Show PrettyTree where
  show (PT t) = showRTree t

------------------------------------------------------------
-- Properties!

-- should preserve semantics
-- should result in attributes in question only being over OK nodes
-- should not move other attributes

{-

-- Should preserve tree shape?
-- Actually, this isn't true (and shouldn't be)!  New RStyle nodes have
-- to get introduced sometimes.

class Matchable m where
  matches :: m -> m -> Bool
  matches _ _ = True

instance Matchable m => Matchable [m] where
  matches [] [] = True
  matches (x:xs) (y:ys) = matches x y && matches xs ys
  matches _ _ = False

instance Matchable m => Matchable (Tree m) where
  matches (Node x xs) (Node y ys)
    = matches x y && matches xs ys

instance Matchable (RNode b v a)

prop_split_pres_shape :: PrettyTree -> Bool
prop_split_pres_shape (PT t) = matches t (splitA t)
-}

-- Should preserve semantics

type AB = (Maybe A, Maybe B)

removeA :: AB -> AB
removeA (_,b) = (Nothing,b)

flattenTree :: Style R2 -> RTree NullBackend R2 () -> [(Style R2, FakePath)]
flattenTree sty (Node REmpty ts) = flattenForest sty ts
flattenTree sty (Node (RStyle sty') ts) = flattenForest (sty <> sty') ts
flattenTree sty (Node (RPrim _ (Prim p)) _) =
  case cast p of
    Nothing -> []
    Just (l :: FakePath) -> [(sty, l)]
flattenTree sty _ = []

flattenForest :: Style R2 -> [RTree NullBackend R2 ()] -> [(Style R2, FakePath)]
flattenForest = concatMap . flattenTree

semantics :: RTree NullBackend R2 () -> [(AB, FakePath)]
semantics = map postProcess . flattenTree mempty
  where
    styleToAB :: Style R2 -> AB
    styleToAB = (,) <$> getAttr <*> getAttr
    postProcess (sty, l) = ((if l == Ln then removeA else id) (styleToAB sty), l)

prop_splitA_pres_semantics :: PrettyTree -> Bool
prop_splitA_pres_semantics (PT t) = semantics t == semantics (splitA t)

-- Ha, this one caught a bug! Success! =D
