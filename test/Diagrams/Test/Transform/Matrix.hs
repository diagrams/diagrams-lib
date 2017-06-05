{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Diagrams.Test.Transform.Matrix where


import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Diagrams.Transform.Matrix
import           Diagrams.Prelude
import           Data.Distributive (distribute)

import Instances

tests :: TestTree
tests = testGroup "Transform.Matrix"
  [
    testProperty "mkMat column vectors (2D)" $
      \(Blind (t :: T2 Double)) -> distribute (mkMat t) =~ V2 (transform t unitX) (transform t unitY)
  , testProperty "mkMat / fromMat22" $
      \(m :: V2 (V2 Double)) -> mkMat (fromMat22 m zero) =~ m

  , testProperty "mkMat / fromMat33" $
      \(m :: V3 (V3 Double)) -> mkMat (fromMat33 m zero) =~ m
  ]
