{-# LANGUAGE FlexibleContexts #-}


module Diagrams.Test.Trail where

import           Diagrams.Prelude
import           Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Trail"
  [ let wrap :: Trail' Line V2 Double -> Located (Trail V2 Double)
        wrap = (`at` origin) . wrapLine
    in
    testProperty "unfixTrail . fixTrail == id for lines" $
    \l -> (unfixTrail . fixTrail $ wrap l) =~ (wrap l)

  , testProperty "glueLine . cutLoop == id" $
    \loop -> (glueLine . cutLoop $ loop) =~ (loop :: Trail' Loop V2 Double)

  , testProperty "trailOffset == sumV . trailOffsets" $
    \t -> trailOffset t =~ (sumV . trailOffsets $ (t :: Trail V2 Double))

  , testProperty "reverseTrail . reverseTrail == id" $
    \t -> (reverseTrail . reverseTrail $ t) =~ (t :: Trail V2 Double)

  , testProperty "reverseLocTrail . reverseLocTrail == id" $
    \t -> (reverseLocTrail . reverseLocTrail $ t) =~
          (t :: Located (Trail V2 Double))

  , testProperty "reverseLine . reverseLine == id" $
    \t -> (reverseLine . reverseLine $ t) =~
          (t :: Trail' Line V2 Double)

  , testProperty "reverseLocLine . reverseLocLine == id" $
    \t -> (reverseLocLine . reverseLocLine $ t) =~
          (t :: Located (Trail' Line V2 Double))

  , testProperty "reverseLoop . reverseLoop == id" $
    \t -> (reverseLoop . reverseLoop $ t) =~
          (t :: Trail' Loop V2 Double)

  , testProperty "reverseLocLoop . reverseLocLoop == id" $
    \t -> (reverseLocLoop . reverseLocLoop $ t) =~
          (t :: Located (Trail' Loop V2 Double))

  ]
