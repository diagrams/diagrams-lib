-- |

module Diagrams.Test.TwoD where

import           Diagrams.Prelude
import qualified Diagrams.Query as Query (sample)
import           Diagrams.Trail        (linePoints)
import           Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

newtype SmallAngle = SmallAngle (Angle Double)
  deriving (Eq, Ord, Show)

-- Generate random angles within a reasonably small range (+/- 5
-- turns).
instance Arbitrary SmallAngle where
  arbitrary = SmallAngle . (@@turn) <$> choose (-5, 5)

tests :: TestTree
tests = testGroup "TwoD"
    [ testGroup "TwoD.Arc" [
           testProperty "arc start point is at radius 1 in the starting direction" $ \d (SmallAngle a) ->
               pathVertices (arc d a :: Path V2 Double) ^? _head . _head =~ Just (origin .+^ fromDirection d )
           , testProperty "arc end point is at radius 1 in the ending direction" $ \d (SmallAngle a) ->
               pathVertices (arc d a :: Path V2 Double) ^? _head . _last =~ Just (origin .+^ fromDirection (rotate a d))
         ]
    , testGroup "TwoD.Types" [
         testProperty "R2 vector addition is commutative" $
           \u v -> (u :: V2 Double) ^+^ v =~ v ^+^ u
         , testProperty "R2 subtraction is the inverse of addition" $
           \u v -> u ^+^ v ^-^ v =~ (u :: V2 Double)
         , testProperty "R2 vector negation squared is identity" $
           \u -> negated (negated (u :: V2 Double)) =~ u
         ]
      , testGroup "cubicSpline" [
         testProperty "Open cubic spline interpolates all points" $
         \pts -> length pts > 1 ==> and (zipWith (=~) pts (cubicSpline False pts :: [P2 Double]))
         , testProperty "Closed cubic spline interpolates all points" $
           \pts -> length pts > 1 ==> and (zipWith (=~) pts (cubicSpline True pts :: [P2 Double]))
    ]
      , testGroup "Trail" [
         testProperty "glueLine . cutLoop === id" $
         \l -> glueLine (cutLoop l :: Trail' Line V2 Double) =~ l
         , testProperty "cutLoop ends at starting point" $
           \l -> let ps = linePoints (cutLoop (l :: Trail' Loop V2 Double) `at` origin) in (ps ^? _head) =~ (ps ^? _last)
         , testProperty "cutTrail makes a Line" $
           \t -> isLine (cutTrail (t :: Trail V2 Double))
          , testProperty "fromSegments . lineSegments === id" $
            \l -> fromSegments (lineSegments l) =~ (l :: Trail' Line V2 Double)
          , testProperty "lineSegments . fromSegments === id" $
            \segs -> lineSegments (fromSegments segs) =~ (segs :: [Segment Closed V2 Double])
          ]
    , testGroup "Queries and Backgrounds"
        (let dia :: QDiagram NullBackend V2 Double [Int]
             dia = circle 5 # scaleX 2 # rotateBy (1/14) # value [1]
                   <>
                   circle 2 # scaleX 5 # rotateBy (-4/14) # value [2]
         in [
           testProperty "sample dia pt === sample (dia # bg color) pt" $
           \pt -> Query.sample dia pt QC.=== Query.sample (dia # bg orange) pt
         , testProperty "sample dia pt === sample (dia # bgFrame 0.1 color) pt" $
           \pt -> Query.sample dia pt QC.=== Query.sample (dia # bgFrame 0.1 green) pt
         ])
    ]
