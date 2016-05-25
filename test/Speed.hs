module Main where

import Criterion
import Criterion.Main (defaultMain)

import Diagrams.Prelude
import Diagrams.TwoD.Transform
--import Diagrams.Backend.Cairo
--import Diagrams.Backend.Cairo.Internal
import Diagrams.Core.Types
--import Graphics.Rendering.Cairo


main :: IO ()
main = defaultMain
    [ bgroup "rotates"
        [ bench "rotate"  $ whnf (rotate (90 @@ deg)) (V2 3 3)
         ,bench "rotate1" $ whnf (rotate' (90 @@ deg)) (V2 3 3)
         ,bench "rotate2" $ whnf (rotate'' (90 @@ deg)) (V2 3 3)
        ]
    ]

--the original " '' " and a secondary " ' " rotate function for testing

rotation'' :: Floating n => Angle n -> T2 n
rotation'' theta = fromLinear r (linv r)
              where
                r               = rot theta <-> rot (negated theta)
                rot th (V2 x y) = V2 (cosA th * x - sinA th * y)
                                     (sinA th * x + cosA th * y)

rotate'' :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate'' = transform . rotation''

rotation' :: Floating n => Angle n -> T2 n
rotation' theta = fromLinear r (linv r)
                where
                r               = rot theta <-> rot (negated theta)
                rot th (V2 x y) = V2 (c * x - s * y)
                                     (s * x + c * y)
                                      where
                                        c = cosA th
                                        s = sinA th

rotate' :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate'  = transform . rotation'
