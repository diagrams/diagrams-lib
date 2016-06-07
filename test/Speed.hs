{-# LANGUAGE FlexibleContexts      #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import Diagrams.Prelude


main :: IO ()
main = defaultMain
    [ bgroup "rotates"
        [ bench "rotate"  $ whnf (rotate (90 @@ deg :: Angle Double)) (V2 3 3)
         ,bench "rotate1" $ whnf (rotate' (90 @@ deg :: Angle Double)) (V2 3 3)
         ,bench "rotate2" $ whnf (rotate'' (90 @@ deg :: Angle Double)) (V2 3 3)
        ]
    ]

--the original " '' " and a secondary " ' " rotate function for comparing speed testing
--note: function time changes dramatically when function is in this file rather than imported


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

rotation'' :: Floating n => Angle n -> T2 n
rotation'' theta = fromLinear r (linv r)
              where
                r               = rot theta <-> rot (negated theta)
                rot th (V2 x y) = V2 (cosA th * x - sinA th * y)
                                     (sinA th * x + cosA th * y)

rotate'' :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate'' = transform . rotation''
