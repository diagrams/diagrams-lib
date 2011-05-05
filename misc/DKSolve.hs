------------------------------------------------------------
-- Durand-Kerner method
------------------------------------------------------------

-- See http://en.wikipedia.org/wiki/Durand–Kerner_method


import Data.Complex
import Data.List (inits, tails)

eps :: Double
eps = 1e-14

-- | Given as input a list of polynomial coefficients (least
--   significant first), return a list of the /real/ roots.
durandKerner :: [Double] -> [Double]
durandKerner as = map realPart . filter ((<(sqrt eps)) . abs . imagPart) . fixedPt eps (dkIter as) $ initial
  where initial = take (length as - 1) $ iterate (*(0.4 :+ 0.9)) 1

-- | Given the polynomial coefficients, perform one iteration of the
--   D-K method.
dkIter :: [Double] -> [Complex Double] -> [Complex Double]
dkIter as rs = zipWith (-) rs (zipWith (/) evals denoms)
  where evals     = map (eval as) rs
        denoms    = zipWith mkDenom rs (drops rs) -- (skipZip rs' rs)
        mkDenom r = product . map ((-) r)

drops :: [a] -> [[a]]
drops [x]     = [[]]
drops (x:xs) = xs : map (x:) (drops xs)

{-
skipZip :: [a] -> [a] -> [[a]]
skipZip xs ys = zipWith (++) (initsL xs) (tail (tails ys))

initsL :: [a] -> [[a]]
initsL xs = [] : initsL' xs
  where initsL' []     = []
        initsL' (x:xs) = map (x:) (initsL xs)
-}

-- | Evaluate a polynomial for a complex input.
eval :: [Double] -> Complex Double -> Complex Double
eval as x = foldr (\a v -> (a :+ 0) + x*v) 0 as

type C = Complex Double

fixedPt :: Double -> ([C] -> [C]) -> [C] -> [C]
fixedPt eps f as | all ((<eps) . realPart . abs) $ zipWith (-) as as' = as
                 | otherwise          = fixedPt eps f as'
  where as' = f as