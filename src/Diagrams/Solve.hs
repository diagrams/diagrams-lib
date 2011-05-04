{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Solve
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Exact solving of low-degree (n <= 3) polynomials.
--
-----------------------------------------------------------------------------
module Diagrams.Solve
       ( quadForm
       , cubForm
       ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

------------------------------------------------------------
-- Quadratic formula
------------------------------------------------------------

-- | The quadratic formula.
quadForm :: (Floating d, Ord d) => d -> d -> d -> [d]
quadForm a b c

    -- There are infinitely many solutions in this case,
    -- so arbitrarily return 0
  | a == 0 && b == 0 && c == 0 = [0]

    -- c = 0
  | a == 0 && b == 0 = []

    -- linear
  | a == 0    = [-c/b]

    -- no real solutions
  | d < 0     = []

    -- multiplicity 2 solution
  | d == 0    = [-b/(2*a)]

  | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)]
 where d = b*b - 4*a*c

quadForm_prop :: Double -> Double -> Double -> Bool
quadForm_prop a b c = all (aboutZero . eval) (quadForm a b c)
  where eval x = a*x*x + b*x + c
        aboutZero x = abs x < tolerance
        tolerance = 1e-10

------------------------------------------------------------
-- Cubic formula
------------------------------------------------------------

-- See http://en.wikipedia.org/wiki/Cubic_formula#General_formula_of_roots

-- | Solve the cubic equation ax^3 + bx^2 + cx + d = 0, returning a
--   list of all real roots.
cubForm :: (Floating d, Ord d) => d -> d -> d -> d -> [d]
cubForm a b c d
  | a == 0                  = quadForm b c d

    -- three real roots, use trig method to avoid complex numbers
  | delta >  0              = map trig [0,1,2]

    -- one real root of multiplicity 3
  | delta == 0 && disc == 0 = [ -b/(3*a) ]

    -- two real roots, one of multiplicity 2
  | delta == 0 && disc /= 0 = [ (b*c - 9*a*d)/(2*disc)
                              , (9*a*a*d - 4*a*b*c + b*b*b)/(a * disc)
                              ]

    -- one real root (and two complex)
  | otherwise               = [-b/(3*a) - cc/(3*a) + disc/(3*a*cc)]

 where delta  = 18*a*b*c*d - 4*b*b*b*d + b*b*c*c - 4*a*c*c*c - 27*a*a*d*d
       disc   = 3*a*c - b*b
       qq     = sqrt(-27*a*a*delta)
       qq'    | aboutZero disc = maximumBy (comparing (abs . (+xx))) [qq, -qq]
              | otherwise = qq
       cc     = cubert (1/2*(qq' + xx))
       xx     = 2*b*b*b - 9*a*b*c + 27*a*a*d
       p      = disc/(3*a*a)
       q      = xx/(27*a*a*a)
       trig k = 2 * sqrt(-p/3) * cos(1/3*acos(3*q/(2*p)*sqrt(-3/p)) - k*2*pi/3)
                - b/(3*a)

       cubert x | x < 0     = -((-x)**(1/3))
                | otherwise = x**(1/3)

       aboutZero x = abs x < toler
       toler = 1e-10

cubForm_prop :: Double -> Double -> Double -> Double -> Bool
cubForm_prop a b c d = all (aboutZero . eval) (cubForm a b c d)
  where eval x = a*x*x*x + b*x*x + c*x + d
        aboutZero x = abs x < tolerance
        tolerance = 1e-5
           -- Basically, however large you set the tolerance it seems
           -- that quickcheck can always come up with examples where
           -- the returned solutions evaluate to something near zero
           -- but larger than the tolerance (but it takes it more
           -- tries the larger you set the tolerance). Wonder if this
           -- is an inherent limitation or (more likely) a problem
           -- with numerical stability.  If this turns out to be an
           -- issue in practice we could, say, use the solutions
           -- generated here as very good guesses to a numerical
           -- solver which can give us a more precise answer?