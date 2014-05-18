{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Solve
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Exact solving of low-degree (n <= 4) polynomials.
--
-----------------------------------------------------------------------------
module Diagrams.Solve
       ( quadForm
       , cubForm
       , quartForm
       ) where

import           Data.List     (maximumBy)
import           Data.Ord      (comparing)

import           Diagrams.Util (tau)

------------------------------------------------------------
-- Quadratic formula
------------------------------------------------------------

-- | The quadratic formula.
quadForm :: (Floating d, Ord d) => d -> d -> d -> [d]
quadForm a b c

    -- There are infinitely many solutions in this case,
    -- so arbitrarily return 0
  | a == 0 && b == 0 && c == 0 = [0]

    -- c /= 0
  | a == 0 && b == 0 = []

    -- linear
  | a == 0    = [-c/b]

    -- no real solutions
  | d < 0     = []

    -- ax^2 + c = 0
  | b == 0    = [sqrt (-c/a), -sqrt (-c/a)]

    -- multiplicity 2 solution
  | d == 0    = [-b/(2*a)]

    -- see http://www.mpi-hd.mpg.de/astrophysik/HEA/internal/Numerical_Recipes/f5-6.pdf
  | otherwise = [q/a, c/q]
 where d = b*b - 4*a*c
       q = -1/2*(b + signum b * sqrt d)

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
  | aboutZero a             = quadForm b c d

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
       phi = 1/3*acos(3*q/(2*p)*sqrt(-3/p))
       trig k = 2 * sqrt(-p/3) * cos(phi - k*tau/3) - b/(3*a)
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
           
------------------------------------------------------------
-- Quartic formula
------------------------------------------------------------

-- Based on http://tog.acm.org/resources/GraphicsGems/gems/Roots3b/and4.c
-- as of 5/12/14, with help from http://en.wikipedia.org/wiki/Quartic_function

-- | Solve the quartic equation c4 x^4 + c3 x^3 + c2 x^2 + c1 x + c0 = 0, returning a
--   list of all real roots.
quartForm :: (Floating d, Ord d) => d -> d -> d -> d -> d -> [d]
quartForm c4 c3 c2 c1 c0
  -- obvious cubic
  | aboutZero c4 = cubForm c3 c2 c1 c0
  -- x(ax^3+bx^2+cx+d)
  | aboutZero c0 = 0 : cubForm c4 c3 c2 c1
  | otherwise = 
      let -- eliminate c4: x^4+ax^3+bx^2+cx+d
          a=c3/c4; b=c2/c4; c=c1/c4; d=c0/c4
          -- substitute x = u - a/4 to eliminate cubic term:
          -- u^4 + pu^2 + qu + r = 0
          -- 
          p = b - 3/8*a*a
          q = 1/8*a*a*a-a*b/2+c
          r = (-3/256)*a*a*a*a+a*a*b/16-a*c/4+d
      in
        map (\x->x-(a/4)) $ if aboutZero r then
          -- no constant term: u(u^3 + pu + q) = 0
          0 : cubForm 1 0 p q
        else do
          -- solve the resolvent cubic - should have exactly one solution, but cubic solver is inaccurate
          let z:_ = cubForm 1 (-p/2) (-r) (p*r/2 - q*q/8)
          -- quadratic equations
          let u = z * z - r
              v = 2 * z - p
          if u < 0 || v < 0 then [] else
            let u' = if aboutZero u then 0 else sqrt u
                v' = if aboutZero v then 0 else sqrt v
                s1 = quadForm 1 (if' (q<0) (-v') v') (z-u')
                s2 = quadForm 1 (if' (q<0) v' (-v')) (z+u')
            in
              s1++s2
 where
   aboutZero x = abs x < toler
   toler = 1e-10
   if' a b c = if a then b else c

quartForm_prop :: Double -> Double -> Double -> Double -> Double -> Bool
quartForm_prop a b c d e = all (aboutZero . eval) (quartForm a b c d e)
  where eval x = a*x*x*x*x + b*x*x*x + c*x*x + d*x + e
        aboutZero x = abs x < tolerance
        tolerance = 1e-5
           -- Same note about tolerance as for cubic
