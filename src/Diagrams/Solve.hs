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
       , cubForm'
       , quartForm'
       ) where

import           Data.List     (maximumBy)
import           Data.Ord      (comparing)

import           Diagrams.Util (tau)

import           Prelude hiding ((^))
import qualified Prelude as P ((^))

-- | A specialization of (^) to Integer
--   c.f. http://comments.gmane.org/gmane.comp.lang.haskell.libraries/21164
--   for discussion. "The choice in (^) and (^^) to overload on the
--   power's Integral type... was a genuinely bad idea." - Edward Kmett
(^) :: (Num a) => a -> Integer -> a
(^) = (P.^)

-- | Utility function used to avoid singularities
aboutZero' :: (Ord a, Num a) => a -> a -> Bool
aboutZero' toler x = abs x < toler

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
 where d = b^2 - 4*a*c
       q = -1/2*(b + signum b * sqrt d)

_quadForm_prop :: Double -> Double -> Double -> Bool
_quadForm_prop a b c = all (aboutZero' 1e-10 . eval) (quadForm a b c)
  where eval x = a*x^2 + b*x + c

------------------------------------------------------------
-- Cubic formula
------------------------------------------------------------

-- See http://en.wikipedia.org/wiki/Cubic_formula#General_formula_of_roots

-- | Solve the cubic equation ax^3 + bx^2 + cx + d = 0, returning a
--   list of all real roots. First argument is tolerance.
cubForm' :: (Floating d, Ord d) => d -> d -> d -> d -> d -> [d]
cubForm' toler a b c d
  | aboutZero' toler a      = quadForm b c d

    -- three real roots, use trig method to avoid complex numbers
  | delta >  0              = map trig [0,1,2]

    -- one real root of multiplicity 3
  | delta == 0 && disc == 0 = [ -b/(3*a) ]

    -- two real roots, one of multiplicity 2
  | delta == 0 && disc /= 0 = [ (b*c - 9*a*d)/(2*disc)
                              , (9*a^2*d - 4*a*b*c + b^3)/(a * disc)
                              ]

    -- one real root (and two complex)
  | otherwise               = [-b/(3*a) - cc/(3*a) + disc/(3*a*cc)]

 where delta  = 18*a*b*c*d - 4*b^3*d + b^2*c^2 - 4*a*c^3 - 27*a^2*d^2
       disc   = 3*a*c - b^2
       qq     = sqrt(-27*(a^2)*delta)
       qq'    | aboutZero' toler disc = maximumBy (comparing (abs . (+xx))) [qq, -qq]
              | otherwise = qq
       cc     = cubert (1/2*(qq' + xx))
       xx     = 2*b^3 - 9*a*b*c + 27*a^2*d
       p      = disc/(3*a^2)
       q      = xx/(27*a^3)
       phi = 1/3*acos(3*q/(2*p)*sqrt(-3/p))
       trig k = 2 * sqrt(-p/3) * cos(phi - k*tau/3) - b/(3*a)
       cubert x | x < 0     = -((-x)**(1/3))
                | otherwise = x**(1/3)

-- | Solve the cubic equation ax^3 + bx^2 + cx + d = 0, returning a
--   list of all real roots within 1e-10 tolerance
--   (although currently it's closer to 1e-5)
cubForm :: (Floating d, Ord d) => d -> d -> d -> d -> [d]
cubForm = cubForm' 1e-10

_cubForm_prop :: Double -> Double -> Double -> Double -> Bool
_cubForm_prop a b c d = all (aboutZero' 1e-5 . eval) (cubForm a b c d)
  where eval x = a*x^3 + b*x^2 + c*x + d
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
--   list of all real roots. First argument is tolerance.
quartForm' :: (Floating d, Ord d) => d -> d -> d -> d -> d -> d -> [d]
quartForm' toler c4 c3 c2 c1 c0
  -- obvious cubic
  | aboutZero' toler c4 = cubForm c3 c2 c1 c0
  -- x(ax^3+bx^2+cx+d)
  | aboutZero' toler c0 = 0 : cubForm c4 c3 c2 c1
  -- substitute solutions of y back to x
  | otherwise = map (\x->x-(a/4)) roots
    where
      -- eliminate c4: x^4+ax^3+bx^2+cx+d
      [a,b,c,d] = map (/c4) [c3,c2,c1,c0]
      -- eliminate cubic term via x = y - a/4
      -- reduced quartic: y^4 + py^2 + qy + r = 0
      p = b - 3/8*a^2
      q = 1/8*a^3-a*b/2+c
      r = (-3/256)*a^4+a^2*b/16-a*c/4+d

      -- | roots of the reduced quartic
      roots | aboutZero' toler r =
                0 : cubForm 1 0 p q   -- no constant term: y(y^3 + py + q) = 0
            | u < 0 || v < 0 = []     -- no real solutions due to square root
            | otherwise      = s1++s2 -- solutions of the quadratics

      -- solve the resolvent cubic - only one solution is needed
      z:_ = cubForm 1 (-p/2) (-r) (p*r/2 - q^2/8)
      
      -- solve the two quadratic equations
      -- y^2 ± v*y-(±u-z)
      u = z^2 - r
      v = 2*z - p
      u' = if aboutZero' toler u then 0 else sqrt u
      v' = if aboutZero' toler v then 0 else sqrt v
      s1 = quadForm 1 (if q<0 then -v' else v') (z-u')
      s2 = quadForm 1 (if q<0 then v' else -v') (z+u')

-- | Solve the quartic equation c4 x^4 + c3 x^3 + c2 x^2 + c1 x + c0 = 0, returning a
--   list of all real roots within 1e-10 tolerance
--   (although currently it's closer to 1e-5)
quartForm :: (Floating d, Ord d) => d -> d -> d -> d -> d -> [d]
quartForm = quartForm' 1e-10

_quartForm_prop :: Double -> Double -> Double -> Double -> Double -> Bool
_quartForm_prop a b c d e = all (aboutZero' 1e-5 . eval) (quartForm a b c d e)
  where eval x = a*x^4 + b*x^3 + c*x^2 + d*x + e
           -- Same note about tolerance as for cubic
