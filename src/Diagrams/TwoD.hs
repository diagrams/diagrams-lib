{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module Diagrams.TwoD where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Expressions
import Graphics.Rendering.Diagrams.Transform

import Data.VectorSpace
import Data.LinearMap
import Data.Monoid

import qualified Data.Map as M
import Control.Arrow (first, second)

type P2 = (Double, Double)
type P3 = (Double, Double, Double)
type Aug3 = (P2,Double)

type FlatFunc = P3 -> P3
type AugFunc = Aug3 -> Aug3

instance Transformable P2 where
  type TSpace P2 = P2
  transform = papply

data Box = Box P2 P2 P2 P2
           deriving (Show)

instance Transformable Box where
  type TSpace Box = P2
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

box :: (BSpace b ~ P2, Renderable Box b) => Diagram b
box = Diagram [Prim (Box (-1,-1) (1,-1) (1,1) (-1,1))]
              (Bounds boxBounds)
              (fromNames [ ("LL", (-1,-1))
                         , ("LR", ( 1,-1))
                         , ("UR", ( 1, 1))
                         , ("UL", (-1, 1)) ])
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]

-- circle

type Radius = Double

data Ellipse = Ellipse Double Double Double Double Double Double
               deriving (Show)
-- 6 Doubles are A, B, C, D, E, F in A x^2 + B x y + C y^2 + D x + E y + F = 0

instance Transformable Ellipse where
  type TSpace Ellipse = P2
  transform (Projective t) ell
      = ellipseFromFunc ((aug3Transpose tinv)
                         . (funcFromEllipse ell)
                         . tinv) where
                               tinv = apply $ inv t

func3Transpose :: (P3 -> P3) -> (P3 -> P3)
func3Transpose func3 = \v -> (v1 `dot` v
                             ,v2 `dot` v
                             ,v3 `dot` v) where
    (v1,v2,v3) = columnsFromLinearMap func3

aug3Transpose :: (Aug3 -> Aug3) -> (Aug3 -> Aug3)
aug3Transpose = augmentFunc . func3Transpose . flattenFunc

{- need this inverse stuff?
func3Inverse :: (P3 -> P3) -> (P3 -> P3)
func3Inverse func3 = \v -> ((v2 `cross` v3) `dot` v
                           ,(v3 `cross` v1) `dot` v
                           ,(v1 `cross` v2) `dot` v) ^/ det where
    (v1,v2,v3) = columnsFromLinearMap func3
    det = v1 `dot` (v2 `cross` v3)


inverse3d :: Projective (TSpace Ellipse) -> Projective (TSpace Ellipse)
inverse3d (Projective ()
    = Projective $ linear $ \((x,y),c) -> (((v2 `cross` v3) `dot` (x,y,c)
                                           ,(v3 `cross` v1) `dot` (x,y,c))
                                          ,(v1 `cross` v2) `dot` (x,y,c)) ^/ det
      where
        (v1,v2,v3) = columnsFromLinearMap (unclimp . lapply lmap . climp)
        det = v1 `dot` (v2 `cross` v3)
-}

linearMapFromColumns (v1,v2,v3) = linear $ \(x,y,z) -> (x*^v1,y*^v2,z*^v3)

columnsFromLinearMap lmap = (v1,v2,v3) where
    v1 = lmap (1,0,0)
    v2 = lmap (0,1,0)
    v3 = lmap (0,0,1)

climp :: P3 -> Aug3
climp (a,b,c) = ((a,b),c)

unclimp :: Aug3 -> P3
unclimp ((a,b),c) = (a,b,c)

augmentFunc :: (P3 -> P3) -> (Aug3 -> Aug3)
augmentFunc f = climp . f . unclimp

flattenFunc :: (Aug3 -> Aug3) -> (P3 -> P3)
flattenFunc g = unclimp . g . climp

cross :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
cross (ax,ay,az) (bx,by,bz) = (ay * bz - az * by,az * bx - ax * bz,ax * by - ay * bx)

dot :: (Double,Double,Double) -> (Double,Double,Double) -> Double
dot (ax,ay,az) (bx,by,bz) = ax * bx + ay * by + az * bz

--showProj3d (Projective lmap)
--    = columnsFromLinearMap (unclimp . lapply lmap . climp)

{-
projectiveFromEllipse :: Ellipse -> Projective (TSpace Ellipse)
projectiveFromEllipse (Ellipse a b c d e f)
    = Projective $ linear $ \((x,y),z) -> ((a   * x + b/2 * y + d/2 * z
                                           ,b/2 * x + c   * y + e/2 * z)
                                          ,d/2 * x + e/2 * y + f   * z)

ellipseFromProjective :: Projective (TSpace Ellipse) -> Ellipse
ellipseFromProjective (Projective lmap)
    = Ellipse a b c d e f where
      ((a     ,halfb1),halfd1) = lapply lmap ((1,0),0)
      ((halfb2,c     ),halfe1) = lapply lmap ((0,1),0)
      ((halfd2,halfe2),f     ) = lapply lmap ((0,0),1)
      b = halfb1 + halfb2
      d = halfd1 + halfd2
      e = halfe1 + halfe2
-}

funcFromEllipse :: Ellipse -> AugFunc
funcFromEllipse (Ellipse a b c d e f)
    = \((x,y),z) -> ((a   * x + b/2 * y + d/2 * z
                     ,b/2 * x + c   * y + e/2 * z)
                    ,d/2 * x + e/2 * y + f   * z)

ellipseFromFunc :: AugFunc -> Ellipse
ellipseFromFunc func
    = Ellipse a b c d e f where
      ((a     ,halfb1),halfd1) = func ((1,0),0)
      ((halfb2,c     ),halfe1) = func ((0,1),0)
      ((halfd2,halfe2),f     ) = func ((0,0),1)
      b = halfb1 + halfb2
      d = halfd1 + halfd2
      e = halfe1 + halfe2

circle :: (BSpace b ~ P2, Renderable Ellipse b) => Diagram b
circle = Diagram [Prim (Ellipse 1 0 1 0 0 (-1))]
                 (Bounds circleBounds)
                 (fromNames [ ("C", ( 0, 0))
                            , ("E", ( 1, 0))
                            , ("N", ( 0, 1))
                            , ("W", (-1, 0))
                            , ("S", ( 0,-1)) ])
    where circleBounds (x,y) = 1 / sqrt(x*x + y*y)

ellipseCenter :: Ellipse -> P2
ellipseCenter (Ellipse a b c d e f)
    = (2*c*d-b*e,2*a*e-b*d) ^/ (b*b - 4*a*c)

-- long axis angle, measured ccw from x toward y
ellipseAngle :: Ellipse -> Angle
ellipseAngle (Ellipse a b c d e f)
    = atan2 (sqrt((c - a)^2 + b^2) + c - a) b

-- result is (xScale,yScale)
ellipseScale :: Ellipse -> P2
ellipseScale (Ellipse a b c d e f)
    = (1/sqrt(lam1),1/sqrt(lam2)) where
      lam1 = (a + c + sqrt((a - c)^2 + b^2))/(-2 * f)
      lam2 = (a + c - sqrt((a - c)^2 + b^2))/(-2 * f)

ellipseCenterScaleAngle :: Ellipse -> (Double,Double,Double,Double,Double)
ellipseCenterScaleAngle (Ellipse a b c d e f)
    = (xc,yc,xs,ys,th) where  -- xc = x center, ys = y scale, th = angle
      (xc,yc) = (2*c*d-b*e,2*a*e-b*d) ^/ (b*b - 4*a*c)
      th = atan2 (sqrt((c - a)^2 + b^2) + c - a) b
      (xs,ys) = (1/sqrt(lam1),1/sqrt(lam2))
      lam1 = (a + c + sqrt((a - c)^2 + b^2))/(-2 * f')
      lam2 = (a + c - sqrt((a - c)^2 + b^2))/(-2 * f')
      f' = f - (a * xc^2 + b * xc * yc + c * yc^2)

-- rotation and scaling

type Angle = Double  -- in radians

-- Do we want to rotate things in arbitrary dimensions?

rotation :: Angle -> Projective P2
rotation theta = fromLinear $ rot theta <-> rot (-theta)
  where
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)

rotate :: (TSpace t ~ P2, Transformable t) => Angle -> t -> t
rotate = transform . rotation

horizontalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
horizontalScale c = transform . fromLinear $ first (*c) <-> first (/c)

verticalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
verticalScale c = transform . fromLinear $ second (*c) <-> second (/c)


