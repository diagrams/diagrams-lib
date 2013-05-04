{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrow
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Drawing arrows in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arrow
       ( arrow
       , arrow'
       , arrowPath
       , arrowPath'
       , connect
       , connect'
       -- , connectPath
       -- , connectPath'

       , ArrowOpts(..)
       ) where

import           Data.Default.Class
import           Data.Semigroup
import           Data.VectorSpace
import           Diagrams.Core

import           Data.Colour             (black)
import           Diagrams.Attributes     (fc)
import           Diagrams.Path
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Path      (stroke)
import           Diagrams.TwoD.Shapes    (triangle)
import           Diagrams.TwoD.Transform (rotateBy)
import           Diagrams.TwoD.Types
import           Diagrams.Util           (( # ))

data ArrowOpts b
  = ArrowOpts
    { arrowHead :: Diagram b R2
    , arrowTail :: Diagram b R2
    -- add other options:
    --   + arrow head/tail size
    --   + style attributes
    --   + whether to put gaps @ end and how big
    --   + shape of arrow path (e.g. arc), etc.
    --   + method for choosing endpoints (by location or by trace)
    --   + whether to draw the arrow over or under other stuff
    }

instance Renderable (Path R2) b => Default (ArrowOpts b) where
  def = ArrowOpts
        { arrowHead = triangle 0.1 # rotateBy (-1/4) # fc black
          -- XXX define standard arrow heads/tails in another module
        , arrowTail = mempty
        }

arrow :: Renderable (Path R2) b => P2 -> P2 -> Diagram b R2
arrow = arrow' def

arrow' :: Renderable (Path R2) b => ArrowOpts b -> P2 -> P2 -> Diagram b R2
arrow' opts s e = stroke p <> hd <> tl
  where
    p  = arrowPath' opts s e
    hd = arrowHead opts # moveTo e
         -- XXX rotate hd appropriately
         -- XXX wrap in scaleInv
    tl = arrowTail opts # moveTo s

arrowPath :: (V p ~ R2, PathLike p) => P2 -> P2 -> p
arrowPath = arrowPath' (def :: ArrowOpts NullBackend)

arrowPath' :: (V p ~ R2, PathLike p) => ArrowOpts b -> P2 -> P2 -> p
arrowPath' opts s e = s ~~ e

connect
  :: forall b n1 n2. ( Renderable (Path R2) b, IsName n1, IsName n2 )
  => n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect = connect' (def :: ArrowOpts b)

connect'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts b -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrow' opts s e)

{-
connectPath :: forall b n1 n2. (Renderable (Path R2) b, IsName n1, IsName n2) => n1 -> n2 -> Diagram b R2 -> Path R2
connectPath = connectPath' (def :: ArrowOpts b)

connectPath'
  :: (IsName n1, IsName n2)
  => ArrowOpts b -> n1 -> n2 -> Diagram b R2 -> Path R2
connectPath' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  const (arrowPath' opts s e)

    -- XXX ugh, how to actually make the above work?

    -- XXX how to remove duplication between connect' and connectPath'?
-}
