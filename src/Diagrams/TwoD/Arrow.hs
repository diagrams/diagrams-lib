{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
       , connectPath
       , connectPath'

       , ArrowOpts(..)
       ) where

import           Data.Default.Class
import           Data.Semigroup
import           Data.VectorSpace
import           Diagrams.Core

import           Diagrams.Attributes     (fc)
import           Diagrams.Path
import           Diagrams.Prelude        (black)
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Path      (stroke)
import           Diagrams.TwoD.Shapes    (triangle)
import           Diagrams.TwoD.Transform (rotateBy)
import           Diagrams.TwoD.Types
import           Diagrams.Util           (( # ))

data ArrowOpts b m
  = ArrowOpts
    { arrowHead :: QDiagram b R2 m
    , arrowTail :: QDiagram b R2 m
    -- add other options:
    --   + arrow head/tail size
    --   + style attributes
    --   + whether to put gaps @ end and how big
    --   + shape of arrow path (e.g. arc), etc.
    --   + method for choosing endpoints (by location or by trace)
    --   + whether to draw the arrow over or under other stuff
    }

instance Semigroup m => Default (ArrowOpts b m) where
  def = ArrowOpts
        { arrowHead = triangle 0.1 # rotateBy (-1/4) # fc black
          -- XXX define standard arrow heads/tails in another module
        , arrowTail = mempty
        }

arrow :: Semigroup m => P2 -> P2 -> QDiagram b R2 m
arrow = arrow' def

arrow' :: ArrowOpts b m -> P2 -> P2 -> QDiagram b R2 m
arrow' opts s e = stroke p <> hd <> tl
  where
    p  = arrowPath' opts s e
    hd = arrowHead opts # moveTo e
         -- XXX rotate hd appropriately
         -- XXX wrap in scaleInv
    tl = arrowTail opts # moveTo s

arrowPath :: PathLike p => P2 -> P2 -> p
arrowPath = arrowPath' (def :: ArrowOpts NullBackend Any)

arrowPath' :: PathLike p => ArrowOpts b m -> P2 -> P2 -> p
arrowPath' opts s e = s ~~ e

connect
  :: forall b n1 n2. ( Renderable (Path R2) b, IsName n1, IsName n2 )
  => n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect = connect' (def :: ArrowOpts b Any)

connect'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts b Any -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrow' opts s e)

connectPath :: forall b n1 n2. (IsName n1, IsName n2) => n1 -> n2 -> Diagram b R2 -> Path R2
connectPath = connectPath' (def :: ArrowOpts b Any)

connectPath'
  :: (IsName n1, IsName n2)
  => ArrowOpts b m -> n1 -> n2 -> Diagram b R2 -> Path R2
connectPath' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  arrowPath' opts s e

    -- XXX how to remove duplication between connect' and connectPath'?
