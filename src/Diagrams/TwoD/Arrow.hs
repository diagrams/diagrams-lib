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

import           Diagrams.Path
import           Diagrams.TwoD.Path  ()
import           Diagrams.TwoD.Types
import           Diagrams.Util       (( # ))

data ArrowOpts b m
  = ArrowOpts
    { arrowHead :: QDiagram b R2 m
    , arrowTail :: QDiagram b R2 m
    -- other options like:
    --   + whether to put gaps @ end and how big
    --   + shape of arrow path (e.g. arc), etc.
    --   + method for choosing endpoints (by location or by trace)
    --   + whether to draw the arrow over or under other stuff
    --   + style attributes
    }

instance Semigroup m => Default (ArrowOpts b m) where
  def = ArrowOpts
        { arrowHead = mempty
        , arrowTail = mempty
        }

arrow :: Semigroup m => P2 -> P2 -> QDiagram b R2 m
arrow = arrow' def

arrow' :: ArrowOpts b m -> P2 -> P2 -> QDiagram b R2 m
arrow' = undefined

arrowPath :: PathLike p => P2 -> P2 -> p
arrowPath = arrowPath' (def :: ArrowOpts NullBackend Any)

arrowPath' :: PathLike p => ArrowOpts b m -> P2 -> P2 -> p
arrowPath' = undefined

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
    let [tl,hd] = map location [sub1, sub2]
    in  atop ((tl ~~ hd) <> arrowTail opts # moveTo tl <> arrowHead opts # moveTo hd)

connectPath :: forall b n1 n2. (IsName n1, IsName n2) => n1 -> n2 -> Diagram b R2 -> Path R2
connectPath = connectPath' (def :: ArrowOpts b Any)

connectPath'
  :: (IsName n1, IsName n2)
  => ArrowOpts b m -> n1 -> n2 -> Diagram b R2 -> Path R2
connectPath' = undefined
