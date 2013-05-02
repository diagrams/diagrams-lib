{-# LANGUAGE FlexibleContexts #-}
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

module Diagrams.Arrow
       ( connect
       , connect'
       ) where

import           Data.Default.Class
import           Data.Semigroup
import           Data.VectorSpace
import           Diagrams.Core

data ArrowOpts b v m
  = ArrowOpts
    { arrowHead :: QDiagram b v m
    , arrowTail :: QDiagram b v m
    -- other options like gaps @ end, etc.
    }

instance (InnerSpace v, HasLinearMap v, OrderedField (Scalar v), Semigroup m)
    => Default (ArrowOpts b v m) where
  def = ArrowOpts
        { arrowHead = mempty
        , arrowTail = mempty
        }

{-
-- this should go in Diagrams.TwoD.Arrow??

arrow :: Point v -> Point v -> QDiagram b v m
arrow = arrow' def

arrow' :: ArrowOpts b v m -> Point v -> Point v -> QDiagram b v m
arrow' = undefined
-}

connect
  :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m, IsName n1, IsName n2)
  => n1 -> n2 -> (QDiagram b v m -> QDiagram b v m)
connect = connect' def

connect' :: (IsName n1, IsName n2) => ArrowOpts b v m -> n1 -> n2 -> (QDiagram b v m -> QDiagram b v m)
connect' = undefined
