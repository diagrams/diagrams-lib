{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder.Class
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- General class for building diagrams to files.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Build where

import Diagrams.Core
import Diagrams.Size
import Data.Monoid (Any)
import Linear.V2
import Control.Lens (Lens')

-- | Generic class for building diagrams whose output is a file with a
--   2D size.
class Backend b v n => BackendBuild b v n where
  -- | Lens onto the size of the output file.
  outputSize :: Lens' (Options b v n) (SizeSpec V2 n)

  -- | Build a diagram of the given format to the path using the
  --   backend's options. The @Maybe String@ returns any errors.
  saveDia :: FilePath -> Options b v n -> QDiagram b v n Any -> IO ()

