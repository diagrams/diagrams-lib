{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.CmdLine
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.CmdLine
       ( DiagramOpts(..)
       , diagramOpts
       , width
       , height
       , output

       , DiagramMultiOpts(..)
       , diagramMultiOpts
       , selection
       , list

       , DiagramAnimOpts(..)
       , diagramAnimOpts
       , fpu

       , Parseable(..)
       , ToResult(..)
       , Mainable(..)
       ) where

import Diagrams.Core hiding (value)
import Control.Lens hiding (argument)

import Options.Applicative hiding ((&))

import Prelude

import Control.Monad       (forM_)
import Control.Applicative ((<$>))
import Data.Data
import Data.Monoid
import Data.Typeable

import System.Environment  (getArgs, getProgName)


-- | Standard options most diagrams are likely to have.
data DiagramOpts = DiagramOpts
    { _width     :: Maybe Int
    , _height    :: Maybe Int
    , _output    :: FilePath
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramOpts

-- | Extra options for a program that can offer a choice
--   between multiple diagrams.
data DiagramMultiOpts = DiagramMultiOpts
    { _selection :: Maybe String
    , _list      :: Bool
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramMultiOpts

-- | Extra options for animations.
data DiagramAnimOpts = DiagramAnimOpts
    { _fpu :: Double
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramAnimOpts

-- | Command line parser for 'DiagramOpts'.
diagramOpts :: Parser DiagramOpts
diagramOpts = DiagramOpts
    <$> (optional . option)
        ( long "width" <> short 'w'
       <> value 400
       <> metavar "WIDTH"
       <> help "Desired WIDTH of the output image (default 400)")
    <*> (optional . option)
        ( long "height" <> short 'h'
       <> value 400
       <> metavar "HEIGHT"
       <> help "Desired HEIGHT of the output image (default 400)")
    <*> strOption
        ( long "output" <> short 'o'
       <> value ""
       <> metavar "OUTPUT"
       <> help "OUTPUT file")

-- | Command line parser for 'DiagramMultiOpts'.
diagramMultiOpts :: Parser DiagramMultiOpts
diagramMultiOpts = DiagramMultiOpts
    <$> (optional . strOption)
        ( long "selection" <> short 's'
       <> metavar "NAME"
       <> help "NAME of the diagram to render")
    <*> switch
        ( long "list" <> short 'l'
       <> help "List all available diagrams")

-- | Command line parser for 'DiagramAnimOpts'
diagramAnimOpts :: Parser DiagramAnimOpts
diagramAnimOpts = DiagramAnimOpts
    <$> option
        ( long "fpu" <> short 'f'
       <> value 30.0
       <> help "Frames per unit time (for animations)")

-- | A hidden \"helper\" option which always fails.
--   Taken from Options.Applicative.Extra but without the
--   short option 'h'.  We want the 'h' for Height.
helper' :: Parser (a -> a)
helper' = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short '?'
  , help "Show this help text" 
  ]

-- | Apply a parser to the command line that includes the standard 
--   program description and help behavior.  Results in parsed commands
--   or fails with a help message.
defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper' <*> optsParser)
                ( fullDesc
               <> progDesc "Command-line diagram generation."
               <> header prog)
    execParser p-- | Parseable instances give a command line parser for a type.
--   If a custom parser for a common type is wanted a newtype
--   wrapper could be used to make a new 'Parseable' instance.
--   Notice that there are instances that we do /not/ want as
--   many instances as 'Read' because we want to limit ourselves
--   to things that make sense to parse from the command line.
class Parseable a where
    parser :: Parser a

-- The following instance would overlap with the product instance for
-- Parseable.  We can't tell if one wants to parse (a,b) as one argument or a
-- as one argument and b as another.  Since this is the command line we almost
-- certainly want the latter.  So we need to have less Read instances.
--
-- instance Read a => Parseable a where
--    parser = argument readMaybe mempty
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing

instance Parseable Int where
    parser = argument readMaybe mempty
   
instance Parseable Double where
    parser = argument readMaybe mempty

instance Parseable String where
    parser = argument Just mempty

instance Parseable DiagramOpts where
    parser = diagramOpts

instance Parseable DiagramMultiOpts where
    parser = diagramMultiOpts

instance Parseable DiagramAnimOpts where
    parser = diagramAnimOpts


-- This instance is needed to signal the end of a chain of
-- nested tuples.
instance Parseable () where
    parser = pure ()

-- Allow 'Parseable' things to be combined.
instance (Parseable a, Parseable b) => Parseable (a,b) where
    parser = (,) <$> parser <*> parser

-- Allow lists of 'Parseable'.
instance Parseable a => Parseable [a] where
    parser = many parser


-- | This class allows us to abstract over functions that take some arguments
--   and produce a final value.  When something 'd' is an instance of
--   'ToResult' we get a type 'Args d' that is the type of /all/ the arguments
--   at once, and a type 'ResultOf d' that is the type of the final result from
--   some base case instance.
class ToResult d where
    type Args d :: *
    type ResultOf d :: *

    toResult :: d -> Args d -> ResultOf d

-- | A diagram can always produce a diagram when given '()' as an argument.
--   This is our base case.
instance ToResult (Diagram b v) where
    type Args (Diagram b v) = ()
    type ResultOf (Diagram b v) = Diagram b v

    toResult d _ = d

-- | A list of diagrams can produce pages.
instance ToResult [Diagram b v] where
   type Args [Diagram b v]  = ()
   type ResultOf [Diagram b v] = [Diagram b v]

   toResult ds _ = ds

-- | A list of named diagrams can give the multi-diagram interface.
instance ToResult [(String,Diagram b v)] where
   type Args [(String,Diagram b v)]  = ()
   type ResultOf [(String,Diagram b v)] = [(String,Diagram b v)]

   toResult ds _ = ds

-- | An instance for a function that, given some 'a', can produce a 'd' that is
--   also an instance of 'ToResult'.  For this to work we need both the
--   argument 'a' and all the arguments that 'd' will need.  Producing the
--   result is simply applying the argument to the producer and passing the
--   remaining arguments to the produced producer.
--
--   The previous paragraph stands as a witness to the fact that Haskell code
--   is clearer and easier to understand then paragraphs in English written by
--   me.
instance ToResult d => ToResult (a -> d) where
    type Args (a -> d) = (a, Args d)
    type ResultOf (a -> d) = ResultOf d

    toResult f (a,args) = toResult (f a) args


-- | This class represents the various ways we want to support diagram creation
--   from the command line.  It has the right instances to select between creating
--   single static diagrams, multiple static diagrams, static animations, and 
--   functions that produce diagrams as long as the arguments are 'Parseable'.
class Mainable d where
    type MainOpts d :: *

    mainArgs :: (Parseable a, Parseable (MainOpts d)) => d -> IO (MainOpts d, a)
    mainArgs _ = defaultOpts ((,) <$> parser <*> parser)

    mainRender :: MainOpts d -> d -> IO ()

    mainWith :: Parseable (MainOpts d) => d -> IO ()
    mainWith d = do
        (opts,()) <- mainArgs d
        mainRender opts d

-- | This instance allows functions resulting in something that is 'Mainable' to
--   be 'Mainable'.  It takes a parse of collected arguments and applies them to
--   the given function producing the 'Mainable' result.
instance (Parseable a, Parseable (Args d), ToResult d, Mainable (ResultOf d))
        => Mainable (a -> d) where
    type MainOpts (a -> d) = (MainOpts (ResultOf (a -> d)), Args (a -> d))

    mainRender (opts, a) f  = mainRender opts (toResult f a)



