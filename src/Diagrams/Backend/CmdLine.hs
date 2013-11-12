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

       , DiagramLoopOpts(..)
       , diagramLoopOpts
       , loop
       , src
       , interval

       , Parseable(..)
       , ToResult(..)
       , Mainable(..)

       , defaultAnimMainRender
       , defaultMultiMainRender
       , readHexColor
       ) where

import Diagrams.Core hiding (value)
import Diagrams.Animation
import Diagrams.Attributes
import Control.Lens hiding (argument)

import Options.Applicative hiding ((&))

import Prelude

import Control.Monad       (forM_)

import Data.Active  hiding (interval)
import Data.Data
import Data.Char           (isDigit)
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List           (intercalate)
import Data.Monoid

import Numeric

import Safe                (readMay)

import System.Environment  (getProgName)
import System.FilePath     (addExtension, splitExtension)

import Text.Printf

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

-- | Extra options for command-line looping.
data DiagramLoopOpts = DiagramLoopOpts
    { _loop     :: Bool
    , _src      :: Maybe FilePath
    , _interval :: Int
    }

makeLenses ''DiagramLoopOpts

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

-- | CommandLine parser for 'DiagramLoopOpts'
diagramLoopOpts :: Parser DiagramLoopOpts
diagramLoopOpts = DiagramLoopOpts
    <$> switch (long "loop" <> help "Run in a self-recompiling loop")
    <*> (optional . strOption)
        ( long "src" <> short 's'
       <> help "Source file to watch")
    <*> option
        ( long "interval" <> short 'i'
       <> value 1
       <> metavar "INTERVAL"
       <> help "When running in a loop, check for changes every INTERVAL seconds.")

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
    execParser p

-- | Parseable instances give a command line parser for a type.  If a custom
--   parser for a common type is wanted a newtype wrapper could be used to make
--   a new 'Parseable' instance.  Notice that we do /not/ want as many
--   instances as 'Read' because we want to limit ourselves to things that make
--   sense to parse from the command line.
class Parseable a where
    parser :: Parser a

-- The following instance would overlap with the product instance for
-- Parseable.  We can't tell if one wants to parse (a,b) as one argument or a
-- as one argument and b as another.  Since this is the command line we almost
-- certainly want the latter.  So we need to have less Read instances.
--
-- instance Read a => Parseable a where
--    parser = argument readMay mempty

-- | Parse 'Int' according to its 'Read' instance.
instance Parseable Int where
    parser = argument readMay mempty
   
-- | Parse 'Double' according to its 'Read' instance.
instance Parseable Double where
    parser = argument readMay mempty

-- | Parse a string by just accepting the given string.
instance Parseable String where
    parser = argument Just mempty

-- | Parse 'DiagramOpts' using the 'diagramOpts' parser.
instance Parseable DiagramOpts where
    parser = diagramOpts

-- | Parse 'DiagramMultiOpts' using the 'diagramMultiOpts' parser.
instance Parseable DiagramMultiOpts where
    parser = diagramMultiOpts

-- | Parse 'DiagramAnimOpts' using the 'diagramAnimOpts' parser.
instance Parseable DiagramAnimOpts where
    parser = diagramAnimOpts

-- | Parse 'DiagramLoopOpts' using the 'diagramLoopOpts' parser.
instance Parseable DiagramLoopOpts where
    parser = diagramLoopOpts


-- | Parse @'Colour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexidecimal color.
instance Parseable (Colour Double) where
    parser = argument (liftA2 (<|>) rc rh) mempty
      where
        rh s = (f . colorToSRGBA) <$> readHexColor s
        rc s = readColourName s
        f (r,g,b,_) = sRGB r g b -- TODO: this seems unfortunate.  Should the alpha
                                 -- value be applied to the r g b values?

-- | Parse @'AlphaColour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexidecimal color.
instance Parseable (AlphaColour Double) where
    parser = argument (liftA2 (<|>) rc rh) mempty
      where
        rh s = readHexColor s
        rc s = opaque <$> readColourName s

-- Addapted from the Clay.Color module of the clay package

-- | Parses a hexidecimal color.  The string can start with @\"0x\"@ or @\"#\"@
--   or just be a string of hexidecimal values.  If four or three digits are
--   given each digit is repeated to form a full 24 or 32 bit color.  For
--   example, @\"0xfc4\"@ is the same as @\"0xffcc44\"@.  When eight or six
--   digits are given each pair of digits is a color or alpha channel with the
--   order being red, green, blue, alpha.
readHexColor :: String -> Maybe (AlphaColour Double)
readHexColor cs = case cs of
     ('0':'x':hs) -> handle hs
     ('#':hs)     -> handle hs
     hs           -> handle hs
  where
    handle hs | length hs <= 8 && all isHexDigit hs
      = case hs of
        [a,b,c,d,e,f,g,h] -> withOpacity <$> (sRGB <$> hex a b <*> hex c d <*> hex e f) <*> hex g h
        [a,b,c,d,e,f    ] -> opaque      <$> (sRGB <$> hex a b <*> hex c d <*> hex e f)
        [a,b,c,d        ] -> withOpacity <$> (sRGB <$> hex a a <*> hex b b <*> hex c c) <*> hex d d
        [a,b,c          ] -> opaque      <$> (sRGB <$> hex a a <*> hex b b <*> hex c c)
        _                 -> Nothing
    handle _ = Nothing

    isHexDigit c = isDigit c|| c `elem` "abcdef"

    hex a b = (/ 255) <$> case readHex [a,b] of
                [(h,"")] -> Just h
                _        -> Nothing


-- | This instance is needed to signal the end of a chain of
--   nested tuples.
instance Parseable () where
    parser = pure ()

-- | Allow 'Parseable' things to be combined.
instance (Parseable a, Parseable b) => Parseable (a,b) where
    parser = (,) <$> parser <*> parser

-- | Allow lists of 'Parseable'.
instance Parseable a => Parseable [a] where
    parser = many parser


-- | This class allows us to abstract over functions that take some arguments
--   and produce a final value.  When something @d@ is an instance of
--   'ToResult' we get a type @'Args' d@ that is the type of /all/ the arguments
--   at once, and a type @'ResultOf' d@ that is the type of the final result from
--   some base case instance.
class ToResult d where
    type Args d :: *
    type ResultOf d :: *

    toResult :: d -> Args d -> ResultOf d

-- | A diagram can always produce a diagram when given @()@ as an argument.
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

-- | @defaultMultiMainRender@ is an implementation of 'mainRender' where
--   instead of a single diagram it takes a list of diagrams paired with names
--   as input.  The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The list of
--   available diagrams may also be printed by passing the option @--list@.
defaultMultiMainRender :: Mainable d => (MainOpts d, DiagramMultiOpts) -> [(String, d)] -> IO () 
defaultMultiMainRender (opts,multi) ds =
    if multi^.list
      then showDiaList (map fst ds)
      else case multi^.selection of
             Nothing  -> putStrLn "No diagram selected." >> showDiaList (map fst ds)
             Just sel -> case lookup sel ds of
                           Nothing -> putStrLn $ "Unknown diagram: " ++ sel
                           Just d  -> mainRender opts d

-- | Display the list of diagrams available for rendering.
showDiaList :: [String] -> IO ()
showDiaList ds = do
  putStrLn "Available diagrams:"
  putStrLn $ "  " ++ intercalate " " ds

-- | @defaultAnimMainRender@ is an implementation of 'mainRender' which renders
-- an animation as numbered frames, named by extending the given output file
-- name by consecutive integers.  For example if the given output file name is
-- @foo\/blah.ext@, the frames will be saved in @foo\/blah001.ext@,
-- @foo\/blah002.ext@, and so on (the number of padding digits used depends on
-- the total number of frames).  It is up to the user to take these images and
-- stitch them together into an actual animation format (using, /e.g./
-- @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
-- The @fpu@ option from 'DiagramAnimOpts' can be used to control how many frames will
-- be output for each second (unit time) of animation.
defaultAnimMainRender :: (Mainable (Diagram b v), MainOpts (Diagram b v) ~ DiagramOpts) 
                      => (DiagramOpts,DiagramAnimOpts) -> Animation b v -> IO ()
defaultAnimMainRender (opts,animOpts) anim = do
    let frames  = simulate (toRational $ animOpts^.fpu) anim
        nDigits = length . show . length $ frames
    forM_ (zip [1..] frames) $ \(i,d) -> mainRender (indexize nDigits i opts) d

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
indexize :: Int -> Integer -> DiagramOpts -> DiagramOpts
indexize nDigits i opts = opts & output .~ output'
  where fmt         = "%0" ++ show nDigits ++ "d"
        output'     = addExtension (base ++ printf fmt (i::Integer)) ext
        (base, ext) = splitExtension (opts^.output)
