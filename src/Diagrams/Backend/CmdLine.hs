{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.CmdLine
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams.  This module provides a general framework
-- and default behaviors for parsing command-line arguments,
-- records for diagram creation options in various forms, and
-- classes and instances for a unified entry point to command-line-driven
-- diagram creation executables.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.CmdLine
       (

         -- * Options

         -- ** Standard options
         DiagramOpts(..)
       , diagramOpts
       , width
       , height
       , output

         -- ** Multi-diagram options
       , DiagramMultiOpts(..)
       , diagramMultiOpts
       , selection
       , list

         -- ** Animation options
       , DiagramAnimOpts(..)
       , diagramAnimOpts
       , fpu

         -- ** Loop options
       , DiagramLoopOpts(..)
       , diagramLoopOpts
       , loop
       , src
       , interval

         -- * Parsing
       , Parseable(..)
       , readHexColor

         -- * Command-line programs (@Mainable@)
         -- ** Arguments, rendering, and entry point
       , Mainable(..)

         -- ** General currying
       , ToResult(..)

         -- ** Default @mainRender@ implementations
       , defaultAnimMainRender
       , defaultMultiMainRender
       ) where

import           Control.Lens        hiding (argument)
import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Core       hiding (value)

import           Options.Applicative hiding ((&))

import           Prelude

import           Control.Monad       (forM_)

import           Data.Active         hiding (interval)
import           Data.Char           (isDigit)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Data
import           Data.List           (intercalate)
import           Data.Monoid

import           Numeric

import           Safe                (readMay)

import           System.Environment  (getProgName)
import           System.FilePath     (addExtension, splitExtension)

import           Text.Printf

-- | Standard options most diagrams are likely to have.
data DiagramOpts = DiagramOpts
    { _width  :: Maybe Int -- ^ Final output width of diagram.
    , _height :: Maybe Int -- ^ Final output height of diagram.
    , _output :: FilePath  -- ^ Output file path, format is typically chosen by extension.
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramOpts

-- | Extra options for a program that can offer a choice
--   between multiple diagrams.
data DiagramMultiOpts = DiagramMultiOpts
    { _selection :: Maybe String -- ^ Selected diagram to render.
    , _list      :: Bool         -- ^ Flag to indicate that a list of available diagrams should
                                 --   be printed to standard out.
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramMultiOpts

-- | Extra options for animations.
data DiagramAnimOpts = DiagramAnimOpts
    { _fpu :: Double -- ^ Number of frames per unit time to generate for the animation.
    }
  deriving (Show, Data, Typeable)

makeLenses ''DiagramAnimOpts

-- | Extra options for command-line looping.
data DiagramLoopOpts = DiagramLoopOpts
    { _loop     :: Bool            -- ^ Flag to indicate that the program should loop creation.
    , _src      :: Maybe FilePath  -- ^ File path for the source file to recompile.
    , _interval :: Int             -- ^ Interval in seconds at which to check for recompilation.
    }

makeLenses ''DiagramLoopOpts

-- | Command line parser for 'DiagramOpts'.
--   Width is option @--width@ or @-w@.
--   Height is option @--height@ or @-h@ (note we change help to be @-?@ due to this).
--   Output is option @--output@ or @-o@.
diagramOpts :: Parser DiagramOpts
diagramOpts = DiagramOpts
    <$> (optional . option)
        ( long "width" <> short 'w'
       <> metavar "WIDTH"
       <> help "Desired WIDTH of the output image")
    <*> (optional . option)
        ( long "height" <> short 'h'
       <> metavar "HEIGHT"
       <> help "Desired HEIGHT of the output image")
    <*> strOption
        ( long "output" <> short 'o'
       <> value ""
       <> metavar "OUTPUT"
       <> help "OUTPUT file")

-- | Command line parser for 'DiagramMultiOpts'.
--   Selection is option @--selection@ or @-s@.
--   List is @--list@ or @-l@.
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
--   Frames per unit is @--fpu@ or @-f@.
diagramAnimOpts :: Parser DiagramAnimOpts
diagramAnimOpts = DiagramAnimOpts
    <$> option
        ( long "fpu" <> short 'f'
       <> value 30.0
       <> help "Frames per unit time (for animations)")

-- | CommandLine parser for 'DiagramLoopOpts'
--   Loop is @--loop@ or @-l@.
--   Source is @--src@ or @-s@.
--   Interval is @-i@ defaulting to one second.
diagramLoopOpts :: Parser DiagramLoopOpts
diagramLoopOpts = DiagramLoopOpts
    <$> switch (long "loop" <> short 'l' <> help "Run in a self-recompiling loop")
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
--   or a hexadecimal color.
instance Parseable (Colour Double) where
    parser = argument (liftA2 (<|>) rc rh) mempty
      where
        rh s = (f . colorToSRGBA) <$> readHexColor s
        rc s = readColourName s
        f (r,g,b,_) = sRGB r g b -- TODO: this seems unfortunate.  Should the alpha
                                 -- value be applied to the r g b values?

-- | Parse @'AlphaColour' Double@ as either a named color from "Data.Colour.Names"
--   or a hexadecimal color.
instance Parseable (AlphaColour Double) where
    parser = argument (liftA2 (<|>) rc rh) mempty
      where
        rh s = readHexColor s
        rc s = opaque <$> readColourName s

-- Addapted from the Clay.Color module of the clay package

-- | Parses a hexadecimal color.  The string can start with @\"0x\"@ or @\"#\"@
--   or just be a string of hexadecimal values.  If four or three digits are
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
--   nested tuples, it always just results in the unit value
--   without consuming anything.
instance Parseable () where
    parser = pure ()

-- | Allow 'Parseable' things to be combined.
instance (Parseable a, Parseable b) => Parseable (a,b) where
    parser = (,) <$> parser <*> parser


-- | This class allows us to abstract over functions that take some arguments
--   and produce a final value.  When some @d@ is an instance of
--   'ToResult' we get a type @'Args' d@ that is a type of /all/ the arguments
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
   type Args [Diagram b v] = ()
   type ResultOf [Diagram b v] = [Diagram b v]

   toResult ds _ = ds

-- | A list of named diagrams can give the multi-diagram interface.
instance ToResult [(String,Diagram b v)] where
   type Args [(String,Diagram b v)]  = ()
   type ResultOf [(String,Diagram b v)] = [(String,Diagram b v)]

   toResult ds _ = ds

-- | An animation is another suitable base case.
instance ToResult (Animation b v) where
   type Args (Animation b v) = ()
   type ResultOf (Animation b v) = Animation b v

   toResult a _ = a

-- | Diagrams that require IO to build are a base case.
instance ToResult d => ToResult (IO d) where
   type Args (IO d) = Args d
   type ResultOf (IO d) = IO (ResultOf d)

   toResult d args = flip toResult args <$> d

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
--
--   Backends are expected to create @Mainable@ instances for the types that are
--   suitable for generating output in the backend's format.  For instance,
--   Postscript can handle single diagrams, pages of diagrams, animations as
--   separate files, and association lists.  This implies instances for
--   @Diagram Postscript R2@, @[Diagram Postscript R2]@, @Animation Postscript R2@,
--   and @[(String,Diagram Postscript R2)]@.  We can consider these as the base
--   cases for the function instance.
--
--   The associated type 'MainOpts' describes the options which need to be parsed
--   from the command-line and passed to @mainRender@.
class Mainable d where
    -- | Associated type that describes the options which need to be parsed
    -- from the command-line and passed to @mainRender@.
    type MainOpts d :: *

    -- | This method invokes the command-line parser resulting in an options
    -- value or ending the program with an error or help message.
    -- Typically the default instance will work.  If a different help message
    -- or parsing behavior is desired a new implementation is appropriate.
    --
    -- Note the @d@ argument should only be needed to fix the type @d@.  Its
    -- value should not be relied on as a parameter.
    mainArgs :: Parseable (MainOpts d) => d -> IO (MainOpts d)
    mainArgs _ = defaultOpts parser

    -- | Backend specific work of rendering with the given options and mainable
    -- value is done here.  All backend instances should implement this method.
    mainRender :: MainOpts d -> d -> IO ()

    -- | Main entry point for command-line diagram creation.  This is the method
    -- that users will call from their program @main@.  For instance an expected
    -- user program would take the following form.
    --
    -- > import Diagrams.Prelude
    -- > import Diagrams.Backend.TheBestBackend.CmdLine
    -- >
    -- > d :: Diagram B R2
    -- > d = ...
    -- >
    -- > main = mainWith d
    --
    -- Most backends should be able to use the default implementation.  A different
    -- implementation should be used to handle more complex interactions with the user.
    mainWith :: Parseable (MainOpts d) => d -> IO ()
    mainWith d = do
        opts <- mainArgs d
        mainRender opts d

-- | This instance allows functions resulting in something that is 'Mainable' to
--   be 'Mainable'.  It takes a parse of collected arguments and applies them to
--   the given function producing the 'Mainable' result.
instance (Parseable (Args (a -> d)), ToResult d, Mainable (ResultOf d))
        => Mainable (a -> d) where
    type MainOpts (a -> d) = (MainOpts (ResultOf (a -> d)), Args (a -> d))

    mainRender (opts, a) f  = mainRender opts (toResult f a)
-- TODO: why can't we get away with: instance (Parseable (Args (a -> d)), Mainable (ResultOf d)) => ...
--       Doesn't `Args (a -> d)` imply `ToResult (a -> d)` which implies `ToResult d` ?

-- | With this instance we can perform IO to produce something 
--   'Mainable' before rendering.
instance Mainable d => Mainable (IO d) where
    type MainOpts (IO d) = MainOpts d

    mainRender opts dio = dio >>= mainRender opts

-- | @defaultMultiMainRender@ is an implementation of 'mainRender' where
--   instead of a single diagram it takes a list of diagrams paired with names
--   as input.  The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The list of
--   available diagrams may also be printed by passing the option @--list@.
--
--   Typically a backend can write its @[(String,Diagram B V)]@ instance as
--
--   > instance Mainable [(String,Diagram B V)] where
--   >     type MainOpts [(String,Diagram B V)] = (DiagramOpts, DiagramMultiOpts)
--   >     mainRender = defaultMultiMainRender
--
--   We do not provide this instance in general so that backends can choose to
--   opt-in to this form or provide a different instance that makes more sense.
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
--   an animation as numbered frames, named by extending the given output file
--   name by consecutive integers.  For example if the given output file name is
--   @foo\/blah.ext@, the frames will be saved in @foo\/blah001.ext@,
--   @foo\/blah002.ext@, and so on (the number of padding digits used depends on
--   the total number of frames).  It is up to the user to take these images and
--   stitch them together into an actual animation format (using, /e.g./
--   @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
--   The @fpu@ option from 'DiagramAnimOpts' can be used to control how many frames will
--   be output for each second (unit time) of animation.
--
--   This function requires a lens into the structure that the particular backend
--   uses for it's diagram base case.  If @MainOpts (Diagram b v) ~ DiagramOpts@
--   then this lens will simply be 'output'.  For a backend supporting looping
--   it will most likely be @_1 . output@.  This lens is required because the
--   implementation works by modifying the output field and running the base @mainRender@.
--   Typically a backend can write its @Animation B V@ instance as
--
--   > instance Mainable (Animation B V) where
--   >     type MainOpts (Animation B V) = (DiagramOpts, DiagramAnimOpts)
--   >     mainRender = defaultAnimMainRender output
--
--   We do not provide this instance in general so that backends can choose to
--   opt-in to this form or provide a different instance that makes more sense.
defaultAnimMainRender :: (Mainable (Diagram b v))
                      => (Lens' (MainOpts (Diagram b v)) FilePath) -- ^ A lens into the output path.
                      -> (MainOpts (Diagram b v),DiagramAnimOpts)
                      -> Animation b v
                      -> IO ()
defaultAnimMainRender out (opts,animOpts) anim = do
    let frames  = simulate (toRational $ animOpts^.fpu) anim
        nDigits = length . show . length $ frames
    forM_ (zip [1..] frames) $ \(i,d) -> mainRender (indexize out nDigits i opts) d

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
indexize :: Lens' s FilePath ->  Int -> Integer -> s -> s
indexize out nDigits i opts = opts & out .~ output'
  where fmt         = "%0" ++ show nDigits ++ "d"
        output'     = addExtension (base ++ printf fmt (i::Integer)) ext
        (base, ext) = splitExtension (opts^.out)
