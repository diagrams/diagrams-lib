-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Util
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Some miscellaneous utilities provided by the diagrams-lib package.
--
-----------------------------------------------------------------------------

module Diagrams.Util
  ( -- * Utilities for users

    with
  , applyAll
  , (#)
  , (##)

  , iterateN

  , tau

    -- * Finding files
  , findHsFile

    -- * Finding sandboxes
  , findSandbox
  , globalPackage

    -- * Internal utilities
  , foldB

  ) where

import           Control.Applicative
import           Control.Lens              hiding (( # ))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Default.Class
import           Data.List
import           Data.List.Lens
import           Data.Maybe
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FilePath.Lens
import           System.Process

-- | Several functions exported by the diagrams library take a number
--   of arguments giving the user control to \"tweak\" various aspects
--   of their behavior.  Rather than give such functions a long list
--   of arguments, and to make it possible for the user to selectively
--   override only certain arguments and use default values for
--   others, such sets of arguments are collected into a record with
--   named fields (see 'PolygonOpts' in "Diagrams.TwoD.Shapes" for an
--   example).  Such record types are made instances of the 'Default'
--   class, which provides a single record structure ('def')
--   collecting the \"default\" arguments to the function.  @with@ is
--   a synonym for 'def', which provides nice-looking syntax for
--   simulating optional, named arguments in Haskell.  For example,
--
--   @
--   polygon with {sides = 7, edgeSkip = 2}
--   @
--
--   calls the 'polygon' function with a single argument (note that
--   record update binds more tightly than function application!),
--   namely, 'with' (the record of default arguments) where the
--   @sides@ and @edgeSkip@ fields have been updated.
with :: Default d => d
with = def

-- | @applyAll@ takes a list of functions and applies them all to a
--   value, in sequence from the last function in the list to the first.
--   For example, @applyAll [f1, f2, f3] a == f1 . f2 . f3 $ a@.
applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

infixl 8 #

-- | Postfix function application, for conveniently applying
--   attributes.  Unlike @($)@, @(#)@ has a high precedence (8), so @d
--   \# foo \# bar@ can be combined with other things using operators
--   like @(|||)@ or @(\<\>)@ without needing parentheses.
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | A replacement for lenses' @#@ operator. @(##) = 'review'@.
(##) :: AReview t b -> b -> t
(##) = review
{-# INLINE (##) #-}
infixr 8 ##


-- | @iterateN n f x@ returns the list of the first @n@ iterates of
--   @f@ starting at @x@, that is, the list @[x, f x, f (f x), ...]@
--   of length @n@. (Note that the last element of the list will be
--   @f@ applied to @x@ @(n-1)@ times.)
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f = take n . iterate f

-- | The circle constant, the ratio of a circle's circumference to its
--   /radius/.  Note that @pi = tau/2@.
--
--   For more information and a well-reasoned argument why we should
--   all be using tau instead of pi, see /The Tau Manifesto/,
--   <http://tauday.com/>.
--
--   To hear what it sounds like (and to easily memorize the first 30
--   digits or so), try <http://youtu.be/3174T-3-59Q>.
tau :: Floating a => a
tau = 2*pi

-- | Given an associative binary operation and a default value to use
--   in the case of an empty list, perform a /balanced/ fold over a
--   list.  For example,
--
--   @
--   foldB (+) z [a,b,c,d,e,f] == ((a+b) + (c+d)) + (e+f)
--   @
--
foldB :: (a -> a -> a) -> a -> [a] -> a
foldB _ z [] = z
foldB f _ as = foldB' as
  where foldB' [x] = x
        foldB' xs  = foldB' (go xs)
        go []         = []
        go [x]        = [x]
        go (x1:x2:xs) = f x1 x2 : go xs

------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------

-- | Given some file (no extension or otherwise) try to find a haskell
--   source file.
findHsFile :: FilePath -> IO (Maybe FilePath)
findHsFile file = runMaybeT $ self <|> hs <|> lhs
  where
    self    = guard (hasExtension file) >> check file
    hs      = check (addExtension file "hs")
    lhs     = check (addExtension file "lhs")
    check f = do
      lift (doesFileExist f) >>= guard
      pure f

------------------------------------------------------------------------
-- Sandbox
------------------------------------------------------------------------

-- | Parse cabal config file to find the location of the package
--   database.
parseConfig :: FilePath -> MaybeT IO FilePath
parseConfig file = do
  config <- maybeIO $ readFile file
  hoistMaybe $ config ^? lined . prefixed "package-db: "

-- | Seach the given directory and all parent directories until a cabal
--   config file is found. First search for \"cabal.config\", then
--   \"cabal.sandbox.config\". Return the location of the package
--   database in the config file.
configSearch :: FilePath -> MaybeT IO FilePath
configSearch p0 = do
  p0' <- maybeIO $ canonicalizePath p0

  let mkPaths p
        | all isPathSeparator p || p == "."
                    = []
        | otherwise = (p </> "cabal.sandbox.config")
                    : mkPaths (p ^. directory)

  foldMaybeT parseConfig (mkPaths p0')

-- | Check if the folder is a database, or if it contains a database.
--   Returns the database location if it's found.
isDB :: FilePath -> MaybeT IO FilePath
isDB path =
  if isConf path
    then return path
    else maybeIO (getDirectoryContents path) >>= hoistMaybe . find isConf
    where
      isConf = isSuffixOf ".conf.d"

-- | Search for a sandbox in the following order:
--
--   * Test given FilePaths if they point directly to a database or
--     contain a cabal config file (or any parent directory containing a
--     config file).
--
--   * Same test for @DIAGRAMS_SANDBOX@ environment value
--
--   * Environment values of @GHC_PACKAGE_PATH@, @HSENV@ and
--     @PACKAGE_DB_FOR_GHC@ that point to a database.
--
--   * Test for config file in current directory (or any parents).
--
findSandbox :: [FilePath] -> IO (Maybe FilePath)
findSandbox paths = runMaybeT $ pathsTest <|> diaSB <|> envDB <|> wdConfig
  where
    -- first path in environment
    lookEnv = MaybeT . (fmap . fmap) (head . splitSearchPath) . lookupEnv
    envDB   = foldMaybeT lookEnv ["GHC_PACKAGE_PATH", "HSENV", "PACKAGE_DB_FOR_GHC"]

    -- test if path points directly to db or contains a config file
    test x    = isDB x <|> configSearch x
    pathsTest = foldMaybeT test paths
    diaSB     = lookEnv "DIAGRAMS_SANDBOX" >>= test
    wdConfig  = maybeIO getCurrentDirectory >>= configSearch

-- | Use the given path for the sandbox in the @GHC_PACKAGE_PATH@
--   environment (appending the ghc global package database from @ghc
--   --info@. @GHC_PACKAGE_PATH@ if the variable ghc and other tools use
--   to find the package database. (This is what @cabal exec@ sets)
-- ghcPackagePath :: FilePath -> IO ()
-- ghcPackagePath db = do
--   gdb <- globalPackage
--   let dbs = intercalate [searchPathSeparator] [db,gdb]
--   setEnv "GHC_PACKAGE_PATH" dbs
--
-- setEnv is only in base > 4.7, either need to use setenv package or
-- -package-db flag

-- | Find ghc's global package database. Throws an error if it isn't
--   found.
globalPackage :: IO FilePath
globalPackage = do
  info <- read <$> readProcess "ghc" ["--info"] ""
  return $ fromMaybe (error "Unable to parse ghc --info.")
                     (lookup "Global Package DB" info)

-- MaybeT utilities

-- | Lift an 'IO' action. If any exceptions are raised, return Nothing.
maybeIO :: (MonadCatch m, MonadIO m) => IO a -> MaybeT m a
maybeIO io = liftIO io `catchAll` const mzero

-- | Lift a maybe value to a MaybeT of any monad.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

-- | Fold a list of 'MaybeT's that short-circuits as soon as a Just value
--   is found (instead going through the whole list).
foldMaybeT :: Monad m => (a -> MaybeT m b) -> [a] -> MaybeT m b
foldMaybeT _ []     = mzero
foldMaybeT f (a:as) = MaybeT $ do
  x <- runMaybeT (f a)
  if isJust x
    then return x
    else runMaybeT (foldMaybeT f as)

