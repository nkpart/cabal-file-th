{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Utility functions for reading cabal file fields through template haskell.
module Distribution.PackageDescription.TH (
    -- * Template Haskell functions
    packageVariable,
    packageVariableFrom,
    packageString,
    -- * Cabal file data structures
    -- | The data structures for the cabal file are re-exported here for ease of use.
    PackageDescription(..),
    PackageIdentifier(..),
    Version(..)
    ) where

import Distribution.PackageDescription 
import Distribution.Package
import Distribution.Version

import Distribution.Text
import Distribution.Compat.ReadP
import Distribution.Verbosity (Verbosity, silent)
import Text.PrettyPrint
import System.Directory (getCurrentDirectory, getDirectoryContents)
import Data.List (isSuffixOf)
import Language.Haskell.TH (Q, Exp, stringE, runIO)

-- readPackageDescription was deprecated by readGenericPackageDescription
-- which was introduced in Cabal-2.0.0.2.
-- readPackageDescription was removed in Cabal-2.2.0.0
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
readPkgDesc = readGenericPackageDescription
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
readPkgDesc = readPackageDescription
#endif
readPkgDesc :: Verbosity -> FilePath -> IO GenericPackageDescription

newtype DocString = DocString String

instance Text DocString where
  parse = DocString `fmap` (readS_to_P read)
  disp (DocString s) = text s

-- | Provides a Text instance for String, allowing text fields to be used
--   in `packageVariable`. Use it composed with an accessor, eg.
--       packageVariable (packageString . copyright)
packageString :: String -> DocString
packageString = DocString

-- | Renders the package variable specified by the function.
-- The cabal file interrogated is the first one that is found 
-- in the current working directory.
packageVariable :: Text a => (PackageDescription -> a) -> Q Exp
packageVariable = renderField currentPackageDescription

-- | Renders the package variable specified by the function, from a cabal file
-- and the given path.
packageVariableFrom :: Text a => FilePath -> (PackageDescription -> a) -> Q Exp
packageVariableFrom s = renderField $ fmap packageDescription (readPkgDesc silent s)

------
renderField :: Text b => IO a -> (a -> b) -> Q Exp
renderField pd f = renderFieldS pd (display . f)

renderFieldS :: IO a -> (a -> String) -> Q Exp
renderFieldS pd f = runIO pd >>= stringE . f

currentPackageDescription :: IO PackageDescription
currentPackageDescription = fmap packageDescription $ do
  dir <- getCurrentDirectory
  cs <- cabalFiles dir
  case cs of
    (c:_) -> readPkgDesc silent c
    [] -> error $ "Couldn't find a cabal file in the current working directory (" ++ dir ++ ")"

cabalFiles :: FilePath -> IO [FilePath]
cabalFiles dir = do
  files <- getDirectoryContents dir
  return $ filter (".cabal" `isSuffixOf`) files

{-

Smart ways of getting the cabal file:
  * Get this module name, use TH.location and loc_module. Parse each
    cabal file in the cwd and look for references to this module
    in each thing.


  -}

