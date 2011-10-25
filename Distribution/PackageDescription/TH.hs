{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Utility functions for reading cabal file fields through template haskell.
module Distribution.PackageDescription.TH (
    -- * Template Haskell functions
    packageVariable,
    packageVariableFrom,
    -- * Cabal file data structures
    -- | The data structures for the cabal file are re-exported here for ease of use.
    PackageDescription(..),
    PackageIdentifier(..),
    Version(..)
    ) where

import Distribution.PackageDescription 
import Distribution.Package
import Distribution.Version

import Distribution.Text (Text, display)
import Distribution.Verbosity (silent)
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import Data.List (isSuffixOf)
import Language.Haskell.TH (Q, Exp, stringE, runIO)

-- | Renders the package variable specified by the function.
-- The cabal file interrogated is the first one that is found 
-- in the current working directory.
packageVariable :: Text a => (PackageDescription -> a) -> Q Exp
packageVariable = renderField currentPackageDescription

-- | Renders the package variable specified by the function, from a cabal file
-- and the given path.
packageVariableFrom :: Text a => FilePath -> (PackageDescription -> a) -> Q Exp
packageVariableFrom s = renderField $ fmap packageDescription (readPackageDescription silent s)

------
renderField :: Text b => IO a -> (a -> b) -> Q Exp
renderField pd f = runIO pd >>= stringE . display . f 

currentPackageDescription :: IO PackageDescription
currentPackageDescription = fmap packageDescription $ do
  dir <- getCurrentDirectory
  cs <- cabalFiles dir
  case cs of
    (c:_) -> readPackageDescription silent c
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

