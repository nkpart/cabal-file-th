{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution.PackageDescription.TH (
    -- Reads a field from the cabal file in the current working directory
    packageVariable,
    -- Reads a field from the supplied cabal file
    packageVariableFrom,
    -- Re-exports so that using `packageVariable` is as simple as importing this package
    Version(..),
    PackageIdentifier(..),
    PackageDescription(..)
    ) where

-- For re-exporting.
import Distribution.PackageDescription 
import Distribution.Package
import Distribution.Version

import Distribution.Text (Text, display)
import Distribution.Verbosity (silent)
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import Data.List (isSuffixOf)
import Language.Haskell.TH (Q, Exp, stringE, runIO)

packageVariable :: Text a => (PackageDescription -> a) -> Q Exp
packageVariable = renderField currentPackageDescription

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

cabalFiles dir = do
  files <- getDirectoryContents dir
  return $ filter (".cabal" `isSuffixOf`) files

{-

Smart ways of getting the cabal file:
  * Get this module name, use TH.location and loc_module. Parse each
    cabal file in the cwd and look for references to this module
    in each thing.
  

  -}

