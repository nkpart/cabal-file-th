module Distribution.PackageDescription.TH (
    packageVar,
    Version(..),
    PackageIdentifier(..),
    PackageDescription(..)
    ) where

import Distribution.PackageDescription 
import Distribution.Package
import Distribution.Version

import Distribution.Text
import Distribution.Verbosity 
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory
import Data.List (isSuffixOf)
import Language.Haskell.TH

packageVar :: Text a => (PackageDescription -> a) -> Q Exp
packageVar f = stringE . display . f =<< runIO currentPackageDescription
------
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

